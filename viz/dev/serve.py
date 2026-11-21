#!/usr/bin/env python3
"""Dev server for the visualizer: serves the last promoted build (.viz-dev/live, see promote.py) and
reloads open pages when a new build is promoted.  Pages keep their view and settings across the
reload (app.js saves them to sessionStorage/localStorage).

  python3 viz/dev/serve.py [PORT]        # default 8765
"""
import http.server, json, os, signal, socketserver, subprocess, sys, time
from functools import partial
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
DEV = ROOT / '.viz-dev'
LIVE, STATUS = DEV / 'live', DEV / 'status.json'
PORT = int(sys.argv[1]) if len(sys.argv) > 1 else 8765

# Injected into index.html: reload on a new build, show a banner when the latest changes were rejected.
SNIPPET = b"""<script>(() => {
  let build = null, banner = null;
  const es = new EventSource('/__dev/events');
  es.addEventListener('build', e => { if (build !== null && e.data !== build) location.reload(); build = e.data; });
  es.addEventListener('status', e => {
    const s = JSON.parse(e.data);
    if (s.ok) { if (banner) banner.remove(); banner = null; return; }
    if (!banner) {
      banner = document.createElement('details');
      banner.style.cssText = 'position:fixed;right:12px;bottom:12px;z-index:9999;max-width:min(640px,90vw);max-height:60vh;overflow:auto;' +
        'background:#3b0d0d;color:#ffd7d7;border:1px solid #a33;border-radius:6px;padding:6px 10px;font:12px/1.4 ui-monospace,monospace';
      document.body.appendChild(banner);
    }
    banner.innerHTML = '<summary style="cursor:pointer">Latest changes rejected \\u2014 showing the last good build</summary><pre style="white-space:pre-wrap;margin:6px 0 0"></pre>';
    banner.querySelector('pre').textContent = s.message;
  });
})();</script>
"""


def current_build():
    try:
        return os.readlink(LIVE)
    except OSError:
        return ''


def read_status():
    try:
        return STATUS.read_text()
    except OSError:
        return '{"ok": true}'


class Handler(http.server.SimpleHTTPRequestHandler):
    def __init__(self, *a, **kw):
        # resolve the symlink per request, so a swap takes effect immediately
        super().__init__(*a, directory=os.path.realpath(LIVE), **kw)

    def end_headers(self):
        self.send_header('Cache-Control', 'no-store')
        super().end_headers()

    def log_message(self, fmt, *args):
        if not str(args[1] if len(args) > 1 else '').startswith(('2', '3')):
            super().log_message(fmt, *args)

    def do_GET(self):
        path = self.path.split('?', 1)[0].split('#', 1)[0]
        if path == '/__dev/events':
            return self.events()
        if path in ('/', '/index.html'):
            body = (Path(self.directory) / 'index.html').read_bytes().replace(b'</body>', SNIPPET + b'</body>', 1)
            self.send_response(200)
            self.send_header('Content-Type', 'text/html; charset=utf-8')
            self.send_header('Content-Length', str(len(body)))
            self.end_headers()
            self.wfile.write(body)
            return
        super().do_GET()

    def events(self):
        self.send_response(200)
        self.send_header('Content-Type', 'text/event-stream')
        self.end_headers()
        sent = (None, None)
        last_ping = 0
        try:
            while True:
                now = (current_build(), read_status())
                if now[0] != sent[0]:
                    self.wfile.write(f'event: build\ndata: {now[0]}\n\n'.encode())
                if now[1] != sent[1]:
                    self.wfile.write(f'event: status\ndata: {json.dumps(json.loads(now[1]))}\n\n'.encode())
                if now != sent or time.time() - last_ping > 15:
                    if now == sent:
                        self.wfile.write(b': ping\n\n')
                    self.wfile.flush()
                    last_ping = time.time()
                sent = now
                time.sleep(0.3)
        except (BrokenPipeError, ConnectionResetError, ValueError):
            pass


class Server(socketserver.ThreadingMixIn, http.server.HTTPServer):
    daemon_threads = True
    allow_reuse_address = True


def main():
    DEV.mkdir(exist_ok=True)
    if not LIVE.exists():
        print('no promoted build yet, promoting the working copy...')
        r = subprocess.run([sys.executable, str(ROOT / 'viz/dev/promote.py'), '--force'], stdin=subprocess.DEVNULL)
        if r.returncode != 0 or not LIVE.exists():
            sys.exit('initial promotion failed; fix the errors above and retry')
    srv = Server(('127.0.0.1', PORT), Handler)
    signal.signal(signal.SIGTERM, lambda *_: sys.exit(0))  # so server.json gets removed
    (DEV / 'server.json').write_text(json.dumps({'pid': os.getpid(), 'port': PORT}))
    print(f'serving {current_build()} at http://localhost:{PORT}/  (Ctrl-C to stop)')
    try:
        srv.serve_forever()
    except KeyboardInterrupt:
        pass
    finally:
        (DEV / 'server.json').unlink(missing_ok=True)


if __name__ == '__main__':
    main()
