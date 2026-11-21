#!/usr/bin/env python3
"""Serve the visualizer and run user-submitted benchmarks on demand.

    python3 viz/server.py [--port 8765] [--data data] [--exporter PATH]

Static files are served from viz/ (like ``python3 -m http.server --directory viz``).
In addition:

  GET  /api/status   -> {"ok": true, "busy": bool, "exporter": path}
  POST /api/run      -> run viz-export on a program given in the request

Request body of /api/run (JSON):

  { "name": "myBench",                 # [A-Za-z][A-Za-z0-9_]*, at most 40 characters
    "source": "appendo x y xy = ...",   # definitions in the .mk syntax of bench/measure
    "query": "appendo x y z",           # specialization query (replaces '?' lines of source)
    "runs": "appendo [A] [B] r\\n...",   # run-time queries, one per line ('?' optional)
    "specs": ["original", "cpd-la"],    # specializers; "original" is always added
    "answers": 10, "fuel": 300000, "vanilla": false }

The benchmark is written to a temporary directory as <name>/<name>.mk and .runs,
viz-export runs on it, and the resulting <name>.json (and <name>@vanilla.json) are
copied into the data directory and added to its index.json under the group "user".
The response is {"ok": true, "benches": [names], "log": "..."} or
{"ok": false, "error": "...", "log": "..."}.  Runs are serialized; the server only
listens on 127.0.0.1.
"""

import argparse
import json
import os
import re
import shutil
import subprocess
import tempfile
import threading
import time
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer

VIZ_DIR = os.path.dirname(os.path.abspath(__file__))
REPO = os.path.dirname(VIZ_DIR)
NAME_RE = re.compile(r'^[A-Za-z][A-Za-z0-9_]{0,39}$')
SPEC_RE = re.compile(r'^[a-z][a-z0-9+-]{0,40}$')
MAX_BODY = 1 << 20          # 1 MiB of program text is plenty
RUN_TIMEOUT = 30 * 60       # whole export; each specializer has its own 60 s limit
run_lock = threading.Lock()


def find_exporter():
    """The viz-export binary of the default stack work directory."""
    try:
        root = subprocess.run(['stack', 'path', '--local-install-root'], cwd=REPO, capture_output=True,
                              text=True, check=True, timeout=120).stdout.strip()
        exe = os.path.join(root, 'bin', 'viz-export')
        if os.path.exists(exe):
            return exe
    except Exception:
        pass
    return None


def write_bench(tmp, name, source, query, runs):
    d = os.path.join(tmp, 'user', name)
    os.makedirs(d)
    lines = source.replace('\r\n', '\n').split('\n')
    if query.strip():
        # the specialization query given separately replaces the source's '?' line
        lines = [l for l in lines if not l.lstrip().startswith('?')]
        while lines and not lines[-1].strip():
            lines.pop()
        lines += ['', '? ' + query.strip().lstrip('?').strip()]
    with open(os.path.join(d, name + '.mk'), 'w') as f:
        f.write('\n'.join(lines) + '\n')
    rl = []
    for l in runs.replace('\r\n', '\n').split('\n'):
        s = l.strip()
        if not s:
            continue
        rl.append(s if s.startswith('--') or s.startswith('?') else '? ' + s)
    if rl:
        with open(os.path.join(d, name + '.runs'), 'w') as f:
            f.write('\n'.join(rl) + '\n')
    return os.path.join(tmp, 'user')


def merge_into_index(data_dir, out_dir):
    idx_path = os.path.join(data_dir, 'index.json')
    try:
        with open(idx_path) as f:
            idx = json.load(f)
    except (OSError, ValueError):
        idx = {'fuel': 300000, 'answers': 10, 'benches': []}
    with open(os.path.join(out_dir, 'index.json')) as f:
        new = json.load(f)['benches']
    names = []
    for e in new:
        src = os.path.join(out_dir, e['file'])
        if not os.path.exists(src):
            continue
        shutil.copyfile(src, os.path.join(data_dir, e['file']))
        names.append(e['name'])
    idx['benches'] = [b for b in idx.get('benches', []) if b.get('name') not in names] + \
                     [e for e in new if e['name'] in names]
    tmp_idx = idx_path + '.tmp'
    with open(tmp_idx, 'w') as f:
        json.dump(idx, f)
    os.replace(tmp_idx, idx_path)
    return names


class Handler(SimpleHTTPRequestHandler):
    exporter = None
    data_dir = None

    def __init__(self, *a, **kw):
        super().__init__(*a, directory=VIZ_DIR, **kw)

    def log_message(self, fmt, *args):
        if '/api/' in (self.path or ''):
            super().log_message(fmt, *args)

    def end_headers(self):
        self.send_header('Cache-Control', 'no-cache')
        super().end_headers()

    def reply(self, code, obj):
        body = json.dumps(obj).encode()
        self.send_response(code)
        self.send_header('Content-Type', 'application/json')
        self.send_header('Content-Length', str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path.split('?')[0] == '/api/status':
            return self.reply(200, {'ok': True, 'busy': run_lock.locked(), 'exporter': self.exporter})
        return super().do_GET()

    def do_POST(self):
        if self.path.split('?')[0] != '/api/run':
            return self.reply(404, {'ok': False, 'error': 'unknown endpoint'})
        try:
            n = int(self.headers.get('Content-Length') or 0)
            if n <= 0 or n > MAX_BODY:
                return self.reply(413, {'ok': False, 'error': 'request too large or empty'})
            req = json.loads(self.rfile.read(n))
        except (ValueError, OSError) as e:
            return self.reply(400, {'ok': False, 'error': 'bad request: %s' % e})
        name = str(req.get('name', ''))
        if not NAME_RE.match(name):
            return self.reply(400, {'ok': False, 'error': 'name must match [A-Za-z][A-Za-z0-9_]* (at most 40 chars)'})
        source = str(req.get('source', ''))
        if not source.strip():
            return self.reply(400, {'ok': False, 'error': 'empty source'})
        specs = [s for s in req.get('specs') or [] if isinstance(s, str) and SPEC_RE.match(s)]
        specs = ['original'] + [s for s in specs if s != 'original']
        try:
            answers = max(1, min(100, int(req.get('answers', 10))))
            fuel = max(1000, min(5_000_000, int(req.get('fuel', 300000))))
        except (TypeError, ValueError):
            return self.reply(400, {'ok': False, 'error': 'answers and fuel must be integers'})
        # do not overwrite a benchmark of the suite
        try:
            with open(os.path.join(self.data_dir, 'index.json')) as f:
                for b in json.load(f).get('benches', []):
                    if b.get('name') in (name, name + '@vanilla') and b.get('group') != 'user':
                        return self.reply(409, {'ok': False, 'error': f'"{name}" is a benchmark of the suite (group {b.get("group")}); choose another name'})
        except (OSError, ValueError):
            pass
        if not self.exporter:
            return self.reply(500, {'ok': False, 'error': 'viz-export not found; build it with: stack build uKanren-transformation:exe:viz-export'})
        if not run_lock.acquire(blocking=False):
            return self.reply(409, {'ok': False, 'error': 'another run is in progress; try again when it finishes'})
        tmp = tempfile.mkdtemp(prefix='viz-run-')
        try:
            bench_dir = write_bench(tmp, name, source, str(req.get('query', '')), str(req.get('runs', '')))
            out = os.path.join(tmp, 'out')
            cmd = [self.exporter, '--out', out, '--answers', str(answers), '--fuel', str(fuel),
                   '--spec', ','.join(specs)] + (['--vanilla'] if req.get('vanilla') else []) + [bench_dir]
            t0 = time.time()
            try:
                p = subprocess.run(cmd, cwd=REPO, capture_output=True, text=True, timeout=RUN_TIMEOUT)
            except subprocess.TimeoutExpired:
                return self.reply(504, {'ok': False, 'error': f'export took longer than {RUN_TIMEOUT} s'})
            # paths of the temporary directory mean nothing to the user
            log = (p.stderr or '').replace(os.path.join(bench_dir, name) + os.sep, '')[-8000:]
            produced = os.path.exists(os.path.join(out, name + '.json'))
            if p.returncode != 0 or not produced:
                ls = log.splitlines()
                at = next((i for i, l in enumerate(ls) if 'load failed' in l), None)
                err = ('\n'.join(l.rstrip() for l in ls[at:at + 8]).strip() if at is not None else
                       next((l.strip() for l in ls if 'error' in l.lower()), None))
                return self.reply(422, {'ok': False, 'log': log,
                                        'error': err or f'viz-export exited with code {p.returncode} and produced no result'})
            names = merge_into_index(self.data_dir, out)
            return self.reply(200, {'ok': True, 'benches': names, 'seconds': round(time.time() - t0, 1), 'log': log})
        finally:
            run_lock.release()
            shutil.rmtree(tmp, ignore_errors=True)


def main():
    ap = argparse.ArgumentParser(description=__doc__.split('\n')[0])
    ap.add_argument('--port', type=int, default=8765)
    ap.add_argument('--data', default='data', help='data directory, relative to viz/ (default: data)')
    ap.add_argument('--exporter', help='path of the viz-export binary (default: from stack path)')
    a = ap.parse_args()
    Handler.data_dir = os.path.join(VIZ_DIR, a.data)
    os.makedirs(Handler.data_dir, exist_ok=True)
    Handler.exporter = a.exporter or find_exporter()
    print(f'exporter: {Handler.exporter or "NOT FOUND (build viz-export)"}')
    print(f'data:     {Handler.data_dir}')
    print(f'serving   http://localhost:{a.port}/')
    ThreadingHTTPServer(('127.0.0.1', a.port), Handler).serve_forever()


if __name__ == '__main__':
    main()
