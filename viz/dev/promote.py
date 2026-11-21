#!/usr/bin/env python3
"""Promote the working copy of the visualizer to the build served by serve.py, but only if it passes the checks.

Meant to run as a Claude Code Stop hook, i.e. once per finished chunk of changes, but it can also be
run by hand.  It does nothing unless serve.py is running (or --force is given) and something changed
since the last promotion.

  1. Collect the front end (index.html, app.js, style.css, sample/) and the data into a staging build.
     The data starts from the live build, or from viz/data when viz/data itself changed.
  2. If Haskell sources changed: `stack build` viz-export, re-export the benchmarks listed in
     .viz-dev/export-args into a temporary directory, and merge them into the staging data.
  3. Check: `node --check app.js`, every data file parses as JSON, and headless Chrome renders the
     overview and each tab of the smoke benchmarks without an error message.
  4. On success, atomically repoint .viz-dev/live at the new build; serve.py notices and the open
     pages reload.  On failure, keep the old build, write .viz-dev/status.json (serve.py shows a
     banner) and exit 2 so Claude sees the error (once per distinct state, to avoid loops).
"""
import fcntl, hashlib, select, http.server, json, os, re, shlex, shutil, socketserver, subprocess, sys
import tempfile, threading, time
from functools import partial
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
VIZ = ROOT / 'viz'
DEV = ROOT / '.viz-dev'
BUILDS, LIVE, STATE, STATUS = DEV / 'builds', DEV / 'live', DEV / 'state.json', DEV / 'status.json'
FRONT = ['index.html', 'app.js', 'style.css']
HS_GLOBS = ['src/**/*.hs', 'app/viz/**/*.hs', 'package.yaml', 'stack.yaml']
DEFAULT_EXPORT_ARGS = '--only doubleAppend bench/measure/dppd bench/measure/v2s bench/measure/new bench/measure/papers'
KEEP_BUILDS = 3


class Reject(Exception):
    pass


def sha(items):
    h = hashlib.sha256()
    for it in items:
        h.update(repr(it).encode())
    return h.hexdigest()[:16]


def fingerprints():
    front = [(f, (VIZ / f).read_bytes()) for f in FRONT if (VIZ / f).exists()]
    front += [(str(p), p.read_bytes()) for p in sorted((VIZ / 'sample').glob('*')) if p.is_file()]
    hs = [(str(p), p.read_bytes()) for g in HS_GLOBS for p in sorted(ROOT.glob(g)) if p.is_file()]
    data = [(p.name, p.stat().st_mtime_ns, p.stat().st_size) for p in sorted((VIZ / 'data').glob('*.json'))]
    return {'front': sha(front), 'hs': sha(hs), 'data': sha(data)}


def load_json(p, default):
    try:
        return json.loads(Path(p).read_text())
    except Exception:
        return default


def server_running():
    pid = load_json(DEV / 'server.json', {}).get('pid')
    if not pid:
        return False
    try:
        os.kill(pid, 0)
        return True
    except OSError:
        return False


def run(cmd, what, timeout=None, **kw):
    r = subprocess.run(cmd, cwd=ROOT, capture_output=True, text=True, timeout=timeout, **kw)
    if r.returncode != 0:
        out = (r.stdout + r.stderr).strip()
        raise Reject(f'{what} failed (exit {r.returncode}):\n' + '\n'.join(out.splitlines()[-60:]))
    return r


def rebuild_data(stage_data):
    """Rebuild viz-export and re-export the configured benchmarks into stage_data."""
    run(['stack', 'build', 'uKanren-transformation:exe:viz-export'], 'stack build viz-export', timeout=1500)
    args_file = DEV / 'export-args'
    args = shlex.split(args_file.read_text() if args_file.exists() else DEFAULT_EXPORT_ARGS, comments=True)
    with tempfile.TemporaryDirectory(dir=DEV) as tmp:
        run(['stack', 'exec', 'viz-export', '--', '--out', tmp] + args, 'viz-export', timeout=1500)
        new_idx = load_json(Path(tmp) / 'index.json', None)
        if not new_idx or not new_idx.get('benches'):
            raise Reject('viz-export produced no benchmarks (see "load failed" in its output)')
        if '--only' in args:
            want = set(args[args.index('--only') + 1].split(','))
            got = {b['name'] for b in new_idx['benches']}
            if not want <= got:
                raise Reject(f'viz-export did not produce: {", ".join(sorted(want - got))}')
        idx = load_json(stage_data / 'index.json', {'benches': []})
        fresh = {b['name']: b for b in new_idx['benches']}
        idx['benches'] = [fresh.pop(b['name'], b) for b in idx.get('benches', [])] + list(fresh.values())
        idx['generated'] = new_idx.get('generated')
        for b in new_idx['benches']:
            dst = stage_data / b['file']
            dst.unlink(missing_ok=True)  # never write through a shared/hard-linked file
            shutil.copy2(Path(tmp) / b['file'], dst)
        (stage_data / 'index.json').unlink()
        (stage_data / 'index.json').write_text(json.dumps(idx, indent=1))
        return sorted(b['name'] for b in new_idx['benches'])


class _Quiet(http.server.SimpleHTTPRequestHandler):
    def log_message(self, *a):
        pass


def smoke_test(stage, benches):
    """Render pages of the staged build in headless Chrome and look for error messages in the DOM."""
    node = shutil.which('node')
    if node:
        run([node, '--check', str(stage / 'app.js')], 'node --check app.js')
    for p in (stage / 'data').glob('*.json'):
        try:
            json.loads(p.read_text())
        except Exception as e:
            raise Reject(f'data/{p.name} is not valid JSON: {e}')
    chrome = next((c for c in ['google-chrome-stable', 'google-chrome', 'chromium', 'chromium-browser'] if shutil.which(c)), None)
    if not chrome:
        return 'skipped (no Chrome)'
    srv = socketserver.ThreadingTCPServer(('127.0.0.1', 0), partial(_Quiet, directory=str(stage)))
    srv.daemon_threads = True
    threading.Thread(target=srv.serve_forever, daemon=True).start()
    base = f'http://127.0.0.1:{srv.server_address[1]}/'
    routes = [''] + [f'#/{b}/{t}' for b in benches for t in ('program', 'spec', 'runs')]
    try:
        with tempfile.TemporaryDirectory() as prof:
            for r in routes:
                res = subprocess.run([chrome, '--headless=new', '--disable-gpu', '--no-first-run', f'--user-data-dir={prof}',
                                      '--virtual-time-budget=15000', '--dump-dom', base + r],
                                     capture_output=True, text=True, timeout=90)
                dom = res.stdout
                page = r or 'overview'
                if 'id="main"' not in dom:
                    raise Reject(f'headless Chrome could not render {page}:\n{res.stderr[-2000:]}')
                if not re.search(r'id="data-info"[^>]*>\d+ benchmarks', dom):
                    raise Reject(f'{page}: the data index did not load (sidebar does not show "N benchmarks")')
                m = re.search(r'class="msg error"[^>]*>(.*?)</div>', dom, re.S)
                if m:
                    raise Reject(f'{page}: page shows an error: {re.sub("<[^>]+>", "", m.group(1))[:500]}')
                if r and ('Loading ' + r.split('/')[1] + '…') in dom:
                    raise Reject(f'{page}: still "Loading…" after 15 s of virtual time (exception in render?)')
    finally:
        srv.shutdown()
    return f'{len(routes)} pages rendered'


def promote(fp, force=False):
    DEV.mkdir(exist_ok=True)
    BUILDS.mkdir(exist_ok=True)
    state = load_json(STATE, {})
    last = state.get('promoted', {})
    if not force and fp == last and LIVE.exists():
        return None, 'unchanged'
    if not force and fp == state.get('failed'):
        return None, 'unchanged since the last rejected attempt'

    changed = [k for k in fp if fp[k] != last.get(k)] if LIVE.exists() else ['front', 'hs', 'data']
    stage = Path(tempfile.mkdtemp(prefix='.staging-', dir=BUILDS))
    notes = []
    try:
        for f in FRONT:
            shutil.copy2(VIZ / f, stage / f)
        if (VIZ / 'sample').is_dir():
            shutil.copytree(VIZ / 'sample', stage / 'sample')
        src = VIZ / 'data' if ('data' in changed or not (LIVE / 'data').exists()) else LIVE / 'data'
        shutil.copytree(src, stage / 'data', copy_function=os.link if src != VIZ / 'data' else shutil.copy2)
        if 'hs' in changed and last.get('hs') is not None or force and '--rebuild' in sys.argv:
            notes.append('re-exported ' + ', '.join(rebuild_data(stage / 'data')))
        smoke = load_json(DEV / 'smoke-benches', None) or ['doubleAppend']
        idx_names = {b['name'] for b in load_json(stage / 'data' / 'index.json', {}).get('benches', [])}
        notes.append(smoke_test(stage, [b for b in smoke if b in idx_names]))
    except BaseException:
        shutil.rmtree(stage, ignore_errors=True)
        raise

    name = time.strftime('%Y%m%d-%H%M%S-') + fp['front'][:6]
    final = BUILDS / name
    stage.rename(final)
    tmp_link = DEV / 'live.tmp'
    tmp_link.unlink(missing_ok=True)
    tmp_link.symlink_to(Path('builds') / name)
    os.replace(tmp_link, LIVE)  # atomic swap
    state = {'promoted': fp, 'build': name}
    STATE.write_text(json.dumps(state))
    old = sorted(p for p in BUILDS.iterdir() if p.is_dir() and not p.name.startswith('.') and p.name != name)
    for p in old[:-(KEEP_BUILDS - 1)] if KEEP_BUILDS > 1 else old:
        shutil.rmtree(p, ignore_errors=True)
    return name, '; '.join(['changed: ' + ', '.join(changed)] + notes)


def main():
    force = '--force' in sys.argv
    hook = {}
    if not sys.stdin.isatty() and select.select([sys.stdin], [], [], 0.5)[0]:  # hook input, if any
        try:
            hook = json.loads(sys.stdin.read() or '{}')
        except Exception:
            pass
    if not force and not server_running():
        return 0
    DEV.mkdir(exist_ok=True)
    with open(DEV / 'lock', 'w') as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        try:
            fp = fingerprints()  # the state being checked; edits made after this go to the next run
            build, msg = promote(fp, force)
        except (Reject, subprocess.TimeoutExpired) as e:
            st = load_json(STATE, {})
            st['failed'] = fp
            STATE.write_text(json.dumps(st))
            STATUS.write_text(json.dumps({'ok': False, 'time': time.time(), 'message': str(e)}))
            print(f'viz dev: changes NOT promoted, the page keeps serving the last good build.\n{e}', file=sys.stderr)
            # exit 2 makes Claude continue and fix it; a repeated failure of the same state is not re-reported
            return 2 if not hook.get('stop_hook_active') else 1
        if not build and msg == 'unchanged' and not load_json(STATUS, {}).get('ok', True):
            # back at the promoted state (e.g. the broken change was reverted): clear the banner
            STATUS.write_text(json.dumps({'ok': True, 'time': time.time(), 'message': 'back at the live build'}))
        if build:
            STATUS.write_text(json.dumps({'ok': True, 'time': time.time(), 'message': f'{build}: {msg}'}))
            print(f'viz dev: promoted {build} ({msg})')
        elif force:
            print(f'viz dev: {msg}')
    return 0


if __name__ == '__main__':
    sys.exit(main())
