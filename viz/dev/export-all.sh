#!/usr/bin/env bash
# Re-export the whole suite (and the vanilla twins) into viz/data, one process per benchmark
# directory, then merge the partial indexes.  Benchmarks of the group "user" are kept.
set -e
cd "$(dirname "$0")/../.."
P=viz/parts/full
rm -rf "$P"; mkdir -p "$P"
for d in dppd v2s new papers; do
  stack exec viz-export -- --vanilla --out "$P/$d" bench/measure/$d > "$P/$d.log" 2>&1 &
done
wait
python3 -I - "$P" <<'PY'
import json, os, shutil, sys
P = sys.argv[1]
idx = json.load(open('viz/data/index.json')) if os.path.exists('viz/data/index.json') else {'benches': []}
user = [b for b in idx['benches'] if b.get('group') == 'user']
benches, meta = [], {}
for d in ['dppd', 'v2s', 'new', 'papers']:
    sub = json.load(open(os.path.join(P, d, 'index.json')))
    meta = {k: v for k, v in sub.items() if k != 'benches'}
    for e in sub['benches']:
        shutil.copyfile(os.path.join(P, d, e['file']), os.path.join('viz/data', e['file']))
        benches.append(e)
meta['benches'] = benches + user
json.dump(meta, open('viz/data/index.json', 'w'))
print(len(benches), 'benchmarks exported,', len(user), 'user benchmarks kept')
PY
