#!/usr/bin/env python3
"""Compare two sweeps of the same specializers: timeouts, specialization time, steps.

    compare_spec.py OLD_DIR NEW_DIR SPEC [SPEC ...]

OLD_DIR and NEW_DIR hold measure CSVs (all *.csv are read).  For every
specializer: the number of benchmarks whose specialization timed out, the
geometric mean of the specialization-time ratio new/old on benchmarks where
both succeed, how many queries have identical / different step counts, and
the queries gained and lost.
"""
import csv
import glob
import math
import sys


def load(d):
    rows = {}
    for f in glob.glob(d.rstrip('/') + '/*.csv'):
        for r in csv.DictReader(open(f)):
            rows[(r['bench'], r['query'], r['specializer'])] = r
    return rows


def ok(r):
    return r is not None and r['outcome'] in ('Finished', 'GotEnough')


def steps(r):
    return int(r['unifs'] or 0) + int(r['calls'] or 0)


def main():
    old, new = load(sys.argv[1]), load(sys.argv[2])
    for sp in sys.argv[3:]:
        benches = sorted({b for (b, _, s) in new if s == sp})
        to_old = to_new = same = diff = 0
        ratios, gained, lost, diffs = [], [], [], []
        for b in benches:
            fo = 'timeout' in old.get((b, '*', sp), {}).get('outcome', '')
            fn = 'timeout' in new.get((b, '*', sp), {}).get('outcome', '')
            to_old += fo
            to_new += fn
            if fo and not fn:
                gained.append(f'{b} (no longer times out)')
            if fn and not fo:
                lost.append(f'{b} (now times out)')
            qs = sorted(q for (bb, q, s) in new if bb == b and s == sp and q != '*')
            if qs and (b, qs[0], sp) in old:
                to, tn = float(old[(b, qs[0], sp)]['spec_ms']), float(new[(b, qs[0], sp)]['spec_ms'])
                if to > 0 and tn > 0:
                    ratios.append(tn / to)
            for q in qs:
                ro, rn = old.get((b, q, sp)), new[(b, q, sp)]
                if ok(ro) and ok(rn):
                    if steps(ro) == steps(rn):
                        same += 1
                    else:
                        diff += 1
                        diffs.append(f'{b}/{q} {steps(ro)}->{steps(rn)}')
                if ok(rn) and not ok(ro):
                    gained.append(f'{b}/{q}')
                if ok(ro) and not ok(rn):
                    lost.append(f'{b}/{q}')
        gm = math.exp(sum(map(math.log, ratios)) / len(ratios)) if ratios else float('nan')
        print(f'## {sp}: timeouts {to_old} -> {to_new}; spec time {gm:.3f}x (geometric mean, {len(ratios)} benchmarks); '
              f'steps identical {same}, different {diff}')
        for label, xs in (('gained', gained), ('lost', lost), ('different', diffs[:10])):
            if xs:
                print(f'  {label}: ' + ', '.join(xs))


if __name__ == '__main__':
    main()
