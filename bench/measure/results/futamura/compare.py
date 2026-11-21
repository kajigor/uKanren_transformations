#!/usr/bin/env python3
"""Compare specializer variants pairwise on measure CSVs.

    compare.py CSV... --pair BASE:VARIANT [--pair ...] [--top N]

For each pair: queries solved by each (Finished or GotEnough), queries gained and
lost, answer mismatches of the variant, the geometric mean of the step ratio
variant/base on queries both solve, and the largest wins and losses.
"""
import argparse
import csv
import math


def ok(r):
    return r is not None and r['outcome'] in ('Finished', 'GotEnough')


def steps(r):
    return int(r['unifs'] or 0) + int(r['calls'] or 0)


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('csv', nargs='+')
    ap.add_argument('--pair', action='append', required=True)
    ap.add_argument('--top', type=int, default=6)
    a = ap.parse_args()
    rows = {}
    for f in a.csv:
        for r in csv.DictReader(open(f)):
            if r['query'] != '*':
                rows[(r['bench'], r['query'], r['specializer'])] = r
    queries = sorted({(b, q) for (b, q, _) in rows})
    for pair in a.pair:
        base, var = pair.split(':')
        sb = sv = 0
        gained, lost, ratios, mism = [], [], [], []
        for (b, q) in queries:
            rb, rv = rows.get((b, q, base)), rows.get((b, q, var))
            if rv is None:
                continue
            sb += ok(rb)
            sv += ok(rv)
            if 'MISMATCH' in rv['check']:
                mism.append(f'{b}/{q}')
            if ok(rv) and not ok(rb):
                gained.append(f'{b}/{q}')
            if ok(rb) and not ok(rv):
                lost.append(f'{b}/{q}')
            if ok(rb) and ok(rv) and steps(rb) > 0 and steps(rv) > 0:
                ratios.append((steps(rv) / steps(rb), f'{b}/{q}', steps(rb), steps(rv)))
        gm = math.exp(sum(math.log(x[0]) for x in ratios) / len(ratios)) if ratios else float('nan')
        print(f'## {var} vs {base}')
        print(f'solved {sv} vs {sb}; gained {len(gained)}, lost {len(lost)}; mismatches {len(mism)}; '
              f'geometric mean steps ratio {gm:.3f} on {len(ratios)} common queries')
        if gained:
            print('  gained: ' + ', '.join(gained))
        if lost:
            print('  lost:   ' + ', '.join(lost))
        if mism:
            print('  MISMATCH: ' + ', '.join(mism))
        ratios.sort()
        print('  best:  ' + ', '.join(f'{n} {x:.2f} ({b}->{v})' for x, n, b, v in ratios[:a.top]))
        print('  worst: ' + ', '.join(f'{n} {x:.2f} ({b}->{v})' for x, n, b, v in ratios[::-1][:a.top]))
        print()


if __name__ == '__main__':
    main()
