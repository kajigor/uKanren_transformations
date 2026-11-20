#!/usr/bin/env python3
"""Compare two measurement sweeps.

usage: compare.py OLD.csv[,OLD2.csv..] NEW.csv[,..] [--only-spec REGEX] [--quiet]

OLD may also be a directory (all *.csv inside are loaded).  Prints, per
specializer present in both sweeps: solved / wrong (MISMATCH) / unverified
counts before -> after, the geometric mean of steps(new)/steps(old) over
queries solved by both, and then every query whose outcome or step count
changed by more than 5%.
"""
import csv, glob, math, os, re, sys

def load(spec):
    rows = {}
    for p in spec.split(","):
        files = sorted(glob.glob(os.path.join(p, "*.csv"))) if os.path.isdir(p) else [p]
        for f in files:
            for r in csv.DictReader(open(f)):
                rows[(r["bench"], r["query"], r["specializer"])] = r
    return rows

def solved(r): return r["outcome"] in ("Finished", "GotEnough")
def wrong(r): return r["check"] == "MISMATCH"
def unver(r): return r["check"].startswith("unverified")
def steps(r): return int(r["unifs"]) + int(r["calls"])

def gmean(xs):
    xs = [x for x in xs if x > 0]
    return math.exp(sum(map(math.log, xs)) / len(xs)) if xs else float("nan")

def main():
    args = [a for a in sys.argv[1:] if not a.startswith("--")]
    flt = None
    if "--only-spec" in sys.argv:
        flt = re.compile(sys.argv[sys.argv.index("--only-spec") + 1])
        args = [a for a in args if a != sys.argv[sys.argv.index("--only-spec") + 1]]
    quiet = "--quiet" in sys.argv
    old, new = load(args[0]), load(args[1])
    specs = sorted({k[2] for k in new} & {k[2] for k in old})
    if flt: specs = [s for s in specs if flt.search(s)]
    print("| specializer | solved | wrong | unverif. | spec fail | steps new/old |")
    print("|---|---|---|---|---|---:|")
    for s in specs:
        o = {k: r for k, r in old.items() if k[2] == s}
        n = {k: r for k, r in new.items() if k[2] == s}
        oq = [r for k, r in o.items() if k[1] != "*"]
        nq = [r for k, r in n.items() if k[1] != "*"]
        of = sum(1 for k in o if k[1] == "*"); nf = sum(1 for k in n if k[1] == "*")
        ratios = [steps(n[k]) / max(1, steps(o[k])) for k in n
                  if k in o and k[1] != "*" and solved(n[k]) and solved(o[k])]
        print(f"| {s} | {sum(map(solved, oq))} → {sum(map(solved, nq))} | {sum(map(wrong, oq))} → {sum(map(wrong, nq))} "
              f"| {sum(map(unver, oq))} → {sum(map(unver, nq))} | {of} → {nf} | {gmean(ratios):.3f} |")
    if quiet: return
    print("\nChanged queries (outcome/check changed, or steps changed by >5%):\n")
    for k in sorted(new):
        if k[2] not in specs: continue
        r, o = new[k], old.get(k)
        if o is None:
            if k[1] == "*" or (k[0], "*", k[2]) in old: print(f"- {k}: now {r['outcome']} {r['check']}")
            continue
        if k[1] == "*": continue
        changed = (solved(o), o["check"]) != (solved(r), r["check"]) or \
                  (solved(o) and solved(r) and abs(steps(r) - steps(o)) > 0.05 * max(1, steps(o)))
        if changed:
            fo = f"{steps(o)} {o['outcome']} {o['check']}"; fn = f"{steps(r)} {r['outcome']} {r['check']}"
            print(f"- {k[0]} {k[1]} {k[2]}: {fo} → {fn}")
    for k in sorted(old):
        if k[1] == "*" and k[2] in specs and k not in new:
            print(f"- {k[0]} {k[2]}: was {old[k]['outcome'][:50]}, now specializes")

if __name__ == "__main__":
    main()
