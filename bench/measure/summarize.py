#!/usr/bin/env python3
"""Summarize measurement CSVs produced by the `measure` executable.

usage: summarize.py OUT.md CSV...

Cost = unifications + calls ("steps").  A run is *solved* when the search
finished or produced the requested number of answers within the fuel; runs
that ran out of fuel are censored at the fuel limit.
"""
import csv, math, sys
from collections import defaultdict

BASES = ["original", "pd", "cpd-det", "cpd-branch", "cpd-la", "conspd-det", "conspd-branch", "conspd-la"]
VARIANTS = ["", "+opt", "+sched", "+opt+sched"]

def load(paths):
    rows = []
    for p in paths:
        with open(p) as f:
            rows += list(csv.DictReader(f))
    return rows

def solved(r):
    return r["outcome"] in ("Finished", "GotEnough")

def bad(r):
    # only exact answer-set mismatches (both searches finished) count as wrong;
    # fuel-limited cross checks are reported separately as "unverified"
    return r["check"] == "MISMATCH"

def unverified(r):
    return r["check"].startswith("unverified") or r["check"].endswith("?")

def steps(r):
    return int(r["unifs"]) + int(r["calls"])

def gmean(xs):
    xs = [x for x in xs if x > 0]
    return math.exp(sum(map(math.log, xs)) / len(xs)) if xs else float("nan")

def main(out, paths):
    rows = load(paths)
    by = {(r["bench"], r["query"], r["specializer"]): r for r in rows}
    queries = sorted({(r["bench"], r["query"]) for r in rows if r["query"] != "*"})
    lines = []
    w = lines.append

    w("## Aggregate per specializer\n")
    w("Steps ratio: geometric mean of steps(variant)/steps(original) over queries both solve "
      "(lower is better). `solved`: queries solved within the fuel. `wrong`: answer-set mismatch "
      "(both searches finished). `unverif.`: a fuel-limited cross-check could not confirm an answer. `spec fail`: specialization crashed or timed out. `size`: total goal nodes, "
      "summed over all benchmarks the specializer handles.\n")
    w("| specializer | solved | wrong | unverif. | spec fail | steps ratio vs original | steps ratio vs base | size |")
    w("|---|---:|---:|---:|---:|---:|---:|---:|")
    for b in BASES:
        for v in VARIANTS:
            name = b + v
            rs = [r for r in rows if r["specializer"] == name]
            if not rs:
                continue
            sf = len({r["bench"] for r in rs if r["query"] == "*"})
            qs = [r for r in rs if r["query"] != "*"]
            ratios, ratios_base = [], []
            for r in qs:
                o = by.get((r["bench"], r["query"], "original"))
                if o and solved(o) and solved(r):
                    ratios.append(steps(r) / max(1, steps(o)))
                base = by.get((r["bench"], r["query"], b))
                if v and base and solved(base) and solved(r):
                    ratios_base.append(steps(r) / max(1, steps(base)))
            sizes = {}
            for r in qs:
                sizes[r["bench"]] = max(sizes.get(r["bench"], 0), int(r["size"]))
            w(f"| {name} | {sum(map(solved, qs))}/{len(queries)} | {sum(map(bad, qs))} | {sum(map(unverified, qs))} | {sf} | "
              f"{gmean(ratios):.3f} | {'-' if not v else f'{gmean(ratios_base):.3f}'} | {sum(sizes.values())} |")

    w("\n## Per query: steps (unifications + calls)\n")
    w("`∞` = out of fuel, `✗` = specialization failed, `!` marks a wrong result.\n")
    cols = [b + v for b in BASES for v in ("", "+opt+sched")]
    w("| bench | query | " + " | ".join(cols) + " |")
    w("|---|---|" + "---:|" * len(cols))
    for (bn, q) in queries:
        cells = []
        for c in cols:
            r = by.get((bn, q, c)) or by.get((bn, "*", c))
            if r is None:
                cells.append("")
            elif r["query"] == "*":
                cells.append("✗")
            else:
                s = str(steps(r)) if solved(r) else f"∞({r['answers']})"
                if bad(r):
                    s += "!"
                cells.append(s)
        w(f"| {bn} | {q} | " + " | ".join(cells) + " |")

    jones(rows, by, w)

    w("\n## Wrong results and specialization failures\n")
    for r in rows:
        if bad(r) or r["query"] == "*" or r["outcome"].startswith("eval-crash"):
            w(f"- {r['bench']} {r['query']} **{r['specializer']}**: {r['outcome']} {r['check']}")
    open(out, "w").write("\n".join(lines) + "\n")
    print("\n".join(lines))

def jones(rows, by, w):
    """Jones-optimality table for benchmarks measured with --vanilla.

    For every specializer S and query q of a benchmark p that has a twin
    p@vanilla (p run by the vanilla self-interpreter):
      overhead  = steps(original(p@vanilla)) / steps(original(p))
      vs direct = steps(S(p@vanilla)) / steps(original(p))   (Jones-optimal: <= 1)
      vs S(p)   = steps(S(p@vanilla)) / steps(S(p))          (interpretive overhead S leaves)
    """
    twins = sorted({r["bench"][:-len("@vanilla")] for r in rows if r["bench"].endswith("@vanilla")})
    if not twins:
        return
    specs = []
    for r in rows:
        if r["specializer"] not in specs:
            specs.append(r["specializer"])
    w("\n## Jones optimality (vanilla self-interpreter)\n")
    w("For each benchmark p, `p@vanilla` runs p through the vanilla self-interpreter "
      "(`src/Measure/Vanilla.hs`). `solved`: queries of p@vanilla solved. `vs direct`: geometric mean of "
      "steps(S(p@vanilla))/steps(original p) (a Jones-optimal S gives <= 1). `vs S(p)`: "
      "steps(S(p@vanilla))/steps(S(p)), the interpretive overhead S fails to remove (1 = none left). "
      "`optimal`: queries with steps(S(p@vanilla)) <= steps(S(p)). Ratios over queries both sides solve.\n")
    w("| specializer | solved | wrong | spec fail | vs direct | vs S(p) | optimal |")
    w("|---|---:|---:|---:|---:|---:|---:|")
    for sp in specs:
        solvedn = wrong = fails = opt = total = 0
        r1, r2 = [], []
        for p in twins:
            vs = [r for r in rows if r["bench"] == p + "@vanilla" and r["specializer"] == sp]
            if any(r["query"] == "*" for r in vs):
                fails += 1
            for r in vs:
                if r["query"] == "*":
                    continue
                total += 1
                solvedn += solved(r)
                wrong += bad(r)
                o = by.get((p, r["query"], "original"))
                d = by.get((p, r["query"], sp))
                if solved(r) and o and solved(o):
                    r1.append(steps(r) / max(1, steps(o)))
                if solved(r) and d and solved(d):
                    r2.append(steps(r) / max(1, steps(d)))
                    opt += steps(r) <= steps(d)
        w(f"| {sp} | {solvedn}/{total} | {wrong} | {fails} | {gmean(r1):.3f} | {gmean(r2):.3f} | {opt} |")

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2:])
