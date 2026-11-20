#!/usr/bin/env python3
"""Summarize measurement CSVs produced by the `measure` executable.

usage: summarize.py OUT.md CSV...

Cost = unifications + calls ("steps").  A run is *solved* when the search
finished or produced the requested number of answers within the fuel; runs
that ran out of fuel are censored at the fuel limit.
"""
import csv, math, sys
from collections import defaultdict

BASES = ["original", "pd", "cpd-det", "cpd-branch", "conspd-det", "conspd-branch"]
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

    w("\n## Wrong results and specialization failures\n")
    for r in rows:
        if bad(r) or r["query"] == "*" or r["outcome"].startswith("eval-crash"):
            w(f"- {r['bench']} {r['query']} **{r['specializer']}**: {r['outcome']} {r['check']}")
    open(out, "w").write("\n".join(lines) + "\n")
    print("\n".join(lines))

if __name__ == "__main__":
    main(sys.argv[1], sys.argv[2:])
