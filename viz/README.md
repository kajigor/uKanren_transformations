# Specialization visualizer

A static web page for inspecting how the specializers transform a benchmark, and for comparing
how the original and the residual programs run.

## Generate the data

```
stack build uKanren-transformation:exe:viz-export
stack exec viz-export -- --out viz/data bench/measure/dppd bench/measure/v2s bench/measure/new bench/measure/papers
# options: --only NAME[,NAME]  --spec original,cpd-det,...  --answers 10  --fuel 300000
#          --vanilla   also export every benchmark run through the vanilla self-interpreter
#                      ("<name>@vanilla", the Jones-optimality test, src/Measure/Vanilla.hs)
```

For every benchmark, `viz-export` (`app/viz/Main.hs`):

* runs each specializer (60 s timeout) and records the residual program;
* for the base specializers (`cpd-*`, `conspd-*`), records the global tree and the local
  unfolding tree of every global node. Each tree is cut at 1500 nodes;
* runs the original program and every residual program on the benchmark's run-time queries with
  the instrumented interpreter (`Measure.Interp.runGoalProfiled`), and records:
  * steps;
  * the step at which each answer appeared;
  * a per-relation profile of calls and successful/failed unifications;
  * the first 400 search events;
  * the same cross-check as the `measure` harness.

The format is described in `SCHEMA.md`.

## View it

```
python3 -m http.server 8765 --directory viz
# open http://localhost:8765/            (generated data in viz/data)
#      http://localhost:8765/?data=sample (small hand-written sample)
```

Views (URL `#/<bench>/program|spec|runs`):

* **Overview**: a benchmark × specializer matrix of steps relative to the original program, for
  the first query or the geometric mean over all queries.
* **Program**: source, specialization query, run-time queries, and a table of specializers.
* **Specialization**:
  * the global tree (pan/zoom, collapsible, or an outline view), coloured by node kind;
  * clicking a node shows its goal, generalizer and local unfolding tree;
  * the residual program next to the source.
* **Runs & comparison**:
  * a per-query summary table, with ratios against the original program and cross-check flags;
  * a bar chart of steps;
  * an answer timeline (cumulative answers against steps);
  * a per-relation cost profile of two programs side by side;
  * a trace player that replays the first search events of the original program next to a
    specialized one.

## Dev mode: auto-reload with only good builds swapped in

```
python3 viz/dev/serve.py            # http://localhost:8765/
```

`serve.py` serves the last *promoted* build (`.viz-dev/live`), not the working copy. Open pages
reload when a new build is promoted, and they keep their route, selections, preferences and scroll
position (`app.js` saves them on `pagehide`).

`viz/dev/promote.py` runs as a Claude Code Stop hook (`.claude/settings.local.json`), so it fires
once per finished chunk of changes. It does nothing unless `serve.py` is running. It stages a
build and swaps it in atomically only if:

* `node --check app.js` passes;
* every data file parses as JSON;
* headless Chrome renders the overview and the three tabs of the smoke benchmarks
  (`.viz-dev/smoke-benches`, a JSON list, default `["doubleAppend"]`) without an error message.

If Haskell sources changed, it first runs `stack build` on `viz-export` and re-exports the
benchmarks listed in `.viz-dev/export-args` (default: `--only doubleAppend` plus the benchmark
directories), then merges them into the data.

If a check fails, the old build stays live and the page shows a red banner with the error. The
hook exits with code 2, so Claude sees the error and keeps working on it. Each failing state is
reported only once. Run it by hand with `python3 viz/dev/promote.py --force [--rebuild]`.
