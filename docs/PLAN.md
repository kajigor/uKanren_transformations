# Plan: incremental improvements on branch `pipeline`

Each step builds on the previous one and ends with a commit and a measurement in
`bench/measure/results/step<N>/`. The baseline is `bench/measure/results/final/`, the
`all-improvements` sweep. Settings for every step are 10 answers and fuel 300 000.

| step | what | why (REPORT.md item / observation) | status |
|---|---|---|---|
| 1 | **Modular pipeline.** Every post-pass is a named flag that can be switched on and off: `base+ue+fail+inl+merge+dead+sched`. Add presets, an ablation preset, `--jobs` (benchmarks run in parallel) and `--timeout`. | Experiments need components that can be swapped and turned off. Sweeps are slow. | |
| 2 | **Total CPD residualizer.** Fix the root cause in `CPD.GlobalControl.getNodes` so that every leaf has a definition. | §3.1: raw residuals call undefined relations; "missing means failure" can lose answers. | |
| 3 | **Fix PD residualization.** | §3.1: 9 wrong PD results. | |
| 4 | **RAF/FAR pass** (redundant argument filtering, both directions). | §5 item 2. | |
| 5 | **Determinism- and cost-aware scheduling.** | §4: the `queens5`, `regexp` and `prop` regressions under `+sched`. | |
| 6 | **Budgets in global control.** Generalize to the original call when the budget runs out. | §4: `queens5` times out; `typecheck` and `unifier` time out for `cpd-branch`; `sortGeneral` crashes. | |
| 7 | **New benchmarks:** fusion of collection operations (map/fold/zip/filter, forward and backward) and anything else found along the way, plus mitigations for what they expose. | Discussion of fusion and lookahead. | |
| 8 | **Revise REPORT.md** with the new results, ablations and updated directions. | | |

Steps can be reordered if a measurement shows a more urgent problem. Each step records in this
file what changed and what was measured.

## Log

### Step 1: modular pipeline (done)
* `Measure.Runner.mkSpecializer` parses `BASE(+COMPONENT)*`. The components are `ue fail inl merge dead sched`,
  always applied in pipeline order, and the preset is `opt = ue+fail+inl+merge+dead`. `measure --list` prints them.
* `--variants base|std|full|ablation` (ablation = full pipeline minus one component at a time), `--bases`,
  `--spec`, `--timeout`, and `--jobs` (a worker pool over benchmarks; the executable is now `-threaded`).
* Raw residualizer outputs are available as bases for linting: `pd/raw`, `cpd-*/raw`, `conspd-det/raw`.
* Check: the `std` sweep reproduces all 1080 rows of `results/final` exactly (steps, answers, outcome,
  check). With `--jobs 8` a full sweep takes 11.5 min.
