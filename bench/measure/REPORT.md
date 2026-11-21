# Specialization of miniKanren programs: directions, improvements, measurements

This report covers four things: how the specializers in this repository were evaluated, the
improvements implemented on top of them, the results, and suggested next steps.

## Branches

| branch | based on | contents |
|---|---|---|
| `measure-harness` | `main` | Instrumented interpreter (`src/Measure/Interp.hs`), cross-checking runner (`src/Measure/Runner.hs`), executable `measure`, benchmark suite `bench/measure/` |
| `fix-residual-soundness` | `measure-harness` | Two correctness fixes in residualization/purification (found by the harness) |
| `post-spec-opt` | `measure-harness` | `src/PostOpt.hs`: post-specialization optimizer. `src/Schedule.hs`: mode-directed polyvariant conjunct scheduling. Harness variants `+opt` and `+sched`, plus `summarize.py` |
| `all-improvements` | `post-spec-opt` + `fix-residual-soundness` | Integration branch used for the final measurements (`bench/measure/results/final/`) |

How to reproduce:

```
stack build uKanren-transformation:exe:measure
stack exec measure -- --answers 10 --fuel 300000 --csv out.csv bench/measure
python3 bench/measure/summarize.py summary.md out.csv
# Options: --only NAME[,NAME]  --spec original,cpd-det+opt+sched  --dump (print residual programs and answers)
```

## 1. Methodology

* **Cost model.** The interpreter follows standard microKanren semantics:
  * every relation call is an inverse-η delay, and these delays are the only interleaving points;
  * disjunction is interleaving `mplus`;
  * conjunction is `bind`.

  It counts *unifications* and *calls* until N answers are found, the search space is exhausted,
  or a fuel budget runs out (`steps = unifs + calls`). These counts do not depend on the machine
  and closely follow the cost of OCanren/faster-miniKanren execution. Wall-clock times are also
  recorded, but they are noisy because the suite runs in parallel.
* **Specialize once, run many times.** Each benchmark consists of a specialization query
  (`<name>.mk`, `? goal`) and a list of run-time queries (`<name>.runs`), each an instance of the
  specialization query. Each run-time query is evaluated against the residual program as
  `(t1..tn) == (s1..sn) & residual-goal`. Running "backward" queries against a program
  specialized for "nothing" shows how specialization affects the different directions of a
  relation.
* **Cross-checking.** Every residual program is checked against the original program:
  * When both searches finish, the answer sets must be equal; otherwise the result is a `MISMATCH`.
  * Otherwise, each of the first 20 answers of one program is replayed as a query against the
    other program, with fuel. A failure is reported as `unverified-sound` or
    `unverified-complete`. These checks are heuristic, because the original program often
    diverges even on instantiated queries; `flip` is an example.
* **Specializers measured.**
  * `original` (no specialization), `pd`, `cpd-det`, `cpd-branch`, `conspd-det`,
    `conspd-branch` (the ConsPD global-control variant).
  * Each one also with `+opt` (PostOpt), `+sched` (Schedule) and `+opt+sched`.
  * Offline PD is not included, because it needs per-mode annotation files and does not
    specialize on static data.

## 2. Benchmarks

* **`bench/measure/dppd`**: the 16 DPPD-derived examples already in the repository, plus
  generic variants with several run-time queries (`doubleAppend` forward/backward/partial,
  `revGeneral`).
* **`bench/measure/v2s`**: the verifier-to-solver examples from the repository: prop ×4, path,
  sort, relSort, typecheck, unifier, arith. Also generic variants `sortGeneral`,
  `relSortGeneral` and `arithGeneral`, specialized for `sorto x y` / `multiply x y z` and run
  both forward and backward.
* **`bench/measure/new`**: harder examples proposed in this work.
  * `queens5`: a generic 5-queens placement checker used as a solver (generate-and-test). Both
    online specializers time out on it (60 s), which shows that local and global control need
    budgets.
  * `coloring`: 3-colouring of a fixed graph given as an edge list, interpreted by the checker
    through `nth`. Specialization should compile the edge list away.
  * `exprEval`: an interpreter for Peano arithmetic expressions, specialized to a fixed
    expression `x*x + 2*y` and run forward and backward. Specializing the interpreter ("the
    first Futamura projection") produces residual programs whose conjunct order is wrong for
    the forward direction.
  * `exprEvalGeneral`: the same interpreter with nothing static.

## 3. Implemented improvements

### 3.1 Soundness fixes (`fix-residual-soundness`)

The harness's cross-check found residual programs with **extra answers**. Two causes:

* **`Purification.clearGoal`.** It deleted conjuncts that call relations with no residual
  definition. Deleting a conjunct weakens the conjunction, so the program gains answers. Such
  calls now mean failure: the enclosing conjunction fails and the call is dropped from an
  enclosing disjunction.
* **`Syntax.isSuccess`/`isFailure`.** `isSuccess (Invoke successName [])` binds the constant as a
  pattern variable, so *every* nullary call counted as both success and failure, and
  purification erased it. Nullary residual definitions were also filtered out.

Effect, measured against a sweep run before the fix:

* `matchSimple`: an exact answer-set mismatch under cpd-det and conspd-det. Both produced an
  extra, wrong answer, e.g. `x = 0 :: S _ :: _`. Both are now `ok`.
* `path`: unverified-sound answers under cpd-det and conspd-det. Both now pass the cross-check,
  and lint confirms the raw residual had the same defect.
* No regressions anywhere in the suite.

The root cause is upstream. Linting the raw residualization output (branch `design-proposals`,
`Core.Lint`) shows that CPD's residualizer emits calls to definitions it never generates, on
`matchSimple`, `path` and `relSort`. The likely culprit is `CPD.GlobalControl.getNodes`, which
skips nodes whose local tree is an immediate `Leaf`. "Missing means failure" is sound, but it
can lose answers if such a goal actually has solutions. No such loss was observed in the suite.
The proper fix is a total residualizer (see `docs/DESIGN.md` on that branch).

**Not fixed: PD residualization.** `PD.Residualization` pairs leaves with the wrong definitions.
On `exprEval` the residual calls `_eval … (Var 0) q1`, but `_eval`'s body requires
`y5 = Mul (Var 0) (Var 0)`. The same bug causes wrong results on `coloring`,
`deforestation1`, `exDepth`, `matchSimple`, `queens5`, `rev`, `sort` and `uptoSum`: 9 wrong
query results in total. The `Gen` case of `findNode` is the first suspect. PD rows in the
tables below should be read with that in mind.

### 3.2 `PostOpt`: post-specialization optimizer (`src/PostOpt.hs`)

Answer-preserving passes, iterated to a fixpoint:

* normalization;
* **existential unification elimination** (`∃v.(v = t ∧ G) ≡ G[t/v]`), with constructor
  decomposition and clash detection;
* **failure analysis**: a least-fixpoint "may-succeed" analysis plus static unification
  consistency;
* **post-unfolding** of non-recursive definitions (≤ 12 goal nodes or called once), in the spirit
  of ECCE's determinate post-unfolding;
* **duplicate-definition merging** by coarsest partition refinement, a bisimulation over
  definitions modulo renaming;
* unreachable-definition removal.

### 3.3 `Schedule`: mode-directed polyvariant conjunct scheduling (`src/Schedule.hs`)

The pass is applied per run-time query, because the query determines which arguments are ground.

* For every reachable (relation, call mode) pair it creates a copy whose conjunctions are
  reordered greedily:
  1. unifications first;
  2. then calls *estimated to terminate* in their mode, preferring those with more ground
     arguments;
  3. then the remaining calls in their original order.

  A call is estimated to terminate if it is structurally recursive on a ground argument, checked
  over the SCC DAG, so callees in other mode copies must terminate too.
* Groundness propagates through a success-pattern table (relation × call mode ↦ ground
  arguments), computed as a decreasing fixpoint.
* Conjunction is commutative w.r.t. the answer set of a complete interleaving search, so the
  pass preserves answers. It changes only cost, termination and the order of answers.

## 4. Results (`bench/measure/results/final`, branch `all-improvements`)

Settings: 10 answers, fuel 300 000 steps, 34 benchmarks, 46 run-time queries. The full per-query
table is in `bench/measure/results/summary.md`.

Column meanings:

* `steps vs base`: the geometric mean of steps(variant) / steps(base specializer), over queries
  both solve. It is the apples-to-apples measure for the effect of a pass.
* `steps vs original`: the same ratio with the unspecialized program as the reference. It is
  computed over different query sets per row, because `+sched` solves additional, harder
  queries.

| specializer | solved | wrong | spec fail | steps vs original | steps vs base | size |
|---|---:|---:|---:|---:|---:|---:|
| original | 34/46 | 0 | 0 | 1.000 | – | 1009 |
| original+opt+sched | 37/46 | 0 | 0 | 0.759 | 0.759 | 1351 |
| cpd-det | 30/46 | 0 | 2 | 0.180 | – | 1555 |
| cpd-det+opt | 30/46 | 0 | 2 | 0.117 | 0.640 | 2063 |
| cpd-det+sched | 35/46 | 0 | 2 | 0.219 | 1.005 | 2226 |
| cpd-det+opt+sched | 34/46 | 0 | 2 | 0.145 | 0.653 | 2574 |
| cpd-branch | 30/46 | 0 | 3 | 0.117 | – | 3394 |
| cpd-branch+opt+sched | 33/46 | 0 | 3 | 0.097 | 0.692 | 3694 |
| conspd-det | 33/46 | 0 | 1 | 0.112 | – | 1187 |
| conspd-det+opt | 33/46 | 0 | 1 | 0.086 | 0.773 | 821 |
| conspd-det+opt+sched | 35/46 | 0 | 1 | 0.104 | 0.799 | 1064 |
| conspd-branch | 33/46 | 0 | 1 | 0.110 | – | 1312 |
| conspd-branch+opt+sched | 35/46 | 0 | 1 | 0.104 | 0.809 | 1762 |
| pd (buggy, see 3.1) | 37/46 | 9 | 0 | 0.108 | – | 1838 |

**Findings.**

* **PostOpt never reduces the number of solved queries** (PD+opt solves one more). It cuts steps
  by 20–36 % over CPD and ConsPD, and by 57 % over PD.
  * Most of the saving is in calls. For example, depth/cpd-det goes from 8 calls to 0, and
    coloring/cpd-det from 193 calls to 0.
  * ConsPD residuals also shrink, e.g. conspd-det from 1187 to 821 goal nodes.
  * CPD residuals grow, because inlining duplicates small bodies.
* **Scheduling solves queries that every specializer, or the original, gave up on.** On queries
  that are already solved it is cost-neutral: 0.96–1.01 relative to the base. Examples, in steps
  (∞ = out of fuel):

  | query | before | after |
  |---|---|---|
  | `flip`, original | ∞ | 89 |
  | `rotatePrune`, original | ∞ | 46 |
  | `doubleAppend` q3 (x known, rest backward), original | ∞ | 46 |
  | `sortGeneral` q2 (sort *backward* = permutation search), original | 6 answers, then ∞ | 10 answers in 64 885 |
  | `exprEval` q1 (forward), *all* CPD/ConsPD residuals | ∞ | 109–123 (original: 187) |
  | `relSort`, cpd-det / cpd-branch | ∞ | 529 / 243 |
  | `arithGeneral` q3, conspd-* | ∞ | 150 |
  | `typecheck` q2, cpd-det | ∞ | 149 |

  The `exprEval` row is the key observation. Specializing an interpreter for a static program
  produced residual conjunctions whose order is wrong for the forward direction. Online PD fixes
  the *data* but not the *schedule*.
* **Regressions.**
  * `queens5` under original+opt+sched: 259 551 steps → out of fuel at 8 of 10 answers. q2 goes
    from 61k to 80k. Generate-and-test gains nothing from reordering at this granularity, and the
    tie-breaking changes the enumeration order.
  * `regexp`/conspd-det: 41 → 73.
  * `propFirst`/`propLast`, original: 155–163 → 181.
  * PD+sched is worse than PD on several queries, but PD's residuals are often wrong anyway.
  * Scheduling grows code, about 1.1–1.5× from per-mode copies.
* **Specializer robustness.**
  * `queens5` makes cpd-det, conspd-det and conspd-branch time out at 60 s. cpd-branch also timed
    out under heavier machine load in an earlier sweep.
  * cpd-det and cpd-branch crash on `sortGeneral` ("invocation of an undefined relation").
  * cpd-branch times out on `typecheck` and `unifier`.
  * ConsPD is the most robust, but it makes `revGeneral` q2 and `sortGeneral` diverge where the
    original does not.
  * No single specializer dominates. This agrees with the VPT'21 study.

## 5. Related work and directions

The survey covered the papers in `kajigor/tex` and recent work, 2020–2026. The group's own
future-work sections keep naming three topics:

* conjunct ordering justified by mode and determinism (miniKanren'22/'23, PEPM'24);
* more precise BTA (HCVS'24);
* a cost model and hybrid offline/online control (VPT'21).

Recent related work:

* Rozplokhas & Boulytchev, *Scheduling complexity of interleaving search* (FLOPS'22), a basis
  for the cost model.
* Engel & Verbitskaia, typed embedding for functional conversion (miniKanren'25), which names the
  occurs check and EDSL overhead as remaining costs.
* Rational-tree unification for miniKanren (Domoratskiy, Kosarev, Boulytchev, miniKanren'25/'26).
* chrKanren (ICLP'26).
* Kudasov, bottom-up enumeration with memoization (miniKanren'26).
* Kafle & Gallagher, query–answer transformation (a mode-free form of magic sets).
* Leuschel & Vidal, size-change-based offline PE (2014).

No learning-guided specialization work was found. That niche looks open.

Ranked next steps; the measurements above support items 1–3:

1. **Make scheduling a first-class pipeline stage, driven by a determinism analysis.** Today's
   greedy heuristic already solves 2–5 extra queries per specializer. A Mercury-style
   determinism domain would fix the `queens5`/`regexp` regressions: keep semidet tests early,
   and do not reorder past nondeterministic generators without evidence. So would a cost
   estimate (Rozplokhas–Boulytchev).
2. **Always run PostOpt on residual programs, and add RAF/FAR on its output.** It is cheap and
   reliable: 20–36 % fewer steps, never worse in solved count.
3. **A total residualizer and budgets in global control.** This would fix the undefined-relation
   crashes and silent failures, and make specialization *anytime* for `queens5`-like inputs.
4. **Groundness, sharing and freeness analysis (Pos domain) feeding the BTA and the backend.**
   This replaces the exhaustive direction enumeration criticised in TEASE'20 and lets the backend
   drop the occurs check (named as overhead in miniKanren'25).
5. **Finite-domain case splitting and characteristic trees** for polyvariance control, e.g. for
   `evalo` on a boolean output (PEPM'24).
6. **Disequality constraints (constrained PD), then tabling / query–answer transformation** for
   `path`-like and cyclic search problems.
7. **A multi-result driver with an empirical cost model.** The step counter in this harness
   already is one: pick the best residual per mode.

**Harder examples worth adding next.** Relational interpreters run backward (quines, Barliman
style), STLC type inhabitation with polymorphism, Einstein/zebra (the repository has a
translation under `src/TranslatedExamples`), Sudoku 4×4, binary arithmetic (Kiselyov et al.),
cyclic-graph reachability, and bridge-and-torch or goat-wolf-cabbage with unknown move counts
(`bench/Goat`, `bench/Hanoi`).

The suite already includes three new examples that stress the specializers in different ways.
`queens5` breaks every online specializer's control. `exprEval` shows interpreter specialization
producing badly ordered residuals. `coloring` compiles an interpreted edge list away: 37k steps
originally, 61 for ConsPD+opt+sched.

All cited works, with links and provenance, are collected in `docs/RELATED_WORK.md`.

The design-level proposals (one core IR with linting, one analysis engine, a strategy-based
specializer driver, DSL changes) are in `docs/DESIGN.md` on branch `design-proposals`.

## 6. Futamura projections, Jones optimality, and the collected roadmap (branch `futamura`)

### 6.1 What was added

* **Jones-optimality harness (`src/Measure/Vanilla.hs`, `measure --vanilla`).** Every
  benchmark `p` gets a twin `p@vanilla`: `p` encoded as clauses of the classic vanilla
  self-interpreter, with the query `r ts` replaced by `jo_solve [Jo_r ts]`.
  * Each disjunct of a definition's DNF becomes one `jo_clause` clause. Its body list keeps
    the original conjunct order, and object-level unifications become `Jo_eq` atoms.
  * Specializing `jo_solve` for the static object program is the **first Futamura
    projection**. A Jones-optimal specializer gives back a program no slower than `p`.
  * `summarize.py` reports, per specializer `S`:
    * `steps(S(p@vanilla)) / steps(p)` ("vs direct");
    * `steps(S(p@vanilla)) / steps(S(p))` ("vs S(p)", the interpretive overhead `S` leaves);
    * the number of queries on which `S` is optimal.
  * This turns the whole suite (34 benchmarks) into a Jones-optimality test.
* **Determinate unfolding with failure lookahead (`Unfold.pruneByLookahead`; specializers
  `cpd-la`, `conspd-la`).** Before counting the alternatives of a selected call, local control
  drops resultants in which some call fails within `k = 2` further unfolding steps (Gallagher
  1991; Leuschel's ECCE).
  * Without it, `jo_atom (Jo_eq a b)` is never unfolded deterministically: its second branch
    (`jo_clause (Jo_eq a b) _`) only fails one step down. Every object-level unification then
    stays in the residual program as an extra relation.
  * Only finitely failing branches are removed, so answer sets are unchanged.
  * A first version reused the fresh-name supply in nested unfoldings and produced wrong
    answers. The harness caught it, and the version measured here threads the supply.
* **Stronger cross-check.** When one side's answer set is complete, every answer of the other
  side must be an *instance* of one of its answers. Before, an over-general wrong answer (e.g.
  `S v` where `12` is the only solution, `fclMult`/conspd-branch) passed the replay check as
  `ok~`.
* **Benchmarks from the literature (`bench/measure/papers`, see its README).** Seven programs,
  all with forward and backward queries:
  * `tmInterp`: Turing-machine interpreter (Jones–Gomard–Sestoft 1993);
  * `fclMult`: flowchart-language interpreter (JGS 1993);
  * `kmp`: the KMP test (Consel–Danvy 1989; Sørensen–Glück–Jones 1996);
  * `binMul` / `binFactor`: Kiselyov et al.'s binary arithmetic (FLOPS 2008);
  * `lambdaAppend`: relational λ-interpreter (Byrd et al. 2012/2017);
  * `power`: JGS 1993, Ershov 1977.
* **Visualizer (`viz/`, `viz-export`).** A static web page with:
  * global and local trees of every specializer;
  * residual programs next to the source;
  * profiled runs of original and residual programs: per-relation cost, answer timeline, and a
    trace player.

  See `viz/README.md`.

### 6.2 Results of the full sweep (`bench/measure/results/futamura/`)

**Setup.**
* `measure --vanilla --answers 10 --fuel 300000` over all 41 benchmarks (`dppd`, `v2s`, `new`,
  `papers`) and their 41 `@vanilla` twins: 136 run-time queries, 68 of them on twins.
* Raw data: `la.csv`; summary: `summary.md`.
* This sweep still used the old cross-check. Answer sets that are not exact matches therefore
  appear as `unverif.` rather than with the stronger instance check of §6.1.

**Aggregate.**

| specializer | solved | wrong | unverif. | spec fail | steps vs original | size |
|---|---:|---:|---:|---:|---:|---:|
| original | 95/136 | 0 | 0 | 0 | 1.000 | 2842 |
| cpd-det | 70/136 | 0 | 17 | 6 | 0.189 | 7266 |
| **cpd-la** | **85/136** | 0 | 6 | 5 | 0.119 | 5231 |
| cpd-det+opt+sched | 90/136 | 0 | 9 | 6 | 0.084 | 9262 |
| **cpd-la+opt+sched** | **98/136** | 0 | 3 | 5 | 0.082 | 9437 |
| conspd-det | 49/136 | 0 | 19 | 26 | 0.119 | 7850 |
| conspd-la | 52/136 | 0 | 14 | 29 | 0.089 | 3320 |
| conspd-det+opt+sched | 55/136 | 0 | 13 | 26 | 0.100 | 17231 |
| conspd-la+opt+sched | 58/136 | 0 | 13 | 29 | 0.087 | 15663 |

**Jones optimality** (`@vanilla` twins only). "vs direct" is the geometric mean of
steps(S(p@vanilla)) / steps(p); "vs S(p)" is the geometric mean of
steps(S(p@vanilla)) / steps(S(p)); "optimal" counts queries with
steps(S(p@vanilla)) ≤ steps(S(p)).

| specializer | solved | spec fail | vs direct | vs S(p) | optimal |
|---|---:|---:|---:|---:|---:|
| original (interpreter, unspecialized) | 45/68 | 0 | 22.9 | – | – |
| cpd-det | 26/63 | 4 | 5.04 | 16.6 | 0 |
| cpd-la | 41/63 | 3 | 2.38 | 10.6 | 1 |
| cpd-det+opt+sched | 39/63 | 4 | 0.97 | 4.84 | 6 |
| cpd-la+opt+sched | **46/63** | 3 | 1.18 | 7.40 | **10** |
| conspd-det | 4/29 | 24 | 7.51 | 29.4 | 0 |
| conspd-la | 7/24 | 27 | 0.70 | 2.19 | 3 |

**Findings.**

* **Lookahead is the largest single improvement measured in this repository.**
  * cpd-la solves 15 more queries than cpd-det (85 vs 70; it gains 18 and loses 3).
  * It needs fewer steps (×0.119 vs ×0.189 of the original) and produces smaller residuals
    (5231 vs 7266 goal nodes).
  * With PostOpt and scheduling it solves **98/136, more than the original program (95)** and
    more than any other configuration.
  * Its queries lost against cpd-det are `binMul` q2 and `coloring@vanilla` q1/q2. Its one
    regression above 20 % is `unifier@vanilla` (20k → 43k steps).
* **Interpretive overhead is far from removed.**
  * The unspecialized self-interpreter costs about 23× the direct program.
  * The best configuration brings the twins it solves to about 1.2× the direct program.
  * It is still about 7× slower than specializing the direct program, and Jones-optimal on only
    10 of 63 queries.
  * 17 twin queries remain unsolved.
* **ConsPD does not scale to the self-interpreter.** It times out on 24–27 of the 34 twins.
  Where it finishes, it is the closest to Jones optimality (conspd-la: 0.70 vs direct). This is
  problem P4 below.
* **No exact answer-set mismatches anywhere.** The `unverif.` counts are fuel-limited
  cross-checks.

### 6.3 Problems found by the new benchmarks

These numbers come from the paper benchmarks (`bench/measure/papers`, all specializers) and from
the self-interpreter versions of `doubleAppend`, `rev`, `exprEval` and `lambdaAppend`, at
`--answers 10 --fuel 300000`. Aggregates over the whole suite are in §6.2.

**P1. Without lookahead, the self-interpreter's object-level unifications are never compiled
away.**
* *Evidence:* `doubleAppend@vanilla` runs out of fuel on q1 under cpd-det (12 definitions) and
  conspd-det (53 definitions); the unspecialized direct program needs 90 steps.
* *Cause:* `jo_atom (Jo_eq a b)` has two branches, and the second one fails only one call
  deeper, so deterministic unfolding stops there.
* *Effect of the fix:* with lookahead (`conspd-la`), the residual has 8 definitions and needs
  75/56/45 steps on q1–q3. On `rev@vanilla`, cpd-det needs 52 steps and cpd-la 25.

**P2. Interpreter specialization yields residuals in the wrong conjunct order.**
* *Evidence:*
  * `doubleAppend@vanilla` / cpd-la is `append` and `double_append` up to renaming, but the two
    calls are swapped. It finds the correct answer, then makes 70k calls before running out of
    fuel. With `+opt+sched` it needs 85 steps.
  * `exprEval` forward: every CPD/ConsPD residual runs out of fuel without scheduling, and needs
    109–123 steps with it (original: 141).
  * `binMul` q3 (finite failure): runs out of fuel under every plain CPD/ConsPD variant; only
    `+opt+sched` restores termination (149–163 steps).
  * `power` q2/q3 (backward): out of fuel without scheduling, 307–7743 steps with it.
* *Reading:* online PD fixes the data, not the schedule. Scheduling has to be a mandatory
  pipeline stage.

**P3. Online specializers can make a terminating forward query diverge after its last
answer.**
* *Evidence:* `power` q1 (2^3) needs 124 steps unspecialized. Every online specializer, with or
  without `+opt+sched`, finds the answer and then runs out of fuel.
* *Reading:* the residual loses a termination argument that the original had. This is not a
  scheduling problem. It needs a diagnosis: compare the residual's recursion with the original
  in `viz/`.

**P4. ConsPD's global control blows up on interpreters.**
* *Evidence:*
  * conspd-det and cpd-branch time out (60 s) on `exprEval@vanilla` and `lambdaAppend@vanilla`;
    conspd-la does on `lambdaAppend@vanilla`.
  * conspd-det and conspd-det+opt+sched time out on `fclMult`.
  * conspd-det/conspd-branch produce 211 definitions on `tmInterp`, and find no answer for the
    forward query.
* *Reading:* the interpreter's growing goal lists and stores keep producing new conjunctions.
  There is no budget, so the specializer cannot stop with a partial result.

**P5. Two interpreter layers are not removed.**
* *Evidence:* `lambdaAppend@vanilla` (the vanilla interpreter running a λ-interpreter running
  `append`) needs about 10.8k steps under cpd-la; cpd-det runs out of fuel. Specializing the
  λ-interpreter directly gives 18 steps: an exact `append`, the ideal first-projection result.
* *Reading:* the whistle fires on the static object program (expressions, environments) and
  generalizes it away.

**P6. The KMP test is not passed.**
* *Evidence:* on `kmp` (pattern `[A,A,B,A,B]`) the best residual (conspd-branch) is 5.3× faster
  than the original, but its step counts still grow with how much text is re-scanned. No
  residual is a KMP automaton.
* *Reading:* this is the classical result that partial deduction without *negative
  information* cannot reach KMP, while positive supercompilation with disequalities can
  (Sørensen–Glück–Jones 1996).

**P7. Soundness and completeness defects in specializers.**
* *Wrong answers:*
  * `fclMult` / conspd-branch returns over-general answers (`1 + v` for `3*4`). The old
    cross-check accepted them as `ok~`; the instance-based check now flags them.
  * `pd` is wrong on every paper benchmark (13 MISMATCHes); this is the known
    `PD.Residualization` bug.
* *Residualizer failures:* "invocation of the undefined relation" on `rev@vanilla` (conspd-la),
  `binFactor` (conspd-branch) and `sortGeneral` (cpd-*).
* *Missing answers:* `tmInterp` / conspd-det loses the forward answer (`unverified-complete`).

**P8. The harness and tooling are slow on hard inputs.**
* A sweep with `--vanilla` takes hours: every timeout costs 60 s per specializer, and the
  exporter recomputes trees.
* The 60 s timeout relies on asynchronous exceptions, so it cannot interrupt a loop that never
  allocates.

### 6.4 Collected roadmap

Status: **done** = implemented and measured on this or an earlier branch; **started** =
partially implemented; **open** = proposed only. Priority (**A** highest) is set by how many of
the problems in §6.3 an item addresses.

| # | idea | addresses | source | status | prio |
|---|---|---|---|---|---|
| 1 | Measurement harness: cost model, cross-check, suite | – | §1 | done (`measure-harness`) | – |
| 2 | Soundness fixes in purification | P7 | §3.1 | done | – |
| 3 | Fix `PD.Residualization` (leaves paired with wrong definitions) | P7 | §3.1 | open | B |
| 4 | Find and fix the over-general answers of conspd-branch (`fclMult`) and the lost answer of conspd-det (`tmInterp`); run every specializer under the strengthened cross-check in CI | P7 | §6.3 | open | **A** |
| 5 | Total residualizer (no calls to undefined relations) | P7 | §5 item 3 | open | **A** |
| 6 | Budgets in global control and anytime specialization: cap the number of global nodes or driving steps and residualize what exists (generalize everything left); a specializer-internal step counter that replaces wall-clock timeouts | P4, P8 | §5 item 3 | open | **A** |
| 7 | **Scheduling as a mandatory pipeline stage** after every specializer. The greedy version exists (`Schedule.hs`). | P2 | §3.3, §5 item 1 | started | **A** |
| 7a | …driven by a determinism analysis (Mercury-style: keep semidet tests early, do not reorder past nondeterministic generators), which fixes the `queens5`/`regexp` regressions | P2 | §5 item 1 | open | **A** |
| 7b | …compared against, then unified with, the pre-existing `Mode.Analysis.prioritySelection` and `FunConversion.DetMode`; `Schedule.hs` was written independently and never compared with them | P2 | discussion on this branch | open | **A** |
| 7c | …with a cost estimate (Rozplokhas–Boulytchev, FLOPS'22) | P2 | §5 item 1 | open | B |
| 8 | **Termination-preserving residualization**: diagnose `power` q1; check that every residual recursion keeps a decreasing argument the original had (size-change graphs), and otherwise refuse the transformation | P3 | §6.3 | open | **A** |
| 9 | **Failure lookahead** in determinate unfolding (`cpd-la`, `conspd-la`) | P1 | §6.1 | done | – |
| 9a | Make lookahead the default local control. The sweep (§6.2) shows +15 solved for CPD, and none lost for ConsPD; first examine the 3 queries CPD loses (`binMul` q2, `coloring@vanilla`) and the `unifier@vanilla` regression | P1 | §6.2 | open | **A** |
| 10 | **BTA-informed online control**: never generalize arguments the BTA marks static (the object program inside an interpreter); allow unfolding when recursion descends structurally on static data (size-change termination, Leuschel–Vidal 2014) | P4, P5 | §6.1 | open, next | **A** |
| 11 | **Conjunction splitting at interpreter boundaries**: CPD carries the vanilla goal list (`jo_solve rest`) along in its conjunctions, which makes the whistle generalize object-level calls; split off continuation goals that share no variables with the selected atom | P4, P5 | `doubleAppend@vanilla` global tree | open | B |
| 12 | **Negative information**: disequality constraints (constrained PD / positive supercompilation with `≠`), then tabling / query–answer transformation for `path`-like problems; target: pass the KMP test | P6 | §5 item 6 | open | B |
| 13 | Finite-domain case splitting, characteristic trees for polyvariance control ("The Trick") | P5, P6 | §5 item 5 | open | B |
| 14 | **Cogen / generating extensions** (the second Futamura projection in practice): from BTA-annotated programs (`src/BTA`, `src/OfflinePD`), generate a program that maps static input directly to residual code, as LOGEN does for Prolog. Hybrid control: a generating extension for repeated specialization, online CPD/ConsPD for one-off runs. This also sidesteps P4 for interpreters. | P4, P8 | §6.1 | open | B |
| 15 | Specialization-cost benchmarks: driving-step counter (from item 6); break-even N = cogen time / (online − genext time) on many static inputs (`exprEval` over ~50 expressions, `regexp` over many patterns, `prop` over many formulas, `fclMult` over several programs) | P8 | §6.1 | open (wall-clock `spec_ms` only) | C |
| 16 | PostOpt on every residual (done); add RAF/FAR redundant-argument filtering | – | §3.2, §5 item 2 | PostOpt done, RAF/FAR open | C |
| 17 | Groundness/sharing/freeness (Pos) analysis feeding the BTA and the backend (drop the occurs check); a prerequisite for 7a and 10 | P2, P5 | §5 item 4 | open | B |
| 18 | Multi-result driver with an empirical cost model: pick the best residual per mode, using the harness's step counter as the cost model | P2, P3 | §5 item 7 | open | C |
| 19 | One core IR with lint, one analysis engine, a strategy-based specializer framework, DSL changes (`docs/DESIGN.md`, branch `design-proposals`); item 7b is a first concrete case of merging duplicate analyses | P7 | `docs/DESIGN.md` | `Core.Lint` prototype only | B |
| 20 | **Jones-optimality test**: every benchmark through the vanilla self-interpreter (`--vanilla`) | P1, P5 | §6.1 | done | – |
| 21 | Benchmarks from the literature (`bench/measure/papers`); still to add: Barliman-style relational interpreters run backward (quines), STLC inhabitation with polymorphism, Einstein/zebra (`src/TranslatedExamples`), 4×4 Sudoku, cyclic-graph reachability, bridge-and-torch and goat-wolf-cabbage with unknown move counts (`bench/Goat`, `bench/Hanoi`) | – | §5, §6.1 | 7 added, rest open | C |
| 22 | Visualizer of specialization trees, residuals and runs (`viz/`); next: highlight where the whistle fired and which generalization it caused (needed for 10/11) | P3, P5 | §6.1 | done; extension open | C |
| 23 | Learning-guided specialization (no prior work found) | – | §5 | open | C |

**Suggested order.**
1. Correctness first: items 4 and 5, so that later measurements can be trusted.
2. Then items 6 and 7/7a/7b: these are the largest measured wins (P2, P4).
3. Then item 8, which removes the `power` regression.
4. Then item 10, the step towards Jones optimality on two-layer interpreters (P5), with item 22
   to see why the whistle fires.
5. Items 12 and 14 are longer research projects.

## 7. Fixes, out-of-fuel analysis, and static analyses for the residual programs (branch `futamura`, continued)

All numbers: `--answers 10 --fuel 300000`, the 41 benchmarks and their `@vanilla` twins (136
queries), steps = unifications + calls. "Solved" means `Finished` or `GotEnough`. Every sweep
below has **no answer mismatch**. The comparisons are made with
`bench/measure/results/futamura/compare.py`.

### 7.1 Correctness fixes (roadmap item 5)

* **`split` took the wrong complement** (CPD, ConsPD and OfflinePD).
  * After the whistle, the best matching subconjunction `b` is generalized, and the rest of the
    goal was computed by `complementSubconjs b q'`. That function matches goals one at a time
    against the *generalized* `b`.
  * With repeated calls (two `splito`, two `sorto` in `sortGeneral`), it removed the wrong goals.
    The two parts then overlapped, and some goals were in neither. These goals could not be
    folded, hence "Residualization failed: invocation of an undefined relation".
  * Candidates now carry their positions, and the complement is taken by position.
* **ConsPD remembered conjunctions that fail outright.** Such a conjunction becomes a `Fail` node,
  which gets no definition, but it stayed in `seen`. A later instance of it was folded into a call
  to the missing definition (`depth@vanilla`, `deforestation1@vanilla`). Such goals are now
  unfolded again; they fail immediately.
* **Scheduler defects** (`Schedule.hs`):
  * success patterns were computed on the unscheduled order;
  * the fixpoint, made non-monotone by reordering, could settle in a pessimistic table; it now
    decreases from "all ground";
  * unifications were propagated only once.

  Together these kept producers after their consumers: in `relSortGeneral`, `minmaxo` ran
  before the recursive call that grounds its inputs.
* **Precise instance folding** (`cpd-lap`). CPD's local control folded `multiply 0 y 4` into
  `multiply x y 4`, which lost the static `0`. It now specializes an instance that differs only by
  ground bindings on its own. `PostOpt.failureAnalysis` then removes the loop without answers.

Sweep `results/futamura/v3` against the lookahead sweep of §6.2:

| configuration | solved before → after | steps (geometric mean, common queries) |
|---|---|---:|
| cpd-la+opt+sched | 98 → **106** | 0.875× |
| cpd-det+opt+sched | 90 → 97 | 0.981× |
| conspd-la+opt+sched | 58 → **74** (−1: `power` q2) | 1.017× |
| conspd-la | 52 → 64 | 1.000× |
| cpd-lap+opt+sched | – → 106 (`arith`: out of fuel → 10 steps) | 0.997× vs cpd-la+opt+sched |

### 7.2 Why queries run out of fuel, and what the ideal residual would be

| family | example | what happens | ideal program | what gets there |
|---|---|---|---|---|
| A. conjunct order wrong for the mode | `relSortGeneral` forward | `smallesto` runs `minmaxo h s'` with `s'` unbound, so nothing downstream is ground | selection sort in dataflow order: the recursive call first, then the comparison with both inputs ground | dataflow scheduling (§7.1): out of fuel → 436 steps |
| B. finite answers, but search goes on forever; static data known at specialization time | `arith`: `multiply x y 4` | the branch `multiply 0 _ 4` loops without answers | the table of the 3 answers | precise folding + failure analysis: 10 steps |
| C. as B, but the static data arrives only at run time | `arithGeneral` q2 | the specialization query is `multiply x y z` | as B, per query | run-time specialization / generating extensions (item 14); no static analysis helps |
| D. a bound that no unfolding sees | `revAcc` backward, `fclMult` q3 | the accumulator grows forever | the search cut at `len acc = len y` | size invariants as guards (§7.4, experiment 3) |
| E. generate-and-test order | `power` q2 under ConsPD, `coloring` | a call that enumerates runs before a test | tests and deterministic calls first | determinism-aware scheduling (§7.4, experiment 2) |
| F. reordering breaks termination | `binFactor@vanilla` q3, `power` q1 | the interpreter loop is reordered, so the object program's goals run right to left | the source order where termination depends on data | open: barriers in the scheduler fix `binFactor@vanilla` q3 but lose more elsewhere (§7.4, experiment 4); a termination proof per order (size-change, item 8) is needed |

### 7.3 Visualizer additions

* `viz/server.py` runs a program and queries entered in the page (**New benchmark**, or **Edit a
  copy…** of a suite benchmark) through `viz-export`.
* Specializations that time out show the tree as far as the specializer got.
* Variants show the tree of their base specializer.

On `contains@vanilla`, the partial trees show why ConsPD times out: 1760 global nodes in 20 s but
only about 1400 more in the next 100 s, with conjunctions of 21–23 goals. The cost per node grows
with the conjunction length. A likely cause, not yet profiled, is the subsequence enumeration
(2ⁿ) in `Embed.subconjs`, `split` and `generateSplits` (new roadmap item 27).

### 7.4 Static analyses on the residual programs

**Headroom.** Take the best CPD configuration on the 75 solved queries with more than 50 steps:

* in the median query, 24% of the steps are **failed unifications**;
* in 15 queries the share is 40–86%, almost all of them interpreters (`binFactor@vanilla` 82%,
  `unifier@vanilla` 73%, `typecheck@vanilla` 73%);
* the reason is that the vanilla interpreter's clause table survives specialization. A call scans
  about 45 clause heads, nearly all of which fail.

Each analysis is a separate pass and variant, measured against its baseline from the same build.

| # | analysis and transformation | variant | solved | steps (geometric mean) | largest effects |
|---|---|---|---|---:|---|
| 1 | **constructor analysis + constructor dispatch** (`src/Index.hs`): remove branches whose test can never succeed; group clauses that test the same argument against the same constructor (Mercury-style switch detection) | `+idx` | +1 (`binMul` q2), −0 | **0.909×** (cpd-la+opt), 0.913× (cpd-lap+opt+sched) | `binFactor@vanilla` 0.41×, `typecheck@vanilla` q2 0.45×, `unifier@vanilla` 0.47×, `regexp@vanilla` 0.57×; `typecheck` q1 1.37× (10 answers, in a different order) |
| 2 | **determinism in the scheduler**: switch-deterministic calls (at most one clause matches) and tests (all arguments ground) before producers that enumerate | `+sched` | original+sched 99 → 101, −0 | 0.955× (original+sched), 0.993× (cpd-la+opt+sched) | `coloring` q1 0.11×, q2 0.24×; `sortGeneral` q2 0.64× |
| 3 | **size invariants as guards** (`src/SizeGuard.hs`): infer `\|p_i\| ≤ \|p_j\|` and `\|p_i\| ≤ K` (list length, Peano value) as the greatest inductive set (difference constraints); check them before recursive calls whose bound is ground | `+guard` | +2 (`revAcc`, `fclMult` q3), −0 | 1.000× | `revAcc`: out of fuel → about 100 steps |
| 4 | **barriers at calls of unknown termination**: a call that terminates in no order keeps its source position; only producers of its inputs may overtake it | `+sched` | cpd-la+opt+sched 106 → 104 (+`binFactor@vanilla` q3; −`exprEvalGeneral` q2, `fclMult@vanilla` q1, `relSort@vanilla`) | 1.010×; original+sched 1.066× | **negative, not adopted** (`results/futamura/barrier`, `barrier.patch`): `coloring` q1 24.7× worse; source order blocks too many good reorderings |

Experiments 5–7 target the specializer's cost and the interpreter layer:

| # | change | where | result |
|---|---|---|---|
| 5 | greedy ordered-subconjunction embedding in the whistle (exact, O(n·m) instead of C(n, k)) | `Embed.subseqMatch` | residuals identical; specialization 0.82–0.91×; timeouts unchanged (`results/futamura/embed`) |
| 5b | instance check stops at the first mismatch (profiling: 65% of ConsPD's time on `contains@vanilla` was `length` inside it) | `Embed.instList` | residuals identical; ConsPD 10× more nodes per second (`results/futamura/inst`) |
| 6 | budget for global control (ConsPD 200, CPD 2000 processed conjunctions; then most general calls), plus `subsequencesOfLength` (residualization no longer 2ⁿ in the conjunction length) and a range-restricted instance check | `CPD/ConsPD.GlobalControl`, `Util.Miscellaneous` | residuals identical wherever both finish; conspd-det timeouts 26 → 20, `contains@vanilla` and `uptoSum@vanilla` now solved (`results/futamura/budget`) |
| 7 | **constructor specialization** (`+spc`, `src/SpecConstr.hs`): a call `f … (C us) …` at a dispatch position of `f` becomes `f_C … us …`; with `PostOpt` and one unfolding per round of single-clause specializations, this unrolls `jo_solve` over the literal goal lists and calls the object relations' clauses directly | `+spc` | steps **0.528×** (cpd-la+opt), **0.612×** with scheduling (solved 106 → 114); `unifier@vanilla` 42129 → 453, `binFactor@vanilla` 14648 → 160, `typecheck@vanilla` 11164 → 284 (the direct program: 273); no mismatch (`results/futamura/spc`) |

**Jones optimality after experiment 7** (`results/futamura/spc/summary-full.md`; compare §6.2):

| specializer | twin queries solved | vs direct | vs S(p) (overhead left) | optimal |
|---|---:|---:|---:|---:|
| cpd-la+opt | 42/63 | 1.04× | 6.65× | 11 |
| cpd-la+opt+spc | 43/63 | 0.27× | **1.90×** | 15 |
| cpd-lap+opt+sched+guard+idx | 48/60 | 0.73× | 5.00× | 10 |
| cpd-lap+opt+spc+sched+guard+idx | **55/60** | **0.26×** | **1.88×** | **22** |

All together, `cpd-lap+opt+spc+sched+guard+idx` against the unspecialized program
(`results/futamura/spc`): it solves **115 of 136** queries (original: 90) with **0.039×** the
steps on the 86 both solve, and loses 4 (`exDepth`, `power` q1, `sortGeneral` q1,
`tmInterp@vanilla` q1). Before experiment 7, `cpd-lap+opt+sched+guard+idx`
(`results/futamura/guard`):

* it solves **108 of 136** queries, against 90 for the original;
* it takes **0.064×** the steps on the 85 queries both solve;
* it loses 5 queries the original solves: `binFactor@vanilla` q3, `exDepth`, `power` q1,
  `sortGeneral` q1 and `tmInterp@vanilla` q1 (family F above; experiment 4 did not solve it).

**Where static analysis does and does not help.**

* It helps most on **interpreters**, whose clause table survives specialization (experiment 1):
  the dispatch is decided at the residual level instead of at run time.
* It helps on **generate-and-test** programs (experiment 2) and on **backward runs with size
  bounds** (experiment 3).
* It does not help when the static data exists only at run time (family C), or on programs that
  are already close to one step per answer element (`doubleAppend`).
* Analyses that only change what a step costs, such as groundness, sharing and freeness to drop the
  occurs check, show no gain in this step metric. They need wall-clock measurements on the OCanren
  backend.

### 7.5 Roadmap additions (towards larger interpreters and compilers)

| # | idea | why | prio |
|---|---|---|---|
| 24 | (partly done by experiment 7, as a residual pass) **Dispatch inside the specializer**: use the constructor analysis during global control, so a goal with a known constructor is specialized against only the matching clauses, instead of indexing the residual afterwards. Then generalization no longer turns an object program into a run-time clause table. | experiment 1 removes at most the head tests (≤ 2.4×); keeping the dispatch static removes the interpreter layer (P5) | **A** |
| 25 | **Abstract multiple specialization** (CiaoPP): one abstract-interpretation engine (constructor sets, groundness, size relations, determinism) driving the creation of polyvariant versions and the removal of tests, as the shared basis of `Schedule`, `Index` and `SizeGuard` | the three passes each recompute part of the same information | **A** |
| 26 | Cardinality / number-of-solutions analysis (Debray–Lin) in the scheduler, beyond switch determinism | `power` q2 under ConsPD: no input is ground at the top-level call, so determinism cannot decide | B |
| 27 | (done: experiments 5, 5b, 6) Ordered-subconjunction embedding in O(n·m) instead of enumerating subsequences; record the fold of every leaf in the global tree, so the residualizer does not search for it | ConsPD/CPD time on long conjunctions (§7.3); residualization becomes total by construction | **A** |
| 28 | Size invariants beyond pairs (polyhedra, Benoy–King) and conditional invariants (`mul x y z` with `y > 0`), for `power` q3 and `fclMult` q2 | experiment 3 covers only pairwise bounds | B |
| 29 | Check that every run-time query is an instance of the specialization query; flag it in the harness and the visualizer | user-entered queries in the visualizer silently produced no answers | C |

**Suggested next steps.** Items 6 (budgets) and 27 make ConsPD usable on interpreters. Then items
24 and 10 make specialization keep the object program static, which is what larger interpreters
and compilers need. Item 25 is the architecture the analyses of §7.4 should be merged into.
