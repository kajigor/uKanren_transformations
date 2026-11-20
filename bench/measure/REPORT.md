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

Effect on the earlier sweep, which ran before the fix:

| specializer | answer-set mismatches before → after |
|---|---|
| cpd-det | 3 → 0 |
| cpd-branch | 1 → 0 |
| conspd-det | 3 → 0 |
| conspd-branch | 1 → 0 |

The affected benchmarks were `matchSimple` and `path`.

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

* **PostOpt never changes which queries are solved.** It cuts steps by 20–36 % over CPD and
  ConsPD, and by 57 % over PD.
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

The design-level proposals (one core IR with linting, one analysis engine, a strategy-based
specializer driver, DSL changes) are in `docs/DESIGN.md` on branch `design-proposals`.
