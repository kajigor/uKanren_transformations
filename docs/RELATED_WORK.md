# Related work: specialization, analysis and execution of miniKanren programs

This list collects every work found during the survey behind `bench/measure/REPORT.md` and
`docs/DESIGN.md` (branch `design-proposals`). For each entry it gives what the work contributes
and where it touches this repository.

**Provenance markers.**

* **[V]** means the entry was verified during the survey, either in the `kajigor/tex` sources or
  on the linked page.
* **[BK]** means the entry comes from background knowledge and was not re-checked. Check the
  venue and year before citing it.

Contents:

1. The group's own papers (`kajigor/tex`)
2. Recent miniKanren and relational-programming work, 2020–2026
3. Classic partial deduction and specialization of logic programs
4. Program analyses for logic programs
5. Supercompilation, cost models and other specializers
6. Learning-guided specialization: a negative result
7. Where each work maps onto the proposals

---

## 1. The group's own papers (`kajigor/tex`) [V]

These are in chronological order. The "future work" column quotes or paraphrases each paper's
own stated open problems. These are the directions the repository is expected to move in.

| # | Work | Contribution | Stated limitations / future work |
|---|---|---|---|
| G1 | Lozov, Verbitskaia, Boulytchev. *Relational Interpreters for Search Problems.* miniKanren 2019. [page](https://icfp19.sigplan.org/details/minikanren-2019-papers/3/Relational-Interpreters-for-Search-Problems) | Verifier-to-solver: a functional verifier is translated to a relation, and CPD/supercompilation removes the interpretation overhead. | "No single good strategy"; backward execution is still slow. |
| G2 | Verbitskaia, Artemeva, Berezun. *Binding-Time Analysis for miniKanren.* TEASE-LP 2020 (plus a Russian SEIM-2020 version). | The first BTA for miniKanren. It computes the order in which variables get bound for a given direction. | All directions of a call are enumerated exhaustively when a variable occurs only in calls. Integration with the functional translator is planned. |
| G3 | Verbitskaia, Berezun, Boulytchev. *An Empirical Study of Partial Deduction for miniKanren.* miniKanren 2020; VPT 2021, EPTCS 341. [arXiv 2109.02814](https://arxiv.org/abs/2109.02814) | Introduces **ConsPD**. It joins unfolded calls back under a "less-branching" heuristic, and is compared with ECCE on `evalo` variants and typecheckers. ConsPD is stable under reordering, while ECCE slows down all four `evalo` versions. | Wants a **cost/performance model**, **hybrid offline+online PE**, a study of how PE changes the interleaving order, more deforestation/tupling, and a larger benchmark set. |
| G4 | *On a Direction-Driven Functional Conversion.* miniKanren 2022 (draft). | Converts a relation with a fixed direction into functional code, by hand. | **Conjunct ordering is the top priority.** Mode analysis must take ordering into account. Also: formalize the conversion and integrate it. |
| G5 | Verbitskaia, Engel, Berezun. *Semi-Automated Direction-Driven Functional Conversion.* miniKanren 2023. | In/out mode inference plus an automated conversion. | Add a **determinism check** to mode analysis; integrate with PD. |
| G6 | Verbitskaia, Engel, Berezun. *A Case Study in Functional Conversion and Mode Inference in miniKanren.* PEPM 2024, pp. 107–118. [page](https://popl24.sigplan.org/details/pepm-2024/7/A-Case-Study-in-Functional-Conversion-and-Mode-Inference-in-miniKanren) | Heuristic mode inference. Speed-ups come from replacing unification with matching and from cheap-goals-first ordering; deterministic directions get a cheaper monad. | It is unclear whether the heuristics generalize; determinism should *justify* the ordering. Suggests specializing `evalo` on its two-valued output. |
| G7 | Shefer, Verbitskaia. *Integration of Offline Partial Deduction and Functional Conversion for miniKanren.* HCVS 2024 (extended abstract). [PDF](https://www.sci.unich.it/hcvs24/papers/HCVS2024_paper_8.pdf) | Online PD gives "extremely large" programs. The paper instead uses a pipeline: modes, then reordering, then BTA with a termination check, then offline PD, then Haskell. Reported speed-ups are 10³–10⁵× on `sorto` and Hanoi. | A limited evaluation. A more precise BTA would allow more complete unfolding. |
| G8 | Verbitskaia. *Towards Efficient Search: Leveraging Relational Interpreters and Partial Deduction Techniques.* PhD proposal, 2024. | Objectives: PD for relational interpreters, functional conversion, integrating the two, and a benchmark suite. | The thesis is still a skeleton. |
| G9 | Engel, Verbitskaia. *Typed Embedding of miniKanren for Functional Conversion.* miniKanren 2025. [arXiv 2511.00740](https://arxiv.org/abs/2511.00740) | A typed tagless-final Haskell embedding. It drops the monolithic `Term` type, adds automatic determinism analysis and removes manual annotations. | Remaining overheads are the **occurs check** and **EDSL interpretation**. Proposes combining PD and conversion. |

Two sets of background slides in the same repo are relevant:

* *Mode System in Mercury* (2022): insts, reordering and mode inference.
* *Tupling via Constructive Algorithmics* (2021): eureka tuples and the mutu theorem.

## 2. Recent miniKanren and relational-programming work, 2020–2026 [V]

### Semantics and cost

* Rozplokhas, Vyatkin, Boulytchev. *Certified Semantics for Relational Programming.* APLAS 2020,
  LNCS 12470. [arXiv 2005.01018](https://arxiv.org/abs/2005.01018). Gives Coq-certified
  denotational and interleaving operational semantics. The harness interpreter
  (`src/Measure/Interp.hs`) follows the same interleaving discipline.
* Rozplokhas, Boulytchev. *Scheduling Complexity of Interleaving Search.* FLOPS 2022.
  [arXiv 2202.08511](https://arxiv.org/abs/2202.08511). Gives cost recurrences over
  disjunction/conjunction search states. **It is the natural basis for the static cost model**
  that G3 asks for, and for scoring conjunct orders in `Schedule`.
* Pfingsten, Hemann. Small-step semantics for miniKanren. miniKanren 2025.

### miniKanren 2024 — proceedings ed. Verbitskaia & Near, [arXiv 2409.06505](https://arxiv.org/html/2409.06505/)

* Domoratskiy, Boulytchev. *A Relational Solver for Constraint-based Type Inference.* This is a
  candidate hard benchmark.
* Kudasov, Starikov. *typedKanren*: a typed embedding, related to G9 and to the ADT proposal in
  DESIGN.md §4.1.
* The stableKanren papers (stable-model semantics and negation in miniKanren).
* *Six Ways to Implement Divisibility by Three.* A small benchmark family that shows how strongly
  relation formulation affects search.
* Donahue. *Relational Reactive Programming.*

### miniKanren 2025 — [program](https://conf.researchr.org/home/minikanren-2025)

* Domoratskiy, Kosarev, Boulytchev. *An Empirical Study of Rational Tree Unification for
  miniKanren.* This is one way to remove the occurs-check cost that G9 names.
* Volkov, Yang, Shan. Semiring arrays and SAT solving in miniKanren.
* Sanna, Byrd, Amin. *Beyond Cons.*
* *Walrus*; *The CoCompiler*.
* Engel, Verbitskaia (G9).

### miniKanren 2026 — [program](https://icfp26.sigplan.org/home/minikanren-2026)

* Domoratskiy, Boulytchev. *Efficient Rational Unification.*
* Kudasov. *Towards Bottom-Up Enumeration in miniKanren via Pruning and Memoization.* This is
  related to the tabling and query–answer direction (REPORT §5, item 6) for `path`-like problems.
* Volkov, Shan, Yang. *Compiling Polymorphic Relations without Monomorphization.*
  [arXiv 2607.24678](https://arxiv.org/abs/2607.24678). Relevant to typed residual code and to
  polymorphic ADTs in the DSL.

### Constraints

* Sanna, Byrd, Amin. *chrKanren: Constraint Handling Rules in a Relational Language.* ICLP 2026,
  EPTCS 450. [arXiv 2607.21204](https://arxiv.org/abs/2607.21204). A host for disequality and
  domain constraints, which constrained PD (DESIGN.md §4.5) would need to residualize.

## 3. Classic partial deduction and specialization of logic programs

* **[BK]** De Schreye, Glück, Jørgensen, Leuschel, Martens, Sørensen. *Conjunctive Partial
  Deduction: Foundations, Control, Algorithms and Experiments.* JLP 41, 1999. This is the basis
  of `src/CPD`.
* **[BK]** Leuschel, Sørensen. *Redundant Argument Filtering of Logic Programs.* LOPSTR 1996.
  It covers RAF and the reverse variant FAR. `Purification.removeRedundantArgs` is a form of it,
  and REPORT §5 item 2 proposes running it after `PostOpt`.
* **[BK]** Leuschel, Martens, De Schreye. *Controlling Generalization and Polyvariance in Partial
  Deduction of Normal Logic Programs.* TOPLAS 20(1), 1998. It introduces characteristic trees.
* **[BK]** Leuschel, De Schreye. *Constrained Partial Deduction and the Preservation of
  Characteristic Trees.* New Generation Computing, about 1998.
* **[BK]** Leuschel. *On the Power of Homeomorphic Embedding for Online Termination.* SAS 1998.
  This is the whistle used by the online specializers here.
* **[BK]** Bruynooghe, Leuschel, Sagonas. *A Polyvariant Binding-Time Analysis for Off-line
  Partial Deduction.* ESOP 1998.
* **[BK]** Craig, Gallagher, Leuschel, Henriksen. *Fully Automatic Binding-Time Analysis for
  Prolog.* LOPSTR 2004. It uses norms, convex-hull size analysis and filter propagation, and is a
  template for a more precise BTA than G2.
* **[BK]** Leuschel, Jørgensen, Vanhoof, Bruynooghe. *Offline Specialisation in Prolog Using a
  Hand-Written Compiler Generator.* TPLP 4(1–2), 2004. This is LOGEN.
* **[V]** Vidal. *Quasi-Terminating Logic Programs for Ensuring the Termination of Partial
  Evaluation.* PEPM 2007.
* **[V]** Leuschel, Tamarit, Vidal. *Improving Size-Change Analysis in Offline Partial
  Evaluation.* 2009. [arXiv 0903.2202](https://arxiv.org/abs/0903.2202)
* **[V]** Leuschel, Vidal. *Fast Offline Partial Evaluation of Logic Programs.* Information and
  Computation 235, 2014. Size-change-based BTA, a replacement candidate for
  `BTA/TerminationCheck`.
* **[V]** Kafle, Gallagher. Constraint specialisation of Horn clauses via the **query–answer
  transformation** (PEPM 2015; SCP). This is a mode-free magic-set transformation, and is the
  formulation suggested for the analysis engine in DESIGN.md §2.
* **[V]** De Angelis, Fioravanti, Gallagher, Hermenegildo, Pettorossi, Proietti. *Analysis and
  Transformation of Constrained Horn Clauses for Program Verification.* TPLP survey (arXiv 2021).
  [arXiv 2108.00739](https://arxiv.org/abs/2108.00739)
* **[V]** Puebla, Albert, Hermenegildo. *A Generic Framework for the Analysis and
  Specialization of Logic Programs* (abstract interpretation combined with PD, CiaoPP). [arXiv cs/0508111](https://arxiv.org/abs/cs/0508111)
* **[V]** Hanus, Peemöller. A partial evaluator for Curry. WFLP 2014.
* **[V, negative]** No 2022–2025 papers on ECCE or LOGEN, and no Curry PE papers, were found.
  Classic logic-program PE has been quiet recently.

DPPD benchmark suite (Leuschel): <https://github.com/leuschel/DPPD>. It is the source of
`bench/measure/dppd`.

## 4. Program analyses for logic programs [BK]

* Marriott, Søndergaard. *Precise and Efficient Groundness Analysis for Logic Programs.* LOPLAS
  1993. This is the Pos domain, a more precise replacement for `Schedule.successTable`'s sets of
  ground positions.
* Jacobs, Langen (1992) and Muthukumar, Hermenegildo (1991–92): set-sharing and
  sharing+freeness. These would let the backend drop the occurs check and turn unification into
  construction or matching, which is the overhead G9 names.
* Debray, Warren. *Automatic Mode Inference for Logic Programs.* JLP 1988.
* Somogyi, Henderson, Conway. *The Execution Algorithm of Mercury.* JLP 1996. Also Henderson et
  al. on determinism analysis, about 1996. This is the det/semidet/multi/nondet domain proposed
  for scheduling.
* Overton, Somogyi, Stuckey. *Constraint-Based Mode Analysis of Mercury.* PPDP 2002.
* Lee, Jones, Ben-Amram. *The Size-Change Principle for Program Termination.* POPL 2001. This
  would replace the structural check in `Schedule.terminates`.
* Bulwahn. Smart test-data generators with mode-driven reordering (Isabelle/Quickcheck). It is
  cited by G6.

## 5. Supercompilation, cost models and other specializers

* **[BK]** Klyuchnikov, Romanenko. Multi-result supercompilation, about 2012. It is the model for
  the multi-result driver in DESIGN.md §3.
* **[BK]** Vidal. Cost-augmented partial evaluation of functional logic programs. HOSC, about
  2004. It is cited by G3 as the cost model to follow.
* **[V]** Fallin, Bernstein. *weval* (partial evaluation of interpreters in WebAssembly). PLDI
  2025. [arXiv 2411.10559](https://arxiv.org/abs/2411.10559). It is not learning-based, but it is
  a recent practical take on interpreter specialization, the same problem as `exprEval` and
  `evalo`.

## 6. Learning-guided specialization [V, negative result]

Targeted searches found no work on learned or LLM-guided unfolding, generalization or
conjunct-ordering control for partial deduction or supercompilation. The empirical step counter
in `Measure.Runner` could supply training signal or a selector for a multi-result driver. This
looks like an open niche.

## 7. Where each work maps onto the proposals

| proposal (REPORT §5 / DESIGN) | works |
|---|---|
| Mode/determinism-driven conjunct scheduling (`Schedule`) | G4, G5, G6, G9; Rozplokhas–Boulytchev FLOPS'22; Mercury det analysis; Debray–Warren; Bulwahn |
| Post-specialization optimizer (`PostOpt`), RAF/FAR | G7 ("extremely large programs"); Leuschel–Sørensen RAF/FAR; ECCE post-processing |
| Total residualizer, linting, soundness | CPD foundations (De Schreye et al. 1999); Rozplokhas et al. APLAS'20 (reference semantics) |
| More precise BTA, termination | G2, G7; Craig et al. 2004; Bruynooghe–Leuschel–Sagonas 1998; Vidal 2007; Leuschel–Tamarit–Vidal 2009; Leuschel–Vidal 2014; Lee–Jones–Ben-Amram 2001 |
| A single abstract-interpretation engine | Kafle–Gallagher query–answer; CiaoPP (Puebla et al.); Pos (Marriott–Søndergaard); sharing+freeness; De Angelis et al. 2021 |
| Finite-domain splitting, characteristic trees | G6 (`evalo` on a boolean output); Leuschel–Martens–De Schreye 1998 |
| Constrained PD, disequality, constraints | Leuschel–De Schreye ~1998; chrKanren (ICLP'26) |
| Tabling / bottom-up for `path`-like problems | Kudasov (miniKanren'26); Kafle–Gallagher |
| Cost model and multi-result driver | G3; Vidal cost-augmented PE; Rozplokhas–Boulytchev; Klyuchnikov–Romanenko |
| Removing occurs-check / EDSL overhead in the backend | G9; rational-tree unification (Domoratskiy et al. '25, '26); sharing+freeness analysis |
| Typed DSL (ADTs, polymorphism) | G9; typedKanren; Volkov–Shan–Yang (miniKanren'26) |
| Harder benchmarks | G1 (verifier-to-solver), constraint-based type inference (miniKanren'24), divisibility-by-three (miniKanren'24), DPPD suite |
