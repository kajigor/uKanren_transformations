# Design proposals: analyses, specializers and the DSL

These proposals come from working with the code while building the measurement harness
(`measure-harness`), the soundness fixes (`fix-residual-soundness`) and the post-specialization
passes (`post-spec-opt`). Each problem is tied to a concrete observation in the code or in the
measurements. This branch also contains one prototype, `src/Core/Lint.hs`, which demonstrates
proposal 1.

## 0. What the current design looks like

| concern | current state |
|---|---|
| goal representations | `Syntax.G` (with `Delay`; `success`/`fail` encoded as magic nullary `Invoke`s), `BTA.InvokeAnnotation.AnnG` (a copy of `G` with an annotation per call), `Mode.Syntax.Goal`, `Mode.NormSyntax` (flat DNF), `FunConversion.Lang` and `FunConversion.Updated`. Each comes with its own traversals, substitution and free-variable code. |
| variables | two namespaces: `X = String` (syntactic) and `S = Int` (semantic). Fresh names come from several ad-hoc generators (`FreshNames`, purification's `y<n>`/`q<n>`, residualization's `x<n>`). No representation guarantees well-scopedness. |
| specializers | `PD`, `CPD`, `ConsPD`, `ConsPDGlobal`, `OfflinePD`. `OfflinePD` duplicates CPD's local and global control over `AnnG` and converts its trees back (`transformSld`, `transformGlobal`). ConsPD has its own `Unfold`, `Split`, residualization and state. About 3.1k lines in `src/{PD,CPD,ConsPD,OfflinePD}` alone. |
| analyses | Mode analysis (`Mode/*`), BTA with termination check (`BTA/*`), determinism (`FunConversion/DetCheck`, `DetMode`), and now `PostOpt`'s failure analysis and `Schedule`'s groundness/termination. Each implements its own fixpoint over its own IR. |
| residual clean-up | `Purification`: redundant-argument erasure and link removal, mixed with error recovery (`clearGoal`). |
| front end | untyped constructors, positional relations, `?` query, separate `.ann.mk` files with `filter (static dynamic)` and `Unfold`/`Memo` annotations, two parsers (Simple, Irina). |

## 1. One core IR with checked invariants (prototype: `Core.Lint`)

**Observation.** Both soundness bugs fixed in `fix-residual-soundness` were invariant violations
that no component noticed.

* Residualization emitted calls to relations it never defined. `clearGoal` "repaired" this by
  deleting the calls, which was unsound.
* `isSuccess (Invoke successName [])` bound the constant as a pattern variable.

The prototype linter, applied at the residualization boundary (`cpd-det/raw` etc. in the harness),
flags exactly the affected benchmarks:

```
matchSimple  cpd-det/raw     1 error  (___match1: call to undefined relation ____match1)
path         cpd-det/raw     9 errors (_eq_pairElem: call to undefined relation ___elem)
relSort      cpd-det/raw     4 errors; cpd-branch/raw 39 errors
```

The likely root cause is `CPD.GlobalControl.getNodes`, which skips nodes whose local tree is an
immediate `Leaf` ("This happens because of the instance check"). Leaves elsewhere still refer to
those nodes. All final residual programs are lint-clean after the fix. The only remaining warning
is binder shadowing in the source of `rotatePrune`.

**Proposal.**

* Introduce one core IR, `Core`, and make every pass `Core → Core`:
  * relations with explicit parameters;
  * bodies in a normal form: a disjunction of `∃ x̄. conj` clauses whose atoms are `x = C(ȳ)`,
    `x = y`, `p(x̄)`, `success` and `fail`;
  * unique binder ids, with `Unique`/`Int` names and pretty names kept as metadata.

  `Mode.NormSyntax` already comes close to this; generalize it and retire the other goal types.
* The surface syntax (`Syntax.G` with nested terms) is desugared once by flattening. Every
  backend (OCanren, Haskell functional conversion) reads `Core`.
* Annotations become *data on Core*, not a copy of the IR:
  * BTA results as `M.Map CallSite Ann`;
  * modes as `M.Map (Rel, Mode) ...`.

  This removes `AnnG` and the tree converters in `Transformer/OfflinePD.hs`.
* Run `Core.Lint` after every pass in tests and debug builds, in the spirit of GHC's `-dcore-lint`.
  Add the answer cross-check from `Measure.Runner` as a hedgehog property: generate random
  instances of the benchmark queries and compare answer sets.

## 2. One abstract-interpretation engine for all analyses

**Observation.** Mode inference, BTA, determinism checking, the may-succeed analysis
(`PostOpt.failureAnalysis`), groundness success patterns and termination
(`Schedule.successTable`, `Schedule.terminates`) all share the same shape. Each one computes a
polyvariant table *(relation, call pattern) ↦ success pattern* by fixpoint iteration over clause
bodies. Every implementation is separate and was written against a different IR.

**Proposal.**

* Write a single engine over `Core`, parameterized by an abstract domain:

  ```haskell
  class Lattice d => Domain d where
    unifyA   :: Var -> Atom -> d -> d        -- x = C(ys) / x = y
    callA    :: Rel -> [Var] -> d -> CallPat d -- project to a call pattern
    returnA  :: Rel -> [Var] -> SuccPat d -> d -> d
    joinClauses :: [d] -> d
  ```

  Then `analyse :: Domain d => Program -> Entry d -> Table d` does the top-down call-pattern
  exploration plus the success-pattern fixpoint, i.e. the "query–answer" (magic) formulation of
  Kafle and Gallagher.
* Instances:
  * groundness (the Pos domain, which is more precise than the current set-of-ground-positions);
  * freeness/sharing (lets the backend drop the occurs check and replace unification by
    construction or matching);
  * determinism (Mercury-style det/semidet/multi/nondet);
  * may-succeed / type-based failure;
  * size or norms for termination (size-change graphs instead of the current structural check);
  * finite-domain detection.
* Consumers read the shared tables:
  * BTA reads groundness plus termination;
  * scheduling reads groundness, determinism and termination;
  * functional conversion reads modes and determinism;
  * PostOpt reads may-succeed.

  The *product* of domains gives the mode/determinism/termination information that HCVS'24 and
  PEPM'24 list as future work, and computes it consistently.
* Make analysis results first-class artifacts: print them, diff them in tests, and attach them to
  residual programs, so that a specializer can re-analyse its output. `Schedule` effectively does
  this already, per run mode.

## 3. A specializer framework instead of five specializers

**Observation.**

* `CPD`, `ConsPD`, `ConsPDGlobal` and `OfflinePD` differ in a handful of decisions but are
  separate code bases.
* The measurements show that none of them dominates:
  * `conspd-*` is the most robust;
  * `cpd-branch` times out on `typecheck` and `unifier`;
  * every online specializer times out on `queens5`;
  * `cpd-det` and `cpd-branch` crash on `sortGeneral`.
* No specializer controls its cost. A 60 s timeout is the only guard.

**Proposal.** A single driver parameterized by a strategy record:

```haskell
data Strategy = Strategy
  { unfoldRule   :: LocalState -> Maybe Selection   -- det / branching / ConsPD join / BTA oracle
  , whistle      :: History -> Conj -> Bool         -- embedding, size-change, budget
  , abstraction  :: History -> Conj -> [Conj]       -- split + msg, characteristic trees
  , budget       :: Budget                          -- fuel for local and global control
  }
```

* Offline PD becomes `unfoldRule = oracle (bta annotations)`, and hybrid PD becomes `oracle` with
  an online fallback. The VPT'21 paper's "hybrid offline+online" future work then needs no new
  specializer.
* The residualizer is shared and must be total. Every leaf either maps to a generated definition
  or the driver fails loudly. This fixes the bug in section 1 by construction.
* Budgets make specialization anytime: on exhaustion, stop unfolding and generalize to the
  original call. `queens5` then degrades to "no speed-up" instead of "no program".
* Post-processing is a fixed, analysis-driven pipeline applied to every specializer's output:

  `PostOpt (failure, unification elimination, inlining, duplicate merging) → RAF/FAR →
  per-mode Schedule → backend`

  The measurements support this: `+opt` reduces steps by 20–35 % relative to the base
  specializer for every base specializer, and `+sched` turns 16 unsolved (benchmark, specializer)
  cases into solved ones.
* Multi-result driving. Since strategies are values, run several (for example `conspd-det` and
  `cpd-branch+opt`), estimate each residual's cost, and keep the best. The estimate can be static
  (the Rozplokhas–Boulytchev scheduling complexity) or empirical (the harness's step counter on
  sample queries). This is the cost model the VPT'21 paper asks for.

## 4. The DSL

The current surface language is untyped, positional and annotation-poor. Analyses and
specializers therefore have to re-derive what the programmer knows, and sometimes get it wrong:
the BTA's exhaustive enumeration of directions is one example, conjunct ordering is another.

1. **Algebraic data types.**
   ```
   type nat  = O | S nat
   type list a = Nil | Cons a (list a)
   type bool = Trueo | Falso
   ```
   * Enables type-based failure analysis: a unification of different types is a static failure.
   * Enables finite-domain detection: `bool` arguments allow case-splitting, as in the PEPM'24
     suggestion to specialize `evalo` on its output.
   * Enables typed functional conversion without a monolithic `Term` type (Engel & Verbitskaia,
     miniKanren'25).
   * Enables smaller `Core` terms, because constructors get arity checks. `Core.Lint` would then
     also check constructor arity.
2. **Mode and determinism declarations, optional and checked.**
   ```
   rel appendo(list a, list a, list a)
     mode (in, in, out) is det
     mode (out, out, in) is multi
   ```
   * The analysis engine (section 2) checks declared modes and infers the missing ones.
   * Declared modes replace `--ground=[..]` on the command line and the `filter` lines in
     `.ann.mk`.
   * Scheduling and functional conversion get a contract instead of a heuristic.
3. **Pragmas instead of separate annotation files.** `@unfold`, `@memo`, `@inline`, `@tabled`,
   `@budget(n)` attach to calls or relations in the source. The `.ann.mk` files, which duplicate
   whole programs (`propFirst.ann.mk` vs `propFirst.mk`), go away. BTA *suggests* annotations, and
   the user can pin them.
4. **Clausal syntax with head patterns**, alongside the current goal syntax:
   ```
   appendo [] ys ys.
   appendo (h :: t) ys (h :: r) :- appendo t ys r.
   ```
   Head unifications become explicit. Clause indexing, determinism detection ("clauses with
   distinct head constructors on an `in` argument") and functional conversion to `case` become
   syntactic.
5. **First-class `success`/`fail` and disequality.** `success`, `fail` and `x =/= t` become
   keywords. The parser no longer injects `fail = fail`, a definition that loops in
   `Eval`. Disequality lets PD express negative information from failed unification branches
   (constrained PD), and conversion maps it to guards for free.
6. **Specialization and run queries in the language.** Generalize the harness's `.mk` + `.runs`
   convention:
   ```
   specialize sorto(x, y) as sort_any
   run sort_any([S O, O, S (S O)], y)        -- expect: y = [O, S O, S (S O)]
   run sort_any(x, [O, S O])  take 10
   ```
   * Examples double as regression tests with expected answers.
   * The cross-check in `Measure.Runner` becomes the language's test runner.
7. **Functions and pattern-matching sugar desugared to relations.** The verifier-to-solver
   approach (miniKanren'19) starts from functional verifiers. A `fun` construct, e.g.
   `fun leq : nat -> nat -> bool`, would be translated by the existing relational conversion and
   would make the generated relations' modes *known*: a function's inputs are `in`. This removes
   much of the mode-inference guesswork for exactly the benchmarks the group cares about.

## 5. Suggested order

1. `Core` IR, `Core.Lint` in tests, and a total residualizer. This fixes the class of bugs found
   in this round.
2. The analysis engine with groundness, determinism and may-succeed. Port `Schedule` and
   `PostOpt`'s analyses first, since they are small and already measured.
3. The strategy-parameterized driver, with budgets.
4. ADTs and mode declarations in the front end, then pragmas, then `specialize`/`run` blocks.
5. A cost model and the multi-result driver.
