# `bench/measure/papers`: interpreter specialization and classic PE tests

Benchmarks taken from (or written to follow) well-known papers. Most are *first Futamura
projection* tests: an interpreter specialized to a fixed object program. They measure how much
interpretive overhead the specializers remove. The full citation, what is static, and the ideal
residual are in each `<name>.mk` header. Format and methodology: `bench/measure/REPORT.md`, §1.

Numbers are Peano (`O`, `S n`) except in `binMul`/`binFactor`, which use little-endian bit lists
(`B0`, `B1`). There are no disequality constraints. Where a source program needs `≠` on a finite
alphabet, it is written as an enumerated relation (`neq`, `neqsym`).

| name | source paper | static input | run-time queries | ideal specializer output |
|---|---|---|---|---|
| `tmInterp` | Jones, Gomard, Sestoft, *Partial Evaluation and Automatic Program Generation*, Prentice Hall 1993, Ch. 4 (Turing machine interpreter compiled by mix) | TM program `0: if 0 goto 3; 1: right; 2: goto 0; 3: write 1` | q1 forward on a 10-symbol tape; q2 backward: which inputs give tape `111` (3 answers, then the search diverges); q3 partially known input `[x,y,z,1]` | a single scan loop (move right while the head is not `0`, then write `1`), with no instruction list, no `Goto`/`If` dispatch and no `newtail` |
| `fclMult` | Jones, Gomard, Sestoft 1993, Ch. 4 (flow chart language and mix) | FCL program for multiplication (6 basic blocks, store `[x,y,z,t]`) | q1 forward `3*4`; q2 backward `x*y = 6`; q3 division `3*y = 6` | two mutually recursive relations, one per loop header (`test`, `inner`), over plain Peano arguments; no block lookup, store list or expression dispatch |
| `kmp` | Consel & Danvy, *Partial evaluation of pattern matching in strings*, IPL 30(2) 1989; Sørensen, Glück, Jones, *A positive supercompiler*, JFP 6(6) 1996 | pattern `[A,A,B,A,B]` for the naive `loop`/`next` matcher | q1 30-symbol text, match at the end; q2 20-symbol text without a match (finite failure); q3 backward: texts of length 7; q4 partially known text | a KMP-like automaton: one relation per pattern prefix that never re-scans the text, and no pattern list left in the residual |
| `binMul` | Kiselyov, Byrd, Friedman, Shan, *Pure, declarative, and constructive arithmetic relations*, FLOPS 2008 | `n = 5` in `multo n m p` | q1 forward `5*13`; q2 exact division `5*m = 35`; q3 `5*m = 36` (finite failure) | shift-and-add `pluso (B0::B0::m) m p`, with the case analysis on `n` and the `boundMulto` recursion on `n` gone |
| `binFactor` | Kiselyov et al., FLOPS 2008 (factoring by running `multo` backward) | `p = 15` in `multo n m p` | q1 all factorizations of 15 (4 answers, finite); q2 `3*m = 15`; q3 check `3*5 = 15` | a finite disjunction of the four factor pairs |
| `lambdaAppend` | Byrd, Holk, Friedman, *miniKanren, live and untagged*, Scheme Workshop 2012; Byrd, Ballantyne, Rosenblatt, Might, *A unified approach to solving seven programming problems*, ICFP 2017 | object program `((app app) x) y`, with `app` a U-combinator `append` (de Bruijn variables, `Case` list eliminator) | q1 forward `[A,B,C] ++ [D,E]`; q2 backward: all splits of `[A,B,C,D]`; q3 `x ++ [D] = [A,B,C,D]` | plain `appendo x y v`, with no expression dispatch, environments or closures |
| `power` | Jones, Gomard, Sestoft 1993, Ch. 1 (power); cf. Ershov, *On the partial computation principle*, IPL 6(2) 1977 | exponent `n = 3` | q1 forward `2^3`; q2 cube root `x^3 = 27`; q3 `x^3 = 9` (no solution) | the recursion on `n` fully unrolled: `mul x x t & mul x t r` |

Behaviour of the unspecialized program (`original`, `--answers 10 --fuel 300000`):

* Every forward query finishes and is `ok`.
* `kmp`, `binMul`, `binFactor` and `lambdaAppend` finish on all queries.
* These backward queries run out of fuel (`ok~`):
  * `tmInterp` q2 and q3, after finding all of their answers (3 and 7);
  * `fclMult` q2 and q3, after finding all of their answers (4 and 1);
  * `power` q2, after finding `x = 3`;
  * `power` q3, with no answer.

In each case the search does not terminate after the last answer, because the interpreter
keeps enumerating ever larger inputs.
