## Aggregate per specializer

Steps ratio: geometric mean of steps(variant)/steps(original) over queries both solve (lower is better). `solved`: queries solved within the fuel. `wrong`: answer-set mismatch (both searches finished). `unverif.`: a fuel-limited cross-check could not confirm an answer. `spec fail`: specialization crashed or timed out. `size`: total goal nodes, summed over all benchmarks the specializer handles.

| specializer | solved | wrong | unverif. | spec fail | steps ratio vs original | steps ratio vs base | size |
|---|---:|---:|---:|---:|---:|---:|---:|
| original | 95/136 | 0 | 0 | 0 | 1.000 | - | 2842 |
| cpd-la+opt | 88/136 | 0 | 7 | 4 | 0.074 | nan | 3804 |
| cpd-la+opt+sched | 106/136 | 0 | 3 | 4 | 0.070 | nan | 8373 |

## Per query: steps (unifications + calls)

`∞` = out of fuel, `✗` = specialization failed, `!` marks a wrong result.

| bench | query | original | original+opt+sched | pd | pd+opt+sched | cpd-det | cpd-det+opt+sched | cpd-branch | cpd-branch+opt+sched | cpd-la | cpd-la+opt+sched | conspd-det | conspd-det+opt+sched | conspd-branch | conspd-branch+opt+sched | conspd-la | conspd-la+opt+sched |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| appLast | spec | 271 |  |  |  |  |  |  |  |  | 29 |  |  |  |  |  |  |
| appLast@vanilla | spec | 4131 |  |  |  |  |  |  |  |  | 245 |  |  |  |  |  |  |
| arith | q1 | ∞(3) |  |  |  |  |  |  |  |  | ∞(3) |  |  |  |  |  |  |
| arith@vanilla | q1 | ∞(3) |  |  |  |  |  |  |  |  | ∞(3) |  |  |  |  |  |  |
| arithGeneral | q1 | 81 |  |  |  |  |  |  |  |  | 101 |  |  |  |  |  |  |
| arithGeneral | q2 | ∞(6) |  |  |  |  |  |  |  |  | ∞(6) |  |  |  |  |  |  |
| arithGeneral | q3 | 140 |  |  |  |  |  |  |  |  | 149 |  |  |  |  |  |  |
| arithGeneral@vanilla | q1 | 1048 |  |  |  |  |  |  |  |  | 924 |  |  |  |  |  |  |
| arithGeneral@vanilla | q2 | ∞(6) |  |  |  |  |  |  |  |  | ∞(6) |  |  |  |  |  |  |
| arithGeneral@vanilla | q3 | 1830 |  |  |  |  |  |  |  |  | 1632 |  |  |  |  |  |  |
| binFactor | q1 | 1568 |  |  |  |  |  |  |  |  | 15 |  |  |  |  |  |  |
| binFactor | q2 | 395 |  |  |  |  |  |  |  |  | 12 |  |  |  |  |  |  |
| binFactor | q3 | 279 |  |  |  |  |  |  |  |  | 10 |  |  |  |  |  |  |
| binFactor@vanilla | q1 | 66471 |  |  |  |  |  |  |  |  | 14651 |  |  |  |  |  |  |
| binFactor@vanilla | q2 | 16089 |  |  |  |  |  |  |  |  | 10382 |  |  |  |  |  |  |
| binFactor@vanilla | q3 | 11482 |  |  |  |  |  |  |  |  | ∞(1) |  |  |  |  |  |  |
| binMul | q1 | 611 |  |  |  |  |  |  |  |  | 101 |  |  |  |  |  |  |
| binMul | q2 | 793 |  |  |  |  |  |  |  |  | 181 |  |  |  |  |  |  |
| binMul | q3 | 4673 |  |  |  |  |  |  |  |  | 177 |  |  |  |  |  |  |
| binMul@vanilla | q1 | 25025 |  |  |  |  |  |  |  |  | 140 |  |  |  |  |  |  |
| binMul@vanilla | q2 | 32116 |  |  |  |  |  |  |  |  | 251 |  |  |  |  |  |  |
| binMul@vanilla | q3 | 201702 |  |  |  |  |  |  |  |  | 624 |  |  |  |  |  |  |
| coloring | q1 | 37409 |  |  |  |  |  |  |  |  | 250 |  |  |  |  |  |  |
| coloring | q2 | 7335 |  |  |  |  |  |  |  |  | 70 |  |  |  |  |  |  |
| coloring@vanilla | q1 | ∞(0) |  |  |  |  |  |  |  |  | ✗ |  |  |  |  |  |  |
| coloring@vanilla | q2 | 184235 |  |  |  |  |  |  |  |  | ✗ |  |  |  |  |  |  |
| contains | spec | 2774 |  |  |  |  |  |  |  |  | 222 |  |  |  |  |  |  |
| contains@vanilla | spec | 53349 |  |  |  |  |  |  |  |  | 2591 |  |  |  |  |  |  |
| deforestation1 | spec | 60 |  |  |  |  |  |  |  |  | 2 |  |  |  |  |  |  |
| deforestation1@vanilla | spec | 929 |  |  |  |  |  |  |  |  | 2 |  |  |  |  |  |  |
| depth | spec | 189 |  |  |  |  |  |  |  |  | 7 |  |  |  |  |  |  |
| depth@vanilla | spec | 3467 |  |  |  |  |  |  |  |  | 8 |  |  |  |  |  |  |
| doubleAppend | q1 | 90 |  |  |  |  |  |  |  |  | 56 |  |  |  |  |  |  |
| doubleAppend | q2 | 115 |  |  |  |  |  |  |  |  | 62 |  |  |  |  |  |  |
| doubleAppend | q3 | ∞(5) |  |  |  |  |  |  |  |  | 37 |  |  |  |  |  |  |
| doubleAppend@vanilla | q1 | 1074 |  |  |  |  |  |  |  |  | 85 |  |  |  |  |  |  |
| doubleAppend@vanilla | q2 | 1588 |  |  |  |  |  |  |  |  | 69 |  |  |  |  |  |  |
| doubleAppend@vanilla | q3 | ∞(5) |  |  |  |  |  |  |  |  | 42 |  |  |  |  |  |  |
| exDepth | spec | 1993 |  |  |  |  |  |  |  |  | ∞(0) |  |  |  |  |  |  |
| exDepth@vanilla | spec | 39748 |  |  |  |  |  |  |  |  | 17 |  |  |  |  |  |  |
| exprEval | q1 | 187 |  |  |  |  |  |  |  |  | 109 |  |  |  |  |  |  |
| exprEval | q2 | ∞(2) |  |  |  |  |  |  |  |  | 1512 |  |  |  |  |  |  |
| exprEval@vanilla | q1 | 3550 |  |  |  |  |  |  |  |  | 147 |  |  |  |  |  |  |
| exprEval@vanilla | q2 | ∞(2) |  |  |  |  |  |  |  |  | 3232 |  |  |  |  |  |  |
| exprEvalGeneral | q1 | 187 |  |  |  |  |  |  |  |  | 187 |  |  |  |  |  |  |
| exprEvalGeneral | q2 | ∞(2) |  |  |  |  |  |  |  |  | 1927 |  |  |  |  |  |  |
| exprEvalGeneral@vanilla | q1 | 3550 |  |  |  |  |  |  |  |  | 3252 |  |  |  |  |  |  |
| exprEvalGeneral@vanilla | q2 | ∞(2) |  |  |  |  |  |  |  |  | ∞(2) |  |  |  |  |  |  |
| fclMult | q1 | 2847 |  |  |  |  |  |  |  |  | 81 |  |  |  |  |  |  |
| fclMult | q2 | ∞(4) |  |  |  |  |  |  |  |  | ∞(4) |  |  |  |  |  |  |
| fclMult | q3 | ∞(1) |  |  |  |  |  |  |  |  | ∞(1) |  |  |  |  |  |  |
| fclMult@vanilla | q1 | 100809 |  |  |  |  |  |  |  |  | 2680 |  |  |  |  |  |  |
| fclMult@vanilla | q2 | ∞(0) |  |  |  |  |  |  |  |  | ∞(4) |  |  |  |  |  |  |
| fclMult@vanilla | q3 | ∞(1) |  |  |  |  |  |  |  |  | ∞(1) |  |  |  |  |  |  |
| flip | spec | ∞(0) |  |  |  |  |  |  |  |  | 2 |  |  |  |  |  |  |
| flip@vanilla | spec | ∞(0) |  |  |  |  |  |  |  |  | 38 |  |  |  |  |  |  |
| kmp | q1 | 712 |  |  |  |  |  |  |  |  | 505 |  |  |  |  |  |  |
| kmp | q2 | 492 |  |  |  |  |  |  |  |  | 340 |  |  |  |  |  |  |
| kmp | q3 | 3624 |  |  |  |  |  |  |  |  | 2266 |  |  |  |  |  |  |
| kmp | q4 | 518 |  |  |  |  |  |  |  |  | 375 |  |  |  |  |  |  |
| kmp@vanilla | q1 | 13470 |  |  |  |  |  |  |  |  | 505 |  |  |  |  |  |  |
| kmp@vanilla | q2 | 9102 |  |  |  |  |  |  |  |  | 340 |  |  |  |  |  |  |
| kmp@vanilla | q3 | 69046 |  |  |  |  |  |  |  |  | 2266 |  |  |  |  |  |  |
| kmp@vanilla | q4 | 9803 |  |  |  |  |  |  |  |  | 375 |  |  |  |  |  |  |
| lambdaAppend | q1 | 619 |  |  |  |  |  |  |  |  | 18 |  |  |  |  |  |  |
| lambdaAppend | q2 | 839 |  |  |  |  |  |  |  |  | 24 |  |  |  |  |  |  |
| lambdaAppend | q3 | 839 |  |  |  |  |  |  |  |  | 23 |  |  |  |  |  |  |
| lambdaAppend@vanilla | q1 | 13232 |  |  |  |  |  |  |  |  | 141 |  |  |  |  |  |  |
| lambdaAppend@vanilla | q2 | 17904 |  |  |  |  |  |  |  |  | 206 |  |  |  |  |  |  |
| lambdaAppend@vanilla | q3 | 17781 |  |  |  |  |  |  |  |  | 202 |  |  |  |  |  |  |
| matchAppend | spec | 93 |  |  |  |  |  |  |  |  | 18 |  |  |  |  |  |  |
| matchAppend@vanilla | spec | 1591 |  |  |  |  |  |  |  |  | 18 |  |  |  |  |  |  |
| matchSimple | spec | 173 |  |  |  |  |  |  |  |  | 10 |  |  |  |  |  |  |
| matchSimple@vanilla | spec | 3306 |  |  |  |  |  |  |  |  | 10 |  |  |  |  |  |  |
| maxLength | spec | 435 |  |  |  |  |  |  |  |  | 58 |  |  |  |  |  |  |
| maxLength@vanilla | spec | 9772 |  |  |  |  |  |  |  |  | 348 |  |  |  |  |  |  |
| path | spec | 11441 |  |  |  |  |  |  |  |  | 76 |  |  |  |  |  |  |
| path@vanilla | spec | ∞(5) |  |  |  |  |  |  |  |  | 426 |  |  |  |  |  |  |
| power | q1 | 124 |  |  |  |  |  |  |  |  | ∞(1) |  |  |  |  |  |  |
| power | q2 | ∞(1) |  |  |  |  |  |  |  |  | 7743 |  |  |  |  |  |  |
| power | q3 | ∞(0) |  |  |  |  |  |  |  |  | 904 |  |  |  |  |  |  |
| power@vanilla | q1 | 1858 |  |  |  |  |  |  |  |  | 131 |  |  |  |  |  |  |
| power@vanilla | q2 | ∞(1) |  |  |  |  |  |  |  |  | ∞(1) |  |  |  |  |  |  |
| power@vanilla | q3 | ∞(0) |  |  |  |  |  |  |  |  | ∞(0) |  |  |  |  |  |  |
| propFirst | spec | 163 |  |  |  |  |  |  |  |  | 64 |  |  |  |  |  |  |
| propFirst@vanilla | spec | 6633 |  |  |  |  |  |  |  |  | 63 |  |  |  |  |  |  |
| propFirstNando | spec | 303 |  |  |  |  |  |  |  |  | 68 |  |  |  |  |  |  |
| propFirstNando@vanilla | spec | 9625 |  |  |  |  |  |  |  |  | 120 |  |  |  |  |  |  |
| propLast | spec | 155 |  |  |  |  |  |  |  |  | 74 |  |  |  |  |  |  |
| propLast@vanilla | spec | 8219 |  |  |  |  |  |  |  |  | 55 |  |  |  |  |  |  |
| propLastNando | spec | 366 |  |  |  |  |  |  |  |  | 94 |  |  |  |  |  |  |
| propLastNando@vanilla | spec | 21711 |  |  |  |  |  |  |  |  | 188 |  |  |  |  |  |  |
| queens5 | q1 | 259551 |  |  |  |  |  |  |  |  | ✗ |  |  |  |  |  |  |
| queens5 | q2 | 61007 |  |  |  |  |  |  |  |  | ✗ |  |  |  |  |  |  |
| queens5@vanilla | q1 | ∞(0) |  |  |  |  |  |  |  |  | ✗ |  |  |  |  |  |  |
| queens5@vanilla | q2 | ∞(0) |  |  |  |  |  |  |  |  | ✗ |  |  |  |  |  |  |
| regexp | spec | 340 |  |  |  |  |  |  |  |  | 73 |  |  |  |  |  |  |
| regexp@vanilla | spec | 9173 |  |  |  |  |  |  |  |  | 8030 |  |  |  |  |  |  |
| relSort | spec | ∞(0) |  |  |  |  |  |  |  |  | 2 |  |  |  |  |  |  |
| relSort@vanilla | spec | ∞(0) |  |  |  |  |  |  |  |  | 278 |  |  |  |  |  |  |
| relSortGeneral | q1 | ∞(0) |  |  |  |  |  |  |  |  | 436 |  |  |  |  |  |  |
| relSortGeneral | q2 | 886 |  |  |  |  |  |  |  |  | 686 |  |  |  |  |  |  |
| relSortGeneral@vanilla | q1 | ∞(0) |  |  |  |  |  |  |  |  | ∞(0) |  |  |  |  |  |  |
| relSortGeneral@vanilla | q2 | 19240 |  |  |  |  |  |  |  |  | 17964 |  |  |  |  |  |  |
| rev | spec | 96 |  |  |  |  |  |  |  |  | 2 |  |  |  |  |  |  |
| rev@vanilla | spec | 1209 |  |  |  |  |  |  |  |  | 2 |  |  |  |  |  |  |
| revAcc | spec | ∞(1) |  |  |  |  |  |  |  |  | ∞(1) |  |  |  |  |  |  |
| revAcc@vanilla | spec | ∞(1) |  |  |  |  |  |  |  |  | ∞(1) |  |  |  |  |  |  |
| revGeneral | q1 | ∞(1) |  |  |  |  |  |  |  |  | 181 |  |  |  |  |  |  |
| revGeneral | q2 | 262 |  |  |  |  |  |  |  |  | 254 |  |  |  |  |  |  |
| revGeneral@vanilla | q1 | ∞(1) |  |  |  |  |  |  |  |  | ∞(1) |  |  |  |  |  |  |
| revGeneral@vanilla | q2 | 3325 |  |  |  |  |  |  |  |  | 2975 |  |  |  |  |  |  |
| rotatePrune | spec | ∞(4) |  |  |  |  |  |  |  |  | 7 |  |  |  |  |  |  |
| rotatePrune@vanilla | spec | ∞(4) |  |  |  |  |  |  |  |  | 8 |  |  |  |  |  |  |
| sort | spec | 185 |  |  |  |  |  |  |  |  | 2 |  |  |  |  |  |  |
| sort@vanilla | spec | 3884 |  |  |  |  |  |  |  |  | ✗ |  |  |  |  |  |  |
| sortGeneral | q1 | 254 |  |  |  |  |  |  |  |  | ∞(1) |  |  |  |  |  |  |
| sortGeneral | q2 | ∞(6) |  |  |  |  |  |  |  |  | 35252 |  |  |  |  |  |  |
| sortGeneral@vanilla | q1 | 5304 |  |  |  |  |  |  |  |  | 4923 |  |  |  |  |  |  |
| sortGeneral@vanilla | q2 | ∞(2) |  |  |  |  |  |  |  |  | ∞(2) |  |  |  |  |  |  |
| tmInterp | q1 | 409 |  |  |  |  |  |  |  |  | 79 |  |  |  |  |  |  |
| tmInterp | q2 | ∞(3) |  |  |  |  |  |  |  |  | ∞(3) |  |  |  |  |  |  |
| tmInterp | q3 | ∞(7) |  |  |  |  |  |  |  |  | 174 |  |  |  |  |  |  |
| tmInterp@vanilla | q1 | 13715 |  |  |  |  |  |  |  |  | 396 |  |  |  |  |  |  |
| tmInterp@vanilla | q2 | ∞(3) |  |  |  |  |  |  |  |  | ∞(3) |  |  |  |  |  |  |
| tmInterp@vanilla | q3 | ∞(7) |  |  |  |  |  |  |  |  | ∞(7) |  |  |  |  |  |  |
| typecheck | q1 | 442 |  |  |  |  |  |  |  |  | 291 |  |  |  |  |  |  |
| typecheck | q2 | 156 |  |  |  |  |  |  |  |  | 138 |  |  |  |  |  |  |
| typecheck@vanilla | q1 | 14081 |  |  |  |  |  |  |  |  | 11164 |  |  |  |  |  |  |
| typecheck@vanilla | q2 | 3256 |  |  |  |  |  |  |  |  | 2901 |  |  |  |  |  |  |
| unifier | spec | 681 |  |  |  |  |  |  |  |  | 228 |  |  |  |  |  |  |
| unifier@vanilla | spec | ∞(7) |  |  |  |  |  |  |  |  | 42128 |  |  |  |  |  |  |
| uptoSum | spec | 405 |  |  |  |  |  |  |  |  | 2 |  |  |  |  |  |  |
| uptoSum@vanilla | spec | 9726 |  |  |  |  |  |  |  |  | 157 |  |  |  |  |  |  |
| vanilla | spec | 405 |  |  |  |  |  |  |  |  | 31 |  |  |  |  |  |  |
| vanilla@vanilla | spec | 6646 |  |  |  |  |  |  |  |  | 84 |  |  |  |  |  |  |

## Jones optimality (vanilla self-interpreter)

For each benchmark p, `p@vanilla` runs p through the vanilla self-interpreter (`src/Measure/Vanilla.hs`). `solved`: queries of p@vanilla solved. `vs direct`: geometric mean of steps(S(p@vanilla))/steps(original p) (a Jones-optimal S gives <= 1). `vs S(p)`: steps(S(p@vanilla))/steps(S(p)), the interpretive overhead S fails to remove (1 = none left). `optimal`: queries with steps(S(p@vanilla)) <= steps(S(p)). Ratios over queries both sides solve.

| specializer | solved | wrong | spec fail | vs direct | vs S(p) | optimal |
|---|---:|---:|---:|---:|---:|---:|
| original | 45/68 | 0 | 0 | 22.876 | 22.876 | 0 |
| cpd-la+opt | 42/63 | 0 | 3 | 1.038 | 6.648 | 11 |
| cpd-la+opt+sched | 49/63 | 0 | 3 | 0.879 | 5.833 | 10 |
| cpd-la+opt+spc | 43/63 | 0 | 3 | 0.271 | 1.901 | 15 |
| cpd-la+opt+spc+sched | 57/63 | 0 | 3 | 0.295 | 2.060 | 20 |
| cpd-lap+opt+sched+guard+idx | 48/60 | 0 | 4 | 0.732 | 4.997 | 10 |
| cpd-lap+opt+spc+sched+guard+idx | 55/60 | 0 | 4 | 0.262 | 1.883 | 22 |

## Wrong results and specialization failures

- sort@vanilla * **cpd-la+opt**: spec-fail: <<timeout>> -
- sort@vanilla * **cpd-la+opt+sched**: spec-fail: <<timeout>> -
- sort@vanilla * **cpd-la+opt+spc**: spec-fail: <<timeout>> -
- sort@vanilla * **cpd-la+opt+spc+sched**: spec-fail: <<timeout>> -
- sort@vanilla * **cpd-lap+opt+sched+guard+idx**: spec-fail: <<timeout>> -
- sort@vanilla * **cpd-lap+opt+spc+sched+guard+idx**: spec-fail: <<timeout>> -
- coloring@vanilla * **cpd-la+opt**: spec-fail: <<timeout>> -
- coloring@vanilla * **cpd-la+opt+sched**: spec-fail: <<timeout>> -
- coloring@vanilla * **cpd-la+opt+spc**: spec-fail: <<timeout>> -
- coloring@vanilla * **cpd-la+opt+spc+sched**: spec-fail: <<timeout>> -
- coloring@vanilla * **cpd-lap+opt+sched+guard+idx**: spec-fail: <<timeout>> -
- coloring@vanilla * **cpd-lap+opt+spc+sched+guard+idx**: spec-fail: <<timeout>> -
- queens5 * **cpd-la+opt**: spec-fail: <<timeout>> -
- queens5 * **cpd-la+opt+sched**: spec-fail: <<timeout>> -
- queens5 * **cpd-la+opt+spc**: spec-fail: <<timeout>> -
- queens5 * **cpd-la+opt+spc+sched**: spec-fail: <<timeout>> -
- queens5 * **cpd-lap+opt+sched+guard+idx**: spec-fail: <<timeout>> -
- queens5 * **cpd-lap+opt+spc+sched+guard+idx**: spec-fail: <<timeout>> -
- queens5@vanilla * **cpd-la+opt**: spec-fail: <<timeout>> -
- queens5@vanilla * **cpd-la+opt+sched**: spec-fail: <<timeout>> -
- queens5@vanilla * **cpd-la+opt+spc**: spec-fail: <<timeout>> -
- queens5@vanilla * **cpd-la+opt+spc+sched**: spec-fail: <<timeout>> -
- queens5@vanilla * **cpd-lap+opt+sched+guard+idx**: spec-fail: <<timeout>> -
- queens5@vanilla * **cpd-lap+opt+spc+sched+guard+idx**: spec-fail: <<timeout>> -
- fclMult@vanilla * **cpd-lap+opt+sched+guard+idx**: spec-fail: <<timeout>> -
- fclMult@vanilla * **cpd-lap+opt+spc+sched+guard+idx**: spec-fail: <<timeout>> -
