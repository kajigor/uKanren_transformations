## Aggregate per specializer

Steps ratio: geometric mean of steps(variant)/steps(original) over queries both solve (lower is better). `solved`: queries solved within the fuel. `wrong`: answer-set mismatch (both searches finished). `unverif.`: a fuel-limited cross-check could not confirm an answer. `spec fail`: specialization crashed or timed out. `size`: total goal nodes, summed over all benchmarks the specializer handles.

| specializer | solved | wrong | unverif. | spec fail | steps ratio vs original | steps ratio vs base | size |
|---|---:|---:|---:|---:|---:|---:|---:|
| original | 34/46 | 0 | 0 | 0 | 1.000 | - | 1009 |
| original+opt | 34/46 | 0 | 0 | 0 | 0.965 | 0.965 | 987 |
| original+sched | 38/46 | 0 | 1 | 0 | 0.769 | 0.769 | 1409 |
| original+opt+sched | 37/46 | 0 | 1 | 0 | 0.759 | 0.759 | 1351 |
| pd | 37/46 | 9 | 10 | 0 | 0.108 | - | 1838 |
| pd+opt | 38/46 | 9 | 10 | 0 | 0.040 | 0.432 | 1295 |
| pd+sched | 34/46 | 7 | 15 | 0 | 0.135 | 1.312 | 2178 |
| pd+opt+sched | 38/46 | 9 | 13 | 0 | 0.038 | 0.483 | 1539 |
| cpd-det | 30/46 | 0 | 2 | 2 | 0.180 | - | 1555 |
| cpd-det+opt | 30/46 | 0 | 2 | 2 | 0.117 | 0.640 | 2063 |
| cpd-det+sched | 35/46 | 0 | 1 | 2 | 0.219 | 1.005 | 2226 |
| cpd-det+opt+sched | 34/46 | 0 | 2 | 2 | 0.145 | 0.653 | 2574 |
| cpd-branch | 30/46 | 0 | 1 | 3 | 0.117 | - | 3394 |
| cpd-branch+opt | 30/46 | 0 | 1 | 3 | 0.084 | 0.696 | 2947 |
| cpd-branch+sched | 34/46 | 0 | 1 | 3 | 0.121 | 0.964 | 3411 |
| cpd-branch+opt+sched | 33/46 | 0 | 1 | 3 | 0.097 | 0.692 | 3694 |
| conspd-det | 33/46 | 0 | 3 | 1 | 0.112 | - | 1187 |
| conspd-det+opt | 33/46 | 0 | 3 | 1 | 0.086 | 0.773 | 821 |
| conspd-det+sched | 35/46 | 0 | 2 | 1 | 0.130 | 1.011 | 1175 |
| conspd-det+opt+sched | 35/46 | 0 | 2 | 1 | 0.104 | 0.799 | 1064 |
| conspd-branch | 33/46 | 0 | 3 | 1 | 0.110 | - | 1312 |
| conspd-branch+opt | 33/46 | 0 | 3 | 1 | 0.088 | 0.800 | 985 |
| conspd-branch+sched | 35/46 | 0 | 2 | 1 | 0.127 | 1.006 | 1843 |
| conspd-branch+opt+sched | 35/46 | 0 | 2 | 1 | 0.104 | 0.809 | 1762 |

## Per query: steps (unifications + calls)

`∞` = out of fuel, `✗` = specialization failed, `!` marks a wrong result.

| bench | query | original | original+opt+sched | pd | pd+opt+sched | cpd-det | cpd-det+opt+sched | cpd-branch | cpd-branch+opt+sched | conspd-det | conspd-det+opt+sched | conspd-branch | conspd-branch+opt+sched |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| appLast | spec | 271 | 270 | 26 | 25 | 42 | 29 | 42 | 29 | 49 | 28 | 49 | 28 |
| arith | q1 | ∞(3) | ∞(3) | 23 | 22 | ∞(3) | ∞(3) | ∞(3) | ∞(3) | ∞(3) | ∞(3) | ∞(3) | ∞(3) |
| arithGeneral | q1 | 81 | 81 | 97 | 97 | 101 | 101 | 101 | 101 | 97 | 97 | 97 | 97 |
| arithGeneral | q2 | ∞(6) | ∞(6) | ∞(6) | ∞(6) | ∞(6) | ∞(6) | ∞(6) | ∞(6) | ∞(4) | ∞(6) | ∞(4) | ∞(6) |
| arithGeneral | q3 | 140 | 140 | 141 | 141 | 149 | 149 | 149 | 149 | ∞(1) | 150 | ∞(1) | 150 |
| coloring | q1 | 37409 | 1447 | 11 | 1 | 536 | 250 | 62 | 61 | 73 | 61 | 73 | 61 |
| coloring | q2 | 7335 | 602 | 7! | 1! | 104 | 70 | 62 | 61 | 43 | 42 | 43 | 42 |
| contains | spec | 2774 | 2724 | 2140 | 2104 | 435 | 222 | 435 | 222 | 210 | 222 | 210 | 222 |
| deforestation1 | spec | 60 | 62 | 15! | 1! | 5 | 2 | 3 | 2 | 3 | 2 | 3 | 2 |
| depth | spec | 189 | 148 | 189 | 178 | 17 | 7 | 12 | 5 | 12 | 5 | 10 | 5 |
| doubleAppend | q1 | 90 | 89 | 111 | 110 | 57 | 56 | 57 | 56 | 57 | 56 | 57 | 56 |
| doubleAppend | q2 | 115 | 76 | 75 | 65 | 63 | 62 | 63 | 62 | 62 | 61 | 62 | 61 |
| doubleAppend | q3 | ∞(5) | 46 | 50 | 115 | 38 | 37 | 38 | 37 | 36 | 35 | 36 | 35 |
| exDepth | spec | 1993 | 1600 | 7! | 1! | ∞(0) | ∞(0) | 20 | 17 | 18 | 17 | 18 | 17 |
| exprEval | q1 | 187 | 187 | 4! | 1! | ∞(1) | 109 | ∞(1) | 109 | ∞(1) | 123 | ∞(1) | 123 |
| exprEval | q2 | ∞(2) | ∞(2) | 4 | 1 | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) |
| exprEvalGeneral | q1 | 187 | 187 | 181 | ∞(0) | ∞(1) | 187 | ∞(1) | 187 | 196 | 196 | 196 | 196 |
| exprEvalGeneral | q2 | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) | ∞(2) |
| flip | spec | ∞(0) | 89 | ∞(0) | 82 | 7 | 2 | 7 | 2 | 3 | 2 | 3 | 2 |
| matchAppend | spec | 93 | 76 | 66 | 65 | 36 | 18 | 36 | 18 | 24 | 11 | 24 | 11 |
| matchSimple | spec | 173 | 113 | 17! | 7! | 17 | 10 | 14 | 10 | 16 | 10 | 14 | 10 |
| maxLength | spec | 435 | 433 | 62 | 61 | 59 | 58 | 59 | 58 | 59 | 58 | 59 | 58 |
| path | spec | 11441 | 2311 | ∞(2) | 3 | 135 | 79 | 63 | 50 | 132 | 72 | 50 | 50 |
| propFirst | spec | 163 | 181 | 104 | 103 | 66 | 64 | 70 | 42 | 68 | 42 | 68 | 42 |
| propFirstNando | spec | 303 | 308 | 136 | 153 | 69 | 68 | 76 | 65 | 68 | 66 | 68 | 66 |
| propLast | spec | 155 | 181 | 70 | 103 | 112 | 74 | 70 | 42 | 68 | 42 | 68 | 42 |
| propLastNando | spec | 366 | 308 | 163 | 173 | 209 | 94 | 159 | 56 | 68 | 66 | 68 | 66 |
| queens5 | q1 | 259551 | ∞(8) | 137 | 1 | ✗ | ✗ | 19369 | 11265 | ✗ | ✗ | ✗ | ✗ |
| queens5 | q2 | 61007 | 79719 | 33! | 1! | ✗ | ✗ | ∞(2) | ∞(2) | ✗ | ✗ | ✗ | ✗ |
| regexp | spec | 340 | 347 | 5 | 2 | 76 | 73 | 62 | 64 | 41 | 73 | 62 | 64 |
| relSort | spec | ∞(0) | ∞(1) | ∞(0) | ∞(4) | ∞(1) | 529 | ∞(1) | 243 | 3 | 2 | 3 | 2 |
| relSortGeneral | q1 | ∞(0) | ∞(0) | ∞(0) | 856 | ∞(0) | ∞(0) | ∞(0) | ∞(0) | ∞(0) | ∞(0) | ∞(0) | ∞(0) |
| relSortGeneral | q2 | 886 | 860 | 636 | 19549 | 725 | 686 | 764 | 723 | 747 | 676 | 809 | 707 |
| rev | spec | 96 | 96 | 5! | 1! | 11 | 2 | 11 | 2 | 3 | 2 | 3 | 2 |
| revAcc | spec | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) |
| revGeneral | q1 | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | ∞(1) | 189 | 189 | 189 | 189 |
| revGeneral | q2 | 262 | 262 | 245 | 245 | 254 | 254 | 254 | 254 | ∞(1) | ∞(1) | ∞(1) | ∞(1) |
| rotatePrune | spec | ∞(4) | 46 | 5 | 1 | 12 | 7 | 10 | 6 | 10 | 7 | 8 | 6 |
| sort | spec | 185 | 191 | 4! | 1! | 8 | 2 | 3 | 2 | 3 | 2 | 3 | 2 |
| sortGeneral | q1 | 254 | 262 | 282 | ∞(1) | ✗ | ✗ | ✗ | ✗ | ∞(0) | ∞(1) | ∞(0) | ∞(1) |
| sortGeneral | q2 | ∞(6) | 64885 | ∞(6) | ∞(6) | ✗ | ✗ | ✗ | ✗ | ∞(0) | ∞(0) | ∞(0) | ∞(0) |
| typecheck | q1 | 442 | 338 | 183 | 152 | 270 | 274 | ✗ | ✗ | 145 | 146 | 144 | 140 |
| typecheck | q2 | 156 | 155 | 98 | 97 | ∞(1) | 149 | ✗ | ✗ | 115 | 117 | 160 | 162 |
| unifier | spec | 681 | 495 | 314 | 315 | 307 | 323 | ✗ | ✗ | 241 | 241 | 209 | 209 |
| uptoSum | spec | 405 | 399 | 4! | 1! | 3 | 2 | 3 | 2 | 3 | 2 | 3 | 2 |
| vanilla | spec | 405 | 386 | 6 | 1 | 44 | 31 | 80 | 72 | 53 | 31 | 62 | 38 |

## Wrong results and specialization failures

- coloring q2 **pd**: Finished MISMATCH
- coloring q2 **pd+opt**: Finished MISMATCH
- coloring q2 **pd+sched**: Finished MISMATCH
- coloring q2 **pd+opt+sched**: Finished MISMATCH
- deforestation1 spec **pd**: Finished MISMATCH
- deforestation1 spec **pd+opt**: Finished MISMATCH
- deforestation1 spec **pd+sched**: Finished MISMATCH
- deforestation1 spec **pd+opt+sched**: Finished MISMATCH
- exDepth spec **pd**: Finished MISMATCH
- exDepth spec **pd+opt**: Finished MISMATCH
- exDepth spec **pd+sched**: Finished MISMATCH
- exDepth spec **pd+opt+sched**: Finished MISMATCH
- exprEval q1 **pd**: Finished MISMATCH
- exprEval q1 **pd+opt**: Finished MISMATCH
- exprEval q1 **pd+opt+sched**: Finished MISMATCH
- matchSimple spec **pd**: Finished MISMATCH
- matchSimple spec **pd+opt**: Finished MISMATCH
- matchSimple spec **pd+sched**: Finished MISMATCH
- matchSimple spec **pd+opt+sched**: Finished MISMATCH
- queens5 q2 **pd**: Finished MISMATCH
- queens5 q2 **pd+opt**: Finished MISMATCH
- queens5 q2 **pd+sched**: Finished MISMATCH
- queens5 q2 **pd+opt+sched**: Finished MISMATCH
- queens5 * **cpd-det**: spec-fail: <<timeout>> -
- queens5 * **cpd-det+opt**: spec-fail: <<timeout>> -
- queens5 * **cpd-det+sched**: spec-fail: <<timeout>> -
- queens5 * **cpd-det+opt+sched**: spec-fail: <<timeout>> -
- queens5 * **conspd-det**: spec-fail: <<timeout>> -
- queens5 * **conspd-det+opt**: spec-fail: <<timeout>> -
- queens5 * **conspd-det+sched**: spec-fail: <<timeout>> -
- queens5 * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- queens5 * **conspd-branch**: spec-fail: <<timeout>> -
- queens5 * **conspd-branch+opt**: spec-fail: <<timeout>> -
- queens5 * **conspd-branch+sched**: spec-fail: <<timeout>> -
- queens5 * **conspd-branch+opt+sched**: spec-fail: <<timeout>> -
- rev spec **pd**: Finished MISMATCH
- rev spec **pd+opt**: Finished MISMATCH
- rev spec **pd+sched**: Finished MISMATCH
- rev spec **pd+opt+sched**: Finished MISMATCH
- sort spec **pd**: Finished MISMATCH
- sort spec **pd+opt**: Finished MISMATCH
- sort spec **pd+opt+sched**: Finished MISMATCH
- sortGeneral * **cpd-det**: spec-fail: Residualization failed: invocation of an undefined relation. -
- sortGeneral * **cpd-det+opt**: spec-fail: Residualization failed: invocation of an undefined relation. -
- sortGeneral * **cpd-det+sched**: spec-fail: Residualization failed: invocation of an undefined relation. -
- sortGeneral * **cpd-det+opt+sched**: spec-fail: Residualization failed: invocation of an undefined relation. -
- sortGeneral * **cpd-branch**: spec-fail: Residualization failed: invocation of an undefined relation. -
- sortGeneral * **cpd-branch+opt**: spec-fail: Residualization failed: invocation of an undefined relation. -
- sortGeneral * **cpd-branch+sched**: spec-fail: Residualization failed: invocation of an undefined relation. -
- sortGeneral * **cpd-branch+opt+sched**: spec-fail: Residualization failed: invocation of an undefined relation. -
- typecheck * **cpd-branch**: spec-fail: <<timeout>> -
- typecheck * **cpd-branch+opt**: spec-fail: <<timeout>> -
- typecheck * **cpd-branch+sched**: spec-fail: <<timeout>> -
- typecheck * **cpd-branch+opt+sched**: spec-fail: <<timeout>> -
- unifier * **cpd-branch**: spec-fail: <<timeout>> -
- unifier * **cpd-branch+opt**: spec-fail: <<timeout>> -
- unifier * **cpd-branch+sched**: spec-fail: <<timeout>> -
- unifier * **cpd-branch+opt+sched**: spec-fail: <<timeout>> -
- uptoSum spec **pd**: Finished MISMATCH
- uptoSum spec **pd+opt**: Finished MISMATCH
- uptoSum spec **pd+sched**: Finished MISMATCH
- uptoSum spec **pd+opt+sched**: Finished MISMATCH
