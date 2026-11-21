## Aggregate per specializer

Steps ratio: geometric mean of steps(variant)/steps(original) over queries both solve (lower is better). `solved`: queries solved within the fuel. `wrong`: answer-set mismatch (both searches finished). `unverif.`: a fuel-limited cross-check could not confirm an answer. `spec fail`: specialization crashed or timed out. `size`: total goal nodes, summed over all benchmarks the specializer handles.

| specializer | solved | wrong | unverif. | spec fail | steps ratio vs original | steps ratio vs base | size |
|---|---:|---:|---:|---:|---:|---:|---:|
| original | 95/136 | 0 | 0 | 0 | 1.000 | - | 2842 |
| cpd-det | 70/136 | 0 | 17 | 6 | 0.189 | - | 7266 |
| cpd-det+opt+sched | 90/136 | 0 | 9 | 6 | 0.084 | 0.469 | 9262 |
| cpd-la | 85/136 | 0 | 6 | 5 | 0.119 | - | 5231 |
| cpd-la+opt+sched | 98/136 | 0 | 3 | 5 | 0.082 | 0.681 | 9437 |
| conspd-det | 49/136 | 0 | 19 | 26 | 0.119 | - | 7850 |
| conspd-det+opt+sched | 55/136 | 0 | 13 | 26 | 0.100 | 0.743 | 17231 |
| conspd-la | 52/136 | 0 | 14 | 29 | 0.089 | - | 3320 |
| conspd-la+opt+sched | 58/136 | 0 | 13 | 29 | 0.087 | 0.862 | 15663 |

## Per query: steps (unifications + calls)

`∞` = out of fuel, `✗` = specialization failed, `!` marks a wrong result.

| bench | query | original | original+opt+sched | pd | pd+opt+sched | cpd-det | cpd-det+opt+sched | cpd-branch | cpd-branch+opt+sched | cpd-la | cpd-la+opt+sched | conspd-det | conspd-det+opt+sched | conspd-branch | conspd-branch+opt+sched | conspd-la | conspd-la+opt+sched |
|---|---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|
| appLast | spec | 271 |  |  |  | 42 | 29 |  |  | 30 | 29 | 49 | 28 |  |  | 29 | 28 |
| appLast@vanilla | spec | 4131 |  |  |  | 163783 | 264 |  |  | 247 | 245 | ∞(4) | ∞(8) |  |  | ✗ | ✗ |
| arith | q1 | ∞(3) |  |  |  | ∞(3) | ∞(3) |  |  | ∞(3) | ∞(3) | ∞(3) | ∞(3) |  |  | ∞(3) | ∞(3) |
| arith@vanilla | q1 | ∞(3) |  |  |  | ∞(2) | ∞(3) |  |  | ∞(3) | ∞(3) | ∞(3) | ∞(3) |  |  | ∞(3) | ∞(3) |
| arithGeneral | q1 | 81 |  |  |  | 101 | 101 |  |  | 101 | 101 | 97 | 97 |  |  | 97 | 97 |
| arithGeneral | q2 | ∞(6) |  |  |  | ∞(6) | ∞(6) |  |  | ∞(6) | ∞(6) | ∞(4) | ∞(6) |  |  | ∞(4) | ∞(6) |
| arithGeneral | q3 | 140 |  |  |  | 149 | 149 |  |  | 149 | 149 | ∞(1) | 150 |  |  | ∞(1) | 150 |
| arithGeneral@vanilla | q1 | 1048 |  |  |  | 931 | 930 |  |  | 925 | 924 | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) |
| arithGeneral@vanilla | q2 | ∞(6) |  |  |  | ∞(6) | ∞(6) |  |  | ∞(6) | ∞(6) | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) |
| arithGeneral@vanilla | q3 | 1830 |  |  |  | 1640 | 1639 |  |  | 1633 | 1632 | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) |
| binFactor | q1 | 1568 |  |  |  | 41 | 25 |  |  | 22 | 15 | 33 | 24 |  |  | 17 | 15 |
| binFactor | q2 | 395 |  |  |  | 29 | 21 |  |  | 15 | 12 | 28 | 22 |  |  | 16 | 14 |
| binFactor | q3 | 279 |  |  |  | 20 | 12 |  |  | 13 | 10 | 18 | 13 |  |  | 12 | 10 |
| binFactor@vanilla | q1 | 66471 |  |  |  | ∞(2) | ∞(2) |  |  | 14660 | 14651 | ✗ | ✗ |  |  | ✗ | ✗ |
| binFactor@vanilla | q2 | 16089 |  |  |  | ∞(1) | ∞(1) |  |  | 10386 | 10382 | ✗ | ✗ |  |  | ✗ | ✗ |
| binFactor@vanilla | q3 | 11482 |  |  |  | ∞(1) | ∞(1) |  |  | 5977 | ∞(1) | ✗ | ✗ |  |  | ✗ | ✗ |
| binMul | q1 | 611 |  |  |  | 153 | 138 |  |  | 171 | 101 | 55 | 53 |  |  | 56 | 55 |
| binMul | q2 | 793 |  |  |  | 227 | 183 |  |  | ∞(1) | 181 | 75 | 64 |  |  | 80 | 79 |
| binMul | q3 | 4673 |  |  |  | ∞(0) | 163 |  |  | ∞(0) | 177 | ∞(0) | 149 |  |  | ∞(0) | 77 |
| binMul@vanilla | q1 | 25025 |  |  |  | ∞(0) | 201 |  |  | 162 | 140 | ✗ | ✗ |  |  | ✗ | ✗ |
| binMul@vanilla | q2 | 32116 |  |  |  | ∞(0) | 306 |  |  | ∞(1) | 251 | ✗ | ✗ |  |  | ✗ | ✗ |
| binMul@vanilla | q3 | 201702 |  |  |  | ∞(0) | 721 |  |  | ∞(0) | 624 | ✗ | ✗ |  |  | ✗ | ✗ |
| coloring | q1 | 37409 |  |  |  | 536 | 250 |  |  | 536 | 250 | 73 | 61 |  |  | 73 | 61 |
| coloring | q2 | 7335 |  |  |  | 104 | 70 |  |  | 104 | 70 | 43 | 42 |  |  | 43 | 42 |
| coloring@vanilla | q1 | ∞(0) |  |  |  | 11845 | 625 |  |  | ✗ | ✗ | ✗ | ✗ |  |  | ✗ | ✗ |
| coloring@vanilla | q2 | 184235 |  |  |  | 2144 | 184 |  |  | ✗ | ✗ | ✗ | ✗ |  |  | ✗ | ✗ |
| contains | spec | 2774 |  |  |  | 435 | 222 |  |  | 227 | 222 | 210 | 222 |  |  | 210 | 222 |
| contains@vanilla | spec | 53349 |  |  |  | 27003 | 3904 |  |  | 3470 | 3192 | ✗ | ✗ |  |  | ✗ | ✗ |
| deforestation1 | spec | 60 |  |  |  | 5 | 2 |  |  | 3 | 2 | 3 | 2 |  |  | 3 | 2 |
| deforestation1@vanilla | spec | 929 |  |  |  | 49 | 2 |  |  | 8 | 2 | ✗ | ✗ |  |  | ✗ | ✗ |
| depth | spec | 189 |  |  |  | 17 | 7 |  |  | 12 | 7 | 12 | 5 |  |  | 6 | 5 |
| depth@vanilla | spec | 3467 |  |  |  | 116 | 5 |  |  | 48 | 8 | ✗ | ✗ |  |  | ✗ | ✗ |
| doubleAppend | q1 | 90 |  |  |  | 57 | 56 |  |  | 57 | 56 | 57 | 56 |  |  | 57 | 56 |
| doubleAppend | q2 | 115 |  |  |  | 63 | 62 |  |  | 63 | 62 | 62 | 61 |  |  | 62 | 61 |
| doubleAppend | q3 | ∞(5) |  |  |  | 38 | 37 |  |  | 38 | 37 | 36 | 35 |  |  | 36 | 35 |
| doubleAppend@vanilla | q1 | 1074 |  |  |  | ∞(1) | 98 |  |  | ∞(1) | 85 | ∞(0) | ∞(1) |  |  | 75 | 74 |
| doubleAppend@vanilla | q2 | 1588 |  |  |  | 279 | 59 |  |  | 71 | 69 | 429 | 68 |  |  | 56 | 55 |
| doubleAppend@vanilla | q3 | ∞(5) |  |  |  | ∞(5) | 43 |  |  | 65 | 58 | ∞(5) | ∞(5) |  |  | 45 | 44 |
| exDepth | spec | 1993 |  |  |  | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) | 18 | 17 |  |  | 18 | 17 |
| exDepth@vanilla | spec | 39748 |  |  |  | ✗ | ✗ |  |  | 326 | 17 | ✗ | ✗ |  |  | ✗ | ✗ |
| exprEval | q1 | 187 |  |  |  | ∞(1) | 109 |  |  | ∞(1) | 109 | ∞(1) | 123 |  |  | ∞(1) | 123 |
| exprEval | q2 | ∞(2) |  |  |  | ∞(2) | ∞(2) |  |  | ∞(2) | ∞(2) | ∞(2) | ∞(2) |  |  | ∞(2) | ∞(2) |
| exprEval@vanilla | q1 | 3550 |  |  |  | ∞(0) | ∞(1) |  |  | ∞(1) | 147 | ✗ | ✗ |  |  | ∞(1) | ∞(0) |
| exprEval@vanilla | q2 | ∞(2) |  |  |  | ∞(2) | ∞(2) |  |  | ∞(2) | ∞(2) | ✗ | ✗ |  |  | ∞(1) | ∞(1) |
| exprEvalGeneral | q1 | 187 |  |  |  | ∞(1) | 187 |  |  | ∞(1) | 187 | 196 | 196 |  |  | 196 | 196 |
| exprEvalGeneral | q2 | ∞(2) |  |  |  | ∞(2) | ∞(2) |  |  | ∞(2) | ∞(2) | ∞(2) | ∞(2) |  |  | ∞(2) | ∞(2) |
| exprEvalGeneral@vanilla | q1 | 3550 |  |  |  | 3267 | 3265 |  |  | 3253 | 3252 | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) |
| exprEvalGeneral@vanilla | q2 | ∞(2) |  |  |  | ∞(2) | ∞(2) |  |  | ∞(2) | ∞(2) | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) |
| fclMult | q1 | 2847 |  |  |  | 95 | 93 |  |  | 82 | 81 | ✗ | ✗ |  |  | ✗ | ✗ |
| fclMult | q2 | ∞(4) |  |  |  | ∞(4) | ∞(4) |  |  | ∞(4) | ∞(4) | ✗ | ✗ |  |  | ✗ | ✗ |
| fclMult | q3 | ∞(1) |  |  |  | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) | ✗ | ✗ |  |  | ✗ | ✗ |
| fclMult@vanilla | q1 | 100809 |  |  |  | ∞(0) | ∞(0) |  |  | ∞(0) | 2680 | ✗ | ✗ |  |  | ✗ | ✗ |
| fclMult@vanilla | q2 | ∞(0) |  |  |  | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(4) | ✗ | ✗ |  |  | ✗ | ✗ |
| fclMult@vanilla | q3 | ∞(1) |  |  |  | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(1) | ✗ | ✗ |  |  | ✗ | ✗ |
| flip | spec | ∞(0) |  |  |  | 7 | 2 |  |  | 7 | 2 | 3 | 2 |  |  | 3 | 2 |
| flip@vanilla | spec | ∞(0) |  |  |  | ∞(1) | 122 |  |  | 74 | 38 | ∞(0) | ∞(0) |  |  | ✗ | ✗ |
| kmp | q1 | 712 |  |  |  | 553 | 505 |  |  | 553 | 505 | 254 | 253 |  |  | 277 | 276 |
| kmp | q2 | 492 |  |  |  | 372 | 340 |  |  | 372 | 340 | 166 | 165 |  |  | 148 | 147 |
| kmp | q3 | 3624 |  |  |  | 2499 | 2266 |  |  | 2499 | 2266 | 1307 | 1306 |  |  | 1380 | 1379 |
| kmp | q4 | 518 |  |  |  | 414 | 375 |  |  | 414 | 375 | 197 | 196 |  |  | 231 | 230 |
| kmp@vanilla | q1 | 13470 |  |  |  | ∞(0) | ∞(0) |  |  | 655 | 505 | ∞(0) | ∞(0) |  |  | ✗ | ✗ |
| kmp@vanilla | q2 | 9102 |  |  |  | ∞(0) | ∞(0) |  |  | 446 | 340 | ∞(0) | ∞(0) |  |  | ✗ | ✗ |
| kmp@vanilla | q3 | 69046 |  |  |  | ∞(7) | 3747 |  |  | 3205 | 2266 | ∞(7) | ∞(7) |  |  | ✗ | ✗ |
| kmp@vanilla | q4 | 9803 |  |  |  | ∞(2) | 537 |  |  | 488 | 375 | ∞(1) | ∞(2) |  |  | ✗ | ✗ |
| lambdaAppend | q1 | 619 |  |  |  | 19 | 18 |  |  | 19 | 18 | 18 | 16 |  |  | 18 | 16 |
| lambdaAppend | q2 | 839 |  |  |  | 25 | 24 |  |  | 25 | 24 | 26 | 24 |  |  | 26 | 24 |
| lambdaAppend | q3 | 839 |  |  |  | 24 | 23 |  |  | 24 | 23 | 25 | 23 |  |  | 25 | 23 |
| lambdaAppend@vanilla | q1 | 13232 |  |  |  | ∞(0) | ∞(0) |  |  | 10795 | 10786 | ✗ | ✗ |  |  | ✗ | ✗ |
| lambdaAppend@vanilla | q2 | 17904 |  |  |  | ∞(1) | ∞(1) |  |  | 15051 | 15042 | ✗ | ✗ |  |  | ✗ | ✗ |
| lambdaAppend@vanilla | q3 | 17781 |  |  |  | ∞(0) | ∞(0) |  |  | 14952 | 14943 | ✗ | ✗ |  |  | ✗ | ✗ |
| matchAppend | spec | 93 |  |  |  | 36 | 18 |  |  | 36 | 18 | 24 | 11 |  |  | 24 | 11 |
| matchAppend@vanilla | spec | 1591 |  |  |  | 278 | 54 |  |  | 43 | 18 | 283 | 42 |  |  | 23 | 14 |
| matchSimple | spec | 173 |  |  |  | 17 | 10 |  |  | 16 | 10 | 16 | 10 |  |  | 14 | 10 |
| matchSimple@vanilla | spec | 3306 |  |  |  | ∞(6) | 160 |  |  | 27 | 10 | 2063 | 285 |  |  | 12 | 8 |
| maxLength | spec | 435 |  |  |  | 59 | 58 |  |  | 59 | 58 | 59 | 58 |  |  | 59 | 58 |
| maxLength@vanilla | spec | 9772 |  |  |  | 1643 | 225 |  |  | 287 | 209 | ∞(4) | ∞(8) |  |  | ✗ | ✗ |
| path | spec | 11441 |  |  |  | 135 | 79 |  |  | 112 | 76 | 132 | 72 |  |  | 110 | 72 |
| path@vanilla | spec | ∞(5) |  |  |  | ✗ | ✗ |  |  | 3487 | 426 | ✗ | ✗ |  |  | ✗ | ✗ |
| power | q1 | 124 |  |  |  | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) |
| power | q2 | ∞(1) |  |  |  | ∞(1) | 7743 |  |  | ∞(1) | 7743 | ∞(1) | 3457 |  |  | ∞(1) | 3457 |
| power | q3 | ∞(0) |  |  |  | ∞(0) | 904 |  |  | ∞(0) | 904 | ∞(0) | 307 |  |  | ∞(0) | 307 |
| power@vanilla | q1 | 1858 |  |  |  | ∞(0) | 109 |  |  | ∞(1) | ∞(1) | ✗ | ✗ |  |  | ✗ | ✗ |
| power@vanilla | q2 | ∞(1) |  |  |  | ∞(0) | ∞(1) |  |  | ∞(1) | ∞(1) | ✗ | ✗ |  |  | ✗ | ✗ |
| power@vanilla | q3 | ∞(0) |  |  |  | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) | ✗ | ✗ |  |  | ✗ | ✗ |
| propFirst | spec | 163 |  |  |  | 66 | 64 |  |  | 64 | 64 | 68 | 42 |  |  | 68 | 42 |
| propFirst@vanilla | spec | 6633 |  |  |  | 315 | 143 |  |  | 76 | 63 | ✗ | ✗ |  |  | ✗ | ✗ |
| propFirstNando | spec | 303 |  |  |  | 69 | 68 |  |  | 67 | 68 | 68 | 66 |  |  | 68 | 66 |
| propFirstNando@vanilla | spec | 9625 |  |  |  | 664 | 166 |  |  | 231 | 127 | ✗ | ✗ |  |  | ✗ | ✗ |
| propLast | spec | 155 |  |  |  | 112 | 74 |  |  | 103 | 74 | 68 | 42 |  |  | 68 | 42 |
| propLast@vanilla | spec | 8219 |  |  |  | 330 | 47 |  |  | 190 | 55 | ✗ | ✗ |  |  | ✗ | ✗ |
| propLastNando | spec | 366 |  |  |  | 209 | 94 |  |  | 219 | 94 | 68 | 66 |  |  | 68 | 66 |
| propLastNando@vanilla | spec | 21711 |  |  |  | 1111 | 118 |  |  | 573 | 188 | ✗ | ✗ |  |  | ✗ | ✗ |
| queens5 | q1 | 259551 |  |  |  | ✗ | ✗ |  |  | ✗ | ✗ | ✗ | ✗ |  |  | ✗ | ✗ |
| queens5 | q2 | 61007 |  |  |  | ✗ | ✗ |  |  | ✗ | ✗ | ✗ | ✗ |  |  | ✗ | ✗ |
| queens5@vanilla | q1 | ∞(0) |  |  |  | ✗ | ✗ |  |  | ✗ | ✗ | ✗ | ✗ |  |  | ✗ | ✗ |
| queens5@vanilla | q2 | ∞(0) |  |  |  | ✗ | ✗ |  |  | ✗ | ✗ | ✗ | ✗ |  |  | ✗ | ✗ |
| regexp | spec | 340 |  |  |  | 76 | 73 |  |  | 76 | 73 | 41 | 73 |  |  | 41 | 73 |
| regexp@vanilla | spec | 9173 |  |  |  | 8052 | 8051 |  |  | 8031 | 8030 | ∞(3) | ∞(3) |  |  | ∞(3) | ∞(3) |
| relSort | spec | ∞(0) |  |  |  | ∞(1) | 529 |  |  | 12 | 2 | 3 | 2 |  |  | 3 | 2 |
| relSort@vanilla | spec | ∞(0) |  |  |  | ∞(0) | 476 |  |  | ∞(0) | ∞(1) | ✗ | ✗ |  |  | ✗ | ✗ |
| relSortGeneral | q1 | ∞(0) |  |  |  | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) |
| relSortGeneral | q2 | 886 |  |  |  | 725 | 686 |  |  | 725 | 686 | 747 | 676 |  |  | 747 | 676 |
| relSortGeneral@vanilla | q1 | ∞(0) |  |  |  | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) |
| relSortGeneral@vanilla | q2 | 19240 |  |  |  | 17996 | 17979 |  |  | 17965 | 17964 | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) |
| rev | spec | 96 |  |  |  | 11 | 2 |  |  | 3 | 2 | 3 | 2 |  |  | 3 | 2 |
| rev@vanilla | spec | 1209 |  |  |  | 52 | 2 |  |  | 25 | 2 | ∞(1) | ∞(1) |  |  | ✗ | ✗ |
| revAcc | spec | ∞(1) |  |  |  | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) |
| revAcc@vanilla | spec | ∞(1) |  |  |  | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) |
| revGeneral | q1 | ∞(1) |  |  |  | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) | 189 | 189 |  |  | 189 | 189 |
| revGeneral | q2 | 262 |  |  |  | 254 | 254 |  |  | 254 | 254 | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) |
| revGeneral@vanilla | q1 | ∞(1) |  |  |  | ∞(1) | ∞(1) |  |  | ∞(1) | ∞(1) | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) |
| revGeneral@vanilla | q2 | 3325 |  |  |  | 2991 | 2982 |  |  | 2976 | 2975 | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) |
| rotatePrune | spec | ∞(4) |  |  |  | 12 | 7 |  |  | 12 | 7 | 10 | 7 |  |  | 10 | 7 |
| rotatePrune@vanilla | spec | ∞(4) |  |  |  | 85 | 40 |  |  | 18 | 8 | ✗ | ✗ |  |  | 28 | 25 |
| sort | spec | 185 |  |  |  | 8 | 2 |  |  | 3 | 2 | 3 | 2 |  |  | 3 | 2 |
| sort@vanilla | spec | 3884 |  |  |  | ✗ | ✗ |  |  | ✗ | ✗ | ✗ | ✗ |  |  | ✗ | ✗ |
| sortGeneral | q1 | 254 |  |  |  | ✗ | ✗ |  |  | ✗ | ✗ | ∞(0) | ∞(1) |  |  | ∞(0) | ∞(1) |
| sortGeneral | q2 | ∞(6) |  |  |  | ✗ | ✗ |  |  | ✗ | ✗ | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) |
| sortGeneral@vanilla | q1 | 5304 |  |  |  | 4937 | 4936 |  |  | 4924 | 4923 | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) |
| sortGeneral@vanilla | q2 | ∞(2) |  |  |  | ∞(2) | ∞(2) |  |  | ∞(2) | ∞(2) | ∞(6) | ∞(6) |  |  | ∞(6) | ∞(6) |
| tmInterp | q1 | 409 |  |  |  | 88 | 87 |  |  | 88 | 79 | ∞(0) | 559 |  |  | ∞(0) | 559 |
| tmInterp | q2 | ∞(3) |  |  |  | ∞(3) | ∞(3) |  |  | ∞(3) | ∞(3) | ∞(2) | ∞(3) |  |  | ∞(2) | ∞(3) |
| tmInterp | q3 | ∞(7) |  |  |  | ∞(7) | ∞(7) |  |  | ∞(7) | 174 | ∞(7) | ∞(7) |  |  | ∞(7) | ∞(7) |
| tmInterp@vanilla | q1 | 13715 |  |  |  | ∞(0) | 619 |  |  | 460 | 396 | ✗ | ✗ |  |  | ✗ | ✗ |
| tmInterp@vanilla | q2 | ∞(3) |  |  |  | ∞(1) | ∞(3) |  |  | ∞(3) | ∞(3) | ✗ | ✗ |  |  | ✗ | ✗ |
| tmInterp@vanilla | q3 | ∞(7) |  |  |  | ∞(1) | ∞(7) |  |  | ∞(7) | ∞(7) | ✗ | ✗ |  |  | ✗ | ✗ |
| typecheck | q1 | 442 |  |  |  | 270 | 274 |  |  | 270 | 274 | 145 | 146 |  |  | 145 | 146 |
| typecheck | q2 | 156 |  |  |  | ∞(1) | 149 |  |  | ∞(1) | 149 | 115 | 117 |  |  | 115 | 117 |
| typecheck@vanilla | q1 | 14081 |  |  |  | 11197 | 11195 |  |  | 11165 | 11164 | 10363 | 10362 |  |  | 10363 | 10362 |
| typecheck@vanilla | q2 | 3256 |  |  |  | 2918 | 2932 |  |  | 2902 | 2901 | ∞(0) | ∞(0) |  |  | ∞(0) | ∞(0) |
| unifier | spec | 681 |  |  |  | 307 | 323 |  |  | 307 | 323 | 241 | 241 |  |  | 241 | 241 |
| unifier@vanilla | spec | ∞(7) |  |  |  | 20073 | 5873 |  |  | 42704 | 42128 | ✗ | ✗ |  |  | ✗ | ✗ |
| uptoSum | spec | 405 |  |  |  | 3 | 2 |  |  | 3 | 2 | 3 | 2 |  |  | 3 | 2 |
| uptoSum@vanilla | spec | 9726 |  |  |  | ∞(0) | 195 |  |  | ∞(0) | 157 | ✗ | ✗ |  |  | ✗ | ✗ |
| vanilla | spec | 405 |  |  |  | 44 | 31 |  |  | 44 | 31 | 53 | 31 |  |  | 53 | 31 |
| vanilla@vanilla | spec | 6646 |  |  |  | 369 | 119 |  |  | 191 | 84 | ✗ | ✗ |  |  | ✗ | ✗ |

## Jones optimality (vanilla self-interpreter)

For each benchmark p, `p@vanilla` runs p through the vanilla self-interpreter (`src/Measure/Vanilla.hs`). `solved`: queries of p@vanilla solved. `vs direct`: geometric mean of steps(S(p@vanilla))/steps(original p) (a Jones-optimal S gives <= 1). `vs S(p)`: steps(S(p@vanilla))/steps(S(p)), the interpretive overhead S fails to remove (1 = none left). `optimal`: queries with steps(S(p@vanilla)) <= steps(S(p)). Ratios over queries both sides solve.

| specializer | solved | wrong | spec fail | vs direct | vs S(p) | optimal |
|---|---:|---:|---:|---:|---:|---:|
| original | 45/68 | 0 | 0 | 22.876 | 22.876 | 0 |
| cpd-det | 26/63 | 0 | 4 | 5.035 | 16.633 | 0 |
| cpd-det+opt+sched | 39/63 | 0 | 4 | 0.967 | 4.841 | 6 |
| cpd-la | 41/63 | 0 | 3 | 2.381 | 10.564 | 1 |
| cpd-la+opt+sched | 46/63 | 0 | 3 | 1.176 | 7.402 | 10 |
| conspd-la | 7/24 | 0 | 27 | 0.696 | 2.193 | 3 |
| conspd-la+opt+sched | 7/24 | 0 | 27 | 0.577 | 2.342 | 2 |
| conspd-det | 4/29 | 0 | 24 | 7.506 | 29.447 | 0 |
| conspd-det+opt+sched | 4/29 | 0 | 24 | 1.792 | 9.633 | 0 |

## Wrong results and specialization failures

- appLast@vanilla * **conspd-la**: spec-fail: Residualization failed: invocation of the undefined relation -
- appLast@vanilla * **conspd-la+opt+sched**: spec-fail: Residualization failed: invocation of the undefined relation -
- contains@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- contains@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- contains@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- contains@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- deforestation1@vanilla * **conspd-la**: spec-fail: Residualization failed: invocation of the undefined relation -
- deforestation1@vanilla * **conspd-la+opt+sched**: spec-fail: Residualization failed: invocation of the undefined relation -
- deforestation1@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- deforestation1@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- depth@vanilla * **conspd-la**: spec-fail: Residualization failed: invocation of the undefined relation -
- depth@vanilla * **conspd-la+opt+sched**: spec-fail: Residualization failed: invocation of the undefined relation -
- depth@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- depth@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- exDepth@vanilla * **cpd-det**: spec-fail: <<timeout>> -
- exDepth@vanilla * **cpd-det+opt+sched**: spec-fail: <<timeout>> -
- exDepth@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- exDepth@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- exDepth@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- exDepth@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- flip@vanilla * **conspd-la**: spec-fail: Residualization failed: invocation of the undefined relation -
- flip@vanilla * **conspd-la+opt+sched**: spec-fail: Residualization failed: invocation of the undefined relation -
- maxLength@vanilla * **conspd-la**: spec-fail: Residualization failed: invocation of the undefined relation -
- maxLength@vanilla * **conspd-la+opt+sched**: spec-fail: Residualization failed: invocation of the undefined relation -
- rev@vanilla * **conspd-la**: spec-fail: Residualization failed: invocation of the undefined relation -
- rev@vanilla * **conspd-la+opt+sched**: spec-fail: Residualization failed: invocation of the undefined relation -
- rotatePrune@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- rotatePrune@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- uptoSum@vanilla * **conspd-la**: spec-fail: Residualization failed: invocation of the undefined relation -
- uptoSum@vanilla * **conspd-la+opt+sched**: spec-fail: Residualization failed: invocation of the undefined relation -
- uptoSum@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- uptoSum@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- vanilla@vanilla * **conspd-la**: spec-fail: Residualization failed: invocation of the undefined relation -
- vanilla@vanilla * **conspd-la+opt+sched**: spec-fail: Residualization failed: invocation of the undefined relation -
- vanilla@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- vanilla@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- path@vanilla * **cpd-det**: spec-fail: <<timeout>> -
- path@vanilla * **cpd-det+opt+sched**: spec-fail: <<timeout>> -
- path@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- path@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- path@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- path@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- propFirst@vanilla * **conspd-la**: spec-fail: Residualization failed: invocation of the undefined relation -
- propFirst@vanilla * **conspd-la+opt+sched**: spec-fail: Residualization failed: invocation of the undefined relation -
- propFirst@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- propFirst@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- propFirstNando@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- propFirstNando@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- propFirstNando@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- propFirstNando@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- propLast@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- propLast@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- propLast@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- propLast@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- propLastNando@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- propLastNando@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- propLastNando@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- propLastNando@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- relSort@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- relSort@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- relSort@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- relSort@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- sort@vanilla * **cpd-det**: spec-fail: <<timeout>> -
- sort@vanilla * **cpd-det+opt+sched**: spec-fail: <<timeout>> -
- sort@vanilla * **cpd-la**: spec-fail: <<timeout>> -
- sort@vanilla * **cpd-la+opt+sched**: spec-fail: <<timeout>> -
- sort@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- sort@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- sort@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- sort@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- sortGeneral * **cpd-det**: spec-fail: Residualization failed: invocation of an undefined relation. -
- sortGeneral * **cpd-det+opt+sched**: spec-fail: Residualization failed: invocation of an undefined relation. -
- sortGeneral * **cpd-la**: spec-fail: Residualization failed: invocation of an undefined relation. -
- sortGeneral * **cpd-la+opt+sched**: spec-fail: Residualization failed: invocation of an undefined relation. -
- unifier@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- unifier@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- unifier@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- unifier@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- coloring@vanilla * **cpd-la**: spec-fail: <<timeout>> -
- coloring@vanilla * **cpd-la+opt+sched**: spec-fail: <<timeout>> -
- coloring@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- coloring@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- coloring@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- coloring@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- exprEval@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- exprEval@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- queens5 * **cpd-det**: spec-fail: <<timeout>> -
- queens5 * **cpd-det+opt+sched**: spec-fail: <<timeout>> -
- queens5 * **cpd-la**: spec-fail: <<timeout>> -
- queens5 * **cpd-la+opt+sched**: spec-fail: <<timeout>> -
- queens5 * **conspd-la**: spec-fail: <<timeout>> -
- queens5 * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- queens5 * **conspd-det**: spec-fail: <<timeout>> -
- queens5 * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- queens5@vanilla * **cpd-det**: spec-fail: <<timeout>> -
- queens5@vanilla * **cpd-det+opt+sched**: spec-fail: <<timeout>> -
- queens5@vanilla * **cpd-la**: spec-fail: <<timeout>> -
- queens5@vanilla * **cpd-la+opt+sched**: spec-fail: <<timeout>> -
- queens5@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- queens5@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- queens5@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- queens5@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- binFactor@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- binFactor@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- binFactor@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- binFactor@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- binMul@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- binMul@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- binMul@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- binMul@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- fclMult * **conspd-la**: spec-fail: <<timeout>> -
- fclMult * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- fclMult * **conspd-det**: spec-fail: <<timeout>> -
- fclMult * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- fclMult@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- fclMult@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- fclMult@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- fclMult@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- kmp@vanilla * **conspd-la**: spec-fail: Residualization failed: invocation of the undefined relation -
- kmp@vanilla * **conspd-la+opt+sched**: spec-fail: Residualization failed: invocation of the undefined relation -
- lambdaAppend@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- lambdaAppend@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- lambdaAppend@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- lambdaAppend@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- power@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- power@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- power@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- power@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
- tmInterp@vanilla * **conspd-la**: spec-fail: <<timeout>> -
- tmInterp@vanilla * **conspd-la+opt+sched**: spec-fail: <<timeout>> -
- tmInterp@vanilla * **conspd-det**: spec-fail: <<timeout>> -
- tmInterp@vanilla * **conspd-det+opt+sched**: spec-fail: <<timeout>> -
