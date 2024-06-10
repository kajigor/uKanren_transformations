[![GitHub CI](https://github.com/kajigor/uKanren_transformations/workflows/CI/badge.svg)](https://github.com/kajigor/uKanren_transformations/actions)

# uKanren-transformation

In this branch, we've explored the integration of Mode Analysis, Binding Time Analysis, Conjunctive Partial Deduction, and Functional Conversion. 

The implementation of Binding Time Analysis was inspired by one, described in the work "Maurice Bruynooghe, Michael Leuschel, and Konstantinos Sagonas. A Polyvariant
Binding-Time Analysis for Off-line Partial Deduction. 2000". It can be seen here [BTA](https://github.com/kajigor/uKanren_transformations/tree/BTA/src/BTA).

Command to run the conversion: 

```stack run -- --funTransformer -i input_file --rel="relName" --ground=[groundVars] --deduction=Offline```

We evaluated the approach by comparing it with two other approaches: the first (<ins>Simple Conversion</ins>) uses simple Functional Conversion, and the second (<ins>Online Conversion</ins>) runs Conjunctive Partial Deduction before the conversion. We compared approaches on the [DPPD](https://github.com/leuschel/DPPD) benchmarks and some examples of the Verifier-to-Solver approach.

Command to run the benchmarks:

```stack bench```

Results of the evaluation are presented below

#### DPPD ([examples](https://github.com/kajigor/uKanren_transformations/tree/BTA/test/resources/newSyntax/withTypeAnnotations/dppd), [benchmarks](https://github.com/kajigor/uKanren_transformations/tree/BTA/bench/DPPD)):

|               | Simple Conversion | Online Conversion  | Offline Conversion |
| -------------:| -----------------:| ------------------:| ------------------:| 
|       applast | 0.87 $\micro s$   | 0.10 $\micro s$    |  0.10 $\micro s$   |
|      contains | 196.00 $\micro s$ | 170.00 $\micro s$  | 758.00 $\micro s$  |
|deforestation1 | 1.51 $\micro s$   |   0.02 $\micro s$  |   0.02 $\micro s$  |
|deforestation2 | 4420.00 $\micro s$  | 29300.00 $\micro s$   |   0.08 $\micro s$  |
|       depth   | timeout           |  0.24 $\micro s$   | 0.23 $\micro s$    |
|  doubleAppend | 14.10 $\micro s$  | 126.00 $\micro s$  | 2.21 $\micro s$    |
|      exDepth  | timeout           |   timeout          | 2.01 $\micro s$    |
|         flip  | 2.77 $\micro s$   |  0.18 $\micro s$   | 0.19 $\micro s$    |
|        match  | 16.20 $\micro s$  |  0.78 $\micro s$   | 0.78 $\micro s$    |
|  matchSimple  |8660.00 $\micro s$ |  19.10 $\micro s$  | 0.72 $\micro s$    |
|  multiply     | 0.17 $\micro s$   |  0.11 $\micro s$   | 0.12 $\micro s$    |
|       regexp  | 5.80 $\micro s$   |  7.93 $\micro s$   | 9.18 $\micro s$    |
|       remove  | 138.00 $\micro s$ | 54.40 $\micro s$   | 48.20 $\micro s$   |
|   rotatePrune | 48.80 $\micro s$  | 71.20 $\micro s$   | 159.00 $\micro s$  |
|      upto.Sum | 16.20 $\micro s$  | 0.04 $\micro s$    | 0.04 $\micro s$    |
|       vanilla |         timeout   | 6.23 $\micro s$    | 1.38 $\micro s$    |
