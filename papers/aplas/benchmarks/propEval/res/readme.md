# Evaluator of propositional formulas

* `prop.pdf`
  * 10 iterations
  * 1000 results
  * Query: formulas with 2 variables which evaluate to true

Implementations:

* `FirstPlain`: boolean connectives are the first conjuncts, table-based implementation
* `FirstNando`: boolean connectives are the first conjuncts, implementation via `nando`
* `LastPlain`: boolean connectives are the last conjuncts, table-based implementation
* `LastNando`: boolean connectives are the last conjuncts, implementation via `nando`
