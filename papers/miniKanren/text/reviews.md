# Review 9A

* It would be nice to see some discussion of why this particular heuristic was selected. 
* A systematic investigation of which features of a relation are most predictive of a particular unfolding leading to a contradiction would be an interesting piece of work in itself.

# Review 9B

* review=true
* Performance results are underwhelming
* Why those examples were chosen 
* Why this particular implementation of unification example
  * Why these particular inputs 
* Introduce non-concjunctive partial deduction sooner.
* The typical miniKanren search strategy is itself interleaving over a
left-to-right depth-first search. Are those sub-portions of the
evaluation more amenable to the older Prolog techniques? 
* Complete search does not mean re-ordering goals is safe, full stop. 
  * Reordering _finitely_ many goals, offline/ahead-of-time? 
* Does "different directions" mean "multiple modes"?
  * If so, use the established terminoloy 
  * Otherwise distinguish it here.
* "In general, it is possible to create a solver for a recognizer by
translating it into miniKanren and running it in the appropriate
direction" This makes me want to ask P/NP questions Should those be
addressed? Does separation foreclose this possibility?
* What is meant by performance? Time? Memory? Specific metric is better
  be specified.
* Had you considered specializing your evalo to Horn clauses?

# Review 9C

* Which approach might be better for which kinds of programs, such (informal) feeling will be helpful to know the examples used in this paper are not too specific.
* The evaluation results in Section 4.2 uses the heuristic in Section 3.2. If we remove the heuristic, how is the result?
