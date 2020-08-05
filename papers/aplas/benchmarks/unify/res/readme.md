# Unify

Unification of two terms

* `unify.pdf`
  * `first`
    * 10000 iterations
    * 1 result
    * Query: `unify q (f(X, a)) (f(a, X))`
  * `second`
    * 100 iterations
    * 3 results
    * Query: `unify q (append([a, b], [c, d], Ls)) (append([X | Xs], Ys, [X | Zs]))`
  * `third`
    * 10 iterations
    * 3 results
    * Query: `unify q (f(X, X, g(Z, b))) (f(g(a, Z), Y, Y))`