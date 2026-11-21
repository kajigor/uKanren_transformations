-- A relational interpreter for a small call-by-value Scheme-like lambda
-- calculus, specialized to a fixed object program (first Futamura projection).
-- Source: W. E. Byrd, E. Holk, D. P. Friedman, "miniKanren, live and untagged:
-- quine generation via relational interpreters (programming pearl)", Scheme
-- Workshop 2012; W. E. Byrd, M. Ballantyne, G. Rosenblatt, M. Might, "A unified
-- approach to solving seven programming problems (functional pearl)", ICFP 2017
-- (running append backward through the interpreter). Deviations, because the DSL
-- has no disequality constraints: variables are de Bruijn indices (env is a list
-- of values, looked up with nth) and quoted data contain no closures (no absento
-- needed); `ConsE a d` builds a pair and `Case s en ec` is the list eliminator (ec is under two binders: head
-- = Var 0, tail = Var 1). Recursion is via self-application (U combinator).
-- Values: data lists [] / (h :: t) of atoms A, B, C, ..., and closures Clo body env.
--
-- Object program (free variables x = Var 0, y = Var 1 in the initial env):
--   app = \f. \xs. \ys. case xs of [] -> ys | h :: t -> h :: ((f f) t) ys
--   main = ((app app) x) y
-- Static: the object program. Dynamic: values of x and y, result v.
-- Ideal specializer: the residual is plain appendo x y v (one recursive relation
-- on data lists), with no expression dispatch, no environments and no closures.
lookup n env v =
  (fresh t in n == O & env == (v :: t)) |
  (fresh n1, h, t in n == S n1 & env == (h :: t) & lookup n1 t v);

eval e env v =
  (fresh n in e == Var n & lookup n env v) |
  (e == Quote v) |
  (fresh b in e == Lam b & v == Clo b env) |
  (fresh f, a, b, cenv, av in
     e == App f a & eval f env (Clo b cenv) & eval a env av & eval b (av :: cenv) v) |
  (fresh a, d, va, vd in e == ConsE a d & v == (va :: vd) & eval a env va & eval d env vd) |
  (fresh s, en, ec, vs in
     e == Case s en ec & eval s env vs &
     ((vs == [] & eval en env v) |
      (fresh h, t in vs == (h :: t) & eval ec (h :: t :: env) v)));

? eval (App (App (App (Lam (Lam (Lam (Case (Var (S O)) (Var O) (ConsE (Var O) (App (App (App (Var (S (S (S (S O))))) (Var (S (S (S (S O)))))) (Var (S O))) (Var (S (S O))))))))) (Lam (Lam (Lam (Case (Var (S O)) (Var O) (ConsE (Var O) (App (App (App (Var (S (S (S (S O))))) (Var (S (S (S (S O)))))) (Var (S O))) (Var (S (S O)))))))))) (Var O)) (Var (S O))) [x, y] v
