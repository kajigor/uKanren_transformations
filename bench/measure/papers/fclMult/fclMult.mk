-- Interpreter for Jones's flowchart language FCL, specialized to a fixed FCL
-- program (first Futamura projection). Source: N. D. Jones, C. K. Gomard,
-- P. Sestoft, "Partial Evaluation and Automatic Program Generation", Prentice
-- Hall, 1993, Ch. 4 (the flow chart language and mix, whose first projection
-- compiles a program by specializing an interpreter for it).
-- A program is a list of basic blocks  Blk label [Asg var expr, ...] jump  with
-- jump = Goto l | IfZ e l_zero l_nonzero | Return e; the first block is the
-- entry. Expressions over Peano: Const v | Var n | Succ e | Pred e. The store is
-- a fixed-size list [x, y, z, t] indexed by Peano variable numbers.
--
-- Static: the multiplication program
--   init:  z := 0; goto test
--   test:  if x = 0 goto done else outer
--   outer: x := x - 1; t := y; goto inner
--   inner: if t = 0 goto test else step
--   step:  t := t - 1; z := z + 1; goto inner
--   done:  return z
-- Dynamic: inputs x, y and result r.
-- Ideal specializer: two mutually recursive residual relations (one per loop
-- header, test and inner) over plain Peano arguments x, y, z, t -- no block
-- lookup, no store list, no expression dispatch (as in JGS's compiled output).
pred a b = (a == O & b == O) | a == S b;

nth n st v =
  (fresh t in n == O & st == (v :: t)) |
  (fresh n1, h, t in n == S n1 & st == (h :: t) & nth n1 t v);

upd n v st st2 =
  (fresh h, t in n == O & st == (h :: t) & st2 == (v :: t)) |
  (fresh n1, h, t, t2 in n == S n1 & st == (h :: t) & st2 == (h :: t2) & upd n1 v t t2);

evalE e st v =
  (e == Const v) |
  (fresh n in e == Var n & nth n st v) |
  (fresh e1, v1 in e == Succ e1 & v == S v1 & evalE e1 st v1) |
  (fresh e1, v1 in e == Pred e1 & evalE e1 st v1 & pred v1 v);

assigns asg st st2 =
  (asg == [] & st2 == st) |
  (fresh n, e, rest, v, st1 in
     asg == (Asg n e :: rest) & evalE e st v & upd n v st st1 & assigns rest st1 st2);

lookupBlk prog l asg j =
  fresh l1, a1, j1, rest in
    prog == (Blk l1 a1 j1 :: rest) &
    ((l1 == l & a1 == asg & j1 == j) | lookupBlk rest l asg j);

block prog l st r =
  fresh asg, j, st1 in lookupBlk prog l asg j & assigns asg st st1 & jump prog j st1 r;

jump prog j st r =
  (fresh l in j == Goto l & block prog l st r) |
  (fresh e, l1, l2, v in
     j == IfZ e l1 l2 & evalE e st v &
     ((v == O & block prog l1 st r) | (fresh v1 in v == S v1 & block prog l2 st r))) |
  (fresh e in j == Return e & evalE e st r);

run prog x y r =
  fresh l, a, j, rest in prog == (Blk l a j :: rest) & block prog l [x, y, O, O] r;

? run [Blk Init [Asg (S (S O)) (Const O)] (Goto Test), Blk Test [] (IfZ (Var O) Done Outer), Blk Outer [Asg O (Pred (Var O)), Asg (S (S (S O))) (Var (S O))] (Goto Inner), Blk Inner [] (IfZ (Var (S (S (S O)))) Test Step), Blk Step [Asg (S (S (S O))) (Pred (Var (S (S (S O))))), Asg (S (S O)) (Succ (Var (S (S O))))] (Goto Inner), Blk Done [] (Return (Var (S (S O))))] x y r
