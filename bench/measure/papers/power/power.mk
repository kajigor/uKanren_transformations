-- Classic first example of partial evaluation: x^n specialized to a static
-- exponent n. Source: N. D. Jones, C. K. Gomard, P. Sestoft, "Partial Evaluation
-- and Automatic Program Generation", Prentice Hall, 1993, Ch. 1 (the power
-- function, the textbook introductory example); cf. A. P. Ershov, "On the
-- partial computation principle", Information Processing Letters 6(2), 1977.
--
-- Static: exponent n = 3 (Peano). Dynamic: base x, result r.
-- Ideal specializer: power 3 x r  ==>  mul x x t1 & mul x t1 r (plus the
-- trivial mul x 1), i.e. the recursion on n completely unrolled, no test on n.
add x y z = (x == O & y == z) | (fresh x1, z1 in x == S x1 & z == S z1 & add x1 y z1);

mul x y z = (x == O & z == O) | (fresh x1, z1 in x == S x1 & mul x1 y z1 & add y z1 z);

power n x r =
  (n == O & r == S O) |
  (fresh n1, r1 in n == S n1 & power n1 x r1 & mul x r1 r);

? power (S (S (S O))) x r
