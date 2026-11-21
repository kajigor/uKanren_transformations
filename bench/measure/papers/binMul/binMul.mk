-- Relational binary arithmetic, multiplication by a static constant.
-- Source: O. Kiselyov, W. E. Byrd, D. P. Friedman, C. Shan, "Pure, declarative,
-- and constructive arithmetic relations (declarative pearl)", FLOPS 2008, LNCS
-- 4989 (also The Reasoned Schemer, ch. 7-8). The bounds (poso, gt1o, boundMulto)
-- are the paper's and make multo terminate whenever p is ground or n, m are.
--
-- Static: n = 5 = [B1, B0, B1]. Dynamic: m, p.
-- Ideal specializer: p = 5*m compiled to shift-and-add, i.e. pluso (B0 :: B0 :: m) m p
-- (4m + m) with all case analysis on n and the boundMulto recursion on n removed;
-- the backward direction (division by 5) should then be a single addero pass.
-- Kiselyov, Byrd, Friedman, Shan, FLOPS 2008, transcribed clause by clause
-- (names: >1o -> gt1o, *o -> multo, odd-*o -> oddMulto, bound-*o -> boundMulto).
-- Numbers are little-endian bit lists without trailing zeros: 6 = [B0, B1, B1].
fullAdder b x y r c =
  (b == B0 & x == B0 & y == B0 & r == B0 & c == B0) |
  (b == B1 & x == B0 & y == B0 & r == B1 & c == B0) |
  (b == B0 & x == B1 & y == B0 & r == B1 & c == B0) |
  (b == B1 & x == B1 & y == B0 & r == B0 & c == B1) |
  (b == B0 & x == B0 & y == B1 & r == B1 & c == B0) |
  (b == B1 & x == B0 & y == B1 & r == B0 & c == B1) |
  (b == B0 & x == B1 & y == B1 & r == B0 & c == B1) |
  (b == B1 & x == B1 & y == B1 & r == B1 & c == B1);

poso n = fresh a, d in n == (a :: d);

gt1o n = fresh a, b, d in n == (a :: b :: d);

addero d n m r =
  (d == B0 & m == [] & n == r) |
  (d == B0 & n == [] & m == r & poso m) |
  (d == B1 & m == [] & addero B0 n [B1] r) |
  (d == B1 & n == [] & poso m & addero B0 [B1] m r) |
  (n == [B1] & m == [B1] & (fresh a, c in r == [a, c] & fullAdder d B1 B1 a c)) |
  (n == [B1] & genAddero d n m r) |
  (m == [B1] & gt1o n & gt1o r & addero d [B1] n r) |
  (gt1o n & genAddero d n m r);

genAddero d n m r =
  fresh a, b, c, e, x, y, z in
    n == (a :: x) & m == (b :: y) & poso y & r == (c :: z) & poso z &
    fullAdder d a b c e & addero e x y z;

pluso n m k = addero B0 n m k;

multo n m p =
  (n == [] & p == []) |
  (poso n & m == [] & p == []) |
  (n == [B1] & poso m & m == p) |
  (gt1o n & m == [B1] & n == p) |
  (fresh x, z in n == (B0 :: x) & poso x & p == (B0 :: z) & poso z & gt1o m & multo x m z) |
  (fresh x, y in n == (B1 :: x) & poso x & m == (B0 :: y) & poso y & multo m n p) |
  (fresh x, y in n == (B1 :: x) & poso x & m == (B1 :: y) & poso y & oddMulto x n m p);

oddMulto x n m p = fresh q in boundMulto q p n m & multo x m q & pluso (B0 :: q) m p;

boundMulto q p n m =
  (q == [] & (fresh a, d in p == (a :: d))) |
  (fresh a0, x, a1, y, z, a2 in
     q == (a0 :: x) & p == (a1 :: y) &
     ((n == [] & m == (a2 :: z) & boundMulto x y z []) |
      (fresh a3 in n == (a3 :: z) & boundMulto x y z m)));

? multo [B1, B0, B1] m p
