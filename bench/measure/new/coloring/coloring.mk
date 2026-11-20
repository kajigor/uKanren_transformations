-- 3-colouring of a fixed graph given as an edge list of vertex indices:
-- the checker interprets the edge list; specialization should compile it away.
color c = c == R | c == G | c == B;

diff a b =
  (a == R & b == G) | (a == R & b == B) | (a == G & b == R) |
  (a == G & b == B) | (a == B & b == R) | (a == B & b == G);

colorall cs = cs == [] | (fresh h, t in cs == (h :: t) & color h & colorall t);

nth n l x =
  (fresh t in n == O & l == (x :: t)) |
  (fresh n1, h, t in n == S n1 & l == (h :: t) & nth n1 t x);

okedges es cs =
  es == [] |
  (fresh a, b, ca, cb, t in
     es == (Pair a b :: t) & nth a cs ca & nth b cs cb & diff ca cb & okedges t cs);

coloring es cs = colorall cs & okedges es cs;

? coloring [Pair O (S O), Pair O (S (S O)), Pair (S O) (S (S O)), Pair (S O) (S (S (S O))), Pair (S (S O)) (S (S (S (S O)))), Pair (S (S (S O))) (S (S (S (S O)))), Pair (S (S (S O))) (S (S (S (S (S O))))), Pair (S (S (S (S O)))) (S (S (S (S (S O)))))] [c0, c1, c2, c3, c4, c5]
