-- N-queens as verifier-to-solver: a generic placement checker specialized
-- for a board of 5 unknown queens (generate-and-test).
neq x y =
  (fresh y1 in x == O & y == S y1) |
  (fresh x1 in x == S x1 & y == O) |
  (fresh x1, y1 in x == S x1 & y == S y1 & neq x1 y1);

-- a /= b + d
notplus d b a =
  (d == O & neq a b) |
  (fresh d1 in d == S d1 & a == O) |
  (fresh d1, a1 in d == S d1 & a == S a1 & notplus d1 b a1);

safe a b d = neq a b & notplus d b a & notplus d a b;

noattack q qs d =
  qs == [] |
  (fresh h, t in qs == (h :: t) & safe q h d & noattack q t (S d));

queens qs =
  qs == [] |
  (fresh h, t in qs == (h :: t) & noattack h t (S O) & queens t);

dom x = x == O | x == S O | x == S (S O) | x == S (S (S O)) | x == S (S (S (S O)));

domall qs = qs == [] | (fresh h, t in qs == (h :: t) & dom h & domall t);

solve qs = domall qs & queens qs;

? solve [a, b, c, d, e]
