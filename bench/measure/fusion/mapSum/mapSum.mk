-- fold . map fusion: sum (map double xs).  CPD should remove the
-- intermediate list ys and traverse xs once.
add x y z = (x == O & y == z) | (fresh x1, z1 in x == S x1 & z == S z1 & add x1 y z1);

double x y = add x x y;

mapDouble xs ys = (xs == [] & ys == []) |
  (fresh h, t, h1, t1 in xs == (h :: t) & ys == (h1 :: t1) & double h h1 & mapDouble t t1);

sum xs s = (xs == [] & s == O) |
  (fresh h, t, s1 in xs == (h :: t) & sum t s1 & add h s1 s);

mapSum xs s = fresh ys in mapDouble xs ys & sum ys s;

? mapSum xs s
