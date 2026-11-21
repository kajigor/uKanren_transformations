-- tupling: two traversals of the same list (sum and length, e.g. for an
-- average).  CPD should produce one traversal computing both.
add x y z = (x == O & y == z) | (fresh x1, z1 in x == S x1 & z == S z1 & add x1 y z1);

sum xs s = (xs == [] & s == O) |
  (fresh h, t, s1 in xs == (h :: t) & sum t s1 & add h s1 s);

len xs n = (xs == [] & n == O) |
  (fresh h, t, n1 in xs == (h :: t) & n == S n1 & len t n1);

sumLength xs s n = sum xs s & len xs n;

? sumLength xs s n
