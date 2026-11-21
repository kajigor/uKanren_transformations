-- dot product: fold (+) 0 (zipWith mul xs ys)
add x y z = (x == O & y == z) | (fresh x1, z1 in x == S x1 & z == S z1 & add x1 y z1);

mul x y z = (x == O & z == O) | (fresh x1, z1 in x == S x1 & mul x1 y z1 & add y z1 z);

zipMul xs ys ps = (xs == [] & ys == [] & ps == []) |
  (fresh a, b, c, xt, yt, pt in xs == (a :: xt) & ys == (b :: yt) & ps == (c :: pt) & mul a b c & zipMul xt yt pt);

sum xs s = (xs == [] & s == O) |
  (fresh h, t, s1 in xs == (h :: t) & sum t s1 & add h s1 s);

dot xs ys d = fresh ps in zipMul xs ys ps & sum ps d;

? dot xs ys d
