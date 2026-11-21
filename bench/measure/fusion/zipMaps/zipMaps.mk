-- zipWith add (map inc xs) (map inc ys): two producers consumed in lockstep.
add x y z = (x == O & y == z) | (fresh x1, z1 in x == S x1 & z == S z1 & add x1 y z1);

mapInc xs ys = (xs == [] & ys == []) |
  (fresh h, t, t1 in xs == (h :: t) & ys == (S h :: t1) & mapInc t t1);

zipAdd xs ys zs = (xs == [] & ys == [] & zs == []) |
  (fresh a, b, c, xt, yt, zt in xs == (a :: xt) & ys == (b :: yt) & zs == (c :: zt) & add a b c & zipAdd xt yt zt);

zipMaps xs ys zs = fresh xs1, ys1 in mapInc xs xs1 & mapInc ys ys1 & zipAdd xs1 ys1 zs;

? zipMaps xs ys zs
