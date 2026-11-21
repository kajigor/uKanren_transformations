-- filter even (map inc xs)
even n = n == O | (fresh m in n == S (S m) & even m);
odd n = n == S O | (fresh m in n == S (S m) & odd m);

mapInc xs ys = (xs == [] & ys == []) |
  (fresh h, t, t1 in xs == (h :: t) & ys == (S h :: t1) & mapInc t t1);

filterEven xs ys = (xs == [] & ys == []) |
  (fresh h, t, t1 in xs == (h :: t) & even h & ys == (h :: t1) & filterEven t t1) |
  (fresh h, t in xs == (h :: t) & odd h & filterEven t ys);

filterMap xs ys = fresh zs in mapInc xs zs & filterEven zs ys;

? filterMap xs ys
