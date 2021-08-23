import nat

listo x = x == [] | (fresh h, t in x == (h :: t) & listo t);

membero x list =
  fresh h, t in
    list == (h :: t) & (x == h | membero x t);

inBotho x ys zs =
  membero x ys & membero x zs;

nilo l = l == [];

singletono l x = l == [x];

maxLengtho x m l =
  maxo x m & lengtho x l;

maxMino x m l =
  maxo x m & mino x l;

copy l c =
  l == [] & c == [] |
  (fresh h, t, t' in
    l == (h :: t) &
    c == (h :: t') &
    copy t t');

copy2 l c =
  l == [] & c == [] |
  (fresh h in l == [h] & c == [h]) |
  (fresh h1, h2, t, t' in l == (h1 :: h2 :: t) & c == (h1 :: t') & copy2 t t');

copycopy l l1 l2 =
  copy l l1 & copy2 l l2;

lengtho x l =
  x == [] & l == Zero |
  (fresh h, t, z in
    x == (h :: t) &
    l == Succ z &
    lengtho t z);

lengtho' x l =
  x == [] & l == Zero |
  (fresh h, t, z in
    x == (h :: t) &
    lengtho' t z &
    l == Succ z
  );

maxo x m = maxo1 x Zero m;

maxo1 x n m =
  x == [] & m == n |
  ( fresh h, t, z in
    x == (h :: t) &
    leo h n Trueo &
    maxo1 t n m
  ) |
  ( fresh h, t, z in
    x == (h :: t) &
    gto h n Trueo &
    maxo1 t h m
  );

mino x m =
  x == [] & m == Zero |
  (fresh h, t in
    x == (h :: t)  & mino1 t h m
  );

mino1 x n m =
  x == [] & m == n |
  (fresh h, t, z in
    x == (h :: t) & leo h n Trueo & mino1 t h m
  ) |
  (fresh h, t, z in
    x == (h :: t) & gto h n Trueo & mino1 t n m
  );

appLengtho =
  (fresh xs, ys, zs, m, n, s in
    appendo xs ys zs &
    lengtho xs m &
    lengtho ys n &
    lengtho zs s &
    addo m n s
  );

appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    xy == (h :: ty) &
    appendo t y ty
  );

appendo' x y xy =
  x == [] & xy == y |
  (fresh h, t, ty in
    x == (h :: t) |
    xy == (h :: ty) |
    appendo' t y ty
  );

reverso x y =
  x == [] & y == [] |
  (fresh h, t, rt in
    x == (h :: t) &
    reverso t rt &
    appendo rt [h] y
  );

doubleReverso xs =
  (fresh sx in reverso xs sx & reverso sx xs);

revAcco xs acc sx =
  xs == [] & sx == acc |
  (fresh h, t in
    xs == (h :: t) & revacco t (h :: acc) sx
  );

assoco x xs v =
  (fresh a, b, tl in
    xs == (Pair a b :: tl) &
    (
      a == x & b == v |
      assoco x tl v  -- & a =/= x
    )
  );

nthOpt xs n r =
  (fresh h, t, x in
    xs == [] & r == None |
    xs == (h :: t) &
    (
      n == Zero & r == Some h |
      n == Succ x & nthOpt t n r
    )
  );

? appendo xs ys ts & appendo ts zs rs
