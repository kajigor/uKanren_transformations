maxLengtho x m l =
  maxo x m & lengtho x l;

lengtho x l =
  x == [] & l == Zero |
  (fresh h, t, z in
    x == (h :: t) &
    l == Succ z &
    lengtho t z);

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

leo x y b =
  (x == O & b == Trueo) |
  (fresh zz in
    (x == S zz & y == O & b == Falso)
  ) |
  (fresh x', y' in
    (x == S x' & y == S y' & leo x' y' b)
  );

gto x y b =
  (fresh zz in x == S zz & y == O & b == Trueo) |
  (x == O & b == Falso) |
  (fresh x', y' in x == S x' & y == S y' & gto x' y' b);

? maxLengtho xs max len
