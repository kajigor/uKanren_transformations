filter (static dynamic dynamic)
maxLengtho x m l =
  Unfold maxo x m & Unfold lengtho x l;


filter (static dynamic)
lengtho x l =
  x == [] & l == Zero |
  (fresh h, t, z in
    x == (h :: t) &
    l == Succ z &
    Memo lengtho t z);


filter (static dynamic)
maxo x m = Unfold maxo1 x Zero m;


filter (static static dynamic)
maxo1 x n m =
  x == [] & m == n |
  ( fresh h, t, z in
    x == (h :: t) &
    Unfold leo h n Trueo &
    Memo maxo1 t n m
  ) |
  ( fresh h, t, z in
    x == (h :: t) &
    Unfold gto h n Trueo &
    Memo maxo1 t h m
  );

filter (static dynamic static)
leo x y b =
  (x == O & b == Trueo) |
  (fresh zz in
    (x == S zz & y == O & b == Falso)
  ) |
  (fresh x', y' in
    (x == S x' & y == S y' & Memo leo x' y' b)
  );

filter (static dynamic static)
gto x y b =
  (fresh zz in x == S zz & y == O & b == Trueo) |
  (x == O & b == Falso) |
  (fresh x', y' in x == S x' & y == S y' & Memo gto x' y' b);

(fresh xs, max, len in (Unfold maxLengtho xs max len))
