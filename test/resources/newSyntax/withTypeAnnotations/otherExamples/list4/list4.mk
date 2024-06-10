filter (static static dynamic)
leo x y b =
  (x == Zero & b == Trueo) |
  ( fresh z in
      x == Succ z & y == Zero & b == Falso
  ) |
  ( fresh x', y' in
      x == Succ x' & y == Succ y' & leo x' y' b
  );


filter (static static dynamic)
gto x y b =
  (x == Zero & b == Falso) |
  (fresh z in
      x == Succ z & y == Zero & b == Trueo
  ) |
  (fresh x', y' in
      x == Succ x' & y == Succ y' & gto x' y' b
  );


filter (static dynamic)
maxo x m = maxo1 x Zero m;

filter (static static dynamic)
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

filter (static dynamic)
mino x m =
  x == [] & m == Zero |
  (fresh h, t in
    x == (h :: t) & mino1 t h m
  );

filter (static static dynamic)
mino1 x n m =
  x == [] & m == n |
  (fresh h, t, z in
    x == (h :: t) & leo h n Trueo & mino1 t h m
  ) |
  (fresh h, t, z in
    x == (h :: t) & gto h n Trueo & mino1 t n m
  );

filter (static dynamic dynamic)
maxMino x m l =
  maxo x m & mino x l;

? maxMino [Succ Zero, Zero, Succ (Succ Zero)] m l