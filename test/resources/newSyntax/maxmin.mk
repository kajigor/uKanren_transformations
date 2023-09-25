le x y b =
  (x == Zero & b == Trueo) |
  ( fresh z in
      x == Succ z & y == Zero & b == Falso
  ) |
  ( fresh x', y' in
      x == Succ x' & y == Succ y' & le x' y' b
  );

gt x y b =
  (x == Zero & b == Falso) |
  (fresh z in
      x == Succ z & y == Zero & b == Trueo
  ) |
  (fresh x', y' in
      x == Succ x' & y == Succ y' & gt x' y' b
  );

maxmin x a i =
  (x == [] & a == Zero & i == Zero) |
  (fresh h, t in
      x == (h :: t) &
      max t h a &
      min t h i
  );

max x n m =
  (x == [] & m == n) |
  (fresh h, t in
      x == (h :: t) &
      ( (le h n Trueo & max t n m) |
        (gt h n Trueo & max t h m)
      )
  );

min x n m =
  (x == [] & m == n) |
  (fresh h, t in
      x == (h :: t) &
      ( (le h n Trueo & min t h m) |
        (gt h n Trueo & min t n m)
      )
  );


? maxmin a mi ma
