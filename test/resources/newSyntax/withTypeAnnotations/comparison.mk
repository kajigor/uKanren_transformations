
filter (dynamic static dynamic)
leo x y b =
  (x == Zero & b == Trueo) |
  ( fresh z in
      x == Succ z & y == Zero & b == Falso
  ) |
  ( fresh x', y' in
      x == Succ x' & y == Succ y' & leo x' y' b
  );


filter (dynamic static dynamic)
gto x y b =
  (x == Zero & b == Falso) |
  (fresh z in
      x == Succ z & y == Zero & b == Trueo
  ) |
  (fresh x', y' in
      x == Succ x' & y == Succ y' & gto x' y' b
  );

? leo a b True & gto b a True