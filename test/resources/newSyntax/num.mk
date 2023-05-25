notZero x = fresh y in x == S y;

{-
addo x y z =
  (x == O & z == y) |
  (fresh x', z' in
    x == S x' & addo x' (S y) z
  );
-}

addo x y z =
  (x == O & y == z) |
  (fresh x', z' in
    x == S x' & z == S z' & addo x' y z'
  );


mulo x y z =
  (x == O & z == O) |
  (fresh x', z' in
    (x == S x' &
     addo y z' z &
     mulo x' y z'
    )
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

geo x y z =
  leo y x z ;

lto x y z =
  gto y x z ;

? addo (S (S O)) y z