filter (dynamic dynamic)
le x y =
  x == Zero |
  ( fresh x', y' in
      x == Succ x' & y == Succ y' & le x' y'
  );

filter (dynamic dynamic)
gt x y =
  (fresh z in
      x == Succ z & y == Zero
  ) |
  (fresh x', y' in
      x == Succ x' & y == Succ y' & gt x' y'
  );

filter (dynamic dynamic static static)
splito x xs l g =
  (xs == [] & l == [] & g == []) |
  (fresh x1, xs1 in
    xs == (x1 :: xs1) &
	(
		(
			fresh l1 in
			le x1 x &
			l == (x1 :: l1) &
			splito x xs1 l1 g
		) |
		(fresh g1 in
			gt x1 x &
			g == (x1 :: g1) &
			splito x xs1 l g1
		)
	)
  );

? splito a b ([Zero, Succ (Succ Zero), Succ Zero]) ([Succ (Succ (Succ (Succ Zero))), Succ (Succ (Succ Zero))])