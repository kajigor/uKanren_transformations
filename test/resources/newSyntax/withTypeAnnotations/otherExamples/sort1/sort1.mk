
filter (static static)
le x y =
  x == Zero |
  ( fresh x', y' in
      x == Succ x' & y == Succ y' & le x' y'
  );

filter (dynamic static)
gt x y =
  (fresh z in
      x == Succ z & y == Zero
  ) |
  (fresh x', y' in
      y == Succ y' & gt x' y' & x == Succ x'
  );


filter (dynamic dynamic static)
appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    xy == (h :: ty) &
    appendo t y ty &
    x == (h :: t)
  );


filter (static dynamic static dynamic)
splito x xs l g =
  (xs == [] & l == [] & g == []) |
  (fresh x1, xs1 in
	(
		(
			fresh l1 in
			l == (x1 :: l1) &
			le x1 x &
			splito x xs1 l1 g
		) |
		(fresh g1 in
			gt x1 x &
			splito x xs1 l g1 &
			g == (x1 :: g1)
		)
	) &
	xs == (x1 :: xs1)
  );

filter (dynamic static)
sorto lst reslst =
  (lst == [] & reslst == []) |
  (fresh h, t, l, r, lres, rres in
	appendo lres (h :: rres) reslst &
	sorto l lres &
	sorto r rres &
	splito h t l r &
	lst == (h :: t)
  );

? sorto reslst [Zero, Succ Zero, Succ (Succ Zero), Succ (Succ (Succ Zero))]