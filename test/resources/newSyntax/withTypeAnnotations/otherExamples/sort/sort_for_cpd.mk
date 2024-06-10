
le x y =
  x == Zero |
  ( fresh x', y' in
      x == Succ x' & y == Succ y' & le x' y'
  );

gt x y =
  (fresh z in
      x == Succ z & y == Zero
  ) |
  (fresh x', y' in
      x == Succ x' & y == Succ y' & gt x' y'
  );

appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    xy == (h :: ty) &
    appendo t y ty
  );


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

sorto lst reslst =
  (lst == [] & reslst == []) |
  (fresh h, t, l, r, lres, rres in
	lst == (h :: t) &
	splito h t l r &
	sorto l lres &
	sorto r rres &
	appendo lres (h :: rres) reslst
  );

? sorto [Succ Zero, Zero, Succ (Succ Zero), Succ Zero, Zero] reslst