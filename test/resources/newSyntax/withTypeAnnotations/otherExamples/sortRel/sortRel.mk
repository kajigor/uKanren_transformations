filter (dynamic dynamic static)
leo x y b =
  (x == Zero & b == Trueo) |
  ( fresh z in
      x == Succ z & y == Zero & b == Falso
  ) |
  ( fresh x', y' in
      x == Succ x' & y == Succ y' & leo x' y' b
  );

filter (dynamic dynamic static)
gto x y b =
  (x == Zero & b == Falso) |
  (fresh z in
      x == Succ z & y == Zero & b == Trueo
  ) |
  (fresh x', y' in
      x == Succ x' & y == Succ y' & gto x' y' b
  );

filter (dynamic dynamic dynamic dynamic)
minmaxo a b min max =
  min == a & b == max & leo a b Trueo |
  max == a & b == min & gto a b Trueo;

filter (dynamic static static)
smallesto l s l' =
  l == [s] & l' == Nil |
  (fresh  h, t, s', t', max in
      l' == (max :: t') &
      minmaxo h s' s max &
      smallesto t s' t' &
      l == (h :: t)
    );

filter (dynamic static)
sorto x y =
  x == Nil & y == Nil |
  (fresh s, xs, xs' in
    y == (s :: xs') &
    sorto xs xs' &
    smallesto x s xs);

? sorto x0 ([Zero, Succ Zero, Succ (Succ Zero), Succ (Succ (Succ Zero)),
              Succ (Succ (Succ (Succ Zero)))])
