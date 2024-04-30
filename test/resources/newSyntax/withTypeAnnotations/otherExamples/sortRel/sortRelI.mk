
filter (static static static)
leo x y b =
  (x == Zero & b == Trueo) |
  ( fresh z in
      x == Succ z & y == Zero & b == Falso
  ) |
  ( fresh x', y' in
      x == Succ x' & y == Succ y' & leo x' y' b
  );

filter (static static static)
gto x y b =
  (x == Zero & b == Falso) |
  (fresh z in
      x == Succ z & y == Zero & b == Trueo
  ) |
  (fresh x', y' in
      x == Succ x' & y == Succ y' & gto x' y' b
  );

filter (static static dynamic dynamic)
minmaxo a b min max =
  min == a & b == max & leo a b Trueo |
  max == a & b == min & gto a b Trueo;

filter (static dynamic dynamic)
smallesto l s l' =
  l == [s] & l' == Nil |
  (fresh  h, t, s', t', max in
      l == (h :: t) &
      smallesto t s' t' &
      minmaxo h s' s max &
      l' == (max :: t')
    );

filter (static dynamic)
sorto x y =
  x == Nil & y == Nil |
  (fresh s, xs, xs' in
    smallesto x s xs &
    sorto xs xs' &
    y == (s :: xs'));

? sorto ([Zero, Succ Zero, Succ (Succ Zero), Succ (Succ (Succ Zero)), Succ (Succ (Succ (Succ Zero))), Succ (Succ (Succ (Succ (Succ Zero))))]) x0
