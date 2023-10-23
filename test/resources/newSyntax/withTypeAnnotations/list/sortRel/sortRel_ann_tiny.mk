filter (dynamic static)
 sorto x y =
  ((x == [] & y == []) |
  (fresh s, xs, xs' in
    ((y == (s :: xs') &
    Unfold sorto xs xs' &
    Unfold smallesto x s xs))));

filter (dynamic static static)
 smallesto l s l' =
  ((l == [s] & l' == []) |
    (fresh h, t, s', t', max in
      ((l' == (max :: t') &
        Unfold minmaxo h s' s max &
        Unfold smallesto t s' t' &
        l == (h :: t)))));

filter (dynamic dynamic static static)
 minmaxo a b min max =
  ((min == a & b == max &
    Unfold leo a b Trueo) |
   (max == a & b == min &
   Unfold gto a b Trueo));

filter (static static dynamic)
 gto x y b =
  ((x == Zero & b == Falso) |
    (fresh z in
      ((x == Succ z & y == Zero & b == Trueo))) |
      (fresh x', y' in
        ((x == Succ x' & y == Succ y' & Unfold gto x' y' b))));

filter (static static dynamic)
 leo x y b =
  ((x == Zero & b == Trueo) |
  (fresh z in
    ((x == Succ z & y == Zero & b == Falso))) |
    (fresh x', y' in
      ((x == Succ x' & y == Succ y' & Unfold leo x' y' b))));

filter ()
 fail  = Memo fail [];

(fresh x0 in (Unfold sorto x0 ([Zero, Zero, Succ Zero, Succ (Succ Zero)])))