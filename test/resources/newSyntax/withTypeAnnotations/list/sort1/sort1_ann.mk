filter (dynamic static)
 sorto lst reslst =
  ((lst == [] & reslst == []) |
   (fresh h, t, l, r, lres, rres in
    ((Unfold appendo lres ((h :: rres)) reslst &
      Memo sorto l lres &
      Unfold sorto r rres &
      Unfold splito h t l r &
      lst == (h :: t)))));

filter (static dynamic static dynamic)
 splito x xs l g =
  ((xs == [] & l == [] & g == []) |
   (fresh x1, xs1 in
    ((((fresh l1 in
      ((l == (x1 :: l1) &
       Unfold le x1 x &
       Unfold splito x xs1 l1 g))) |
    (fresh g1 in
      ((Unfold gt x1 x &
        Memo splito x xs1 l g1 &
        g == (x1 :: g1))))) &
     xs == (x1 :: xs1)))));

filter (dynamic dynamic static)
 appendo x y xy = ((x == [] & y == xy) | (fresh h, t, ty in ((xy == (h :: ty) & Unfold appendo t y ty & x == (h :: t)))));
filter (dynamic static)
 gt x y = ((fresh z in ((x == Succ z & y == Zero))) | (fresh x', y' in ((y == Succ y' & Unfold gt x' y' & x == Succ x'))));
filter (static static)
 le x y = (x == Zero | (fresh x', y' in ((x == Succ x' & y == Succ y' & Unfold le x' y'))));
filter ()
 fail  = Memo fail [];

(fresh reslst in (Unfold sorto reslst ((Zero :: (Succ Zero :: (Succ Succ Zero :: [Succ Succ Succ Zero]))))))