filter (static static dynamic dynamic) 
 splito x xs l g = ((xs == [] & l == [] & g == []) | (fresh x1, xs1 in ((xs == (x1 :: xs1) & ((fresh l1 in ((Unfold le x1 x & l == (x1 :: l1) & Unfold splito x xs1 l1 g))) | (fresh g1 in ((Unfold gt x1 x & g == (x1 :: g1) & Unfold splito x xs1 l g1))))))));
filter (static static) 
 gt x y = ((fresh z in ((x == Succ z & y == Zero))) | (fresh x', y' in ((x == Succ x' & y == Succ y' & Unfold gt x' y'))));
filter (static static) 
 le x y = (x == Zero | (fresh x', y' in ((x == Succ x' & y == Succ y' & Unfold le x' y'))));
filter () 
 fail  = Memo fail [];

(fresh a, b, c, d in (Unfold splito a b c d))