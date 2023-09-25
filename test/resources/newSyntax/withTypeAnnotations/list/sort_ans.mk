filter (static dynamic)
 sorto lst reslst = ((lst == [] & reslst == []) | (fresh h, t, l, r, g, lres, rres in ((lst == (h :: t) & Unfold splito h t l g & Memo sorto l lres & Memo sorto r rres & Unfold appendo lres ((h :: rres)) reslst))));
filter (static static dynamic dynamic)
 splito x xs l g = ((xs == [] & l == [] & g == []) | (fresh x1, xs1 in ((xs == (x1 :: xs1) & ((fresh l1 in ((Unfold le x1 x & l == (x1 :: l1) & Unfold splito x xs1 l1 g))) | (fresh g1 in ((Unfold gt x1 x & g == (x1 :: g1) & Unfold splito x xs1 l g1))))))));
filter (dynamic dynamic dynamic)
 appendo x y xy = ((x == [] & y == xy) | (fresh h, t, ty in ((x == (h :: t) & xy == (h :: ty) & Memo appendo t y ty))));
filter (static static)
 gt x y = ((fresh z in ((x == Succ z & y == Zero))) | (fresh x', y' in ((x == Succ x' & y == Succ y' & Unfold gt x' y'))));
filter (static static)
 le x y = (x == Zero | (fresh x', y' in ((x == Succ x' & y == Succ y' & Unfold le x' y'))));
filter ()
 fail  = Memo fail [];

(fresh lst, reslst in (Unfold sorto ([Succ Zero, Zero, Succ (Succ Zero), Succ Zero, Zero]) reslst))