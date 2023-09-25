filter (dynamic dynamic)
 reverso x y = ((x == [] & y == []) | (fresh h, t, rt in ((x == (h :: t) & Memo reverso t rt & Unfold appendo rt [h] y))));
filter (static dynamic dynamic)
 appendo x y xy = ((x == [] & y == xy) | (fresh h, t, ty in ((x == (h :: t) & xy == (h :: ty) & Unfold appendo t y ty))));
filter ()
 fail  = Memo fail [];

(fresh xs, ys, ts in ((Unfold appendo xs ys ts & Unfold reverso ts ys)))