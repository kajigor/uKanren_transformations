filter (static dynamic)
 reverso x y = ((x == [] & y == []) | (fresh h, t, rt in ((Memo reverso t rt & x == (h :: t) & Unfold appendo rt [h] y))));
filter (static static dynamic)
 appendo x y xy = ((x == [] & y == xy) | (fresh h, t, ty in ((x == (h :: t) & xy == (h :: ty) & Unfold appendo t y ty))));
filter ()
 fail  = Memo fail [];

filter (static static static dynamic)
  help xs ys zs ts = Unfold appendo xs ys ts & Unfold reverso zs ts;

(fresh ts in ((Unfold help [Zero, Cons Zero] [Cons Zero, Zero, Cons (Cons Zero)] [Cons (Cons Zero), Zero, Cons Zero, Cons Zero, Zero, Cons Zero] (Zero :: ts))))