filter (static dynamic)
 reverso x y = ((x == [] & y == []) | (fresh h, t, rt in ((x == (h :: t) & Unfold reverso t rt & Unfold appendo rt [h] y))));
filter (static static dynamic)
 appendo x y xy = ((x == [] & y == xy) | (fresh h, t, ty in ((x == (h :: t) & xy == (h :: ty) & Unfold appendo t y ty))));
filter ()
 fail  = Memo fail [];

filter (static static static dynamic)
  help xs ys zs ts = Unfold appendo xs ys ts & Unfold reverso zs ts;

(fresh x, y, ts in ((Unfold help [x, y] [Succ Zero, Zero, Succ (Succ Zero)] [Succ (Succ Zero), Zero, Succ Zero, Succ Zero, Zero] (Zero :: ts))))