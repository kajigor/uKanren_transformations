applasto l x lst =
  fresh lx in appendo l Cons x Nil lx & lasto lst lx;

lasto x ys =
  ys == Cons x Nil | (fresh h, t in ys == Cons h t & lasto x t);

appendo xs ys rs =
  xs == Nil & rs == ys |
  (fresh h, t, ts in
     xs == Cons h t & rs == Cons h ts & appendo t ys rs);

? fresh l, x, lst in
    applasto l x lst