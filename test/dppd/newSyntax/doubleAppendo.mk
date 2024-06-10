doubleApp x y z res =
  fresh int in appendo x y int & appendo int z res;

appendo xs ys rs =
  xs == Nil & rs == ys | 
  (fresh h, t, ts in
     xs == Cons h t & rs == Cons h ts & appendo t ys rs);

? fresh x, y, z, res in
    doubleApp x y z res