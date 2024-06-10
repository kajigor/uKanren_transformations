transpose xs ys =
  ys == Nil & nullrows xs | 
  (fresh h, t, zs in
     ys == Cons h t & makeRow xs h zs & transpose zs t);

makeRow xss yss zss =
  xss == Nil & yss == Nil & zss == Nil | 
  (fresh x, xs, ys, zs, xs1, temp in
     temp == Cons x xs & xss == Cons temp ys & yss == Cons x xs1 & zss == Cons xs zs & makeRow ys xs1 zs);

nullRows xs =
  xs == Nil | (fresh t in xs == Cons Nil t & nullRows t);

? fresh xs, ys in
    transpose xs ys