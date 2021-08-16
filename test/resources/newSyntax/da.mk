appendo x y z = 
  x == [] & z == y | 
  (fresh h, t, r in 
    x == (h :: t) & 
    appendo t y r & 
    z == Cons h r)

doubleAppendo x y z r = 
  fresh t in
    appendo x y t & appendo t z r
  
? doubleAppendo x y z r 
