filter (static dynamic dynamic)
addo x y z = 
  (x == Zero & z == y) | 
  ( fresh x' in 
     x == Succ x' & addo x' (Succ y) z
  );

filter (static dynamic)
evalo fm r = 
  ( fm == Num r) | 
  ( fresh x, y, xr, yr in
    evalo x xr & 
    evalo y yr & 
    fm == Sum x y & 
    addo xr yr r 
  );

? addo (Zero) y (Succ (Succ Zero)) 
