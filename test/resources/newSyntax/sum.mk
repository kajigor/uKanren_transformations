addo x y z = 
  (x == Zero & z == y) | 
  ( fresh x' in 
     x == Succ x' & addo x' (Succ y) z
  )

evalo fm r = 
  ( fm == Num r) | 
  ( fresh x, y, xr, yr in
    evalo x xr & 
    evalo y yr & 
    fm == Sum x y & 
    addo xr yr r 
  )

? evalo y (Succ (Succ Zero)) 
