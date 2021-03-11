:: addo x y z = 
  (x === zero /\ z === y) \/ 
  [ x' : 
     x === succ x' /\ {addo x' (succ y) z}
  ]

:: evalo fm r = 
  ( fm === <num :r>) \/ 
  [ x y xr yr :
    {evalo x xr} /\ 
    {evalo y yr} /\ 
    fm === <sum :x y> /\ 
    {addo xr yr r} 
  ]    

? {evalo y (succ (succ zero))} 
