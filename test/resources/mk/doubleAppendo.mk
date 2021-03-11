:: appendo x y z = 
  (x === [] /\ z === y) \/ 
  [ h t r : 
    x === h % t /\ 
    {appendo t y r} /\ 
    z === h % r
  ] 

:: doubleAppendo x y z r = 
  [ t :
    {appendo x y t} /\ {appendo t z r}
  ]

? {doubleAppendo x y z r} 
