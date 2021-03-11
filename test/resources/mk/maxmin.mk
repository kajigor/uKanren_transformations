:: le x y b = 
  (x === zero /\ b === trueo) \/
  [ z : 
      x === succ z /\ y === zero /\ b === falso 
  ] \/ 
  [ x' y' : 
      x === succ x' /\ y === succ y' /\ {le x' y' b}
  ]

:: gt x y b = 
  (x === zero /\ b === falso) \/ 
  [ z : 
      x === succ z /\ y === zero /\ b === trueo
  ] \/ 
  [ x' y' : 
      x === succ x' /\ y === succ y' /\ {gt x' y' b}
  ]

:: maxmin x a i = 
  (x === [] /\ a === zero /\ i === zero) \/ 
  [ h t : 
      x === h % t /\ 
      {max t h a} /\ 
      {min t h i} 
  ] 

:: max x n m = 
  (x === [] /\ m === n) \/
  [ h t : 
      x === h % t /\ 
      ( ({le h n trueo} /\ {max t n m}) \/ 
        ({gt h n trueo} /\ {max t h m})
      ) 
  ]

:: min x n m = 
  (x === [] /\ m === n) \/
  [ h t : 
      x === h % t /\ 
      ( ({le h n trueo} /\ {min t h m}) \/ 
        ({gt h n trueo} /\ {min t n m})
      ) 
  ]


? {maxmin x a i} 
