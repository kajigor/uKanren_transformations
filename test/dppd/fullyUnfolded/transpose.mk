:: transpose xs ys = conde
  (ys === [] /\ {nullrows xs})
  ([h t zs:
      ys === (h % t) /\
      {makeRow xs h zs} /\
      {transpose zs t}])


:: makeRow xss yss zss = conde
  (xss === [] /\ yss === [] /\ zss === [])
  ([x xs ys zs xs1 temp:
    temp === (x % xs) /\
    xss === (temp % ys) /\
    yss === (x % xs1) /\
    zss === (xs % zs) /\
    {makeRow ys xs1 zs}])

:: nullRows xs = conde
  (xs === [])
  ([t: xs === ([] % t) /\ {nullRows t}])

? {transpose xs ys}
