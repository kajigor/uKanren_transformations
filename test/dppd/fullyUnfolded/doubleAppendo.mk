:: doubleApp x y z res =
  [int: {appendo x y int} /\ {appendo int z res}]

:: appendo xs ys rs = conde
  (xs === [] /\ rs === ys)
  ([h t ts: xs === (h % t) /\ rs === (h % ts) /\ {appendo t ys rs}])

? {doubleApp x y z res}