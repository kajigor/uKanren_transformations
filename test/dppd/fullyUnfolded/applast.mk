:: applasto l x lst = 
  [lx: {appendo l (x % []) lx} /\ {lasto lst lx}]

:: lasto x ys = conde 
  (ys === (x % [])) 
  ([h t: ys === (h % t) /\ {lasto x t}])

:: appendo xs ys rs = conde 
  (xs === [] /\ rs === ys)
  ([h t ts: xs === (h % t) /\ rs === (h % ts) /\ {appendo t ys rs}])
