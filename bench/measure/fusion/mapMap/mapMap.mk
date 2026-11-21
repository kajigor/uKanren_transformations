-- map inc (map inc xs), mostly run backward (map as a relation).
mapInc xs ys = (xs == [] & ys == []) |
  (fresh h, t, t1 in xs == (h :: t) & ys == (S h :: t1) & mapInc t t1);

mapMap xs zs = fresh ys in mapInc xs ys & mapInc ys zs;

? mapMap xs zs
