filter (dynamic static static)
applasto l x lst =
  fresh lx in
    appendo l [x] lx &
    lasto lst lx;

filter (static dynamic)
lasto x ys =
  fresh h, t in
    ys == [x] |
    (ys == (h :: t) &
     lasto x t);

filter (dynamic dynamic dynamic)
appendo xs ys rs =
  (xs == [] & rs == ys) |
  (fresh h, t, ts in
    xs == (h :: t) &
    rs == (h :: ts) &
    appendo t ys ts);

? applasto l (S O) (S O)