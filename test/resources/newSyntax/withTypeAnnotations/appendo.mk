appendo x y xy =
  ((x == [] & y == xy) |
  (fresh h, t, ty in
    x == (h :: t) & 
    xy == (h :: ty) & 
    appendo t y ty 
  ));

? appendo [a, b] [c] x