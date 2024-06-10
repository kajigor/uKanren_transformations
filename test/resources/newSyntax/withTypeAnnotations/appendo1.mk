appendo x y xy =
  ((x == [] & y == xy) |
  (fresh h, t, ty in
    x == (h :: t) & 
    xy == (h :: ty) & 
    appendo t y ty 
  ));

? appendo x y [a, b, c]