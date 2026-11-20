appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    appendo t y ty & 
    xy == (h :: ty));

double_appendo x y z res =
  fresh t in
    appendo x y t &
    appendo t z res;

-- ? double_appendo x y z ([S O, S (S O), S (S (S O)), O, O, S O, S (S O)])
? double_appendo x y z res