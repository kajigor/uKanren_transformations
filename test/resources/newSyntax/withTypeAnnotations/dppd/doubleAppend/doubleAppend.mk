filter (dynamic dynamic dynamic)
appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    xy == (h :: ty) &
    appendo t y ty
  );

filter (dynamic dynamic dynamic dynamic)
double_apppendo x y z res =
  fresh t in
    appendo x y t &
    appendo t z res;

? double_apppendo x y z res