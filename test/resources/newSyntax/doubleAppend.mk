appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    xy == (h :: ty) &
    appendo t y ty
  );

double_appendo x y z res =
  fresh t in
    appendo x y t &
    appendo t z res;

? double_appendo x y z w