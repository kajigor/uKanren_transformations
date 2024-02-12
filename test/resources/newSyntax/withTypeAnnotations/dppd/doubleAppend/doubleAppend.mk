filter (dynamic dynamic static)
appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    xy == (h :: ty) &
    appendo t y ty
  );

filter (dynamic dynamic dynamic static)
double_appendo x y z res =
  fresh t in
    appendo t z res &
    appendo x y t;

? double_appendo x y z [S O, S (S O), S (S (S O)), O, O, S O, S (S O)]