matcho p s =
  fresh t1, t2, s1 in
    appendo t2 p s1 & 
    appendo s1 t1 s; 

appendo x y xy =
  x == [] & y == xy |
  (fresh h, t, ty in
    x == (h :: t) &
    appendo t y ty & 
    xy == (h :: ty)
  );

? matcho x ([S (S O), S O, S O, O, O, S (S O), O, S O])