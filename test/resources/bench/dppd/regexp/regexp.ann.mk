filter (static dynamic static)
 generate exp str lst =
  ((exp == Empty & str == lst) |
   (fresh x in
    ((exp == Char x & str == (x :: lst)))) |
   (fresh x, y in
    ((exp == Or x y &
     (Unfold generate x str lst | Unfold generate y str lst)))) |
   (fresh x, y, t1 in
    ((exp == Cat x y & Unfold generate x str t1 & Unfold generate y t1 lst))) |
   (fresh x in
    ((exp == Star x &
      (str == lst |
      (fresh t1 in
        ((Unfold generate x str t1 & Memo generate exp t1 lst))))))));

filter ()
 fail  = Memo fail [];

(fresh a, b, c, d, e, f, g, h, x in (Unfold generate (Star (Cat (Or (Char a) (Char b)) (Or (Char c) (Char d) ) )) x []))