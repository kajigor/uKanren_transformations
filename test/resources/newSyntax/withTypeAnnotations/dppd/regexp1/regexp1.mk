filter (static dynamic static)
generate exp str lst =
  exp == Empty &
  str == lst |
  (
    fresh x in
      exp == Char x &
      str == (x :: lst)
  ) |
  (
    fresh x, y in
      exp == Or x y &
      (
        generate x str lst |
        generate y str lst
      )
  ) |
  (
    fresh x, y, t1 in
      exp == Cat x y &
      generate x str t1 &
      generate y t1 lst
  ) |
  (
    fresh x in
      exp == Star x &
      (
        str == lst |
        (fresh t1 in
        (
          generate x str t1 &
          generate exp t1 lst
        ))
      )
  );

? generate  (Star(Cat (Or (Char a) (Char b)) (Cat (Or (Char c) (Char d) ) (Cat (Or (Char e) (Char f) ) (Or (Char g) (Char h) ) )))) x Nil