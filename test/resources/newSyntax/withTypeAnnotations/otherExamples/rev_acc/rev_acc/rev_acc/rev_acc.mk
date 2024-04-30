filter (dynamic static static)
rev x acc y =
  x == Nil &
  acc == y |
  (
    fresh h, t in
      x == (h :: t) &
      rev t (h :: acc) y
  );

? rev x [] [A, B, C, D]