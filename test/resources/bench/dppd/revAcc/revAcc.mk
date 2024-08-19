rev x acc y =
  x == Nil &
  acc == y |
  (
    fresh h, t in
      rev t (h :: acc) y &
      x == (h :: t)
  );

? rev x [] [A, B, C, D]