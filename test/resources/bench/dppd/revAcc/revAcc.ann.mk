filter (dynamic static static)
rev x acc y =
  x == Nil &
  acc == y |
  (
    fresh h, t in
      Unfold rev t (h :: acc) y &
      x == (h :: t)
  );

(fresh x in (Memo rev x [] [A, B, C, D]))