add x y z =
  x == O &
  y == z |
  (
    fresh t1, t2 in
      x == S t1 &
      z == S t2 &
      add t1 y t2
  );

multiply x y z =
  y == O &
  z == O |
  (fresh t, z1 in
    y == S t &
    add x z1 z &
    multiply x t z1
    );

-- ? multiply x y (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S O))))))))))))))))
? multiply x y (S (S (S (S O))))