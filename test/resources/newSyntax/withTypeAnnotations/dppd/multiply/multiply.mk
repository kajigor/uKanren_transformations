filter (dynamic dynamic static)
add x y z =
  x == Zero &
  y == z |
  (
    fresh t1, t2 in
      x == Succ t1 &
      z == Succ t2 &
      add t1 y t2
  );

filter (dynamic dynamic static)
multiply x y z =
  y == Zero &
  z == Zero |
  (fresh t, z1 in
    y == Succ t &
    add x z1 z &
    multiply x t z1
    );

? multiply x y (Succ (Succ (Succ (Succ Zero))))