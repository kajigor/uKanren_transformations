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

sumsquaresupto n s =
  fresh ns, sons in
    upto (S O) n ns &
    squares ns sons &
    sum sons s;

le m n =
  m == O |
  (fresh t, t1 in
    m == S t &
    n == S t1 &
    le t t1
  );

upto m n lst =
  m == S n &
  lst == Nil |
  le m n &
  (
    fresh t in
      upto (S m) n t &
      lst == (m :: t)
  );

square ns son =
  multiply ns ns son;

squares ns sons =
  ns == Nil &
  sons == Nil |
  (
    fresh h1, t1, h2, t2 in
      ns == (h1 :: t1) &
      sons == (h2 :: t2) &
      square h1 h2 &
      squares t1 t2
  );

sum ns s =
  sum1 ns O s;

sum1 lst s1 s2 =
  lst == Nil &
  s1 == s2 |
  (
    fresh h, t, s0 in
      lst == (h :: t) &
      add h s1 s0 &
      sum1 t s0 s2
  );

? sumsquaresupto (S (S (S (S O)))) s