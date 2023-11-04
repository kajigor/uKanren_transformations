
filter (static static)
le x y =
  x == Zero |
  (
    fresh t, t1 in
    x == Succ t &
    y == Succ t1 &
    le t t1
  );

filter (static dynamic dynamic)
max_length ls m len =
  max ls m &
  my_length ls len;

filter (static dynamic)
my_length l len =
  l == Nil &
  len == Zero |
  (
    fresh h, t, lent in
      l == (h :: t) &
      my_length t lent &
      len == Succ lent
  );

filter (static dynamic)
max x m = max1 x Zero m;

filter (static static dynamic)
max1 l n m =
  l == Nil &
  n == m |
  (
    fresh x, t in
      l == (x :: t) &
      (
        le x n &
        max1 t n m |
        le n x &
        max1 t x m
      )
  );

? max_length ([Succ Zero, Succ (Succ Zero), Zero, Zero, Succ Zero]) x y