filter (dynamic dynamic dynamic)
add x y z =
  x == O &
  y == z |
  (
    fresh t1, t2 in
      x == S t1 &
      z == S t2 &
      add t1 y t2
  );

filter (dynamic dynamic dynamic)
multiply x y z =
  y == O &
  z == O |
  (fresh t, z1 in
    y == S t &
    add x z1 z &
    multiply x t z1
    );

filter (dynamic dynamic)
le m n =
  m == O |
  (fresh t, t1 in
    m == S t &
    n == S t1 &
    le t t1
  );

filter (dynamic dynamic)
square ns son =
  multiply ns ns son;

filter (static dynamic)
sumtrsquaretr xt s =
  fresh soxt in
   squaretr xt soxt &
   sumtr soxt s;

filter (static dynamic)
sumtr tr s =
  (
    fresh x in
      tr == Leaf x &
      x == s
  ) |
  (
    fresh xt, yt, sx, sy in
      tr == Branch xt yt &
      sumtr xt sx &
      sumtr yt sy &
      add sx sy s
  );

filter (static dynamic)
squaretr tin tres =
  (
    fresh x, sox in
      tin == Leaf x &
      square x sox &
      tres == Leaf sox
  ) |
  (
    fresh xt, yt, soxt, soyt in
      tin == Branch xt yt &
      squaretr xt soxt &
      squaretr yt soyt &
      tres == Branch soxt soyt
  );

? sumtrsquaretr (Branch (Branch (Branch (Leaf x1) (Leaf x2) ) (Leaf x3) ) (Branch (Leaf x4) (Branch (Leaf x1) (Leaf x2)))) s