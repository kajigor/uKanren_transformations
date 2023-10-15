
filter (dynamic dynamic)
flipflip xt yt =
  fresh tt in
    flip xt tt &
    flip tt yt;

filter (dynamic dynamic)
flip left right =
  (
    fresh x in
      (left == Leaf x &
      right == Leaf x)
  ) |
  (
    fresh xt, yt, info, fyt, fxt in
      (left == Tree xt info yt &
      flip xt fxt &
      flip yt fyt &
      right == Tree fyt info fxt)
  );

? flipflip x y