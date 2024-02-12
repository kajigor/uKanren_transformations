filter (dynamic static)
flipflip xt yt =
  fresh tt in
    flip tt yt &
    flip xt tt;

filter (dynamic static)
flip left right =
  (
    fresh x in
      (right == Leaf x &
       left == Leaf x)
  ) |
  (
    fresh xt, yt, info, fyt, fxt in
      (right == Tree fyt info fxt &
      flip xt fxt &
      flip yt fyt &
      left == Tree xt info yt)
  );

? flipflip x (Tree (Tree (Leaf (A [])) (E []) (Leaf (B []) ) ) (D []) (Leaf (C []) ) )