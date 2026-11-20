flipflip xt yt =
  fresh tt in
    flip xt tt & 
    flip tt yt;

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

? flipflip x (Tree (Tree (Tree (Tree (Leaf (A)) (E) (Leaf (B))) (D) (Leaf (C))) (E) (Leaf (B))) (D) (Tree (Leaf (F)) (R) (Leaf (C))))