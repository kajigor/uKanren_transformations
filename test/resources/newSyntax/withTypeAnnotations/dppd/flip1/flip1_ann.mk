filter (dynamic static)
 flip left right =
  ((fresh x in
    ((right == Leaf x & left == Leaf x))) |
     (fresh xt, yt, info, fyt, fxt in
      ((right == Tree fyt info fxt &
        Unfold flip xt fxt &
        Unfold flip yt fyt &
        left == Tree xt info yt))));

filter (dynamic static)
 flipflip xt yt =
  (fresh tt in
    ((Unfold flip tt yt &
      Unfold flip xt tt)));

filter ()
 fail  = Memo fail [];

(fresh x, y in (Unfold flipflip x (Tree (Tree (Leaf (A [])) (E []) (Leaf (B []) ) ) (D []) (Leaf (C []) ) )))