filter (dynamic dynamic)
flip left right =
((fresh x in
  ((left == Leaf x & right == Leaf x))) |
  (fresh xt, yt, info, fyt, fxt in
    ((left == Tree xt info yt &
    right == Tree fyt info fxt &
    Memo flip xt fxt &
    Memo flip yt fyt ))
    ));

filter (dynamic dynamic)
flipflip xt yt =
(fresh tt in
  ((Unfold flip xt tt & Unfold flip tt yt)));

filter ()
 fail  = Memo fail [];

(fresh x, y in (Unfold flipflip x y))