filter (dynamic static static static static)
 unsafe x p cs y c = (
  (fresh x in
    (cs == S (S x))) |
  (fresh x1, p1 in
    ((x == S x1 & p == S p1 & Unfold unsafe x1 p1 (S cs) y c))) |
  (fresh cs1 in
    ((cs == S cs1 & Memo unsafe x (S p) cs1 (S y) c))) |
  (fresh y1 in
    ((y == S y1 & Unfold unsafe (S x) p cs y1 (S c)))));

filter ()
 fail  = Memo fail [];

(fresh x in (Unfold unsafe x (S O) O O O))
