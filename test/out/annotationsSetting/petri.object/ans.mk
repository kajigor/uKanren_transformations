filter (dynamic static static static static) 
 unsafe x p cs y c = ((fresh x in (cs == (2 + x))) | (fresh x1, p1 in ((x == (1 + x1) & p == (1 + p1) & Unfold unsafe x1 p1 ((1 + cs)) y c))) | (fresh cs1 in ((cs == (1 + cs1) & Memo unsafe x ((1 + p)) cs1 ((1 + y)) c))) | (fresh y1 in ((y == (1 + y1) & Unfold unsafe ((1 + x)) p cs y1 ((1 + c))))));
filter () 
 fail  = Memo fail [];

(fresh x in (Unfold unsafe x 1 0 0 0))