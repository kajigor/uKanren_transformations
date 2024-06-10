filter (static dynamic) 
 r x y = ((x == [] & y == []) | (fresh a, t, t1 in ((x == (a :: (a :: t)) & y == (a :: t1) & Unfold r t t1))) | (fresh ax, ay, t, t1 in ((x == (ax :: (ay :: t)) & y == (ax :: t1) & Unfold neq ax ay & Unfold r ((ay :: t)) t1))));
filter (static dynamic) 
 rr x y = (fresh t in ((Unfold r x t & Unfold r t y)));
filter (static dynamic) 
 neq x y = ((fresh t in ((x == 0 & y == (1 + t)))) | (fresh t in ((x == (1 + t) & y == 0))) | (fresh tx, ty in ((x == (1 + tx) & y == (1 + ty) & Unfold neq tx ty))));
filter () 
 fail  = Memo fail [];

(fresh x, y in (Unfold rr x y))