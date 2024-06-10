filter (dynamic) 
 nullrows xs = (xs == [] | (fresh t in ((Memo nullrows t & xs == ([] :: t)))));
filter (dynamic static dynamic) 
 makerow x y z = ((x == [] & y == [] & z == []) | (fresh xs1, xs2, x1, xs3, t1, t2 in ((y == (x :: t1) & z == (xs3 :: t2) & x == (xs1 :: xs2) & Unfold makerow xs2 t1 t2 & xs1 == (x :: xs3)))));
filter (dynamic static) 
 transpose from to = ((to == [] & Unfold nullrows from) | (fresh y, ys, zs in ((to == (y :: ys) & Unfold transpose zs ys & Unfold makerow from y zs))));
filter () 
 fail  = Memo fail [];

(fresh x in (Unfold transpose x (((0 :: (1 :: [2])) :: ((1 :: (2 :: [0])) :: [(1 :: (2 :: [0]))])))))