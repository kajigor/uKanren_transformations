filter (dynamic dynamic dynamic dynamic static) 
 g a b t s y = (fresh a1, y1 in ((y == (a1 :: y1) & a == a1 & ((a == b & Unfold f s y1) | (Unfold neq a b & Unfold f t y1)))));
filter (static dynamic) 
 neq a b = ((fresh t in (((a == Zero & b == Succ t) | (a == Succ t & b == Zero)))) | (fresh t, t1 in ((a == Succ t & b == Succ t1 & Unfold neq t t1))));
filter (dynamic dynamic static) 
 h a t y = ((t == [] & y == [a]) | (fresh b, s in ((t == (b :: s) & Unfold g a b t s y))));
filter (static dynamic) 
 f x y = ((x == [] & y == []) | (fresh a, t in ((x == (a :: t) & Unfold h a t y))));
filter (static dynamic) 
 rr x y = (fresh t in ((Unfold f x t & Unfold f t y)));
filter () 
 fail  = Memo fail [];

(fresh x in (Unfold rr x ((Succ Zero :: (Zero :: (Succ Succ Zero :: (Zero :: [Zero])))))))