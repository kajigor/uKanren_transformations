filter (dynamic static dynamic static) 
 match1 patl tl pat t = (patl == [] | (fresh a, b, ps, ts in ((patl == (a :: ps) & tl == (b :: ts) & ((a == b & Memo match1 ps ts pat t) | (fresh x, t1 in ((t == (x :: t1) & Unfold neq a b & Memo match1 pat t1 pat t1))))))));
filter (dynamic static) 
 neq a b = ((fresh t in ((a == Succ t & b == Zero))) | (fresh t in ((b == Succ t & a == Zero))) | (fresh t, t1 in ((a == Succ t & b == Succ t1 & Unfold neq t t1))));
filter (dynamic static) 
 match pat t = Unfold match1 pat t pat t;
filter () 
 fail  = Memo fail [];

(fresh x in (Unfold match x ((2 :: (1 :: (1 :: (0 :: (0 :: (2 :: (0 :: [1]))))))))))