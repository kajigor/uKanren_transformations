filter (static static dynamic) 
 max1 l n m = ((l == [] & n == m) | (fresh x, t in ((l == (x :: t) & ((Unfold le x n & Unfold max1 t n m) | (Unfold le n x & Unfold max1 t x m))))));
filter (static dynamic) 
 max x m = Unfold max1 x Zero m;
filter (static dynamic) 
 my_length l len = ((l == [] & len == Zero) | (fresh h, t, lent in ((l == (h :: t) & Unfold my_length t lent & len == Succ lent))));
filter (static dynamic dynamic) 
 max_length ls m len = (Unfold max ls m & Unfold my_length ls len);
filter (static static) 
 le x y = (x == Zero | (fresh t, t1 in ((x == Succ t & y == Succ t1 & Unfold le t t1))));
filter () 
 fail  = Memo fail [];

(fresh x, y in (Unfold max_length ((Succ Zero :: (Succ Succ Zero :: (Zero :: (Zero :: [Succ Zero]))))) x y))