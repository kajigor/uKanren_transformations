filter (dynamic dynamic static) 
 appendo x y xy = ((x == [] & y == xy) | (fresh h, t, ty in ((x == (h :: t) & xy == (h :: ty) & Unfold appendo t y ty))));
filter (dynamic static) 
 matcho p s = (fresh t1, t2, s1 in ((Unfold appendo s1 t1 s & Unfold appendo t2 p s1)));
filter () 
 fail  = Memo fail [];

(fresh x in (Unfold matcho x ((2 :: (1 :: (1 :: (0 :: (0 :: (2 :: (0 :: [1]))))))))))