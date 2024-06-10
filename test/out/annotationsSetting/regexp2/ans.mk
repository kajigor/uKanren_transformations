filter () 
 fail  = Memo fail [];
filter (dynamic static dynamic) 
 generate exp str lst = ((exp == Empty & str == lst) | (fresh x in ((exp == Char x & str == (x :: lst)))) | (fresh x, y in ((exp == Or x y & (Memo generate x str lst | Memo generate y str lst)))) | (fresh x, y, t1 in ((exp == Cat x y & Memo generate x str t1 & Memo generate y t1 lst))) | (fresh x in ((exp == Star x & (str == lst | (fresh t1 in ((Memo generate x str t1 & Memo generate exp t1 lst))))))));

(fresh x in (Unfold generate x ((Succ Succ Zero :: (Succ Succ Zero :: (Zero :: (Zero :: (Zero :: [Succ Zero])))))) []))