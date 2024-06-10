filter (static dynamic) 
 solve lst res = Unfold solve1 lst 0 res;
filter (static dynamic) 
 claus exp body = ((fresh x, t in ((exp == Member x (x :: t) & body == []))) | (fresh x, t, y in ((exp == Member x (y :: t) & body == [Member x t]))) | (fresh x, l1, l2 in ((exp == InBoth x l1 l2 & body == (Member x l1 :: [Member x l2])))) | (fresh l, l1 in ((l1 == [] & exp == App l1 l l & body == []))) | (fresh h, x, y, z in ((exp == App (h :: x) y (h :: z) & body == [App x y z]))) | (fresh x, t in ((exp == Delete x (x :: t) t & body == []))) | (fresh x, y, t, d in ((exp == Delete x (y :: t) (y :: d) & body == [Delete x t d]))) | (fresh a, l1, l2, res, d1 in ((exp == Test a l1 l2 res & body == (InBoth a l1 l2 :: (Delete a l1 d1 :: [App d1 l2 res]))))));
filter (static static dynamic) 
 solve1 lst depth res = ((lst == [] & depth == res) | (fresh head, tail, body, intDepth in ((lst == (head :: tail) & Unfold claus head body & Memo solve1 body ((1 + depth)) intDepth & Unfold solve1 tail intDepth res))));
filter () 
 fail  = Memo fail [];

(fresh y, x in (Unfold solve ([Test 0 (2 :: (1 :: (0 :: (0 :: (2 :: [1]))))) (0 :: (1 :: (2 :: [0]))) y]) x))