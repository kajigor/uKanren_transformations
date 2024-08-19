filter (dynamic dynamic static) 
 appendodds v0 v1 v2 = 
  ((v0 == [] & v1 == v2) | 
  (fresh v6, v7, v3, v5, v4 in 
    ((v2 == (v6 :: v7) & 
      v6 == v3 & 
      v7 == v5 & 
      Unfold appendodds v4 v1 v5 & v0 == (v3 :: v4)))));

filter (dynamic dynamic dynamic static) 
 double_appendoddds v0 v1 v2 v3 = 
  (fresh v4 in 
    ((Unfold appendodds v4 v2 v3 & 
      Unfold appendodds v0 v1 v4)));

(fresh x, y, z in (Memo double_appendoddds x y z (S O :: (S (S O) :: (S (S (S O)) :: (O :: (O :: (S O :: [S (S O)]))))))))