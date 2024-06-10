filter (dynamic) 
 nullrowsd v0 = (v0 == [] | (fresh v12, v13, v1, v14 in ((v12 == [] & v13 == v12 & Memo nullrowsd v1 & v14 == v1 & v0 == (v13 :: v14)))));
filter (dynamic static static) 
 makerowdss v0 v1 v2 = ((v0 == [] & v1 == [] & v2 == []) | (fresh v9, v7, v6, v8, v10, v11, v3, v4 in ((v1 == (v9 :: v7) & v2 == (v6 :: v8) & v10 == v9 & v11 == v6 & v3 == (v10 :: v11) & Unfold makerowdss v4 v7 v8 & v0 == (v3 :: v4)))));
filter (dynamic static) 
 transposeds v0 v1 = ((v1 == [] & Unfold nullrowsd v0) | (fresh v2, v3, v4 in ((v1 == (v2 :: v3) & Unfold transposeds v4 v3 & Unfold makerowdss v0 v2 v4))));

(fresh x in (Memo transposeds x (((0 :: (1 :: [2])) :: ((1 :: (2 :: [0])) :: [(1 :: (2 :: [0]))])))))