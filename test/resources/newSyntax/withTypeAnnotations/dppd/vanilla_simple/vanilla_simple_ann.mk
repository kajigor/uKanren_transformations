filter (static dynamic) 
 my_clausesd v0 v1 = ((fresh v11, v2, v3, v4, v5, v10, v6, v7, v9, v8, v12, v13 in ((v11 == [] & v0 == DoubleApp v2 v3 v4 v5 & v10 == v6 & v7 == App v2 v3 v6 & v9 == App v10 v4 v5 & v8 == (v9 :: v11) & v12 == v7 & v13 == v8 & v1 == (v12 :: v13)))) | (fresh v14, v16, v17, v18, v2, v15 in ((v14 == [] & v1 == [] & v0 == App v16 v17 v18 & v16 == v14 & v17 == v2 & v15 == v2 & v18 == v15))) | (fresh v25, v22, v4, v23, v19, v2, v3, v21, v20, v5, v24, v26, v27 in ((v25 == [] & v0 == App v22 v4 v23 & v22 == v19 & v19 == (v2 :: v3) & v21 == v2 & v23 == v20 & v20 == (v21 :: v5) & v24 == App v3 v4 v5 & v26 == v24 & v27 == v25 & v1 == (v26 :: v27)))));
filter (static) 
 solve_atoms v0 = (fresh v1 in ((Unfold my_clausesd v0 v1 & Memo solves v1)));
filter (static) 
 solves v0 = (v0 == [] | (fresh v1, v2 in ((v0 == (v1 :: v2) & Unfold solve_atoms v1 & Unfold solves v2))));

(fresh x, y, z in (Memo solves ([DoubleApp x y z (2 :: (1 :: (2 :: [1])))])))