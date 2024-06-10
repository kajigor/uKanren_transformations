filter (static dynamic) 
 uptosd v0 v1 = ((v1 == [] & v0 == 0) | (fresh v3, v6, v2, v7 in ((v0 == (1 + v3) & v6 == v0 & Unfold uptosd v3 v2 & v7 == v2 & v1 == (v6 :: v7)))));
filter (static static dynamic) 
 sum1ssd v0 v1 v2 = ((v0 == [] & v1 == v2) | (fresh v3, v4, v5 in ((v0 == (v3 :: v4) & Unfold addssd v3 v1 v5 & Unfold sum1ssd v4 v5 v2))));
filter (static dynamic) 
 sumsd v0 v1 = (fresh v9 in ((v9 == 0 & Unfold sum1ssd v0 v9 v1)));
filter (static static dynamic) 
 addssd v0 v1 v2 = ((v0 == 0 & v1 == v2) | (fresh v3, v4 in ((v0 == (1 + v3) & Unfold addssd v3 v1 v4 & v2 == (1 + v4)))));
filter (static static dynamic) 
 multiplyssd v0 v1 v2 = ((v2 == 0 & v1 == 0) | (fresh v3, v4 in ((v1 == (1 + v3) & Unfold multiplyssd v0 v3 v4 & Unfold addssd v0 v4 v2))));
filter (static dynamic) 
 squaresd v0 v1 = (fresh v8 in ((v8 == v0 & Unfold multiplyssd v0 v8 v1)));
filter (static dynamic) 
 squaressd v0 v1 = ((v1 == [] & v0 == []) | (fresh v2, v3, v4, v5 in ((v0 == (v2 :: v3) & Unfold squaresd v2 v4 & Unfold squaressd v3 v5 & v1 == (v4 :: v5)))));
filter (static dynamic) 
 sumsquaresuptosd v0 v1 = (fresh v2, v3 in ((Unfold uptosd v0 v2 & Unfold squaressd v2 v3 & Unfold sumsd v3 v1)));

(fresh s in (Memo sumsquaresuptosd 4 s))