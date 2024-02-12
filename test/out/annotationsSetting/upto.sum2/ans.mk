filter (static dynamic) 
 squaretr tin tres = ((fresh x, sox in ((tin == Leaf x & Unfold square x sox & tres == Leaf sox))) | (fresh xt, yt, soxt, soyt in ((tin == Branch xt yt & Unfold squaretr xt soxt & Unfold squaretr yt soyt & tres == Branch soxt soyt))));
filter (static dynamic) 
 sumtr tr s = ((fresh x in ((tr == Leaf x & x == s))) | (fresh xt, yt, sx, sy in ((tr == Branch xt yt & Unfold sumtr xt sx & Unfold sumtr yt sy & Unfold add sx sy s))));
filter (static dynamic) 
 sumtrsquaretr xt s = (fresh soxt in ((Unfold squaretr xt soxt & Unfold sumtr soxt s)));
filter (dynamic dynamic) 
 square ns son = Unfold multiply ns ns son;
filter (dynamic dynamic) 
 le m n = (m == 0 | (fresh t, t1 in ((m == (1 + t) & n == (1 + t1) & Memo le t t1))));
filter (dynamic dynamic dynamic) 
 multiply x y z = ((y == 0 & z == 0) | (fresh t, z1 in ((y == (1 + t) & Unfold add x z1 z & Memo multiply x t z1))));
filter (dynamic dynamic dynamic) 
 add x y z = ((x == 0 & y == z) | (fresh t1, t2 in ((x == (1 + t1) & z == (1 + t2) & Memo add t1 y t2))));
filter () 
 fail  = Memo fail [];

(fresh x1, x2, x3, x4, s in (Unfold sumtrsquaretr (Branch Branch Branch Leaf x1 Leaf x2 Leaf x3 Branch Leaf x4 Branch Leaf x1 Leaf x2) s))