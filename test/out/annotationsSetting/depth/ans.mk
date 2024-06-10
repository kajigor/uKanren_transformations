filter (static dynamic) 
 prog_clause exp body = ((fresh x, xs, y, z in ((exp == C Member x xs & body == C Append y (x :: z) xs))) | (fresh x in ((exp == C Append [] x x & body == C True []))) | (fresh x, l1, l2, l3 in ((exp == C Append (x :: l1) l2 (x :: l3) & body == C Append l1 l2 l3))));
filter (static static dynamic) 
 max a b mx = ((b == 0 & a == mx) | (a == 0 & b == mx) | (fresh a1, b1, mx1 in ((a == (1 + a1) & b == (1 + b1) & mx == (1 + mx1) & Unfold max a1 b1 mx1))));
filter (static dynamic) 
 depth exp d = ((exp == C True [] & d == 0) | (fresh l, r, d1, d2 in ((exp == C Cons l r & Unfold depth l d1 & Unfold depth r d2 & Unfold max d1 d2 d))) | (fresh body, d1 in ((d == (1 + d1) & Unfold prog_clause exp body & Memo depth body d1))));
filter () 
 fail  = Memo fail [];

(fresh exp, d in (Unfold depth exp d))