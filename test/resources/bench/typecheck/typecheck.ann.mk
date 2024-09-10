filter (dynamic static static)
idx k xs v = 
  fresh k1, t, h in 
    (k == Zero & xs == [] & v == None) | 
    (k == Zero & xs == (h :: t) & v == Some h) | 
    (k == Succ k1 & xs == (h :: t) & Unfold idx k1 t v); 

filter (dynamic static static)
type_ term gamma ttype = 
  fresh els, thn, cond, btype1, btype, body, bound, t1, t, r, l, n, m, v, y, x in 
    (term == BConst_ x & ttype == Some Boolean) | 
    (term == IConst_ y & ttype == Some Integer) | 
    (term == Var_ v & Unfold idx v gamma ttype) | 
    (term == Plus_ m n & Unfold type_ m gamma (Some Integer) & Unfold type_ n gamma (Some Integer) & Unfold ttype == Some Integer) | 
    (term == Mult_ m n & Unfold type_ m gamma (Some Integer) & Unfold type_ n gamma (Some Integer) & Unfold ttype == Some Integer) | 
    (term == Eq_ l r & Unfold type_ l gamma t & Unfold type_ r gamma t & Unfold ttype == Some Boolean & t == Some t1) | 
    (term == Lt_ l r & Unfold type_ l gamma (Some Integer) & Unfold type_ r gamma (Some Integer) & Unfold ttype == Some Boolean) |  
    (term == Let_ bound body & Unfold type_ bound gamma btype & btype == Some btype1 & Unfold type_ body (btype1 :: gamma) ttype) | 
    (term == If_ cond thn els & Unfold type_ cond gamma (Some Boolean) & Unfold type_ thn gamma ttype & Unfold type_ els gamma ttype); 
 
(fresh q in Unfold type_ q [] (Some Integer))
