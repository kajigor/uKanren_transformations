idx k xs v = 
  fresh k1, t, h in 
    (k == Zero & xs == [] & v == None) | 
    (k == Zero & xs == (h :: t) & v == Some h) | 
    (k == Succ k1 & xs == (h :: t) & idx k1 t v); 

type_ term gamma ttype = 
  fresh els, thn, cond, btype1, btype, body, bound, t1, t, r, l, n, m, v, y, x in 
    (term == BConst_ x & ttype == Some Boolean) | 
    (term == IConst_ y & ttype == Some Integer) | 
    (term == Var_ v & idx v gamma ttype) | 
    (term == Plus_ m n & type_ m gamma (Some Integer) & type_ n gamma (Some Integer) & ttype == Some Integer) | 
    (term == Mult_ m n & type_ m gamma (Some Integer) & type_ n gamma (Some Integer) & ttype == Some Integer) | 
    (term == Eq_ l r & type_ l gamma t & type_ r gamma t & ttype == Some Boolean & t == Some t1) | 
    (term == Lt_ l r & type_ l gamma (Some Integer) & type_ r gamma (Some Integer) & ttype == Some Boolean) |  
    (term == Let_ bound body & type_ bound gamma btype & btype == Some btype1 & type_ body (btype :: gamma) ttype) | 
    (term == If_ cond thn els & type_ cond gamma (Some Boolean) & type_ thn gamma ttype & type_ els gamma ttype); 
 
? type_ q [] (Some Integer)
