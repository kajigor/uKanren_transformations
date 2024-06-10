evalo y0 = (fresh q1, q2 in ((y0 == Lit (Trueo) | y0 == Var (Zero) | y0 == Var (Succ (Zero)) | y0 == Var (Succ (Succ (Zero))) | (y0 == Neg (q1) & _evalo q1) | (y0 == Disj (q1) (q2) & evalo q1 & evalo q2) | (y0 == Disj (q1) (q2) & _evalo q1 & evalo q2) | (y0 == Disj (q1) (q2) & evalo q1 & _evalo q2) | (y0 == Conj (q1) (q2) & evalo q1 & evalo q2) | (y0 == Impl (q1) (q2) & _evalo q1 & evalo q2) | (y0 == Impl (q1) (q2) & _evalo q1 & _evalo q2) | (y0 == Impl (q1) (q2) & evalo q1 & evalo q2))));

_evalo y1 = (fresh q1, q2 in ((y1 == Lit (Falso) | (y1 == Neg (q1) & evalo q1) | (y1 == Disj (q1) (q2) & _evalo q1 & _evalo q2) | (y1 == Conj (q1) (q2) & _evalo q1 & evalo q2) | (y1 == Conj (q1) (q2) & evalo q1 & _evalo q2) | (y1 == Conj (q1) (q2) & _evalo q1 & _evalo q2) | (y1 == Impl (q1) (q2) & evalo q1 & _evalo q2))));


? evalo x0