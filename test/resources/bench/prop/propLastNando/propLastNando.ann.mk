
filter (dynamic dynamic static)
nando a b c = 
  ( a == Falso & b == Falso & c == Trueo ) |
  ( a == Falso & b == Trueo & c == Trueo ) |
  ( a == Trueo & b == Falso & c == Trueo ) |
  ( a == Trueo & b == Trueo & c == Falso ) ;

filter (dynamic dynamic static)
ando a b c =
  fresh ab in Unfold nando a b ab & Unfold nando ab ab c ; 

filter (dynamic dynamic static)
oro a b c =
  fresh aa, bb in Unfold nando a a aa & Unfold nando b b bb & Unfold nando aa bb c; 

filter (dynamic static)
noto a na =
  Unfold nando a a na; 

filter (dynamic dynamic static)
implicationo x y r = 
  fresh yy in Unfold nando y y yy & Unfold nando x yy r ;

filter (static dynamic static)
evalo st fm u =
  fresh x, y, v, w, z in
    (fm == Lit u) |
    (fm == Var z & Unfold elemo z st u) |
    (fm == Neg x & Memo evalo st x v & Unfold noto v u) |
    (fm == Disj x y & Memo evalo st x v & Memo evalo st y w & Unfold oro v w u) |
    (fm == Conj x y & Memo evalo st x v & Memo evalo st y w & Unfold ando v w u) |
    (fm == Impl x y & Memo evalo st x v & Memo evalo st y w & Unfold implicationo v w u);

filter (dynamic static static)
elemo n s v =
  fresh h, t, n' in
    (n == Zero & s == (h :: t) & v == h) |
    (s == (h :: t) & n == Succ n' & Unfold elemo n' t v );

(fresh x in Unfold evalo [Trueo, Trueo, Trueo] x Trueo)
