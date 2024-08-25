nando a b c = 
  ( a == Falso & b == Falso & c == Trueo ) |
  ( a == Falso & b == Trueo & c == Trueo ) |
  ( a == Trueo & b == Falso & c == Trueo ) |
  ( a == Trueo & b == Trueo & c == Falso ) ;

noto a na = nando a a na; 

oro a b c =
  fresh aa, bb in nando a a aa & nando b b bb & nando aa bb c; 

ando a b c = 
  fresh ab in nando a b ab & nando ab ab c ; 

implicationo x y b =
  fresh yy in nando y y yy & nando x yy b ; 

evalo st fm u =
  fresh x, y, v, w, z in
    (fm == Lit u) |
    (fm == Var z & elemo z st u) |
    (fm == Neg x & evalo st x v & noto v u) |
    (fm == Disj x y & evalo st x v & evalo st y w & oro v w u) |
    (fm == Conj x y & evalo st x v & evalo st y w & ando v w u) |
    (fm == Impl x y & evalo st x v & evalo st y w & implicationo v w u );

elemo n s v =
  fresh h, t, n' in
    (n == Zero & s == (h :: t) & v == h) |
    (s == (h :: t) & n == Succ n' & elemo n' t v);

? evalo [Trueo, Trueo] x Trueo
