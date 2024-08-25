
filter (dynamic dynamic static)
ando x y b =
  (x == Trueo & y == Trueo & b == Trueo) |
  (x == Falso & y == Trueo & b == Falso) |
  (x == Trueo & y == Falso & b == Falso) |
  (x == Falso & y == Falso & b == Falso);

filter (dynamic dynamic static)
oro x y b =
  (x == Trueo & y == Trueo & b == Trueo) |
  (x == Falso & y == Trueo & b == Trueo) |
  (x == Trueo & y == Falso & b == Trueo) |
  (x == Falso & y == Falso & b == Falso);

filter (dynamic static)
noto x b =
  (x == Trueo & b == Falso) |
  (x == Falso & b == Trueo);

filter (dynamic dynamic static)
implicationo x y b = 
  (x == Falso & y == Trueo & b == Trueo) | 
  (x == Falso & y == Falso & b == Trueo) | 
  (x == Trueo & y == Trueo & b == Trueo) | 
  (x == Trueo & y == Falso & b == Falso);

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
