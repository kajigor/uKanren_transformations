
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
    (elemo z st u & fm == Var z) |
    (noto v u & evalo st x v & fm == Neg x) |
    (oro v w u & evalo st x v & evalo st y w & fm == Disj x y) |
    (ando v w u & evalo st x v & evalo st y w & fm == Conj x y) |
    (implicationo v w u & evalo st x v & evalo st y w & fm == Impl x y);

filter (dynamic static static)
elemo n s v =
  fresh h, t, n' in
    (n == Zero & s == (h :: t) & v == h) |
    (s == (h :: t) & elemo n' t v & n == Succ n');

? evalo [Trueo, Trueo, Trueo] x Trueo
