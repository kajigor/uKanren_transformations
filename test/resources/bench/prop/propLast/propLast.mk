
ando x y b =
  (x == Trueo & y == Trueo & b == Trueo) |
  (x == Falso & y == Trueo & b == Falso) |
  (x == Trueo & y == Falso & b == Falso) |
  (x == Falso & y == Falso & b == Falso);

oro x y b =
  (x == Trueo & y == Trueo & b == Trueo) |
  (x == Falso & y == Trueo & b == Trueo) |
  (x == Trueo & y == Falso & b == Trueo) |
  (x == Falso & y == Falso & b == Falso);

noto x b =
  (x == Trueo & b == Falso) |
  (x == Falso & b == Trueo);

implicationo x y b =
  (x == Falso & y == Trueo & b == Trueo) |
  (x == Falso & y == Falso & b == Trueo) |
  (x == Trueo & y == Trueo & b == Trueo) |
  (x == Trueo & y == Falso & b == Falso);

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
