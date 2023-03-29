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

evalo st fm u =
  fresh x, y, v, w, z in
    (fm == Conj x y & evalo st x v & evalo st y w & ando v w u) |
    (fm == Disj x y & evalo st x v & evalo st y w & oro v w u) |
    (fm == Neg x & evalo st x v & noto v u) |
    (fm == Var z & elemo z st u);

elemo n s v =
  fresh h, t, n' in
    (n == Zero & s == (h :: t) & v == h) |
    (n == Succ n' & s == (h :: t) & elemo n' t v);

? evalo st fm Trueo
