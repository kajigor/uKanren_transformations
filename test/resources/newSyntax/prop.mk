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
    (ando v w u & Delay evalo st x v & Delay evalo st y w & fm == Conj x y) |
    (oro v w u & Delay evalo st x v & Delay evalo st y w & fm == Disj x y) |
    (noto v u & Delay evalo st x v & fm == Neg x) |
    (fm == Var z & elemo z st u) |
    (fm == Lit u) ;

elemo n s v =
  fresh h, t, n' in
    (n == Zero & s == (h :: t) & v == h) |
    (s == (h :: t) & elemo n' t v & n == Succ n');

? evalo st fm Trueo
