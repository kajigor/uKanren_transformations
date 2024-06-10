ando x y b =
  (x == trueo & y == trueo & b == trueo) |
  (x == falso & y == trueo & b == falso) |
  (x == trueo & y == falso & b == falso) |
  (x == falso & y == falso & b == falso);


oro x y b =
  (x == trueo & y == trueo & b == trueo) |
  (x == falso & y == trueo & b == trueo) |
  (x == trueo & y == falso & b == trueo) |
  (x == falso & y == falso & b == falso);

noto x b =
  (x == trueo & b == falso) |
  (x == falso & b == trueo);

evalo st fm u =
  (fresh x, y, v, w in
    (evalo st x v & (
      (fm == Neg x & noto v u) |
      (evalo st y w &
        ((fm == Conj x y & ando v w u) |
         (fm == Disj x y & oro v w u)))))) |
  (fresh z in (fm == Var z & elemo z st u));

elemo n s v =
  (fresh h, t, n' in
    (n == Zero & s == (h :: t) & v == h) |
    (n == Succ n' & s == (h :: t) & elemo n' t v));

? evalo st fm trueo
