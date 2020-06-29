open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 =
  let rec evalo y0 y1 =
    fresh (q1 q2 q3)
      ( y0 === conj q1 q2 &&& (evalo q1 y1 &&& evalo q2 y1)
      ||| (y0 === disj q1 q2 &&& oroEvaloEvalo y1 q1 q2)
      ||| (y0 === neg q1 &&& _evalo y1 q1)
      ||| (y0 === var q3 &&& _elemo y1 q3) )
  and oroEvaloEvalo y2 y3 y4 = evalo y3 y2 &&& evalo y4 y2 ||| (_evalo y2 y3 &&& evalo y4 y2) ||| (evalo y3 y2 &&& _evalo y2 y4)
  and _evalo y7 y8 =
    fresh (q1 q2 q3)
      ( y8 === conj q1 q2 &&& andoEvaloEvalo y7 q1 q2
      ||| (y8 === disj q1 q2 &&& (_evalo y7 q1 &&& _evalo y7 q2))
      ||| (y8 === neg q1 &&& evalo q1 y7)
      ||| (y8 === var q3 &&& elemo y7 q3) )
  and andoEvaloEvalo y9 y10 y11 = _evalo y9 y10 &&& evalo y11 y9 ||| (evalo y10 y9 &&& _evalo y9 y11) ||| (_evalo y9 y10 &&& _evalo y9 y11)
  and elemo y14 y15 =
    fresh (q1 q2 q3) (y15 === Nat.zero &&& (y14 === ( % ) !!false q1) ||| (y15 === Nat.succ q2 &&& (y14 === ( % ) q3 q1) &&& elemo q1 q2))
  and _elemo y16 y17 =
    fresh (q1 q2 q3) (y17 === Nat.zero &&& (y16 === ( % ) !!true q1) ||| (y17 === Nat.succ q2 &&& (y16 === ( % ) q3 q1) &&& _elemo q1 q2))
  in
  evalo x1 x0