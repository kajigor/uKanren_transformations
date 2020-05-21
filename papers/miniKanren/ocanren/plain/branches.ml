open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 =
  let rec evalo y0 y1 =
    fresh (q1 q2 q3)
      ( y0 === conj q1 q2
      &&& (evalo q1 y1 &&& evalo q2 y1)
      ||| (y0 === disj q1 q2 &&& (evalo q1 y1 &&& evalo q2 y1))
      ||| (y0 === disj q1 q2 &&& (_evalo y1 q1 &&& evalo q2 y1))
      ||| (y0 === disj q1 q2 &&& (evalo q1 y1 &&& _evalo y1 q2))
      ||| (y0 === neg q1 &&& _evalo y1 q1)
      ||| (y0 === var q3 &&& _elemo y1 q3) )
  and _evalo y2 y3 =
    fresh (q1 q2 q3)
      ( y3 === conj q1 q2
      &&& (_evalo y2 q1 &&& evalo q2 y2)
      ||| (y3 === conj q1 q2 &&& (evalo q1 y2 &&& _evalo y2 q2))
      ||| (y3 === conj q1 q2 &&& (_evalo y2 q1 &&& _evalo y2 q2))
      ||| (y3 === disj q1 q2 &&& (_evalo y2 q1 &&& _evalo y2 q2))
      ||| (y3 === neg q1 &&& evalo q1 y2)
      ||| (y3 === var q3 &&& elemo y2 q3) )
  and elemo y4 y5 = fresh (q1 q2 q3) (y5 === Nat.zero &&& (y4 === !!false % q1) ||| (y5 === Nat.succ q2 &&& (y4 === q3 % q1) &&& elemo q1 q2))
  and _elemo y6 y7 = fresh (q1 q2 q3) (y7 === Nat.zero &&& (y6 === !!true % q1) ||| (y7 === Nat.succ q2 &&& (y6 === q3 % q1) &&& _elemo q1 q2)) in
  evalo x0 x1