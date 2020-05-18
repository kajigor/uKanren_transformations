open OCanren
open GT
open Helper

let topLevel x0 x1 =
  let rec _evalo y2 y3 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y2 === conj q1 q2 &&& evaloEvalo y3 q1 q2
      ||| (y2 === disj q1 q2 &&& (evaloEvalo y3 q1 q2 ||| (__evalo y3 q1 &&& _evalo q2 y3) ||| ___evaloEvalo y3 q1 q2))
      ||| (y2 === neg q1 &&& __evalo y3 q1)
      ||| (y2 === var q3 &&& (q3 === Std.Nat.zero &&& (y3 === Std.( % ) !!true q4) ||| (q3 === Std.Nat.succ q5 &&& (y3 === Std.( % ) q6 q4) &&& _elemo q4 q5)))
      )
  and evaloEvalo y4 y5 y6 = _evalo y5 y4 &&& _evalo y6 y4
  and _evaloEvalo y7 y8 y9 = __evalo y7 y8 &&& _evalo y9 y7
  and __evalo y10 y11 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y11 === conj q1 q2
      &&& (_evaloEvalo y10 q1 q2 ||| (_evalo q1 y10 &&& __evalo y10 q2) ||| __evaloEvalo y10 q1 q2)
      ||| (y11 === disj q1 q2 &&& __evaloEvalo y10 q1 q2)
      ||| (y11 === neg q1 &&& _evalo q1 y10)
      ||| ( y11 === var q3 &&& (q3 === Std.Nat.zero &&& (y10 === Std.( % ) !!false q4) ||| (q3 === Std.Nat.succ q5 &&& (y10 === Std.( % ) q6 q4) &&& elemo q4 q5)) ) )
  and __evaloEvalo y12 y13 y14 = __evalo y12 y13 &&& __evalo y12 y14
  and elemo y15 y16 =
    fresh (q1 q2 q3) (y16 === Std.Nat.zero &&& (y15 === Std.( % ) !!false q1) ||| (y16 === Std.Nat.succ q2 &&& (y15 === Std.( % ) q3 q1) &&& elemo q1 q2))
  and ___evaloEvalo y17 y18 y19 = _evalo y18 y17 &&& __evalo y17 y19
  and _elemo y20 y21 =
    fresh (q1 q2 q3) (y21 === Std.Nat.zero &&& (y20 === Std.( % ) !!true q1) ||| (y21 === Std.Nat.succ q2 &&& (y20 === Std.( % ) q3 q1) &&& _elemo q1 q2))
  in
  _evalo x1 x0