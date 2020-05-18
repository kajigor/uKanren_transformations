open OCanren
open GT
open Helper

(* after changes aimed at bottles *)
let topLevel x0 x1 =
  let rec _evalo y2 y3 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y3 === conj q1 q2 &&& ___evaloEvalo y2 q1 q2
      ||| (y3 === disj q1 q2 &&& (__evalo y2 q1 &&& _evalo y2 q2 ||| __evaloEvalo y2 q1 q2 ||| ___evaloEvalo y2 q1 q2))
      ||| (y3 === neg q1 &&& __evalo y2 q1)
      ||| (y3 === var q3 &&& (q3 === Std.Nat.zero &&& (y2 === Std.( % ) !!true q4) ||| (q3 === Std.Nat.succ q5 &&& (y2 === Std.( % ) q6 q4) &&& _elemo q4 q5)))
      )
  and __evalo y4 y5 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y5 === conj q1 q2
      &&& (_evaloEvalo y4 q1 q2 ||| evaloEvalo y4 q1 q2 ||| (_evalo y4 q1 &&& __evalo y4 q2))
      ||| (y5 === disj q1 q2 &&& _evaloEvalo y4 q1 q2)
      ||| (y5 === neg q1 &&& _evalo y4 q1)
      ||| (y5 === var q3 &&& (q3 === Std.Nat.zero &&& (y4 === Std.( % ) !!false q4) ||| (q3 === Std.Nat.succ q5 &&& (y4 === Std.( % ) q6 q4) &&& elemo q4 q5)))
      )
  and evaloEvalo y6 y7 y8 = __evalo y6 y7 &&& _evalo y6 y8
  and _evaloEvalo y9 y10 y11 = __evalo y9 y10 &&& __evalo y9 y11
  and elemo y12 y13 =
    fresh (q1 q2 q3) (y13 === Std.Nat.zero &&& (y12 === Std.( % ) !!false q1) ||| (y13 === Std.Nat.succ q2 &&& (y12 === Std.( % ) q3 q1) &&& elemo q1 q2))
  and __evaloEvalo y14 y15 y16 = _evalo y14 y15 &&& __evalo y14 y16
  and ___evaloEvalo y17 y18 y19 = _evalo y17 y18 &&& _evalo y17 y19
  and _elemo y20 y21 =
    fresh (q1 q2 q3) (y21 === Std.Nat.zero &&& (y20 === Std.( % ) !!true q1) ||| (y21 === Std.Nat.succ q2 &&& (y20 === Std.( % ) q3 q1) &&& _elemo q1 q2))
  in
  _evalo x0 x1