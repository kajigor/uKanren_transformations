open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 =
  let rec eq_nat y3 y4 =
    fresh (q1 q2 q3 q4)
      ( y3 === Std.Nat.zero &&& (y4 === Std.Nat.zero)
      ||| (y3 === Std.Nat.succ q1 &&& (y4 === Std.Nat.succ q2) &&& (q1 === Std.Nat.zero &&& (q2 === Std.Nat.zero) ||| (q1 === Std.Nat.succ q3 &&& (q2 === Std.Nat.succ q4) &&& _eq_nat q3 q4))) )
  and forall2 y5 y6 y7 =
    fresh (q1 q2 q3 q4)
      ( y6 === Std.List.nil ()
      &&& (y7 === Std.List.nil ())
      ||| (y6 === Std.( % ) q1 q2 &&& (y7 === Std.( % ) q3 q4) &&& (_check_uni y5 q1 q3 &&& forall2 y5 q2 q4)) )
  and _eq_nat y8 y9 = fresh (q1 q2) (y8 === Std.Nat.zero &&& (y9 === Std.Nat.zero) ||| (y8 === Std.Nat.succ q1 &&& (y9 === Std.Nat.succ q2) &&& _eq_nat q1 q2))
  and _check_uni y10 y11 y12 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15)
      ( y11 === constr q1 q2
      &&& (y12 === constr q3 q4)
      &&& (eq_nat q1 q3 &&& forall2 y10 q2 q4)
      ||| (y11 === var_ q5 &&& (y12 === constr q6 q7) &&& (get_term y10 q5 q8 &&& _check_uni y10 q8 (constr q6 q7)))
      ||| (y11 === constr q9 q10 &&& (y12 === var_ q11) &&& (get_term y10 q11 q12 &&& _check_uni y10 (constr q9 q10) q12))
      ||| (y11 === var_ q13 &&& (y12 === var_ q14) &&& (get_term y10 q13 q15 &&& _check_uni y10 q15 (var_ q14)))
      ||| (y11 === var_ q13 &&& (y12 === var_ q14) &&& (get_termGet_term y10 q13 q14 &&& _eq_nat q13 q14)) )
  and get_term y13 y14 y15 =
    fresh (q1 q2 q3)
      (y13 === Std.( % ) (Std.Option.some y15) q1 &&& (y14 === Std.Nat.zero) ||| (y13 === Std.( % ) q2 q1 &&& (y14 === Std.Nat.succ q3) &&& get_term q1 q3 y15))
  and get_termGet_term y16 y17 y18 = _get_term y16 y17 &&& _get_term y16 y18
  and _get_term y19 y20 =
    fresh (q1 q2 q3)
      ( y19 === Std.List.nil ()
      ||| (y19 === Std.( % ) (Std.Option.none ()) q1 &&& (y20 === Std.Nat.zero))
      ||| (y19 === Std.( % ) q2 q1 &&& (y20 === Std.Nat.succ q3) &&& _get_term q1 q3) )
  in
  _check_uni x0 x1 x2