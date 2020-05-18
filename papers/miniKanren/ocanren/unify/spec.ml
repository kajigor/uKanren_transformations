open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 =
  let rec check_uni y0 y1 y2 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12)
      ( y1 === constr q1 q2
      &&& (y2 === constr q3 q4)
      &&& (eq_nat q1 q3 &&& forall2 y0 q2 q4)
      ||| (y1 === var_ q5 &&& (y2 === constr q6 q7) &&& get_termCheck_uni y0 q5 q6 q7)
      ||| (y1 === constr q8 q9 &&& (y2 === var_ q10) &&& _get_termCheck_uni y0 q8 q9 q10)
      ||| (y1 === var_ q11 &&& (y2 === var_ q12) &&& __get_termCheck_uni y0 q11 q12)
      ||| (y1 === var_ q11 &&& (y2 === var_ q12) &&& get_termGet_termEq_nat y0 q11 q12) )
  and eq_nat y3 y4 = fresh (q1 q2) (y3 === Std.Nat.zero &&& (y4 === Std.Nat.zero) ||| (y3 === Std.Nat.succ q1 &&& (y4 === Std.Nat.succ q2) &&& eq_nat q1 q2))
  and forall2 y5 y6 y7 =
    fresh (q1 q2 q3 q4) (y6 === nil () &&& (y7 === nil ()) ||| (y6 === q1 % q2 &&& (y7 === q3 % q4) &&& (check_uni y5 q1 q3 &&& forall2 y5 q2 q4)))
  and get_termCheck_uni y8 y9 y10 y11 =
    fresh (q1 q2 q3 q4)
      ( y8
      === some q1 % q2
      &&& (y9 === Std.Nat.zero)
      &&& check_uni (some q1 % q2) q1 (constr y10 y11)
      ||| (y8 === q3 % q2 &&& (y9 === Std.Nat.succ q4) &&& (__get_term q1 q2 q4 &&& check_uni (q3 % q2) q1 (constr y10 y11))) )
  and _get_termCheck_uni y16 y17 y18 y19 =
    fresh (q1 q2 q3 q4)
      ( y16
      === some q1 % q2
      &&& (y19 === Std.Nat.zero)
      &&& check_uni (some q1 % q2) (constr y17 y18) q1
      ||| (y16 === q3 % q2 &&& (y19 === Std.Nat.succ q4) &&& (__get_term q1 q2 q4 &&& check_uni (q3 % q2) (constr y17 y18) q1)) )
  and __get_termCheck_uni y24 y25 y26 =
    fresh (q1 q2 q3 q4)
      ( y24
      === some q1 % q2
      &&& (y25 === Std.Nat.zero)
      &&& check_uni (some q1 % q2) q1 (var_ y26)
      ||| (y24 === q3 % q2 &&& (y25 === Std.Nat.succ q4) &&& (__get_term q1 q2 q4 &&& check_uni (q3 % q2) q1 (var_ y26))) )
  and __get_term y28 y29 y30 = fresh (q1 q2 q3) (y29 === some y28 % q1 &&& (y30 === Std.Nat.zero) ||| (y29 === q2 % q1 &&& (y30 === Std.Nat.succ q3) &&& __get_term y28 q1 q3))
  and get_termGet_termEq_nat y31 y32 y33 =
    fresh (q1 q2 q3 q4)
      ( y31 === nil () &&& eq_nat y32 y33
      ||| (y31 === none () % q1 &&& (y32 === Std.Nat.zero) &&& (y33 === Std.Nat.zero))
      ||| (y31 === q2 % q1 &&& (y32 === Std.Nat.succ q3) &&& (y33 === Std.Nat.succ q4) &&& get_termGet_termEq_nat q1 q3 q4) )
  in
  check_uni x0 x1 x2