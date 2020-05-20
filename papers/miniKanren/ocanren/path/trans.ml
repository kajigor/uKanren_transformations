open GT
open OCanren
open Helper
open OCanren.Std

let topLevel x0 x1 =
  let rec elem y2 y3 y4 =
    fresh (q1 q2 q3 q4)
      (y2 === ( % ) (Pair.pair q1 q2) q3 &&& (eqNat y3 q1 &&& _eqNat y4 q2) ||| (y2 === ( % ) q4 q3 &&& (eqPair y3 y4 q4 &&& elem q3 y3 y4)))
  and eqNat y5 y6 =
    fresh (q1 q2 q3 q4)
      ( y5 === Nat.zero &&& (y6 === Nat.zero)
      ||| (y5 === Nat.succ q1 &&& (y6 === Nat.succ q2) &&& (q1 === Nat.zero &&& (q2 === Nat.zero) ||| (q1 === Nat.succ q3 &&& (q2 === Nat.succ q4) &&& _eqNat q3 q4))) )
  and _eqNat y7 y8 = fresh (q1 q2) (y7 === Nat.zero &&& (y8 === Nat.zero) ||| (y7 === Nat.succ q1 &&& (y8 === Nat.succ q2) &&& _eqNat q1 q2))
  and eqPair y9 y10 y11 =
    fresh (q1 q2)
      (y11 === Pair.pair q1 q2 &&& (__eqNat y9 q1 &&& ___eqNat y10 q2) ||| (y11 === Pair.pair q1 q2 &&& (_eqNat y9 q1 &&& ____eqNat y10 q2)))
  and __eqNat y12 y13 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8)
      ( y12 === Nat.succ q1 &&& (y13 === Nat.zero)
      ||| (y12 === Nat.zero &&& (y13 === Nat.succ q2))
      ||| ( y12 === Nat.succ q3
          &&& (y13 === Nat.succ q4)
          &&& (q3 === Nat.succ q5 &&& (q4 === Nat.zero) ||| (q3 === Nat.zero &&& (q4 === Nat.succ q6)) ||| (q3 === Nat.succ q7 &&& (q4 === Nat.succ q8) &&& ____eqNat q7 q8)) ) )
  and ___eqNat y14 y15 =
    fresh (q1 q2 q3 q4)
      ( y14 === Nat.zero &&& (y15 === Nat.zero)
      ||| (y14 === Nat.succ q1 &&& (y15 === Nat.zero))
      ||| (y14 === Nat.zero &&& (y15 === Nat.succ q2))
      ||| (y14 === Nat.succ q3 &&& (y15 === Nat.succ q4) &&& ___eqNat q3 q4) )
  and ____eqNat y17 y18 =
    fresh (q1 q2 q3 q4)
      (y17 === Nat.succ q1 &&& (y18 === Nat.zero) ||| (y17 === Nat.zero &&& (y18 === Nat.succ q2)) ||| (y17 === Nat.succ q3 &&& (y18 === Nat.succ q4) &&& ____eqNat q3 q4))
  and _isPath y19 y20 =
    fresh (q1 q2 q3 q4)
      ( y19 === List.nil ()
      ||| (y19 === ( % ) q1 (List.nil ()))
      ||| (y19 === ( % ) q2 (( % ) q3 q4) &&& (elem y20 q2 q3 &&& _isPath (( % ) q3 q4) y20)) )
  in
  _isPath x0 x1