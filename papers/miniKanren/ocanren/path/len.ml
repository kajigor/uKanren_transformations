open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x2 x3 x4 x5 x6 =
  let rec isPath y0 y1 y2 y3 y4 y5 = elem y0 y1 y2 &&& _isPath y0 y3 y4 y5 y2
  and _isPath y6 y7 y8 y9 y10 = elem y6 y10 y7 &&& __isPath y6 y8 y9 y7
  and elem y11 y12 y13 =
    fresh (q1 q2 q3 q4)
      ( y11 === ( % ) (Pair.pair q1 q2) q3
      &&& (eqNat y12 q1 &&& _eqNat y13 q2)
      ||| (y11 === ( % ) q4 q3 &&& (eqPair y12 y13 q4 &&& elem q3 y12 y13)) )
  and __isPath y14 y15 y16 y17 = elem y14 y17 y15 &&& ___isPath y14 y16 y15
  and ___isPath y18 y19 y20 =
    fresh (q1 q2 q3 q4)
      ( y18 === ( % ) (Pair.pair q1 q2) q3
      &&& (eqNat y20 q1 &&& _eqNat y19 q2)
      ||| (y18 === ( % ) q4 q3 &&& (eqPair y20 y19 q4 &&& elem q3 y20 y19)) )
  and eqNat y21 y22 =
    fresh (q1 q2 q3 q4)
      ( y21 === Nat.zero &&& (y22 === Nat.zero)
      ||| (y21 === Nat.succ q1 &&& (y22 === Nat.succ q2) &&& (q1 === Nat.zero &&& (q2 === Nat.zero) ||| (q1 === Nat.succ q3 &&& (q2 === Nat.succ q4) &&& _eqNat q3 q4))) )
  and _eqNat y23 y24 = fresh (q1 q2) (y23 === Nat.zero &&& (y24 === Nat.zero) ||| (y23 === Nat.succ q1 &&& (y24 === Nat.succ q2) &&& _eqNat q1 q2))
  and eqPair y25 y26 y27 =
    fresh (q1 q2)
      (y27 === Pair.pair q1 q2 &&& (__eqNat y25 q1 &&& ___eqNat y26 q2) ||| (y27 === Pair.pair q1 q2 &&& (_eqNat y25 q1 &&& ____eqNat y26 q2)))
  and __eqNat y28 y29 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8)
      ( y28 === Nat.succ q1 &&& (y29 === Nat.zero)
      ||| (y28 === Nat.zero &&& (y29 === Nat.succ q2))
      ||| ( y28 === Nat.succ q3
          &&& (y29 === Nat.succ q4)
          &&& (q3 === Nat.succ q5 &&& (q4 === Nat.zero) ||| (q3 === Nat.zero &&& (q4 === Nat.succ q6)) ||| (q3 === Nat.succ q7 &&& (q4 === Nat.succ q8) &&& ____eqNat q7 q8)) ) )
  and ___eqNat y30 y31 =
    fresh (q1 q2 q3 q4)
      ( y30 === Nat.zero &&& (y31 === Nat.zero)
      ||| (y30 === Nat.succ q1 &&& (y31 === Nat.zero))
      ||| (y30 === Nat.zero &&& (y31 === Nat.succ q2))
      ||| (y30 === Nat.succ q3 &&& (y31 === Nat.succ q4) &&& ___eqNat q3 q4) )
  and ____eqNat y33 y34 =
    fresh (q1 q2 q3 q4)
      (y33 === Nat.succ q1 &&& (y34 === Nat.zero) ||| (y33 === Nat.zero &&& (y34 === Nat.succ q2)) ||| (y33 === Nat.succ q3 &&& (y34 === Nat.succ q4) &&& ____eqNat q3 q4))
  in
  isPath x0 x2 x3 x4 x5 x6