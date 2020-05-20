open GT
open OCanren
open OCanren.Std

let topLevel x0 x1 =
  let rec isPath y0 y1 = fresh (q1 q2 q3 q4) (y0 === nil () ||| (y0 === q1 % nil ()) ||| (y0 === q2 % (q3 % q4) &&& elemIsPath y1 q2 q3 q4))
  and elemIsPath y2 y3 y4 y5 =
    fresh (q1 q2 q3 q4)
      ( y2 === pair q1 q2 % q3
      &&& (___eqNat y3 q1 !!true &&& ___eqNat y4 q2 !!true &&& isPath (y4 % y5) (pair q1 q2 % q3))
      ||| (y2 === q4 % q3 &&& (eqPairElem y3 y4 q4 q3 &&& isPath (y4 % y5) (q4 % q3))) )
  and eqPairElem y10 y11 y12 y13 =
    fresh (q1 q2 q3)
      ( y12 === pair q1 q2
      &&& (eqNatElem y13 y10 y11 q1 &&& ___eqNat y11 q2 q3)
      ||| (y12 === pair q1 q2 &&& (___eqNat y10 q1 !!true &&& ___eqNat y11 q2 !!false &&& ____elem y13 y11 y10)) )
  and eqNatElem y14 y15 y16 y17 =
    fresh (q1 q2 q3 q4)
      ( y15 === Nat.succ q1
      &&& (y17 === Nat.zero)
      &&& ____elem y14 y16 (Nat.succ q1)
      ||| (y15 === Nat.zero &&& (y17 === Nat.succ q2) &&& ____elem y14 y16 (Nat.zero))
      ||| (y15 === Nat.succ q3 &&& (y17 === Nat.succ q4) &&& _eqNatElem y14 y16 q3 q4) )
  and _eqNatElem y26 y27 y28 y29 =
    fresh (q1 q2 q3 q4)
      ( y28 === Nat.succ q1
      &&& (y29 === Nat.zero)
      &&& ____elem y26 y27 (Nat.succ (Nat.succ q1))
      ||| (y28 === Nat.zero &&& (y29 === Nat.succ q2) &&& ____elem y26 y27 (Nat.succ (Nat.zero)))
      ||| (y28 === Nat.succ q3 &&& (y29 === Nat.succ q4) &&& (___eqNat q3 q4 !!false &&& ____elem y26 y27 (Nat.succ (Nat.succ q3)))) )
  and ____elem y39 y40 y41 =
    fresh (q1 q2 q3 q4) (y39 === pair q1 q2 % q3 &&& (___eqNat y41 q1 !!true &&& ___eqNat y40 q2 !!true) ||| (y39 === q4 % q3 &&& eqPairElem y41 y40 q4 q3))
  and ___eqNat y42 y43 y44 =
    fresh (q1 q2 q3 q4)
      ( y42 === Nat.zero
      &&& (y43 === Nat.zero)
      &&& (y44 === !!true)
      ||| (y42 === Nat.succ q1 &&& (y43 === Nat.zero) &&& (y44 === !!false))
      ||| (y42 === Nat.zero &&& (y43 === Nat.succ q2) &&& (y44 === !!false))
      ||| (y42 === Nat.succ q3 &&& (y43 === Nat.succ q4) &&& ___eqNat q3 q4 y44) )
  in
  isPath x0 x1
