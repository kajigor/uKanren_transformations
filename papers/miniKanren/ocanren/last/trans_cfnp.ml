open OCanren
open GT
open Helper

(* close fresh, no purification *)
let topLevel x0 x1 =
  let rec _evalo y2 y3 =
    fresh (x7 x5 x4 x3 x2) (y3 === conj x2 x3 &&& (x4 === !!true &&& (x5 === !!true) &&& (x7 === !!false) &&& ___evaloEvalo y2 x2 x3))
    ||| fresh (x3 x2)
          ( y3 === disj x2 x3
          &&& ( fresh (x8 x5 x7 x4)
                  (x4 === !!false &&& (x7 === !!true) &&& (x5 === !!true) &&& (x8 === !!false) &&& (success &&& __evalo y2 x2 &&& (success &&& _evalo y2 x3)))
              ||| fresh (x8 x5 x7 x4) (x4 === !!true &&& (x7 === !!false) &&& (x5 === !!false) &&& (x8 === !!true) &&& __evaloEvalo y2 x2 x3)
              ||| fresh (x8 x5 x7 x4) (x4 === !!true &&& (x7 === !!false) &&& (x5 === !!true) &&& (x8 === !!false) &&& ___evaloEvalo y2 x2 x3) ) )
    ||| fresh (x4 x2) (y3 === neg x2 &&& (x4 === !!false &&& __evalo y2 x2))
    ||| fresh (x6)
          ( y3 === var x6
          &&& ( fresh (x8 x7) (x6 === Std.Nat.zero &&& (y2 === Std.( % ) x7 x8) &&& (x7 === !!true))
              ||| fresh (x8 x7 x9) (x6 === Std.Nat.succ x9 &&& (y2 === Std.( % ) x7 x8) &&& _elemo x8 x9) ) )
  and __evalo y4 y5 =
    fresh (x10 x9)
      ( y5 === conj x9 x10
      &&& ( fresh (x14 x12 x11) (x11 === !!false &&& (x12 === !!false) &&& (x14 === !!true) &&& _evaloEvalo y4 x9 x10)
          ||| fresh (x14 x12 x11) (x11 === !!false &&& (x12 === !!true) &&& (x14 === !!true) &&& evaloEvalo y4 x9 x10)
          ||| fresh (x14 x12 x11) (x11 === !!true &&& (x12 === !!false) &&& (x14 === !!true) &&& (success &&& _evalo y4 x9 &&& (success &&& __evalo y4 x10)))
          ) )
    ||| fresh (x15 x12 x14 x11 x10 x9)
          (y5 === disj x9 x10 &&& (x11 === !!false &&& (x14 === !!true) &&& (x12 === !!false) &&& (x15 === !!true) &&& _evaloEvalo y4 x9 x10))
    ||| fresh (x11 x9) (y5 === neg x9 &&& (x11 === !!true &&& _evalo y4 x9))
    ||| fresh (x13)
          ( y5 === var x13
          &&& ( fresh (x15 x14) (x13 === Std.Nat.zero &&& (y4 === Std.( % ) x14 x15) &&& (x14 === !!false))
              ||| fresh (x15 x14 x16) (x13 === Std.Nat.succ x16 &&& (y4 === Std.( % ) x14 x15) &&& elemo x15 x16) ) )
  and evaloEvalo y6 y7 y8 = __evalo y6 y7 &&& _evalo y6 y8
  and _evaloEvalo y9 y10 y11 = __evalo y9 y10 &&& __evalo y9 y11
  and elemo y12 y13 =
    fresh (x15 x14) (y13 === Std.Nat.zero &&& (y12 === Std.( % ) x14 x15) &&& (x14 === !!false))
    ||| fresh (x15 x14 x16) (y13 === Std.Nat.succ x16 &&& (y12 === Std.( % ) x14 x15) &&& elemo x15 x16)
  and __evaloEvalo y14 y15 y16 = _evalo y14 y15 &&& __evalo y14 y16
  and ___evaloEvalo y17 y18 y19 = _evalo y17 y18 &&& _evalo y17 y19
  and _elemo y20 y21 =
    fresh (x8 x7) (y21 === Std.Nat.zero &&& (y20 === Std.( % ) x7 x8) &&& (x7 === !!true))
    ||| fresh (x8 x7 x9) (y21 === Std.Nat.succ x9 &&& (y20 === Std.( % ) x7 x8) &&& _elemo x8 x9)
  in
  _evalo x0 x1