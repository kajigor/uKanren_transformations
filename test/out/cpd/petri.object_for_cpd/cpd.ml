open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec unsafe y0 = (fresh (q1) ((____unsafe q1 ((Nat.succ Nat.zero)) Nat.zero))) 
  and ___unsafe y5 y6 y7 = (fresh (q1 q2) (((____unsafe q1 ((Nat.succ y6)) y7) ||| ((y6 === (Nat.succ q2)) &&& (___unsafe ((Nat.succ q1)) q2 ((Nat.succ y7))))))) 
  and ____unsafe y8 y9 y10 = (fresh (q1 q2 q3) (((___unsafe q1 y9 y10) ||| ((y9 === (Nat.succ q2)) &&& (____unsafe ((Nat.succ q3)) q2 ((Nat.succ y10))))))) 
  in    (unsafe x0)
