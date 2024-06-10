open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec unsafe y0 = (fresh (q1) ((__unsafe q1 ((Nat.succ Nat.zero)) Nat.zero))) 
  and __unsafe y3 y4 y5 = (fresh (q1 q2 q3 q4) (((__unsafe q1 ((Nat.succ y4)) y5) ||| ((y4 === (Nat.succ q2)) &&& (___unsafe y5 q2)) ||| ((y4 === (Nat.succ q3)) &&& (__unsafe ((Nat.succ q4)) q3 ((Nat.succ y5))))))) 
  and ___unsafe y6 y8 = (fresh (q1 q2) (((__unsafe q1 ((Nat.succ y8)) ((Nat.succ y6))) ||| ((y8 === (Nat.succ q2)) &&& (___unsafe ((Nat.succ y6)) q2))))) 
  and _____unsafe y11 y13 = (fresh (q1 q2) (((__unsafe q1 ((Nat.succ q2)) ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ y13))))))))))))))))))) ||| (_____unsafe ((Nat.succ q1)) ((Nat.succ y13)))))) 
  in     (unsafe x0)
