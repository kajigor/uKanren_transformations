open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec addo y0 y1 = (fresh (x2 x3 x4 x5) ((success &&& (x2 === (Nat.succ Nat.zero)) &&& (y1 === (Nat.succ x3)) &&& (x4 === Nat.zero) &&& (x3 === (Nat.succ x5)) &&& (y0 === x5)))) 
  in  (addo x0 x1)
