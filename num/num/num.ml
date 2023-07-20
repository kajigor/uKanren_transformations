open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec addo y0 y1 = (y1 === (Nat.succ ((Nat.succ y0)))) 
  in  (addo x0 x1)
