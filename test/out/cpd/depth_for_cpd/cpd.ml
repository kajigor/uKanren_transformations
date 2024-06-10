open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec depth y0 = ((y0 === (Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))))))))))) ||| (y0 === (Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))))))))))))))) ||| (y0 === (Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))))))))))))))))))))) ||| (y0 === (Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ Nat.zero))))))))))))))))))))))))))) 
  in  (depth x0)
