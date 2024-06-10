open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec depth y0 = (fresh (q1 q2) ((((y0 === (Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ q1)))))))))))))) &&& (_depth q1)) ||| ((y0 === (Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ q2)))))))))))))))))) &&& (prog_clauseDepth q2))))) 
  and _depth y1 = (y1 === Nat.zero) 
  and prog_clauseDepth y5 = (fresh (q1) (((_depth y5) ||| ((y5 === (Nat.succ ((Nat.succ ((Nat.succ q1)))))) &&& (_prog_clauseDepth q1))))) 
  and _prog_clauseDepth y9 = (fresh (q1) (((_depth y9) ||| ((y9 === (Nat.succ q1)) &&& (__prog_clauseDepth q1))))) 
  and __prog_clauseDepth y13 = (_depth y13) 
  in      (depth x2)
