open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 x4 = 
  let rec sumtrsquaretr y0 y1 y2 y3 = (fresh (q1 q2 q3 q4 q5) ((((y0 === Nat.zero) &&& (multiplyAdd y2 y3 q1) &&& (multiply y1 y1 q1)) ||| ((y3 === (Nat.succ q2)) &&& (addMultiplyAddMultiplyAddAddAdd q3 q4 q5 q2) &&& (y0 === (Nat.succ q3)) &&& (multiply y2 y2 q4) &&& (multiply y1 y1 q5))))) 
  and multiplyAdd y4 y5 y7 = (fresh (q1 q2 q3) ((((y5 === y7) &&& (y4 === Nat.zero)) ||| ((y5 === (Nat.succ q1)) &&& (_addAdd y7 q2 q3 q1) &&& (y4 === (Nat.succ q2)) &&& (multiply ((Nat.succ q2)) q2 q3))))) 
  and multiply y8 y9 y10 = (fresh (q1 q2) ((((y10 === Nat.zero) &&& (y9 === Nat.zero)) ||| ((y9 === (Nat.succ q1)) &&& (_add q2 y8 y10) &&& (multiply y8 q1 q2))))) 
  and _add y14 y15 y16 = (fresh (q1 q2) ((((y15 === Nat.zero) &&& (y14 === y16)) ||| ((y16 === (Nat.succ q1)) &&& (y15 === (Nat.succ q2)) &&& (_add y14 q2 q1))))) 
  and addMultiplyAddMultiplyAddAddAdd y19 y21 y22 y26 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10) ((((y19 === Nat.zero) &&& (_add ((Nat.succ y22)) y21 y26)) ||| ((y26 === (Nat.succ q1)) &&& (__addAdd ((Nat.succ ((Nat.succ q2)))) q3 q4 q5 q2) &&& (y19 === (Nat.succ q5)) &&& (__addAdd ((Nat.succ ((Nat.succ q6)))) q7 q8 q5 q6) &&& (multiply ((Nat.succ ((Nat.succ q5)))) q5 q4) &&& (multiply ((Nat.succ ((Nat.succ q5)))) q5 q8) &&& (_add y22 q7 q9) &&& (_add ((Nat.succ ((Nat.succ q9)))) y21 q10) &&& (_add q10 q3 q1))))) 
  and _addAdd y32 y33 y34 y36 = (fresh (q1 q2) ((((y33 === Nat.zero) &&& (_add y32 y34 y36)) ||| ((y36 === (Nat.succ q1)) &&& (y33 === (Nat.succ q2)) &&& (_addAdd y32 q2 y34 q1))))) 
  and __addAdd y37 y38 y39 y40 y41 = (fresh (q1 q2 q3) ((((y40 === Nat.zero) &&& (y37 === y38) &&& (_add y39 Nat.zero y41)) ||| ((y41 === (Nat.succ q1)) &&& (y40 === (Nat.succ q2)) &&& (y38 === (Nat.succ q3)) &&& (__addAdd y37 q3 y39 q2 q1))))) 
  in        (sumtrsquaretr x0 x1 x3 x4)
