open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 = 
  let rec sumtrsquaretr y0 y1 y2 y3 = (fresh (q1 q2 q3 q4 q5 q6) ((((y0 === Nat.zero) &&& (squareAdd Nat.zero q1 q2) &&& (squareAdd y1 y3 q1) &&& (square y2 q2)) ||| ((y3 === (Nat.succ q3)) &&& (addMultiplySquareAddAddAdd q4 q5 q6 q3) &&& (y0 === (Nat.succ q4)) &&& (square y1 q5) &&& (square y2 q6))))) 
  and squareAdd y4 y5 y6 = (fresh (q1 q2) ((((y5 === y6) &&& (y4 === Nat.zero)) ||| ((y5 === (Nat.succ q1)) &&& (y4 === (Nat.succ q2)) &&& (addMultiplyAdd y6 q2 q1))))) 
  and addMultiplyAdd y8 y10 y12 = (fresh (q1 q2 q3) ((((y10 === Nat.zero) &&& (y8 === y12)) ||| ((y12 === (Nat.succ q1)) &&& (_addMultiply q2 q3 ((Nat.succ q2)) q2) &&& (y10 === (Nat.succ q2)) &&& (addAdd y8 q2 ((Nat.succ ((Nat.succ q3)))) q1))))) 
  and addAdd y13 y14 y15 y17 = (fresh (q1 q2) ((((y14 === Nat.zero) &&& (add y13 y15 y17)) ||| ((y17 === (Nat.succ q1)) &&& (y14 === (Nat.succ q2)) &&& (addAdd y13 q2 y15 q1))))) 
  and multiply y18 y19 y20 = (fresh (q1 q2) ((((y20 === Nat.zero) &&& (y19 === Nat.zero)) ||| ((y20 === (Nat.succ q1)) &&& (y19 === (Nat.succ q2)) &&& (_addMultiply y18 q1 y18 q2))))) 
  and add y21 y22 y23 = (fresh (q1 q2) ((((y22 === Nat.zero) &&& (y21 === y23)) ||| ((y23 === (Nat.succ q1)) &&& (y22 === (Nat.succ q2)) &&& (add y21 q2 q1))))) 
  and square y24 y25 = (fresh (q1 q2) ((((y25 === Nat.zero) &&& (y24 === Nat.zero)) ||| ((y25 === (Nat.succ q1)) &&& (y24 === (Nat.succ q2)) &&& (_addMultiply q2 q1 q2 q2))))) 
  and _addMultiply y29 y31 y32 y33 = (fresh (q1 q2) ((((y29 === Nat.zero) &&& (multiply y32 y33 y31)) ||| ((y31 === (Nat.succ q1)) &&& (y29 === (Nat.succ q2)) &&& (_addMultiply q2 q1 y32 y33))))) 
  and addMultiplySquareAddAddAdd y35 y38 y40 y42 = (fresh (q1 q2 q3 q4) ((((y35 === Nat.zero) &&& (squareAddAdd y42 y38 y40 Nat.zero)) ||| ((y42 === (Nat.succ q1)) &&& (squareAddAdd q2 y38 y40 ((Nat.succ q3))) &&& (y35 === (Nat.succ q3)) &&& (_addMultiply q3 q4 ((Nat.succ q3)) q3) &&& (addAdd q2 q3 ((Nat.succ ((Nat.succ q4)))) q1))))) 
  and squareAddAdd y43 y45 y47 y48 = (fresh (q1) ((((y48 === Nat.zero) &&& (add ((Nat.succ y47)) y45 y43)) ||| ((y48 === (Nat.succ q1)) &&& (addMultiplyAddAdd y43 y45 y47 q1))))) 
  and addMultiplyAddAdd y49 y50 y51 y53 = (fresh (q1) ((((y53 === Nat.zero) &&& (add ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ y51)))))))) y50 y49)) ||| ((y53 === (Nat.succ q1)) &&& (addAddAddMultiplyAddAdd y49 y50 y51 q1))))) 
  and addAddAddMultiplyAddAdd y56 y57 y58 y61 = (fresh (q1 q2 q3 q4 q5) ((((y61 === Nat.zero) &&& (add ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ y58)))))))))))))))))) y57 y56)) ||| ((y61 === (Nat.succ q1)) &&& (_addMultiply q1 q2 ((Nat.succ ((Nat.succ ((Nat.succ q1)))))) q1) &&& (addAdd y58 q1 ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ q3)))))))) q4) &&& (add ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ q5)))))))) q1 q3) &&& (add ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ q2)))))))) q1 q5) &&& (add ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ q4)))))))) y57 y56))))) 
  in             (sumtrsquaretr x0 x1 x2 x3)
