open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec sumsquaresupto y0 = (_uptoMultiplySquaresAddSum1 y0 Nat.zero Nat.zero) 
  and _uptoMultiplySquaresAddSum1 y7 y8 y13 = (fresh (q1) ((((y8 === Nat.zero) &&& (uptoSquaresSum1 y7 y13)) ||| ((y8 === (Nat.succ q1)) &&& (leUptoAddAddAddMultiplySquaresAddSum1 y7 y13 q1))))) 
  and uptoSquaresSum1 y15 y16 = ((y15 === (Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ y16)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) ||| (_uptoMultiplySquaresAddSum1 y15 ((Nat.succ ((Nat.succ ((Nat.succ Nat.zero)))))) ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ y16)))))))))))))))))))))))))))))))))))))))))))))))))))))))))))) 
  and leUptoAddAddAddMultiplySquaresAddSum1 y19 y21 y25 = (((y25 === Nat.zero) &&& (_uptoSquaresSum1 y19 ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ y21)))))))))))))))))))) ||| ((y25 === (Nat.succ Nat.zero)) &&& (_uptoSquaresSum1 y19 y21))) 
  and _uptoSquaresSum1 y29 y31 = (y29 === (Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ ((Nat.succ y31)))))))))))))))))))))))))))))))))) 
  in      (sumsquaresupto x0)
