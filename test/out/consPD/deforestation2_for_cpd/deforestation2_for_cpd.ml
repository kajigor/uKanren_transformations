open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec rr y0 = (fresh (q1) (((r q1) &&& (_r y0 q1)))) 
  and r y1 = (fresh (q1) (((y1 === (((Nat.succ Nat.zero)) % ((((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % ((((s ((s ())) Nat.zero)) % ((((s ((s ())) Nat.zero)) % ((Nat.zero % ((Nat.zero % ((Nat.zero % ((Nat.zero % ((List.nil ())))))))))))))))))))))) ||| ((y1 === (((Nat.succ Nat.zero)) % ((Nat.zero % ((Nat.zero % q1)))))) &&& (__r q1))))) 
  and _r y2 y3 = (fresh (q1 q2 q3 q4 q5 q6 q7) ((((y3 === (List.nil ())) &&& (y2 === (List.nil ()))) ||| ((y3 === (q1 % q2)) &&& (y2 === (q1 % ((q1 % q3)))) &&& (_r q3 q2)) ||| ((y3 === (q4 % q5)) &&& (y2 === (q4 % ((q6 % q7)))) &&& (neq q4 q6) &&& (_r ((q6 % q7)) q5))))) 
  and __r y4 = (y4 === (((s ((s ())) Nat.zero)) % ((((s ((s ())) Nat.zero)) % ((Nat.zero % ((Nat.zero % ((Nat.zero % ((Nat.zero % ((List.nil ())))))))))))))) 
  and neq y5 y6 = (fresh (q1 q2 q3 q4) ((((y6 === (Nat.succ q1)) &&& (y5 === Nat.zero)) ||| ((y6 === Nat.zero) &&& (y5 === (Nat.succ q2))) ||| ((y6 === (Nat.succ q3)) &&& (y5 === (Nat.succ q4)) &&& (neq q4 q3))))) 
  in      (rr x0)
