open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec rr y0 y1 = (fresh (q1 q2 q3 q4 q5) ((((y1 === (List.nil ())) &&& (y0 === (List.nil ()))) ||| ((y0 === (q1 % ((q1 % q2)))) &&& (rR y1 q1 q2)) ||| ((y0 === (q3 % ((q4 % q5)))) &&& (neqRR y1 q3 q4 q5))))) 
  and rR y2 y3 y4 = (fresh (q1 q2 q3 q4 q5 q6 q7) ((((y4 === (q1 % ((q1 % q2)))) &&& (r q2 q3) &&& (r ((y3 % ((q1 % q3)))) y2)) ||| ((y4 === (q4 % ((q5 % q6)))) &&& (neq q4 q5) &&& (r ((q5 % q6)) q7) &&& (r ((y3 % ((q4 % q7)))) y2))))) 
  and r y6 y7 = (fresh (q1 q2 q3 q4 q5 q6 q7) ((((y7 === (List.nil ())) &&& (y6 === (List.nil ()))) ||| ((y7 === (q1 % q2)) &&& (y6 === (q1 % ((q1 % q3)))) &&& (r q3 q2)) ||| ((y7 === (q4 % q5)) &&& (neq q4 q6) &&& (y6 === (q4 % ((q6 % q7)))) &&& (r ((q6 % q7)) q5))))) 
  and neq y8 y9 = (fresh (q1 q2 q3 q4) ((((y9 === (Nat.succ q1)) &&& (y8 === Nat.zero)) ||| ((y9 === Nat.zero) &&& (y8 === (Nat.succ q2))) ||| ((y9 === (Nat.succ q3)) &&& (y8 === (Nat.succ q4)) &&& (neq q4 q3))))) 
  and neqRR y10 y11 y12 y13 = (fresh (q1 q2 q3 q4) ((((y12 === (Nat.succ q1)) &&& (y11 === Nat.zero) &&& (rR y10 Nat.zero ((((Nat.succ q1)) % y13)))) ||| ((y12 === Nat.zero) &&& (y11 === (Nat.succ q2)) &&& (rR y10 ((Nat.succ q2)) ((Nat.zero % y13)))) ||| ((y12 === (Nat.succ q3)) &&& (rR y10 ((Nat.succ q4)) ((((Nat.succ q3)) % y13))) &&& (y11 === (Nat.succ q4)) &&& (neq q4 q3))))) 
  in      (rr x0 x1)
