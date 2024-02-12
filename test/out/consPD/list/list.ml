open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 x4 = 
  let rec appendo y5 y6 y7 = (fresh (q1 q2 q3) ((((y6 === (List.nil ())) &&& (y5 === y7)) ||| ((y7 === (q1 % q2)) &&& (y6 === (q1 % q3)) &&& (appendo y5 q3 q2))))) 
  and _appendoAppendo y8 y9 y10 y11 y12 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10) ((((y11 === y12) &&& (y10 === (List.nil ())) &&& (y9 === y10) &&& (y8 === (List.nil ()))) ||| ((y12 === (q1 % q2)) &&& (y10 === (q1 % q3)) &&& (y9 === y10) &&& (y8 === (List.nil ())) &&& (((q3 === (List.nil ())) &&& (y11 === q2)) ||| ((q2 === (q4 % q5)) &&& (q3 === (q4 % q6)) &&& (appendo y11 q6 q5)))) ||| ((y12 === (q7 % q8)) &&& (y10 === (q7 % q9)) &&& (y8 === (q7 % q10)) &&& (_appendoAppendo q10 y9 q9 y11 q8))))) 
  in   (_appendoAppendo x0 x1 x2 x3 x4)
