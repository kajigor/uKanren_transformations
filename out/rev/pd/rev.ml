open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec _reverso y2 y3 = (fresh (q1 q2 q3 q4 q5 q6) ((((y3 === (List.nil ())) &&& (y2 === (List.nil ()))) ||| ((y2 === (q1 % q2)) &&& (_reverso q2 q3) &&& (((q3 === (List.nil ())) &&& (y3 === (q1 % ((List.nil ()))))) ||| ((q3 === (q4 % q5)) &&& (y3 === (q4 % q6)) &&& (appendo q6 q1 q5))))))) 
  and appendo y4 y5 y6 = (fresh (q1 q2 q3) ((((y6 === (List.nil ())) &&& (y4 === (y5 % ((List.nil ()))))) ||| ((y6 === (q1 % q2)) &&& (y4 === (q1 % q3)) &&& (appendo q3 y5 q2))))) 
  in   (_reverso x0 x1)
