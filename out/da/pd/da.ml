open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 = 
  let rec doubleAppendo y0 y1 y2 y3 = (fresh (q1 q2 q3 q4) (((((y1 === q1) &&& (y0 === (List.nil ()))) ||| ((q1 === (q2 % q3)) &&& (y0 === (q2 % q4)) &&& (appendo q4 y1 q3))) &&& (appendo q1 y2 y3)))) 
  and appendo y4 y5 y6 = (fresh (q1 q2 q3) ((((y5 === y6) &&& (y4 === (List.nil ()))) ||| ((y6 === (q1 % q2)) &&& (y4 === (q1 % q3)) &&& (appendo q3 y5 q2))))) 
  in   (doubleAppendo x0 x1 x2 x3)
