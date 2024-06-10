open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec applasto y0 y1 y2 = (fresh (q1 q2) ((((y0 === (List.nil ())) &&& (lasto y1 y2)) ||| ((y0 === (q1 % q2)) &&& (appendoLasto y1 y2 q1 q2))))) 
  and lasto y3 y4 = (y3 === y4) 
  and appendoLasto y5 y6 y7 y8 = (fresh (q1 q2) ((((y8 === (List.nil ())) &&& (lasto y5 y6)) ||| ((y8 === (q1 % q2)) &&& (appendoLasto y5 y6 q1 q2))))) 
  in    (applasto x0 x1 x2)
