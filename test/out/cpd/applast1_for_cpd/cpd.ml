open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec applasto y0 = (fresh (q1 q2) (((y0 === (List.nil ())) ||| ((y0 === (q1 % q2)) &&& (appendoLasto q1 q2))))) 
  and appendoLasto y1 y2 = (fresh (q1 q2) (((y2 === (List.nil ())) ||| ((y2 === (q1 % q2)) &&& (appendoLasto q1 q2))))) 
  in   (applasto x0)
