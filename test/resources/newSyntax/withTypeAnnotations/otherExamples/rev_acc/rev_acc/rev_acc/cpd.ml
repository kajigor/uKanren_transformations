open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec rev y0 = (fresh (q1 q2) (((y0 === (q1 % q2)) &&& (_rev q2 ((q1 % ((List.nil ())))))))) 
  and _rev y1 y2 = (fresh (q1 q2) ((((y2 === (((a ())) % ((((b ())) % ((((c ())) % ((((d ())) % ((List.nil ())))))))))) &&& (y1 === (List.nil ()))) ||| ((y1 === (q1 % q2)) &&& (_rev q2 ((q1 % y2))))))) 
  in   (rev x0)
