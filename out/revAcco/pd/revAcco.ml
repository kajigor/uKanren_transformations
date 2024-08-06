open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec revacco y0 y1 = (fresh (q1 q2) ((((y1 === (List.nil ())) &&& (y0 === (List.nil ()))) ||| ((y0 === (q1 % q2)) &&& (_revacco y1 q2 ((q1 % ((List.nil ()))))))))) 
  and _revacco y2 y3 y4 = (fresh (q1 q2) ((((y3 === (List.nil ())) &&& (y2 === y4)) ||| ((y3 === (q1 % q2)) &&& (__revacco y2 q2 ((q1 % y4))))))) 
  and __revacco y5 y6 y7 = (_revacco y5 y6 y7) 
  in    (revacco x0 x1)
