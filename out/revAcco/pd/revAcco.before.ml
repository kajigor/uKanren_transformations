open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec revacco y0 y1 = (fresh (x2 x3) ((success &&& (((y1 === (List.nil ())) &&& (y0 === (List.nil ()))) ||| ((y0 === (x2 % x3)) &&& (_revacco y1 x3 ((x2 % ((List.nil ())))))))))) 
  and _revacco y2 y3 y4 = (fresh (x0 x2 x3 x6 x7) (((x0 === (x2 % x3)) &&& (((y3 === (List.nil ())) &&& (y2 === y4)) ||| ((y3 === (x6 % x7)) &&& (__revacco y2 x7 ((x6 % y4)))))))) 
  and __revacco y5 y6 y7 = (fresh (x4 x6 x7 x0 x2 x3) (((x4 === (x6 % x7)) &&& (x0 === (x2 % x3)) &&& (_revacco y5 y6 y7)))) 
  in    (revacco x0 x1)
