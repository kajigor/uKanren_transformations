open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec solve y0 y1 y2 = (_contrapositiveProveall y0 y1 y2 ((List.nil ()))) 
  and _contrapositiveProveall y15 y16 y17 y19 = (fresh (q1 q2 q3) ((_input_clauseProveall y15 y16 y17 y19))) 
  and _input_clauseProveall y20 y21 y22 y23 = (fresh (q1 q2 q3) ((((y21 === y22) &&& (y20 === (List.nil ()))) ||| ((y22 === (q1 % q2)) &&& (y20 === (q1 % q3)) &&& (_prove y23 q1 q3 y21 q2))))) 
  and _prove y25 y26 y27 y28 y29 = ((member y27 y28 y29 y25) ||| (_contrapositiveProveall y27 y28 y29 ((((pos ((app ((y26 % y27)) y28 ((y26 % y29)))))) % y25)))) 
  and member y30 y31 y32 y33 = (fresh (q1 q2 q3) (((y33 === (((neg ((app y30 y31 y32)))) % q1)) ||| ((y33 === (q2 % q3)) &&& (member y30 y31 y32 q3))))) 
  in      (solve x0 x1 x2)
