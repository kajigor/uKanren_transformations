open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec maxmin y0 y1 y2 = (fresh (q1 q2) ((((y2 === (zero ())) &&& (y1 === (zero ())) &&& (y0 === (List.nil ()))) ||| ((y0 === (q1 % q2)) &&& (_maxMin y1 y2 q1 q2 q2))))) 
  and _maxMin y7 y8 y9 y10 y11 = (fresh (q1 q2) ((((y10 === (List.nil ())) &&& (y7 === y9) &&& (min y8 y9 y11)) ||| ((y10 === (q1 % q2)) &&& (_maxMin y7 y8 y9 q2 y11) &&& (le y9 q1)) ||| ((y10 === (q1 % q2)) &&& (gt y9 q1) &&& (max y7 q2 q1) &&& (min y8 y9 y11))))) 
  and le y12 y13 = (fresh (q1 q2) (((y13 === (zero ())) ||| ((y13 === (succ q1)) &&& (y12 === (succ q2)) &&& (le q2 q1))))) 
  and max y14 y15 y16 = (fresh (q1 q2) ((((y15 === (List.nil ())) &&& (y14 === y16)) ||| ((y15 === (q1 % q2)) &&& (le y16 q1) &&& (max y14 q2 y16)) ||| ((y15 === (q1 % q2)) &&& (gt y16 q1) &&& (max y14 q2 q1))))) 
  and min y17 y18 y19 = (fresh (q1 q2) ((((y19 === (List.nil ())) &&& (y17 === y18)) ||| ((y19 === (q1 % q2)) &&& (le y18 q1) &&& (min y17 q1 q2)) ||| ((y19 === (q1 % q2)) &&& (gt y18 q1) &&& (min y17 y18 q2))))) 
  and gt y20 y21 = (fresh (q1 q2 q3) ((((y21 === (succ q1)) &&& (y20 === (zero ()))) ||| ((y21 === (succ q2)) &&& (y20 === (succ q3)) &&& (gt q3 q2))))) 
  in       (maxmin x0 x1 x2)
