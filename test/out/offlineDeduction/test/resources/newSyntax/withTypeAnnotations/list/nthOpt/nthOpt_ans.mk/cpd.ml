open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec nthOpt y0 y1 = (fresh (q1 q2) ((((y1 === (Option.none ())) &&& (y0 === (List.nil ()))) ||| ((y0 === (q1 % q2)) &&& (_nthOpt y1 q2))))) 
  and _nthOpt y2 y3 = (fresh (q1 q2) ((((y3 === (List.nil ())) &&& (y2 === (Option.none ()))) ||| ((y3 === (q1 % q2)) &&& (__nthOpt y2 q2))))) 
  and __nthOpt y4 y5 = (fresh (q1 q2) ((((y5 === (List.nil ())) &&& (y4 === (Option.none ()))) ||| ((y5 === (q1 % q2)) &&& (___nthOpt y4 q2))))) 
  and ___nthOpt y6 y7 = (fresh (q1 q2) ((((y7 === (List.nil ())) &&& (y6 === (Option.none ()))) ||| ((y7 === (q1 % q2)) &&& (y6 === (Option.some q1)))))) 
  in     (nthOpt x0 x2)
