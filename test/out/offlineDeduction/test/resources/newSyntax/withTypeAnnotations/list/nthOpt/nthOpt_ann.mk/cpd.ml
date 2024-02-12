open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = 
  let rec nthOpt y0 y1 = (fresh (q1 q2 q3 q4 q5) ((((y1 === (Option.none ())) &&& (y0 === (List.nil ()))) ||| ((y1 === (Option.none ())) &&& (y0 === (q1 % ((List.nil ()))))) ||| ((y1 === (Option.none ())) &&& (y0 === (q1 % ((q2 % ((List.nil ()))))))) ||| ((y1 === (Option.none ())) &&& (y0 === (q1 % ((q2 % ((q3 % ((List.nil ()))))))))) ||| ((y1 === (Option.some q4)) &&& (y0 === (q1 % ((q2 % ((q3 % ((q4 % q5)))))))))))) 
  in  (nthOpt x0 x2)
