open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 = 
  let rec transpose y0 y1 y2 y3 = (fresh (q1 q2 q3 q4 q5 q6 q7) ((((y3 === (List.nil ())) &&& (y2 === (List.nil ())) &&& (y1 === (List.nil ())) &&& (y0 === (List.nil ()))) ||| ((y3 === (q1 % q2)) &&& (y2 === (q3 % q4)) &&& (y1 === (q5 % q6)) &&& (y0 === (((q5 % ((q3 % ((q1 % ((List.nil ())))))))) % q7)) &&& (nullrowsMakerowMakerowMakerow q2 q4 q7 q6))))) 
  and nullrowsMakerowMakerowMakerow y4 y6 y8 y9 = (fresh (q1 q2 q3 q4 q5 q6 q7) ((((y9 === (List.nil ())) &&& (y8 === (List.nil ())) &&& (y6 === (List.nil ())) &&& (y4 === (List.nil ()))) ||| ((y9 === (q1 % q2)) &&& (y8 === (((q1 % ((q3 % ((q4 % ((List.nil ())))))))) % q5)) &&& (y6 === (q3 % q6)) &&& (y4 === (q4 % q7)) &&& (nullrowsMakerowMakerowMakerow q7 q6 q5 q2))))) 
  in   (transpose x0 x1 x2 x3)
