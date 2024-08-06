open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 = 
  let rec evalo y0 y1 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10) ((((y0 === (conj q1 q2)) &&& (((!!false === !!true) &&& (q3 === !!false) &&& (q4 === !!false)) ||| ((!!false === !!true) &&& (q3 === !!true) &&& (q4 === !!false)) ||| ((!!false === !!true) &&& (q3 === !!false) &&& (q4 === !!true)) ||| ((!!false === !!false) &&& (q3 === !!true) &&& (q4 === !!true))) &&& (_evalo y1 q1 q4) &&& (_evalo y1 q2 q3)) ||| ((y0 === (disj q1 q2)) &&& (_nando q5 q4) &&& (_evalo y1 q1 q4) &&& (((q6 === !!false) &&& (q5 === !!false)) ||| ((q6 === !!true) &&& (q5 === !!false)) ||| ((q6 === !!false) &&& (q5 === !!true))) &&& (_nando q6 q3) &&& (_evalo y1 q2 q3)) ||| ((y0 === (neg q1)) &&& (__nando q4) &&& (_evalo y1 q1 q4)) ||| ((y0 === (var q7)) &&& ((y1 === (((pair q7 !!true)) % q8)) ||| ((y1 === (((pair q9 q10)) % q8)) &&& (_assoco q8 q7)))) ||| (y0 === (lit !!true))))) 
  and nando y2 y3 y4 = (((y4 === !!true) &&& (y3 === !!false) &&& (y2 === !!false)) ||| ((y4 === !!true) &&& (y3 === !!true) &&& (y2 === !!false)) ||| ((y4 === !!true) &&& (y3 === !!false) &&& (y2 === !!true)) ||| ((y4 === !!false) &&& (y3 === !!true) &&& (y2 === !!true))) 
  and _evalo y5 y6 y7 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11) ((((y6 === (conj q1 q2)) &&& (nando q3 q4 q5) &&& (_evalo y5 q1 q3) &&& (((q5 === !!false) &&& (y7 === !!true)) ||| ((q5 === !!true) &&& (y7 === !!false))) &&& (_evalo y5 q2 q4)) ||| ((y6 === (disj q1 q2)) &&& (_nando q6 q3) &&& (_evalo y5 q1 q3) &&& (nando q6 q7 y7) &&& (_nando q7 q4) &&& (_evalo y5 q2 q4)) ||| ((y6 === (neg q1)) &&& (_nando y7 q3) &&& (_evalo y5 q1 q3)) ||| ((y6 === (var q8)) &&& ((y5 === (((pair q8 y7)) % q9)) ||| ((y5 === (((pair q10 q11)) % q9)) &&& (assoco q9 y7 q8)))) ||| (y6 === (lit y7))))) 
  and _nando y8 y9 = (((y9 === !!false) &&& (y8 === !!true)) ||| ((y9 === !!true) &&& (y8 === !!false))) 
  and assoco y10 y11 y12 = (fresh (q1 q2 q3) (((y10 === (((pair y12 y11)) % q1)) ||| ((y10 === (((pair q2 q3)) % q1)) &&& (assoco q1 y11 y12))))) 
  and __nando y13 = (y13 === !!false) 
  and _assoco y14 y15 = (fresh (q1 q2 q3) (((y14 === (((pair y15 !!true)) % q1)) ||| ((y14 === (((pair q2 q3)) % q1)) &&& (_assoco q1 y15))))) 
  in        (evalo x0 x1)
