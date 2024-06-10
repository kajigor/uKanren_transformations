open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec sorto y0 = (fresh (q1 q2 q3 q4 q5) (((y0 === (((zero ())) % q1)) &&& (sortoAppendoAppendo q2 q3 q1) &&& (__sorto q4 q3) &&& (__sorto q5 q2)))) 
  and sortoAppendoAppendo y1 y2 y6 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8) ((((y6 === (((zero ())) % q1)) &&& (_appendo q1 ((succ ((zero ())))) y2 y1)) ||| ((splitoAppendo q2 q3 q4 q5 q6 q7) &&& (appendoAppendo y1 y2 q2 y6) &&& (__sorto q5 q6) &&& (__sorto q8 q7))))) 
  and appendoAppendo y10 y11 y12 y14 = (fresh (q1 q2 q3) ((((y12 === (List.nil ())) &&& (_appendo y14 ((succ ((zero ())))) ((((zero ())) % y11)) y10)) ||| ((y14 === (q1 % q2)) &&& (y12 === (q1 % q3)) &&& (appendoAppendo y10 y11 q3 q2))))) 
  and splitoAppendo y15 y16 y17 y18 y20 y21 = (fresh (q1 q2 q3) ((((y18 === (List.nil ())) &&& (y17 === (List.nil ())) &&& (_appendo y15 y16 y20 y21)) ||| ((y18 === (q1 % q2)) &&& (splitoAppendo y15 y16 q3 q2 y20 y21) &&& (y17 === (q1 % q3)) &&& (le y16 q1)) ||| ((y17 === (q1 % q3)) &&& (splitoAppendo y15 y16 q3 y18 y20 y21) &&& (gt y16 q1))))) 
  and _appendo y22 y23 y24 y25 = (fresh (q1 q2 q3) ((((y24 === (List.nil ())) &&& (y22 === (y23 % y25))) ||| ((y24 === (q1 % q2)) &&& (y22 === (q1 % q3)) &&& (_appendo q3 y23 q2 y25))))) 
  and le y26 y27 = (fresh (q1 q2) (((y27 === (zero ())) ||| ((y27 === (succ q1)) &&& (y26 === (succ q2)) &&& (le q2 q1))))) 
  and gt y28 y29 = (fresh (q1 q2 q3) ((((y29 === (succ q1)) &&& (y28 === (zero ()))) ||| ((y29 === (succ q2)) &&& (y28 === (succ q3)) &&& (gt q3 q2))))) 
  and __sorto y32 y33 = (fresh (q1 q2 q3 q4 q5 q6) ((((y33 === (List.nil ())) &&& (y32 === (List.nil ()))) ||| ((y32 === (q1 % q2)) &&& (splitoAppendo y33 q1 q2 q3 q4 q5) &&& (__sorto q3 q4) &&& (__sorto q6 q5))))) 
  in         (sorto x0)
