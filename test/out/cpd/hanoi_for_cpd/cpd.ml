open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec check y0 = (fresh (q1 q2) (((y0 === (q1 % q2)) &&& (one_stepCheck q1 q2)))) 
  and one_stepCheck y1 y2 = (fresh (q1) (((y1 === (pair ((one ())) q1)) &&& (notEqStickGetSetCheck y2 q1)))) 
  and notEqStickGetSetCheck y4 y6 = (((y6 === (two ())) &&& (_check y4)) ||| ((y6 === (thr ())) &&& (___check y4))) 
  and _check y7 = (fresh (q1 q2) (((y7 === (q1 % q2)) &&& (_one_stepCheck q1 q2)))) 
  and _one_stepCheck y8 y9 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8) ((((y8 === (pair q1 ((thr ())))) &&& (_notEqStickGetSetCheck y9 q1)) ||| ((y8 === (pair q1 q2)) &&& (_notEqStickGetGetLessSetSetCheck y9 q3 q1 q2 q4 q5 q6 q7 q8))))) 
  and _notEqStickGetSetCheck y11 y12 = (((y12 === (one ())) &&& (__check y11)) ||| ((y12 === (two ())) &&& (___check y11))) 
  and __check y18 = (fresh (q1 q2) (((y18 === (q1 % q2)) &&& (__one_stepCheck q1 q2)))) 
  and __one_stepCheck y19 y20 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8) (((y19 === (pair q1 q2)) &&& (notEqStickGetGetLessSetSetCheck y20 q3 q1 q2 q4 q5 q6 q7 q8)))) 
  and ___check y22 = (fresh (q1 q2) (((y22 === (q1 % q2)) &&& (___one_stepCheck q1 q2)))) 
  and ___one_stepCheck y23 y24 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8) ((((y23 === (pair q1 ((two ())))) &&& (__notEqStickGetSetCheck y24 q1)) ||| ((y23 === (pair q1 q2)) &&& (___notEqStickGetGetLessSetSetCheck y24 q3 q1 q2 q4 q5 q6 q7 q8))))) 
  and __notEqStickGetSetCheck y26 y27 = (((y27 === (one ())) &&& (____check y26)) ||| ((y27 === (thr ())) &&& (_check y26))) 
  and ____check y33 = (fresh (q1 q2) (((y33 === (q1 % q2)) &&& (____one_stepCheck q1 q2)))) 
  and ____one_stepCheck y34 y35 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8) (((y34 === (pair q1 q2)) &&& (__notEqStickGetGetLessSetSetCheck y35 q3 q1 q2 q4 q5 q6 q7 q8)))) 
  in              (check x0)
