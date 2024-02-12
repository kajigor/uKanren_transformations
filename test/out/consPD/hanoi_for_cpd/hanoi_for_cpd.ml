open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec check y0 = (fresh (q1 q2 q3) (((y0 === (q1 % q2)) &&& (one_step q1 q3) &&& (_check q2 q3)))) 
  and one_step y1 y2 = (fresh (q1 q2) (((y1 === (pair q1 q2)) &&& (((q2 === (thr ())) &&& (q1 === (one ())) &&& (y2 === (triple ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))))) ((List.nil ())) ((Nat.zero % ((List.nil ()))))))) ||| ((q2 === (two ())) &&& (q1 === (one ())) &&& (y2 === (triple ((((Nat.succ Nat.zero)) % ((((Nat.succ ((Nat.succ Nat.zero)))) % ((List.nil ())))))) ((Nat.zero % ((List.nil ())))) ((List.nil ()))))))))) 
  and _check y3 y4 = (fresh (q1 q2 q3 q4) ((((y3 === (List.nil ())) &&& (y4 === (triple ((List.nil ())) ((List.nil ())) q1))) ||| ((y3 === (q2 % q3)) &&& (_one_step y4 q2 q4) &&& (__check q3 q4))))) 
  and _one_step y5 y6 y7 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14) ((((y6 === (pair q1 q2)) &&& (((q2 === (two ())) &&& (q1 === (thr ())) &&& (y5 === (triple q3 ((List.nil ())) ((q4 % q5)))) &&& (set y7 q4 q3 q5)) ||| ((q2 === (one ())) &&& (q1 === (thr ())) &&& (y5 === (triple ((List.nil ())) q6 ((q4 % q5)))) &&& (_set y7 q4 q6 q5)) ||| ((q2 === (thr ())) &&& (q1 === (two ())) &&& (y5 === (triple q3 ((q4 % q5)) ((List.nil ())))) &&& (__set y7 q4 q3 q5)) ||| ((q2 === (one ())) &&& (q1 === (two ())) &&& (y7 === (triple ((q4 % ((List.nil ())))) q5 q7)) &&& (y5 === (triple ((List.nil ())) ((q4 % q5)) q7))) ||| ((q2 === (thr ())) &&& (q1 === (one ())) &&& (y7 === (triple q5 q6 ((q4 % ((List.nil ())))))) &&& (y5 === (triple ((q4 % q5)) q6 ((List.nil ()))))) ||| ((q2 === (two ())) &&& (q1 === (one ())) &&& (y7 === (triple q5 ((q4 % ((List.nil ())))) q7)) &&& (y5 === (triple ((q4 % q5)) ((List.nil ())) q7))))) ||| ((y6 === (pair q1 q2)) &&& (((q2 === (two ())) &&& (q1 === (thr ())) &&& (y5 === (triple q8 ((q9 % q10)) ((q4 % q5)))) &&& (lessSet y7 q4 q8 q9 q10 q5)) ||| ((q2 === (one ())) &&& (q1 === (thr ())) &&& (y5 === (triple ((q9 % q10)) q11 ((q4 % q5)))) &&& (_lessSet y7 q4 q11 q9 q10 q5)) ||| ((q2 === (thr ())) &&& (q1 === (two ())) &&& (y5 === (triple q8 ((q4 % q5)) ((q9 % q10)))) &&& (__lessSet y7 q4 q8 q9 q10 q5)) ||| ((q2 === (one ())) &&& (q1 === (two ())) &&& (y7 === (triple ((q4 % ((q9 % q10)))) q5 q12)) &&& (y5 === (triple ((q9 % q10)) ((q4 % q5)) q12)) &&& (less q4 q9)) ||| ((q2 === (thr ())) &&& (q1 === (one ())) &&& (y7 === (triple q5 q11 ((q4 % ((q9 % q10)))))) &&& (y5 === (triple ((q4 % q5)) q11 ((q9 % q10)))) &&& (less q4 q9)) ||| ((q2 === (two ())) &&& (q1 === (one ())) &&& (((y7 === (triple q5 ((Nat.zero % ((((Nat.succ q13)) % q10)))) q12)) &&& (y5 === (triple ((Nat.zero % q5)) ((((Nat.succ q13)) % q10)) q12))) ||| ((y7 === (triple q5 ((((Nat.succ q14)) % ((((Nat.succ q13)) % q10)))) q12)) &&& (y5 === (triple ((((Nat.succ q14)) % q5)) ((((Nat.succ q13)) % q10)) q12)) &&& (less q14 q13))))))))) 
  and __check y8 y9 = (fresh (q1 q2) ((success ||| ((_one_step q1 q2 y9) &&& (__check y8 y9))))) 
  and set y10 y11 y12 y13 = (y10 === (triple y12 ((y11 % ((List.nil ())))) y13)) 
  and _set y14 y15 y16 y17 = (y14 === (triple ((y15 % ((List.nil ())))) y16 y17)) 
  and __set y18 y19 y20 y21 = (y18 === (triple y20 y21 ((y19 % ((List.nil ())))))) 
  and lessSet y22 y23 y24 y25 y26 y27 = ((y22 === (triple y24 ((y23 % ((y25 % y26)))) y27)) &&& (less y23 y25)) 
  and _lessSet y28 y29 y30 y31 y32 y33 = ((y28 === (triple ((y29 % ((y31 % y32)))) y30 y33)) &&& (less y29 y31)) 
  and __lessSet y34 y35 y36 y37 y38 y39 = ((y34 === (triple y36 y39 ((y35 % ((y37 % y38)))))) &&& (less y35 y37)) 
  and less y40 y41 = (fresh (q1 q2) ((((y41 === (Nat.succ q1)) &&& (y40 === Nat.zero)) ||| ((y41 === (Nat.succ q1)) &&& (y40 === (Nat.succ q2)) &&& (less q2 q1))))) 
  in             (check x0)
