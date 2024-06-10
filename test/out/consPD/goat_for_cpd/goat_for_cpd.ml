open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = 
  let rec _eval y1 = (fresh (q1 q2 q3 q4 q5) (((y1 === (List.nil ())) ||| ((y1 === (q1 % q2)) &&& (((q1 === (empty ())) &&& (q2 === (((empty ())) % q3)) &&& (_eval q3)) ||| ((q1 === (goat ())) &&& (q2 === (q4 % q5)) &&& (stepEval q4 q5))))))) 
  and stepEval y2 y3 = (fresh (q1 q2 q3 q4 q5) ((((y2 === (goat ())) &&& ((y3 === (List.nil ())) ||| ((y3 === (q1 % q2)) &&& (((q1 === (empty ())) &&& (q2 === (((empty ())) % q3)) &&& (_eval q3)) ||| ((q1 === (goat ())) &&& (q2 === (q4 % q5)) &&& (stepEval q4 q5)))))) ||| ((y2 === (empty ())) &&& (__eval y3))))) 
  and __eval y5 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12) (((y5 === (q1 % q2)) &&& (((q1 === (empty ())) &&& (((q2 === (((goat ())) % q3)) &&& ((q3 === (List.nil ())) ||| ((q3 === (q4 % q5)) &&& (((q4 === (empty ())) &&& (q5 === (((empty ())) % q6)) &&& (_eval q6)) ||| ((q4 === (goat ())) &&& (q5 === (q7 % q8)) &&& (stepEval q7 q8)))))) ||| ((q2 === (((empty ())) % q3)) &&& (__eval q3)))) ||| ((q1 === (wolf ())) &&& (swapEval q2)) ||| ((q1 === (cabbage ())) &&& (((q2 === (((cabbage ())) % q9)) &&& (__eval q9)) ||| ((q2 === (((goat ())) % q9)) &&& (((q9 === (((goat ())) % ((q10 % q11)))) &&& (_stepEval q10 q11)) ||| ((q9 === (((wolf ())) % q12)) &&& (_swapEval q12)))))))))) 
  and swapEval y6 = (fresh (q1 q2) (((y6 === (q1 % q2)) &&& (___stepEval q1 q2)))) 
  and _stepEval y8 y9 = (fresh (q1 q2 q3) ((((y8 === (cabbage ())) &&& (__eval y9)) ||| ((y8 === (goat ())) &&& (((y9 === (((goat ())) % ((q1 % q2)))) &&& (_stepEval q1 q2)) ||| ((y9 === (((wolf ())) % q3)) &&& (_swapEval q3))))))) 
  and _swapEval y11 = (fresh (q1 q2) (((y11 === (q1 % q2)) &&& (__stepEval q1 q2)))) 
  and ___eval y13 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16) (((y13 === (q1 % q2)) &&& (((q1 === (goat ())) &&& (((q2 === (((wolf ())) % q3)) &&& (((q3 === (((empty ())) % q4)) &&& (((q4 === (((goat ())) % q5)) &&& ((q5 === (List.nil ())) ||| ((q5 === (q6 % q7)) &&& (((q6 === (empty ())) &&& (q7 === (((empty ())) % q8)) &&& (_eval q8)) ||| ((q6 === (goat ())) &&& (q7 === (q9 % q10)) &&& (stepEval q9 q10)))))) ||| ((q4 === (((empty ())) % q5)) &&& (__eval q5)))) ||| ((q3 === (((wolf ())) % q4)) &&& (swapEval q4)) ||| ((q3 === (((cabbage ())) % q4)) &&& (((q4 === (((cabbage ())) % q11)) &&& (__eval q11)) ||| ((q4 === (((goat ())) % q11)) &&& (((q11 === (((goat ())) % ((q12 % q13)))) &&& (_stepEval q12 q13)) ||| ((q11 === (((wolf ())) % q14)) &&& (_swapEval q14)))))))) ||| ((q2 === (((goat ())) % q3)) &&& (___eval q3)))) ||| ((q1 === (cabbage ())) &&& (q2 === (q15 % q16)) &&& (__stepEval q15 q16)))))) 
  and __stepEval y14 y15 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26) ((((y14 === (cabbage ())) &&& (((y15 === (((goat ())) % q1)) &&& (((q1 === (((wolf ())) % q2)) &&& (((q2 === (((empty ())) % q3)) &&& (((q3 === (((goat ())) % q4)) &&& ((q4 === (List.nil ())) ||| ((q4 === (q5 % q6)) &&& (((q5 === (empty ())) &&& (q6 === (((empty ())) % q7)) &&& (_eval q7)) ||| ((q5 === (goat ())) &&& (q6 === (q8 % q9)) &&& (stepEval q8 q9)))))) ||| ((q3 === (((empty ())) % q4)) &&& (__eval q4)))) ||| ((q2 === (((wolf ())) % q3)) &&& (swapEval q3)) ||| ((q2 === (((cabbage ())) % q3)) &&& (((q3 === (((cabbage ())) % q10)) &&& (__eval q10)) ||| ((q3 === (((goat ())) % q10)) &&& (((q10 === (((goat ())) % ((q11 % q12)))) &&& (_stepEval q11 q12)) ||| ((q10 === (((wolf ())) % q13)) &&& (_swapEval q13)))))))) ||| ((q1 === (((goat ())) % q2)) &&& (___eval q2)))) ||| ((y15 === (((cabbage ())) % ((q14 % q15)))) &&& (__stepEval q14 q15)))) ||| ((y14 === (wolf ())) &&& (((y15 === (((empty ())) % q16)) &&& (____eval q16)) ||| ((y15 === (((wolf ())) % q16)) &&& (((q16 === (((cabbage ())) % q17)) &&& (((q17 === (((empty ())) % q18)) &&& (((q18 === (((wolf ())) % q19)) &&& (((q19 === (((empty ())) % ((((empty ())) % q20)))) &&& (_____eval q20)) ||| ((q19 === (((wolf ())) % ((q21 % q22)))) &&& (___stepEval q21 q22)) ||| ((q19 === (((cabbage ())) % q23)) &&& (((q23 === (((cabbage ())) % q24)) &&& (_____eval q24)) ||| ((q23 === (((empty ())) % q24)) &&& (______eval q24)))))) ||| ((q18 === (((empty ())) % q19)) &&& (_______eval q19)))) ||| ((q17 === (((cabbage ())) % q18)) &&& (__swapEval q18)))) ||| ((q16 === (((wolf ())) % q17)) &&& (______eval q17)) ||| ((q16 === (((empty ())) % ((((empty ())) % ((q25 % q26)))))) &&& (____stepEval q25 q26)))))) ||| ((y14 === (empty ())) &&& (________eval y15))))) 
  and ____eval y17 = (fresh (q1 q2) (((y17 === (q1 % q2)) &&& (((q1 === (cabbage ())) &&& (_____eval q2)) ||| ((q1 === (empty ())) &&& (______eval q2)))))) 
  and _____eval y18 = (fresh (q1 q2 q3 q4 q5 q6) (((y18 === (q1 % q2)) &&& (((q1 === (empty ())) &&& (q2 === (((empty ())) % q3)) &&& (_____eval q3)) ||| ((q1 === (wolf ())) &&& (q2 === (q4 % q5)) &&& (___stepEval q4 q5)) ||| ((q1 === (cabbage ())) &&& (((q2 === (((cabbage ())) % q6)) &&& (_____eval q6)) ||| ((q2 === (((empty ())) % q6)) &&& (______eval q6)))))))) 
  and ___stepEval y19 y20 = (fresh (q1 q2 q3 q4 q5) ((((y19 === (wolf ())) &&& (((y20 === (((empty ())) % ((((empty ())) % q1)))) &&& (_____eval q1)) ||| ((y20 === (((wolf ())) % ((q2 % q3)))) &&& (___stepEval q2 q3)) ||| ((y20 === (((cabbage ())) % q4)) &&& (((q4 === (((cabbage ())) % q5)) &&& (_____eval q5)) ||| ((q4 === (((empty ())) % q5)) &&& (______eval q5)))))) ||| ((y19 === (empty ())) &&& (_______eval y20))))) 
  and ______eval y22 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12) (((y22 === (q1 % q2)) &&& (((q1 === (empty ())) &&& (____eval q2)) ||| ((q1 === (wolf ())) &&& (((q2 === (((cabbage ())) % q3)) &&& (((q3 === (((empty ())) % q4)) &&& (((q4 === (((wolf ())) % q5)) &&& (((q5 === (((empty ())) % ((((empty ())) % q6)))) &&& (_____eval q6)) ||| ((q5 === (((wolf ())) % ((q7 % q8)))) &&& (___stepEval q7 q8)) ||| ((q5 === (((cabbage ())) % q9)) &&& (((q9 === (((cabbage ())) % q10)) &&& (_____eval q10)) ||| ((q9 === (((empty ())) % q10)) &&& (______eval q10)))))) ||| ((q4 === (((empty ())) % q5)) &&& (_______eval q5)))) ||| ((q3 === (((cabbage ())) % q4)) &&& (__swapEval q4)))) ||| ((q2 === (((wolf ())) % q3)) &&& (______eval q3)) ||| ((q2 === (((empty ())) % ((((empty ())) % ((q11 % q12)))))) &&& (____stepEval q11 q12)))))))) 
  and _______eval y23 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8) (((y23 === (q1 % q2)) &&& (((q1 === (empty ())) &&& (((q2 === (((wolf ())) % q3)) &&& (((q3 === (((empty ())) % ((((empty ())) % q4)))) &&& (_____eval q4)) ||| ((q3 === (((wolf ())) % ((q5 % q6)))) &&& (___stepEval q5 q6)) ||| ((q3 === (((cabbage ())) % q7)) &&& (((q7 === (((cabbage ())) % q8)) &&& (_____eval q8)) ||| ((q7 === (((empty ())) % q8)) &&& (______eval q8)))))) ||| ((q2 === (((empty ())) % q3)) &&& (_______eval q3)))) ||| ((q1 === (cabbage ())) &&& (__swapEval q2)))))) 
  and __swapEval y24 = (fresh (q1 q2) (((y24 === (q1 % q2)) &&& (____stepEval q1 q2)))) 
  and ____stepEval y26 y27 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9) ((((y26 === (cabbage ())) &&& (((y27 === (((empty ())) % q1)) &&& (((q1 === (((wolf ())) % q2)) &&& (((q2 === (((empty ())) % ((((empty ())) % q3)))) &&& (_____eval q3)) ||| ((q2 === (((wolf ())) % ((q4 % q5)))) &&& (___stepEval q4 q5)) ||| ((q2 === (((cabbage ())) % q6)) &&& (((q6 === (((cabbage ())) % q7)) &&& (_____eval q7)) ||| ((q6 === (((empty ())) % q7)) &&& (______eval q7)))))) ||| ((q1 === (((empty ())) % q2)) &&& (_______eval q2)))) ||| ((y27 === (((cabbage ())) % q1)) &&& (__swapEval q1)))) ||| ((y26 === (wolf ())) &&& (______eval y27)) ||| ((y26 === (empty ())) &&& (y27 === (((empty ())) % ((q8 % q9)))) &&& (____stepEval q8 q9))))) 
  and ________eval y29 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31) (((y29 === (q1 % q2)) &&& (((q1 === (empty ())) &&& (((q2 === (((cabbage ())) % q3)) &&& (((q3 === (((goat ())) % q4)) &&& (((q4 === (((wolf ())) % q5)) &&& (((q5 === (((empty ())) % q6)) &&& (((q6 === (((goat ())) % q7)) &&& ((q7 === (List.nil ())) ||| ((q7 === (q8 % q9)) &&& (((q8 === (empty ())) &&& (q9 === (((empty ())) % q10)) &&& (_eval q10)) ||| ((q8 === (goat ())) &&& (q9 === (q11 % q12)) &&& (stepEval q11 q12)))))) ||| ((q6 === (((empty ())) % q7)) &&& (__eval q7)))) ||| ((q5 === (((wolf ())) % q6)) &&& (swapEval q6)) ||| ((q5 === (((cabbage ())) % q6)) &&& (((q6 === (((cabbage ())) % q13)) &&& (__eval q13)) ||| ((q6 === (((goat ())) % q13)) &&& (((q13 === (((goat ())) % ((q14 % q15)))) &&& (_stepEval q14 q15)) ||| ((q13 === (((wolf ())) % q16)) &&& (_swapEval q16)))))))) ||| ((q4 === (((goat ())) % q5)) &&& (___eval q5)))) ||| ((q3 === (((cabbage ())) % ((q17 % q18)))) &&& (__stepEval q17 q18)))) ||| ((q2 === (((wolf ())) % q3)) &&& (((q3 === (((empty ())) % q19)) &&& (____eval q19)) ||| ((q3 === (((wolf ())) % q19)) &&& (((q19 === (((cabbage ())) % q20)) &&& (((q20 === (((empty ())) % q21)) &&& (((q21 === (((wolf ())) % q22)) &&& (((q22 === (((empty ())) % ((((empty ())) % q23)))) &&& (_____eval q23)) ||| ((q22 === (((wolf ())) % ((q24 % q25)))) &&& (___stepEval q24 q25)) ||| ((q22 === (((cabbage ())) % q26)) &&& (((q26 === (((cabbage ())) % q27)) &&& (_____eval q27)) ||| ((q26 === (((empty ())) % q27)) &&& (______eval q27)))))) ||| ((q21 === (((empty ())) % q22)) &&& (_______eval q22)))) ||| ((q20 === (((cabbage ())) % q21)) &&& (__swapEval q21)))) ||| ((q19 === (((wolf ())) % q20)) &&& (______eval q20)) ||| ((q19 === (((empty ())) % ((((empty ())) % ((q28 % q29)))))) &&& (____stepEval q28 q29)))))) ||| ((q2 === (((empty ())) % q3)) &&& (________eval q3)))) ||| ((q1 === (goat ())) &&& (q2 === (q30 % q31)) &&& (_____stepEval q30 q31)))))) 
  and _____stepEval y30 y31 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31) ((((y30 === (goat ())) &&& (((y31 === (((empty ())) % q1)) &&& (((q1 === (((cabbage ())) % q2)) &&& (((q2 === (((goat ())) % q3)) &&& (((q3 === (((wolf ())) % q4)) &&& (((q4 === (((empty ())) % q5)) &&& (((q5 === (((goat ())) % q6)) &&& ((q6 === (List.nil ())) ||| ((q6 === (q7 % q8)) &&& (((q7 === (empty ())) &&& (q8 === (((empty ())) % q9)) &&& (_eval q9)) ||| ((q7 === (goat ())) &&& (q8 === (q10 % q11)) &&& (stepEval q10 q11)))))) ||| ((q5 === (((empty ())) % q6)) &&& (__eval q6)))) ||| ((q4 === (((wolf ())) % q5)) &&& (swapEval q5)) ||| ((q4 === (((cabbage ())) % q5)) &&& (((q5 === (((cabbage ())) % q12)) &&& (__eval q12)) ||| ((q5 === (((goat ())) % q12)) &&& (((q12 === (((goat ())) % ((q13 % q14)))) &&& (_stepEval q13 q14)) ||| ((q12 === (((wolf ())) % q15)) &&& (_swapEval q15)))))))) ||| ((q3 === (((goat ())) % q4)) &&& (___eval q4)))) ||| ((q2 === (((cabbage ())) % ((q16 % q17)))) &&& (__stepEval q16 q17)))) ||| ((q1 === (((wolf ())) % q2)) &&& (((q2 === (((empty ())) % q18)) &&& (____eval q18)) ||| ((q2 === (((wolf ())) % q18)) &&& (((q18 === (((cabbage ())) % q19)) &&& (((q19 === (((empty ())) % q20)) &&& (((q20 === (((wolf ())) % q21)) &&& (((q21 === (((empty ())) % ((((empty ())) % q22)))) &&& (_____eval q22)) ||| ((q21 === (((wolf ())) % ((q23 % q24)))) &&& (___stepEval q23 q24)) ||| ((q21 === (((cabbage ())) % q25)) &&& (((q25 === (((cabbage ())) % q26)) &&& (_____eval q26)) ||| ((q25 === (((empty ())) % q26)) &&& (______eval q26)))))) ||| ((q20 === (((empty ())) % q21)) &&& (_______eval q21)))) ||| ((q19 === (((cabbage ())) % q20)) &&& (__swapEval q20)))) ||| ((q18 === (((wolf ())) % q19)) &&& (______eval q19)) ||| ((q18 === (((empty ())) % ((((empty ())) % ((q27 % q28)))))) &&& (____stepEval q27 q28)))))) ||| ((q1 === (((empty ())) % q2)) &&& (________eval q2)))) ||| ((y31 === (((goat ())) % ((q29 % q30)))) &&& (_____stepEval q29 q30)))) ||| ((y30 === (empty ())) &&& (y31 === (((empty ())) % q31)) &&& (_________eval q31))))) 
  and _________eval y33 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33) (((y33 === (q1 % q2)) &&& (((q1 === (goat ())) &&& (((q2 === (((empty ())) % q3)) &&& (((q3 === (((cabbage ())) % q4)) &&& (((q4 === (((goat ())) % q5)) &&& (((q5 === (((wolf ())) % q6)) &&& (((q6 === (((empty ())) % q7)) &&& (((q7 === (((goat ())) % q8)) &&& ((q8 === (List.nil ())) ||| ((q8 === (q9 % q10)) &&& (((q9 === (empty ())) &&& (q10 === (((empty ())) % q11)) &&& (_eval q11)) ||| ((q9 === (goat ())) &&& (q10 === (q12 % q13)) &&& (stepEval q12 q13)))))) ||| ((q7 === (((empty ())) % q8)) &&& (__eval q8)))) ||| ((q6 === (((wolf ())) % q7)) &&& (swapEval q7)) ||| ((q6 === (((cabbage ())) % q7)) &&& (((q7 === (((cabbage ())) % q14)) &&& (__eval q14)) ||| ((q7 === (((goat ())) % q14)) &&& (((q14 === (((goat ())) % ((q15 % q16)))) &&& (_stepEval q15 q16)) ||| ((q14 === (((wolf ())) % q17)) &&& (_swapEval q17)))))))) ||| ((q5 === (((goat ())) % q6)) &&& (___eval q6)))) ||| ((q4 === (((cabbage ())) % ((q18 % q19)))) &&& (__stepEval q18 q19)))) ||| ((q3 === (((wolf ())) % q4)) &&& (((q4 === (((empty ())) % q20)) &&& (____eval q20)) ||| ((q4 === (((wolf ())) % q20)) &&& (((q20 === (((cabbage ())) % q21)) &&& (((q21 === (((empty ())) % q22)) &&& (((q22 === (((wolf ())) % q23)) &&& (((q23 === (((empty ())) % ((((empty ())) % q24)))) &&& (_____eval q24)) ||| ((q23 === (((wolf ())) % ((q25 % q26)))) &&& (___stepEval q25 q26)) ||| ((q23 === (((cabbage ())) % q27)) &&& (((q27 === (((cabbage ())) % q28)) &&& (_____eval q28)) ||| ((q27 === (((empty ())) % q28)) &&& (______eval q28)))))) ||| ((q22 === (((empty ())) % q23)) &&& (_______eval q23)))) ||| ((q21 === (((cabbage ())) % q22)) &&& (__swapEval q22)))) ||| ((q20 === (((wolf ())) % q21)) &&& (______eval q21)) ||| ((q20 === (((empty ())) % ((((empty ())) % ((q29 % q30)))))) &&& (____stepEval q29 q30)))))) ||| ((q3 === (((empty ())) % q4)) &&& (________eval q4)))) ||| ((q2 === (((goat ())) % ((q31 % q32)))) &&& (_____stepEval q31 q32)))) ||| ((q1 === (empty ())) &&& (q2 === (((empty ())) % q33)) &&& (_________eval q33)))))) 
  in                   (_________eval x0)