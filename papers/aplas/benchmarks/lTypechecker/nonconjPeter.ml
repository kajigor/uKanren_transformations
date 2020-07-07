open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 = let rec __typecheck_ y4 y5 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20) ((((y4 === iConst_ q1) &&& (y5 === integer ())) ||| (((y4 === bConst_ q2) &&& (y5 === boolean ())) ||| ((((y4 === plus_ q3 q4) &&& (y5 === integer ())) &&& (typecheck_Typecheck_TypeEqTypeEq q3 q4)) ||| ((((y4 === mult_ q5 q6) &&& (y5 === integer ())) &&& (typecheck_Typecheck_TypeEqTypeEq q5 q6)) ||| ((((y4 === equal_ q7 q8) &&& (y5 === boolean ())) &&& ((__typecheck_ q7 q9) &&& ((__typecheck_ q8 q10) &&& (typeEq q9 q10)))) ||| ((((y4 === less_ q11 q12) &&& (y5 === boolean ())) &&& (typecheck_Typecheck_TypeEqTypeEq q11 q12)) ||| (((y4 === if_ q13 q14 q15) &&& ((__typecheck_ q13 q16) &&& ((_typeEq q16) &&& ((__typecheck_ q14 y5) &&& ((__typecheck_ q15 q17) &&& (typeEq y5 q17)))))) ||| ((y4 === let_ q18 q19) &&& (__typecheck_ q18 q20))))))))))) and ___typecheck_ y6 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14) (((y6 === iConst_ q1) ||| (((y6 === plus_ q2 q3) &&& ((___typecheck_ q2) &&& ((__typecheck_ q3 q4) &&& (____typeEq q4)))) ||| (((y6 === mult_ q5 q6) &&& (typecheck_Typecheck_TypeEqTypeEq q5 q6)) ||| (((y6 === if_ q7 q8 q9) &&& ((__typecheck_ q7 q10) &&& ((_typeEq q10) &&& ((___typecheck_ q8) &&& ((__typecheck_ q9 q11) &&& (_______typeEq q11)))))) ||| ((y6 === let_ q12 q13) &&& ((__typecheck_ q12 q14) &&& (_____typecheck_ ((Std.(%) (q14) ((Std.List.nil ())))) q13 ((Option.some (integer ())))))))))))) and typecheck_Typecheck_TypeEqTypeEq y7 y8 = (fresh (q1) (((___typecheck_ y7) &&& ((__typecheck_ y8 q1) &&& (____typeEq q1))))) and typeEq y11 y12 = (((y11 === integer ()) &&& (y12 === integer ())) ||| ((y11 === boolean ()) &&& (y12 === boolean ()))) and _typeEq y13 = (y13 === boolean ()) and nthOpt y16 = (fresh (q1) (((y16 === Std.Nat.zero) ||| ((y16 === (Std.Nat.succ (q1))) &&& (nthOpt ((Std.Nat.succ (q1)))))))) and _nthOpt y17 y18 = (fresh (q1 q2 q3) (((y17 === (Std.List.nil ())) ||| (((y17 === (Std.(%) (q1) (q2))) &&& (y18 === (Std.Nat.succ (q3)))) &&& (_nthOpt q2 ((Std.Nat.succ (q3)))))))) and ____typecheck_ y19 = (fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19) ((((y19 === (Std.List.nil ())) ||| ((y19 === (Std.(%) (q1) (q2))) &&& (_nthOpt q2 ((Std.Nat.succ (q3)))))) ||| ((____typecheck_ y19) ||| (((_____typecheck_ y19 q4 ((Option.some (q5)))) &&& (____typecheck_ y19)) ||| (((_____typecheck_ y19 q4 ((Option.some (q5)))) &&& ((_____typecheck_ y19 q6 ((Option.some (q7)))) &&& ((__typeEq q5) &&& (___typeEq q7)))) ||| (((_____typecheck_ y19 q4 ((Option.some (q5)))) &&& ((_____typecheck_ y19 q6 ((Option.some (q7)))) &&& ((____typeEq q5) &&& (__typeEq q7)))) ||| ((____typecheck_ y19) ||| ((typecheck_Typecheck_ y19) ||| ((_typecheck_Typecheck_TypeEqTypeEq y19) ||| ((__typecheck_Typecheck_TypeEqTypeEq y19) ||| ((____typecheck_ y19) ||| ((typecheck_Typecheck_ y19) ||| (((_____typecheck_ y19 q8 ((Option.some (q9)))) &&& ((_____typecheck_ y19 q10 ((Option.some (q11)))) &&& (_____typeEq q9 q11))) ||| ((____typecheck_ y19) ||| ((typecheck_Typecheck_ y19) ||| ((_typecheck_Typecheck_TypeEqTypeEq y19) ||| ((__typecheck_Typecheck_TypeEqTypeEq y19) ||| ((____typecheck_ y19) ||| (((_____typecheck_ y19 q12 ((Option.some (q13)))) &&& ((_typeEq q13) &&& (____typecheck_ y19))) ||| (((_____typecheck_ y19 q12 ((Option.some (q13)))) &&& ((_typeEq q13) &&& ((_____typecheck_ y19 q14 ((Option.some (q15)))) &&& (____typecheck_ y19)))) ||| (((_____typecheck_ y19 q12 ((Option.some (q13)))) &&& ((_typeEq q13) &&& ((_____typecheck_ y19 q14 ((Option.some (q15)))) &&& ((_____typecheck_ y19 q16 ((Option.some (q17)))) &&& (_____typeEq q15 q17))))) ||| (((_____typecheck_ y19 q12 ((Option.some (q13)))) &&& (______typeEq q13)) ||| ((____typecheck_ y19) ||| ((_____typecheck_ y19 q18 ((Option.some (q19)))) &&& (____typecheck_ ((Std.(%) (q19) (y19))))))))))))))))))))))))))))) and _____typecheck_ y21 y22 y23 = success and __typeEq y24 = (y24 === boolean ()) and ___typeEq y25 = ((y25 === integer ()) ||| (y25 === boolean ())) and ____typeEq y27 = (y27 === integer ()) and typecheck_Typecheck_ y28 = (fresh (q1 q2) (((_____typecheck_ y28 q1 ((Option.some (q2)))) &&& (____typecheck_ y28)))) and _typecheck_Typecheck_TypeEqTypeEq y32 = (fresh (q1 q2 q3 q4) (((_____typecheck_ y32 q1 ((Option.some (q2)))) &&& ((_____typecheck_ y32 q3 ((Option.some (q4)))) &&& ((__typeEq q2) &&& (___typeEq q4)))))) and __typecheck_Typecheck_TypeEqTypeEq y38 = (fresh (q1 q2 q3 q4) (((_____typecheck_ y38 q1 ((Option.some (q2)))) &&& ((_____typecheck_ y38 q3 ((Option.some (q4)))) &&& ((____typeEq q2) &&& (__typeEq q4)))))) and _____typeEq y43 y44 = (((y43 === integer ()) &&& (y44 === boolean ())) ||| ((y43 === boolean ()) &&& (y44 === integer ()))) and ______typeEq y45 = (y45 === integer ()) and _______typeEq y76 = (y76 === integer ()) in                   (___typecheck_ x0)