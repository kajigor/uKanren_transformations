__typecheck_(iConst_(Q1), integer).
__typecheck_(bConst_(Q2), boolean).
__typecheck_(plus_(Q3, Q4), integer) :- typecheck_Typecheck_TypeEqTypeEq(Q3, Q4).
__typecheck_(mult_(Q5, Q6), integer) :- typecheck_Typecheck_TypeEqTypeEq(Q5, Q6).
__typecheck_(equal_(Q7, Q8), boolean) :- __typecheck_(Q7, Q9), __typecheck_(Q8, Q10), typeEq(Q9, Q10).
__typecheck_(less_(Q11, Q12), boolean) :- typecheck_Typecheck_TypeEqTypeEq(Q11, Q12).
__typecheck_(if_(Q13, Q14, Q15), Y5) :- __typecheck_(Q13, Q16), _typeEq(Q16), __typecheck_(Q14, Y5), __typecheck_(Q15, Q17), typeEq(Y5, Q17).
__typecheck_(let_(Q18, Q19), Y5) :- __typecheck_(Q18, Q20).
___typecheck_(iConst_(Q1)).
___typecheck_(plus_(Q2, Q3)) :- ___typecheck_(Q2), __typecheck_(Q3, Q4), ____typeEq(Q4).
___typecheck_(mult_(Q5, Q6)) :- typecheck_Typecheck_TypeEqTypeEq(Q5, Q6).
___typecheck_(if_(Q7, Q8, Q9)) :- __typecheck_(Q7, Q10), _typeEq(Q10), ___typecheck_(Q8), __typecheck_(Q9, Q11), _______typeEq(Q11).
___typecheck_(let_(Q12, Q13)) :- __typecheck_(Q12, Q14), _____typecheck_(cons(Q14, nil), Q13, some(integer)).
typecheck_Typecheck_TypeEqTypeEq(Y7, Y8) :- ___typecheck_(Y7), __typecheck_(Y8, Q1), ____typeEq(Q1).
typeEq(integer, integer).
typeEq(boolean, boolean).
_typeEq(boolean).
nthOpt(o).
nthOpt(s(Q1)) :- nthOpt(s(Q1)).
_nthOpt(nil, Y18).
_nthOpt(cons(Q1, Q2), s(Q3)) :- _nthOpt(Q2, s(Q3)).
____typecheck_(nil).
____typecheck_(cons(Q1, Q2)) :- _nthOpt(Q2, s(Q3)).
____typecheck_(Y19) :- ____typecheck_(Y19).
____typecheck_(Y19) :- _____typecheck_(Y19, Q4, some(Q5)), ____typecheck_(Y19).
____typecheck_(Y19) :- _____typecheck_(Y19, Q4, some(Q5)), _____typecheck_(Y19, Q6, some(Q7)), __typeEq(Q5), ___typeEq(Q7).
____typecheck_(Y19) :- _____typecheck_(Y19, Q4, some(Q5)), _____typecheck_(Y19, Q6, some(Q7)), ____typeEq(Q5), __typeEq(Q7).
____typecheck_(Y19) :- ____typecheck_(Y19).
____typecheck_(Y19) :- typecheck_Typecheck_(Y19).
____typecheck_(Y19) :- _typecheck_Typecheck_TypeEqTypeEq(Y19).
____typecheck_(Y19) :- __typecheck_Typecheck_TypeEqTypeEq(Y19).
____typecheck_(Y19) :- ____typecheck_(Y19).
____typecheck_(Y19) :- typecheck_Typecheck_(Y19).
____typecheck_(Y19) :- _____typecheck_(Y19, Q8, some(Q9)), _____typecheck_(Y19, Q10, some(Q11)), _____typeEq(Q9, Q11).
____typecheck_(Y19) :- ____typecheck_(Y19).
____typecheck_(Y19) :- typecheck_Typecheck_(Y19).
____typecheck_(Y19) :- _typecheck_Typecheck_TypeEqTypeEq(Y19).
____typecheck_(Y19) :- __typecheck_Typecheck_TypeEqTypeEq(Y19).
____typecheck_(Y19) :- ____typecheck_(Y19).
____typecheck_(Y19) :- _____typecheck_(Y19, Q12, some(Q13)), _typeEq(Q13), ____typecheck_(Y19).
____typecheck_(Y19) :- _____typecheck_(Y19, Q12, some(Q13)), _typeEq(Q13), _____typecheck_(Y19, Q14, some(Q15)), ____typecheck_(Y19).
____typecheck_(Y19) :- _____typecheck_(Y19, Q12, some(Q13)), _typeEq(Q13), _____typecheck_(Y19, Q14, some(Q15)), _____typecheck_(Y19, Q16, some(Q17)), _____typeEq(Q15, Q17).
____typecheck_(Y19) :- _____typecheck_(Y19, Q12, some(Q13)), ______typeEq(Q13).
____typecheck_(Y19) :- ____typecheck_(Y19).
____typecheck_(Y19) :- _____typecheck_(Y19, Q18, some(Q19)), ____typecheck_(cons(Q19, Y19)).
_____typecheck_(Y21, Y22, Y23) :- success().
__typeEq(boolean).
___typeEq(integer).
___typeEq(boolean).
____typeEq(integer).
typecheck_Typecheck_(Y28) :- _____typecheck_(Y28, Q1, some(Q2)), ____typecheck_(Y28).
_typecheck_Typecheck_TypeEqTypeEq(Y32) :- _____typecheck_(Y32, Q1, some(Q2)), _____typecheck_(Y32, Q3, some(Q4)), __typeEq(Q2), ___typeEq(Q4).
__typecheck_Typecheck_TypeEqTypeEq(Y38) :- _____typecheck_(Y38, Q1, some(Q2)), _____typecheck_(Y38, Q3, some(Q4)), ____typeEq(Q2), __typeEq(Q4).
_____typeEq(integer, boolean).
_____typeEq(boolean, integer).
______typeEq(integer).
_______typeEq(integer).