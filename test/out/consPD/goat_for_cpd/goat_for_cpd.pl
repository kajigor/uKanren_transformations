_eval(nil).
_eval(cons(empty, cons(empty, Q3))) :- _eval(Q3).
_eval(cons(goat, cons(Q4, Q5))) :- stepEval(Q4, Q5).
stepEval(goat, nil).
stepEval(goat, cons(empty, cons(empty, Q3))) :- _eval(Q3).
stepEval(goat, cons(goat, cons(Q4, Q5))) :- stepEval(Q4, Q5).
stepEval(empty, Y3) :- __eval(Y3).
__eval(cons(empty, cons(goat, nil))).
__eval(cons(empty, cons(goat, cons(empty, cons(empty, Q6))))) :- _eval(Q6).
__eval(cons(empty, cons(goat, cons(goat, cons(Q7, Q8))))) :- stepEval(Q7, Q8).
__eval(cons(empty, cons(empty, Q3))) :- __eval(Q3).
__eval(cons(wolf, Q2)) :- swapEval(Q2).
__eval(cons(cabbage, cons(cabbage, Q9))) :- __eval(Q9).
__eval(cons(cabbage, cons(goat, cons(goat, cons(Q10, Q11))))) :- _stepEval(Q10, Q11).
__eval(cons(cabbage, cons(goat, cons(wolf, Q12)))) :- _swapEval(Q12).
swapEval(cons(Q1, Q2)) :- ___stepEval(Q1, Q2).
_stepEval(cabbage, Y9) :- __eval(Y9).
_stepEval(goat, cons(goat, cons(Q1, Q2))) :- _stepEval(Q1, Q2).
_stepEval(goat, cons(wolf, Q3)) :- _swapEval(Q3).
_swapEval(cons(Q1, Q2)) :- __stepEval(Q1, Q2).
___eval(cons(goat, cons(wolf, cons(empty, cons(goat, nil))))).
___eval(cons(goat, cons(wolf, cons(empty, cons(goat, cons(empty, cons(empty, Q8))))))) :- _eval(Q8).
___eval(cons(goat, cons(wolf, cons(empty, cons(goat, cons(goat, cons(Q9, Q10))))))) :- stepEval(Q9, Q10).
___eval(cons(goat, cons(wolf, cons(empty, cons(empty, Q5))))) :- __eval(Q5).
___eval(cons(goat, cons(wolf, cons(wolf, Q4)))) :- swapEval(Q4).
___eval(cons(goat, cons(wolf, cons(cabbage, cons(cabbage, Q11))))) :- __eval(Q11).
___eval(cons(goat, cons(wolf, cons(cabbage, cons(goat, cons(goat, cons(Q12, Q13))))))) :- _stepEval(Q12, Q13).
___eval(cons(goat, cons(wolf, cons(cabbage, cons(goat, cons(wolf, Q14)))))) :- _swapEval(Q14).
___eval(cons(goat, cons(goat, Q3))) :- ___eval(Q3).
___eval(cons(cabbage, cons(Q15, Q16))) :- __stepEval(Q15, Q16).
__stepEval(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, nil))))).
__stepEval(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, cons(empty, cons(empty, Q7))))))) :- _eval(Q7).
__stepEval(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, cons(goat, cons(Q8, Q9))))))) :- stepEval(Q8, Q9).
__stepEval(cabbage, cons(goat, cons(wolf, cons(empty, cons(empty, Q4))))) :- __eval(Q4).
__stepEval(cabbage, cons(goat, cons(wolf, cons(wolf, Q3)))) :- swapEval(Q3).
__stepEval(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(cabbage, Q10))))) :- __eval(Q10).
__stepEval(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(goat, cons(goat, cons(Q11, Q12))))))) :- _stepEval(Q11, Q12).
__stepEval(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(goat, cons(wolf, Q13)))))) :- _swapEval(Q13).
__stepEval(cabbage, cons(goat, cons(goat, Q2))) :- ___eval(Q2).
__stepEval(cabbage, cons(cabbage, cons(Q14, Q15))) :- __stepEval(Q14, Q15).
__stepEval(wolf, cons(empty, Q16)) :- ____eval(Q16).
__stepEval(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(empty, cons(empty, Q20))))))) :- _____eval(Q20).
__stepEval(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(wolf, cons(Q21, Q22))))))) :- ___stepEval(Q21, Q22).
__stepEval(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(cabbage, Q24))))))) :- _____eval(Q24).
__stepEval(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(empty, Q24))))))) :- ______eval(Q24).
__stepEval(wolf, cons(wolf, cons(cabbage, cons(empty, cons(empty, Q19))))) :- _______eval(Q19).
__stepEval(wolf, cons(wolf, cons(cabbage, cons(cabbage, Q18)))) :- __swapEval(Q18).
__stepEval(wolf, cons(wolf, cons(wolf, Q17))) :- ______eval(Q17).
__stepEval(wolf, cons(wolf, cons(empty, cons(empty, cons(Q25, Q26))))) :- ____stepEval(Q25, Q26).
__stepEval(empty, Y15) :- ________eval(Y15).
____eval(cons(cabbage, Q2)) :- _____eval(Q2).
____eval(cons(empty, Q2)) :- ______eval(Q2).
_____eval(cons(empty, cons(empty, Q3))) :- _____eval(Q3).
_____eval(cons(wolf, cons(Q4, Q5))) :- ___stepEval(Q4, Q5).
_____eval(cons(cabbage, cons(cabbage, Q6))) :- _____eval(Q6).
_____eval(cons(cabbage, cons(empty, Q6))) :- ______eval(Q6).
___stepEval(wolf, cons(empty, cons(empty, Q1))) :- _____eval(Q1).
___stepEval(wolf, cons(wolf, cons(Q2, Q3))) :- ___stepEval(Q2, Q3).
___stepEval(wolf, cons(cabbage, cons(cabbage, Q5))) :- _____eval(Q5).
___stepEval(wolf, cons(cabbage, cons(empty, Q5))) :- ______eval(Q5).
___stepEval(empty, Y20) :- _______eval(Y20).
______eval(cons(empty, Q2)) :- ____eval(Q2).
______eval(cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(empty, cons(empty, Q6))))))) :- _____eval(Q6).
______eval(cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(wolf, cons(Q7, Q8))))))) :- ___stepEval(Q7, Q8).
______eval(cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(cabbage, Q10))))))) :- _____eval(Q10).
______eval(cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(empty, Q10))))))) :- ______eval(Q10).
______eval(cons(wolf, cons(cabbage, cons(empty, cons(empty, Q5))))) :- _______eval(Q5).
______eval(cons(wolf, cons(cabbage, cons(cabbage, Q4)))) :- __swapEval(Q4).
______eval(cons(wolf, cons(wolf, Q3))) :- ______eval(Q3).
______eval(cons(wolf, cons(empty, cons(empty, cons(Q11, Q12))))) :- ____stepEval(Q11, Q12).
_______eval(cons(empty, cons(wolf, cons(empty, cons(empty, Q4))))) :- _____eval(Q4).
_______eval(cons(empty, cons(wolf, cons(wolf, cons(Q5, Q6))))) :- ___stepEval(Q5, Q6).
_______eval(cons(empty, cons(wolf, cons(cabbage, cons(cabbage, Q8))))) :- _____eval(Q8).
_______eval(cons(empty, cons(wolf, cons(cabbage, cons(empty, Q8))))) :- ______eval(Q8).
_______eval(cons(empty, cons(empty, Q3))) :- _______eval(Q3).
_______eval(cons(cabbage, Q2)) :- __swapEval(Q2).
__swapEval(cons(Q1, Q2)) :- ____stepEval(Q1, Q2).
____stepEval(cabbage, cons(empty, cons(wolf, cons(empty, cons(empty, Q3))))) :- _____eval(Q3).
____stepEval(cabbage, cons(empty, cons(wolf, cons(wolf, cons(Q4, Q5))))) :- ___stepEval(Q4, Q5).
____stepEval(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(cabbage, Q7))))) :- _____eval(Q7).
____stepEval(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(empty, Q7))))) :- ______eval(Q7).
____stepEval(cabbage, cons(empty, cons(empty, Q2))) :- _______eval(Q2).
____stepEval(cabbage, cons(cabbage, Q1)) :- __swapEval(Q1).
____stepEval(wolf, Y27) :- ______eval(Y27).
____stepEval(empty, cons(empty, cons(Q8, Q9))) :- ____stepEval(Q8, Q9).
________eval(cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, nil))))))).
________eval(cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, cons(empty, cons(empty, Q10))))))))) :- _eval(Q10).
________eval(cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, cons(goat, cons(Q11, Q12))))))))) :- stepEval(Q11, Q12).
________eval(cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(empty, Q7))))))) :- __eval(Q7).
________eval(cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(wolf, Q6)))))) :- swapEval(Q6).
________eval(cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(cabbage, Q13))))))) :- __eval(Q13).
________eval(cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(goat, cons(goat, cons(Q14, Q15))))))))) :- _stepEval(Q14, Q15).
________eval(cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(goat, cons(wolf, Q16)))))))) :- _swapEval(Q16).
________eval(cons(empty, cons(cabbage, cons(goat, cons(goat, Q5))))) :- ___eval(Q5).
________eval(cons(empty, cons(cabbage, cons(cabbage, cons(Q17, Q18))))) :- __stepEval(Q17, Q18).
________eval(cons(empty, cons(wolf, cons(empty, Q19)))) :- ____eval(Q19).
________eval(cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(empty, cons(empty, Q23))))))))) :- _____eval(Q23).
________eval(cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(wolf, cons(Q24, Q25))))))))) :- ___stepEval(Q24, Q25).
________eval(cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(cabbage, Q27))))))))) :- _____eval(Q27).
________eval(cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(empty, Q27))))))))) :- ______eval(Q27).
________eval(cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(empty, Q22))))))) :- _______eval(Q22).
________eval(cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(cabbage, Q21)))))) :- __swapEval(Q21).
________eval(cons(empty, cons(wolf, cons(wolf, cons(wolf, Q20))))) :- ______eval(Q20).
________eval(cons(empty, cons(wolf, cons(wolf, cons(empty, cons(empty, cons(Q28, Q29))))))) :- ____stepEval(Q28, Q29).
________eval(cons(empty, cons(empty, Q3))) :- ________eval(Q3).
________eval(cons(goat, cons(Q30, Q31))) :- _____stepEval(Q30, Q31).
_____stepEval(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, nil))))))).
_____stepEval(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, cons(empty, cons(empty, Q9))))))))) :- _eval(Q9).
_____stepEval(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, cons(goat, cons(Q10, Q11))))))))) :- stepEval(Q10, Q11).
_____stepEval(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(empty, Q6))))))) :- __eval(Q6).
_____stepEval(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(wolf, Q5)))))) :- swapEval(Q5).
_____stepEval(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(cabbage, Q12))))))) :- __eval(Q12).
_____stepEval(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(goat, cons(goat, cons(Q13, Q14))))))))) :- _stepEval(Q13, Q14).
_____stepEval(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(goat, cons(wolf, Q15)))))))) :- _swapEval(Q15).
_____stepEval(goat, cons(empty, cons(cabbage, cons(goat, cons(goat, Q4))))) :- ___eval(Q4).
_____stepEval(goat, cons(empty, cons(cabbage, cons(cabbage, cons(Q16, Q17))))) :- __stepEval(Q16, Q17).
_____stepEval(goat, cons(empty, cons(wolf, cons(empty, Q18)))) :- ____eval(Q18).
_____stepEval(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(empty, cons(empty, Q22))))))))) :- _____eval(Q22).
_____stepEval(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(wolf, cons(Q23, Q24))))))))) :- ___stepEval(Q23, Q24).
_____stepEval(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(cabbage, Q26))))))))) :- _____eval(Q26).
_____stepEval(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(empty, Q26))))))))) :- ______eval(Q26).
_____stepEval(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(empty, Q21))))))) :- _______eval(Q21).
_____stepEval(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(cabbage, Q20)))))) :- __swapEval(Q20).
_____stepEval(goat, cons(empty, cons(wolf, cons(wolf, cons(wolf, Q19))))) :- ______eval(Q19).
_____stepEval(goat, cons(empty, cons(wolf, cons(wolf, cons(empty, cons(empty, cons(Q27, Q28))))))) :- ____stepEval(Q27, Q28).
_____stepEval(goat, cons(empty, cons(empty, Q2))) :- ________eval(Q2).
_____stepEval(goat, cons(goat, cons(Q29, Q30))) :- _____stepEval(Q29, Q30).
_____stepEval(empty, cons(empty, Q31)) :- _________eval(Q31).
_________eval(cons(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, nil)))))))).
_________eval(cons(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, cons(empty, cons(empty, Q11)))))))))) :- _eval(Q11).
_________eval(cons(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(goat, cons(goat, cons(Q12, Q13)))))))))) :- stepEval(Q12, Q13).
_________eval(cons(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(empty, cons(empty, Q8)))))))) :- __eval(Q8).
_________eval(cons(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(wolf, Q7))))))) :- swapEval(Q7).
_________eval(cons(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(cabbage, Q14)))))))) :- __eval(Q14).
_________eval(cons(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(goat, cons(goat, cons(Q15, Q16)))))))))) :- _stepEval(Q15, Q16).
_________eval(cons(goat, cons(empty, cons(cabbage, cons(goat, cons(wolf, cons(cabbage, cons(goat, cons(wolf, Q17))))))))) :- _swapEval(Q17).
_________eval(cons(goat, cons(empty, cons(cabbage, cons(goat, cons(goat, Q6)))))) :- ___eval(Q6).
_________eval(cons(goat, cons(empty, cons(cabbage, cons(cabbage, cons(Q18, Q19)))))) :- __stepEval(Q18, Q19).
_________eval(cons(goat, cons(empty, cons(wolf, cons(empty, Q20))))) :- ____eval(Q20).
_________eval(cons(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(empty, cons(empty, Q24)))))))))) :- _____eval(Q24).
_________eval(cons(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(wolf, cons(Q25, Q26)))))))))) :- ___stepEval(Q25, Q26).
_________eval(cons(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(cabbage, Q28)))))))))) :- _____eval(Q28).
_________eval(cons(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(wolf, cons(cabbage, cons(empty, Q28)))))))))) :- ______eval(Q28).
_________eval(cons(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(empty, cons(empty, Q23)))))))) :- _______eval(Q23).
_________eval(cons(goat, cons(empty, cons(wolf, cons(wolf, cons(cabbage, cons(cabbage, Q22))))))) :- __swapEval(Q22).
_________eval(cons(goat, cons(empty, cons(wolf, cons(wolf, cons(wolf, Q21)))))) :- ______eval(Q21).
_________eval(cons(goat, cons(empty, cons(wolf, cons(wolf, cons(empty, cons(empty, cons(Q29, Q30)))))))) :- ____stepEval(Q29, Q30).
_________eval(cons(goat, cons(empty, cons(empty, Q4)))) :- ________eval(Q4).
_________eval(cons(goat, cons(goat, cons(Q31, Q32)))) :- _____stepEval(Q31, Q32).
_________eval(cons(empty, cons(empty, Q33))) :- _________eval(Q33).