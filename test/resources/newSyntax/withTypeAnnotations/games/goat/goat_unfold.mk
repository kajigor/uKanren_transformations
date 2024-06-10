eval y0 =
  (fresh q1, q2, q3, q4, q5, q6, q7, q8, q9, q10, q11, q12, q13, q14, q15, q16, q17, q18, q19, q20, q21, q22, q23, q24, q25, q26, q27, q28 in
    (((y0 == (Empty :: (Empty :: q1)) & eval q1) |
      (y0 == (Goat :: (Empty :: (Empty :: q2))) & _eval q2) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Empty :: (Empty :: q3))))) & __eval q3) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Empty :: (Cabbage :: (Empty :: (Empty :: q4))))))) & ___eval q4) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: (Empty :: (Empty :: q5)))))))) & swapEval q5) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: (Empty :: (Cabbage :: (Empty :: (Empty :: q6)))))))))) & _swapEval q6) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: q7))))))))) & __eval q7) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: (Empty :: (Cabbage :: (Cabbage :: q7))))))))) & ____eval q7) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: (Wolf :: q8))))))) & ___eval q8) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Empty :: (Cabbage :: (Cabbage :: q9)))))) & __swapEval q9) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Wolf :: (Empty :: (Empty :: q9)))))) & _swapEval q9) | (
      y0 == (Goat :: (Empty :: (Wolf :: (Wolf :: (Wolf :: q10))))) & __eval q10) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Wolf :: (Cabbage :: (Empty :: (Empty :: q8))))))) & ____eval q8) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Wolf :: (Cabbage :: (Empty :: (Wolf :: (Empty :: (Empty :: q7))))))))) & ___eval q7) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Wolf :: (Cabbage :: (Empty :: (Wolf :: (Wolf :: q11)))))))) & swapEval q11) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Wolf :: (Cabbage :: (Empty :: (Wolf :: (Cabbage :: (Empty :: q12))))))))) & __eval q12) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Wolf :: (Cabbage :: (Empty :: (Wolf :: (Cabbage :: (Cabbage :: q12))))))))) & ___eval q12) |
      (y0 == (Goat :: (Empty :: (Wolf :: (Wolf :: (Cabbage :: (Cabbage :: q13)))))) & _swapEval q13) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Goat :: q10))))) & _____eval q10) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Empty :: (Empty :: q14))))))) & ______eval q14) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Empty :: (Goat :: q14))))))) & _______eval q14) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Empty :: (Empty :: q15)))))))) & swapEval q15) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Empty :: (Cabbage :: (Empty :: (Empty :: q16)))))))))) & _swapEval q16) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: (Empty :: (Empty :: q17))))))))))) & __eval q17) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: (Empty :: (Cabbage :: (Empty :: (Empty :: q18))))))))))))) & ___eval q18) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: q19)))))))))))) & swapEval q19) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: (Empty :: (Cabbage :: (Cabbage :: q19)))))))))))) & __swapEval q19) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Empty :: (Cabbage :: (Wolf :: (Wolf :: q20)))))))))) & _swapEval q20) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Empty :: (Cabbage :: (Cabbage :: q21))))))))) & ____eval q21) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Wolf :: (Empty :: (Empty :: q21))))))))) & ___eval q21) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Wolf :: (Wolf :: q22)))))))) & swapEval q22) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Wolf :: (Cabbage :: (Empty :: (Empty :: q20)))))))))) & __swapEval q20) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Wolf :: (Cabbage :: (Empty :: (Wolf :: (Empty :: (Empty :: q19)))))))))))) & _swapEval q19) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Wolf :: (Cabbage :: (Empty :: (Wolf :: (Wolf :: q23))))))))))) & __eval q23) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Wolf :: (Cabbage :: (Empty :: (Wolf :: (Cabbage :: (Empty :: q24)))))))))))) & swapEval q24) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Wolf :: (Cabbage :: (Empty :: (Wolf :: (Cabbage :: (Cabbage :: q24)))))))))))) & _swapEval q24) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Wolf :: (Wolf :: (Cabbage :: (Cabbage :: q25))))))))) & ___eval q25) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Cabbage :: (Goat :: (Goat :: q22)))))))) & ___swapEval q22) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Cabbage :: (Goat :: (Wolf :: q22)))))))) & ____swapEval q22) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Goat :: (Wolf :: (Cabbage :: (Cabbage :: q26))))))) & ______eval q26) |
      (y0 == (Goat :: (Empty :: (Cabbage :: (Cabbage :: q27)))) & ____swapEval q27) |
      (y0 == (Goat :: (Goat :: q28)) & eval q28))));

_eval y1 = (fresh q1 in (((y1 == (Empty :: q1) & ____swapEval q1) | (y1 == (Goat :: q1) & eval q1))));

__eval y2 = (fresh q1 in (((y2 == (Empty :: q1) & __swapEval q1) | (y2 == (Wolf :: q1) & _swapEval q1))));

___eval y3 = (fresh q1, q2 in (((y3 == (Empty :: (Empty :: q1)) & ___eval q1) | (y3 == (Wolf :: q2) & swapEval q2) | (y3 == (Cabbage :: q2) & __swapEval q2))));

swapEval y4 = (fresh q1 in (((y4 == (Empty :: q1) & ____eval q1) | (y4 == (Wolf :: q1) & ___eval q1))));

_swapEval y6 = (fresh q1, q2 in (((y6 == (Empty :: (Empty :: q1)) & _swapEval q1) | (y6 == (Wolf :: q2) & __eval q2) | (y6 == (Cabbage :: q2) & ____eval q2))));

____eval y8 = (fresh q1 in (((y8 == (Empty :: q1) & swapEval q1) | (y8 == (Cabbage :: q1) & _swapEval q1))));

__swapEval y9 = (fresh q1 in (((y9 == (Empty :: q1) & __eval q1) | (y9 == (Cabbage :: q1) & ___eval q1))));

_____eval y11 = (fresh q1, q2 in (((y11 == (Goat :: (Goat :: q1)) & _____eval q1) | (y11 == (Goat :: (Wolf :: q1)) & ______eval q1) | (y11 == (Cabbage :: q2) & ____swapEval q2))));

______eval y12 = (fresh q1, q2 in (((y12 == (Empty :: (Empty :: q1)) & ______eval q1) | (y12 == (Empty :: (Goat :: q1)) & _______eval q1) | (y12 == (Wolf :: q2) & swapEval q2) | (y12 == (Cabbage :: q2) & ___swapEval q2))));

_______eval y13 = (fresh q1, q2 in ((y13 == [] | (y13 == (Empty :: (Empty :: q1)) & _______eval q1) | (y13 == (Goat :: (Empty :: q2)) & ______eval q2) | (y13 == (Goat :: (Goat :: q2)) & _______eval q2))));

___swapEval y14 = (fresh q1, q2 in (((y14 == (Goat :: (Goat :: q1)) & ___swapEval q1) | (y14 == (Goat :: (Wolf :: q1)) & ____swapEval q1) | (y14 == (Cabbage :: q2) & ______eval q2))));

____swapEval y16 = (fresh q1 in (((y16 == (Empty :: q1) & _eval q1) | (y16 == (Wolf :: q1) & __eval q1) | (y16 == (Cabbage :: q1) & _____eval q1))));


? eval x0