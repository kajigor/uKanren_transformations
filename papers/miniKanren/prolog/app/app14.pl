appendo(nil, Y2, Y2).
appendo(cons(Q1, nil), Q3, cons(Q1, Q3)).
appendo(cons(Q1, cons(Q4, nil)), Q6, cons(Q1, cons(Q4, Q6))).
appendo(cons(Q1, cons(Q4, cons(Q7, nil))), Q9, cons(Q1, cons(Q4, cons(Q7, Q9)))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, nil)))), Q12, cons(Q1, cons(Q4, cons(Q7, cons(Q10, Q12))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, nil))))), Q15, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, Q15)))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, nil)))))), Q18, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, Q18))))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, nil))))))), Q21, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, Q21)))))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, nil)))))))), Q24, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, Q24))))))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, nil))))))))), Q27, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, Q27)))))))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, nil)))))))))), Q30, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, Q30))))))))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, cons(Q31, nil))))))))))), Q33, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, cons(Q31, Q33)))))))))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, cons(Q31, cons(Q34, nil)))))))))))), Q36, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, cons(Q31, cons(Q34, Q36))))))))))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, cons(Q31, cons(Q34, cons(Q37, nil))))))))))))), Q39, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, cons(Q31, cons(Q34, cons(Q37, Q39)))))))))))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, cons(Q31, cons(Q34, cons(Q37, cons(Q40, nil)))))))))))))), Q42, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, cons(Q31, cons(Q34, cons(Q37, cons(Q40, Q42))))))))))))))).
appendo(cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, cons(Q31, cons(Q34, cons(Q37, cons(Q40, cons(Q43, Q44))))))))))))))), Y1, cons(Q1, cons(Q4, cons(Q7, cons(Q10, cons(Q13, cons(Q16, cons(Q19, cons(Q22, cons(Q25, cons(Q28, cons(Q31, cons(Q34, cons(Q37, cons(Q40, cons(Q43, Q45)))))))))))))))) :- _appendo(Y1, Q44, Q45).
_appendo(Y5, nil, Y5).
_appendo(Y3, cons(Q1, Q2), cons(Q1, Q3)) :- _appendo(Y3, Q2, Q3).