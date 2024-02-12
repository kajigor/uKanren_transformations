solve(cons(s(s(o)), cons(s(o), Q1)), Y1) :- clausSolve1Solve1Solve1(Q2, Q3, Y1), clausClaus(Q2, Q1, Q3).
solve(cons(s(s(o)), cons(s(o), Q4)), Y1) :- clausSolve1Solve1Solve1Solve1(Q5, Q6, Q7, Y1), clausClaus(Q6, Q4, Q7), claus(member(o, cons(o, cons(s(o), cons(s(s(o)), cons(o, nil))))), Q5).
clausSolve1Solve1Solve1(Y4, Y6, Y7) :- __solve1Solve1(s(s(s(s(s(s(o)))))), Y4, Y6, Y7).
clausSolve1Solve1Solve1(Y4, Y6, Y7) :- _clausSolve1Solve1Solve1(Y4, Y6, Y7).
claus(member(Q1, cons(Q1, Q2)), nil).
claus(member(Q3, cons(Q5, Q4)), cons(member(Q3, Q4), nil)).
claus(inBoth(Q6, Q7, Q8), cons(member(Q6, Q7), cons(member(Q6, Q8), nil))).
claus(app(nil, Q9, Q9), nil).
claus(app(cons(Q13, Q10), Q11, cons(Q13, Q12)), cons(app(Q10, Q11, Q12), nil)).
claus(delete(Q14, cons(Q14, Q15), Q15), nil).
claus(delete(Q16, cons(Q19, Q17), cons(Q19, Q18)), cons(delete(Q16, Q17, Q18), nil)).
claus(test(Q20, Q21, Q22, Q24), cons(inBoth(Q20, Q21, Q22), cons(delete(Q20, Q21, Q23), cons(app(Q23, Q22, Q24), nil)))).
solve1(Y16, nil, Y16).
solve1(Y14, cons(Q1, Q2), Y16) :- claus(Q1, Q3), solve1(Q4, Q3, s(Y16)), solve1(Y14, Q2, Q4).
_clausSolve1Solve1Solve1(Y17, Y19, Y20) :- __solve1Solve1(s(s(s(s(s(s(s(s(s(o))))))))), Y17, Y19, Y20).
clausClaus(nil, Y29, Y30) :- claus(app(cons(o, cons(s(s(o)), cons(s(o), nil))), cons(o, cons(s(o), cons(s(s(o)), cons(o, nil)))), Y29), Y30).
clausClaus(cons(delete(o, cons(o, cons(s(s(o)), cons(s(o), nil))), Q1), nil), Y29, Y30) :- claus(app(cons(o, Q1), cons(o, cons(s(o), cons(s(s(o)), cons(o, nil)))), Y29), Y30).
clausSolve1Solve1Solve1Solve1(Y33, Y35, Y37, Y38) :- solve1Solve1Solve1(Y33, Y35, Y37, Y38).
solve1Solve1Solve1(nil, Y41, Y43, Y44) :- __solve1Solve1(s(s(s(s(s(s(s(o))))))), Y41, Y43, Y44).
solve1Solve1Solve1(cons(Q1, Q2), Y41, Y43, Y44) :- __solve1Solve1(Q3, Y41, Y43, Y44), claus(Q1, Q4), solve1(Q5, Q4, s(s(s(s(s(s(s(s(o))))))))), solve1(Q3, Q2, Q5).
__solve1Solve1(Y45, nil, Y48, Y49) :- solve1(Y49, Y48, s(s(s(s(s(s(Y45))))))).
__solve1Solve1(Y45, cons(Q1, Q2), Y48, Y49) :- claus(Q1, Q3), solve1(Q4, Q3, s(s(s(s(Y45))))), solve1(Q5, Q2, Q4), solve1(Y49, Y48, s(s(s(Q5)))).