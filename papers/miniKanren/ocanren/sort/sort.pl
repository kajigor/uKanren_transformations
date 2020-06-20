smallesto(cons(Y3, nil), Y3, nil).
smallesto(cons(Q3, Q4), Y3, cons(Q1, Q2)) :- minmaxo(Y3, Q3, Q5, Q1), smallesto(Q4, Q5, Q2).
sorto(nil, nil).
sorto(Y5, cons(Q1, Q2)) :- sorto(Q3, Q2), smallesto(Y5, Q1, Q3).
minmaxo(o, o, Y10, Y10).
minmaxo(s(Q1), s(Q1), s(Q2), s(Q2)) :- leo(Q1, Q2).
minmaxo(o, s(Q3), o, s(Q3)).
minmaxo(s(Q5), s(Q4), s(Q5), s(Q4)) :- gto(Q5, Q4).
leo(o, Y12).
leo(s(Q1), s(Q2)) :- leo(Q1, Q2).
gto(o, s(Q1)).
gto(s(Q3), s(Q2)) :- gto(Q3, Q2).
