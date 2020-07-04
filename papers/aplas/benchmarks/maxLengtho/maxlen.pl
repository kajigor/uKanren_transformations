maxLengtho(Y0, Y1, Y2) :- maxo(Y0, Y1), lengtho(Y0, Y2).
maxo(nil, o).
maxo(cons(Q1, Q2), Y4) :- leo(Q1), maxo1(Q2, Y4).
maxo(cons(s(Q5), nil), s(Q5)).
maxo(cons(s(Q5), cons(Q6, Q7)), Y4) :- _leo(Q5, Q6), _maxo1(Y4, Q7, Q5).
maxo(cons(s(Q5), cons(Q8, Q9)), Y4) :- gto(Q5, Q8), __maxo1(Y4, Q8, Q9).
lengtho(nil, o).
lengtho(cons(Q1, Q2), s(Q3)) :- lengtho(Q2, Q3).
leo(o).
maxo1(nil, o).
maxo1(cons(Q1, Q2), Y9) :- leo(Q1), maxo1(Q2, Y9).
maxo1(cons(s(Q5), nil), s(Q5)).
maxo1(cons(s(Q5), cons(Q6, Q7)), Y9) :- _leo(Q5, Q6), _maxo1(Y9, Q7, Q5).
maxo1(cons(s(Q5), cons(Q8, Q9)), Y9) :- gto(Q5, Q8), __maxo1(Y9, Q8, Q9).
_leo(Y10, o).
_leo(Y10, s(o)).
_leo(s(Q3), s(s(Q2))) :- __leo(Q2, Q3).
__leo(o, Y13).
__leo(s(Q1), s(Q2)) :- __leo(Q1, Q2).
_maxo1(s(Y16), nil, Y16).
_maxo1(Y14, cons(Q1, Q2), Y16) :- _leo(Y16, Q1), _maxo1(Y14, Q2, Y16).
_maxo1(Y14, cons(Q3, Q4), Y16) :- gto(Y16, Q3), __maxo1(Y14, Q3, Q4).
gto(o, s(s(Q2))).
gto(s(Q4), s(s(Q3))) :- _gto(Q3, Q4).
__maxo1(Y20, Y20, nil).
__maxo1(Y19, Y20, cons(Q1, Q2)) :- __leo(Q1, Y20), __maxo1(Y19, Y20, Q2).
__maxo1(Y19, Y20, cons(Q3, Q4)) :- _gto(Q3, Y20), __maxo1(Y19, Q3, Q4).
_gto(s(Q1), o).
_gto(s(Q2), s(Q3)) :- _gto(Q2, Q3).