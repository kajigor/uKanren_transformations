check(cons(Q1, Q2)) :- one_step(Q1, Q3), _check(Q2, Q3).
one_step(pair(one, thr), triple(cons(s(z), cons(s(s(z)), nil)), nil, cons(z, nil))).
one_step(pair(one, two), triple(cons(s(z), cons(s(s(z)), nil)), cons(z, nil), nil)).
_check(nil, triple(nil, nil, Q1)).
_check(cons(Q2, Q3), Y4) :- _one_step(Y4, Q2, Q4), __check(Q3, Q4).
_one_step(triple(Q3, nil, cons(Q4, Q5)), pair(thr, two), Y7) :- set(Y7, Q4, Q3, Q5).
_one_step(triple(nil, Q6, cons(Q4, Q5)), pair(thr, one), Y7) :- _set(Y7, Q4, Q6, Q5).
_one_step(triple(Q3, cons(Q4, Q5), nil), pair(two, thr), Y7) :- __set(Y7, Q4, Q3, Q5).
_one_step(triple(nil, cons(Q4, Q5), Q7), pair(two, one), triple(cons(Q4, nil), Q5, Q7)).
_one_step(triple(cons(Q4, Q5), Q6, nil), pair(one, thr), triple(Q5, Q6, cons(Q4, nil))).
_one_step(triple(cons(Q4, Q5), nil, Q7), pair(one, two), triple(Q5, cons(Q4, nil), Q7)).
_one_step(triple(Q8, cons(Q9, Q10), cons(Q4, Q5)), pair(thr, two), Y7) :- lessSet(Y7, Q4, Q8, Q9, Q10, Q5).
_one_step(triple(cons(Q9, Q10), Q11, cons(Q4, Q5)), pair(thr, one), Y7) :- _lessSet(Y7, Q4, Q11, Q9, Q10, Q5).
_one_step(triple(Q8, cons(Q4, Q5), cons(Q9, Q10)), pair(two, thr), Y7) :- __lessSet(Y7, Q4, Q8, Q9, Q10, Q5).
_one_step(triple(cons(Q9, Q10), cons(Q4, Q5), Q12), pair(two, one), triple(cons(Q4, cons(Q9, Q10)), Q5, Q12)) :- less(Q4, Q9).
_one_step(triple(cons(Q4, Q5), Q11, cons(Q9, Q10)), pair(one, thr), triple(Q5, Q11, cons(Q4, cons(Q9, Q10)))) :- less(Q4, Q9).
_one_step(triple(cons(o, Q5), cons(s(Q13), Q10), Q12), pair(one, two), triple(Q5, cons(o, cons(s(Q13), Q10)), Q12)).
_one_step(triple(cons(s(Q14), Q5), cons(s(Q13), Q10), Q12), pair(one, two), triple(Q5, cons(s(Q14), cons(s(Q13), Q10)), Q12)) :- less(Q14, Q13).
__check(Y8, Y9) :- success().
__check(Y8, Y9) :- _one_step(Q1, Q2, Y9), __check(Y8, Y9).
set(triple(Y12, cons(Y11, nil), Y13), Y11, Y12, Y13).
_set(triple(cons(Y15, nil), Y16, Y17), Y15, Y16, Y17).
__set(triple(Y20, Y21, cons(Y19, nil)), Y19, Y20, Y21).
lessSet(triple(Y24, cons(Y23, cons(Y25, Y26)), Y27), Y23, Y24, Y25, Y26, Y27) :- less(Y23, Y25).
_lessSet(triple(cons(Y29, cons(Y31, Y32)), Y30, Y33), Y29, Y30, Y31, Y32, Y33) :- less(Y29, Y31).
__lessSet(triple(Y36, Y39, cons(Y35, cons(Y37, Y38))), Y35, Y36, Y37, Y38, Y39) :- less(Y35, Y37).
less(o, s(Q1)).
less(s(Q2), s(Q1)) :- less(Q2, Q1).