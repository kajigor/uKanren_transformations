__type_(iConst_(Q1)).
__type_(plus_(Q2, Q3)) :- type_Type_(Q2, Q3).
__type_(mult_(Q2, Q3)) :- type_Type_(Q2, Q3).
__type_(let_(Q4, Q5)) :- ___type_(Q4, Q6), _____type_(nil, Q5, Q6).
__type_(if_(Q7, Q8, Q9)) :- _________type_(Q7), __type_(Q8), __type_(Q9).
type_Type_(Y5, Y6) :- __type_(Y5), __type_(Y6).
___type_(bConst_(Q1), boolean).
___type_(iConst_(Q2), integer).
___type_(plus_(Q3, Q4), integer) :- type_Type_(Q3, Q4).
___type_(mult_(Q3, Q4), integer) :- type_Type_(Q3, Q4).
___type_(equal_(Q5, Q6), boolean) :- ___type_(Q5, Q7), ___type_(Q6, Q7).
___type_(less_(Q5, Q6), boolean) :- type_Type_(Q5, Q6).
___type_(let_(Q8, Q9), Y8) :- ___type_(Q8, Q10).
___type_(if_(Q11, Q12, Q13), Y8) :- _________type_(Q11), ___type_(Q12, Y8), ___type_(Q13, Y8).
____type_(Y12, iConst_(Q1)).
____type_(cons(integer, Q3), var_(o)).
____type_(cons(Q5, Q3), var_(s(Q4))) :- _idx(Q3, Q4).
____type_(Y12, plus_(Q6, Q7)) :- _type_Type_(Y12, Q6, Q7).
____type_(Y12, mult_(Q6, Q7)) :- _type_Type_(Y12, Q6, Q7).
____type_(Y12, let_(Q8, Q9)) :- ______type_(Q8, Y12, some(Q10)), _____type_(Y12, Q9, Q10).
____type_(Y12, if_(Q11, Q12, Q13)) :- ________type_(Y12, Q11), ____type_(Y12, Q12), ____type_(Y12, Q13).
_idx(cons(integer, Q1), o).
_idx(cons(Q3, Q1), s(Q2)) :- _idx(Q1, Q2).
_type_Type_(Y16, Y17, Y18) :- ____type_(Y16, Y17), ____type_(Y16, Y18).
_____type_(Y19, iConst_(Q1), Y21).
_____type_(Y19, var_(o), integer).
_____type_(Y19, var_(s(Q3)), Y21) :- _idx(Y19, Q3).
_____type_(Y19, plus_(Q4, Q5), Y21) :- __type_Type_(Y19, Y21, Q4, Q5).
_____type_(Y19, mult_(Q4, Q5), Y21) :- __type_Type_(Y19, Y21, Q4, Q5).
_____type_(Y19, let_(Q6, Q7), Y21) :- ______type_(Q6, cons(Y21, Y19), some(Q8)), _____type_(cons(Y21, Y19), Q7, Q8).
_____type_(Y19, if_(Q9, Q10, Q11), Y21) :- _______type_(Y19, Y21, Q9), _____type_(Y19, Q10, Y21), _____type_(Y19, Q11, Y21).
______type_(Y22, Y23, Y24) :- success().
__type_Type_(Y25, Y26, Y27, Y28) :- _____type_(Y25, Y27, Y26), _____type_(Y25, Y28, Y26).
_______type_(Y29, Y30, bConst_(Q1)).
_______type_(Y29, boolean, var_(o)).
_______type_(cons(boolean, Q4), Y30, var_(s(o))).
_______type_(cons(Q6, Q4), Y30, var_(s(s(Q5)))) :- __idx(Q4, Q5).
_______type_(Y29, Y30, equal_(Q7, Q8)) :- ______type_(Q7, cons(Y30, Y29), some(Q9)), ______type_(Q8, cons(Y30, Y29), some(Q9)).
_______type_(Y29, Y30, less_(Q7, Q8)) :- __type_Type_(Y29, Y30, Q7, Q8).
_______type_(Y29, Y30, let_(Q10, Q11)) :- ______type_(Q10, cons(Y30, Y29), some(Q12)), _______type_(cons(Y30, Y29), Q12, Q11).
_______type_(Y29, Y30, if_(Q13, Q14, Q15)) :- _______type_(Y29, Y30, Q13), _______type_(Y29, Y30, Q14), _______type_(Y29, Y30, Q15).
__idx(cons(boolean, Q1), o).
__idx(cons(Q3, Q1), s(Q2)) :- __idx(Q1, Q2).
________type_(Y34, bConst_(Q1)).
________type_(Y34, var_(Q2)) :- __idx(Y34, Q2).
________type_(Y34, equal_(Q3, Q4)) :- ______type_(Q3, Y34, some(Q5)), ______type_(Q4, Y34, some(Q5)).
________type_(Y34, less_(Q3, Q4)) :- _type_Type_(Y34, Q3, Q4).
________type_(Y34, let_(Q6, Q7)) :- ______type_(Q6, Y34, some(Q8)), _______type_(Y34, Q8, Q7).
________type_(Y34, if_(Q9, Q10, Q11)) :- ________type_(Y34, Q9), ________type_(Y34, Q10), ________type_(Y34, Q11).
_________type_(bConst_(Q1)).
_________type_(equal_(Q2, Q3)) :- ____type_Type_(Q2, Q3).
_________type_(less_(Q2, Q3)) :- type_Type_(Q2, Q3).
_________type_(let_(Q4, Q5)) :- ___type_(Q4, Q6), ________type_(cons(Q6, nil), Q5).
_________type_(if_(Q7, Q8, Q9)) :- _________type_(Q7), _________type_(Q8), _________type_(Q9).
____type_Type_(Y41, Y42) :- ___type_(Y41, Q1), ___type_(Y42, Q1).