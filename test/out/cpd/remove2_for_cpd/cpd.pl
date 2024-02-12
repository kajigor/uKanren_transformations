rr(cons(Q1, cons(Q2, Q3))) :- gG(Q1, Q2, Q3).
gG(Y5, Y5, cons(Q1, Q2)) :- hG(Y5, Q1, Q2).
gG(Y1, Y5, Y6) :- _neqHG(Y1, Y5, Y6).
hG(Y9, Y10, nil) :- ______g(Y9, Y10, Y10, nil, nil).
hG(Y9, Y10, cons(Q1, Q2)) :- _gG(Y9, Y10, Q1, Q2).
_gG(Y14, Y16, Y16, Y17) :- fG(Y14, Y17, Y16).
_gG(Y14, Y15, Y16, Y17) :- neqHG(Y14, Y15, Y16, Y17).
fG(Y18, nil, Y20) :- ______g(Y18, Y20, Y20, nil, nil).
fG(Y18, cons(Q1, Q2), Y20) :- __hG(Y18, Q2, Q1, Y20, Y20).
f(nil, nil).
f(cons(Q1, Q2), Y37) :- h(Q1, Q2, Y37).
h(Y42, nil, cons(Y42, nil)).
h(Y42, cons(Q1, Q2), Y44) :- ____g(Y42, Y44, Q1, Q2).
____g(Y47, cons(Y47, Q1), Y47, Y48) :- f(Y48, Q1).
____g(Y45, cons(Y45, Q1), Y47, Y48) :- neq(Y45, Y47), h(Y47, Y48, Q1).
neq(zero, succ(Q1)).
neq(succ(Q1), zero).
neq(succ(Q3), succ(Q2)) :- neq(Q3, Q2).
neqHG(Y51, zero, succ(Q1), Y55) :- __hG(Y51, Y55, succ(Q1), zero, zero).
neqHG(Y51, succ(Q1), zero, Y55) :- __hG(Y51, Y55, zero, succ(Q1), succ(Q1)).
neqHG(Y51, succ(Q3), succ(Q2), Y55) :- __hG(Y51, Y55, succ(Q2), succ(Q3), succ(Q3)), neq(Q3, Q2).
__hG(Y56, nil, Y59, Y60, Y61) :- ______g(Y56, Y60, Y61, cons(Y59, nil), cons(Y59, nil)).
__hG(Y56, cons(Q1, Q2), Y59, Y60, Y61) :- ___gG(Y56, Y59, Y60, Y61, Q1, Q2).
___gG(Y66, Y71, Y69, Y70, Y71, Y72) :- _fG(Y66, Y69, Y70, Y72, Y71).
___gG(Y66, Y68, Y69, Y70, Y71, Y72) :- neq(Y68, Y71), h(Y71, Y72, Q1), ______g(Y66, Y69, Y70, cons(Y68, Q1), cons(Y68, Q1)).
_fG(Y73, Y74, Y75, nil, Y77) :- ______g(Y73, Y74, Y75, cons(Y77, nil), cons(Y77, nil)).
_fG(Y73, Y74, Y75, cons(Q1, Q2), Y77) :- h(Q1, Q2, Q3), ______g(Y73, Y74, Y75, cons(Y77, Q3), cons(Y77, Q3)).
______g(succ(zero), succ(zero), Y81, Y82, Y83) :- f(Y83, cons(zero, cons(succ(succ(zero)), cons(zero, cons(zero, nil))))).
______g(succ(zero), zero, Y81, Y82, Y83) :- f(cons(Y81, Y82), cons(zero, cons(succ(succ(zero)), cons(zero, cons(zero, nil))))).
______g(succ(zero), succ(succ(Q1)), Y81, Y82, Y83) :- f(cons(Y81, Y82), cons(zero, cons(succ(succ(zero)), cons(zero, cons(zero, nil))))).
_neqHG(zero, succ(Q1), Y88) :- hG(zero, succ(Q1), Y88).
_neqHG(succ(Q1), zero, Y88) :- hG(succ(Q1), zero, Y88).
_neqHG(succ(Q3), succ(Q2), Y88) :- hG(succ(Q3), succ(Q2), Y88), neq(Q3, Q2).