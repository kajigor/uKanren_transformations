rr(cons(Q1, cons(Q2, Q3))) :- gG(Q1, Q2, Q3).
gG(Y5, Y5, cons(Q1, Q2)) :- hG(Y5, Q1, Q2).
gG(Y1, Y5, Y6) :- _neqHG(Y1, Y5, Y6).
hG(Y9, Y10, nil) :- _______g(Y9, Y10, Y10, nil, nil).
hG(Y9, Y10, cons(Q1, Q2)) :- _gG(Y9, Y10, Q1, Q2).
_gG(Y14, Y16, Y16, Y17) :- fG(Y14, Y17, Y16).
_gG(Y14, Y15, Y16, Y17) :- neqHG(Y14, Y15, Y16, Y17).
fG(Y18, nil, Y20) :- _______g(Y18, Y20, Y20, nil, nil).
fG(Y18, cons(Q1, Q2), Y20) :- __hG(Y18, Q2, Q1, Y20, Y20).
f(nil, nil).
f(cons(Q1, Q2), Y39) :- h(Q1, Q2, Y39).
_neqG(Y46) :- _____g(zero, cons(zero, cons(succ(succ(zero)), cons(zero, cons(zero, nil)))), Q1, Y46).
h(Y49, nil, cons(Y49, nil)).
h(Y49, cons(Q1, Q2), Y51) :- _____g(Y49, Y51, Q1, Q2).
_____g(Y54, cons(Y54, Q1), Y54, Y55) :- f(Y55, Q1).
_____g(Y52, cons(Y52, Q1), Y54, Y55) :- neq(Y52, Y54), h(Y54, Y55, Q1).
neq(zero, succ(Q1)).
neq(succ(Q1), zero).
neq(succ(Q3), succ(Q2)) :- neq(Q3, Q2).
neqHG(Y58, zero, succ(Q1), Y62) :- __hG(Y58, Y62, succ(Q1), zero, zero).
neqHG(Y58, succ(Q1), zero, Y62) :- __hG(Y58, Y62, zero, succ(Q1), succ(Q1)).
neqHG(Y58, succ(Q3), succ(Q2), Y62) :- __hG(Y58, Y62, succ(Q2), succ(Q3), succ(Q3)), neq(Q3, Q2).
__hG(Y63, nil, Y66, Y67, Y68) :- _______g(Y63, Y67, Y68, cons(Y66, nil), cons(Y66, nil)).
__hG(Y63, cons(Q1, Q2), Y66, Y67, Y68) :- ___gG(Y63, Y66, Y67, Y68, Q1, Q2).
___gG(Y73, Y78, Y76, Y77, Y78, Y79) :- _fG(Y73, Y76, Y77, Y79, Y78).
___gG(Y73, Y75, Y76, Y77, Y78, Y79) :- neq(Y75, Y78), h(Y78, Y79, Q1), _______g(Y73, Y76, Y77, cons(Y75, Q1), cons(Y75, Q1)).
_fG(Y80, Y81, Y82, nil, Y84) :- _______g(Y80, Y81, Y82, cons(Y84, nil), cons(Y84, nil)).
_fG(Y80, Y81, Y82, cons(Q1, Q2), Y84) :- h(Q1, Q2, Q3), _______g(Y80, Y81, Y82, cons(Y84, Q3), cons(Y84, Q3)).
_______g(succ(zero), succ(zero), Y88, Y89, Y90) :- f(Y90, cons(zero, cons(succ(succ(zero)), cons(zero, cons(zero, nil))))).
_______g(succ(zero), Y87, Y88, cons(Q1, Q2), Y90) :- neq(succ(zero), Y87), _____g(Y88, cons(zero, cons(succ(succ(zero)), cons(zero, cons(zero, nil)))), Q1, Q2).
_neqHG(zero, succ(Q1), Y95) :- hG(zero, succ(Q1), Y95).
_neqHG(succ(Q1), zero, Y95) :- hG(succ(Q1), zero, Y95).
_neqHG(succ(Q3), succ(Q2), Y95) :- hG(succ(Q3), succ(Q2), Y95), neq(Q3, Q2).