check(cons(Q1, nil)) :- one_step(Q1, Q2).
check(cons(Q1, cons(Q3, Q4))) :- one_stepOne_stepCheck(Q1, Q3, Q4).
one_stepOne_stepCheck(pair(one, two), Y3, nil) :- _one_step(Y3, Q1).
one_stepOne_stepCheck(pair(one, two), Y3, cons(Q2, Q3)) :- _one_stepOne_stepCheck(Y3, Q2, Q3).
one_stepOne_stepCheck(pair(one, thr), Y3, nil) :- __________________________one_step(Y3, Q1).
one_stepOne_stepCheck(pair(one, thr), Y3, cons(Q2, Q3)) :- __________________________one_stepOne_stepCheck(Y3, Q2, Q3).
_one_stepOne_stepCheck(pair(one, thr), Y8, nil) :- __one_step(Y8, Q1).
_one_stepOne_stepCheck(pair(one, thr), Y8, cons(Q2, Q3)) :- __one_stepOne_stepCheck(Y8, Q2, Q3).
_one_stepOne_stepCheck(pair(two, thr), Y8, nil) :- __________________________one_step(Y8, Q1).
_one_stepOne_stepCheck(pair(two, thr), Y8, cons(Q2, Q3)) :- __________________________one_stepOne_stepCheck(Y8, Q2, Q3).
_one_stepOne_stepCheck(pair(two, one), Y8, nil) :- one_step(Y8, Q4).
_one_stepOne_stepCheck(pair(two, one), Y8, cons(Q5, Q6)) :- one_stepOne_stepCheck(Y8, Q5, Q6).
__one_stepOne_stepCheck(pair(two, one), Y13, nil) :- ___one_step(Y13, Q1).
__one_stepOne_stepCheck(pair(two, one), Y13, cons(Q2, Q3)) :- ___one_stepOne_stepCheck(Y13, Q2, Q3).
__one_stepOne_stepCheck(pair(two, thr), Y13, nil) :- _________________________one_step(Y13, Q1).
__one_stepOne_stepCheck(pair(two, thr), Y13, cons(Q2, Q3)) :- _________________________one_stepOne_stepCheck(Y13, Q2, Q3).
__one_stepOne_stepCheck(pair(thr, one), Y13, nil) :- _one_step(Y13, Q4).
__one_stepOne_stepCheck(pair(thr, one), Y13, cons(Q5, Q6)) :- _one_stepOne_stepCheck(Y13, Q5, Q6).
___one_stepOne_stepCheck(pair(one, two), Y18, nil) :- __one_step(Y18, Q1).
___one_stepOne_stepCheck(pair(one, two), Y18, cons(Q2, Q3)) :- __one_stepOne_stepCheck(Y18, Q2, Q3).
___one_stepOne_stepCheck(pair(thr, two), Y18, nil) :- ____one_step(Y18, Q1).
___one_stepOne_stepCheck(pair(thr, two), Y18, cons(Q2, Q3)) :- ____one_stepOne_stepCheck(Y18, Q2, Q3).
___one_stepOne_stepCheck(pair(one, thr), Y18, nil) :- _________________________one_step(Y18, Q4).
___one_stepOne_stepCheck(pair(one, thr), Y18, cons(Q5, Q6)) :- _________________________one_stepOne_stepCheck(Y18, Q5, Q6).
____one_stepOne_stepCheck(pair(one, thr), Y23, nil) :- _____one_step(Y23, Q1).
____one_stepOne_stepCheck(pair(one, thr), Y23, cons(Q2, Q3)) :- _____one_stepOne_stepCheck(Y23, Q2, Q3).
____one_stepOne_stepCheck(pair(two, thr), Y23, nil) :- ___one_step(Y23, Q1).
____one_stepOne_stepCheck(pair(two, thr), Y23, cons(Q2, Q3)) :- ___one_stepOne_stepCheck(Y23, Q2, Q3).
____one_stepOne_stepCheck(pair(one, two), Y23, nil) :- ______one_step(Y23, Q4).
____one_stepOne_stepCheck(pair(one, two), Y23, cons(Q5, Q6)) :- ______one_stepOne_stepCheck(Y23, Q5, Q6).
_____one_stepOne_stepCheck(pair(two, one), Y28, nil) :- __________________________one_step(Y28, Q1).
_____one_stepOne_stepCheck(pair(two, one), Y28, cons(Q2, Q3)) :- __________________________one_stepOne_stepCheck(Y28, Q2, Q3).
_____one_stepOne_stepCheck(pair(thr, one), Y28, nil) :- ____one_step(Y28, Q4).
_____one_stepOne_stepCheck(pair(thr, one), Y28, cons(Q5, Q6)) :- ____one_stepOne_stepCheck(Y28, Q5, Q6).
_____one_stepOne_stepCheck(pair(thr, two), Y28, nil) :- ______one_step(Y28, Q4).
_____one_stepOne_stepCheck(pair(thr, two), Y28, cons(Q5, Q6)) :- ______one_stepOne_stepCheck(Y28, Q5, Q6).
______one_stepOne_stepCheck(pair(one, thr), Y33, nil) :- _______one_step(Y33, Q1).
______one_stepOne_stepCheck(pair(one, thr), Y33, cons(Q2, Q3)) :- _______one_stepOne_stepCheck(Y33, Q2, Q3).
______one_stepOne_stepCheck(pair(two, thr), Y33, nil) :- _____one_step(Y33, Q1).
______one_stepOne_stepCheck(pair(two, thr), Y33, cons(Q2, Q3)) :- _____one_stepOne_stepCheck(Y33, Q2, Q3).
______one_stepOne_stepCheck(pair(two, one), Y33, nil) :- ____one_step(Y33, Q4).
______one_stepOne_stepCheck(pair(two, one), Y33, cons(Q5, Q6)) :- ____one_stepOne_stepCheck(Y33, Q5, Q6).
_______one_stepOne_stepCheck(pair(two, one), Y38, nil) :- ________one_step(Y38, Q1).
_______one_stepOne_stepCheck(pair(two, one), Y38, cons(Q2, Q3)) :- ________one_stepOne_stepCheck(Y38, Q2, Q3).
_______one_stepOne_stepCheck(pair(thr, one), Y38, nil) :- ______one_step(Y38, Q1).
_______one_stepOne_stepCheck(pair(thr, one), Y38, cons(Q2, Q3)) :- ______one_stepOne_stepCheck(Y38, Q2, Q3).
_______one_stepOne_stepCheck(pair(two, thr), Y38, nil) :- ________________________one_step(Y38, Q4).
_______one_stepOne_stepCheck(pair(two, thr), Y38, cons(Q5, Q6)) :- ________________________one_stepOne_stepCheck(Y38, Q5, Q6).
________one_stepOne_stepCheck(pair(one, two), Y43, nil) :- _______one_step(Y43, Q1).
________one_stepOne_stepCheck(pair(one, two), Y43, cons(Q2, Q3)) :- _______one_stepOne_stepCheck(Y43, Q2, Q3).
________one_stepOne_stepCheck(pair(one, thr), Y43, nil) :- ________________________one_step(Y43, Q1).
________one_stepOne_stepCheck(pair(one, thr), Y43, cons(Q2, Q3)) :- ________________________one_stepOne_stepCheck(Y43, Q2, Q3).
________one_stepOne_stepCheck(pair(two, thr), Y43, nil) :- _________one_step(Y43).
________one_stepOne_stepCheck(pair(two, thr), Y43, cons(Q4, Q5)) :- _________one_stepOne_stepCheck(Y43, Q4, Q5).
_________one_step(pair(one, thr)).
_________one_stepOne_stepCheck(pair(one, two), Y50, nil) :- __________one_step(Y50).
_________one_stepOne_stepCheck(pair(one, two), Y50, cons(Q1, Q2)) :- __________one_stepOne_stepCheck(Y50, Q1, Q2).
_________one_stepOne_stepCheck(pair(thr, two), Y50, nil) :- ________one_step(Y50, Q3).
_________one_stepOne_stepCheck(pair(thr, two), Y50, cons(Q1, Q2)) :- ________one_stepOne_stepCheck(Y50, Q1, Q2).
_________one_stepOne_stepCheck(pair(one, thr), Y50, nil) :- _______________________one_step(Y50, Q4).
_________one_stepOne_stepCheck(pair(one, thr), Y50, cons(Q5, Q6)) :- _______________________one_stepOne_stepCheck(Y50, Q5, Q6).
__________one_step(pair(two, thr)).
__________one_stepOne_stepCheck(pair(two, one), Y57, nil) :- _________one_step(Y57).
__________one_stepOne_stepCheck(pair(two, one), Y57, cons(Q1, Q2)) :- _________one_stepOne_stepCheck(Y57, Q1, Q2).
__________one_stepOne_stepCheck(pair(thr, one), Y57, nil) :- ___________one_step(Y57, Q3).
__________one_stepOne_stepCheck(pair(thr, one), Y57, cons(Q1, Q2)) :- ___________one_stepOne_stepCheck(Y57, Q1, Q2).
__________one_stepOne_stepCheck(pair(two, thr), Y57, nil) :- _______________________one_step(Y57, Q4).
__________one_stepOne_stepCheck(pair(two, thr), Y57, cons(Q5, Q6)) :- _______________________one_stepOne_stepCheck(Y57, Q5, Q6).
___________one_stepOne_stepCheck(pair(one, thr), Y62, nil) :- __________one_step(Y62).
___________one_stepOne_stepCheck(pair(one, thr), Y62, cons(Q1, Q2)) :- __________one_stepOne_stepCheck(Y62, Q1, Q2).
___________one_stepOne_stepCheck(pair(two, one), Y62, nil) :- ____________one_step(Y62, Q3).
___________one_stepOne_stepCheck(pair(two, one), Y62, cons(Q4, Q5)) :- ____________one_stepOne_stepCheck(Y62, Q4, Q5).
___________one_stepOne_stepCheck(pair(two, thr), Y62, nil) :- ______________________one_step(Y62, Q3).
___________one_stepOne_stepCheck(pair(two, thr), Y62, cons(Q4, Q5)) :- ______________________one_stepOne_stepCheck(Y62, Q4, Q5).
____________one_stepOne_stepCheck(pair(one, two), Y67, nil) :- ___________one_step(Y67, Q1).
____________one_stepOne_stepCheck(pair(one, two), Y67, cons(Q2, Q3)) :- ___________one_stepOne_stepCheck(Y67, Q2, Q3).
____________one_stepOne_stepCheck(pair(thr, two), Y67, nil) :- _____________one_step(Y67, Q1).
____________one_stepOne_stepCheck(pair(thr, two), Y67, cons(Q2, Q3)) :- _____________one_stepOne_stepCheck(Y67, Q2, Q3).
____________one_stepOne_stepCheck(pair(one, thr), Y67, nil) :- ______________________one_step(Y67, Q4).
____________one_stepOne_stepCheck(pair(one, thr), Y67, cons(Q5, Q6)) :- ______________________one_stepOne_stepCheck(Y67, Q5, Q6).
_____________one_stepOne_stepCheck(pair(one, thr), Y72, nil) :- ______________one_step(Y72, Q1).
_____________one_stepOne_stepCheck(pair(one, thr), Y72, cons(Q2, Q3)) :- ______________one_stepOne_stepCheck(Y72, Q2, Q3).
_____________one_stepOne_stepCheck(pair(two, thr), Y72, nil) :- ____________one_step(Y72, Q1).
_____________one_stepOne_stepCheck(pair(two, thr), Y72, cons(Q2, Q3)) :- ____________one_stepOne_stepCheck(Y72, Q2, Q3).
_____________one_stepOne_stepCheck(pair(one, two), Y72, nil) :- _____________________one_step(Y72, Q4).
_____________one_stepOne_stepCheck(pair(one, two), Y72, cons(Q5, Q6)) :- _____________________one_stepOne_stepCheck(Y72, Q5, Q6).
______________one_stepOne_stepCheck(pair(one, two), Y77, nil) :- _______________one_step(Y77, Q1).
______________one_stepOne_stepCheck(pair(one, two), Y77, cons(Q2, Q3)) :- _______________one_stepOne_stepCheck(Y77, Q2, Q3).
______________one_stepOne_stepCheck(pair(thr, one), Y77, nil) :- _____________one_step(Y77, Q4).
______________one_stepOne_stepCheck(pair(thr, one), Y77, cons(Q5, Q6)) :- _____________one_stepOne_stepCheck(Y77, Q5, Q6).
______________one_stepOne_stepCheck(pair(thr, two), Y77, nil) :- _____________________one_step(Y77, Q4).
______________one_stepOne_stepCheck(pair(thr, two), Y77, cons(Q5, Q6)) :- _____________________one_stepOne_stepCheck(Y77, Q5, Q6).
_______________one_stepOne_stepCheck(pair(two, one), Y82, nil) :- ______________one_step(Y82, Q1).
_______________one_stepOne_stepCheck(pair(two, one), Y82, cons(Q2, Q3)) :- ______________one_stepOne_stepCheck(Y82, Q2, Q3).
_______________one_stepOne_stepCheck(pair(thr, one), Y82, nil) :- ________________one_step(Y82, Q1).
_______________one_stepOne_stepCheck(pair(thr, one), Y82, cons(Q2, Q3)) :- ________________one_stepOne_stepCheck(Y82, Q2, Q3).
_______________one_stepOne_stepCheck(pair(thr, two), Y82, nil) :- ____________________one_step(Y82, Q4).
_______________one_stepOne_stepCheck(pair(thr, two), Y82, cons(Q5, Q6)) :- ____________________one_stepOne_stepCheck(Y82, Q5, Q6).
________________one_stepOne_stepCheck(pair(one, thr), Y87, nil) :- _______________one_step(Y87, Q1).
________________one_stepOne_stepCheck(pair(one, thr), Y87, cons(Q2, Q3)) :- _______________one_stepOne_stepCheck(Y87, Q2, Q3).
________________one_stepOne_stepCheck(pair(two, thr), Y87, nil) :- _________________one_step(Y87, Q1).
________________one_stepOne_stepCheck(pair(two, thr), Y87, cons(Q2, Q3)) :- _________________one_stepOne_stepCheck(Y87, Q2, Q3).
________________one_stepOne_stepCheck(pair(one, two), Y87, nil) :- ____________________one_step(Y87, Q4).
________________one_stepOne_stepCheck(pair(one, two), Y87, cons(Q5, Q6)) :- ____________________one_stepOne_stepCheck(Y87, Q5, Q6).
_________________one_stepOne_stepCheck(pair(one, two), Y92, nil) :- __________________one_step(Y92, Q1).
_________________one_stepOne_stepCheck(pair(one, two), Y92, cons(Q2, Q3)) :- __________________one_stepOne_stepCheck(Y92, Q2, Q3).
_________________one_stepOne_stepCheck(pair(one, thr), Y92, nil) :- ___________________one_step(Y92, Q1).
_________________one_stepOne_stepCheck(pair(one, thr), Y92, cons(Q2, Q3)) :- ___________________one_stepOne_stepCheck(Y92, Q2, Q3).
_________________one_stepOne_stepCheck(pair(thr, two), Y92, nil) :- ________________one_step(Y92, Q4).
_________________one_stepOne_stepCheck(pair(thr, two), Y92, cons(Q5, Q6)) :- ________________one_stepOne_stepCheck(Y92, Q5, Q6).
__________________one_stepOne_stepCheck(pair(two, one), Y97, nil) :- _________________one_step(Y97, Q1).
__________________one_stepOne_stepCheck(pair(two, one), Y97, cons(Q2, Q3)) :- _________________one_stepOne_stepCheck(Y97, Q2, Q3).
__________________one_stepOne_stepCheck(pair(thr, one), Y97, nil) :- _____________________one_step(Y97, Q1).
__________________one_stepOne_stepCheck(pair(thr, one), Y97, cons(Q2, Q3)) :- _____________________one_stepOne_stepCheck(Y97, Q2, Q3).
__________________one_stepOne_stepCheck(pair(two, thr), Y97, nil) :- ___________________one_step(Y97, Q4).
__________________one_stepOne_stepCheck(pair(two, thr), Y97, cons(Q5, Q6)) :- ___________________one_stepOne_stepCheck(Y97, Q5, Q6).
___________________one_stepOne_stepCheck(pair(two, one), Y102, nil) :- _________________________one_step(Y102, Q1).
___________________one_stepOne_stepCheck(pair(two, one), Y102, cons(Q2, Q3)) :- _________________________one_stepOne_stepCheck(Y102, Q2, Q3).
___________________one_stepOne_stepCheck(pair(thr, one), Y102, nil) :- _________________one_step(Y102, Q1).
___________________one_stepOne_stepCheck(pair(thr, one), Y102, cons(Q2, Q3)) :- _________________one_stepOne_stepCheck(Y102, Q2, Q3).
___________________one_stepOne_stepCheck(pair(thr, two), Y102, nil) :- __________________one_step(Y102, Q4).
___________________one_stepOne_stepCheck(pair(thr, two), Y102, cons(Q5, Q6)) :- __________________one_stepOne_stepCheck(Y102, Q5, Q6).
____________________one_stepOne_stepCheck(pair(two, one), Y107, nil) :- ________________one_step(Y107, Q1).
____________________one_stepOne_stepCheck(pair(two, one), Y107, cons(Q2, Q3)) :- ________________one_stepOne_stepCheck(Y107, Q2, Q3).
____________________one_stepOne_stepCheck(pair(two, thr), Y107, nil) :- _______________one_step(Y107, Q1).
____________________one_stepOne_stepCheck(pair(two, thr), Y107, cons(Q2, Q3)) :- _______________one_stepOne_stepCheck(Y107, Q2, Q3).
_____________________one_stepOne_stepCheck(pair(one, thr), Y112, nil) :- __________________one_step(Y112, Q1).
_____________________one_stepOne_stepCheck(pair(one, thr), Y112, cons(Q2, Q3)) :- __________________one_stepOne_stepCheck(Y112, Q2, Q3).
_____________________one_stepOne_stepCheck(pair(two, thr), Y112, nil) :- ______________one_step(Y112, Q1).
_____________________one_stepOne_stepCheck(pair(two, thr), Y112, cons(Q2, Q3)) :- ______________one_stepOne_stepCheck(Y112, Q2, Q3).
_____________________one_stepOne_stepCheck(pair(two, one), Y112, nil) :- _____________one_step(Y112, Q4).
_____________________one_stepOne_stepCheck(pair(two, one), Y112, cons(Q5, Q6)) :- _____________one_stepOne_stepCheck(Y112, Q5, Q6).
______________________one_stepOne_stepCheck(pair(one, two), Y117, nil) :- ________________________one_step(Y117, Q1).
______________________one_stepOne_stepCheck(pair(one, two), Y117, cons(Q2, Q3)) :- ________________________one_stepOne_stepCheck(Y117, Q2, Q3).
______________________one_stepOne_stepCheck(pair(thr, two), Y117, nil) :- ___________one_step(Y117, Q1).
______________________one_stepOne_stepCheck(pair(thr, two), Y117, cons(Q2, Q3)) :- ___________one_stepOne_stepCheck(Y117, Q2, Q3).
______________________one_stepOne_stepCheck(pair(thr, one), Y117, nil) :- ____________one_step(Y117, Q4).
______________________one_stepOne_stepCheck(pair(thr, one), Y117, cons(Q5, Q6)) :- ____________one_stepOne_stepCheck(Y117, Q5, Q6).
_______________________one_stepOne_stepCheck(pair(thr, one), Y122, nil) :- _________one_step(Y122).
_______________________one_stepOne_stepCheck(pair(thr, one), Y122, cons(Q1, Q2)) :- _________one_stepOne_stepCheck(Y122, Q1, Q2).
_______________________one_stepOne_stepCheck(pair(thr, two), Y122, nil) :- __________one_step(Y122).
_______________________one_stepOne_stepCheck(pair(thr, two), Y122, cons(Q1, Q2)) :- __________one_stepOne_stepCheck(Y122, Q1, Q2).
________________________one_stepOne_stepCheck(pair(two, one), Y127, nil) :- ______________________one_step(Y127, Q1).
________________________one_stepOne_stepCheck(pair(two, one), Y127, cons(Q2, Q3)) :- ______________________one_stepOne_stepCheck(Y127, Q2, Q3).
________________________one_stepOne_stepCheck(pair(thr, one), Y127, nil) :- ________one_step(Y127, Q1).
________________________one_stepOne_stepCheck(pair(thr, one), Y127, cons(Q2, Q3)) :- ________one_stepOne_stepCheck(Y127, Q2, Q3).
________________________one_stepOne_stepCheck(pair(thr, two), Y127, nil) :- _______one_step(Y127, Q4).
________________________one_stepOne_stepCheck(pair(thr, two), Y127, cons(Q5, Q6)) :- _______one_stepOne_stepCheck(Y127, Q5, Q6).
_________________________one_stepOne_stepCheck(pair(one, two), Y132, nil) :- ___________________one_step(Y132, Q1).
_________________________one_stepOne_stepCheck(pair(one, two), Y132, cons(Q2, Q3)) :- ___________________one_stepOne_stepCheck(Y132, Q2, Q3).
_________________________one_stepOne_stepCheck(pair(thr, two), Y132, nil) :- __one_step(Y132, Q1).
_________________________one_stepOne_stepCheck(pair(thr, two), Y132, cons(Q2, Q3)) :- __one_stepOne_stepCheck(Y132, Q2, Q3).
_________________________one_stepOne_stepCheck(pair(thr, one), Y132, nil) :- ___one_step(Y132, Q4).
_________________________one_stepOne_stepCheck(pair(thr, one), Y132, cons(Q5, Q6)) :- ___one_stepOne_stepCheck(Y132, Q5, Q6).
__________________________one_stepOne_stepCheck(pair(one, two), Y137, nil) :- _____one_step(Y137, Q1).
__________________________one_stepOne_stepCheck(pair(one, two), Y137, cons(Q2, Q3)) :- _____one_stepOne_stepCheck(Y137, Q2, Q3).
__________________________one_stepOne_stepCheck(pair(thr, two), Y137, nil) :- _one_step(Y137, Q1).
__________________________one_stepOne_stepCheck(pair(thr, two), Y137, cons(Q2, Q3)) :- _one_stepOne_stepCheck(Y137, Q2, Q3).
__________________________one_stepOne_stepCheck(pair(thr, one), Y137, nil) :- one_step(Y137, Q4).
__________________________one_stepOne_stepCheck(pair(thr, one), Y137, cons(Q5, Q6)) :- one_stepOne_stepCheck(Y137, Q5, Q6).