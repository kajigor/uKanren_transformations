check(cons(Q1, Q2)) :- one_stepCheck(Q1, Q2).
one_stepCheck(pair(one, Q1), Y2) :- notEqStickGetSetCheck(Y2, Q1).
notEqStickGetSetCheck(Y4, two) :- _check(Y4).
notEqStickGetSetCheck(Y4, thr) :- __________________________check(Y4).
_check(cons(Q1, Q2)) :- _one_stepCheck(Q1, Q2).
_one_stepCheck(pair(Q1, thr), Y9) :- _notEqStickGetSetCheck(Y9, Q1).
_one_stepCheck(pair(Q1, Q2), Y9) :- ______________________notEqStickGetGetLessSetSetCheck(Y9, Q1, Q2).
_notEqStickGetSetCheck(Y11, one) :- __check(Y11).
_notEqStickGetSetCheck(Y11, two) :- __________________________check(Y11).
__check(cons(Q1, Q2)) :- __one_stepCheck(Q1, Q2).
__one_stepCheck(pair(Q1, Q2), Y20) :- notEqStickGetGetLessSetSetCheck(Y20, Q1, Q2).
notEqStickGetGetLessSetSetCheck(Y22, two, one) :- ___check(Y22).
notEqStickGetGetLessSetSetCheck(Y22, two, thr) :- _________________________check(Y22).
notEqStickGetGetLessSetSetCheck(Y22, thr, one) :- _check(Y22).
___check(cons(Q1, Q2)) :- ___one_stepCheck(Q1, Q2).
___one_stepCheck(pair(Q1, two), Y33) :- __notEqStickGetSetCheck(Y33, Q1).
___one_stepCheck(pair(Q1, Q2), Y33) :- ____________________notEqStickGetGetLessSetSetCheck(Y33, Q1, Q2).
__notEqStickGetSetCheck(Y35, one) :- __check(Y35).
__notEqStickGetSetCheck(Y35, thr) :- ____check(Y35).
____check(cons(Q1, Q2)) :- ____one_stepCheck(Q1, Q2).
____one_stepCheck(pair(Q1, thr), Y44) :- ___notEqStickGetSetCheck(Y44, Q1).
____one_stepCheck(pair(Q1, Q2), Y44) :- ___________________notEqStickGetGetLessSetSetCheck(Y44, Q1, Q2).
___notEqStickGetSetCheck(Y46, one) :- _____check(Y46).
___notEqStickGetSetCheck(Y46, two) :- ___check(Y46).
_____check(cons(Q1, Q2)) :- _____one_stepCheck(Q1, Q2).
_____one_stepCheck(pair(Q1, Q2), Y55) :- _notEqStickGetGetLessSetSetCheck(Y55, Q1, Q2).
_notEqStickGetGetLessSetSetCheck(Y57, two, one) :- __________________________check(Y57).
_notEqStickGetGetLessSetSetCheck(Y57, thr, one) :- ____check(Y57).
_notEqStickGetGetLessSetSetCheck(Y57, thr, two) :- ______check(Y57).
______check(cons(Q1, Q2)) :- ______one_stepCheck(Q1, Q2).
______one_stepCheck(pair(Q1, thr), Y68) :- ____notEqStickGetSetCheck(Y68, Q1).
______one_stepCheck(pair(Q1, Q2), Y68) :- __________________notEqStickGetGetLessSetSetCheck(Y68, Q1, Q2).
____notEqStickGetSetCheck(Y70, one) :- _______check(Y70).
____notEqStickGetSetCheck(Y70, two) :- _____check(Y70).
_______check(cons(Q1, Q2)) :- _______one_stepCheck(Q1, Q2).
_______one_stepCheck(pair(Q1, one), Y79) :- _____notEqStickGetSetCheck(Y79, Q1).
_______one_stepCheck(pair(Q1, Q2), Y79) :- _________________notEqStickGetGetLessSetSetCheck(Y79, Q1, Q2).
_____notEqStickGetSetCheck(Y81, two) :- ________check(Y81).
_____notEqStickGetSetCheck(Y81, thr) :- ______check(Y81).
________check(cons(Q1, Q2)) :- ________one_stepCheck(Q1, Q2).
________one_stepCheck(pair(Q1, Q2), Y90) :- __notEqStickGetGetLessSetSetCheck(Y90, Q1, Q2).
__notEqStickGetGetLessSetSetCheck(Y92, one, two) :- _______check(Y92).
__notEqStickGetGetLessSetSetCheck(Y92, one, thr) :- _________check(Y92).
__notEqStickGetGetLessSetSetCheck(Y92, two, thr) :- ________________________check(Y92).
_________check(cons(Q1, Q2)) :- _________one_stepCheck(Q1, Q2).
_________one_stepCheck(pair(Q1, one), Y103) :- ______notEqStickGetSetCheck(Y103, Q1).
_________one_stepCheck(pair(Q1, Q2), Y103) :- _______________notEqStickGetGetLessSetSetCheck(Y103, Q1, Q2).
______notEqStickGetSetCheck(Y105, two) :- __________check(Y105).
______notEqStickGetSetCheck(Y105, thr) :- ________check(Y105).
__________check(cons(Q1, Q2)) :- __________one_stepCheck(Q1, Q2).
__________one_stepCheck(pair(Q1, two), Y114) :- _______notEqStickGetSetCheck(Y114, Q1).
__________one_stepCheck(pair(Q1, Q2), Y114) :- ______________notEqStickGetGetLessSetSetCheck(Y114, Q1, Q2).
_______notEqStickGetSetCheck(Y116, one) :- _________check(Y116).
_______notEqStickGetSetCheck(Y116, thr) :- ___________check(Y116).
___________check(cons(Q1, Q2)) :- ___________one_stepCheck(Q1, Q2).
___________one_stepCheck(pair(Q1, Q2), Y125) :- ___notEqStickGetGetLessSetSetCheck(Y125, Q1, Q2).
___notEqStickGetGetLessSetSetCheck(Y127, one, thr) :- ____________check(Y127).
___notEqStickGetGetLessSetSetCheck(Y127, two, one) :- ______________check(Y127).
___notEqStickGetGetLessSetSetCheck(Y127, two, thr) :- __________check(Y127).
____________check(cons(Q1, Q2)) :- ____________one_stepCheck(Q1, Q2).
____________one_stepCheck(pair(Q1, one), Y138) :- ________notEqStickGetSetCheck(Y138, Q1).
____________one_stepCheck(pair(Q1, Q2), Y138) :- ____notEqStickGetGetLessSetSetCheck(Y138, Q1, Q2).
________notEqStickGetSetCheck(Y140, two) :- ________________________check(Y140).
________notEqStickGetSetCheck(Y140, thr) :- ___________check(Y140).
____notEqStickGetGetLessSetSetCheck(Y147, two, thr) :- _____________check(Y147).
_____________check(nil).
_____________check(cons(Q1, Q2)) :- _____________one_stepCheck(Q1, Q2).
_____________one_stepCheck(pair(thr, Q1), Y158) :- _________notEqStickGetSetCheck(Y158, Q1).
_________notEqStickGetSetCheck(Y160, one) :- ________________________check(Y160).
_________notEqStickGetSetCheck(Y160, two) :- ____________check(Y160).
______________check(cons(Q1, Q2)) :- ______________one_stepCheck(Q1, Q2).
______________one_stepCheck(pair(Q1, two), Y165) :- __________notEqStickGetSetCheck(Y165, Q1).
______________one_stepCheck(pair(Q1, Q2), Y165) :- _____________notEqStickGetGetLessSetSetCheck(Y165, Q1, Q2).
__________notEqStickGetSetCheck(Y167, one) :- ___________check(Y167).
__________notEqStickGetSetCheck(Y167, thr) :- _______________check(Y167).
_______________check(cons(Q1, Q2)) :- _______________one_stepCheck(Q1, Q2).
_______________one_stepCheck(pair(Q1, thr), Y176) :- ___________notEqStickGetSetCheck(Y176, Q1).
_______________one_stepCheck(pair(Q1, Q2), Y176) :- ____________notEqStickGetGetLessSetSetCheck(Y176, Q1, Q2).
___________notEqStickGetSetCheck(Y178, one) :- ________________check(Y178).
___________notEqStickGetSetCheck(Y178, two) :- ______________check(Y178).
________________check(cons(Q1, Q2)) :- ________________one_stepCheck(Q1, Q2).
________________one_stepCheck(pair(Q1, Q2), Y187) :- _____notEqStickGetGetLessSetSetCheck(Y187, Q1, Q2).
_____notEqStickGetGetLessSetSetCheck(Y189, one, two) :- _________________check(Y189).
_____notEqStickGetGetLessSetSetCheck(Y189, thr, one) :- _______________check(Y189).
_____notEqStickGetGetLessSetSetCheck(Y189, thr, two) :- _______________________check(Y189).
_________________check(cons(Q1, Q2)) :- _________________one_stepCheck(Q1, Q2).
_________________one_stepCheck(pair(Q1, one), Y200) :- ____________notEqStickGetSetCheck(Y200, Q1).
_________________one_stepCheck(pair(Q1, Q2), Y200) :- __________notEqStickGetGetLessSetSetCheck(Y200, Q1, Q2).
____________notEqStickGetSetCheck(Y202, two) :- ________________check(Y202).
____________notEqStickGetSetCheck(Y202, thr) :- __________________check(Y202).
__________________check(cons(Q1, Q2)) :- __________________one_stepCheck(Q1, Q2).
__________________one_stepCheck(pair(Q1, thr), Y211) :- _____________notEqStickGetSetCheck(Y211, Q1).
__________________one_stepCheck(pair(Q1, Q2), Y211) :- _________notEqStickGetGetLessSetSetCheck(Y211, Q1, Q2).
_____________notEqStickGetSetCheck(Y213, one) :- _________________check(Y213).
_____________notEqStickGetSetCheck(Y213, two) :- ___________________check(Y213).
___________________check(cons(Q1, Q2)) :- ___________________one_stepCheck(Q1, Q2).
___________________one_stepCheck(pair(Q1, Q2), Y222) :- ______notEqStickGetGetLessSetSetCheck(Y222, Q1, Q2).
______notEqStickGetGetLessSetSetCheck(Y224, one, two) :- ____________________check(Y224).
______notEqStickGetGetLessSetSetCheck(Y224, one, thr) :- _____________________check(Y224).
______notEqStickGetGetLessSetSetCheck(Y224, thr, two) :- __________________check(Y224).
____________________check(cons(Q1, Q2)) :- ____________________one_stepCheck(Q1, Q2).
____________________one_stepCheck(pair(Q1, one), Y235) :- ______________notEqStickGetSetCheck(Y235, Q1).
____________________one_stepCheck(pair(Q1, Q2), Y235) :- _______notEqStickGetGetLessSetSetCheck(Y235, Q1, Q2).
______________notEqStickGetSetCheck(Y237, two) :- ___________________check(Y237).
______________notEqStickGetSetCheck(Y237, thr) :- _______________________check(Y237).
_______notEqStickGetGetLessSetSetCheck(Y244, two, thr) :- _____________________check(Y244).
_____________________check(cons(Q1, Q2)) :- _____________________one_stepCheck(Q1, Q2).
_____________________one_stepCheck(pair(Q1, one), Y255) :- _______________notEqStickGetSetCheck(Y255, Q1).
_____________________one_stepCheck(pair(Q1, Q2), Y255) :- ________notEqStickGetGetLessSetSetCheck(Y255, Q1, Q2).
_______________notEqStickGetSetCheck(Y257, two) :- _________________________check(Y257).
_______________notEqStickGetSetCheck(Y257, thr) :- ___________________check(Y257).
________notEqStickGetGetLessSetSetCheck(Y264, thr, two) :- ____________________check(Y264).
_________notEqStickGetGetLessSetSetCheck(Y273, one, two) :- ______________________check(Y273).
______________________check(cons(Q1, Q2)) :- ______________________one_stepCheck(Q1, Q2).
______________________one_stepCheck(pair(two, Q1), Y284) :- ________________notEqStickGetSetCheck(Y284, Q1).
________________notEqStickGetSetCheck(Y286, one) :- __________________check(Y286).
________________notEqStickGetSetCheck(Y286, thr) :- _________________check(Y286).
__________notEqStickGetGetLessSetSetCheck(Y289, thr, two) :- ______________________check(Y289).
_______________________check(cons(Q1, Q2)) :- _______________________one_stepCheck(Q1, Q2).
_______________________one_stepCheck(pair(Q1, thr), Y300) :- _________________notEqStickGetSetCheck(Y300, Q1).
_______________________one_stepCheck(pair(Q1, Q2), Y300) :- ___________notEqStickGetGetLessSetSetCheck(Y300, Q1, Q2).
_________________notEqStickGetSetCheck(Y302, one) :- ____________________check(Y302).
_________________notEqStickGetSetCheck(Y302, two) :- ________________check(Y302).
___________notEqStickGetGetLessSetSetCheck(Y309, two, one) :- _______________check(Y309).
____________notEqStickGetGetLessSetSetCheck(Y318, one, two) :- _______________________check(Y318).
_____________notEqStickGetGetLessSetSetCheck(Y327, one, thr) :- __________check(Y327).
______________notEqStickGetGetLessSetSetCheck(Y336, thr, one) :- ______________check(Y336).
_______________notEqStickGetGetLessSetSetCheck(Y345, thr, two) :- _______check(Y345).
________________________check(cons(Q1, Q2)) :- ________________________one_stepCheck(Q1, Q2).
________________________one_stepCheck(pair(Q1, two), Y356) :- __________________notEqStickGetSetCheck(Y356, Q1).
________________________one_stepCheck(pair(Q1, Q2), Y356) :- ________________notEqStickGetGetLessSetSetCheck(Y356, Q1, Q2).
__________________notEqStickGetSetCheck(Y358, one) :- ____________check(Y358).
__________________notEqStickGetSetCheck(Y358, thr) :- ________check(Y358).
________________notEqStickGetGetLessSetSetCheck(Y365, one, thr) :- _____________check(Y365).
_________________notEqStickGetGetLessSetSetCheck(Y374, two, thr) :- _________check(Y374).
__________________notEqStickGetGetLessSetSetCheck(Y383, two, one) :- ____check(Y383).
___________________notEqStickGetGetLessSetSetCheck(Y392, one, two) :- ______check(Y392).
____________________notEqStickGetGetLessSetSetCheck(Y401, one, thr) :- _________________________check(Y401).
_________________________check(cons(Q1, Q2)) :- _________________________one_stepCheck(Q1, Q2).
_________________________one_stepCheck(pair(Q1, two), Y412) :- ___________________notEqStickGetSetCheck(Y412, Q1).
_________________________one_stepCheck(pair(Q1, Q2), Y412) :- _____________________notEqStickGetGetLessSetSetCheck(Y412, Q1, Q2).
___________________notEqStickGetSetCheck(Y414, one) :- _____________________check(Y414).
___________________notEqStickGetSetCheck(Y414, thr) :- __check(Y414).
_____________________notEqStickGetGetLessSetSetCheck(Y421, thr, one) :- ___check(Y421).
______________________notEqStickGetGetLessSetSetCheck(Y430, two, one) :- check(Y430).
__________________________check(cons(Q1, Q2)) :- __________________________one_stepCheck(Q1, Q2).
__________________________one_stepCheck(pair(Q1, two), Y441) :- ____________________notEqStickGetSetCheck(Y441, Q1).
__________________________one_stepCheck(pair(Q1, Q2), Y441) :- _______________________notEqStickGetGetLessSetSetCheck(Y441, Q1, Q2).
____________________notEqStickGetSetCheck(Y443, one) :- _____check(Y443).
____________________notEqStickGetSetCheck(Y443, thr) :- _check(Y443).
_______________________notEqStickGetGetLessSetSetCheck(Y450, thr, one) :- check(Y450).