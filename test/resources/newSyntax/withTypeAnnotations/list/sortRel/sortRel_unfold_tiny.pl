fail() :- fail().
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- fn1(Q1, Q2, Q3, Q4).
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- fn2(Q1, Q2, Q3, Q4).
fn1(Y2, Y5, Y8, Y9) :- fn3(Q1, Y5, Y8, Y9), fn9(Y2, Q1).
fn1(Y2, Y5, Y8, Y9) :- fn10(Q1, Y5, Y8, Y9), fn13(Y2, Q1).
fn3(Y11, Y12, Y15, Y16) :- fn4(Y11, Y12, Y15, Y16).
fn3(Y11, Y12, Y15, Y16) :- fn7(Y11, Y12, Y15, Y16).
fn4(zero, zero, Y21, Y22) :- fn5(Y21, Y22).
fn4(succ(zero), succ(zero), Y21, Y22) :- fn5(Y21, Y22).
fn4(zero, succ(zero), Y21, Y22) :- fn6(Y21, Y22).
fn5(succ(zero), succ(succ(zero))).
fn5(succ(succ(zero)), succ(zero)).
fn6(zero, succ(succ(zero))).
fn6(succ(succ(zero)), zero).
fn7(zero, succ(succ(zero)), Y30, Y31) :- fn13(Y30, Y31).
fn7(succ(zero), succ(succ(zero)), Y30, Y31) :- fn8(Y30, Y31).
fn8(succ(zero), succ(zero)).
fn9(zero, zero).
fn10(Y36, Y37, Y40, Y41) :- fn11(Y36, Y37, Y40, Y41).
fn10(Y36, Y37, Y40, Y41) :- fn12(Y36, Y37, Y40, Y41).
fn11(zero, zero, Y46, Y47) :- fn6(Y46, Y47).
fn12(zero, succ(succ(zero)), Y51, Y52) :- fn9(Y51, Y52).
fn13(zero, succ(zero)).
fn13(succ(zero), zero).
fn2(Y56, Y59, Y62, Y63) :- fn14(Q1, Y59, Y62, Y63), fn6(Y56, Q1).
fn14(Y65, Y66, Y69, Y70) :- fn15(Y65, Y66, Y69, Y70).
fn14(Y65, Y66, Y69, Y70) :- fn16(Y65, Y66, Y69, Y70).
fn15(zero, zero, Y75, Y76) :- fn13(Y75, Y76).
fn16(zero, succ(zero), Y80, Y81) :- fn9(Y80, Y81).