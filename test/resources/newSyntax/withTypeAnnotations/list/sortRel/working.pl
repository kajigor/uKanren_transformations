sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- fn1o(Q1, Q2, Q3, Q4).
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- fn2o(Q1, Q2, Q3, Q4).
fn1o(Y2, Y5, Y8, Y9) :- fn3o(Q1, Y5, Y8, Y9), fn9o(Y2, Q1).
fn1o(Y2, Y5, Y8, Y9) :- fn10o(Q1, Y5, Y8, Y9), fn13o(Y2, Q1).
fn3o(Y11, Y12, Y15, Y16) :- fn4o(Y11, Y12, Y15, Y16).
fn3o(Y11, Y12, Y15, Y16) :- fn7o(Y11, Y12, Y15, Y16).
fn4o(zero, zero, Y21, Y22) :- fn5o(Y21, Y22).
fn4o(succ(zero), succ(zero), Y21, Y22) :- fn5o(Y21, Y22).
fn4o(zero, succ(zero), Y21, Y22) :- fn6o(Y21, Y22).
fn5o(succ(zero), succ(succ(zero))).
fn5o(succ(succ(zero)), succ(zero)).
fn6o(zero, succ(succ(zero))).
fn6o(succ(succ(zero)), zero).
fn7o(zero, succ(succ(zero)), Y30, Y31) :- fn13o(Y30, Y31).
fn7o(succ(zero), succ(succ(zero)), Y30, Y31) :- fn8o(Y30, Y31).
fn8o(succ(zero), succ(zero)).
fn9o(zero, zero).
fn10o(Y36, Y37, Y40, Y41) :- fn11o(Y36, Y37, Y40, Y41).
fn10o(Y36, Y37, Y40, Y41) :- fn12o(Y36, Y37, Y40, Y41).
fn11o(zero, zero, Y46, Y47) :- fn6o(Y46, Y47).
fn12o(zero, succ(succ(zero)), Y51, Y52) :- fn9o(Y51, Y52).
fn13o(zero, succ(zero)).
fn13o(succ(zero), zero).
fn2o(Y56, Y59, Y62, Y63) :- fn14o(Q1, Y59, Y62, Y63), fn6o(Y56, Q1).
fn14o(Y65, Y66, Y69, Y70) :- fn15o(Y65, Y66, Y69, Y70).
fn14o(Y65, Y66, Y69, Y70) :- fn16o(Y65, Y66, Y69, Y70).
fn15o(zero, zero, Y75, Y76) :- fn13o(Y75, Y76).
fn16o(zero, succ(zero), Y80, Y81) :- fn9o(Y80, Y81).