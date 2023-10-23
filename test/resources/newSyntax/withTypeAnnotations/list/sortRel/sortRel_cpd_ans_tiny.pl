fail() :- fail().
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Q1, Q2, Q3, Q4).
sorto(cons(Q1, cons(Q2, cons(Q3, cons(Q4, nil))))) :- fn5(Q1, Q2, Q3, Q4).
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y2, Y5, Y8, Y9) :- minmaxoMinmaxoMinmaxo(Q1, Y5, Y8, Y9), fn12(Y2, Q1).
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo(Y2, Y5, Y8, Y9) :- fn11(Q1, Y5, Y8, Y9), fn6(Y2, Q1).
minmaxoMinmaxoMinmaxo(Y11, Y12, Y15, Y16) :- minmaxoMinmaxo(Y11, Y12, Y15, Y16).
minmaxoMinmaxoMinmaxo(Y11, Y12, Y15, Y16) :- fn18(Y11, Y12, Y15, Y16).
minmaxoMinmaxo(Y19, Y19, Y21, Y22) :- leo(Y19), minmaxo(Y21, Y22).
minmaxoMinmaxo(Y18, succ(zero), Y21, Y22) :- fn20(Y18, Y21, Y22).
leo(zero).
leo(succ(zero)).
minmaxo(succ(zero), succ(succ, zero)) :- leo(zero).
minmaxo(succ(succ, zero), succ(zero)).
fn20(zero, Y27, Y28) :- fn19(Y27, Y28).
fn19(zero, succ(succ, zero)).
fn19(succ(succ, zero), zero).
fn18(Y32, Y32, Y34, Y35) :- fn17(Y32), fn16(Y34, Y35).
fn18(Y31, succ(succ, zero), Y34, Y35) :- fn15(Y31, Y34, Y35).
fn17(zero).
fn17(succ(Q1)) :- leo(Q1).
fn16(succ(succ, zero), succ(zero)) :- leo(succ(succ, zero)).
fn15(zero, Y40, Y41) :- fn6(Y40, Y41).
fn15(succ(Q1), Y40, Y41) :- fn14(Y40, Y41, Q1).
fn14(Y42, Y43, zero) :- fn13(Y42, Y43).
fn13(succ(zero), succ(zero)) :- leo(succ(zero)).
fn12(zero, zero).
fn11(Y49, Y50, Y53, Y54) :- fn10(Y49, Y50, Y53, Y54).
fn11(Y49, Y50, Y53, Y54) :- fn9(Y49, Y50, Y53, Y54).
fn10(zero, zero, Y59, Y60) :- fn19(Y59, Y60).
fn9(Y61, succ(succ, zero), Y64, Y65) :- fn8(Y61, Y64, Y65).
fn8(zero, Y67, Y68) :- fn12(Y67, Y68).
fn6(zero, succ(zero)) :- leo(zero).
fn6(succ(zero), zero).
fn5(Y75, Y78, Y81, Y82) :- fn4(Q1, Y78, Y81, Y82), fn19(Y75, Q1).
fn4(Y84, Y85, Y88, Y89) :- fn3(Y84, Y85, Y88, Y89).
fn4(Y84, Y85, Y88, Y89) :- fn2(Y84, Y85, Y88, Y89).
fn3(zero, zero, Y94, Y95) :- fn6(Y94, Y95).
fn2(Y96, succ(zero), Y99, Y100) :- fn1(Y96, Y99, Y100).
fn1(zero, Y102, Y103) :- fn12(Y102, Y103).