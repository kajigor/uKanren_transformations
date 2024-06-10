sumsquaresupto(Y0) :- _uptoMultiplySquaresAddSum1(Y0, o, o).
_uptoMultiplySquaresAddSum1(Y7, o, Y13) :- uptoSquaresSum1(Y7, Y13).
_uptoMultiplySquaresAddSum1(Y7, s(Q1), Y13) :- leUptoAddAddAddMultiplySquaresAddSum1(Y7, Y13, Q1).
uptoSquaresSum1(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(Y16)))))))))))))))))))))))))))))), Y16).
uptoSquaresSum1(Y15, Y16) :- _uptoMultiplySquaresAddSum1(Y15, s(s(s(o))), s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(Y16)))))))))))))))))))))))))))))).
leUptoAddAddAddMultiplySquaresAddSum1(Y19, Y21, o) :- _uptoSquaresSum1(Y19, s(s(s(s(s(s(s(s(s(Y21)))))))))).
leUptoAddAddAddMultiplySquaresAddSum1(Y19, Y21, s(o)) :- _uptoSquaresSum1(Y19, Y21).
_uptoSquaresSum1(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(s(Y31))))))))))))))))), Y31).