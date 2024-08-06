evalo(conj(X2, X3), Y1) :- success(), success(), success(), success(), success(), success(), _evalo(Y1, X2, true), success(), _evalo(Y1, X3, true).
evalo(disj(X2, X3), Y1) :- success(), success(), success(), success(), _nando(false, X4), success(), _nando(false, X5), success(), success(), _evalo(Y1, X2, X4), success(), _evalo(Y1, X3, X5).
evalo(disj(X2, X3), Y1) :- success(), success(), success(), success(), _nando(false, X4), success(), _nando(true, X5), success(), success(), _evalo(Y1, X2, X4), success(), _evalo(Y1, X3, X5).
evalo(disj(X2, X3), Y1) :- success(), success(), success(), success(), _nando(true, X4), success(), _nando(false, X5), success(), success(), _evalo(Y1, X2, X4), success(), _evalo(Y1, X3, X5).
evalo(neg(X2), Y1) :- success(), success(), success(), __nando(X4), success(), _evalo(Y1, X2, X4).
evalo(var(X21), cons(pair(X21, true), X23)) :- success(), success().
evalo(var(X6), cons(pair(X21, X22), X23)) :- success(), success(), _assoco(X23, X6).
evalo(lit(true), Y1) :- success().
nando(false, false, true).
nando(false, true, true).
nando(true, false, true).
nando(true, true, false).
_evalo(Y5, conj(X8, X9), true) :- success(), success(), success(), nando(X10, X11, false), success(), success(), _evalo(Y5, X8, X10), success(), _evalo(Y5, X9, X11).
_evalo(Y5, conj(X8, X9), false) :- success(), success(), success(), nando(X10, X11, true), success(), success(), _evalo(Y5, X8, X10), success(), _evalo(Y5, X9, X11).
_evalo(Y5, disj(X8, X9), Y7) :- success(), success(), success(), _nando(X14, X10), success(), _nando(X15, X11), success(), nando(X14, X15, Y7), success(), _evalo(Y5, X8, X10), success(), _evalo(Y5, X9, X11).
_evalo(Y5, neg(X8), Y7) :- success(), success(), _nando(Y7, X10), success(), _evalo(Y5, X8, X10).
_evalo(cons(pair(X16, X17), X18), var(X16), X17) :- success().
_evalo(cons(pair(X16, X17), X18), var(X12), Y7) :- success(), assoco(X18, Y7, X12).
_evalo(Y5, lit(Y7), Y7).
_nando(true, false).
_nando(false, true).
assoco(cons(pair(X16, X17), X18), X17, X16) :- success().
assoco(cons(pair(X16, X17), X18), Y11, Y12) :- success(), assoco(X18, Y11, Y12).
__nando(false).
_assoco(cons(pair(X21, true), X23), X21) :- success().
_assoco(cons(pair(X21, X22), X23), Y15) :- success(), _assoco(X23, Y15).