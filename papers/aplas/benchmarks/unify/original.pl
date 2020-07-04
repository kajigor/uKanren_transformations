eq_nat(z, z, true).
eq_nat(s(Q39), z, false).
eq_nat(z, s(Q41), false).
eq_nat(s(X), s(Y), Q36) :- eq_nat(X, Y, Q36).
get_term(Var, nil, none).
get_term(z, cons(Q32, Xs), Q32).
get_term(s(N), cons(X, Xs), Q32) :- get_term(N, Xs, Q32).
check_uni(Subst, constr(N1, A1), constr(N2, A2), false) :- eq_nat(N1, N2, false), forall2(Subst, A1, A2, Q13).
check_uni(Subst, constr(N1, A1), constr(N2, A2), Q13) :- eq_nat(N1, N2, true), forall2(Subst, A1, A2, Q13).
check_uni(Subst, var_(V), constr(N, A), false) :- get_term(V, Subst, none).
check_uni(Subst, var_(V), constr(N, A), Q31) :- get_term(V, Subst, some(T)), check_uni(Subst, T, constr(N, A), Q31).
check_uni(Subst, constr(N, A), var_(V), false) :- get_term(V, Subst, none).
check_uni(Subst, constr(N, A), var_(V), Q31) :- get_term(V, Subst, some(T)), check_uni(Subst, constr(N, A), T, Q31).
check_uni(Subst, var_(V1), var_(V2), Q31) :- get_term(V1, Subst, some(T11)), check_uni(Subst, T11, var_(V2), Q31).
check_uni(Subst, var_(V1), var_(V2), false) :- get_term(V1, Subst, none), get_term(V2, Subst, some(Q28)).
check_uni(Subst, var_(V1), var_(V2), Q31) :- get_term(V1, Subst, none), get_term(V2, Subst, none), eq_nat(V1, V2, Q31).
get_term(Var, nil, none).
get_term(z, cons(Q32, Xs), Q32).
get_term(s(N), cons(X, Xs), Q32) :- get_term(N, Xs, Q32).
eq_nat(z, z, true).
eq_nat(s(Q39), z, false).
eq_nat(z, s(Q41), false).
eq_nat(s(X), s(Y), Q36) :- eq_nat(X, Y, Q36).
forall2(Subst, nil, nil, true).
forall2(Subst, cons(X, Xs), cons(Y, Ys), false) :- check_uni(Subst, X, Y, false), forall2(Subst, Xs, Ys, Q4).
forall2(Subst, cons(X, Xs), cons(Y, Ys), Q4) :- check_uni(Subst, X, Y, true), forall2(Subst, Xs, Ys, Q4).
forall2(Subst, nil, nil, true).
forall2(Subst, cons(X, Xs), cons(Y, Ys), false) :- check_uni(Subst, X, Y, false), forall2(Subst, Xs, Ys, Q4).
forall2(Subst, cons(X, Xs), cons(Y, Ys), Q4) :- check_uni(Subst, X, Y, true), forall2(Subst, Xs, Ys, Q4).
check_uni(Subst, constr(N1, A1), constr(N2, A2), false) :- eq_nat(N1, N2, false), forall2(Subst, A1, A2, Q13).
check_uni(Subst, constr(N1, A1), constr(N2, A2), Q13) :- eq_nat(N1, N2, true), forall2(Subst, A1, A2, Q13).
check_uni(Subst, var_(V), constr(N, A), false) :- get_term(V, Subst, none).
check_uni(Subst, var_(V), constr(N, A), Q31) :- get_term(V, Subst, some(T)), check_uni(Subst, T, constr(N, A), Q31).
check_uni(Subst, constr(N, A), var_(V), false) :- get_term(V, Subst, none).
check_uni(Subst, constr(N, A), var_(V), Q31) :- get_term(V, Subst, some(T)), check_uni(Subst, constr(N, A), T, Q31).
check_uni(Subst, var_(V1), var_(V2), Q31) :- get_term(V1, Subst, some(T11)), check_uni(Subst, T11, var_(V2), Q31).
check_uni(Subst, var_(V1), var_(V2), false) :- get_term(V1, Subst, none), get_term(V2, Subst, some(Q28)).
check_uni(Subst, var_(V1), var_(V2), Q31) :- get_term(V1, Subst, none), get_term(V2, Subst, none), eq_nat(V1, V2, Q31).
get_term(Var, nil, none).
get_term(z, cons(Q32, Xs), Q32).
get_term(s(N), cons(X, Xs), Q32) :- get_term(N, Xs, Q32).
eq_nat(z, z, true).
eq_nat(s(Q39), z, false).
eq_nat(z, s(Q41), false).
eq_nat(s(X), s(Y), Q36) :- eq_nat(X, Y, Q36).
forall2(Subst, nil, nil, true).
forall2(Subst, cons(X, Xs), cons(Y, Ys), false) :- check_uni(Subst, X, Y, false), forall2(Subst, Xs, Ys, Q4).
forall2(Subst, cons(X, Xs), cons(Y, Ys), Q4) :- check_uni(Subst, X, Y, true), forall2(Subst, Xs, Ys, Q4).