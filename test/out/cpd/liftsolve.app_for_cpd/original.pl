fail() :- fail().
solve(Rules, nil).
solve(Rules, cons(Ngh, Ngt)) :- non_ground_member(term(clause, cons(Ngh, Ngbody)), Rules), solve(Rules, Ngbody), solve(Rules, Ngt).
non_ground_member(Ngx, cons(Grh, Grt)) :- non_ground_member(Ngx, Grt).
non_ground_member(Ngx, cons(Grh, Grt)) :- make_non_ground(Grh, Ngx).
make_non_ground(G, Ng) :- mkng(G, Ng, nil, Sub).
neq(zero, succ(T)).
neq(succ(T), zero).
neq(succ(T1), succ(T2)) :- neq(T1, T2).
mkng(var(N), Term2, nil, cons(sub(N, Term2), nil)).
mkng(var(N), Term2, cons(sub(N, Term2), T), cons(sub(N, Term2), T)).
mkng(var(N), Term2, cons(sub(M, Y), T), cons(sub(M, Y), T1)) :- neq(N, M), mkng(var(N), Term2, T, T1).
mkng(term(F, Args), term(F, IArgs), InSub, OutSub) :- l_mkng(Args, IArgs, InSub, OutSub).
l_mkng(nil, nil, OutSub, OutSub).
l_mkng(cons(H, T), cons(Ih, It), InSub, OutSub) :- mkng(H, Ih, InSub, IntSub), l_mkng(T, It, IntSub, OutSub).