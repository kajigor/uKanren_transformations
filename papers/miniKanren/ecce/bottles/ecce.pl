checkAnswer0(A,B,C,true) :- 
    checkAnswer0__1(A,B,C).
checkAnswer0__1([],A,o).
checkAnswer0__1([pair(A,B)|C],D,E) :- 
    checkStep_conj__2(A,B,D,C,E).
checkStep_conj__2(fill,fst,pair(A,B),C,D) :- 
    checkAnswer__4(A,o,C,A,B,D).
checkStep_conj__2(fill,snd,pair(A,B),C,D) :- 
    checkAnswer__4(o,B,C,A,B,D).
checkStep_conj__2(empty,fst,pair(o,A),B,C) :- 
    checkAnswer__3(B,o,A,C).
checkStep_conj__2(empty,snd,pair(A,o),B,C) :- 
    checkAnswer__3(B,A,o,C).
checkAnswer__3([],A,B,o).
checkAnswer__3([pair(A,B)|C],D,E,F) :- 
    checkStep_conj__2(A,B,pair(D,E),C,F).
checkAnswer__4(A,B,[],C,D,E) :- 
    isFinishState__5(A,B,E).
checkAnswer__4(A,B,[pair(C,D)|E],F,G,H) :- 
    checkStep_conj__6(A,B,C,D,F,G,E,H).
isFinishState__5(A,B,C) :- 
    fancyEq__16(A,C), 
    fancyEq__18(B,C).
isFinishState__5(A,B,C) :- 
    fancyEq_conj__17(A,C,B).
checkStep_conj__6(o,A,fill,fst,B,C,D,E) :- 
    checkAnswer__4(B,A,D,B,C,E).
checkStep_conj__6(A,o,fill,snd,B,C,D,E) :- 
    checkAnswer__4(A,C,D,B,C,E).
checkStep_conj__6(A,B,empty,fst,C,D,E,F) :- 
    fancyEq__16(A,C), 
    checkAnswer__4(o,B,E,C,D,F).
checkStep_conj__6(A,B,empty,snd,C,D,E,F) :- 
    fancyEq__16(B,D), 
    checkAnswer__4(A,o,E,C,D,F).
checkStep_conj__6(s(A),B,pour,fst,C,D,E,F) :- 
    fancyEq__7(B,D), 
    doStep_conj__15(A,B,C,D,E,F).
checkStep_conj__6(A,s(B),pour,snd,C,D,E,F) :- 
    fancyEq__7(A,C), 
    doStep_conj__8(A,B,C,D,E,F).
fancyEq__7(o,s(A)).
fancyEq__7(s(A),o).
fancyEq__7(s(A),s(B)) :- 
    fancyEq__7(A,B).
doStep_conj__8(A,B,C,D,E,F) :- 
    add__12(A,s(B),s(G)), 
    greater__11(G,C), 
    add__12(A,s(B),s(H)), 
    sub_conj__13(H,C,I,C,I,E,C,D,F).
doStep_conj__8(A,B,s(C),D,E,F) :- 
    add_conj__9(A,s(B),s(G),G,C,s(H)), 
    checkAnswer__4(s(H),o,E,s(C),D,F).
add_conj__9(o,A,A,B,C,A) :- 
    greater__10(B,C).
add_conj__9(s(A),B,s(C),s(D),s(E),s(F)) :- 
    add_conj__9(A,B,C,D,E,F).
greater__10(o,A).
greater__10(s(A),s(B)) :- 
    greater__10(A,B).
greater__11(A,o).
greater__11(s(A),s(B)) :- 
    greater__11(A,B).
add__12(o,A,A).
add__12(s(A),B,s(C)) :- 
    add__12(A,B,C).
sub_conj__13(A,o,s(A),B,C,D,E,F,G) :- 
    checkAnswer__4(B,C,D,E,F,G).
sub_conj__13(A,s(B),C,D,E,F,G,H,I) :- 
    sub_conj__14(A,B,C,D,E,F,G,H,I).
sub_conj__14(A,o,A,B,C,D,E,F,G) :- 
    checkAnswer__4(B,C,D,E,F,G).
sub_conj__14(o,s(A),o,B,C,D,E,F,G) :- 
    checkAnswer__4(B,C,D,E,F,G).
sub_conj__14(s(A),s(B),C,D,E,F,G,H,I) :- 
    sub_conj__14(A,B,C,D,E,F,G,H,I).
doStep_conj__15(A,B,C,D,E,F) :- 
    add__12(A,B,G), 
    greater__11(G,D), 
    add__12(A,B,H), 
    sub_conj__13(H,D,I,I,D,E,C,D,F).
doStep_conj__15(A,B,C,s(D),E,F) :- 
    add_conj__9(A,B,G,G,D,H), 
    checkAnswer__4(o,s(H),E,C,s(D),F).
fancyEq__16(o,o).
fancyEq__16(s(A),s(B)) :- 
    fancyEq__16(A,B).
fancyEq_conj__17(o,s(A),s(B)) :- 
    fancyEq__16(B,A).
fancyEq_conj__17(s(A),o,o).
fancyEq_conj__17(s(A),s(B),s(C)) :- 
    fancyEq_conj__17(A,B,C).
fancyEq__18(o,o).
fancyEq__18(o,s(A)).
fancyEq__18(s(A),o).
fancyEq__18(s(A),s(B)) :- 
    fancyEq__18(A,B).

capacities1(pair(s(s(s(s(o)))), s(s(s(s(s(s(s(s(s(o))))))))))).


topLevel(Res) :- capacities1(Cap), checkAnswer0(Res, Cap, s(s(s(s(s(s(s(o))))))),true).
