depth(s(s(s(s(s(s(s(Q1)))))))) :- _depth(Q1).
depth(s(s(s(s(s(s(s(s(s(Q2)))))))))) :- prog_clauseDepth(Q2).
_depth(o).
prog_clauseDepth(Y5) :- _depth(Y5).
prog_clauseDepth(s(s(s(Q1)))) :- _prog_clauseDepth(Q1).
_prog_clauseDepth(Y9) :- _depth(Y9).
_prog_clauseDepth(s(Q1)) :- __prog_clauseDepth(Q1).
__prog_clauseDepth(Y13) :- _depth(Y13).