depth y0 = fresh q1 q2 (((y0 = (7 + q1) /\ _depth q1) \/ (y0 = (9 + q2) /\ prog_clauseDepth q2)))
_depth y1 = y1 = 0
prog_clauseDepth y5 = fresh q1 ((_depth y5 \/ (y5 = (3 + q1) /\ _prog_clauseDepth q1)))
_prog_clauseDepth y9 = fresh q1 ((_depth y9 \/ (y9 = (1 + q1) /\ __prog_clauseDepth q1)))
__prog_clauseDepth y13 = _depth y13

depth x0