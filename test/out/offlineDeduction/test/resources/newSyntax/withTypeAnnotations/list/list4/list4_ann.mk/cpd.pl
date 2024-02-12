maxMino(Y0, Y1) :- maxo1(Y0), mino1(Y1).
maxo1(Y2) :- _maxo1(Y2).
_maxo1(succ(succ(zero))).
mino1(Y4) :- _mino1(Y4).
_mino1(zero).