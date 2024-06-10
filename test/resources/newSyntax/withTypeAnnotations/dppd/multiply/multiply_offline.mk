multiplydds y0 y1 = (fresh q1, q2, q3, q4, q5 in (((y1 == S (S (q1)) & y0 == O & multiplysds q1) | (y1 == S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (q2)))))))))))))))) & y0 == S (O) & _multiplysds q2) | (y1 == S (S (S (S (S (S (S (S (q3)))))))) & y0 == S (S (O)) & __multiplysds q3) | (y1 == S (S (S (S (q4)))) & y0 == S (S (S (S (O)))) & ___multiplysds q4) | (y1 == S (S (q5)) & y0 == S (S (S (S (S (S (S (S (O)))))))) & ____multiplysds q5) | (y1 == S (O) & y0 == S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (O))))))))))))))))))));

multiplysds y2 = (fresh q1 in ((y2 == S (q1) & multiplysds q1)));

_multiplysds y3 = y3 == O;

__multiplysds y4 = y4 == O;

___multiplysds y5 = y5 == O;

____multiplysds y6 = y6 == O;


? multiplydds x0 x1