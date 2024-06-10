unsafe y0 = (fresh q1 in (____unsafe q1 (S (O)) O));

___unsafe y5 y6 y7 = (fresh q1, q2 in ((____unsafe q1 (S (y6)) y7 | (y6 == S (q2) & ___unsafe (S (q1)) q2 (S (y7))))));

____unsafe y8 y9 y10 = (fresh q1, q2, q3 in ((___unsafe q1 y9 y10 | (y9 == S (q2) & ____unsafe (S (q3)) q2 (S (y10))))));


? unsafe x0