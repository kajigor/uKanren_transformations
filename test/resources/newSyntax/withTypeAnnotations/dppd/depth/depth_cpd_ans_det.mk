depth y0 =
  fresh q1, q2 in
    (((y0 == S (S (S (S (S (S (S q1)))))) & depth1 q1) |
      (y0 == S (S (S (S (S (S (S (S (S q2)))))))) & prog_clauseDepth q2)));

depth1 y1 =
  y1 == O;

prog_clauseDepth y5 =
  fresh q1 in
    ((depth1 y5 | (y5 == S (S (S q1)) & prog_clauseDepth1 q1)));

prog_clauseDepth1 y9 =
  fresh q1 in
    ((depth1 y9 | (y9 == S q1 & prog_clauseDepth2 q1)));

prog_clauseDepth2 y13 = depth1 y13;

? depth x0