containso y0 =
  fresh q1,q2,q3,q4 in
    (((y0 == (S O :: q1 :: q2) & newoCono q1 q2) | (y0 == (q3 :: q4) & cono1 q4)));

newoCono y3 y4 =
  fresh q1, q2 in
    (((y4 == (q1 :: q2) & y3 == O & newoCono1 q1 q2) | appendo2Appendo2Appendo1Cono2 y3 y4));

newoCono1 y7 y8 = ((y7 == S (S O) & cono y8) | appendo2Appendo2Appendo1Cono y7 y8);
cono y9 = y9 == y9;

appendo2Appendo2Appendo1Cono y12 y13 =
  (cono1 y13 | appendo2Appendo2Appendo1Cono1 y12 y13);

appendo2Appendo2Appendo1Cono1 y17 y18 =
  fresh q1, q2 in
    ((y18 == (q1 :: q2) & newoCono q1 q2 & appendo2 y17));

appendo2 y22 = y22 == S O;

appendo2Appendo2Appendo1Cono2 y26 y27 =
  fresh q1, q2 in
    ((cono1 y27 | (y27 == (q1 :: q2) & newoCono q1 q2 & appendo2 y26)));

cono1 y30 =
  fresh q1, q2, q3, q4 in
    (((y30 == (S O :: q1 :: q2) & newoCono q1 q2) | (y30 == (q3 :: q4) & cono1 q4)));

? containso x0