sorto y0 =
  fresh q1, q2, q3, q4 in
    ((splitoSplitoSortoAppendoAppendo y0 q1 q2 & sorto1 q3 q2 & sorto1 q4 q1));

splitoSplitoSortoAppendoAppendo y1 y4 y9 =
  splitoSplitoSortoAppendoAppendo1 y1 y4 y9;

splitoSplitoSortoAppendoAppendo1 y10 y12 y17 =
  splitoSplitoSortoAppendoAppendo2 y10 y12 y17;

splitoSplitoSortoAppendoAppendo2 y19 y21 y25 =
  splitoSortoAppendoAppendo y19 y21 y25;

splitoSortoAppendoAppendo y28 y30 y34 =
  splitoSortoAppendoAppendo1 y28 y30 y34;

splitoSortoAppendoAppendo1 y35 y37 y40 =
  fresh q1 in
    ((y35 == (Zero :: q1) & sortoAppendoAppendo y37 y40 q1));

sortoAppendoAppendo y42 y43 y47 =
  fresh q1, q2, q3, q4, q5, q6, q7, q8 in
   (((y47 == (Zero :: q1) & appendo1 q1 (Succ Zero) y43 y42) |
   (splitoAppendo q2 q3 q4 q5 q6 q7 & appendoAppendo y42 y43 q2 y47 & sorto1 q5 q6 & sorto1 q8 q7)));

appendoAppendo y51 y52 y53 y55 =
  fresh q1, q2, q3 in
   (((y53 == [] & appendo1 y55 (Succ Zero) ((Zero :: y52)) y51) | (y55 == (q1 :: q2) & y53 == (q1 :: q3) & appendoAppendo y51 y52 q3 q2)));

splitoAppendo y56 y57 y58 y59 y61 y62 =
  fresh q1, q2, q3 in
    (((y59 == [] & y58 == [] & appendo1 y56 y57 y61 y62) | (y59 == (q1 :: q2) &
    splitoAppendo y56 y57 q3 q2 y61 y62 & y58 == (q1 :: q3) & le y57 q1) | (y58 == (q1 :: q3) &
    splitoAppendo y56 y57 q3 y59 y61 y62 & gt y57 q1)));

appendo1 y63 y64 y65 y66 =
  fresh q1, q2, q3 in
    (((y65 == [] & y63 == (y64 :: y66)) | (y65 == (q1 :: q2) & y63 == (q1 :: q3) & appendo1 q3 y64 q2 y66)));

le y67 y68 =
  fresh q1, q2 in
    ((y68 == Zero | (y68 == Succ q1 & y67 == Succ q2 & le q2 q1)));

gt y69 y70 =
  fresh q1, q2, q3 in
    (((y70 == Succ q1 & y69 == Zero) | (y70 == Succ q2 & y69 == Succ q3 & gt q3 q2)));

sorto1 y73 y74 =
  fresh q1, q2, q3, q4, q5, q6 in
    (((y74 == [] & y73 == []) | (y73 == (q1 :: q2) & splitoAppendo y74 q1 q2 q3 q4 q5 & sorto1 q3 q4 & sorto1 q6 q5)));

? sorto x0