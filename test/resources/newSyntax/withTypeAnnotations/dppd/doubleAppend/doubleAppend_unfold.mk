double_apppendo y0 y1 y2 y3 =
  fresh q1 q2 q3
    (((y0 = [] /\ appendo y2 y3 y1) \/
    (y3 = (q1 :: q2) /\ y0 = (q1 :: q3) /\
    appendoAppendo y1 y2 q3 q2)))

appendo y4 y5 y6 =
  fresh q1 q2 q3
    (((y6 = [] /\ y4 = y5) \/
    (y6 = (q1 :: q2) /\ y5 = (q1 :: q3) /\ appendo y4 q3 q2)))

appendoAppendo y7 y8 y9 y11 =
  fresh q1 q2 q3
    (((y9 = [] /\ appendo y8 y11 y7) \/
    (y11 = (q1 :: q2) /\ y9 = (q1 :: q3) /\
    appendoAppendo y7 y8 q3 q2)))

double_apppendo x0 x1 x2 x3