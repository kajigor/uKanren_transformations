transpose y0 y1 y2 y3 =
  (fresh q1, q2, q3, q4, q5, q6, q7 in
    (((y3 == [] & y2 == [] & y1 == [] & y0 == []) |
      (y3 == (q1 :: q2) & y2 == (q3 :: q4) & y1 == (q5 :: q6) & y0 == ((q5 :: (q3 :: [q1])) :: q7) & nullrowsMakerowMakerowMakerow q2 q4 q7 q6))));

nullrowsMakerowMakerowMakerow y4 y6 y8 y9 =
  (fresh q1, q2, q3, q4, q5, q6, q7 in
    (((y9 == [] & y8 == [] & y6 == [] & y4 == []) |
      (y9 == (q1 :: q2) & y8 == ((q1 :: (q3 :: [q4])) :: q5) & y6 == (q3 :: q6) & y4 == (q4 :: q7) & nullrowsMakerowMakerowMakerow q7 q6 q5 q2))));


? transpose x0 x1 x2 x3