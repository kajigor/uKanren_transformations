transpose y0 y1 y2 y3 y4 y5 = (fresh q1, q2, q3, q4, q5, q6, q7, q8 in ((y5 == (q1 :: (q2 :: q3)) & y4 == (q4 :: (q5 :: q6)) & y0 == ((y1 :: (q4 :: [q1])) :: ((y2 :: (q5 :: (q2 :: q7))) :: [(y3 :: q8)])) & nullrowsMakerowMakerow q6 q7 q3 q8)));

nullrowsMakerowMakerow y6 y8 y9 y11 = (fresh q1, q2 in ((y11 == (q1 :: [q2]) & y9 == [q2] & y8 == [] & y6 == [q1])));


? transpose x0 x1 x2 x3 x4 x5