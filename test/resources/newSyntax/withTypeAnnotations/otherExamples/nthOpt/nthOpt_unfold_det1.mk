nthOpt y0 y1 =
  (fresh q1, q2, q3, q4, q5 in
    (((y1 == None & y0 == []) |
      (y1 == None & y0 == [q1]) |
      (y1 == None & y0 == (q1 :: [q2])) |
      (y1 == None & y0 == (q1 :: (q2 :: [q3]))) |
      (y1 == Some (q4) & y0 == (q1 :: (q2 :: (q3 :: (q4 :: q5))))))));


? nthOpt x0 x1