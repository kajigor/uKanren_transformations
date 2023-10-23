check y0 = 
  fresh q1, q2, q3, q4 in 
    (((y0 == (q1 :: q3 :: q4) & one_stepOne_stepCheck q1 q3 q4)));
    
one_stepOne_stepCheck y1 y3 y4 = 
  fresh q1, q2, q3 in
    ((y4 == (q2 :: q3) & y1 == Pair One Two & one_stepOne_stepCheck1 y3 q2 q3) |
    (y4 == (q2 :: q3) & y1 == Pair One Thr & one_stepOne_stepCheck28 y3 q2 q3));
    
one_stepOne_stepCheck1 y6 y8 y9 =
  fresh q1, q2, q3, q4, q5, q6 in
    ((y9 == (q2 :: q3) & y6 == Pair One Thr & one_stepOne_stepCheck2 y8 q2 q3) |
    (y9 == (q2 :: q3) & y6 == Pair Two Thr & one_stepOne_stepCheck28 y8 q2 q3) |
    (y9 == (q5 :: q6) & y6 == Pair Two One & one_stepOne_stepCheck y8 q5 q6));
    
one_stepOne_stepCheck2 y11 y13 y14 =
  fresh q1, q2, q3, q4, q5, q6 in
    (((y14 == (q2 :: q3) & y11 == Pair Two One & one_stepOne_stepCheck3 y13 q2 q3) |
      (y14 == (q2 :: q3) & y11 == Pair Two Thr & one_stepOne_stepCheck27 y13 q2 q3) |
      (y14 == (q5 :: q6) & y11 == Pair Thr One & one_stepOne_stepCheck1 y13 q5 q6)));
      
one_stepOne_stepCheck3 y16 y18 y19 =
  fresh q1, q2, q3, q4, q5, q6 in
    ((y19 == (q2 :: q3) & y16 == Pair One Two & one_stepOne_stepCheck2 y18 q2 q3) |
    (y19 == (q2 :: q3) & y16 == Pair Thr Two & one_stepOne_stepCheck4 y18 q2 q3) |
    (y19 == (q5 :: q6) & y16 == Pair One Thr & one_stepOne_stepCheck27 y18 q5 q6));
    
one_stepOne_stepCheck4 y21 y23 y24 =
  fresh q1, q2, q3, q4, q5, q6 in
    (((y24 == (q2 :: q3) & y21 == Pair One Thr & one_stepOne_stepCheck5 y23 q2 q3) |
      (y24 == (q2 :: q3) & y21 == Pair Two Thr & one_stepOne_stepCheck3 y23 q2 q3) |
      (y24 == (q5 :: q6) & y21 == Pair One Two & one_stepOne_stepCheck6 y23 q5 q6)));
      
one_stepOne_stepCheck5 y26 y28 y29 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y29 == (q2 :: q3) & y26 == Pair Two One & one_stepOne_stepCheck28 y28 q2 q3) |
      (y29 == (q5 :: q6) & y26 == Pair Thr One & one_stepOne_stepCheck4 y28 q5 q6) |
      (y29 == (q5 :: q6) & y26 == Pair Thr Two & one_stepOne_stepCheck6 y28 q5 q6)));
      
one_stepOne_stepCheck6 y31 y33 y34 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y34 == (q2 :: q3) & y31 == Pair One Thr & one_stepOne_stepCheck7 y33 q2 q3) |
      (y34 == (q2 :: q3) & y31 == Pair Two Thr & one_stepOne_stepCheck5 y33 q2 q3) |
      (y34 == (q5 :: q6) & y31 == Pair Two One & one_stepOne_stepCheck4 y33 q5 q6)));
     
one_stepOne_stepCheck7 y36 y38 y39 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y39 == (q2 :: q3) & y36 == Pair Two One & one_stepOne_stepCheck8 y38 q2 q3) |
      (y39 == (q2 :: q3) & y36 == Pair Thr One & one_stepOne_stepCheck6 y38 q2 q3) |
      (y39 == (q5 :: q6) & y36 == Pair Two Thr & one_stepOne_stepCheck26 y38 q5 q6)));
    
one_stepOne_stepCheck8 y41 y43 y44 =
  fresh q1, q2, q3, q4, q5 in 
    (((y44 == (q2 :: q3) & y41 == Pair One Two & one_stepOne_stepCheck7 y43 q2 q3) |
      (y44 == (q2 :: q3) & y41 == Pair One Thr & one_stepOne_stepCheck26 y43 q2 q3) |
      (y44 == [] & y41 == Pair Two Thr & one_step9 y43) |
      (y44 == (q4 :: q5) & y41 == Pair Two Thr & one_stepOne_stepCheck10 y43 q4 q5)));
    
one_step9 y46 =
  y46 == Pair One Thr;
  
one_stepOne_stepCheck10 y48 y50 y51 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y51 == [] & y48 == Pair One Two & one_step11 y50) |
      (y51 == (q1 :: q2) & y48 == Pair One Two & one_stepOne_stepCheck12 y50 q1 q2) |
      (y51 == (q1 :: q2) & y48 == Pair Thr Two & one_stepOne_stepCheck8 y50 q1 q2) |
      (y51 == (q5 :: q6) & y48 == Pair One Thr & one_stepOne_stepCheck25 y50 q5 q6)));
    
one_step11 y53 =
  y53 == Pair Two Thr;
  
one_stepOne_stepCheck12 y55 y57 y58 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y58 == [] & y55 == Pair Two One & one_step9 y57) |
      (y58 == (q1 :: q2) & y55 == Pair Two One & one_stepOne_stepCheck10 y57 q1 q2) |
      (y58 == (q1 :: q2) & y55 == Pair Thr One & one_stepOne_stepCheck13 y57 q1 q2) |
      (y58 == (q5 :: q6) & y55 == Pair Two Thr & one_stepOne_stepCheck25 y57 q5 q6)));
    
one_stepOne_stepCheck13 y60 y62 y63 =
  fresh q1, q2, q3, q4, q5 in 
    (((y63 == [] & y60 == Pair One Thr & one_step11 y62) |
      (y63 == (q1 :: q2) & y60 == Pair One Thr & one_stepOne_stepCheck12 y62 q1 q2) |
      (y63 == (q4 :: q5) & y60 == Pair Two One & one_stepOne_stepCheck14 y62 q4 q5) |
      (y63 == (q4 :: q5) & y60 == Pair Two Thr & one_stepOne_stepCheck24 y62 q4 q5)));
    
one_stepOne_stepCheck14 y65 y67 y68 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y68 == (q2 :: q3) & y65 == Pair One Two & one_stepOne_stepCheck13 y67 q2 q3) |
      (y68 == (q2 :: q3) & y65 == Pair Thr Two & one_stepOne_stepCheck15 y67 q2 q3) |
      (y68 == (q5 :: q6) & y65 == Pair One Thr & one_stepOne_stepCheck24 y67 q5 q6)));
      
one_stepOne_stepCheck15 y70 y72 y73 =
  fresh q1, q2, q3, q4, q5, q6 in
    (((y73 == (q2 :: q3) & y70 == Pair One Thr & one_stepOne_stepCheck16 y72 q2 q3) |
      (y73 == (q2 :: q3) & y70 == Pair Two Thr & one_stepOne_stepCheck14 y72 q2 q3) |
      (y73 == (q5 :: q6) & y70 == Pair One Two & one_stepOne_stepCheck23 y72 q5 q6)));
      
one_stepOne_stepCheck16 y75 y77 y78 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y78 == (q2 :: q3) & y75 == Pair One Two & one_stepOne_stepCheck17 y77 q2 q3) |
      (y78 == (q5 :: q6) & y75 == Pair Thr One & one_stepOne_stepCheck15 y77 q5 q6) |
      (y78 == (q5 :: q6) & y75 == Pair Thr Two & one_stepOne_stepCheck23 y77 q5 q6)));
    
one_stepOne_stepCheck17 y80 y82 y83 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y83 == (q2 :: q3) & y80 == Pair Two One & one_stepOne_stepCheck16 y82 q2 q3) |
      (y83 == (q2 :: q3) & y80 == Pair Thr One & one_stepOne_stepCheck18 y82 q2 q3) |
      (y83 == (q5 :: q6) & y80 == Pair Thr Two & one_stepOne_stepCheck22 y82 q5 q6)));
    
one_stepOne_stepCheck18 y85 y87 y88 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y88 == (q2 :: q3) & y85 == Pair One Thr & one_stepOne_stepCheck17 y87 q2 q3) |
      (y88 == (q2 :: q3) & y85 == Pair Two Thr & one_stepOne_stepCheck19 y87 q2 q3) |
      (y88 == (q5 :: q6) & y85 == Pair One Two & one_stepOne_stepCheck22 y87 q5 q6)));
    
one_stepOne_stepCheck19 y90 y92 y93 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y93 == (q2 :: q3) & y90 == Pair One Two & one_stepOne_stepCheck20 y92 q2 q3) |
      (y93 == (q2 :: q3) & y90 == Pair One Thr & one_stepOne_stepCheck21 y92 q2 q3) |
      (y93 == (q5 :: q6) & y90 == Pair Thr Two & one_stepOne_stepCheck18 y92 q5 q6)));
    
one_stepOne_stepCheck20 y95 y97 y98 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y98 == (q2 :: q3) & y95 == Pair Two One & one_stepOne_stepCheck19 y97 q2 q3) |
      (y98 == (q2 :: q3) & y95 == Pair Thr One & one_stepOne_stepCheck23 y97 q2 q3) |
      (y98 == (q5 :: q6) & y95 == Pair Two Thr & one_stepOne_stepCheck21 y97 q5 q6)));
    
one_stepOne_stepCheck21 y100 y102 y103 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y103 == (q2 :: q3) & y100 == Pair Two One & one_stepOne_stepCheck27 y102 q2 q3) |
      (y103 == (q2 :: q3) & y100 == Pair Thr One & one_stepOne_stepCheck19 y102 q2 q3) |
      (y103 == (q5 :: q6) & y100 == Pair Thr Two & one_stepOne_stepCheck20 y102 q5 q6)));
    
one_stepOne_stepCheck22 y105 y107 y108 =
  fresh q1, q2, q3 in 
    (((y108 == (q2 :: q3) & y105 == Pair Two One & one_stepOne_stepCheck18 y107 q2 q3) |
      (y108 == (q2 :: q3) & y105 == Pair Two Thr & one_stepOne_stepCheck17 y107 q2 q3)));
      
one_stepOne_stepCheck23 y110 y112 y113 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y113 == (q2 :: q3) & y110 == Pair One Thr & one_stepOne_stepCheck20 y112 q2 q3) |
      (y113 == (q2 :: q3) & y110 == Pair Two Thr & one_stepOne_stepCheck16 y112 q2 q3) |
      (y113 == (q5 :: q6) & y110 == Pair Two One & one_stepOne_stepCheck15 y112 q5 q6)));
      
one_stepOne_stepCheck24 y115 y117 y118 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y118 == (q2 :: q3) & y115 == Pair One Two & one_stepOne_stepCheck26 y117 q2 q3) |
      (y118 == (q2 :: q3) & y115 == Pair Thr Two & one_stepOne_stepCheck13 y117 q2 q3) |
      (y118 == (q5 :: q6) & y115 == Pair Thr One & one_stepOne_stepCheck14 y117 q5 q6)));
    
one_stepOne_stepCheck25 y120 y122 y123 =
  fresh q1, q2 in 
    (((y123 == [] & y120 == Pair Thr One & one_step9 y122) |
      (y123 == (q1 :: q2) & y120 == Pair Thr One & one_stepOne_stepCheck10 y122 q1 q2) |
      (y123 == [] & y120 == Pair Thr Two & one_step11 y122) |
      (y123 == (q1 :: q2) & y120 == Pair Thr Two & one_stepOne_stepCheck12 y122 q1 q2)));
    
one_stepOne_stepCheck26 y125 y127 y128 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y128 == (q2 :: q3) & y125 == Pair Two One & one_stepOne_stepCheck24 y127 q2 q3) |
      (y128 == (q2 :: q3) & y125 == Pair Thr One & one_stepOne_stepCheck8 y127 q2 q3) |
      (y128 == (q5 :: q6) & y125 == Pair Thr Two & one_stepOne_stepCheck7 y127 q5 q6)));
    
one_stepOne_stepCheck27 y130 y132 y133 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y133 == (q2 :: q3) & y130 == Pair One Two & one_stepOne_stepCheck21 y132 q2 q3) |
      (y133 == (q2 :: q3) & y130 == Pair Thr Two & one_stepOne_stepCheck2 y132 q2 q3) |
      (y133 == (q5 :: q6) & y130 == Pair Thr One & one_stepOne_stepCheck3 y132 q5 q6)));
    
one_stepOne_stepCheck28 y135 y137 y138 =
  fresh q1, q2, q3, q4, q5, q6 in 
    (((y138 == (q2 :: q3) & y135 == Pair One Two & one_stepOne_stepCheck5 y137 q2 q3) |
      (y138 == (q2 :: q3) & y135 == Pair Thr Two & one_stepOne_stepCheck1 y137 q2 q3) |
      (y138 == (q5 :: q6) & y135 == Pair Thr One & one_stepOne_stepCheck y137 q5 q6)));

? check x0