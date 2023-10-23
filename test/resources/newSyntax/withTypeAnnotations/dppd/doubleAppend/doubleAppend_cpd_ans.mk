double_appendo y0 y1 y2 = 
  fresh q1 in 
    (((y0 == Nil & appendo y2 y1) | (y0 == (S O :: q1) & appendoAppendo y1 y2 q1)));
    
appendo y3 y4 = 
  fresh q1 in 
    (((y4 == Nil & y3 == [S O, S (S O), S (S (S O)), O, O, S O, S (S O)]) | (y4 == (S O :: q1) & appendo01 y3 q1)));
    
appendo01 y5 y6 = 
  fresh q1 in 
    (((y6 == Nil & y5 == [S (S O), S (S (S O)), O, O, S O, S (S O)]) | (y6 == (S (S O) :: q1) & appendo02 y5 q1)));
    
appendo02 y7 y8 = 
  fresh q1 in 
    (((y8 == Nil & y7 == [S (S (S O)), O, O, S O, S (S O)]) | (y8 == (S (S (S O)) :: q1) & appendo03 y7 q1)));
    
appendo03 y9 y10 = 
  fresh q1 in 
    (((y10 == Nil & y9 == [O, O, S O, S (S O)]) | (y10 == (O :: q1) & appendo04 y9 q1)));
    
appendo04 y11 y12 = 
  fresh q1 in 
    (((y12 == Nil & y11 == [O, S O, S (S O)]) | (y12 == (O :: q1) & appendo05 y11 q1)));
    
appendo05 y13 y14 = 
  fresh q1 in 
    (((y14 == Nil & y13 == [S O, S (S O)]) | (y14 == (S O :: q1) & appendo06 y13 q1)));
    
appendo06 y15 y16 = ((y16 == Nil & y15 == [S (S O)]) | (y16 == [S (S O)] & y15 == Nil));

appendoAppendo y17 y18 y19 = 
  fresh q1 in 
    (((y19 == Nil & appendo01 y18 y17) | (y19 == (S (S O) :: q1) & appendo1 y17 y18 q1)));
    
appendo1 y21 y22 y23 = 
  fresh q1 in 
    (((y23 == Nil & appendo02 y22 y21) | (y23 == (S (S (S O)) :: q1) & appendo2 y21 y22 q1)));
    
appendo2 y25 y26 y27 = 
  fresh q1 in 
    (((y27 == Nil & appendo03 y26 y25) | (y27 == (O :: q1) & appendo3 y25 y26 q1)));
    
appendo3 y29 y30 y31 =
  fresh q1 in 
    (((y31 == Nil & appendo04 y30 y29) | (y31 == (O :: q1) & appendo4 y29 y30 q1)));
    
appendo4 y33 y34 y35 = 
  fresh q1 in 
    (((y35 == Nil & appendo05 y34 y33) | (y35 == (S O :: q1) & appendo5 y33 y34 q1)));
    
appendo5 y37 y38 y39 =
  fresh q1 in 
    (((y39 == Nil & appendo06 y38 y37) | (y39 == (S (S O) :: q1) & y38 == Nil & appendo7 y37 q1)));
   
appendo7 y41 y42 = (y42 == Nil & y41 == Nil);

? double_appendo x0 x1 x2