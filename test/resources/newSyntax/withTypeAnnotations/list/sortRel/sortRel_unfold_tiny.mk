sorto y0 = 
  fresh q1, q2, q3, q4 in
    (y0 == ([q1, q2, q3, q4]) & fn1 q1 q2 q3 q4) |
    (y0 == ([q1, q2, q3, q4]) & fn2 q1 q2 q3 q4);
    
fn1 y2 y5 y8 y9 = 
  fresh q1 in 
    (fn3 q1 y5 y8 y9 & fn9 y2 q1) |
    (fn10 q1 y5 y8 y9 & fn13 y2 q1);
    
fn3 y11 y12 y15 y16 = 
  (fn4 y11 y12 y15 y16 | fn7 y11 y12 y15 y16);
  
fn4 y18 y19 y21 y22 = 
  y19 == Zero & y18 == y19 & fn5 y21 y22 |
  y19 == Succ Zero & y18 == y19 & fn5 y21 y22 |
  y19 == Succ Zero & y18 == Zero & fn6 y21 y22;
  
fn5 y23 y24 = 
  y24 == Succ (Succ Zero) & y23 == Succ Zero |
  y24 == Succ Zero & y23 == Succ (Succ Zero);
  
fn6 y25 y26 = 
  y26 == Succ (Succ Zero) & y25 == Zero |
  y26 == Zero & y25 == Succ (Succ Zero);
  
fn7 y27 y28 y30 y31 =
  y28 == Succ (Succ Zero) & y27 == Zero & fn13 y30 y31 |
  y28 == Succ (Succ Zero) & y27 == Succ Zero & fn8 y30 y31;
  
fn8 y32 y33 = 
  y33 == Succ Zero & y32 == Succ Zero;
  
fn9 y34 y35 = 
  y35 == Zero & y34 == Zero;
  
fn10 y36 y37 y40 y41 = 
  fn11 y36 y37 y40 y41 | fn12 y36 y37 y40 y41;
  
fn11 y43 y44 y46 y47 = 
  y44 == Zero & y43 == y44 & fn6 y46 y47;
  
fn12 y48 y49 y51 y52 = 
  y49 == Succ (Succ Zero) & y48 == Zero & fn9 y51 y52;
   
fn13 y53 y54 = 
  y54 == Succ Zero & y53 == Zero | y54 == Zero & y53 == Succ Zero;
  
fn2 y56 y59 y62 y63 = 
  fresh q1, q2, q3, q4 in 
    fn14 q1 y59 y62 y63 & fn6 y56 q1;
    
fn14 y65 y66 y69 y70 = 
  fn15 y65 y66 y69 y70 | fn16 y65 y66 y69 y70;
  
fn15 y72 y73 y75 y76 = 
  y73 == Zero & y72 == y73 & fn13 y75 y76;
  
fn16 y77 y78 y80 y81 =
  y78 == Succ Zero & y77 == Zero & fn9 y80 y81;

? sorto x0