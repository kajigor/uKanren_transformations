sorto y0 =
  fresh q1, q2, q3, q4 in
    (((y0 == ([q1, q2, q3, q4]) & minmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4) |
    (y0 == ([q1, q2, q3, q4]) & fn5 q1 q2 q3 q4)));
    
minmaxoMinmaxoMinmaxoMinmaxoMinmaxo y2 y5 y8 y9 = 
  fresh q1 in 
    (((minmaxoMinmaxoMinmaxo q1 y5 y8 y9 & fn12 y2 q1) | (fn11 q1 y5 y8 y9 & fn6 y2 q1)));
    
minmaxoMinmaxoMinmaxo y11 y12 y15 y16 =
  (minmaxoMinmaxo y11 y12 y15 y16 | fn18 y11 y12 y15 y16);

minmaxoMinmaxo y18 y19 y21 y22 =
  ((y18 == y19 & leo y19 & minmaxo y21 y22) | (y19 == Succ Zero & fn20 y18 y21 y22));

leo y23 =
  (y23 == Zero | y23 == Succ Zero);

minmaxo y24 y25 =
  ((y25 == Succ (Succ Zero) & y24 == Succ Zero & leo Zero) |
  (y25 == Succ Zero & y24 == Succ (Succ Zero)));

fn20 y26 y27 y28 =
  (y26 == Zero & fn19 y27 y28);

fn19 y29 y30 =
  ((y30 == Succ (Succ Zero) & y29 == Zero) | (y30 == Zero & y29 == Succ (Succ Zero)));

fn18 y31 y32 y34 y35 =
  ((y31 == y32 & fn17 y32 & fn16 y34 y35) | (y32 == Succ (Succ Zero) & fn15 y31 y34 y35));

fn17 y36 =
  fresh q1 in
    ((y36 == Zero | (y36 == Succ q1 & leo q1)));

fn16 y37 y38 =
  (y38 == Succ Zero & y37 == Succ (Succ Zero) & leo (Succ (Succ Zero)));

fn15 y39 y40 y41 =
  fresh q1 in
    (((y39 == Zero & fn6 y40 y41) | (y39 == Succ q1 & fn14 y40 y41 q1)));

fn14 y42 y43 y44 =
  (y44 == Zero & fn13 y42 y43);

fn13 y45 y46 =
  (y46 == Succ Zero & y45 == Succ Zero & leo (Succ Zero));

fn12 y47 y48 =
  (y48 == Zero & y47 == Zero);

fn11 y49 y50 y53 y54 =
  (fn10 y49 y50 y53 y54 | fn9 y49 y50 y53 y54);

fn10 y56 y57 y59 y60 =
  (y57 == Zero & y56 == y57 & fn19 y59 y60);

fn9 y61 y62 y64 y65 =
  ((y62 == Succ (Succ Zero) & fn8 y61 y64 y65));

fn8 y66 y67 y68 =
  fresh q1 in
    (((y66 == Zero & fn12 y67 y68)));


fn6 y72 y73 =
  ((y73 == Succ Zero & y72 == Zero & leo Zero) |
  (y73 == Zero & y72 == Succ Zero));

fn5 y75 y78 y81 y82 =
  fresh q1, q2, q3, q4 in
    (((fn4 q1 y78 y81 y82 & fn19 y75 q1)));

fn4 y84 y85 y88 y89 =
  (fn3 y84 y85 y88 y89 | fn2 y84 y85 y88 y89);

fn3 y91 y92 y94 y95 =
  (y92 == Zero & y91 == y92 & fn6 y94 y95);

fn2 y96 y97 y99 y100 =
  ((y97 == Succ Zero & fn1 y96 y99 y100));

fn1 y101 y102 y103 = (y101 == Zero & fn12 y102 y103);

? sorto x0