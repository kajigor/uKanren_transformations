sorto y0 = (fresh q1, q2, q3, q4 in (((y0 == (q1 :: (q2 :: (q3 :: [q4]))) & minmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4) | (y0 == (q1 :: (q2 :: (q3 :: [q4]))) & _minmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4))));

minmaxoMinmaxoMinmaxoMinmaxoMinmaxo y2 y5 y8 y9 = (fresh q1 in (((minmaxoMinmaxoMinmaxo q1 y5 y8 y9 & ______minmaxo y2 q1) | (_minmaxoMinmaxoMinmaxo q1 y5 y8 y9 & __________minmaxo y2 q1))));

minmaxoMinmaxoMinmaxo y11 y12 y15 y16 = (minmaxoMinmaxo y11 y12 y15 y16 | _minmaxoMinmaxo y11 y12 y15 y16);

minmaxoMinmaxo y18 y19 y21 y22 = ((y19 == Zero & y18 == y19 & minmaxo y21 y22) | (y19 == Succ (Zero) & y18 == y19 & minmaxo y21 y22) | (y19 == Succ (Succ (Zero)) & y18 == y19 & minmaxo y21 y22) | (y19 == Succ (Succ (Zero)) & y18 == Zero & _minmaxo y21 y22) | (y19 == Succ (Succ (Zero)) & y18 == Succ (Zero) & __minmaxo y21 y22));

minmaxo y23 y24 = ((y24 == Succ (Succ (Succ (Zero))) & y23 == Succ (Succ (Zero))) | (y24 == Succ (Succ (Zero)) & y23 == Succ (Succ (Succ (Zero)))));

_minmaxo y25 y26 = ((y26 == Succ (Succ (Succ (Zero))) & y25 == Zero) | (y26 == Zero & y25 == Succ (Succ (Succ (Zero)))));

__minmaxo y27 y28 = ((y28 == Succ (Succ (Succ (Zero))) & y27 == Succ (Zero)) | (y28 == Succ (Zero) & y27 == Succ (Succ (Succ (Zero)))));

_minmaxoMinmaxo y29 y30 y32 y33 = ((y30 == Succ (Succ (Succ (Zero))) & y29 == Zero & __________minmaxo y32 y33) | (y30 == Succ (Succ (Succ (Zero))) & y29 == Succ (Zero) & ____minmaxo y32 y33) | (y30 == Succ (Succ (Succ (Zero))) & y29 == Succ (Succ (Zero)) & _____minmaxo y32 y33));

____minmaxo y34 y35 = ((y35 == Succ (Succ (Zero)) & y34 == Succ (Zero)) | (y35 == Succ (Zero) & y34 == Succ (Succ (Zero))));

_____minmaxo y36 y37 = (y37 == Succ (Succ (Zero)) & y36 == Succ (Succ (Zero)));

______minmaxo y38 y39 = ((y39 == Succ (Zero) & y38 == Zero) | (y39 == Zero & y38 == Succ (Zero)));

_minmaxoMinmaxoMinmaxo y40 y41 y44 y45 = (__minmaxoMinmaxo y40 y41 y44 y45 | ___minmaxoMinmaxo y40 y41 y44 y45);

__minmaxoMinmaxo y47 y48 y50 y51 = ((y48 == Zero & y47 == y48 & __minmaxo y50 y51) | (y48 == Succ (Zero) & y47 == y48 & __minmaxo y50 y51) | (y48 == Succ (Zero) & y47 == Zero & _minmaxo y50 y51));

___minmaxoMinmaxo y52 y53 y55 y56 = ((y53 == Succ (Succ (Succ (Zero))) & y52 == Zero & ______minmaxo y55 y56) | (y53 == Succ (Succ (Succ (Zero))) & y52 == Succ (Zero) & ________minmaxo y55 y56));

________minmaxo y57 y58 = (y58 == Succ (Zero) & y57 == Succ (Zero));

__________minmaxo y59 y60 = ((y60 == Succ (Succ (Zero)) & y59 == Zero) | (y60 == Zero & y59 == Succ (Succ (Zero))));

_minmaxoMinmaxoMinmaxoMinmaxoMinmaxo y62 y65 y68 y69 = (fresh q1, q2, q3, q4 in ((___minmaxoMinmaxoMinmaxo q1 y65 y68 y69 & _minmaxo y62 q1)));

___minmaxoMinmaxoMinmaxo y71 y72 y75 y76 = (____minmaxoMinmaxo y71 y72 y75 y76 | _____minmaxoMinmaxo y71 y72 y75 y76);

____minmaxoMinmaxo y78 y79 y81 y82 = ((y79 == Zero & y78 == y79 & ____minmaxo y81 y82) | (y79 == Succ (Zero) & y78 == y79 & ____minmaxo y81 y82) | (y79 == Succ (Zero) & y78 == Zero & __________minmaxo y81 y82));

_____minmaxoMinmaxo y83 y84 y86 y87 = ((y84 == Succ (Succ (Zero)) & y83 == Zero & ______minmaxo y86 y87) | (y84 == Succ (Succ (Zero)) & y83 == Succ (Zero) & ________minmaxo y86 y87));


? sorto x0