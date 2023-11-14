sorto y0 = (fresh q1, q2, q3, q4 in (((y0 == (q1 :: (q2 :: (q3 :: [q4]))) & minmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4) | (y0 == (q1 :: (q2 :: (q3 :: [q4]))) & _minmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4))));

minmaxoMinmaxoMinmaxoMinmaxoMinmaxo y2 y5 y8 y9 = (fresh q1 in (((minmaxoMinmaxoMinmaxo q1 y5 y8 y9 & ______minmaxo y2 q1) | (_minmaxoMinmaxoMinmaxo q1 y5 y8 y9 & __________minmaxo y2 q1))));

minmaxoMinmaxoMinmaxo y11 y12 y15 y16 = (minmaxoMinmaxo y11 y12 y15 y16 | _minmaxoMinmaxo y11 y12 y15 y16);

minmaxoMinmaxo y18 y19 y21 y22 = ((y18 == y19 & leo y19 & minmaxo y21 y22) | (y19 == Succ (Succ (Zero)) & gtoMinmaxo y18 y21 y22));

leo y23 = (fresh q1 in ((y23 == Zero | (y23 == Succ (q1) & _leo q1))));

_leo y24 = (y24 == Zero | y24 == Succ (Zero));

minmaxo y25 y26 = ((y26 == Succ (Succ (Succ (Zero))) & y25 == Succ (Succ (Zero)) & leo (Succ (Zero))) | (y26 == Succ (Succ (Zero)) & y25 == Succ (Succ (Succ (Zero)))));

gtoMinmaxo y27 y28 y29 = (fresh q1 in (((y27 == Zero & _minmaxo y28 y29) | (y27 == Succ (q1) & _gtoMinmaxo y28 y29 q1))));

_minmaxo y30 y31 = ((y31 == Succ (Succ (Succ (Zero))) & y30 == Zero) | (y31 == Zero & y30 == Succ (Succ (Succ (Zero)))));

_gtoMinmaxo y32 y33 y34 = (y34 == Zero & __minmaxo y32 y33);

__minmaxo y35 y36 = ((y36 == Succ (Succ (Succ (Zero))) & y35 == Succ (Zero) & leo Zero) | (y36 == Succ (Zero) & y35 == Succ (Succ (Succ (Zero)))));

_minmaxoMinmaxo y37 y38 y40 y41 = ((y37 == y38 & __leo y38 & ___minmaxo y40 y41) | (y38 == Succ (Succ (Succ (Zero))) & __gtoMinmaxo y37 y40 y41));

__leo y42 = (fresh q1 in ((y42 == Zero | (y42 == Succ (q1) & leo q1))));

___minmaxo y43 y44 = (y44 == Succ (Succ (Zero)) & y43 == Succ (Succ (Succ (Zero))) & leo (Succ (Succ (Succ (Zero)))));

__gtoMinmaxo y45 y46 y47 = (fresh q1 in (((y45 == Zero & __________minmaxo y46 y47) | (y45 == Succ (q1) & ___gtoMinmaxo y46 y47 q1))));

___gtoMinmaxo y48 y49 y50 = (fresh q1 in (((y50 == Zero & ____minmaxo y48 y49) | (y50 == Succ (q1) & ____gtoMinmaxo y48 y49 q1))));

____minmaxo y51 y52 = ((y52 == Succ (Succ (Zero)) & y51 == Succ (Zero) & leo (Succ (Zero))) | (y52 == Succ (Zero) & y51 == Succ (Succ (Zero))));

____gtoMinmaxo y53 y54 y55 = (y55 == Zero & _____minmaxo y53 y54);

_____minmaxo y56 y57 = (y57 == Succ (Succ (Zero)) & y56 == Succ (Succ (Zero)) & leo (Succ (Succ (Zero))));

______minmaxo y58 y59 = ((y59 == Succ (Zero) & y58 == Zero & _leo Zero) | (y59 == Zero & y58 == Succ (Zero)));

_minmaxoMinmaxoMinmaxo y60 y61 y64 y65 = (__minmaxoMinmaxo y60 y61 y64 y65 | ___minmaxoMinmaxo y60 y61 y64 y65);

__minmaxoMinmaxo y67 y68 y70 y71 = ((y67 == y68 & _leo y68 & __minmaxo y70 y71) | (y68 == Succ (Zero) & _____gtoMinmaxo y67 y70 y71));

_____gtoMinmaxo y72 y73 y74 = (y72 == Zero & _minmaxo y73 y74);

___minmaxoMinmaxo y75 y76 y78 y79 = ((y75 == y76 & __leo y76 & _______minmaxo y78 y79) | (y76 == Succ (Succ (Succ (Zero))) & ______gtoMinmaxo y75 y78 y79));

_______minmaxo y80 y81 = (y81 == Succ (Zero) & y80 == Succ (Succ (Succ (Zero))) & _leo (Succ (Succ (Succ (Zero)))));

______gtoMinmaxo y82 y83 y84 = (fresh q1 in (((y82 == Zero & ______minmaxo y83 y84) | (y82 == Succ (q1) & _______gtoMinmaxo y83 y84 q1))));

_______gtoMinmaxo y85 y86 y87 = (fresh q1 in (((y87 == Zero & ________minmaxo y85 y86) | (y87 == Succ (q1) & ________gtoMinmaxo y85 y86 q1))));

________minmaxo y88 y89 = (y89 == Succ (Zero) & y88 == Succ (Zero) & _leo (Succ (Zero)));

________gtoMinmaxo y90 y91 y92 = (y92 == Zero & _________minmaxo y90 y91);

_________minmaxo y93 y94 = (y94 == Succ (Zero) & y93 == Succ (Succ (Zero)) & _leo (Succ (Succ (Zero))));

__________minmaxo y95 y96 = ((y96 == Succ (Succ (Zero)) & y95 == Zero & leo Zero) | (y96 == Zero & y95 == Succ (Succ (Zero))));

_minmaxoMinmaxoMinmaxoMinmaxoMinmaxo y98 y101 y104 y105 = (fresh q1, q2, q3, q4 in ((___minmaxoMinmaxoMinmaxo q1 y101 y104 y105 & _minmaxo y98 q1)));

___minmaxoMinmaxoMinmaxo y107 y108 y111 y112 = (____minmaxoMinmaxo y107 y108 y111 y112 | _____minmaxoMinmaxo y107 y108 y111 y112);

____minmaxoMinmaxo y114 y115 y117 y118 = ((y114 == y115 & _leo y115 & ____minmaxo y117 y118) | (y115 == Succ (Zero) & _________gtoMinmaxo y114 y117 y118));

_________gtoMinmaxo y119 y120 y121 = (y119 == Zero & __________minmaxo y120 y121);

_____minmaxoMinmaxo y122 y123 y125 y126 = ((y122 == y123 & leo y123 & _________minmaxo y125 y126) | (y123 == Succ (Succ (Zero)) & __________gtoMinmaxo y122 y125 y126));

__________gtoMinmaxo y127 y128 y129 = (fresh q1 in (((y127 == Zero & ______minmaxo y128 y129) | (y127 == Succ (q1) & ___________gtoMinmaxo y128 y129 q1))));

___________gtoMinmaxo y130 y131 y132 = (y132 == Zero & ________minmaxo y130 y131);


? sorto x0