eval y0 = (fresh q1, q2 in ((y0 == (q1 :: q2) & step_Eval q1 q2)));

step_Eval y1 y2 = (fresh q1, q2, q3, q4 in (((y2 == (q1 :: q2) & stepEval q1 q2 & y1 == Empty & _________safe_ True True True & __________safe_ False False False) | (y2 == (q3 :: q4) & __stepEval q3 q4 & y1 == Goat & _________safe_ False True True & __________safe_ True False False) | (y2 == (q3 :: q4) & ________________stepEval q3 q4 & y1 == Wolf & _________safe_ True False True & __________safe_ False True False) | (y2 == (q3 :: q4) & ____________________stepEval q3 q4 & y1 == Cabbage & _________safe_ True True False & __________safe_ False False True))));

stepEval y4 y5 = _step_Eval y4 y5;

_step_Eval y7 y8 = (fresh q1, q2 in ((y8 == (q1 :: q2) & _stepEval q1 q2 & y7 == Empty & _________safe_ False False False & __________safe_ True True True)));

_stepEval y11 y12 = step_Eval y11 y12;

__stepEval y14 y15 = __step_Eval y14 y15;

__step_Eval y17 y18 = (fresh q1, q2, q3, q4 in (((y18 == (q1 :: q2) & ___stepEval q1 q2 & y17 == Empty & _________safe_ True False False & __________safe_ False True True) | (y18 == (q3 :: q4) & _stepEval q3 q4 & y17 == Goat & _________safe_ False False False & __________safe_ True True True))));

___stepEval y21 y22 = ___step_Eval y21 y22;

___step_Eval y24 y25 = (fresh q1, q2, q3, q4 in (((y25 == (q1 :: q2) & __stepEval q1 q2 & y24 == Empty & _________safe_ False True True & __________safe_ True False False) | (y25 == (q3 :: q4) & ____stepEval q3 q4 & y24 == Wolf & _________safe_ False False True & __________safe_ False True False) | (y25 == (q3 :: q4) & ____________stepEval q3 q4 & y24 == Cabbage & _________safe_ False True False & __________safe_ True False True))));

____stepEval y27 y28 = ____step_Eval y27 y28;

____step_Eval y30 y31 = (fresh q1, q2, q3, q4 in (((y31 == (q1 :: q2) & _____stepEval q1 q2 & y30 == Empty & _________safe_ False True False & __________safe_ False False True) | (y31 == (q3 :: q4) & __________stepEval q3 q4 & y30 == Wolf & _________safe_ False False False & __________safe_ False True True))));

_____stepEval y34 y35 = _____step_Eval y34 y35;

_____step_Eval y37 y38 = (fresh q1, q2, q3, q4 in (((y38 == (q1 :: q2) & ____stepEval q1 q2 & y37 == Empty & _________safe_ False False True & __________safe_ False True False) | (y38 == (q3 :: q4) & ______stepEval q3 q4 & y37 == Cabbage & _________safe_ False False False & __________safe_ False True True))));

______stepEval y40 y41 = ______step_Eval y40 y41;

______step_Eval y43 y44 = (fresh q1, q2, q3, q4 in (((y44 == (q1 :: q2) & _______stepEval q1 q2 & y43 == Empty & _________safe_ False True True & __________safe_ False False False) | (y44 == (q3 :: q4) & ________stepEval q3 q4 & y43 == Wolf & _________safe_ False False True & __________safe_ False True False) | (y44 == (q3 :: q4) & _____stepEval q3 q4 & y43 == Cabbage & _________safe_ False True False & __________safe_ False False True))));

_______stepEval y47 y48 = _______step_Eval y47 y48;

_______step_Eval y50 y51 = (fresh q1, q2 in ((y51 == (q1 :: q2) & ______stepEval q1 q2 & y50 == Empty & _________safe_ False False False & __________safe_ False True True)));

________stepEval y53 y54 = ________step_Eval y53 y54;

________step_Eval y56 y57 = (fresh q1, q2, q3, q4 in (((y57 == (q1 :: q2) & _________stepEval q1 q2 & y56 == Empty & _________safe_ False True False & __________safe_ False False True) | (y57 == (q3 :: q4) & ______stepEval q3 q4 & y56 == Wolf & _________safe_ False False False & __________safe_ False True True))));

_________stepEval y59 y60 = (fresh q1, q2 in ((_step_ y59 (Pair (q1) (q2)) False False True False True False & _eval (Pair (q2) (q1)) y60)));

_eval y64 y65 = (fresh q1, q2, q3 in (((y65 == [] & y64 == Pair (Quad (False) (False) (False) (False)) (Quad (True) (True) (True) (True))) | (y65 == (q1 :: q2) & step y64 q1 q3 & _eval q3 q2))));

step y66 y67 y68 = (fresh q1, q2, q3, q4, q5, q6, q7, q8 in (((y66 == Pair (Quad (q1) (q2) (q3) (True)) (Quad (q4) (q5) (q6) (False)) & _step_ y67 y68 q1 q2 q3 q4 q5 q6) | (y68 == Pair (q7) (q8) & y66 == Pair (Quad (q4) (q5) (q6) (False)) (Quad (q1) (q2) (q3) (True)) & _step_ y67 (Pair (q8) (q7)) q1 q2 q3 q4 q5 q6))));

_step_ y69 y70 y71 y72 y73 y74 y75 y76 = ((y70 == Pair (Quad (y71) (y72) (y73) (False)) (Quad (y74) (y75) (y76) (True)) & _________safe_ y71 y72 y73 & y69 == Empty & __________safe_ y74 y75 y76) | (y71 == True & _________safe_ False y72 y73 & y69 == Goat & y70 == Pair (Quad (False) (y72) (y73) (False)) (Quad (True) (y75) (y76) (True)) & __________safe_ True y75 y76) | (y72 == True & _________safe_ y71 False y73 & y69 == Wolf & y70 == Pair (Quad (y71) (False) (y73) (False)) (Quad (y75) (True) (y76) (True)) & __________safe_ y75 True y76) | (y73 == True & _________safe_ y71 y72 False & y69 == Cabbage & y70 == Pair (Quad (y71) (y72) (False) (False)) (Quad (y74) (y75) (True) (True)) & __________safe_ y74 y75 True));

_________safe_ y77 y78 y79 = (y77 == False | (y79 == True & y78 == True & y77 == True) | (y79 == False & y78 == False & y77 == True));

__________safe_ y80 y81 y82 = y80 == y80;

__________stepEval y83 y84 = _________step_Eval y83 y84;

_________step_Eval y86 y87 = (fresh q1, q2, q3, q4 in (((y87 == (q1 :: q2) & ___________stepEval q1 q2 & y86 == Empty & _________safe_ False True True & __________safe_ False False False) | (y87 == (q3 :: q4) & ____stepEval q3 q4 & y86 == Wolf & _________safe_ False False True & __________safe_ False True False) | (y87 == (q3 :: q4) & _________stepEval q3 q4 & y86 == Cabbage & _________safe_ False True False & __________safe_ False False True))));

___________stepEval y89 y90 = __________step_Eval y89 y90;

__________step_Eval y92 y93 = (fresh q1, q2 in ((y93 == (q1 :: q2) & __________stepEval q1 q2 & y92 == Empty & _________safe_ False False False & __________safe_ False True True)));

____________stepEval y96 y97 = ___________step_Eval y96 y97;

___________step_Eval y99 y100 = (fresh q1, q2, q3, q4 in (((y100 == (q1 :: q2) & _____________stepEval q1 q2 & y99 == Empty & _________safe_ True False True & __________safe_ False True False) | (y100 == (q3 :: q4) & ______________stepEval q3 q4 & y99 == Goat & _________safe_ False False True & __________safe_ True True False) | (y100 == (q3 :: q4) & ___stepEval q3 q4 & y99 == Cabbage & _________safe_ True False False & __________safe_ False True True))));

_____________stepEval y103 y104 = ____________step_Eval y103 y104;

____________step_Eval y106 y107 = (fresh q1, q2, q3, q4 in (((y107 == (q1 :: q2) & ____________stepEval q1 q2 & y106 == Empty & _________safe_ False True False & __________safe_ True False True) | (y107 == (q3 :: q4) & ______stepEval q3 q4 & y106 == Wolf & _________safe_ False False False & __________safe_ False True True))));

______________stepEval y109 y110 = _____________step_Eval y109 y110;

_____________step_Eval y112 y113 = (fresh q1, q2, q3, q4 in (((y113 == (q1 :: q2) & ____________________stepEval q1 q2 & y112 == Empty & _________safe_ True True False & __________safe_ False False True) | (y113 == (q3 :: q4) & ____________stepEval q3 q4 & y112 == Goat & _________safe_ False True False & __________safe_ True False True) | (y113 == (q3 :: q4) & _______________stepEval q3 q4 & y112 == Wolf & _________safe_ True False False & __________safe_ False True True))));

_______________stepEval y115 y116 = (fresh q1, q2 in ((_step_ y115 (Pair (q1) (q2)) False True True True False False & _eval (Pair (q2) (q1)) y116)));

________________stepEval y118 y119 = ______________step_Eval y118 y119;

______________step_Eval y121 y122 = (fresh q1, q2, q3, q4 in (((y122 == (q1 :: q2) & _________________stepEval q1 q2 & y121 == Empty & _________safe_ False True False & __________safe_ True False True) | (y122 == (q3 :: q4) & __________stepEval q3 q4 & y121 == Wolf & _________safe_ False False False & __________safe_ False True True))));

_________________stepEval y125 y126 = _______________step_Eval y125 y126;

_______________step_Eval y128 y129 = (fresh q1, q2, q3, q4 in (((y129 == (q1 :: q2) & ________________stepEval q1 q2 & y128 == Empty & _________safe_ True False True & __________safe_ False True False) | (y129 == (q3 :: q4) & __________________stepEval q3 q4 & y128 == Goat & _________safe_ False False True & __________safe_ True True False) | (y129 == (q3 :: q4) & _______________stepEval q3 q4 & y128 == Cabbage & _________safe_ True False False & __________safe_ False True True))));

__________________stepEval y131 y132 = ________________step_Eval y131 y132;

________________step_Eval y134 y135 = (fresh q1, q2, q3, q4 in (((y135 == (q1 :: q2) & ___________________stepEval q1 q2 & y134 == Empty & _________safe_ True True False & __________safe_ False False True) | (y135 == (q3 :: q4) & _________________stepEval q3 q4 & y134 == Goat & _________safe_ False True False & __________safe_ True False True) | (y135 == (q3 :: q4) & ___stepEval q3 q4 & y134 == Wolf & _________safe_ True False False & __________safe_ False True True))));

___________________stepEval y138 y139 = _________________step_Eval y138 y139;

_________________step_Eval y141 y142 = (fresh q1, q2 in (((y142 == (q1 :: q2) & __________________stepEval q1 q2 & y141 == Empty & _________safe_ False False True & __________safe_ True True False) | (y141 == Cabbage & _________safe_ False False False & __________safe_ True True True & _eval (Pair (Quad (False) (False) (False) (False)) (Quad (True) (True) (True) (True))) y142))));

____________________stepEval y144 y145 = __________________step_Eval y144 y145;

__________________step_Eval y147 y148 = (fresh q1, q2, q3, q4 in (((y148 == (q1 :: q2) & ______________stepEval q1 q2 & y147 == Empty & _________safe_ False False True & __________safe_ True True False) | (y148 == (q3 :: q4) & _stepEval q3 q4 & y147 == Cabbage & _________safe_ False False False & __________safe_ True True True))));


? eval x0