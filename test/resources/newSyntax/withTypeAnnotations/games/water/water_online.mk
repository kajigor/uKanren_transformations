checkAnswer y0 = (fresh q1, q2 in (((y0 == [] & ____isFinishState O) | (y0 == (q1 :: q2) & __checkStepDoStepCheckAnswer_ O O q1 q2))));

__checkStepDoStepCheckAnswer_ y21 y22 y23 y25 = (fresh q1 in (((y23 == (Fill, Fst) & y21 == O & __checkAnswer_ y25 y22) | (y23 == (Empt, Fst) & y21 == S (S (S (S (O)))) & ___checkAnswer_ y25 y22) | (y23 == (Pour, Fst) & y21 == S (q1) & eqNatDoStepCheckAnswer_ y25 y22 q1) | (y23 == (Fill, Snd) & y22 == O & _____checkAnswer_ y25 y21) | (y23 == (Empt, Snd) & y22 == S (S (S (S (S (S (S (S (S (O))))))))) & ____checkAnswer_ y25 y21) | (y23 == (Pour, Snd) & y22 == S (q1) & _________eqNatDoStepCheckAnswer_ y25 y21 q1))));

__checkAnswer_ y26 y27 = (fresh q1, q2 in (((y26 == [] & __isFinishState y27) | (y26 == (q1 :: q2) & __checkStepDoStepCheckAnswer_ (S (S (S (S (O))))) y27 q1 q2))));

__isFinishState y28 = y28 == S (S (S (S (S (S (S (O)))))));

___checkAnswer_ y29 y30 = (fresh q1, q2 in (((y29 == [] & ___isFinishState y30) | (y29 == (q1 :: q2) & __checkStepDoStepCheckAnswer_ O y30 q1 q2))));

___isFinishState y31 = y31 == S (S (S (S (S (S (S (O)))))));

eqNatDoStepCheckAnswer_ y33 y34 y35 = (fresh q1 in (((y34 == O & _doStepCheckAnswer_ y35 O y33) | (y34 == S (q1) & _eqNatDoStepCheckAnswer_ y33 y35 q1))));

_eqNatDoStepCheckAnswer_ y37 y38 y39 = (fresh q1 in (((y39 == O & _doStepCheckAnswer_ y38 (S (O)) y37) | (y39 == S (q1) & __eqNatDoStepCheckAnswer_ y37 y38 q1))));

_doStepCheckAnswer_ y40 y41 y43 = (_addCheckAnswer_ y43 y41 y40 | addGreaterCheckAnswer_ y43 y41 y40);

_addCheckAnswer_ y44 y45 y46 = (fresh q1, q2 in (((y46 == O & y45 == S (S (S (S (S (S (S (S (S (q1))))))))) & _____checkAnswer_ y44 (S (q1))) | (y46 == S (q2) & _addCheckAnswer_ y44 (S (y45)) q2))));

addGreaterCheckAnswer_ y48 y49 y51 = (fresh q1 in (((y51 == O & greaterCheckAnswer_ y48 y49) | (y51 == S (q1) & addGreaterCheckAnswer_ y48 (S (y49)) q1))));

greaterCheckAnswer_ y52 y53 = (fresh q1 in (((y53 == O & ___checkAnswer_ y52 (S (O))) | (y53 == S (q1) & _greaterCheckAnswer_ y52 q1))));

_greaterCheckAnswer_ y54 y55 = (fresh q1 in (((y55 == O & ___checkAnswer_ y54 (S (S (O)))) | (y55 == S (q1) & __greaterCheckAnswer_ y54 q1))));

__greaterCheckAnswer_ y56 y57 = (fresh q1 in (((y57 == O & ___checkAnswer_ y56 (S (S (S (O))))) | (y57 == S (q1) & ___greaterCheckAnswer_ y56 q1))));

___greaterCheckAnswer_ y58 y59 = (fresh q1 in (((y59 == O & ___checkAnswer_ y58 (S (S (S (S (O)))))) | (y59 == S (q1) & ____greaterCheckAnswer_ y58 q1))));

____greaterCheckAnswer_ y60 y61 = (fresh q1 in (((y61 == O & ___checkAnswer_ y60 (S (S (S (S (S (O))))))) | (y61 == S (q1) & _____greaterCheckAnswer_ y60 q1))));

_____greaterCheckAnswer_ y62 y63 = (fresh q1 in (((y63 == O & ___checkAnswer_ y62 (S (S (S (S (S (S (O)))))))) | (y63 == S (q1) & ______greaterCheckAnswer_ y62 q1))));

______greaterCheckAnswer_ y64 y65 = (fresh q1 in (((y65 == O & ___checkAnswer_ y64 (S (S (S (S (S (S (S (O))))))))) | (y65 == S (q1) & _______greaterCheckAnswer_ y64 q1))));

_______greaterCheckAnswer_ y66 y67 = ((y67 == O & ___checkAnswer_ y66 (S (S (S (S (S (S (S (S (O)))))))))) | (y67 == S (O) & _____checkAnswer_ y66 O));

__eqNatDoStepCheckAnswer_ y69 y70 y71 = (fresh q1 in (((y71 == O & _doStepCheckAnswer_ y70 (S (S (O))) y69) | (y71 == S (q1) & ___eqNatDoStepCheckAnswer_ y69 y70 q1))));

___eqNatDoStepCheckAnswer_ y73 y74 y75 = (fresh q1 in (((y75 == O & _doStepCheckAnswer_ y74 (S (S (S (O)))) y73) | (y75 == S (q1) & ____eqNatDoStepCheckAnswer_ y73 y74 q1))));

____eqNatDoStepCheckAnswer_ y77 y78 y79 = (fresh q1 in (((y79 == O & _doStepCheckAnswer_ y78 (S (S (S (S (O))))) y77) | (y79 == S (q1) & _____eqNatDoStepCheckAnswer_ y77 y78 q1))));

_____eqNatDoStepCheckAnswer_ y81 y82 y83 = (fresh q1 in (((y83 == O & _doStepCheckAnswer_ y82 (S (S (S (S (S (O)))))) y81) | (y83 == S (q1) & ______eqNatDoStepCheckAnswer_ y81 y82 q1))));

______eqNatDoStepCheckAnswer_ y85 y86 y87 = (fresh q1 in (((y87 == O & _doStepCheckAnswer_ y86 (S (S (S (S (S (S (O))))))) y85) | (y87 == S (q1) & _______eqNatDoStepCheckAnswer_ y85 y86 q1))));

_______eqNatDoStepCheckAnswer_ y89 y90 y91 = (fresh q1 in (((y91 == O & _doStepCheckAnswer_ y90 (S (S (S (S (S (S (S (O)))))))) y89) | (y91 == S (q1) & ________eqNatDoStepCheckAnswer_ y89 y90 q1))));

________eqNatDoStepCheckAnswer_ y93 y94 y95 = (fresh q1 in (((y95 == O & _doStepCheckAnswer_ y94 (S (S (S (S (S (S (S (S (O))))))))) y93) | (y95 == S (S (q1)) & _doStepCheckAnswer_ y94 (S (S (S (S (S (S (S (S (S (S (q1))))))))))) y93))));

____checkAnswer_ y96 y97 = (fresh q1, q2 in (((y96 == [] & ____isFinishState y97) | (y96 == (q1 :: q2) & __checkStepDoStepCheckAnswer_ y97 O q1 q2))));

____isFinishState y98 = y98 == S (S (S (S (S (S (S (O)))))));

_________eqNatDoStepCheckAnswer_ y100 y101 y102 = (fresh q1 in (((y101 == O & __doStepCheckAnswer_ y100 y102) | (y101 == S (q1) & __________eqNatDoStepCheckAnswer_ y100 y102 q1))));

__doStepCheckAnswer_ y104 y105 = (fresh q1 in (((y105 == S (S (S (S (q1)))) & __checkAnswer_ y104 (S (q1))) | ________greaterCheckAnswer_ y104 y105)));

________greaterCheckAnswer_ y106 y107 = (fresh q1 in (((y107 == O & ____checkAnswer_ y106 (S (O))) | (y107 == S (q1) & _________greaterCheckAnswer_ y106 q1))));

_________greaterCheckAnswer_ y108 y109 = (fresh q1 in (((y109 == O & ____checkAnswer_ y108 (S (S (O)))) | (y109 == S (q1) & __________greaterCheckAnswer_ y108 q1))));

__________greaterCheckAnswer_ y110 y111 = ((y111 == O & ____checkAnswer_ y110 (S (S (S (O))))) | (y111 == S (O) & ____checkAnswer_ y110 (S (S (S (S (O)))))));

__________eqNatDoStepCheckAnswer_ y113 y114 y115 = (fresh q1 in (((y115 == O & ___doStepCheckAnswer_ y113 y114) | (y115 == S (q1) & ___________eqNatDoStepCheckAnswer_ y113 y114 q1))));

___doStepCheckAnswer_ y117 y118 = (fresh q1 in (((y118 == S (S (S (q1))) & __checkAnswer_ y117 (S (q1))) | _________greaterCheckAnswer_ y117 y118)));

___________eqNatDoStepCheckAnswer_ y120 y121 y122 = (fresh q1 in (((y122 == O & ____doStepCheckAnswer_ y120 y121) | (y122 == S (q1) & ____________eqNatDoStepCheckAnswer_ y120 y121 q1))));

____doStepCheckAnswer_ y124 y125 = (fresh q1 in (((y125 == S (S (q1)) & __checkAnswer_ y124 (S (q1))) | __________greaterCheckAnswer_ y124 y125)));

____________eqNatDoStepCheckAnswer_ y127 y128 y129 = (fresh q1 in (((y129 == O & _____doStepCheckAnswer_ y127 y128) | (y129 == S (S (q1)) & ______doStepCheckAnswer_ y127 y128 q1))));

_____doStepCheckAnswer_ y131 y132 = (fresh q1 in (((y132 == S (q1) & __checkAnswer_ y131 (S (q1))) | (y132 == O & ____checkAnswer_ y131 (S (S (S (S (O)))))))));

______doStepCheckAnswer_ y134 y135 y136 = (__addCheckAnswer_ y134 y135 y136 | _addGreaterCheckAnswer_ y134 y135 y136);

__addCheckAnswer_ y137 y138 y139 = (fresh q1 in (((y139 == O & __checkAnswer_ y137 (S (S (y138)))) | (y139 == S (q1) & __addCheckAnswer_ y137 (S (y138)) q1))));

_addGreaterCheckAnswer_ y141 y142 y144 = (fresh q1 in (((y144 == O & ________greaterCheckAnswer_ y141 (S (S (S (S (S (y142))))))) | (y144 == S (q1) & _addGreaterCheckAnswer_ y141 (S (y142)) q1))));

___add y147 y149 = (y149 == S (S (S (S (S (S (S (y147))))))) | ___add y147 (S (y149)));

_____checkAnswer_ y150 y151 = (fresh q1, q2 in (((y150 == [] & _____isFinishState y151) | (y150 == (q1 :: q2) & __checkStepDoStepCheckAnswer_ y151 (S (S (S (S (S (S (S (S (S (O)))))))))) q1 q2))));

_____isFinishState y152 = y152 == S (S (S (S (S (S (S (O)))))));


? checkAnswer x0