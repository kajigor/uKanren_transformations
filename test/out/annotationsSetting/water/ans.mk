filter (static dynamic static static) 
 checkAnswer' state0 answer reqLvl result = ((answer == [] & Unfold isFinishState state0 reqLvl result) | ((fresh x, xs, isStepValid in ((Unfold checkStep state0 x isStepValid & ((fresh state' in ((isStepValid == Trueo & Unfold doStep state0 x state' & Memo checkAnswer' state' xs reqLvl result))) | (isStepValid == Falso & result == Falso))))) & answer == (x :: xs)));
filter (dynamic static static) 
 checkAnswer answer reqLvl r = (fresh startState in ((startState == (0, 0) & Unfold checkAnswer' startState answer reqLvl r)));
filter (static static static) 
 isFinishState state0 reqLvl r = (fresh f, s, isFReq, isSReq in ((state0 == (f, s) & Unfold eqNat f reqLvl isFReq & Unfold eqNat s reqLvl isSReq & ((isFReq == Trueo & r == Trueo) | (isFReq == Falso & r == isSReq)))));
filter (static static dynamic) 
 doStep state0 step0 state = (fresh f, s, t, b, lvl2 in ((state0 == (f, s) & step0 == (t, b) & ((b == Fst & s == lvl2) | (b == Snd & f == lvl2)) & ((fresh cap in ((t == Fill & Unfold capacities b cap & Unfold createState b cap lvl2 state))) | (t == Empt & Unfold createState b 0 lvl2 state) | (fresh sum, cap2, b', isGreater in ((t == Pour & Unfold add f s sum & Unfold anotherBottle b b' & Unfold capacities b' cap2 & Unfold greater sum cap2 isGreater & ((fresh diff in ((isGreater == Trueo & Unfold sub sum cap2 diff & Unfold createState b diff cap2 state))) | (isGreater == Falso & Unfold createState b 0 sum state)))))))));
filter (static dynamic dynamic) 
 checkStep state0 step0 isStepValid = (fresh f, s, t, b, lvl1, lvl2 in ((state0 == (f, s) & ((b == Fst & f == lvl1) | (b == Snd & s == lvl1)) & ((b == Fst & s == lvl2) | (b == Snd & f == lvl2)) & ((t == Fill & Unfold eqNat lvl1 0 isStepValid) | (fresh q44 in ((t == Empt & Unfold capacities b q44 & Unfold eqNat lvl1 q44 isStepValid))) | (fresh b', q47, lvl1IsO, lvl2IsCap, b'Cap in ((t == Pour & Unfold anotherBottle b b' & Unfold capacities b' b'Cap & ((lvl1IsO == Trueo & q47 == Trueo) | (lvl1IsO == Falso & q47 == lvl2IsCap)) & Unfold eqNat lvl1 0 lvl1IsO & ((q47 == Trueo & isStepValid == Falso) | (q47 == Falso & isStepValid == Trueo)) & Unfold eqNat lvl2 b'Cap lvl2IsCap)))) & step0 == (t, b))));
filter (static dynamic) 
 capacities b cap = ((b == Fst & cap == 4) | (b == Snd & cap == 9));
filter (static static dynamic dynamic) 
 createState bottle lvl1 lvl2 state = ((bottle == Fst & state == (lvl1, lvl2)) | (bottle == Snd & state == (lvl2, lvl1)));
filter (dynamic static) 
 anotherBottle b b' = ((b == Fst & b' == Snd) | (b == Snd & b' == Fst));
filter (static static dynamic) 
 sub a b r = ((b == 0 & a == r) | (fresh y in ((b == (1 + y) & ((a == 0 & r == 0) | (fresh x in ((a == (1 + x) & Unfold sub x y r))))))));
filter (static static dynamic) 
 greater a b r = ((a == 0 & r == Falso) | (fresh x in ((a == (1 + x) & ((b == 0 & r == Trueo) | (fresh y in ((b == (1 + y) & Unfold greater x y r))))))));
filter (static static dynamic) 
 add a b r = ((a == 0 & b == r) | (fresh x in ((a == (1 + x) & Unfold add x ((1 + b)) r))));
filter (dynamic static static) 
 eqNat a b r = ((a == 0 & ((b == 0 & r == Trueo) | (fresh r1 in ((b == (1 + r1) & r == Falso))))) | (fresh x in ((a == (1 + x) & ((b == 0 & r == Falso) | (fresh y in ((b == (1 + y) & Unfold eqNat x y r))))))));
filter () 
 fail  = Memo fail [];

(fresh answer in (Unfold checkAnswer answer 7 Trueo))