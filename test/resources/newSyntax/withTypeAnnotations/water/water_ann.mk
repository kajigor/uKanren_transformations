filter (static dynamic static static)
 checkAnswer' state0 answer reqLvl result =
  ((answer == [] &
    Unfold isFinishState state0 reqLvl result) |
  ((fresh x, xs, isStepValid in
    ((((fresh state' in
        ((isStepValid == Trueo &
          Unfold doStep state0 x state' &
          Memo checkAnswer' state' xs reqLvl result))) |
         (isStepValid == Falso & result == Falso))))) &
        answer == (x :: xs)));

filter (dynamic static static)
 checkAnswer answer reqLvl r =
  (fresh startState in
    ((startState == Pair Zero Zero &
      Unfold checkAnswer' startState answer reqLvl r)));

filter (static static static)
 isFinishState state0 reqLvl r =
  (fresh f, s, isFReq, isSReq in
    ((state0 == Pair f s &
      Unfold eqNat f reqLvl isFReq &
      Unfold eqNat s reqLvl isSReq &
      ((isFReq == Trueo & r == Trueo) |
       (isFReq == Falso & r == isSReq)))));

filter (static static dynamic)
 doStep state0 step0 state =
  (fresh f, s, t, b, lvl2 in
    ((state0 == Pair f s &
      step0 == Pair t b &
      ((b == Fst & s == lvl2) |
       (b == Snd & f == lvl2)) &
      ((fresh cap in
        ((t == Fill &
          Unfold capacities b cap &
          Unfold createState b cap lvl2 state))) |
       (t == Empt & Unfold createState b Zero lvl2 state) |
       (fresh sum, cap2, b', isGreater in
        ((t == Pour &
          Unfold add f s sum &
          Unfold anotherBottle b b' &
          Unfold capacities b' cap2 &
          Unfold greater sum cap2 isGreater &
          ((fresh diff in
            ((isGreater == Trueo &
              Unfold sub sum cap2 diff &
              Unfold creat`eState b diff cap2 state))) |
           (isGreater == Falso &
            Unfold createState b Zero sum state)))))))));

filter (static dynamic dynamic)
 checkStep state0 step0 isStepValid =
   (fresh f, s, t, b, lvl1, lvl2 in
    ((state0 == Pair f s &
     ((b == Fst & f == lvl1) | 
      (b == Snd & s == lvl1)) & 
     ((b == Fst & s == lvl2) | 
      (b == Snd & f == lvl2)) & 
     ((t == Fill) | 
        ((t == Empt)) | 
        (fresh b', q47, lvl1IsO, lvl2IsCap, b'Cap in 
          ((t == Pour & Unfold anotherBottle b b' & Unfold capacities b' b'Cap & 
          ((lvl1IsO == Trueo & q47 == Trueo) | (lvl1IsO == Falso & q47 == lvl2IsCap)) & 
          Unfold eqNat lvl1 Zero lvl1IsO & 
          ((q47 == Trueo & isStepValid == Falso) | (q47 == Falso & isStepValid == Trueo)) & 
          Unfold eqNat lvl2 b'Cap lvl2IsCap)))) & step0 == Pair t b)));


filter (static dynamic)
 capacities b cap =
  ((b == Fst & cap == Succ (Succ (Succ (Succ Zero)))) |
   (b == Snd & cap == Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))))));
filter (static static dynamic dynamic)
 createState bottle lvl1 lvl2 state = ((bottle == Fst & state == Pair lvl1 lvl2) | (bottle == Snd & state == Pair lvl2 lvl1));
filter (dynamic static)
 anotherBottle b b' = ((b == Fst & b' == Snd) | (b == Snd & b' == Fst));

filter (static static dynamic)
 sub a b r = ((b == Zero & a == r) | (fresh y in ((b == Succ y & ((a == Zero & r == Zero) | (fresh x in ((a == Succ x & Unfold sub x y r))))))));

filter (static static dynamic)
 greater a b r = ((a == Zero & r == Falso) | (fresh x in ((a == Succ x & ((b == Zero & r == Trueo) | (fresh y in ((b == Succ y & Unfold greater x y r))))))));

filter (static static dynamic)
 add a b r = (
    (a == Zero & b == r) | 
    (fresh x in 
      ((a == Succ x & Unfold add x (Succ b) r))));

filter (dynamic static static)
 eqNat a b r = ((a == Zero & ((b == Zero & r == Trueo) | (fresh r1 in ((b == Succ r1 & r == Falso))))) | (fresh x in ((a == Succ x & ((b == Zero & r == Falso) | (fresh y in ((b == Succ y & Unfold eqNat x y r))))))));

filter ()
 fail  = Memo fail [];

(fresh answer in (Unfold checkAnswer answer (Succ (Succ (Succ (Succ (Succ (Succ (Succ Zero))))))) Trueo))