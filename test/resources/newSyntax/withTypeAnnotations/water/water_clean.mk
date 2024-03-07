
eqNat a b r =
  a == O &
    (b == O & r == Trueo |
    (fresh r1 in b == S r1 & r == Falso))
  |
  (fresh x in
    a == S x &
    (b == O & r == Falso |
     (fresh y in b == S y & eqNat x y r)));

add a b r =
  a == O & b == r |
  (fresh x in a == S x & add x (S b) r);

greater a b r =
  a == O & r == Falso |
  (fresh x in
    a == S x &
    (b == O & r == Trueo |
     (fresh y in b == S y & greater x y r)));

sub a b r =
  b == O & a == r |
  (fresh y in
    b == S y &
    (a == O & r == O |
     (fresh x in a == S x & sub x y r)));

anotherBottle b b' =
  b == Fst & b' == Snd |
  b == Snd & b' == Fst;

createState bottle lvl1 lvl2 state =
  bottle == Fst & state == Pair lvl1 lvl2 |
  bottle == Snd & state == Pair lvl2 lvl1;

capacities b cap =
  b == Fst & cap == S (S (S (S O))) |
  b == Snd & cap == S (S (S (S (S (S (S (S (S O)))))))) ;

checkStep state0 step0 isStepValid =
  fresh f, s, t, b, lvl1, lvl2 in
    state0 == Pair f s &
    (b == Fst & f == lvl1 | b == Snd & s == lvl1) &
    (b == Fst & s == lvl2 | b == Snd & f == lvl2) &
    ( t == Fill & eqNat lvl1 O isStepValid |
      (fresh q44 in t == Empt & capacities b q44 & eqNat lvl1 q44 isStepValid) |
      (fresh b', q47, lvl1IsO, lvl2IsCap, b'Cap in
        t == Pour &
        anotherBottle b b' &
        eqNat lvl1 O lvl1IsO &
        capacities b' b'Cap &
        eqNat lvl2 b'Cap lvl2IsCap &
        (lvl1IsO == Trueo & q47 == Trueo | lvl1IsO == Falso & q47 == lvl2IsCap) &
        (q47 == Trueo & isStepValid == Falso | q47 == Falso & isStepValid == Trueo)) & 
    step0 == Pair t b);

doStep state0 step0 state =
  fresh f, s, t, b, lvl2 in
    state0 == Pair f s &
    step0 == Pair t b &
    (b == Fst & s == lvl2 | b == Snd & f == lvl2) &
    ( (fresh cap in t == Fill & capacities b cap & createState b cap lvl2 state) |
      t == Empt & createState b O lvl2 state |
      (fresh sum, cap2, b', isGreater in
        t == Pour &
        add f s sum &
        anotherBottle b b' &
        capacities b' cap2 &
        greater sum cap2 isGreater &
        ((fresh diff in isGreater == Trueo & sub sum cap2 diff & createState b diff cap2 state) |
        isGreater == Falso & createState b O sum state)));

isFinishState state0 reqLvl r =
  fresh f, s, isFReq, isSReq in
    state0 == Pair f s &
    eqNat f reqLvl isFReq &
    eqNat s reqLvl isSReq &
    (isFReq == Trueo & r == Trueo | isFReq == Falso & r == isSReq);

checkAnswer answer reqLvl r =
  fresh startState in startState == Pair O O & checkAnswer' startState answer reqLvl r;

checkAnswer' state0 answer reqLvl result =
  (answer == Nil & isFinishState state0 reqLvl result) |
  (fresh x, xs, isStepValid in
    (checkStep state0 x isStepValid) &
    ((fresh state' in
       isStepValid == Trueo &
       doStep state0 x state' &
       checkAnswer' state' xs reqLvl result) |
      isStepValid == Falso & result == Falso) &
    (answer == Cons x xs));

? checkAnswer answer (S (S (S (S (S (S (S O))))))) Trueo