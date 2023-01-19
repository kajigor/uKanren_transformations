add x y z =
  x == O & z == y |
  (fresh x', z' in
    x == S x' & z == S z' & add x' y z');


greater a b r =
  a == O & r == Falso |
  (fresh x in
    a == S x &
      (b == O & r == Trueo |
      (fresh y in
        b == S y & greater x y r)));

sub a b r =
  b == O & a == r |
  (fresh y in
    b == S y &
     (a == O & r == O |
      (fresh x in
        a == S x & sub x y r))
  );

anotherBottle b c =
  b == Fst & c == Snd |
  b == Snd & c == Fst;

createEnv bottle lvl1 lvl2 env =
  bottle == Fst & env == Pair lvl1 lvl2 |
  bottle == Snd & env == Pair lvl2 lvl1;

fst' pair x =
  fresh y in
    pair == Pair x y;

snd' pair y =
  fresh x in
    pair == Pair x y;

definition q res =
  capacities1 q & checkAnswer res q (S (S (S (S (S (S (S O))))))) Trueo;

getCapacity capacities bottle r =
  bottle == Fst & fst' capacities r |
  bottle == Snd & snd' capacities r;

fancyEq a b r =
  a == O &
    (b == O & r == Trueo |
    (fresh b' in b == S b' & r == Falso)) |
  (fresh x in
    a == S x &
    (b == O & r == Falso |
    (fresh y in
      b == S y & fancyEq x y r)));

isFinishEnv env0 reqLvl r =
  fresh f, s in
    env0 == Pair f s &
    (fresh fReq, sReq in
      fancyEq f reqLvl fReq &
      fancyEq s reqLvl sReq &
      ( fReq == Trueo & r == Trueo |
        fReq == Falso & r == sReq
      ));

checkAnswer answer capacities reqLvl r =
  checkAnswer' (Pair O O) answer capacities reqLvl r;

checkAnswer' env0 answer capacities reqLvl r =
  answer == Nil & isFinishEnv env0 reqLvl r |
  (fresh x, xs in
    answer == Cons x xs &
    (fresh stepValid in
      checkStep env0 x capacities stepValid &
      ( stepValid == Trueo &
        (fresh env1 in
          doStep env0 x capacities env1 &
          checkAnswer' env1 xs capacities reqLvl r)
      ) |
      ( stepValid == Falso & r == Falso )
      )
    );

capacities1 q0 = q0 == Pair (S (S (S (S O)))) (S (S (S (S (S (S (S (S (S O)))))))));

doStep env0 step0 capacities env1 =
  (fresh f, s in
     env0 == Pair f s &
     (fresh t, b in
       step0 == Pair t b &
       (( t == Fill &
          (fresh lvl1, lvl2 in
             getCapacity capacities b lvl1 &
             ( b == Fst & s == lvl2 |
               b == Snd & f == lvl2
             ) &
             createEnv b lvl1 lvl2 env1
           )) |
         ( t == Empty &
           (fresh lvl2 in
              b == Fst & s == lvl2 |
              b == Snd & f == lvl2
           )) &
          createEnv b O lvl2 env1 |
         ( t == Pour &
           (fresh gr in
              (fresh sum, cap in
                (add f s sum) &
                (fresh b' in
                  (anotherBottle b b') &
                  (getCapacity capacities b' cap)) &
                (greater sum cap gr)
              ) &
              ( (gr == Trueo &
                (fresh lvl1, lvl2 in
                  (fresh sum, cap in
                    add f s sum &
                    (fresh b' in
                      (anotherBottle b b') &
                      (getCapacity capacities b' cap)) &
                    sub sum cap lvl1) &
                  (fresh b' in
                    (anotherBottle b b') &
                    (getCapacity capacities b' lvl2)) &
                  (createEnv b lvl1 lvl2 env1))) |
                (gr == Falso &
                (fresh sum in
                  add f s sum &
                  createEnv b O sum env1))))
         )
       )
     )
    );


checkStep env0 step0 capacities result =
  fresh f, s in
    env0 == Pair f s &
    (fresh t, b in
      step0 == Pair t b &
      (t == Fill &
        (fresh lvl in
          (b == Fst & f == lvl |
           b == Snd & s == lvl) &
          fancyEq lvl O result)) |
      (t == Empty &
        (fresh lvl1, lvl2 in
          (b == Fst & f == lvl1 |
           b == Snd & s == lvl1) &
          (getCapacity capacities b lvl2) &
          (fancyEq lvl1 lvl2 result))) |
      (t == Pour &
        (fresh q62 in
          (fresh q66, q67 in
            (fresh lvl in
              (b == Fst & f == lvl |
               b == Snd & s == lvl) &
               fancyEq lvl O q66
              ) &
            (fresh lvl', cap in
              (b == Fst & s == lvl' |
               b == Snd & f == lvl') &
              (fresh b' in
                anotherBottle b b' &
                getCapacity capacities b' cap) &
              (fancyEq lvl' cap q67)
              ) &
            (q66 == Trueo & q62 == Trueo |
             q66 == Falso & q62 == q67)
            ) &
          (q62 == Trueo & result == Falso |
           q62 == Falso & result == Trueo))));

query q res =
  capacities1 q &
  checkAnswer res q (S (S (S (S (S (S (S O))))))) Trueo;


? query q res

-- query = Program bottles $ fresh ["a", "b", "c"] (call "checkAnswer" [V "a", V "b", V "c", true])

-- query' =
--     Program definition $ fresh ["q", "res"] (call "query" [V "q", V "res"])

-- queryEq = Program fancyEq $ fresh ["x", "y"] (call "fancyEq" [V "x", V "y", true])

