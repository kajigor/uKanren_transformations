eqNat a b q155 =
  (a == O &
     ( b == O & q155 == Trueo |
       (fresh q158 in b == S q158 & q155 == Falso))) |
  (fresh x in
    a == S x &
      ( b == O & q155 == Falso |
        (fresh y in b == S y & eqNat x y q155)));

notEqNat a b q148 =
  (a == O &
     ( b == O & q148 == Falso |
       (fresh q151 in b == S q151 & q148 == Trueo))) |
  (fresh x in
    a == S x &
      ( b == O & q148 == Trueo |
        (fresh y in b == S y & notEqNat x y q148)));

add a b q146 =
   a == O & b == q146 |
  (fresh x in a == S x & add x (S b) q146);

geo a b q143 =
   a == O & eqNat O b q143 |
  (fresh x in
    a == S x &
    ( b == O & q143 == Trueo |
      (fresh y in b == S y & geo x y q143)));

sub a b q139 =
  (b == O & a == q139) |
  (fresh y in
    b == S y &
    ( a == O & q139 == O |
      (fresh x in a == S x & sub x y q139)));

elem l n q136 =
  (fresh x, xs in
    l == Cons x xs &
    ( n == O & x == q136 |
      (fresh m in n == S m & elem xs m q136)));

changeElem l n f q130 =
  (fresh x, xs in
    l == Cons x xs &
    ( (fresh q132 in
        n == O &
        q130 == Cons q132 xs &
        f x q132) |
      (fresh m, q134 in
        n == S m &
        q130 == Cons x q134 &
        changeElem xs m f q134)));

checkStep step state len cop q69 =
  (fresh pos, fuel, sts in
    state == St pos fuel sts &
    ((fresh d, q71, q72, q77, q78 in
        step == Left d &
        geo pos d q71 &
        geo fuel d q77 &
        ( d == O & q78 == Falso |
          q78 == Trueo & neq d O) &
        ( q77 == Falso & q72 == Falso |
          q77 == Trueo & q72 == q78) &
        ( q71 == Falso & q69 == Falso |
          q71 == Trueo & q69 == q72)) |
     (fresh d, q86, q87, q92, q94, q95 in
       step == Right d &
       add pos d q92 &
       geo len q92 q86 &
       geo fuel d q94 &
       ( d == O & q95 == Falso |
         q95 == Trueo & neq d O) &
       ( q94 == Falso & q87 == Falso |
         q94 == Trueo & q87 == q95) &
       ( q86 == Falso & q69 == Falso |
         q86 == Trueo & q69 == q87)) |
     (fresh f, q103, q104, q109, q110, q115, q116 in
       step == Pour f &
       notEqNat pos len q103 &
       notEqNat O pos q109 &
       notEqNat O f q115 &
       geo fuel f q116 &
       ( q115 == Falso & q110 == Falso |
         q115 == Trueo & q110 == q116) &
       ( q109 == Falso & q104 == Falso |
         q109 == Trueo & q104 == q110) &
       ( q103 == Falso & q69 == Falso |
         q103 == Trueo & q69 == q104)) |
     (step == Fill &
       ((pos == O & notEqNat fuel cop q69) |
        (fresh x, q122, q123, q128 in
          pos == S x &
          notEqNat fuel cop q122 &
          elem sts x q128 &
          notEqNat O q128 q123 &
          ( q122 == Falso & q69 == Falso |
            q122 == Trueo & q69 == q123))))));

step step state len cop q46 =
  (fresh pos, fuel, sts in
    state == St pos fuel sts &
    ( (fresh d, q48, q49 in
         step == Left d &
         q46 == St q48 q49 sts &
         sub pos d q48 &
         sub fuel d q49) |
      (fresh d, q51, q52 in
         step == Right d &
         q46 == St q51 q52 sts &
         add pos d q51 &
         sub fuel d q52) |
      (fresh f, x, q55, q56 in
         step == Pour f &
         pos == S x &
         q46 == St pos q55 q56 &
         sub fuel f q55 &
         changeElem sts x (fun e -> add f e) q56) |
      (step == Fill &
        ( pos == O & q46 == St pos cop sts |
          (fresh x, stationFuel, totalFuel, q63 in
             pos == S x &
             elem sts x stationFuel &
             add fuel stationFuel totalFuel &
             geo totalFuel cop q63 &
             ( (fresh q64 in
                 q63 == Trueo &
                 q46 == St pos cop q64 &
                 changeElem sts x (fun e -> sub totalFuel cop) q64) |
               (fresh q66 in
                 q63 == Falso &
                 q46 == St pos totalFuel q66 &
                 changeElem sts x (fun e -> fun q68 -> q68 == O) q66)))))));

isFinishState state len q45 =
  (fresh pos, fuel, sts in
    state == St pos fuel sts &
    eqNat pos len q45);

getFuel step state cop q38 =
  (fresh d in step == Left d & q38 == O) |
  (fresh d in step == Right d & q38 == O) |
  (fresh f in step == Pour f & q38 == O) |
  (fresh pos, fuel, sts in
    (step == Fill) &
    (state == St pos fuel sts) &
    ( pos == O & sub cop fuel q38 |
      (fresh x in pos == S x & q38 == O)))

isMove step q33 =
  (fresh x in step == Left x & q33 == Trueo) |
  (fresh x in step == Right x & q33 == Trueo) |
  step == Fill & q33 == Falso |
  (fresh x in step == Pour x & q33 == Falso);

checkAnswer answer len cop q32 =
  (fresh startState in
    (fresh q28 in startState == St O cop q28 & stations len q28) &
    calcFuel startState answer Falso len cop q32);

calcFuel state ans prevIsMove len cop q0 =
  (fresh q2 in
      ans == Nil &
      isFinishState state len q2 &
      ( q2 == Trueo & q0 == Some cop |
        q2 == Falso & q0 == None)) |
  (fresh x, xs, currIsMove, q7 in
    ans == Cons x xs &
    isMove x currIsMove &
    ( prevIsMove == currIsMove & q7 == Trueo |
      q7 == Falso & neq prevIsMove currIsMove) &
    ( q7 == Trueo & q0 == None |
      (fresh q10 in
        q7 == Falso &
        checkStep x state len cop q10 &
        ( (fresh q12, q18 in
            q10 == Trueo &
            step x state len cop q18 &
            calcFuel q18 xs currIsMove len cop q12 &
            ( q12 == None & q0 == None |
              (fresh res, q14, q16 in
                q12 == Some res &
                q0 == Some q14 &
                getFuel x state cop q16 &
                add q16 res q14))) |
            q10 == Falso & q0 == None))));

stations n q24 =
  n == O & q24 == Nil |
  (fresh m, q26 in
    n == S m &
    q24 == Cons O q26 &
    stations m q26);

-- checkAnswer 7 5 (Some 15)
? checkAnswer q (S (S (S (S (S (S (S O))))))) (S (S (S (S (S O))))) (Some (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S O))))))))))))))))