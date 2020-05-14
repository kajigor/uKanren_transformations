open GT
open OCanren
open OCanren.Std
open General




let topLevel x0 x1 x2 =
  let rec checkAnswer y0 y1 y2 =
    fresh
      (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40
         q41)
      ( y0 === Std.List.nil ()
      &&& (y2 === o ())
      ||| ( y0
          === Std.( % ) (Obj.magic q1) (Obj.magic q2)
          &&& ( y1
              === Std.Pair.pair (o ()) (Obj.magic q3)
              ||| (y1 === Std.Pair.pair (Obj.magic q4) (Obj.magic q5) &&& fancyEq (Obj.magic q5))
              &&& ( y1
                  === Std.Pair.pair (Obj.magic q6) (Obj.magic q7)
                  ||| (y1 === Std.Pair.pair (Obj.magic q8) (Obj.magic q6))
                  ||| ( y1
                      === Std.Pair.pair (Obj.magic q8) (Obj.magic q9)
                      ||| (y1 === Std.Pair.pair (Obj.magic q9) (Obj.magic q7) &&& addGreaterAdd (Obj.magic q9)) )
                  &&& ( y2 === o ()
                      ||| (y2 === s (Obj.magic q10) &&& (Obj.magic q10 === o () ||| (Obj.magic q10 === s (Obj.magic q11) &&& _fancyEq (Obj.magic q11))))
                      &&& (y2 === o () ||| (y2 === s (Obj.magic q12)) ||| (y2 === o ()) ||| (y2 === s (Obj.magic q13) ))
                      ||| ( y2
                          === s (Obj.magic q14)
                          &&& _fancyEq (Obj.magic q14)
                          ||| (y2 === o ())
                          ||| (y2 === s (s (Obj.magic q15)) &&& ___fancyEq (Obj.magic q15))
                          ||| (y2 === s (o ()))
                          ||| (y2 === s (s (Obj.magic q16)) &&& (_____fancyEq (Obj.magic q17))) )
                      ||| ( Obj.magic q18
                          === Std.Pair.pair (Obj.magic q19) (Obj.magic q20)

                          ||| (Obj.magic q18 === Std.Pair.pair (Obj.magic q19) (Obj.magic q20))
                          ||| ( Obj.magic q18
                              === Std.Pair.pair (Obj.magic q19) (Obj.magic q20)
                              &&& (y1 === Std.Pair.pair (Obj.magic q21) (Obj.magic q22) &&& _fancyEq (Obj.magic q21)) )
                          ||| ( Obj.magic q18
                              === Std.Pair.pair (Obj.magic q19) (Obj.magic q20)
                              &&& (y1 === Std.Pair.pair (Obj.magic q23) (Obj.magic q21) &&& _fancyEq (Obj.magic q21)) )
                          ||| ( Obj.magic q18
                              === Std.Pair.pair (s (Obj.magic q24)) (Obj.magic q20)
                              &&& (y1 === Std.Pair.pair (Obj.magic q23) (Obj.magic q25) &&& _____fancyEq (Obj.magic q25)) )
                          ||| ( Obj.magic q18
                              === Std.Pair.pair (Obj.magic q19) (Obj.magic q20)
                              &&& (y1 === Std.Pair.pair (Obj.magic q25) (Obj.magic q22) &&& fancyEqFancyEq (Obj.magic q25)) )
                          &&& ( Obj.magic q18
                              === Std.Pair.pair (Obj.magic q26) (Obj.magic q27)
                              &&& (y1 === Std.Pair.pair (Obj.magic q28) (Obj.magic q29))
                              ||| (Obj.magic q18 === Std.Pair.pair (Obj.magic q26) (Obj.magic q27) &&& (y1 === Std.Pair.pair (Obj.magic q30) (Obj.magic q28)))
                              ||| (Obj.magic q18 === Std.Pair.pair (Obj.magic q26) (Obj.magic q27))
                              ||| (Obj.magic q18 === Std.Pair.pair (Obj.magic q26) (Obj.magic q27))
                              ||| ( Obj.magic q18
                                  === Std.Pair.pair (Obj.magic q26) (Obj.magic q27)
                                  &&& ( y1
                                      === Std.Pair.pair (Obj.magic q30) (Obj.magic q31)
                                      &&& ( Obj.magic q26 === o ()
                                          &&& (Obj.magic q27 === Obj.magic q32)
                                          ||| ( Obj.magic q26
                                              === s (Obj.magic q33)
                                              &&& (Obj.magic q32 === s (Obj.magic q34))
                                              &&& add (Obj.magic q33) (Obj.magic q27) (Obj.magic q34) )
                                          &&& (Obj.magic q32 === s (Obj.magic q35) ||| (Obj.magic q32 === s (Obj.magic q35) &&& greater (Obj.magic q35)))
                                          &&& add (Obj.magic q26) (Obj.magic q27) (Obj.magic q36)
                                          &&& ( Obj.magic q37 === Obj.magic q36
                                              ||| (Obj.magic q36 === o () &&& (Obj.magic q37 === o ()))
                                              ||| (Obj.magic q36 === s (Obj.magic q38) &&& sub (Obj.magic q37) (Obj.magic q38)) )
                                          &&& createState (Obj.magic q37) )
                                      ||| ( y1
                                          === Std.Pair.pair (Obj.magic q31) (Obj.magic q29)
                                          &&& ( add (Obj.magic q26) (Obj.magic q27) (Obj.magic q32)
                                              &&& greater (Obj.magic q32)
                                              &&& add (Obj.magic q26) (Obj.magic q27) (Obj.magic q36)
                                              &&& sub (Obj.magic q37) (Obj.magic q36)
                                              &&& _createState (Obj.magic q37) ) ) ) )
                              ||| ( Obj.magic q18
                                  === Std.Pair.pair (Obj.magic q26) (Obj.magic q27)
                                  &&& ( y1
                                      === Std.Pair.pair (Obj.magic q30) (Obj.magic q39)
                                      &&& ( add (Obj.magic q26) (Obj.magic q27) (Obj.magic q32)
                                          &&& (Obj.magic q32 === o () ||| (Obj.magic q32 === s (Obj.magic q40) &&& _greater (Obj.magic q40)))
                                          &&& add (Obj.magic q26) (Obj.magic q27) (Obj.magic q41)
                                          &&& __createState (Obj.magic q41) )
                                      ||| ( y1
                                          === Std.Pair.pair (Obj.magic q39) (Obj.magic q29)
                                          &&& ( add (Obj.magic q26) (Obj.magic q27) (Obj.magic q32)
                                              &&& _greater (Obj.magic q32)
                                              &&& add (Obj.magic q26) (Obj.magic q27) (Obj.magic q41)
                                              &&& ___createState (Obj.magic q41) ) ) ) ) )
                          &&& checkAnswer_ y1 y2 ) ) ) ) ) )
  and fancyEq y3 = y3 === o ()
  and addGreaterAdd y6 = success
  and _fancyEq y8 = fresh (q1) (y8 === o () ||| (y8 === s (Obj.magic q1) &&& _fancyEq (Obj.magic q1)))
  and ___fancyEq y13 = _fancyEq y13
  and _____fancyEq y16 = fresh (q1 q2) (y16 === s (Obj.magic q1) ||| (y16 === o ()) ||| (y16 === s (Obj.magic q2) &&& _____fancyEq (Obj.magic q2)))
  and fancyEqFancyEq y21 = _____fancyEq y21
  and add y22 y23 y24 =
    fresh (q1 q2) (y22 === o () &&& (y23 === y24) ||| (y22 === s (Obj.magic q1) &&& (y24 === s (Obj.magic q2)) &&& add (Obj.magic q1) y23 (Obj.magic q2)))
  and greater y25 = fresh (q1) (y25 === s (Obj.magic q1) ||| (y25 === s (Obj.magic q1) &&& greater (Obj.magic q1)))
  and sub y27 y28 = fresh (q1) (y27 === y28 ||| (y28 === o () &&& (y27 === o ())) ||| (y28 === s (Obj.magic q1) &&& sub y27 (Obj.magic q1)))
  and createState y32 = success
  and _createState y35 = success
  and _greater y36 = fresh (q1) (y36 === o () ||| (y36 === s (Obj.magic q1) &&& _greater (Obj.magic q1)))
  and __createState y39 = success
  and ___createState y41 = success
  and checkAnswer_ y42 y43 =
    fresh
      (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32)
      ( y43 === o ()
      ||| (y43 === s (Obj.magic q1) &&& (Obj.magic q1 === o () ||| (Obj.magic q1 === s (Obj.magic q2) &&& _fancyEq (Obj.magic q2))))
      &&& (y43 === o () ||| (y43 === s (Obj.magic q3)) ||| (y43 === o ()) ||| (y43 === s (Obj.magic q4)))
      ||| ( y43
          === s (Obj.magic q5)
          &&& _fancyEq (Obj.magic q5)
          ||| (y43 === o ())
          ||| (y43 === s (s (Obj.magic q6)) &&& ___fancyEq (Obj.magic q6))
          ||| (y43 === s (o ()))
          ||| (y43 === s (s (Obj.magic q7)) &&& (_____fancyEq (Obj.magic q8) )) )
      ||| ( Obj.magic q9
          === Std.Pair.pair (Obj.magic q10) (Obj.magic q11)
          ||| (Obj.magic q9 === Std.Pair.pair (Obj.magic q10) (Obj.magic q11))
          ||| ( Obj.magic q9
              === Std.Pair.pair (Obj.magic q10) (Obj.magic q11)
              &&& (y42 === Std.Pair.pair (Obj.magic q12) (Obj.magic q13) &&& _fancyEq (Obj.magic q12)) )
          ||| ( Obj.magic q9
              === Std.Pair.pair (Obj.magic q10) (Obj.magic q11)
              &&& (y42 === Std.Pair.pair (Obj.magic q14) (Obj.magic q12) &&& _fancyEq (Obj.magic q12)) )
          ||| ( Obj.magic q9
              === Std.Pair.pair (s (Obj.magic q15)) (Obj.magic q11)
              &&& (y42 === Std.Pair.pair (Obj.magic q14) (Obj.magic q16) &&& _____fancyEq (Obj.magic q16)) )
          ||| ( Obj.magic q9
              === Std.Pair.pair (Obj.magic q10) (Obj.magic q11)
              &&& (y42 === Std.Pair.pair (Obj.magic q16) (Obj.magic q13) &&& fancyEqFancyEq (Obj.magic q16)) )
          &&& ( Obj.magic q9
              === Std.Pair.pair (Obj.magic q17) (Obj.magic q18)
              &&& (y42 === Std.Pair.pair (Obj.magic q19) (Obj.magic q20))
              ||| (Obj.magic q9 === Std.Pair.pair (Obj.magic q17) (Obj.magic q18) &&& (y42 === Std.Pair.pair (Obj.magic q21) (Obj.magic q19)))
              ||| (Obj.magic q9 === Std.Pair.pair (Obj.magic q17) (Obj.magic q18))
              ||| (Obj.magic q9 === Std.Pair.pair (Obj.magic q17) (Obj.magic q18))
              ||| ( Obj.magic q9
                  === Std.Pair.pair (Obj.magic q17) (Obj.magic q18)
                  &&& ( y42
                      === Std.Pair.pair (Obj.magic q21) (Obj.magic q22)
                      &&& ( Obj.magic q17 === o ()
                          &&& (Obj.magic q18 === Obj.magic q23)
                          ||| ( Obj.magic q17
                              === s (Obj.magic q24)
                              &&& (Obj.magic q23 === s (Obj.magic q25))
                              &&& add (Obj.magic q24) (Obj.magic q18) (Obj.magic q25) )
                          &&& (Obj.magic q23 === s (Obj.magic q26) ||| (Obj.magic q23 === s (Obj.magic q26) &&& greater (Obj.magic q26)))
                          &&& add (Obj.magic q17) (Obj.magic q18) (Obj.magic q27)
                          &&& ( Obj.magic q28 === Obj.magic q27
                              ||| (Obj.magic q27 === o () &&& (Obj.magic q28 === o ()))
                              ||| (Obj.magic q27 === s (Obj.magic q29) &&& sub (Obj.magic q28) (Obj.magic q29)) )
                          &&& createState (Obj.magic q28) )
                      ||| ( y42
                          === Std.Pair.pair (Obj.magic q22) (Obj.magic q20)
                          &&& ( add (Obj.magic q17) (Obj.magic q18) (Obj.magic q23)
                              &&& greater (Obj.magic q23)
                              &&& add (Obj.magic q17) (Obj.magic q18) (Obj.magic q27)
                              &&& sub (Obj.magic q28) (Obj.magic q27)
                              &&& _createState (Obj.magic q28) ) ) ) )
              ||| ( Obj.magic q9
                  === Std.Pair.pair (Obj.magic q17) (Obj.magic q18)
                  &&& ( y42
                      === Std.Pair.pair (Obj.magic q21) (Obj.magic q30)
                      &&& ( add (Obj.magic q17) (Obj.magic q18) (Obj.magic q23)
                          &&& (Obj.magic q23 === o () ||| (Obj.magic q23 === s (Obj.magic q31) &&& _greater (Obj.magic q31)))
                          &&& add (Obj.magic q17) (Obj.magic q18) (Obj.magic q32)
                          &&& __createState (Obj.magic q32) )
                      ||| ( y42
                          === Std.Pair.pair (Obj.magic q30) (Obj.magic q20)
                          &&& ( add (Obj.magic q17) (Obj.magic q18) (Obj.magic q23)
                              &&& _greater (Obj.magic q23)
                              &&& add (Obj.magic q17) (Obj.magic q18) (Obj.magic q32)
                              &&& ___createState (Obj.magic q32) ) ) ) ) )
          &&& checkAnswer_ y42 y43 ) )
  in
  checkAnswer x0 x1 x2

(*
let topLevel x0 x1 x2 =
  let rec checkAnswer y0 y1 y2 =
    fresh
      (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30)
      ( y0 === Std.List.nil ()
      &&& (y2 === o ())
      ||| ( y0
          === Std.( % ) (Obj.magic q1) (Obj.magic q2)
          &&& ( Obj.magic q3 === o ()
              &&& (Obj.magic q4 === o ())
              &&& (Obj.magic q1 === Std.Pair.pair (Obj.magic q5) (Obj.magic q6))
              &&& (Obj.magic q5 === fill ())
              &&& (Obj.magic q6 === fst_ ())
              &&& (Obj.magic q7 === Obj.magic q3)
              ||| ( Obj.magic q3 === o ()
                  &&& (Obj.magic q4 === o ())
                  &&& (Obj.magic q1 === Std.Pair.pair (Obj.magic q5) (Obj.magic q6))
                  &&& (Obj.magic q5 === fill ())
                  &&& (Obj.magic q6 === snd_ ())
                  &&& (Obj.magic q7 === Obj.magic q4) )
              ||| ( Obj.magic q3 === o ()
                  &&& (Obj.magic q4 === o ())
                  &&& (Obj.magic q1 === Std.Pair.pair (Obj.magic q5) (Obj.magic q6))
                  &&& (Obj.magic q5 === empty ())
                  &&& (Obj.magic q6 === fst_ ())
                  &&& (Obj.magic q8 === Obj.magic q3)
                  &&& (y1 === Std.Pair.pair (Obj.magic q9) (Obj.magic q10) &&& (Obj.magic q11 === Obj.magic q9) &&& (Obj.magic q9 === o ())) )
              ||| ( Obj.magic q3 === o ()
                  &&& (Obj.magic q4 === o ())
                  &&& (Obj.magic q1 === Std.Pair.pair (Obj.magic q5) (Obj.magic q6))
                  &&& (Obj.magic q5 === empty ())
                  &&& (Obj.magic q6 === snd_ ())
                  &&& (Obj.magic q8 === Obj.magic q4)
                  &&& (y1 === Std.Pair.pair (Obj.magic q9) (Obj.magic q10) &&& (Obj.magic q11 === Obj.magic q10) &&& fancyEq (Obj.magic q10)) )
              &&& ( Obj.magic q3 === o ()
                  &&& (Obj.magic q4 === o ())
                  &&& (Obj.magic q1 === Std.Pair.pair (Obj.magic q5) (Obj.magic q6))
                  &&& (Obj.magic q5 === fill ())
                  &&& (Obj.magic q6 === fst_ ())
                  &&& (Obj.magic q8 === Obj.magic q4)
                  &&& ( y1
                      === Std.Pair.pair (Obj.magic q12) (Obj.magic q13)
                      &&& (Obj.magic q7 === Obj.magic q12)
                      &&& (Obj.magic q14 === Std.Pair.pair (Obj.magic q12) (o ())) )
                  ||| ( Obj.magic q3 === o ()
                      &&& (Obj.magic q4 === o ())
                      &&& (Obj.magic q1 === Std.Pair.pair (Obj.magic q5) (Obj.magic q6))
                      &&& (Obj.magic q5 === fill ())
                      &&& (Obj.magic q6 === snd_ ())
                      &&& (Obj.magic q8 === Obj.magic q3)
                      &&& ( y1
                          === Std.Pair.pair (Obj.magic q12) (Obj.magic q13)
                          &&& (Obj.magic q7 === Obj.magic q13)
                          &&& (Obj.magic q14 === Std.Pair.pair (o ()) (Obj.magic q13)) ) )
                  ||| ( Obj.magic q3 === o ()
                      &&& (Obj.magic q4 === o ())
                      &&& (Obj.magic q1 === Std.Pair.pair (Obj.magic q5) (Obj.magic q6))
                      &&& (Obj.magic q5 === empty ())
                      &&& (Obj.magic q6 === fst_ ())
                      &&& (Obj.magic q11 === Obj.magic q4)
                      &&& (Obj.magic q14 === Std.Pair.pair (o ()) (o ())) )
                  ||| ( Obj.magic q3 === o ()
                      &&& (Obj.magic q4 === o ())
                      &&& (Obj.magic q1 === Std.Pair.pair (Obj.magic q5) (Obj.magic q6))
                      &&& (Obj.magic q5 === empty ())
                      &&& (Obj.magic q6 === snd_ ())
                      &&& (Obj.magic q11 === Obj.magic q3)
                      &&& (Obj.magic q14 === Std.Pair.pair (o ()) (o ())) )
                  ||| ( Obj.magic q3 === o ()
                      &&& (Obj.magic q4 === o ())
                      &&& (Obj.magic q1 === Std.Pair.pair (Obj.magic q5) (Obj.magic q6))
                      &&& (Obj.magic q5 === pour ())
                      &&& (Obj.magic q15 === !!false)
                      &&& ( Obj.magic q6 === fst_ ()
                          &&& (Obj.magic q16 === snd_ ())
                          &&& ( y1
                              === Std.Pair.pair (Obj.magic q12) (Obj.magic q13)
                              &&& (Obj.magic q17 === Obj.magic q13)
                              &&& (Obj.magic q14 === Std.Pair.pair (o ()) (Obj.magic q18) &&& (Obj.magic q19 === o () &&& (Obj.magic q18 === o ()))) )
                          ||| ( Obj.magic q6 === snd_ ()
                              &&& (Obj.magic q16 === fst_ ())
                              &&& ( y1
                                  === Std.Pair.pair (Obj.magic q12) (Obj.magic q13)
                                  &&& (Obj.magic q17 === Obj.magic q12)
                                  &&& (Obj.magic q14 === Std.Pair.pair (Obj.magic q18) (o ()) &&& addGreaterAdd (Obj.magic q19) (Obj.magic q18) (Obj.magic q12))
                                  ) ) ) )
                  &&& ( Obj.magic q2 === Std.List.nil ()
                      &&& ( Obj.magic q14
                          === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                          &&& (Obj.magic q11 === !!true)
                          &&& ( Obj.magic q7 === o ()
                              &&& (y2 === o ())
                              ||| ( Obj.magic q7
                                  === s (Obj.magic q17)
                                  &&& (y2 === s (Obj.magic q16))
                                  &&& ( Obj.magic q17 === o ()
                                      &&& (Obj.magic q16 === o ())
                                      ||| ( Obj.magic q17
                                          === s (Obj.magic q20)
                                          &&& (Obj.magic q16 === s (Obj.magic q21))
                                          &&& _fancyEq (Obj.magic q20) (Obj.magic q21) ) ) )
                              &&& ( Obj.magic q8 === o ()
                                  &&& (y2 === o ())
                                  &&& (Obj.magic q15 === !!true)
                                  ||| (Obj.magic q8 === o () &&& (y2 === s (Obj.magic q19)) &&& (Obj.magic q15 === !!false))
                                  ||| (Obj.magic q8 === s (Obj.magic q17) &&& (y2 === o ()) &&& (Obj.magic q15 === !!false))
                                  ||| ( Obj.magic q8
                                      === s (Obj.magic q17)
                                      &&& (y2 === s (Obj.magic q16))
                                      &&& __fancyEq (Obj.magic q16) (Obj.magic q17) (Obj.magic q15) ) ) )
                          ||| ( Obj.magic q14
                              === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                              &&& (Obj.magic q11 === !!false)
                              &&& (Obj.magic q15 === !!true)
                              &&& ( Obj.magic q7 === o ()
                                  &&& (y2 === s (Obj.magic q19))
                                  &&& (Obj.magic q8 === s (Obj.magic q20) &&& _fancyEq (Obj.magic q20) (Obj.magic q19))
                                  ||| (Obj.magic q7 === s (Obj.magic q17) &&& (y2 === o ()) &&& (Obj.magic q8 === o ()))
                                  ||| ( Obj.magic q7
                                      === s (Obj.magic q17)
                                      &&& (y2 === s (Obj.magic q16))
                                      &&& (Obj.magic q17 === o ())
                                      &&& (Obj.magic q16 === s (Obj.magic q22))
                                      &&& (Obj.magic q8 === s (Obj.magic q10) &&& ___fancyEq (Obj.magic q10) (Obj.magic q22)) )
                                  ||| ( Obj.magic q7
                                      === s (Obj.magic q17)
                                      &&& (y2 === s (Obj.magic q16))
                                      &&& (Obj.magic q17 === s (Obj.magic q20))
                                      &&& (Obj.magic q16 === o ())
                                      &&& (Obj.magic q8 === s (Obj.magic q10) &&& ____fancyEq (Obj.magic q10)) )
                                  ||| ( Obj.magic q7
                                      === s (Obj.magic q17)
                                      &&& (y2 === s (Obj.magic q16))
                                      &&& (Obj.magic q17 === s (Obj.magic q20))
                                      &&& (Obj.magic q16 === s (Obj.magic q21))
                                      &&& ( Obj.magic q20 === o ()
                                          &&& (Obj.magic q21 === s (Obj.magic q9))
                                          ||| (Obj.magic q20 === s (Obj.magic q10) &&& (Obj.magic q21 === o ()))
                                          ||| ( Obj.magic q20
                                              === s (Obj.magic q10)
                                              &&& (Obj.magic q21 === s (Obj.magic q23))
                                              &&& _____fancyEq (Obj.magic q10) (Obj.magic q23) )
                                          &&& ______fancyEq (Obj.magic q8) (Obj.magic q21) ) ) ) ) )
                      ||| ( Obj.magic q2
                          === Std.( % ) (Obj.magic q3) (Obj.magic q4)
                          &&& (Obj.magic q5 === !!true)
                          &&& ( Obj.magic q14
                              === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                              &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                              &&& (Obj.magic q11 === fill ())
                              &&& (Obj.magic q15 === fst_ ())
                              &&& (Obj.magic q7 === Obj.magic q19)
                              &&& ____fancyEq (Obj.magic q19)
                              ||| ( Obj.magic q14
                                  === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                  &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                  &&& (Obj.magic q11 === fill ())
                                  &&& (Obj.magic q15 === snd_ ())
                                  &&& (Obj.magic q8 === Obj.magic q19)
                                  &&& ____fancyEq (Obj.magic q19) )
                              ||| ( Obj.magic q14
                                  === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                  &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                  &&& (Obj.magic q11 === empty ())
                                  &&& (Obj.magic q15 === fst_ ())
                                  &&& (Obj.magic q7 === Obj.magic q17)
                                  &&& ( y1
                                      === Std.Pair.pair (Obj.magic q12) (Obj.magic q13)
                                      &&& (Obj.magic q16 === Obj.magic q12)
                                      &&& _fancyEq (Obj.magic q17) (Obj.magic q12) ) )
                              ||| ( Obj.magic q14
                                  === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                  &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                  &&& (Obj.magic q11 === empty ())
                                  &&& (Obj.magic q15 === snd_ ())
                                  &&& (Obj.magic q8 === Obj.magic q17)
                                  &&& ( y1
                                      === Std.Pair.pair (Obj.magic q12) (Obj.magic q13)
                                      &&& (Obj.magic q16 === Obj.magic q13)
                                      &&& _fancyEq (Obj.magic q17) (Obj.magic q13) ) )
                              ||| ( Obj.magic q14
                                  === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                  &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                  &&& (Obj.magic q11 === pour ())
                                  &&& (Obj.magic q15 === fst_ ())
                                  &&& (Obj.magic q7 === Obj.magic q9)
                                  &&& (Obj.magic q8 === Obj.magic q10)
                                  &&& (Obj.magic q20 === !!false)
                                  &&& (Obj.magic q22 === Obj.magic q21)
                                  &&& (Obj.magic q21 === !!false)
                                  &&& ( Obj.magic q18 === snd_ ()
                                      &&& ( y1
                                          === Std.Pair.pair (Obj.magic q12) (Obj.magic q13)
                                          &&& (Obj.magic q23 === Obj.magic q13)
                                          &&& (Obj.magic q9 === s (Obj.magic q24) &&& _____fancyEq (Obj.magic q10) (Obj.magic q13)) ) ) )
                              ||| ( Obj.magic q14
                                  === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                  &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                  &&& (Obj.magic q11 === pour ())
                                  &&& (Obj.magic q15 === snd_ ())
                                  &&& (Obj.magic q8 === Obj.magic q9)
                                  &&& (Obj.magic q7 === Obj.magic q10)
                                  &&& (Obj.magic q20 === !!false)
                                  &&& (Obj.magic q22 === Obj.magic q21)
                                  &&& (Obj.magic q21 === !!false)
                                  &&& ( Obj.magic q18 === fst_ ()
                                      &&& ( y1
                                          === Std.Pair.pair (Obj.magic q12) (Obj.magic q13)
                                          &&& (Obj.magic q23 === Obj.magic q12)
                                          &&& fancyEqFancyEq (Obj.magic q9) (Obj.magic q10) (Obj.magic q12) ) ) )
                              &&& ( Obj.magic q14
                                  === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                  &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                  &&& (Obj.magic q11 === fill ())
                                  &&& (Obj.magic q15 === fst_ ())
                                  &&& (Obj.magic q8 === Obj.magic q17)
                                  &&& ( y1
                                      === Std.Pair.pair (Obj.magic q19) (Obj.magic q25)
                                      &&& (Obj.magic q6 === Std.Pair.pair (Obj.magic q19) (Obj.magic q17)) )
                                  ||| ( Obj.magic q14
                                      === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                      &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                      &&& (Obj.magic q11 === fill ())
                                      &&& (Obj.magic q15 === snd_ ())
                                      &&& (Obj.magic q7 === Obj.magic q17)
                                      &&& ( y1
                                          === Std.Pair.pair (Obj.magic q26) (Obj.magic q19)
                                          &&& (Obj.magic q6 === Std.Pair.pair (Obj.magic q17) (Obj.magic q19)) ) )
                                  ||| ( Obj.magic q14
                                      === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                      &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                      &&& (Obj.magic q11 === empty ())
                                      &&& (Obj.magic q15 === fst_ ())
                                      &&& (Obj.magic q8 === Obj.magic q16)
                                      &&& (Obj.magic q6 === Std.Pair.pair (o ()) (Obj.magic q16)) )
                                  ||| ( Obj.magic q14
                                      === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                      &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                      &&& (Obj.magic q11 === empty ())
                                      &&& (Obj.magic q15 === snd_ ())
                                      &&& (Obj.magic q7 === Obj.magic q16)
                                      &&& (Obj.magic q6 === Std.Pair.pair (Obj.magic q16) (o ())) )
                                  ||| ( Obj.magic q14
                                      === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                      &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                      &&& (Obj.magic q11 === pour ())
                                      &&& (Obj.magic q22 === !!true)
                                      &&& ( Obj.magic q15 === fst_ ()
                                          &&& (Obj.magic q9 === snd_ ())
                                          &&& ( y1
                                              === Std.Pair.pair (Obj.magic q26) (Obj.magic q21)
                                              &&& ( Obj.magic q13 === snd_ ()
                                                  &&& ( Obj.magic q12 === Obj.magic q21
                                                      &&& ( Obj.magic q23 === Obj.magic q21
                                                          &&& ( add (Obj.magic q7) (Obj.magic q8) (Obj.magic q20)
                                                              &&& ( Obj.magic q20
                                                                  === s (Obj.magic q27)
                                                                  &&& (Obj.magic q21 === o ())
                                                                  ||| ( Obj.magic q20
                                                                      === s (Obj.magic q27)
                                                                      &&& (Obj.magic q21 === s (Obj.magic q28))
                                                                      &&& greater (Obj.magic q27) (Obj.magic q28) ) )
                                                              &&& add (Obj.magic q7) (Obj.magic q8) (Obj.magic q18)
                                                              &&& ( Obj.magic q21 === o ()
                                                                  &&& (Obj.magic q10 === Obj.magic q18)
                                                                  ||| ( Obj.magic q21
                                                                      === s (Obj.magic q27)
                                                                      &&& (Obj.magic q18 === o ())
                                                                      &&& (Obj.magic q10 === o ()) )
                                                                  ||| ( Obj.magic q21
                                                                      === s (Obj.magic q27)
                                                                      &&& (Obj.magic q18 === s (Obj.magic q28))
                                                                      &&& sub (Obj.magic q10) (Obj.magic q28) (Obj.magic q27) ) )
                                                              &&& createState (Obj.magic q6) (Obj.magic q21) (Obj.magic q10) ) ) ) ) )
                                          ||| ( Obj.magic q15 === snd_ ()
                                              &&& (Obj.magic q9 === fst_ ())
                                              &&& ( y1
                                                  === Std.Pair.pair (Obj.magic q21) (Obj.magic q25)
                                                  &&& ( Obj.magic q13 === fst_ ()
                                                      &&& ( Obj.magic q12 === Obj.magic q21
                                                          &&& ( Obj.magic q23 === Obj.magic q21
                                                              &&& ( add (Obj.magic q7) (Obj.magic q8) (Obj.magic q20)
                                                                  &&& greater (Obj.magic q20) (Obj.magic q21)
                                                                  &&& add (Obj.magic q7) (Obj.magic q8) (Obj.magic q18)
                                                                  &&& sub (Obj.magic q10) (Obj.magic q18) (Obj.magic q21)
                                                                  &&& _createState (Obj.magic q6) (Obj.magic q21) (Obj.magic q10) ) ) ) ) ) ) ) )
                                  ||| ( Obj.magic q14
                                      === Std.Pair.pair (Obj.magic q7) (Obj.magic q8)
                                      &&& (Obj.magic q3 === Std.Pair.pair (Obj.magic q11) (Obj.magic q15))
                                      &&& (Obj.magic q11 === pour ())
                                      &&& (Obj.magic q22 === !!false)
                                      &&& ( Obj.magic q15 === fst_ ()
                                          &&& (Obj.magic q9 === snd_ ())
                                          &&& ( y1
                                              === Std.Pair.pair (Obj.magic q26) (Obj.magic q21)
                                              &&& ( add (Obj.magic q7) (Obj.magic q8) (Obj.magic q20)
                                                  &&& ( Obj.magic q20 === o ()
                                                      ||| ( Obj.magic q20
                                                          === s (Obj.magic q29)
                                                          &&& (Obj.magic q21 === s (Obj.magic q30))
                                                          &&& _greater (Obj.magic q29) (Obj.magic q30) ) )
                                                  &&& add (Obj.magic q7) (Obj.magic q8) (Obj.magic q24)
                                                  &&& __createState (Obj.magic q6) (Obj.magic q24) ) )
                                          ||| ( Obj.magic q15 === snd_ ()
                                              &&& (Obj.magic q9 === fst_ ())
                                              &&& ( y1
                                                  === Std.Pair.pair (Obj.magic q21) (Obj.magic q25)
                                                  &&& ( add (Obj.magic q7) (Obj.magic q8) (Obj.magic q20)
                                                      &&& _greater (Obj.magic q20) (Obj.magic q21)
                                                      &&& add (Obj.magic q7) (Obj.magic q8) (Obj.magic q24)
                                                      &&& ___createState (Obj.magic q6) (Obj.magic q24) ) ) ) ) ) )
                              &&& checkAnswer_ y1 y2 (Obj.magic q4) (Obj.magic q6) ) ) ) ) ) ) )
  and fancyEq y3 = y3 === o ()
  and addGreaterAdd y4 y5 y6 = y4 === o () &&& (y5 === o ())
  and _fancyEq y7 y8 =
    fresh (q1 q2) (y7 === o () &&& (y8 === o ()) ||| (y7 === s (Obj.magic q1) &&& (y8 === s (Obj.magic q2)) &&& _fancyEq (Obj.magic q1) (Obj.magic q2)))
  and __fancyEq y9 y10 y11 =
    fresh (q1 q2 q3)
      ( y10 === o ()
      &&& (y9 === o ())
      &&& (y11 === !!true)
      ||| (y10 === o () &&& (y9 === s (Obj.magic q1)) &&& (y11 === !!false))
      ||| (y10 === s (Obj.magic q2) &&& (y9 === o ()) &&& (y11 === !!false))
      ||| (y10 === s (Obj.magic q2) &&& (y9 === s (Obj.magic q3)) &&& __fancyEq (Obj.magic q3) (Obj.magic q2) y11) )
  and ___fancyEq y12 y13 = fresh (q1) (y12 === s (Obj.magic q1) &&& _fancyEq (Obj.magic q1) y13)
  and ____fancyEq y14 = y14 === o ()
  and _____fancyEq y15 y16 =
    fresh (q1 q2 q3)
      ( y15 === o ()
      &&& (y16 === s (Obj.magic q1))
      ||| (y15 === s (Obj.magic q2) &&& (y16 === o ()))
      ||| (y15 === s (Obj.magic q2) &&& (y16 === s (Obj.magic q3)) &&& _____fancyEq (Obj.magic q2) (Obj.magic q3)) )
  and ______fancyEq y17 y18 = fresh (q1) (y17 === s (Obj.magic q1) &&& ___fancyEq (Obj.magic q1) y18)
  and fancyEqFancyEq y19 y20 y21 = fresh (q1) (y19 === s (Obj.magic q1) &&& _____fancyEq y20 y21)
  and add y22 y23 y24 =
    fresh (q1 q2) (y22 === o () &&& (y23 === y24) ||| (y22 === s (Obj.magic q1) &&& (y24 === s (Obj.magic q2)) &&& add (Obj.magic q1) y23 (Obj.magic q2)))
  and greater y25 y26 =
    fresh (q1 q2)
      (y25 === s (Obj.magic q1) &&& (y26 === o ()) ||| (y25 === s (Obj.magic q1) &&& (y26 === s (Obj.magic q2)) &&& greater (Obj.magic q1) (Obj.magic q2)))
  and sub y27 y28 y29 =
    fresh (q1 q2)
      ( y29 === o () &&& (y27 === y28)
      ||| (y29 === s (Obj.magic q1) &&& (y28 === o ()) &&& (y27 === o ()))
      ||| (y29 === s (Obj.magic q1) &&& (y28 === s (Obj.magic q2)) &&& sub y27 (Obj.magic q2) (Obj.magic q1)) )
  and createState y30 y31 y32 = y30 === Std.Pair.pair y32 y31
  and _createState y33 y34 y35 = y33 === Std.Pair.pair y34 y35
  and _greater y36 y37 = fresh (q1 q2) (y36 === o () ||| (y36 === s (Obj.magic q1) &&& (y37 === s (Obj.magic q2)) &&& _greater (Obj.magic q1) (Obj.magic q2)))
  and __createState y38 y39 = y38 === Std.Pair.pair (o ()) y39
  and ___createState y40 y41 = y40 === Std.Pair.pair y41 (o ())
  and checkAnswer_ y42 y43 y44 y45 =
    fresh
      (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26)
      ( y44 === Std.List.nil ()
      &&& ( y45
          === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
          &&& ( Obj.magic q1 === o ()
              &&& (y43 === o ())
              ||| ( Obj.magic q1
                  === s (Obj.magic q3)
                  &&& (y43 === s (Obj.magic q4))
                  &&& ( Obj.magic q3 === o ()
                      &&& (Obj.magic q4 === o ())
                      ||| (Obj.magic q3 === s (Obj.magic q5) &&& (Obj.magic q4 === s (Obj.magic q6)) &&& _fancyEq (Obj.magic q5) (Obj.magic q6)) ) )
              &&& ( Obj.magic q2 === o ()
                  &&& (y43 === o ())
                  ||| (Obj.magic q2 === o () &&& (y43 === s (Obj.magic q7)))
                  ||| (Obj.magic q2 === s (Obj.magic q3) &&& (y43 === o ()))
                  ||| (Obj.magic q2 === s (Obj.magic q3) &&& (y43 === s (Obj.magic q4)) &&& __fancyEq (Obj.magic q4) (Obj.magic q3) (Obj.magic q8)) ) )
          ||| ( y45
              === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
              &&& ( Obj.magic q1 === o ()
                  &&& (y43 === s (Obj.magic q7))
                  &&& (Obj.magic q2 === s (Obj.magic q5) &&& _fancyEq (Obj.magic q5) (Obj.magic q7))
                  ||| (Obj.magic q1 === s (Obj.magic q3) &&& (y43 === o ()) &&& (Obj.magic q2 === o ()))
                  ||| ( Obj.magic q1
                      === s (o ())
                      &&& (y43 === s (s (Obj.magic q9)))
                      &&& (Obj.magic q2 === s (Obj.magic q10) &&& ___fancyEq (Obj.magic q10) (Obj.magic q9)) )
                  ||| (Obj.magic q1 === s (s (Obj.magic q5)) &&& (y43 === s (o ())) &&& (Obj.magic q2 === s (Obj.magic q10) &&& ____fancyEq (Obj.magic q10)))
                  ||| ( Obj.magic q1
                      === s (s (Obj.magic q5))
                      &&& (y43 === s (s (Obj.magic q6)))
                      &&& ( Obj.magic q5 === o ()
                          &&& (Obj.magic q6 === s (Obj.magic q11))
                          ||| (Obj.magic q5 === s (Obj.magic q10) &&& (Obj.magic q6 === o ()))
                          ||| (Obj.magic q5 === s (Obj.magic q10) &&& (Obj.magic q6 === s (Obj.magic q12)) &&& _____fancyEq (Obj.magic q10) (Obj.magic q12))
                          &&& ______fancyEq (Obj.magic q2) (Obj.magic q6) ) ) ) ) )
      ||| ( y44
          === Std.( % ) (Obj.magic q13) (Obj.magic q14)
          &&& ( y45
              === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
              &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
              &&& (Obj.magic q15 === fill ())
              &&& (Obj.magic q8 === fst_ ())
              &&& (Obj.magic q1 === Obj.magic q7)
              &&& ____fancyEq (Obj.magic q7)
              ||| ( y45
                  === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                  &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                  &&& (Obj.magic q15 === fill ())
                  &&& (Obj.magic q8 === snd_ ())
                  &&& (Obj.magic q2 === Obj.magic q7)
                  &&& ____fancyEq (Obj.magic q7) )
              ||| ( y45
                  === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                  &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                  &&& (Obj.magic q15 === empty ())
                  &&& (Obj.magic q8 === fst_ ())
                  &&& (Obj.magic q1 === Obj.magic q3)
                  &&& (y42 === Std.Pair.pair (Obj.magic q16) (Obj.magic q17) &&& (Obj.magic q4 === Obj.magic q16) &&& _fancyEq (Obj.magic q3) (Obj.magic q16))
                  )
              ||| ( y45
                  === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                  &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                  &&& (Obj.magic q15 === empty ())
                  &&& (Obj.magic q8 === snd_ ())
                  &&& (Obj.magic q2 === Obj.magic q3)
                  &&& (y42 === Std.Pair.pair (Obj.magic q16) (Obj.magic q17) &&& (Obj.magic q4 === Obj.magic q17) &&& _fancyEq (Obj.magic q3) (Obj.magic q17))
                  )
              ||| ( y45
                  === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                  &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                  &&& (Obj.magic q15 === pour ())
                  &&& (Obj.magic q8 === fst_ ())
                  &&& (Obj.magic q1 === Obj.magic q11)
                  &&& (Obj.magic q2 === Obj.magic q10)
                  &&& (Obj.magic q5 === !!false)
                  &&& (Obj.magic q9 === Obj.magic q6)
                  &&& (Obj.magic q6 === !!false)
                  &&& ( Obj.magic q18 === snd_ ()
                      &&& ( y42
                          === Std.Pair.pair (Obj.magic q16) (Obj.magic q17)
                          &&& (Obj.magic q12 === Obj.magic q17)
                          &&& (Obj.magic q11 === s (Obj.magic q19) &&& _____fancyEq (Obj.magic q10) (Obj.magic q17)) ) ) )
              ||| ( y45
                  === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                  &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                  &&& (Obj.magic q15 === pour ())
                  &&& (Obj.magic q8 === snd_ ())
                  &&& (Obj.magic q2 === Obj.magic q11)
                  &&& (Obj.magic q1 === Obj.magic q10)
                  &&& (Obj.magic q5 === !!false)
                  &&& (Obj.magic q9 === Obj.magic q6)
                  &&& (Obj.magic q6 === !!false)
                  &&& ( Obj.magic q18 === fst_ ()
                      &&& ( y42
                          === Std.Pair.pair (Obj.magic q16) (Obj.magic q17)
                          &&& (Obj.magic q12 === Obj.magic q16)
                          &&& fancyEqFancyEq (Obj.magic q11) (Obj.magic q10) (Obj.magic q16) ) ) )
              &&& ( y45
                  === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                  &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                  &&& (Obj.magic q15 === fill ())
                  &&& (Obj.magic q8 === fst_ ())
                  &&& (Obj.magic q2 === Obj.magic q3)
                  &&& (y42 === Std.Pair.pair (Obj.magic q7) (Obj.magic q20) &&& (Obj.magic q21 === Std.Pair.pair (Obj.magic q7) (Obj.magic q3)))
                  ||| ( y45
                      === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                      &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                      &&& (Obj.magic q15 === fill ())
                      &&& (Obj.magic q8 === snd_ ())
                      &&& (Obj.magic q1 === Obj.magic q3)
                      &&& (y42 === Std.Pair.pair (Obj.magic q22) (Obj.magic q7) &&& (Obj.magic q21 === Std.Pair.pair (Obj.magic q3) (Obj.magic q7))) )
                  ||| ( y45
                      === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                      &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                      &&& (Obj.magic q15 === empty ())
                      &&& (Obj.magic q8 === fst_ ())
                      &&& (Obj.magic q2 === Obj.magic q4)
                      &&& (Obj.magic q21 === Std.Pair.pair (o ()) (Obj.magic q4)) )
                  ||| ( y45
                      === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                      &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                      &&& (Obj.magic q15 === empty ())
                      &&& (Obj.magic q8 === snd_ ())
                      &&& (Obj.magic q1 === Obj.magic q4)
                      &&& (Obj.magic q21 === Std.Pair.pair (Obj.magic q4) (o ())) )
                  ||| ( y45
                      === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                      &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                      &&& (Obj.magic q15 === pour ())
                      &&& (Obj.magic q9 === !!true)
                      &&& ( Obj.magic q8 === fst_ ()
                          &&& (Obj.magic q11 === snd_ ())
                          &&& ( y42
                              === Std.Pair.pair (Obj.magic q22) (Obj.magic q6)
                              &&& ( Obj.magic q17 === snd_ ()
                                  &&& ( Obj.magic q16 === Obj.magic q6
                                      &&& ( Obj.magic q12 === Obj.magic q6
                                          &&& ( add (Obj.magic q1) (Obj.magic q2) (Obj.magic q5)
                                              &&& ( Obj.magic q5
                                                  === s (Obj.magic q23)
                                                  &&& (Obj.magic q6 === o ())
                                                  ||| ( Obj.magic q5
                                                      === s (Obj.magic q23)
                                                      &&& (Obj.magic q6 === s (Obj.magic q24))
                                                      &&& greater (Obj.magic q23) (Obj.magic q24) ) )
                                              &&& add (Obj.magic q1) (Obj.magic q2) (Obj.magic q18)
                                              &&& ( Obj.magic q6 === o ()
                                                  &&& (Obj.magic q10 === Obj.magic q18)
                                                  ||| (Obj.magic q6 === s (Obj.magic q23) &&& (Obj.magic q18 === o ()) &&& (Obj.magic q10 === o ()))
                                                  ||| ( Obj.magic q6
                                                      === s (Obj.magic q23)
                                                      &&& (Obj.magic q18 === s (Obj.magic q24))
                                                      &&& sub (Obj.magic q10) (Obj.magic q24) (Obj.magic q23) ) )
                                              &&& createState (Obj.magic q21) (Obj.magic q6) (Obj.magic q10) ) ) ) ) )
                          ||| ( Obj.magic q8 === snd_ ()
                              &&& (Obj.magic q11 === fst_ ())
                              &&& ( y42
                                  === Std.Pair.pair (Obj.magic q6) (Obj.magic q20)
                                  &&& ( Obj.magic q17 === fst_ ()
                                      &&& ( Obj.magic q16 === Obj.magic q6
                                          &&& ( Obj.magic q12 === Obj.magic q6
                                              &&& ( add (Obj.magic q1) (Obj.magic q2) (Obj.magic q5)
                                                  &&& greater (Obj.magic q5) (Obj.magic q6)
                                                  &&& add (Obj.magic q1) (Obj.magic q2) (Obj.magic q18)
                                                  &&& sub (Obj.magic q10) (Obj.magic q18) (Obj.magic q6)
                                                  &&& _createState (Obj.magic q21) (Obj.magic q6) (Obj.magic q10) ) ) ) ) ) ) ) )
                  ||| ( y45
                      === Std.Pair.pair (Obj.magic q1) (Obj.magic q2)
                      &&& (Obj.magic q13 === Std.Pair.pair (Obj.magic q15) (Obj.magic q8))
                      &&& (Obj.magic q15 === pour ())
                      &&& (Obj.magic q9 === !!false)
                      &&& ( Obj.magic q8 === fst_ ()
                          &&& (Obj.magic q11 === snd_ ())
                          &&& ( y42
                              === Std.Pair.pair (Obj.magic q22) (Obj.magic q6)
                              &&& ( add (Obj.magic q1) (Obj.magic q2) (Obj.magic q5)
                                  &&& ( Obj.magic q5 === o ()
                                      ||| ( Obj.magic q5
                                          === s (Obj.magic q25)
                                          &&& (Obj.magic q6 === s (Obj.magic q26))
                                          &&& _greater (Obj.magic q25) (Obj.magic q26) ) )
                                  &&& add (Obj.magic q1) (Obj.magic q2) (Obj.magic q19)
                                  &&& __createState (Obj.magic q21) (Obj.magic q19) ) )
                          ||| ( Obj.magic q8 === snd_ ()
                              &&& (Obj.magic q11 === fst_ ())
                              &&& ( y42
                                  === Std.Pair.pair (Obj.magic q6) (Obj.magic q20)
                                  &&& ( add (Obj.magic q1) (Obj.magic q2) (Obj.magic q5)
                                      &&& _greater (Obj.magic q5) (Obj.magic q6)
                                      &&& add (Obj.magic q1) (Obj.magic q2) (Obj.magic q19)
                                      &&& ___createState (Obj.magic q21) (Obj.magic q19) ) ) ) ) ) )
              &&& checkAnswer_ y42 y43 (Obj.magic q14) (Obj.magic q21) ) ) )
  in
  checkAnswer x0 x1 x2

























































(*


let topLevel x0 x1 x2 =
  let rec checkAnswer y0 y1 y2 =
    y0 === Std.List.nil ()
    &&& (y2 === o ())
    ||| fresh (q2 q1)
          ( y0 === Std.( % ) q1 q2
          &&& ( fresh (q7 q6 q5 q4 q3)
                  (q3 === o () &&& (q4 === o ()) &&& (q1 === Std.Pair.pair q5 q6) &&& (q5 === fill ()) &&& (q6 === fst_ ()) &&& (q7 === q3))
              ||| fresh (q7 q6 q5 q4 q3)
                    (q3 === o () &&& (q4 === o ()) &&& (q1 === Std.Pair.pair q5 q6) &&& (q5 === fill ()) &&& (q6 === snd_ ()) &&& (q7 === q4))
              ||| fresh (q11 q10 q9 q8 q6 q5 q4 q3)
                    ( q3 === o ()
                    &&& (q4 === o ())
                    &&& (q1 === Std.Pair.pair q5 q6)
                    &&& (q5 === empty ())
                    &&& (q6 === fst_ ())
                    &&& (q8 === q3)
                    &&& (y1 === Std.Pair.pair q9 q10 &&& (q11 === q9) &&& (q9 === o ())) )
              ||| fresh (q11 q10 q9 q8 q6 q5 q4 q3)
                    ( q3 === o ()
                    &&& (q4 === o ())
                    &&& (q1 === Std.Pair.pair q5 q6)
                    &&& (q5 === empty ())
                    &&& (q6 === snd_ ())
                    &&& (q8 === q4)
                    &&& (y1 === Std.Pair.pair q9 q10 &&& (q11 === q10) &&& fancyEq q10) )
              &&& ( fresh (q14 q7 q13 q12 q8 q6 q5 q4 q3)
                      ( q3 === o ()
                      &&& (q4 === o ())
                      &&& (q1 === Std.Pair.pair q5 q6)
                      &&& (q5 === fill ())
                      &&& (q6 === fst_ ())
                      &&& (q8 === q4)
                      &&& (y1 === Std.Pair.pair q12 q13 &&& (q7 === q12) &&& (q14 === Std.Pair.pair q12 (o ()))) )
                  ||| fresh (q14 q7 q13 q12 q8 q6 q5 q4 q3)
                        ( q3 === o ()
                        &&& (q4 === o ())
                        &&& (q1 === Std.Pair.pair q5 q6)
                        &&& (q5 === fill ())
                        &&& (q6 === snd_ ())
                        &&& (q8 === q3)
                        &&& (y1 === Std.Pair.pair q12 q13 &&& (q7 === q13) &&& (q14 === Std.Pair.pair (o ()) q13)) )
                  ||| fresh (q14 q11 q6 q5 q4 q3)
                        ( q3 === o ()
                        &&& (q4 === o ())
                        &&& (q1 === Std.Pair.pair q5 q6)
                        &&& (q5 === empty ())
                        &&& (q6 === fst_ ())
                        &&& (q11 === q4)
                        &&& (q14 === Std.Pair.pair (o ()) (o ())) )
                  ||| fresh (q14 q11 q6 q5 q4 q3)
                        ( q3 === o ()
                        &&& (q4 === o ())
                        &&& (q1 === Std.Pair.pair q5 q6)
                        &&& (q5 === empty ())
                        &&& (q6 === snd_ ())
                        &&& (q11 === q3)
                        &&& (q14 === Std.Pair.pair (o ()) (o ())) )
                  ||| fresh (q15 q6 q5 q4 q3)
                        ( q3 === o ()
                        &&& (q4 === o ())
                        &&& (q1 === Std.Pair.pair q5 q6)
                        &&& (q5 === pour ())
                        &&& (q15 === !!false)
                        &&& ( fresh (q19 q18 q14 q17 q13 q12 q16)
                                ( q6 === fst_ ()
                                &&& (q16 === snd_ ())
                                &&& ( y1 === Std.Pair.pair q12 q13 &&& (q17 === q13)
                                    &&& (q14 === Std.Pair.pair (o ()) q18 &&& (q19 === o () &&& (q18 === o ()))) ) )
                            ||| fresh (q19 q18 q14 q17 q13 q12 q16)
                                  ( q6 === snd_ ()
                                  &&& (q16 === fst_ ())
                                  &&& (y1 === Std.Pair.pair q12 q13 &&& (q17 === q12) &&& (q14 === Std.Pair.pair q18 (o ()) &&& addGreaterAdd q19 q18 q12)) )
                            ) )
                  &&& ( q2 === Std.List.nil ()
                      &&& ( fresh (q11 q8 q7 q14)
                              ( q14 === Std.Pair.pair q7 q8 &&& (q11 === !!true)
                              &&& ( q7 === o ()
                                  &&& (y2 === o ())
                                  ||| fresh (q16 q17)
                                        ( q7 === s q17
                                        &&& (y2 === s q16)
                                        &&& (q17 === o () &&& (q16 === o ()) ||| fresh (q21 q20) (q17 === s q20 &&& (q16 === s q21) &&& _fancyEq q20 q21)) )
                                  &&& ( fresh (q15) (q8 === o () &&& (y2 === o ()) &&& (q15 === !!true))
                                      ||| fresh (q15 q19) (q8 === o () &&& (y2 === s q19) &&& (q15 === !!false))
                                      ||| fresh (q15 q17) (q8 === s q17 &&& (y2 === o ()) &&& (q15 === !!false))
                                      ||| fresh (q15 q16 q17) (q8 === s q17 &&& (y2 === s q16) &&& __fancyEq q16 q17 q15) ) ) )
                          ||| fresh (q15 q11 q8 q7 q14)
                                ( q14 === Std.Pair.pair q7 q8 &&& (q11 === !!false) &&& (q15 === !!true)
                                &&& ( fresh (q20 q19) (q7 === o () &&& (y2 === s q19) &&& (q8 === s q20 &&& _fancyEq q20 q19))
                                    ||| fresh (q17) (q7 === s q17 &&& (y2 === o ()) &&& (q8 === o ()))
                                    ||| fresh (q10 q22 q16 q17)
                                          (q7 === s q17 &&& (y2 === s q16) &&& (q17 === o ()) &&& (q16 === s q22) &&& (q8 === s q10 &&& ___fancyEq q10 q22))
                                    ||| fresh (q10 q20 q16 q17)
                                          (q7 === s q17 &&& (y2 === s q16) &&& (q17 === s q20) &&& (q16 === o ()) &&& (q8 === s q10 &&& ____fancyEq q10))
                                    ||| fresh (q21 q20 q16 q17)
                                          ( q7 === s q17
                                          &&& (y2 === s q16)
                                          &&& (q17 === s q20)
                                          &&& (q16 === s q21)
                                          &&& ( fresh (q9) (q20 === o () &&& (q21 === s q9))
                                              ||| fresh (q10) (q20 === s q10 &&& (q21 === o ()))
                                              ||| fresh (q23 q10) (q20 === s q10 &&& (q21 === s q23) &&& _____fancyEq q10 q23)
                                              &&& ______fancyEq q8 q21 ) ) ) ) )
                      ||| fresh (q6 q5 q4 q3)
                            ( q2 === Std.( % ) q3 q4 &&& (q5 === !!true)
                            &&& ( fresh (q19 q15 q11 q8 q7 q14)
                                    ( q14 === Std.Pair.pair q7 q8
                                    &&& (q3 === Std.Pair.pair q11 q15)
                                    &&& (q11 === fill ())
                                    &&& (q15 === fst_ ())
                                    &&& (q7 === q19) &&& ____fancyEq q19 )
                                ||| fresh (q19 q15 q11 q8 q7 q14)
                                      ( q14 === Std.Pair.pair q7 q8
                                      &&& (q3 === Std.Pair.pair q11 q15)
                                      &&& (q11 === fill ())
                                      &&& (q15 === snd_ ())
                                      &&& (q8 === q19) &&& ____fancyEq q19 )
                                ||| fresh (q16 q13 q12 q17 q15 q11 q8 q7 q14)
                                      ( q14 === Std.Pair.pair q7 q8
                                      &&& (q3 === Std.Pair.pair q11 q15)
                                      &&& (q11 === empty ())
                                      &&& (q15 === fst_ ())
                                      &&& (q7 === q17)
                                      &&& (y1 === Std.Pair.pair q12 q13 &&& (q16 === q12) &&& _fancyEq q17 q12) )
                                ||| fresh (q16 q13 q12 q17 q15 q11 q8 q7 q14)
                                      ( q14 === Std.Pair.pair q7 q8
                                      &&& (q3 === Std.Pair.pair q11 q15)
                                      &&& (q11 === empty ())
                                      &&& (q15 === snd_ ())
                                      &&& (q8 === q17)
                                      &&& (y1 === Std.Pair.pair q12 q13 &&& (q16 === q13) &&& _fancyEq q17 q13) )
                                ||| fresh
                                      (q24 q23 q13 q12 q18 q21 q22 q20 q10 q9 q15 q11 q8 q7 q14)
                                      ( q14 === Std.Pair.pair q7 q8
                                      &&& (q3 === Std.Pair.pair q11 q15)
                                      &&& (q11 === pour ())
                                      &&& (q15 === fst_ ())
                                      &&& (q7 === q9) &&& (q8 === q10) &&& (q20 === !!false) &&& (q22 === q21) &&& (q21 === !!false)
                                      &&& (q18 === snd_ () &&& (y1 === Std.Pair.pair q12 q13 &&& (q23 === q13) &&& (q9 === s q24 &&& _____fancyEq q10 q13))) )
                                ||| fresh
                                      (q23 q13 q12 q18 q21 q22 q20 q10 q9 q15 q11 q8 q7 q14)
                                      ( q14 === Std.Pair.pair q7 q8
                                      &&& (q3 === Std.Pair.pair q11 q15)
                                      &&& (q11 === pour ())
                                      &&& (q15 === snd_ ())
                                      &&& (q8 === q9) &&& (q7 === q10) &&& (q20 === !!false) &&& (q22 === q21) &&& (q21 === !!false)
                                      &&& (q18 === fst_ () &&& (y1 === Std.Pair.pair q12 q13 &&& (q23 === q12) &&& fancyEqFancyEq q9 q10 q12)) )
                                &&& ( fresh (q25 q19 q17 q15 q11 q8 q7 q14)
                                        ( q14 === Std.Pair.pair q7 q8
                                        &&& (q3 === Std.Pair.pair q11 q15)
                                        &&& (q11 === fill ())
                                        &&& (q15 === fst_ ())
                                        &&& (q8 === q17)
                                        &&& (y1 === Std.Pair.pair q19 q25 &&& (q6 === Std.Pair.pair q19 q17)) )
                                    ||| fresh (q19 q26 q17 q15 q11 q8 q7 q14)
                                          ( q14 === Std.Pair.pair q7 q8
                                          &&& (q3 === Std.Pair.pair q11 q15)
                                          &&& (q11 === fill ())
                                          &&& (q15 === snd_ ())
                                          &&& (q7 === q17)
                                          &&& (y1 === Std.Pair.pair q26 q19 &&& (q6 === Std.Pair.pair q17 q19)) )
                                    ||| fresh (q16 q15 q11 q8 q7 q14)
                                          ( q14 === Std.Pair.pair q7 q8
                                          &&& (q3 === Std.Pair.pair q11 q15)
                                          &&& (q11 === empty ())
                                          &&& (q15 === fst_ ())
                                          &&& (q8 === q16)
                                          &&& (q6 === Std.Pair.pair (o ()) q16) )
                                    ||| fresh (q16 q15 q11 q8 q7 q14)
                                          ( q14 === Std.Pair.pair q7 q8
                                          &&& (q3 === Std.Pair.pair q11 q15)
                                          &&& (q11 === empty ())
                                          &&& (q15 === snd_ ())
                                          &&& (q7 === q16)
                                          &&& (q6 === Std.Pair.pair q16 (o ())) )
                                    ||| fresh (q22 q15 q11 q8 q7 q14)
                                          ( q14 === Std.Pair.pair q7 q8
                                          &&& (q3 === Std.Pair.pair q11 q15)
                                          &&& (q11 === pour ())
                                          &&& (q22 === !!true)
                                          &&& ( fresh (q10 q18 q20 q23 q12 q13 q21 q26 q9)
                                                  ( q15 === fst_ ()
                                                  &&& (q9 === snd_ ())
                                                  &&& ( y1 === Std.Pair.pair q26 q21
                                                      &&& ( q13 === snd_ ()
                                                          &&& ( q12 === q21
                                                              &&& ( q23 === q21
                                                                  &&& ( add q7 q8 q20
                                                                      &&& ( fresh (q27) (q20 === s q27 &&& (q21 === o ()))
                                                                          ||| fresh (q28 q27) (q20 === s q27 &&& (q21 === s q28) &&& greater q27 q28) )
                                                                      &&& add q7 q8 q18
                                                                      &&& ( q21 === o () &&& (q10 === q18)
                                                                          ||| fresh (q27) (q21 === s q27 &&& (q18 === o ()) &&& (q10 === o ()))
                                                                          ||| fresh (q28 q27) (q21 === s q27 &&& (q18 === s q28) &&& sub q10 q28 q27) )
                                                                      &&& createState q6 q21 q10 ) ) ) ) ) )
                                              ||| fresh (q10 q18 q20 q23 q12 q13 q25 q21 q9)
                                                    ( q15 === snd_ ()
                                                    &&& (q9 === fst_ ())
                                                    &&& ( y1 === Std.Pair.pair q21 q25
                                                        &&& ( q13 === fst_ ()
                                                            &&& ( q12 === q21
                                                                &&& ( q23 === q21
                                                                    &&& ( add q7 q8 q20 &&& greater q20 q21 &&& add q7 q8 q18 &&& sub q10 q18 q21
                                                                        &&& _createState q6 q21 q10 ) ) ) ) ) ) ) )
                                    ||| fresh (q22 q15 q11 q8 q7 q14)
                                          ( q14 === Std.Pair.pair q7 q8
                                          &&& (q3 === Std.Pair.pair q11 q15)
                                          &&& (q11 === pour ())
                                          &&& (q22 === !!false)
                                          &&& ( fresh (q24 q20 q21 q26 q9)
                                                  ( q15 === fst_ ()
                                                  &&& (q9 === snd_ ())
                                                  &&& ( y1 === Std.Pair.pair q26 q21
                                                      &&& ( add q7 q8 q20
                                                          &&& (q20 === o () ||| fresh (q30 q29) (q20 === s q29 &&& (q21 === s q30) &&& _greater q29 q30))
                                                          &&& add q7 q8 q24 &&& __createState q6 q24 ) ) )
                                              ||| fresh (q24 q20 q25 q21 q9)
                                                    ( q15 === snd_ ()
                                                    &&& (q9 === fst_ ())
                                                    &&& ( y1 === Std.Pair.pair q21 q25
                                                        &&& (add q7 q8 q20 &&& _greater q20 q21 &&& add q7 q8 q24 &&& ___createState q6 q24) ) ) ) ) )
                                &&& checkAnswer_ y1 y2 q4 q6 ) ) ) ) ) )
  and fancyEq y3 = y3 === o ()
  and addGreaterAdd y4 y5 y6 = y4 === o () &&& (y5 === o ())
  and _fancyEq y7 y8 = y7 === o () &&& (y8 === o ()) ||| fresh (q2 q1) (y7 === s q1 &&& (y8 === s q2) &&& _fancyEq q1 q2)
  and __fancyEq y9 y10 y11 =
    y10 === o ()
    &&& (y9 === o ())
    &&& (y11 === !!true)
    ||| fresh (q1) (y10 === o () &&& (y9 === s q1) &&& (y11 === !!false))
    ||| fresh (q2) (y10 === s q2 &&& (y9 === o ()) &&& (y11 === !!false))
    ||| fresh (q3 q2) (y10 === s q2 &&& (y9 === s q3) &&& __fancyEq q3 q2 y11)
  and ___fancyEq y12 y13 = fresh (q1) (y12 === s q1 &&& _fancyEq q1 y13)
  and ____fancyEq y14 = y14 === o ()
  and _____fancyEq y15 y16 =
    fresh (q1) (y15 === o () &&& (y16 === s q1))
    ||| fresh (q2) (y15 === s q2 &&& (y16 === o ()))
    ||| fresh (q3 q2) (y15 === s q2 &&& (y16 === s q3) &&& _____fancyEq q2 q3)
  and ______fancyEq y17 y18 = fresh (q1) (y17 === s q1 &&& ___fancyEq q1 y18)
  and fancyEqFancyEq y19 y20 y21 = fresh (q1) (y19 === s q1 &&& _____fancyEq y20 y21)
  and add y22 y23 y24 = y22 === o () &&& (y23 === y24) ||| fresh (q2 q1) (y22 === s q1 &&& (y24 === s q2) &&& add q1 y23 q2)
  and greater y25 y26 = fresh (q1) (y25 === s q1 &&& (y26 === o ())) ||| fresh (q2 q1) (y25 === s q1 &&& (y26 === s q2) &&& greater q1 q2)
  and sub y27 y28 y29 =
    y29 === o () &&& (y27 === y28)
    ||| fresh (q1) (y29 === s q1 &&& (y28 === o ()) &&& (y27 === o ()))
    ||| fresh (q2 q1) (y29 === s q1 &&& (y28 === s q2) &&& sub y27 q2 q1)
  and createState y30 y31 y32 = y30 === Std.Pair.pair y32 y31
  and _createState y33 y34 y35 = y33 === Std.Pair.pair y34 y35
  and _greater y36 y37 = y36 === o () ||| fresh (q2 q1) (y36 === s q1 &&& (y37 === s q2) &&& _greater q1 q2)
  and __createState y38 y39 = y38 === Std.Pair.pair (o ()) y39
  and ___createState y40 y41 = y40 === Std.Pair.pair y41 (o ())
  and checkAnswer_ y42 y43 y44 y45 =
    y44 === Std.List.nil ()
    &&& ( fresh (q2 q1)
            ( y45 === Std.Pair.pair q1 q2
            &&& ( q1 === o ()
                &&& (y43 === o ())
                ||| fresh (q4 q3)
                      (q1 === s q3 &&& (y43 === s q4) &&& (q3 === o () &&& (q4 === o ()) ||| fresh (q6 q5) (q3 === s q5 &&& (q4 === s q6) &&& _fancyEq q5 q6)))
                &&& ( q2 === o ()
                    &&& (y43 === o ())
                    ||| fresh (q7) (q2 === o () &&& (y43 === s q7))
                    ||| fresh (q3) (q2 === s q3 &&& (y43 === o ()))
                    ||| fresh (q8 q4 q3) (q2 === s q3 &&& (y43 === s q4) &&& __fancyEq q4 q3 q8) ) ) )
        ||| fresh (q2 q1)
              ( y45 === Std.Pair.pair q1 q2
              &&& ( fresh (q5 q7) (q1 === o () &&& (y43 === s q7) &&& (q2 === s q5 &&& _fancyEq q5 q7))
                  ||| fresh (q3) (q1 === s q3 &&& (y43 === o ()) &&& (q2 === o ()))
                  ||| fresh (q10 q9) (q1 === s (o ()) &&& (y43 === s (s q9)) &&& (q2 === s q10 &&& ___fancyEq q10 q9))
                  ||| fresh (q10 q5) (q1 === s (s q5) &&& (y43 === s (o ())) &&& (q2 === s q10 &&& ____fancyEq q10))
                  ||| fresh (q6 q5)
                        ( q1
                        === s (s q5)
                        &&& (y43 === s (s q6))
                        &&& ( fresh (q11) (q5 === o () &&& (q6 === s q11))
                            ||| fresh (q10) (q5 === s q10 &&& (q6 === o ()))
                            ||| fresh (q12 q10) (q5 === s q10 &&& (q6 === s q12) &&& _____fancyEq q10 q12)
                            &&& ______fancyEq q2 q6 ) ) ) ) )
    ||| fresh (q21 q14 q13)
          ( y44 === Std.( % ) q13 q14
          &&& ( fresh (q7 q8 q15 q2 q1)
                  (y45 === Std.Pair.pair q1 q2 &&& (q13 === Std.Pair.pair q15 q8) &&& (q15 === fill ()) &&& (q8 === fst_ ()) &&& (q1 === q7) &&& ____fancyEq q7)
              ||| fresh (q7 q8 q15 q2 q1)
                    ( y45 === Std.Pair.pair q1 q2
                    &&& (q13 === Std.Pair.pair q15 q8)
                    &&& (q15 === fill ())
                    &&& (q8 === snd_ ())
                    &&& (q2 === q7) &&& ____fancyEq q7 )
              ||| fresh (q4 q17 q16 q3 q8 q15 q2 q1)
                    ( y45 === Std.Pair.pair q1 q2
                    &&& (q13 === Std.Pair.pair q15 q8)
                    &&& (q15 === empty ())
                    &&& (q8 === fst_ ())
                    &&& (q1 === q3)
                    &&& (y42 === Std.Pair.pair q16 q17 &&& (q4 === q16) &&& _fancyEq q3 q16) )
              ||| fresh (q4 q17 q16 q3 q8 q15 q2 q1)
                    ( y45 === Std.Pair.pair q1 q2
                    &&& (q13 === Std.Pair.pair q15 q8)
                    &&& (q15 === empty ())
                    &&& (q8 === snd_ ())
                    &&& (q2 === q3)
                    &&& (y42 === Std.Pair.pair q16 q17 &&& (q4 === q17) &&& _fancyEq q3 q17) )
              ||| fresh (q19 q12 q17 q16 q18 q6 q9 q5 q10 q11 q8 q15 q2 q1)
                    ( y45 === Std.Pair.pair q1 q2
                    &&& (q13 === Std.Pair.pair q15 q8)
                    &&& (q15 === pour ())
                    &&& (q8 === fst_ ())
                    &&& (q1 === q11) &&& (q2 === q10) &&& (q5 === !!false) &&& (q9 === q6) &&& (q6 === !!false)
                    &&& (q18 === snd_ () &&& (y42 === Std.Pair.pair q16 q17 &&& (q12 === q17) &&& (q11 === s q19 &&& _____fancyEq q10 q17))) )
              ||| fresh (q12 q17 q16 q18 q6 q9 q5 q10 q11 q8 q15 q2 q1)
                    ( y45 === Std.Pair.pair q1 q2
                    &&& (q13 === Std.Pair.pair q15 q8)
                    &&& (q15 === pour ())
                    &&& (q8 === snd_ ())
                    &&& (q2 === q11) &&& (q1 === q10) &&& (q5 === !!false) &&& (q9 === q6) &&& (q6 === !!false)
                    &&& (q18 === fst_ () &&& (y42 === Std.Pair.pair q16 q17 &&& (q12 === q16) &&& fancyEqFancyEq q11 q10 q16)) )
              &&& ( fresh (q20 q7 q3 q8 q15 q2 q1)
                      ( y45 === Std.Pair.pair q1 q2
                      &&& (q13 === Std.Pair.pair q15 q8)
                      &&& (q15 === fill ())
                      &&& (q8 === fst_ ())
                      &&& (q2 === q3)
                      &&& (y42 === Std.Pair.pair q7 q20 &&& (q21 === Std.Pair.pair q7 q3)) )
                  ||| fresh (q7 q22 q3 q8 q15 q2 q1)
                        ( y45 === Std.Pair.pair q1 q2
                        &&& (q13 === Std.Pair.pair q15 q8)
                        &&& (q15 === fill ())
                        &&& (q8 === snd_ ())
                        &&& (q1 === q3)
                        &&& (y42 === Std.Pair.pair q22 q7 &&& (q21 === Std.Pair.pair q3 q7)) )
                  ||| fresh (q4 q8 q15 q2 q1)
                        ( y45 === Std.Pair.pair q1 q2
                        &&& (q13 === Std.Pair.pair q15 q8)
                        &&& (q15 === empty ())
                        &&& (q8 === fst_ ())
                        &&& (q2 === q4)
                        &&& (q21 === Std.Pair.pair (o ()) q4) )
                  ||| fresh (q4 q8 q15 q2 q1)
                        ( y45 === Std.Pair.pair q1 q2
                        &&& (q13 === Std.Pair.pair q15 q8)
                        &&& (q15 === empty ())
                        &&& (q8 === snd_ ())
                        &&& (q1 === q4)
                        &&& (q21 === Std.Pair.pair q4 (o ())) )
                  ||| fresh (q9 q8 q15 q2 q1)
                        ( y45 === Std.Pair.pair q1 q2
                        &&& (q13 === Std.Pair.pair q15 q8)
                        &&& (q15 === pour ())
                        &&& (q9 === !!true)
                        &&& ( fresh (q10 q18 q5 q12 q16 q17 q6 q22 q11)
                                ( q8 === fst_ ()
                                &&& (q11 === snd_ ())
                                &&& ( y42 === Std.Pair.pair q22 q6
                                    &&& ( q17 === snd_ ()
                                        &&& ( q16 === q6
                                            &&& ( q12 === q6
                                                &&& ( add q1 q2 q5
                                                    &&& ( fresh (q23) (q5 === s q23 &&& (q6 === o ()))
                                                        ||| fresh (q24 q23) (q5 === s q23 &&& (q6 === s q24) &&& greater q23 q24) )
                                                    &&& add q1 q2 q18
                                                    &&& ( q6 === o () &&& (q10 === q18)
                                                        ||| fresh (q23) (q6 === s q23 &&& (q18 === o ()) &&& (q10 === o ()))
                                                        ||| fresh (q24 q23) (q6 === s q23 &&& (q18 === s q24) &&& sub q10 q24 q23) )
                                                    &&& createState q21 q6 q10 ) ) ) ) ) )
                            ||| fresh (q10 q18 q5 q12 q16 q17 q20 q6 q11)
                                  ( q8 === snd_ ()
                                  &&& (q11 === fst_ ())
                                  &&& ( y42 === Std.Pair.pair q6 q20
                                      &&& ( q17 === fst_ ()
                                          &&& ( q16 === q6
                                              &&& ( q12 === q6
                                                  &&& (add q1 q2 q5 &&& greater q5 q6 &&& add q1 q2 q18 &&& sub q10 q18 q6 &&& _createState q21 q6 q10) ) ) )
                                      ) ) ) )
                  ||| fresh (q9 q8 q15 q2 q1)
                        ( y45 === Std.Pair.pair q1 q2
                        &&& (q13 === Std.Pair.pair q15 q8)
                        &&& (q15 === pour ())
                        &&& (q9 === !!false)
                        &&& ( fresh (q19 q5 q6 q22 q11)
                                ( q8 === fst_ ()
                                &&& (q11 === snd_ ())
                                &&& ( y42 === Std.Pair.pair q22 q6
                                    &&& ( add q1 q2 q5
                                        &&& (q5 === o () ||| fresh (q26 q25) (q5 === s q25 &&& (q6 === s q26) &&& _greater q25 q26))
                                        &&& add q1 q2 q19 &&& __createState q21 q19 ) ) )
                            ||| fresh (q19 q5 q20 q6 q11)
                                  ( q8 === snd_ ()
                                  &&& (q11 === fst_ ())
                                  &&& (y42 === Std.Pair.pair q6 q20 &&& (add q1 q2 q5 &&& _greater q5 q6 &&& add q1 q2 q19 &&& ___createState q21 q19)) ) ) )
                  )
              &&& checkAnswer_ y42 y43 q14 q21 ) )
  in
  checkAnswer x0 x1 x2

(*
let topLevel x0 x1 x2 =
  let rec checkAnswer y0 y1 y2 =
    fresh (x10 x9 x8 x7) (y0 === Std.List.nil () &&& (x7 === o () &&& (x8 === o ()) &&& (x9 === !!true) &&& (y2 === o () &&& (x10 === !!true))))
    ||| fresh (x5 x4 x3)
          ( y0 === Std.( % ) x3 x4 &&& (x5 === !!true)
          &&& ( success
              &&& ( fresh (x11 x10 x9 x8 x7)
                      (x7 === o () &&& (x8 === o ()) &&& (x3 === Std.Pair.pair x9 x10) &&& (x9 === fill ()) &&& (x10 === fst_ ()) &&& (x11 === x7) &&& success)
                  ||| fresh (x11 x10 x9 x8 x7)
                        ( x7 === o ()
                        &&& (x8 === o ())
                        &&& (x3 === Std.Pair.pair x9 x10)
                        &&& (x9 === fill ())
                        &&& (x10 === snd_ ())
                        &&& (x11 === x8) &&& success )
                  ||| fresh (x13 x22 x21 x12 x10 x9 x8 x7)
                        ( x7 === o ()
                        &&& (x8 === o ())
                        &&& (x3 === Std.Pair.pair x9 x10)
                        &&& (x9 === empty ())
                        &&& (x10 === fst_ ())
                        &&& (x12 === x7)
                        &&& (y1 === Std.Pair.pair x21 x22 &&& (x13 === x21) &&& (x21 === o ())) )
                  ||| fresh (x13 x22 x21 x12 x10 x9 x8 x7)
                        ( x7 === o ()
                        &&& (x8 === o ())
                        &&& (x3 === Std.Pair.pair x9 x10)
                        &&& (x9 === empty ())
                        &&& (x10 === snd_ ())
                        &&& (x12 === x8)
                        &&& (y1 === Std.Pair.pair x21 x22 &&& (x13 === x22) &&& fancyEq x22) ) )
              &&& ( success
                  &&& ( success
                      &&& ( fresh (x6 x11 x26 x25 x12 x10 x9 x8 x7)
                              ( x7 === o ()
                              &&& (x8 === o ())
                              &&& (x3 === Std.Pair.pair x9 x10)
                              &&& (x9 === fill ())
                              &&& (x10 === fst_ ())
                              &&& (x12 === x8)
                              &&& (y1 === Std.Pair.pair x25 x26 &&& (x11 === x25) &&& (x6 === Std.Pair.pair x25 (o ()))) )
                          ||| fresh (x6 x11 x26 x25 x12 x10 x9 x8 x7)
                                ( x7 === o ()
                                &&& (x8 === o ())
                                &&& (x3 === Std.Pair.pair x9 x10)
                                &&& (x9 === fill ())
                                &&& (x10 === snd_ ())
                                &&& (x12 === x7)
                                &&& (y1 === Std.Pair.pair x25 x26 &&& (x11 === x26) &&& (x6 === Std.Pair.pair (o ()) x26)) )
                          ||| fresh (x6 x13 x10 x9 x8 x7)
                                ( x7 === o ()
                                &&& (x8 === o ())
                                &&& (x3 === Std.Pair.pair x9 x10)
                                &&& (x9 === empty ())
                                &&& (x10 === fst_ ())
                                &&& (x13 === x8)
                                &&& (x6 === Std.Pair.pair (o ()) (o ())) )
                          ||| fresh (x6 x13 x10 x9 x8 x7)
                                ( x7 === o ()
                                &&& (x8 === o ())
                                &&& (x3 === Std.Pair.pair x9 x10)
                                &&& (x9 === empty ())
                                &&& (x10 === snd_ ())
                                &&& (x13 === x7)
                                &&& (x6 === Std.Pair.pair (o ()) (o ())) )
                          ||| fresh (x14 x10 x9 x8 x7)
                                ( x7 === o ()
                                &&& (x8 === o ())
                                &&& (x3 === Std.Pair.pair x9 x10)
                                &&& (x9 === pour ())
                                &&& (x14 === !!false)
                                &&& ( fresh (x15 x24 x6 x16 x26 x25 x17)
                                        ( x10 === fst_ ()
                                        &&& (x17 === snd_ ())
                                        &&& ( y1 === Std.Pair.pair x25 x26 &&& (x16 === x26)
                                            &&& (x6 === Std.Pair.pair (o ()) x24 &&& (x15 === o () &&& (success &&& (x24 === o ())))) ) )
                                    ||| fresh (x15 x24 x6 x16 x26 x25 x17)
                                          ( x10 === snd_ ()
                                          &&& (x17 === fst_ ())
                                          &&& ( y1 === Std.Pair.pair x25 x26 &&& (x16 === x25)
                                              &&& (x6 === Std.Pair.pair x24 (o ()) &&& addGreaterAdd x15 x24 x25) ) ) ) ) )
                      &&& ( success
                          &&& ( x4 === Std.List.nil ()
                              &&& ( fresh (x13 x12 x11 x6)
                                      ( x6 === Std.Pair.pair x11 x12 &&& (x13 === !!true)
                                      &&& ( success
                                          &&& ( x11 === o ()
                                              &&& (y2 === o ())
                                              ||| fresh (x17 x16)
                                                    ( x11 === s x16
                                                    &&& (y2 === s x17)
                                                    &&& ( x16 === o ()
                                                        &&& (x17 === o ())
                                                        ||| fresh (x20 x19) (x16 === s x19 &&& (x17 === s x20) &&& _fancyEq x19 x20) ) ) )
                                          &&& ( success
                                              &&& ( fresh (x14) (x12 === o () &&& (y2 === o ()) &&& (x14 === !!true))
                                                  ||| fresh (x14 x15) (x12 === o () &&& (y2 === s x15) &&& (x14 === !!false))
                                                  ||| fresh (x14 x16) (x12 === s x16 &&& (y2 === o ()) &&& (x14 === !!false))
                                                  ||| fresh (x14 x17 x16) (x12 === s x16 &&& (y2 === s x17) &&& __fancyEq x17 x16 x14) ) ) ) )
                                  ||| fresh (x14 x13 x12 x11 x6)
                                        ( x6 === Std.Pair.pair x11 x12 &&& (x13 === !!false) &&& (x14 === !!true)
                                        &&& ( fresh (x20 x19 x15) (x11 === o () &&& (y2 === s x15) &&& (x12 === s x19 &&& (x15 === x20) &&& _fancyEq x19 x20))
                                            ||| fresh (x16) (x11 === s x16 &&& (y2 === o ()) &&& (x12 === o ()))
                                            ||| fresh (x23 x22 x18 x17 x16)
                                                  ( x11 === s x16
                                                  &&& (y2 === s x17)
                                                  &&& (x16 === o ())
                                                  &&& (x17 === s x18)
                                                  &&& (x12 === s x22 &&& (x23 === s x18) &&& ___fancyEq x22 x18) )
                                            ||| fresh (x23 x22 x19 x17 x16)
                                                  ( x11 === s x16
                                                  &&& (y2 === s x17)
                                                  &&& (x16 === s x19)
                                                  &&& (x17 === o ())
                                                  &&& (x12 === s x22 &&& (x23 === o ()) &&& ____fancyEq x22) )
                                            ||| fresh (x20 x19 x17 x16)
                                                  ( x11 === s x16
                                                  &&& (y2 === s x17)
                                                  &&& (x16 === s x19)
                                                  &&& (x17 === s x20)
                                                  &&& ( success
                                                      &&& ( fresh (x21) (x19 === o () &&& (x20 === s x21))
                                                          ||| fresh (x22) (x19 === s x22 &&& (x20 === o ()))
                                                          ||| fresh (x23 x22) (x19 === s x22 &&& (x20 === s x23) &&& _____fancyEq x22 x23) )
                                                      &&& (success &&& ______fancyEq x12 x20) ) ) ) ) )
                              ||| fresh (x10 x9 x8 x7)
                                    ( x4 === Std.( % ) x7 x8 &&& (x9 === !!true)
                                    &&& ( success
                                        &&& ( fresh (x15 x14 x13 x12 x11 x6)
                                                ( x6 === Std.Pair.pair x11 x12
                                                &&& (x7 === Std.Pair.pair x13 x14)
                                                &&& (x13 === fill ())
                                                &&& (x14 === fst_ ())
                                                &&& (x11 === x15) &&& ____fancyEq x15 )
                                            ||| fresh (x15 x14 x13 x12 x11 x6)
                                                  ( x6 === Std.Pair.pair x11 x12
                                                  &&& (x7 === Std.Pair.pair x13 x14)
                                                  &&& (x13 === fill ())
                                                  &&& (x14 === snd_ ())
                                                  &&& (x12 === x15) &&& ____fancyEq x15 )
                                            ||| fresh (x17 x26 x25 x16 x14 x13 x12 x11 x6)
                                                  ( x6 === Std.Pair.pair x11 x12
                                                  &&& (x7 === Std.Pair.pair x13 x14)
                                                  &&& (x13 === empty ())
                                                  &&& (x14 === fst_ ())
                                                  &&& (x11 === x16)
                                                  &&& (y1 === Std.Pair.pair x25 x26 &&& (x17 === x25) &&& _fancyEq x16 x25) )
                                            ||| fresh (x17 x26 x25 x16 x14 x13 x12 x11 x6)
                                                  ( x6 === Std.Pair.pair x11 x12
                                                  &&& (x7 === Std.Pair.pair x13 x14)
                                                  &&& (x13 === empty ())
                                                  &&& (x14 === snd_ ())
                                                  &&& (x12 === x16)
                                                  &&& (y1 === Std.Pair.pair x25 x26 &&& (x17 === x26) &&& _fancyEq x16 x26) )
                                            ||| fresh
                                                  (x28 x23 x26 x25 x24 x20 x18 x19 x22 x21 x14 x13 x12 x11 x6)
                                                  ( x6 === Std.Pair.pair x11 x12
                                                  &&& (x7 === Std.Pair.pair x13 x14)
                                                  &&& (x13 === pour ())
                                                  &&& (x14 === fst_ ())
                                                  &&& (x11 === x21) &&& (x12 === x22) &&& (x19 === !!false) &&& (x18 === x20) &&& (x20 === !!false)
                                                  &&& ( x24 === snd_ ()
                                                      &&& (y1 === Std.Pair.pair x25 x26 &&& (x23 === x26) &&& (x21 === s x28 &&& _____fancyEq x22 x26)) ) )
                                            ||| fresh
                                                  (x23 x26 x25 x24 x20 x18 x19 x22 x21 x14 x13 x12 x11 x6)
                                                  ( x6 === Std.Pair.pair x11 x12
                                                  &&& (x7 === Std.Pair.pair x13 x14)
                                                  &&& (x13 === pour ())
                                                  &&& (x14 === snd_ ())
                                                  &&& (x12 === x21) &&& (x11 === x22) &&& (x19 === !!false) &&& (x18 === x20) &&& (x20 === !!false)
                                                  &&& (x24 === fst_ () &&& (y1 === Std.Pair.pair x25 x26 &&& (x23 === x25) &&& fancyEqFancyEq x21 x22 x25)) )
                                            )
                                        &&& ( success
                                            &&& ( fresh (x15 x30 x29 x16 x14 x13 x12 x11 x6)
                                                    ( x6 === Std.Pair.pair x11 x12
                                                    &&& (x7 === Std.Pair.pair x13 x14)
                                                    &&& (x13 === fill ())
                                                    &&& (x14 === fst_ ())
                                                    &&& (x12 === x16)
                                                    &&& (y1 === Std.Pair.pair x29 x30 &&& (x15 === x29) &&& (x10 === Std.Pair.pair x29 x16)) )
                                                ||| fresh (x15 x30 x29 x16 x14 x13 x12 x11 x6)
                                                      ( x6 === Std.Pair.pair x11 x12
                                                      &&& (x7 === Std.Pair.pair x13 x14)
                                                      &&& (x13 === fill ())
                                                      &&& (x14 === snd_ ())
                                                      &&& (x11 === x16)
                                                      &&& (y1 === Std.Pair.pair x29 x30 &&& (x15 === x30) &&& (x10 === Std.Pair.pair x16 x30)) )
                                                ||| fresh (x17 x14 x13 x12 x11 x6)
                                                      ( x6 === Std.Pair.pair x11 x12
                                                      &&& (x7 === Std.Pair.pair x13 x14)
                                                      &&& (x13 === empty ())
                                                      &&& (x14 === fst_ ())
                                                      &&& (x12 === x17)
                                                      &&& (x10 === Std.Pair.pair (o ()) x17) )
                                                ||| fresh (x17 x14 x13 x12 x11 x6)
                                                      ( x6 === Std.Pair.pair x11 x12
                                                      &&& (x7 === Std.Pair.pair x13 x14)
                                                      &&& (x13 === empty ())
                                                      &&& (x14 === snd_ ())
                                                      &&& (x11 === x17)
                                                      &&& (x10 === Std.Pair.pair x17 (o ())) )
                                                ||| fresh (x18 x14 x13 x12 x11 x6)
                                                      ( x6 === Std.Pair.pair x11 x12
                                                      &&& (x7 === Std.Pair.pair x13 x14)
                                                      &&& (x13 === pour ())
                                                      &&& (x18 === !!true)
                                                      &&& ( fresh
                                                              (x22 x24 x19 x23 x34 x33 x27 x25 x32 x31 x26 x20 x30 x29 x21)
                                                              ( x14 === fst_ ()
                                                              &&& (x21 === snd_ ())
                                                              &&& ( y1 === Std.Pair.pair x29 x30 &&& (x20 === x30)
                                                                  &&& ( x26 === snd_ ()
                                                                      &&& ( x29 === x31 &&& (x30 === x32) &&& (x25 === x32)
                                                                          &&& ( x27 === snd_ ()
                                                                              &&& ( x31 === x33 &&& (x32 === x34) &&& (x23 === x34)
                                                                                  &&& ( success
                                                                                      &&& ( success &&& add x11 x12 x19
                                                                                          &&& ( success
                                                                                              &&& ( fresh (x35) (x19 === s x35 &&& (x34 === o ()))
                                                                                                  ||| fresh (x36 x35)
                                                                                                        (x19 === s x35 &&& (x34 === s x36) &&& greater x35 x36)
                                                                                                  ) )
                                                                                          &&& (success &&& add x11 x12 x24)
                                                                                          &&& ( success
                                                                                              &&& ( x34 === o () &&& (x22 === x24)
                                                                                                  ||| fresh (x35)
                                                                                                        (x34 === s x35 &&& (x24 === o ()) &&& (x22 === o ()))
                                                                                                  ||| fresh (x36 x35)
                                                                                                        (x34 === s x35 &&& (x24 === s x36) &&& sub x22 x36 x35)
                                                                                                  ) ) )
                                                                                      &&& (success &&& createState x10 x34 x22) ) ) ) ) ) ) )
                                                          ||| fresh
                                                                (x22 x24 x19 x23 x34 x33 x27 x25 x32 x31 x26 x20 x30 x29 x21)
                                                                ( x14 === snd_ ()
                                                                &&& (x21 === fst_ ())
                                                                &&& ( y1 === Std.Pair.pair x29 x30 &&& (x20 === x29)
                                                                    &&& ( x26 === fst_ ()
                                                                        &&& ( x29 === x31 &&& (x30 === x32) &&& (x25 === x31)
                                                                            &&& ( x27 === fst_ ()
                                                                                &&& ( x31 === x33 &&& (x32 === x34) &&& (x23 === x33)
                                                                                    &&& ( success
                                                                                        &&& ( success &&& add x11 x12 x19
                                                                                            &&& (success &&& greater x19 x33)
                                                                                            &&& (success &&& add x11 x12 x24)
                                                                                            &&& (success &&& sub x22 x24 x33) )
                                                                                        &&& (success &&& _createState x10 x33 x22) ) ) ) ) ) ) ) ) )
                                                ||| fresh (x18 x14 x13 x12 x11 x6)
                                                      ( x6 === Std.Pair.pair x11 x12
                                                      &&& (x7 === Std.Pair.pair x13 x14)
                                                      &&& (x13 === pour ())
                                                      &&& (x18 === !!false)
                                                      &&& ( fresh (x28 x19 x20 x30 x29 x21)
                                                              ( x14 === fst_ ()
                                                              &&& (x21 === snd_ ())
                                                              &&& ( y1 === Std.Pair.pair x29 x30 &&& (x20 === x30)
                                                                  &&& ( success
                                                                      &&& ( success &&& add x11 x12 x19
                                                                          &&& ( success
                                                                              &&& ( x19 === o ()
                                                                                  ||| fresh (x32 x31) (x19 === s x31 &&& (x30 === s x32) &&& _greater x31 x32)
                                                                                  ) )
                                                                          &&& (success &&& add x11 x12 x28) )
                                                                      &&& (success &&& __createState x10 x28) ) ) )
                                                          ||| fresh (x28 x19 x20 x30 x29 x21)
                                                                ( x14 === snd_ ()
                                                                &&& (x21 === fst_ ())
                                                                &&& ( y1 === Std.Pair.pair x29 x30 &&& (x20 === x29)
                                                                    &&& ( success
                                                                        &&& ( success &&& add x11 x12 x19
                                                                            &&& (success &&& _greater x19 x29)
                                                                            &&& (success &&& add x11 x12 x28) )
                                                                        &&& (success &&& ___createState x10 x28) ) ) ) ) ) ) )
                                        &&& (success &&& checkAnswer_ y1 y2 x8 x10) ) ) ) ) ) ) ) )
  and fancyEq y3 = y3 === o ()
  and addGreaterAdd y4 y5 y6 = y4 === o () &&& (y5 === o ())
  and _fancyEq y7 y8 = y7 === o () &&& (y8 === o ()) ||| fresh (x20 x19) (y7 === s x19 &&& (y8 === s x20) &&& _fancyEq x19 x20)
  and __fancyEq y9 y10 y11 =
    y10 === o ()
    &&& (y9 === o ())
    &&& (y11 === !!true)
    ||| fresh (x15) (y10 === o () &&& (y9 === s x15) &&& (y11 === !!false))
    ||| fresh (x16) (y10 === s x16 &&& (y9 === o ()) &&& (y11 === !!false))
    ||| fresh (x17 x16) (y10 === s x16 &&& (y9 === s x17) &&& __fancyEq x17 x16 y11)
  and ___fancyEq y12 y13 = fresh (q1) (y12 === s q1 &&& _fancyEq q1 y13)
  and ____fancyEq y14 = y14 === o ()
  and _____fancyEq y15 y16 =
    fresh (x21) (y15 === o () &&& (y16 === s x21))
    ||| fresh (x22) (y15 === s x22 &&& (y16 === o ()))
    ||| fresh (x23 x22) (y15 === s x22 &&& (y16 === s x23) &&& _____fancyEq x22 x23)
  and ______fancyEq y17 y18 = fresh (q1) (y17 === s q1 &&& ___fancyEq q1 y18)
  and fancyEqFancyEq y19 y20 y21 = fresh (q1) (y19 === s q1 &&& _____fancyEq y20 y21)
  and add y22 y23 y24 = y22 === o () &&& (y23 === y24) ||| fresh (x36 x35) (y22 === s x35 &&& (y24 === s x36) &&& add x35 y23 x36)
  and greater y25 y26 = fresh (x35) (y25 === s x35 &&& (y26 === o ())) ||| fresh (x36 x35) (y25 === s x35 &&& (y26 === s x36) &&& greater x35 x36)
  and sub y27 y28 y29 =
    y29 === o () &&& (y27 === y28)
    ||| fresh (x35) (y29 === s x35 &&& (y28 === o ()) &&& (y27 === o ()))
    ||| fresh (x36 x35) (y29 === s x35 &&& (y28 === s x36) &&& sub y27 x36 x35)
  and createState y30 y31 y32 = y30 === Std.Pair.pair y32 y31
  and _createState y33 y34 y35 = y33 === Std.Pair.pair y34 y35
  and _greater y36 y37 = y36 === o () ||| fresh (x32 x31) (y36 === s x31 &&& (y37 === s x32) &&& _greater x31 x32)
  and __createState y38 y39 = y38 === Std.Pair.pair (o ()) y39
  and ___createState y40 y41 = y40 === Std.Pair.pair y41 (o ())
  and checkAnswer_ y42 y43 y44 y45 =
    y44 === Std.List.nil ()
    &&& ( fresh (x13 x12 x11)
            ( y45 === Std.Pair.pair x11 x12 &&& (x13 === !!true)
            &&& ( success
                &&& ( x11 === o ()
                    &&& (y43 === o ())
                    ||| fresh (x17 x16)
                          ( x11 === s x16
                          &&& (y43 === s x17)
                          &&& (x16 === o () &&& (x17 === o ()) ||| fresh (x20 x19) (x16 === s x19 &&& (x17 === s x20) &&& _fancyEq x19 x20)) ) )
                &&& ( success
                    &&& ( fresh (x14) (x12 === o () &&& (y43 === o ()) &&& (x14 === !!true))
                        ||| fresh (x14 x15) (x12 === o () &&& (y43 === s x15) &&& (x14 === !!false))
                        ||| fresh (x14 x16) (x12 === s x16 &&& (y43 === o ()) &&& (x14 === !!false))
                        ||| fresh (x14 x17 x16) (x12 === s x16 &&& (y43 === s x17) &&& __fancyEq x17 x16 x14) ) ) ) )
        ||| fresh (x14 x13 x12 x11)
              ( y45 === Std.Pair.pair x11 x12 &&& (x13 === !!false) &&& (x14 === !!true)
              &&& ( fresh (x20 x19 x15) (x11 === o () &&& (y43 === s x15) &&& (x12 === s x19 &&& (x15 === x20) &&& _fancyEq x19 x20))
                  ||| fresh (x16) (x11 === s x16 &&& (y43 === o ()) &&& (x12 === o ()))
                  ||| fresh (x23 x22 x18 x17 x16)
                        ( x11 === s x16
                        &&& (y43 === s x17)
                        &&& (x16 === o ())
                        &&& (x17 === s x18)
                        &&& (x12 === s x22 &&& (x23 === s x18) &&& ___fancyEq x22 x18) )
                  ||| fresh (x23 x22 x19 x17 x16)
                        (x11 === s x16 &&& (y43 === s x17) &&& (x16 === s x19) &&& (x17 === o ()) &&& (x12 === s x22 &&& (x23 === o ()) &&& ____fancyEq x22))
                  ||| fresh (x20 x19 x17 x16)
                        ( x11 === s x16
                        &&& (y43 === s x17)
                        &&& (x16 === s x19)
                        &&& (x17 === s x20)
                        &&& ( success
                            &&& ( fresh (x21) (x19 === o () &&& (x20 === s x21))
                                ||| fresh (x22) (x19 === s x22 &&& (x20 === o ()))
                                ||| fresh (x23 x22) (x19 === s x22 &&& (x20 === s x23) &&& _____fancyEq x22 x23) )
                            &&& (success &&& ______fancyEq x12 x20) ) ) ) ) )
    ||| fresh (x10 x9 x8 x7)
          ( y44 === Std.( % ) x7 x8 &&& (x9 === !!true)
          &&& ( success
              &&& ( fresh (x15 x14 x13 x12 x11)
                      ( y45 === Std.Pair.pair x11 x12
                      &&& (x7 === Std.Pair.pair x13 x14)
                      &&& (x13 === fill ())
                      &&& (x14 === fst_ ())
                      &&& (x11 === x15) &&& ____fancyEq x15 )
                  ||| fresh (x15 x14 x13 x12 x11)
                        ( y45 === Std.Pair.pair x11 x12
                        &&& (x7 === Std.Pair.pair x13 x14)
                        &&& (x13 === fill ())
                        &&& (x14 === snd_ ())
                        &&& (x12 === x15) &&& ____fancyEq x15 )
                  ||| fresh (x17 x26 x25 x16 x14 x13 x12 x11)
                        ( y45 === Std.Pair.pair x11 x12
                        &&& (x7 === Std.Pair.pair x13 x14)
                        &&& (x13 === empty ())
                        &&& (x14 === fst_ ())
                        &&& (x11 === x16)
                        &&& (y42 === Std.Pair.pair x25 x26 &&& (x17 === x25) &&& _fancyEq x16 x25) )
                  ||| fresh (x17 x26 x25 x16 x14 x13 x12 x11)
                        ( y45 === Std.Pair.pair x11 x12
                        &&& (x7 === Std.Pair.pair x13 x14)
                        &&& (x13 === empty ())
                        &&& (x14 === snd_ ())
                        &&& (x12 === x16)
                        &&& (y42 === Std.Pair.pair x25 x26 &&& (x17 === x26) &&& _fancyEq x16 x26) )
                  ||| fresh
                        (x28 x23 x26 x25 x24 x20 x18 x19 x22 x21 x14 x13 x12 x11)
                        ( y45 === Std.Pair.pair x11 x12
                        &&& (x7 === Std.Pair.pair x13 x14)
                        &&& (x13 === pour ())
                        &&& (x14 === fst_ ())
                        &&& (x11 === x21) &&& (x12 === x22) &&& (x19 === !!false) &&& (x18 === x20) &&& (x20 === !!false)
                        &&& (x24 === snd_ () &&& (y42 === Std.Pair.pair x25 x26 &&& (x23 === x26) &&& (x21 === s x28 &&& _____fancyEq x22 x26))) )
                  ||| fresh (x23 x26 x25 x24 x20 x18 x19 x22 x21 x14 x13 x12 x11)
                        ( y45 === Std.Pair.pair x11 x12
                        &&& (x7 === Std.Pair.pair x13 x14)
                        &&& (x13 === pour ())
                        &&& (x14 === snd_ ())
                        &&& (x12 === x21) &&& (x11 === x22) &&& (x19 === !!false) &&& (x18 === x20) &&& (x20 === !!false)
                        &&& (x24 === fst_ () &&& (y42 === Std.Pair.pair x25 x26 &&& (x23 === x25) &&& fancyEqFancyEq x21 x22 x25)) ) )
              &&& ( success
                  &&& ( fresh (x15 x30 x29 x16 x14 x13 x12 x11)
                          ( y45 === Std.Pair.pair x11 x12
                          &&& (x7 === Std.Pair.pair x13 x14)
                          &&& (x13 === fill ())
                          &&& (x14 === fst_ ())
                          &&& (x12 === x16)
                          &&& (y42 === Std.Pair.pair x29 x30 &&& (x15 === x29) &&& (x10 === Std.Pair.pair x29 x16)) )
                      ||| fresh (x15 x30 x29 x16 x14 x13 x12 x11)
                            ( y45 === Std.Pair.pair x11 x12
                            &&& (x7 === Std.Pair.pair x13 x14)
                            &&& (x13 === fill ())
                            &&& (x14 === snd_ ())
                            &&& (x11 === x16)
                            &&& (y42 === Std.Pair.pair x29 x30 &&& (x15 === x30) &&& (x10 === Std.Pair.pair x16 x30)) )
                      ||| fresh (x17 x14 x13 x12 x11)
                            ( y45 === Std.Pair.pair x11 x12
                            &&& (x7 === Std.Pair.pair x13 x14)
                            &&& (x13 === empty ())
                            &&& (x14 === fst_ ())
                            &&& (x12 === x17)
                            &&& (x10 === Std.Pair.pair (o ()) x17) )
                      ||| fresh (x17 x14 x13 x12 x11)
                            ( y45 === Std.Pair.pair x11 x12
                            &&& (x7 === Std.Pair.pair x13 x14)
                            &&& (x13 === empty ())
                            &&& (x14 === snd_ ())
                            &&& (x11 === x17)
                            &&& (x10 === Std.Pair.pair x17 (o ())) )
                      ||| fresh (x18 x14 x13 x12 x11)
                            ( y45 === Std.Pair.pair x11 x12
                            &&& (x7 === Std.Pair.pair x13 x14)
                            &&& (x13 === pour ())
                            &&& (x18 === !!true)
                            &&& ( fresh
                                    (x22 x24 x19 x23 x34 x33 x27 x25 x32 x31 x26 x20 x30 x29 x21)
                                    ( x14 === fst_ ()
                                    &&& (x21 === snd_ ())
                                    &&& ( y42 === Std.Pair.pair x29 x30 &&& (x20 === x30)
                                        &&& ( x26 === snd_ ()
                                            &&& ( x29 === x31 &&& (x30 === x32) &&& (x25 === x32)
                                                &&& ( x27 === snd_ ()
                                                    &&& ( x31 === x33 &&& (x32 === x34) &&& (x23 === x34)
                                                        &&& ( success
                                                            &&& ( success &&& add x11 x12 x19
                                                                &&& ( success
                                                                    &&& ( fresh (x35) (x19 === s x35 &&& (x34 === o ()))
                                                                        ||| fresh (x36 x35) (x19 === s x35 &&& (x34 === s x36) &&& greater x35 x36) ) )
                                                                &&& (success &&& add x11 x12 x24)
                                                                &&& ( success
                                                                    &&& ( x34 === o () &&& (x22 === x24)
                                                                        ||| fresh (x35) (x34 === s x35 &&& (x24 === o ()) &&& (x22 === o ()))
                                                                        ||| fresh (x36 x35) (x34 === s x35 &&& (x24 === s x36) &&& sub x22 x36 x35) ) ) )
                                                            &&& (success &&& createState x10 x34 x22) ) ) ) ) ) ) )
                                ||| fresh
                                      (x22 x24 x19 x23 x34 x33 x27 x25 x32 x31 x26 x20 x30 x29 x21)
                                      ( x14 === snd_ ()
                                      &&& (x21 === fst_ ())
                                      &&& ( y42 === Std.Pair.pair x29 x30 &&& (x20 === x29)
                                          &&& ( x26 === fst_ ()
                                              &&& ( x29 === x31 &&& (x30 === x32) &&& (x25 === x31)
                                                  &&& ( x27 === fst_ ()
                                                      &&& ( x31 === x33 &&& (x32 === x34) &&& (x23 === x33)
                                                          &&& ( success
                                                              &&& ( success &&& add x11 x12 x19
                                                                  &&& (success &&& greater x19 x33)
                                                                  &&& (success &&& add x11 x12 x24)
                                                                  &&& (success &&& sub x22 x24 x33) )
                                                              &&& (success &&& _createState x10 x33 x22) ) ) ) ) ) ) ) ) )
                      ||| fresh (x18 x14 x13 x12 x11)
                            ( y45 === Std.Pair.pair x11 x12
                            &&& (x7 === Std.Pair.pair x13 x14)
                            &&& (x13 === pour ())
                            &&& (x18 === !!false)
                            &&& ( fresh (x28 x19 x20 x30 x29 x21)
                                    ( x14 === fst_ ()
                                    &&& (x21 === snd_ ())
                                    &&& ( y42 === Std.Pair.pair x29 x30 &&& (x20 === x30)
                                        &&& ( success
                                            &&& ( success &&& add x11 x12 x19
                                                &&& (success &&& (x19 === o () ||| fresh (x32 x31) (x19 === s x31 &&& (x30 === s x32) &&& _greater x31 x32)))
                                                &&& (success &&& add x11 x12 x28) )
                                            &&& (success &&& __createState x10 x28) ) ) )
                                ||| fresh (x28 x19 x20 x30 x29 x21)
                                      ( x14 === snd_ ()
                                      &&& (x21 === fst_ ())
                                      &&& ( y42 === Std.Pair.pair x29 x30 &&& (x20 === x29)
                                          &&& ( success
                                              &&& (success &&& add x11 x12 x19 &&& (success &&& _greater x19 x29) &&& (success &&& add x11 x12 x28))
                                              &&& (success &&& ___createState x10 x28) ) ) ) ) ) ) )
              &&& (success &&& checkAnswer_ y42 y43 x8 x10) ) )
  in
  checkAnswer x0 x1 x2

(* let topLevel x0 x1 x2 =
  let rec checkAnswer y0 y1 y2 =
    fresh
      (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30)
      ( y0 === Std.List.nil ()
      &&& (y2 === o ())
      ||| ( y0 === Std.( % ) q1 q2
          &&& ( q3 === o ()
              &&& (q4 === o ())
              &&& (q1 === Std.Pair.pair q5 q6)
              &&& (q5 === fill ())
              &&& (q6 === fst_ ())
              &&& (q7 === q3)
              ||| (q3 === o () &&& (q4 === o ()) &&& (q1 === Std.Pair.pair q5 q6) &&& (q5 === fill ()) &&& (q6 === snd_ ()) &&& (q7 === q4))
              ||| ( q3 === o ()
                  &&& (q4 === o ())
                  &&& (q1 === Std.Pair.pair q5 q6)
                  &&& (q5 === empty ())
                  &&& (q6 === fst_ ())
                  &&& (q8 === q3)
                  &&& (y1 === Std.Pair.pair q9 q10 &&& (q11 === q9) &&& (q9 === o ())) )
              ||| ( q3 === o ()
                  &&& (q4 === o ())
                  &&& (q1 === Std.Pair.pair q5 q6)
                  &&& (q5 === empty ())
                  &&& (q6 === snd_ ())
                  &&& (q8 === q4)
                  &&& (y1 === Std.Pair.pair q9 q10 &&& (q11 === q10) &&& fancyEq q10) )
              &&& ( q3 === o ()
                  &&& (q4 === o ())
                  &&& (q1 === Std.Pair.pair q5 q6)
                  &&& (q5 === fill ())
                  &&& (q6 === fst_ ())
                  &&& (q8 === q4)
                  &&& (y1 === Std.Pair.pair q12 q13 &&& (q7 === q12) &&& (q14 === Std.Pair.pair q12 (o ())))
                  ||| ( q3 === o ()
                      &&& (q4 === o ())
                      &&& (q1 === Std.Pair.pair q5 q6)
                      &&& (q5 === fill ())
                      &&& (q6 === snd_ ())
                      &&& (q8 === q3)
                      &&& (y1 === Std.Pair.pair q12 q13 &&& (q7 === q13) &&& (q14 === Std.Pair.pair (o ()) q13)) )
                  ||| ( q3 === o ()
                      &&& (q4 === o ())
                      &&& (q1 === Std.Pair.pair q5 q6)
                      &&& (q5 === empty ())
                      &&& (q6 === fst_ ())
                      &&& (q11 === q4)
                      &&& (q14 === Std.Pair.pair (o ()) (o ())) )
                  ||| ( q3 === o ()
                      &&& (q4 === o ())
                      &&& (q1 === Std.Pair.pair q5 q6)
                      &&& (q5 === empty ())
                      &&& (q6 === snd_ ())
                      &&& (q11 === q3)
                      &&& (q14 === Std.Pair.pair (o ()) (o ())) )
                  ||| (fresh (q16) ( q3 === o ()
                      &&& (q4 === o ())
                      &&& (q1 === Std.Pair.pair q5 q6)
                      &&& (q5 === pour ())
                      &&& (q15 === !!false)
                      &&& (fresh (q18) ( q6 === fst_ ()
                          &&& (q16 === snd_ ())
                          &&& (y1 === Std.Pair.pair q12 q13 &&& (q17 === q13) &&& (q14 === Std.Pair.pair (o ()) q18 &&& (q19 === o () &&& (q18 === o ()))))
                          ||| ( q6 === snd_ ()
                              &&& (q16 === fst_ ())
                              &&& (y1 === Std.Pair.pair q12 q13 &&& (q17 === q12) &&& (q14 === Std.Pair.pair q18 (o ()) &&& addGreaterAdd q19 q18 q12)) ) ) )))
                  &&& ( q2 === Std.List.nil ()
                      &&& (fresh (q11) ( q14 === Std.Pair.pair q7 q8 &&& (q11 === !!true)
                          &&& ( q7 === o ()
                              &&& (y2 === o ())
                              ||| ( q7 === s q17
                                  &&& (y2 === s q16)
                                  &&& (q17 === o () &&& (q16 === o ()) ||| (q17 === s q20 &&& (q16 === s q21) &&& _fancyEq q20 q21)) )
                              &&& ( q8 === o ()
                                  &&& (y2 === o ())
                                  &&& (q15 === !!true)
                                  ||| (q8 === o () &&& (y2 === s q19) &&& (q15 === !!false))
                                  ||| (q8 === s q17 &&& (y2 === o ()) &&& (q15 === !!false))
                                  ||| (q8 === s q17 &&& (y2 === s q16) &&& __fancyEq q16 q17 q15) ) )
                          ||| ( q14 === Std.Pair.pair q7 q8 &&& (q11 === !!false) &&& (q15 === !!true)
                              &&& ( q7 === o ()
                                  &&& (y2 === s q19)
                                  &&& (q8 === s q20 &&& _fancyEq q20 q19)
                                  ||| (q7 === s q17 &&& (y2 === o ()) &&& (q8 === o ()))
                                  ||| (q7 === s q17 &&& (y2 === s q16) &&& (q17 === o ()) &&& (q16 === s q22) &&& (q8 === s q10 &&& ___fancyEq q10 q22))
                                  ||| (q7 === s q17 &&& (y2 === s q16) &&& (q17 === s q20) &&& (q16 === o ()) &&& (q8 === s q10 &&& ____fancyEq q10))
                                  ||| ( q7 === s q17
                                      &&& (y2 === s q16)
                                      &&& (q17 === s q20)
                                      &&& (q16 === s q21)
                                      &&& ( q20 === o ()
                                          &&& (q21 === s q9)
                                          ||| (q20 === s q10 &&& (q21 === o ()))
                                          ||| (q20 === s q10 &&& (q21 === s q23) &&& _____fancyEq q10 q23)
                                          &&& ______fancyEq q8 q21 ) )) ) ) )
                      ||| (fresh (q3 q4 q5 q6 q20 q21 q22) ( q2 === Std.( % ) q3 q4 &&& (q5 === !!true)
                          &&& (fresh (q11 q15) ( q14 === Std.Pair.pair q7 q8
                              &&& (q3 === Std.Pair.pair q11 q15)
                              &&& (q11 === fill ())
                              &&& (q15 === fst_ ())
                              &&& (q7 === q19) &&& ____fancyEq q19
                              ||| ( q14 === Std.Pair.pair q7 q8
                                  &&& (q3 === Std.Pair.pair q11 q15)
                                  &&& (q11 === fill ())
                                  &&& (q15 === snd_ ())
                                  &&& (q8 === q19) &&& ____fancyEq q19 )
                              ||| ( q14 === Std.Pair.pair q7 q8
                                  &&& (q3 === Std.Pair.pair q11 q15)
                                  &&& (q11 === empty ())
                                  &&& (q15 === fst_ ())
                                  &&& (q7 === q17)
                                  &&& (y1 === Std.Pair.pair q12 q13 &&& (q16 === q12) &&& _fancyEq q17 q12) )
                              ||| ( q14 === Std.Pair.pair q7 q8
                                  &&& (q3 === Std.Pair.pair q11 q15)
                                  &&& (q11 === empty ())
                                  &&& (q15 === snd_ ())
                                  &&& (q8 === q17)
                                  &&& (y1 === Std.Pair.pair q12 q13 &&& (q16 === q13) &&& _fancyEq q17 q13) )
                              ||| ( q14 === Std.Pair.pair q7 q8
                                  &&& (q3 === Std.Pair.pair q11 q15)
                                  &&& (q11 === pour ())
                                  &&& (q15 === fst_ ())
                                  &&& (q7 === q9) &&& (q8 === q10) &&& (q20 === !!false) &&& (q22 === q21) &&& (q21 === !!false)
                                  &&& (q18 === snd_ () &&& (y1 === Std.Pair.pair q12 q13 &&& (q23 === q13) &&& (q9 === s q24 &&& _____fancyEq q10 q13))) )
                              ||| ( q14 === Std.Pair.pair q7 q8
                                  &&& (q3 === Std.Pair.pair q11 q15)
                                  &&& (q11 === pour ())
                                  &&& (q15 === snd_ ())
                                  &&& (q8 === q9) &&& (q7 === q10) &&& (q20 === !!false) &&& (q22 === q21) &&& (q21 === !!false)
                                  &&& (q18 === fst_ () &&& (y1 === Std.Pair.pair q12 q13 &&& (q23 === q12) &&& fancyEqFancyEq q9 q10 q12)) )
                              &&& ( q14 === Std.Pair.pair q7 q8
                                  &&& (q3 === Std.Pair.pair q11 q15)
                                  &&& (q11 === fill ())
                                  &&& (q15 === fst_ ())
                                  &&& (q8 === q17)
                                  &&& (y1 === Std.Pair.pair q19 q25 &&& (q6 === Std.Pair.pair q19 q17))
                                  ||| ( q14 === Std.Pair.pair q7 q8
                                      &&& (q3 === Std.Pair.pair q11 q15)
                                      &&& (q11 === fill ())
                                      &&& (q15 === snd_ ())
                                      &&& (q7 === q17)
                                      &&& (y1 === Std.Pair.pair q26 q19 &&& (q6 === Std.Pair.pair q17 q19)) )
                                  ||| ( q14 === Std.Pair.pair q7 q8
                                      &&& (q3 === Std.Pair.pair q11 q15)
                                      &&& (q11 === empty ())
                                      &&& (q15 === fst_ ())
                                      &&& (q8 === q16)
                                      &&& (q6 === Std.Pair.pair (o ()) q16) )
                                  ||| ( q14 === Std.Pair.pair q7 q8
                                      &&& (q3 === Std.Pair.pair q11 q15)
                                      &&& (q11 === empty ())
                                      &&& (q15 === snd_ ())
                                      &&& (q7 === q16)
                                      &&& (q6 === Std.Pair.pair q16 (o ())) )
                                  ||| (fresh (q9 q13 q18 q20 q21 q22 q27) ( q14 === Std.Pair.pair q7 q8
                                      &&& (q3 === Std.Pair.pair q11 q15)
                                      &&& (q11 === pour ())
                                      &&& (q22 === !!true)
                                      &&& ( q15 === fst_ ()
                                          &&& (q9 === snd_ ())
                                          &&& ( y1 === Std.Pair.pair q26 q21
                                              &&& ( q13 === snd_ ()
                                                  &&& ( q12 === q21
                                                      &&& ( q23 === q21
                                                          &&& ( add q7 q8 q20
                                                              &&& (q20 === s q27 &&& (q21 === o ()) ||| (q20 === s q27 &&& (q21 === s q28) &&& greater q27 q28))
                                                              &&& add q7 q8 q18
                                                              &&& ( q21 === o () &&& (q10 === q18)
                                                                  ||| (q21 === s q27 &&& (q18 === o ()) &&& (q10 === o ()))
                                                                  ||| (q21 === s q27 &&& (q18 === s q28) &&& sub q10 q28 q27) )
                                                              &&& createState q6 q21 q10 ) ) ) ) )
                                          ||| ( q15 === snd_ ()
                                              &&& (q9 === fst_ ())
                                              &&& ( y1 === Std.Pair.pair q21 q25
                                                  &&& ( q13 === fst_ ()
                                                      &&& ( q12 === q21
                                                          &&& ( q23 === q21
                                                              &&& ( add q7 q8 q20 &&& greater q20 q21 &&& add q7 q8 q18 &&& sub q10 q18 q21
                                                                  &&& _createState q6 q21 q10 ) ) ) ) ) ) ) ))
                                  ||| (fresh (q9 q18 q20 q21 q22) ( q14 === Std.Pair.pair q7 q8
                                      &&& (q3 === Std.Pair.pair q11 q15)
                                      &&& (q11 === pour ())
                                      &&& (q22 === !!false)
                                      &&& ( q15 === fst_ ()
                                          &&& (q9 === snd_ ())
                                          &&& ( y1 === Std.Pair.pair q26 q21
                                              &&& ( add q7 q8 q20
                                                  &&& (q20 === o () ||| (q20 === s q29 &&& (q21 === s q30) &&& _greater q29 q30))
                                                  &&& add q7 q8 q24 &&& __createState q6 q24 ) )
                                          ||| ( q15 === snd_ ()
                                              &&& (q9 === fst_ ())
                                              &&& ( y1 === Std.Pair.pair q21 q25
                                                  &&& (add q7 q8 q20 &&& _greater q20 q21 &&& add q7 q8 q24 &&& ___createState q6 q24) ) ) ) )) )
                              &&& checkAnswer_ y1 y2 q4 q6 )) ) ) )) ) ) )
  and fancyEq y3 = y3 === o ()
  and addGreaterAdd y4 y5 y6 = y4 === o () &&& (y5 === o ())
  and _fancyEq y7 y8 = fresh (q1 q2) (y7 === o () &&& (y8 === o ()) ||| (y7 === s q1 &&& (y8 === s q2) &&& _fancyEq q1 q2))
  and __fancyEq y9 y10 y11 =
    fresh (q1 q2 q3)
      ( y10 === o ()
      &&& (y9 === o ())
      &&& (y11 === !!true)
      ||| (y10 === o () &&& (y9 === s q1) &&& (y11 === !!false))
      ||| (y10 === s q2 &&& (y9 === o ()) &&& (y11 === !!false))
      ||| (y10 === s q2 &&& (y9 === s q3) &&& __fancyEq q3 q2 y11) )
  and ___fancyEq y12 y13 = fresh (q1) (y12 === s q1 &&& _fancyEq q1 y13)
  and ____fancyEq y14 = y14 === o ()
  and _____fancyEq y15 y16 =
    fresh (q1 q2 q3) (y15 === o () &&& (y16 === s q1) ||| (y15 === s q2 &&& (y16 === o ())) ||| (y15 === s q2 &&& (y16 === s q3) &&& _____fancyEq q2 q3))
  and ______fancyEq y17 y18 = fresh (q1) (y17 === s q1 &&& ___fancyEq q1 y18)
  and fancyEqFancyEq y19 y20 y21 = fresh (q1) (y19 === s q1 &&& _____fancyEq y20 y21)
  and add y22 y23 y24 = fresh (q1 q2) (y22 === o () &&& (y23 === y24) ||| (y22 === s q1 &&& (y24 === s q2) &&& add q1 y23 q2))
  and greater y25 y26 = fresh (q1 q2) (y25 === s q1 &&& (y26 === o ()) ||| (y25 === s q1 &&& (y26 === s q2) &&& greater q1 q2))
  and sub y27 y28 y29 =
    fresh (q1 q2)
      (y29 === o () &&& (y27 === y28) ||| (y29 === s q1 &&& (y28 === o ()) &&& (y27 === o ())) ||| (y29 === s q1 &&& (y28 === s q2) &&& sub y27 q2 q1))
  and createState y30 y31 y32 = y30 === Std.Pair.pair y32 y31
  and _createState y33 y34 y35 = y33 === Std.Pair.pair y34 y35
  and _greater y36 y37 = fresh (q1 q2) (y36 === o () ||| (y36 === s q1 &&& (y37 === s q2) &&& _greater q1 q2))
  and __createState y38 y39 = y38 === Std.Pair.pair (o ()) y39
  and ___createState y40 y41 = y40 === Std.Pair.pair y41 (o ())
  and checkAnswer_ y42 y43 y44 y45 =
    fresh
      (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26)
      ( y44 === Std.List.nil ()
      &&& ( y45 === Std.Pair.pair q1 q2
          &&& ( q1 === o ()
              &&& (y43 === o ())
              ||| (q1 === s q3 &&& (y43 === s q4) &&& (q3 === o () &&& (q4 === o ()) ||| (q3 === s q5 &&& (q4 === s q6) &&& _fancyEq q5 q6)))
              &&& ( q2 === o ()
                  &&& (y43 === o ())
                  ||| (q2 === o () &&& (y43 === s q7))
                  ||| (q2 === s q3 &&& (y43 === o ()))
                  ||| (q2 === s q3 &&& (y43 === s q4) &&& __fancyEq q4 q3 q8) ) )
          ||| ( y45 === Std.Pair.pair q1 q2
              &&& ( q1 === o ()
                  &&& (y43 === s q7)
                  &&& (q2 === s q5 &&& _fancyEq q5 q7)
                  ||| (q1 === s q3 &&& (y43 === o ()) &&& (q2 === o ()))
                  ||| (q1 === s (o ()) &&& (y43 === s (s q9)) &&& (q2 === s q10 &&& ___fancyEq q10 q9))
                  ||| (q1 === s (s q5) &&& (y43 === s (o ())) &&& (q2 === s q10 &&& ____fancyEq q10))
                  ||| ( q1
                      === s (s q5)
                      &&& (y43 === s (s q6))
                      &&& ( q5 === o ()
                          &&& (q6 === s q11)
                          ||| (q5 === s q10 &&& (q6 === o ()))
                          ||| (q5 === s q10 &&& (q6 === s q12) &&& _____fancyEq q10 q12)
                          &&& ______fancyEq q2 q6 ) ) ) ) )
      ||| (fresh (q5 q6 q8 q9) ( y44 === Std.( % ) q13 q14
          &&& ( y45 === Std.Pair.pair q1 q2
              &&& (q13 === Std.Pair.pair q15 q8)
              &&& (q15 === fill ())
              &&& (q8 === fst_ ())
              &&& (q1 === q7) &&& ____fancyEq q7
              ||| (y45 === Std.Pair.pair q1 q2 &&& (q13 === Std.Pair.pair q15 q8) &&& (q15 === fill ()) &&& (q8 === snd_ ()) &&& (q2 === q7) &&& ____fancyEq q7)
              ||| ( y45 === Std.Pair.pair q1 q2
                  &&& (q13 === Std.Pair.pair q15 q8)
                  &&& (q15 === empty ())
                  &&& (q8 === fst_ ())
                  &&& (q1 === q3)
                  &&& (y42 === Std.Pair.pair q16 q17 &&& (q4 === q16) &&& _fancyEq q3 q16) )
              ||| ( y45 === Std.Pair.pair q1 q2
                  &&& (q13 === Std.Pair.pair q15 q8)
                  &&& (q15 === empty ())
                  &&& (q8 === snd_ ())
                  &&& (q2 === q3)
                  &&& (y42 === Std.Pair.pair q16 q17 &&& (q4 === q17) &&& _fancyEq q3 q17) )
              ||| ( y45 === Std.Pair.pair q1 q2
                  &&& (q13 === Std.Pair.pair q15 q8)
                  &&& (q15 === pour ())
                  &&& (q8 === fst_ ())
                  &&& (q1 === q11) &&& (q2 === q10) &&& (q5 === !!false) &&& (q9 === q6) &&& (q6 === !!false)
                  &&& (q18 === snd_ () &&& (y42 === Std.Pair.pair q16 q17 &&& (q12 === q17) &&& (q11 === s q19 &&& _____fancyEq q10 q17))) )
              ||| ( y45 === Std.Pair.pair q1 q2
                  &&& (q13 === Std.Pair.pair q15 q8)
                  &&& (q15 === pour ())
                  &&& (q8 === snd_ ())
                  &&& (q2 === q11) &&& (q1 === q10) &&& (q5 === !!false) &&& (q9 === q6) &&& (q6 === !!false)
                  &&& (q18 === fst_ () &&& (y42 === Std.Pair.pair q16 q17 &&& (q12 === q16) &&& fancyEqFancyEq q11 q10 q16)) )
              &&& ( y45 === Std.Pair.pair q1 q2
                  &&& (q13 === Std.Pair.pair q15 q8)
                  &&& (q15 === fill ())
                  &&& (q8 === fst_ ())
                  &&& (q2 === q3)
                  &&& (y42 === Std.Pair.pair q7 q20 &&& (q21 === Std.Pair.pair q7 q3))
                  ||| ( y45 === Std.Pair.pair q1 q2
                      &&& (q13 === Std.Pair.pair q15 q8)
                      &&& (q15 === fill ())
                      &&& (q8 === snd_ ())
                      &&& (q1 === q3)
                      &&& (y42 === Std.Pair.pair q22 q7 &&& (q21 === Std.Pair.pair q3 q7)) )
                  ||| ( y45 === Std.Pair.pair q1 q2
                      &&& (q13 === Std.Pair.pair q15 q8)
                      &&& (q15 === empty ())
                      &&& (q8 === fst_ ())
                      &&& (q2 === q4)
                      &&& (q21 === Std.Pair.pair (o ()) q4) )
                  ||| ( y45 === Std.Pair.pair q1 q2
                      &&& (q13 === Std.Pair.pair q15 q8)
                      &&& (q15 === empty ())
                      &&& (q8 === snd_ ())
                      &&& (q1 === q4)
                      &&& (q21 === Std.Pair.pair q4 (o ())) )
                  ||| (fresh (q5 q6 q9 q10 q11 q17 q18) ( y45 === Std.Pair.pair q1 q2
                      &&& (q13 === Std.Pair.pair q15 q8)
                      &&& (q15 === pour ())
                      &&& (q9 === !!true)
                      &&& ( q8 === fst_ ()
                          &&& (q11 === snd_ ())
                          &&& ( y42 === Std.Pair.pair q22 q6
                              &&& ( q17 === snd_ ()
                                  &&& ( q16 === q6
                                      &&& ( q12 === q6
                                          &&& ( add q1 q2 q5
                                              &&& (q5 === s q23 &&& (q6 === o ()) ||| (q5 === s q23 &&& (q6 === s q24) &&& greater q23 q24))
                                              &&& add q1 q2 q18
                                              &&& ( q6 === o () &&& (q10 === q18)
                                                  ||| (q6 === s q23 &&& (q18 === o ()) &&& (q10 === o ()))
                                                  ||| (q6 === s q23 &&& (q18 === s q24) &&& sub q10 q24 q23) )
                                              &&& createState q21 q6 q10 ) ) ) ) )
                          ||| ( q8 === snd_ ()
                              &&& (q11 === fst_ ())
                              &&& ( y42 === Std.Pair.pair q6 q20
                                  &&& ( q17 === fst_ ()
                                      &&& ( q16 === q6
                                          &&& (q12 === q6 &&& (add q1 q2 q5 &&& greater q5 q6 &&& add q1 q2 q18 &&& sub q10 q18 q6 &&& _createState q21 q6 q10))
                                          ) ) ) ) ) ))
                  ||| (fresh (q5 q6 q9 q11) ( y45 === Std.Pair.pair q1 q2
                      &&& (q13 === Std.Pair.pair q15 q8)
                      &&& (q15 === pour ())
                      &&& (q9 === !!false)
                      &&& ( q8 === fst_ ()
                          &&& (q11 === snd_ ())
                          &&& ( y42 === Std.Pair.pair q22 q6
                              &&& ( add q1 q2 q5
                                  &&& (q5 === o () ||| (q5 === s q25 &&& (q6 === s q26) &&& _greater q25 q26))
                                  &&& add q1 q2 q19 &&& __createState q21 q19 ) )
                          ||| ( q8 === snd_ ()
                              &&& (q11 === fst_ ())
                              &&& (y42 === Std.Pair.pair q6 q20 &&& (add q1 q2 q5 &&& _greater q5 q6 &&& add q1 q2 q19 &&& ___createState q21 q19)) ) ) ) ))
              &&& checkAnswer_ y42 y43 q14 q21 ) )) )
  in
  checkAnswer x0 x1 x2



(*
let topLevel x0 x1 x2 =
  let rec checkAnswer y0 y1 y2 =
    fresh (q1 q2) (y0 === Std.List.nil () &&& (y2 === o ()) ||| (y0 === Std.( % ) q1 q2 &&& (checkStep y1 q1 &&& doStepCheckAnswer_ y1 y2 q1 q2)))
  and checkStep y3 y4 =
    fresh (q1 q2 q3)
      ( y4
      === Std.Pair.pair (fill ()) (fst_ ())
      ||| (y4 === Std.Pair.pair (fill ()) (snd_ ()))
      ||| (y4 === Std.Pair.pair (empty ()) (fst_ ()) &&& (y3 === Std.Pair.pair (o ()) q1))
      ||| (y4 === Std.Pair.pair (empty ()) (snd_ ()) &&& (y3 === Std.Pair.pair q2 q3 &&& fancyEq q3)) )
  and doStepCheckAnswer_ y5 y6 y7 y8 = fresh (q1) (doStep y5 y7 q1 &&& checkAnswer_ y5 y6 y8 q1)
  and fancyEq y10 = y10 === o ()
  and doStep y11 y12 y13 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y12
      === Std.Pair.pair (fill ()) (fst_ ())
      &&& (y11 === Std.Pair.pair q1 q2 &&& (y13 === Std.Pair.pair q1 (o ())))
      ||| (y12 === Std.Pair.pair (fill ()) (snd_ ()) &&& (y11 === Std.Pair.pair q3 q1 &&& (y13 === Std.Pair.pair (o ()) q1)))
      ||| (y12 === Std.Pair.pair (empty ()) (fst_ ()) &&& (y13 === Std.Pair.pair (o ()) (o ())))
      ||| (y12 === Std.Pair.pair (empty ()) (snd_ ()) &&& (y13 === Std.Pair.pair (o ()) (o ())))
      ||| ( y12
          === Std.Pair.pair (pour ()) q4
          &&& ( q4 === fst_ ()
              &&& (y11 === Std.Pair.pair q3 q5 &&& (y13 === Std.Pair.pair (o ()) (o ())))
              ||| (q4 === snd_ () &&& (y11 === Std.Pair.pair q5 q2 &&& (y13 === Std.Pair.pair q6 (o ()) &&& addGreaterAdd q6 q5))) ) ) )
  and checkAnswer_ y14 y15 y16 y17 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11)
      ( y16 === Std.List.nil ()
      &&& ( y17 === Std.Pair.pair q1 q2
          &&& (_fancyEq y15 q1 &&& __fancyEq y15 q2)
          ||| ( y17 === Std.Pair.pair q1 q2
              &&& ( q1 === o ()
                  &&& (y15 === s q3)
                  &&& (q2 === s q4 &&& ___fancyEq q4 q3)
                  ||| (q1 === s q5 &&& (y15 === o ()) &&& (q2 === o ()))
                  ||| (q1 === s (o ()) &&& (y15 === s (s q6)) &&& (q2 === s q7 &&& ____fancyEq q7 q6))
                  ||| (q1 === s (s q4) &&& (y15 === s (o ())) &&& (q2 === s q7 &&& _____fancyEq q7))
                  ||| (q1 === s (s q4) &&& (y15 === s (s q8)) &&& (______fancyEq q4 q8 &&& _______fancyEq q2 q8)) ) ) )
      ||| (y16 === Std.( % ) q9 q10 &&& (_checkStep y14 y17 q9 &&& _doStep y14 y17 q9 q11 &&& checkAnswer_ y14 y15 q10 q11)) )
  and addGreaterAdd y19 y20 = y19 === o ()
  and _fancyEq y21 y22 =
    fresh (q1 q2 q3 q4)
      ( y22 === o ()
      &&& (y21 === o ())
      ||| (y22 === s q1 &&& (y21 === s q2) &&& (q1 === o () &&& (q2 === o ()) ||| (q1 === s q3 &&& (q2 === s q4) &&& ___fancyEq q3 q4))) )
  and __fancyEq y23 y24 =
    fresh (q1 q2 q3)
      ( y24 === o ()
      &&& (y23 === o ())
      ||| (y24 === o () &&& (y23 === s q1))
      ||| (y24 === s q2 &&& (y23 === o ()))
      ||| (y24 === s q2 &&& (y23 === s q3) &&& __fancyEq q3 q2) )
  and ___fancyEq y26 y27 = fresh (q1 q2) (y26 === o () &&& (y27 === o ()) ||| (y26 === s q1 &&& (y27 === s q2) &&& ___fancyEq q1 q2))
  and ____fancyEq y28 y29 = fresh (q1) (y28 === s q1 &&& ___fancyEq q1 y29)
  and _____fancyEq y30 = y30 === o ()
  and ______fancyEq y31 y32 =
    fresh (q1 q2 q3) (y31 === o () &&& (y32 === s q1) ||| (y31 === s q2 &&& (y32 === o ())) ||| (y31 === s q2 &&& (y32 === s q3) &&& ______fancyEq q2 q3))
  and _______fancyEq y33 y34 = fresh (q1) (y33 === s q1 &&& ____fancyEq q1 y34)
  and _checkStep y35 y36 y37 =
    fresh (q1 q2 q3 q4 q5 q6 q7)
      ( y36 === Std.Pair.pair q1 q2
      &&& (y37 === Std.Pair.pair (fill ()) (fst_ ()))
      &&& _____fancyEq q1
      ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair (fill ()) (snd_ ())) &&& _____fancyEq q2)
      ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair (empty ()) (fst_ ())) &&& (y35 === Std.Pair.pair q3 q4 &&& ___fancyEq q1 q3))
      ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair (empty ()) (snd_ ())) &&& (y35 === Std.Pair.pair q5 q3 &&& ___fancyEq q2 q3))
      ||| (y36 === Std.Pair.pair (s q6) q2 &&& (y37 === Std.Pair.pair (pour ()) (fst_ ())) &&& (y35 === Std.Pair.pair q5 q7 &&& ______fancyEq q2 q7))
      ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair (pour ()) (snd_ ())) &&& (y35 === Std.Pair.pair q7 q4 &&& fancyEqFancyEq q2 q1 q7)) )
  and _doStep y38 y39 y40 y41 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10)
      ( y39 === Std.Pair.pair q1 q2
      &&& (y40 === Std.Pair.pair (fill ()) (fst_ ()))
      &&& (y38 === Std.Pair.pair q3 q4 &&& (y41 === Std.Pair.pair q3 q2))
      ||| (y39 === Std.Pair.pair q1 q2 &&& (y40 === Std.Pair.pair (fill ()) (snd_ ())) &&& (y38 === Std.Pair.pair q5 q3 &&& (y41 === Std.Pair.pair q1 q3)))
      ||| (y39 === Std.Pair.pair q1 q2 &&& (y40 === Std.Pair.pair (empty ()) (fst_ ())) &&& (y41 === Std.Pair.pair (o ()) q2))
      ||| (y39 === Std.Pair.pair q1 q2 &&& (y40 === Std.Pair.pair (empty ()) (snd_ ())) &&& (y41 === Std.Pair.pair q1 (o ())))
      ||| ( y39 === Std.Pair.pair q1 q2
          &&& (y40 === Std.Pair.pair (pour ()) q6)
          &&& ( q6 === fst_ ()
              &&& (y38 === Std.Pair.pair q5 q7 &&& (addGreaterAddSub q1 q2 q8 q7 &&& createState y41 q7 q8))
              ||| (q6 === snd_ () &&& (y38 === Std.Pair.pair q7 q4 &&& (_addGreaterAddSub q1 q2 q8 q7 &&& _createState y41 q7 q8))) ) )
      ||| ( y39 === Std.Pair.pair q1 q2
          &&& (y40 === Std.Pair.pair (pour ()) q6)
          &&& ( q6 === fst_ ()
              &&& (y38 === Std.Pair.pair q5 q9 &&& (_addGreaterAdd q1 q2 q10 q9 &&& __createState y41 q10))
              ||| (q6 === snd_ () &&& (y38 === Std.Pair.pair q9 q4 &&& (__addGreaterAdd q1 q2 q10 q9 &&& ___createState y41 q10))) ) ) )
  and fancyEqFancyEq y42 y43 y44 = fresh (q1) (y42 === s q1 &&& ______fancyEq y43 y44)
  and addGreaterAddSub y45 y46 y48 y50 = fresh (q1 q2) (add y45 y46 q1 &&& greater q1 y50 &&& add y45 y46 q2 &&& sub y48 q2 y50)
  and add y51 y52 y53 = fresh (q1 q2) (y51 === o () &&& (y52 === y53) ||| (y51 === s q1 &&& (y53 === s q2) &&& add q1 y52 q2))
  and greater y54 y55 = fresh (q1 q2) (y54 === s q1 &&& (y55 === o ()) ||| (y54 === s q1 &&& (y55 === s q2) &&& greater q1 q2))
  and sub y56 y57 y58 =
    fresh (q1 q2)
      (y58 === o () &&& (y56 === y57) ||| (y58 === s q1 &&& (y57 === o ()) &&& (y56 === o ())) ||| (y58 === s q1 &&& (y57 === s q2) &&& sub y56 q2 q1))
  and createState y59 y60 y61 = y59 === Std.Pair.pair y61 y60
  and _addGreaterAddSub y62 y63 y65 y67 = fresh (q1 q2) (add y62 y63 q1 &&& greater q1 y67 &&& add y62 y63 q2 &&& sub y65 q2 y67)
  and _createState y68 y69 y70 = y68 === Std.Pair.pair y69 y70
  and _addGreaterAdd y71 y72 y74 y75 = fresh (q1) (add y71 y72 q1 &&& _greater q1 y75 &&& add y71 y72 y74)
  and _greater y76 y77 = fresh (q1 q2) (y76 === o () ||| (y76 === s q1 &&& (y77 === s q2) &&& _greater q1 q2))
  and __createState y78 y79 = y78 === Std.Pair.pair (o ()) y79
  and __addGreaterAdd y80 y81 y83 y84 = fresh (q1) (add y80 y81 q1 &&& _greater q1 y84 &&& add y80 y81 y83)
  and ___createState y85 y86 = y85 === Std.Pair.pair y86 (o ()) in
  checkAnswer x0 x1 x2 *) *) *)



  let topLevel x0 x1 x2 =
    let rec checkAnswer y0 y1 y2 =
      fresh (q1 q2) (y0 === Std.List.nil () &&& (y2 === o ()) ||| (y0 === Std.( % ) q1 q2 &&& (checkStep y1 q1 &&& doStepCheckAnswer_ y1 y2 q1 q2)))
    and checkStep y3 y4 =
      fresh (q1 q2 q3)
        ( y4
        === Std.Pair.pair (fill ()) (fst_ ())
        ||| (y4 === Std.Pair.pair (fill ()) (snd_ ()))
        ||| (y4 === Std.Pair.pair (empty ()) (fst_ ()) &&& (y3 === Std.Pair.pair (o ()) q1))
        ||| (y4 === Std.Pair.pair (empty ()) (snd_ ()) &&& (y3 === Std.Pair.pair q2 q3 &&& fancyEq q3)) )
    and doStepCheckAnswer_ y5 y6 y7 y8 = fresh (q1) (doStep y5 y7 q1 &&& checkAnswer_ y5 y6 y8 q1)
    and fancyEq y10 = y10 === o ()
    and doStep y11 y12 y13 =
      fresh (q1 q2 q3 q4 q5 q6)
        ( y12
        === Std.Pair.pair (fill ()) (fst_ ())
        &&& (y11 === Std.Pair.pair q1 q2 &&& (y13 === Std.Pair.pair q1 (o ())))
        ||| (y12 === Std.Pair.pair (fill ()) (snd_ ()) &&& (y11 === Std.Pair.pair q3 q1 &&& (y13 === Std.Pair.pair (o ()) q1)))
        ||| (y12 === Std.Pair.pair (empty ()) (fst_ ()) &&& (y13 === Std.Pair.pair (o ()) (o ())))
        ||| (y12 === Std.Pair.pair (empty ()) (snd_ ()) &&& (y13 === Std.Pair.pair (o ()) (o ())))
        ||| ( y12
            === Std.Pair.pair (pour ()) q4
            &&& ( q4 === fst_ ()
                &&& (y11 === Std.Pair.pair q3 q5 &&& (y13 === Std.Pair.pair (o ()) (o ())))
                ||| (q4 === snd_ () &&& (y11 === Std.Pair.pair q5 q2 &&& (y13 === Std.Pair.pair q6 (o ()) &&& addGreaterAdd q6 q5))) ) ) )
    and checkAnswer_ y14 y15 y16 y17 =
      fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11)
        ( y16 === Std.List.nil ()
        &&& ( y17 === Std.Pair.pair q1 q2
            &&& (_fancyEq y15 q1 &&& __fancyEq y15 q2)
            ||| ( y17 === Std.Pair.pair q1 q2
                &&& ( q1 === o ()
                    &&& (y15 === s q3)
                    &&& (q2 === s q4 &&& ___fancyEq q4 q3)
                    ||| (q1 === s q5 &&& (y15 === o ()) &&& (q2 === o ()))
                    ||| (q1 === s (o ()) &&& (y15 === s (s q6)) &&& (q2 === s q7 &&& ____fancyEq q7 q6))
                    ||| (q1 === s (s q4) &&& (y15 === s (o ())) &&& (q2 === s q7 &&& _____fancyEq q7))
                    ||| (q1 === s (s q4) &&& (y15 === s (s q8)) &&& (______fancyEq q4 q8 &&& _______fancyEq q2 q8)) ) ) )
        ||| (y16 === Std.( % ) q9 q10 &&& (_checkStep y14 y17 q9 &&& _doStep y14 y17 q9 q11 &&& checkAnswer_ y14 y15 q10 q11)) )
    and addGreaterAdd y19 y20 = y19 === o ()
    and _fancyEq y21 y22 =
      fresh (q1 q2 q3 q4)
        ( y22 === o ()
        &&& (y21 === o ())
        ||| (y22 === s q1 &&& (y21 === s q2) &&& (q1 === o () &&& (q2 === o ()) ||| (q1 === s q3 &&& (q2 === s q4) &&& ___fancyEq q3 q4))) )
    and __fancyEq y23 y24 =
      fresh (q1 q2 q3)
        ( y24 === o ()
        &&& (y23 === o ())
        ||| (y24 === o () &&& (y23 === s q1))
        ||| (y24 === s q2 &&& (y23 === o ()))
        ||| (y24 === s q2 &&& (y23 === s q3) &&& __fancyEq q3 q2) )
    and ___fancyEq y26 y27 = fresh (q1 q2) (y26 === o () &&& (y27 === o ()) ||| (y26 === s q1 &&& (y27 === s q2) &&& ___fancyEq q1 q2))
    and ____fancyEq y28 y29 = fresh (q1) (y28 === s q1 &&& ___fancyEq q1 y29)
    and _____fancyEq y30 = y30 === o ()
    and ______fancyEq y31 y32 =
      fresh (q1 q2 q3) (y31 === o () &&& (y32 === s q1) ||| (y31 === s q2 &&& (y32 === o ())) ||| (y31 === s q2 &&& (y32 === s q3) &&& ______fancyEq q2 q3))
    and _______fancyEq y33 y34 = fresh (q1) (y33 === s q1 &&& ____fancyEq q1 y34)
    and _checkStep y35 y36 y37 =
      fresh (q1 q2 q3 q4 q5 q6 q7)
        ( y36 === Std.Pair.pair q1 q2
        &&& (y37 === Std.Pair.pair (fill ()) (fst_ ()))
        &&& _____fancyEq q1
        ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair (fill ()) (snd_ ())) &&& _____fancyEq q2)
        ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair (empty ()) (fst_ ())) &&& (y35 === Std.Pair.pair q3 q4 &&& ___fancyEq q1 q3))
        ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair (empty ()) (snd_ ())) &&& (y35 === Std.Pair.pair q5 q3 &&& ___fancyEq q2 q3))
        ||| (y36 === Std.Pair.pair (s q6) q2 &&& (y37 === Std.Pair.pair (pour ()) (fst_ ())) &&& (y35 === Std.Pair.pair q5 q7 &&& ______fancyEq q2 q7))
        ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair (pour ()) (snd_ ())) &&& (y35 === Std.Pair.pair q7 q4 &&& fancyEqFancyEq q2 q1 q7)) )
    and _doStep y38 y39 y40 y41 =
      fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10)
        ( y39 === Std.Pair.pair q1 q2
        &&& (y40 === Std.Pair.pair (fill ()) (fst_ ()))
        &&& (y38 === Std.Pair.pair q3 q4 &&& (y41 === Std.Pair.pair q3 q2))
        ||| (y39 === Std.Pair.pair q1 q2 &&& (y40 === Std.Pair.pair (fill ()) (snd_ ())) &&& (y38 === Std.Pair.pair q5 q3 &&& (y41 === Std.Pair.pair q1 q3)))
        ||| (y39 === Std.Pair.pair q1 q2 &&& (y40 === Std.Pair.pair (empty ()) (fst_ ())) &&& (y41 === Std.Pair.pair (o ()) q2))
        ||| (y39 === Std.Pair.pair q1 q2 &&& (y40 === Std.Pair.pair (empty ()) (snd_ ())) &&& (y41 === Std.Pair.pair q1 (o ())))
        ||| ( y39 === Std.Pair.pair q1 q2
            &&& (y40 === Std.Pair.pair (pour ()) q6)
            &&& ( q6 === fst_ ()
                &&& (y38 === Std.Pair.pair q5 q7 &&& (addGreaterAddSub q1 q2 q8 q7 &&& createState y41 q7 q8))
                ||| (q6 === snd_ () &&& (y38 === Std.Pair.pair q7 q4 &&& (_addGreaterAddSub q1 q2 q8 q7 &&& _createState y41 q7 q8))) ) )
        ||| ( y39 === Std.Pair.pair q1 q2
            &&& (y40 === Std.Pair.pair (pour ()) q6)
            &&& ( q6 === fst_ ()
                &&& (y38 === Std.Pair.pair q5 q9 &&& (_addGreaterAdd q1 q2 q10 q9 &&& __createState y41 q10))
                ||| (q6 === snd_ () &&& (y38 === Std.Pair.pair q9 q4 &&& (__addGreaterAdd q1 q2 q10 q9 &&& ___createState y41 q10))) ) ) )
    and fancyEqFancyEq y42 y43 y44 = fresh (q1) (y42 === s q1 &&& ______fancyEq y43 y44)
    and addGreaterAddSub y45 y46 y48 y50 = fresh (q1 q2) (add y45 y46 q1 &&& greater q1 y50 &&& add y45 y46 q2 &&& sub y48 q2 y50)
    and add y51 y52 y53 = fresh (q1 q2) (y51 === o () &&& (y52 === y53) ||| (y51 === s q1 &&& (y53 === s q2) &&& add q1 y52 q2))
    and greater y54 y55 = fresh (q1 q2) (y54 === s q1 &&& (y55 === o ()) ||| (y54 === s q1 &&& (y55 === s q2) &&& greater q1 q2))
    and sub y56 y57 y58 =
      fresh (q1 q2)
        (y58 === o () &&& (y56 === y57) ||| (y58 === s q1 &&& (y57 === o ()) &&& (y56 === o ())) ||| (y58 === s q1 &&& (y57 === s q2) &&& sub y56 q2 q1))
    and createState y59 y60 y61 = y59 === Std.Pair.pair y61 y60
    and _addGreaterAddSub y62 y63 y65 y67 = fresh (q1 q2) (add y62 y63 q1 &&& greater q1 y67 &&& add y62 y63 q2 &&& sub y65 q2 y67)
    and _createState y68 y69 y70 = y68 === Std.Pair.pair y69 y70
    and _addGreaterAdd y71 y72 y74 y75 = fresh (q1) (add y71 y72 q1 &&& _greater q1 y75 &&& add y71 y72 y74)
    and _greater y76 y77 = fresh (q1 q2) (y76 === o () ||| (y76 === s q1 &&& (y77 === s q2) &&& _greater q1 q2))
    and __createState y78 y79 = y78 === Std.Pair.pair (o ()) y79
    and __addGreaterAdd y80 y81 y83 y84 = fresh (q1) (add y80 y81 q1 &&& _greater q1 y84 &&& add y80 y81 y83)
    and ___createState y85 y86 = y85 === Std.Pair.pair y86 (o ()) in
    checkAnswer x0 x1 x2 *) *)