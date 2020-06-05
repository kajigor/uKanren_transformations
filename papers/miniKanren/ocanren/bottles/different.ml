open GT
open OCanren
open OCanren.Std

open Helper

open GT
open OCanren
open OCanren.Std

let topLevel x0 x1 x2 =
  let rec checkAnswer y0 y1 y2 =
    fresh (q1 q2) (y0 === Std.List.nil () &&& (y2 === o ()) ||| (y0 === Std.( % ) q1 q2 &&& (checkStep y1 q1 &&& doStepCheckAnswer_ y1 y2 q1 q2)))
  and checkStep y3 y4 =
    fresh (q1 q2 q3)
      (    y4 === Pair.pair (fill ()) (fst_ ())
      ||| (y4 === Pair.pair (fill ()) (snd_ ()))
      ||| (y4 === Pair.pair (empty ()) (fst_ ()) &&& (y3 === Pair.pair (o ()) q1))
      ||| (y4 === Pair.pair (empty ()) (snd_ ()) &&& (y3 === Pair.pair q2 q3 &&& fancyEq q3)) )
  and doStepCheckAnswer_ y5 y6 y7 y8 = fresh (q1) (doStep y5 y7 q1 &&& checkAnswer_ y5 y6 y8 q1)
  and fancyEq y10 = y10 === o ()
  and doStep y11 y12 y13 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8)
      ( y12 === Pair.pair (fill ()) (fst_ ())
      &&& (y11 === Pair.pair q1 q2 &&& (y13 === Pair.pair q1 (o ())))
      ||| (y12 === Pair.pair (fill ()) (snd_ ()) &&& (y11 === Pair.pair q3 q1 &&& (y13 === Pair.pair (o ()) q1)))
      ||| (y12 === Pair.pair (empty ()) (fst_ ()) &&& (y13 === Pair.pair (o ()) (o ())))
      ||| (y12 === Pair.pair (empty ()) (snd_ ()) &&& (y13 === Pair.pair (o ()) (o ())))
      ||| ( y12 === Pair.pair (pour ()) q4
          &&& ( q4 === fst_ ()
              &&& (y11 === Pair.pair q5 q6 &&& (y13 === Pair.pair (o ()) (o ())))
              ||| (q4 === snd_ () &&& (y11 === Pair.pair q6 q7 &&& (y13 === Pair.pair q8 (o ())) &&& addGreaterAdd q8 q6)) ) ) )
  and checkAnswer_ y14 y15 y16 y17 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9)
      ( y16 === Std.List.nil ()
      &&& ( y17 === Pair.pair q1 q2
          &&& (_fancyEq y15 q1 &&& __fancyEq y15 q2)
          ||| ( y17 === Pair.pair q1 q2
              &&& ( q1 === o ()
                  &&& (y15 === s q3)
                  &&& (q2 === s q4 &&& _fancyEq q3 q4)
                  ||| (q1 === s q5 &&& (y15 === o ()) &&& (q2 === o ()))
                  ||| (q1 === s q5 &&& (y15 === s q6) &&& (___fancyEq q5 q6 &&& ____fancyEq q2 q6)) ) ) )
      ||| (y16 === Std.( % ) q7 q8 &&& (_checkStep y14 y17 q7 &&& _doStep y14 y17 q7 q9 &&& checkAnswer_ y14 y15 q8 q9)) )
  and addGreaterAdd y19 y20 = y19 === o ()
  and _fancyEq y21 y22 = fresh (q1 q2) (y22 === o () &&& (y21 === o ()) ||| (y22 === s q1 &&& (y21 === s q2) &&& _fancyEq q2 q1))
  and __fancyEq y23 y24 =
    fresh (q1 q2 q3)
      ( y24 === o ()
      &&& (y23 === o ())
      ||| (y24 === o () &&& (y23 === s q1))
      ||| (y24 === s q2 &&& (y23 === o ()))
      ||| (y24 === s q2 &&& (y23 === s q3) &&& __fancyEq q3 q2) )
  and ___fancyEq y26 y27 =
    fresh (q1 q2 q3) (y26 === o () &&& (y27 === s q1) ||| (y26 === s q2 &&& (y27 === o ())) ||| (y26 === s q2 &&& (y27 === s q3) &&& ___fancyEq q2 q3))
  and ____fancyEq y28 y29 = fresh (q1) (y28 === s q1 &&& _fancyEq y29 q1)
  and _checkStep y30 y31 y32 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9)
      ( y31 === Pair.pair q1 q2
      &&& (y32 === Pair.pair (fill ()) (fst_ ()))
      &&& _____fancyEq q1
      ||| (y31 === Pair.pair q1 q2 &&& (y32 === Pair.pair (fill ()) (snd_ ())) &&& _____fancyEq q2)
      ||| (y31 === Pair.pair q1 q2 &&& (y32 === Pair.pair (empty ()) (fst_ ())) &&& (y30 === Pair.pair q3 q4 &&& _fancyEq q3 q1))
      ||| (y31 === Pair.pair q1 q2 &&& (y32 === Pair.pair (empty ()) (snd_ ())) &&& (y30 === Pair.pair q5 q3 &&& _fancyEq q3 q2))
      ||| (y31 === Pair.pair (s q6) q2 &&& (y32 === Pair.pair (pour ()) (fst_ ())) &&& (y30 === Pair.pair q7 q8 &&& ___fancyEq q2 q8))
      ||| (y31 === Pair.pair q1 q2 &&& (y32 === Pair.pair (pour ()) (snd_ ())) &&& (y30 === Pair.pair q8 q9 &&& fancyEqFancyEq q2 q1 q8)) )
  and _doStep y33 y34 y35 y36 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15)
      ( y34 === Pair.pair q1 q2
      &&& (y35 === Pair.pair (fill ()) (fst_ ()))
      &&& (y33 === Pair.pair q3 q4 &&& (y36 === Pair.pair q3 q2))
      ||| (y34 === Pair.pair q1 q2 &&& (y35 === Pair.pair (fill ()) (snd_ ())) &&& (y33 === Pair.pair q5 q3 &&& (y36 === Pair.pair q1 q3)))
      ||| (y34 === Pair.pair q1 q2 &&& (y35 === Pair.pair (empty ()) (fst_ ())) &&& (y36 === Pair.pair (o ()) q2))
      ||| (y34 === Pair.pair q1 q2 &&& (y35 === Pair.pair (empty ()) (snd_ ())) &&& (y36 === Pair.pair q1 (o ())))
      ||| ( y34 === Pair.pair q1 q2
          &&& (y35 === Pair.pair (pour ()) q6)
          &&& ( q6 === fst_ ()
              &&& (y33 === Pair.pair q7 q8 &&& (y36 === Pair.pair q9 q8) &&& (add q1 q2 q10 &&& greater q10 q8 &&& addSub q1 q2 q9 q8))
              ||| (q6 === snd_ () &&& (y33 === Pair.pair q8 q11 &&& (y36 === Pair.pair q8 q9) &&& addGreaterAddSub q1 q2 q9 q8)) ) )
      ||| ( y34 === Pair.pair q1 q2
          &&& (y35 === Pair.pair (pour ()) q6)
          &&& ( q6 === fst_ ()
              &&& (y33 === Pair.pair q12 q13 &&& (y36 === Pair.pair (o ()) q14) &&& (add q1 q2 q10 &&& _greater q10 q13 &&& add q1 q2 q14))
              ||| (q6 === snd_ () &&& (y33 === Pair.pair q13 q15 &&& (y36 === Pair.pair q14 (o ())) &&& _addGreaterAdd q1 q2 q14 q13)) ) ) )
  and _____fancyEq y37 = y37 === o ()
  and fancyEqFancyEq y38 y39 y40 = fresh (q1) (y38 === s q1 &&& ___fancyEq y39 y40)
  and add y41 y42 y43 = fresh (q1 q2) (y41 === o () &&& (y42 === y43) ||| (y41 === s q1 &&& (y43 === s q2) &&& add q1 y42 q2))
  and greater y44 y45 = fresh (q1 q2) (y44 === s q1 &&& (y45 === o ()) ||| (y44 === s q1 &&& (y45 === s q2) &&& greater q1 q2))
  and addSub y46 y47 y48 y50 = fresh (q1) (add y46 y47 q1 &&& sub y48 q1 y50)
  and sub y51 y52 y53 =
    fresh (q1 q2)
      (y53 === o () &&& (y51 === y52) ||| (y53 === s q1 &&& (y52 === o ()) &&& (y51 === o ())) ||| (y53 === s q1 &&& (y52 === s q2) &&& sub y51 q2 q1))
  and addGreaterAddSub y54 y55 y57 y59 = fresh (q1) (add y54 y55 q1 &&& greater q1 y59 &&& addSub y54 y55 y57 y59)
  and _greater y60 y61 = fresh (q1 q2) (y60 === o () ||| (y60 === s q1 &&& (y61 === s q2) &&& _greater q1 q2))
  and _addGreaterAdd y62 y63 y65 y66 = fresh (q1) (add y62 y63 q1 &&& _greater q1 y66 &&& add y62 y63 y65) in
  checkAnswer x0 x1 x2

(*
let topLevel x0 x1 x2 =
  let rec checkAnswer y0 y1 y2 =
    fresh (q1 q2) (y0 === Std.List.nil () &&& (y2 === o ()) ||| (y0 === Std.( % ) q1 q2 &&& (checkStep y1 q1 &&& doStepCheckAnswer_ y1 y2 q1 q2)))
  and checkStep y3 y4 =
    fresh (q1 q2 q3)
      ( y4
      === Pair.pair (fill ()) (fst_ ())
      ||| (y4 === Pair.pair (fill ()) (snd_ ()))
      ||| (y4 === Pair.pair (empty ()) (fst_ ()) &&& (y3 === Pair.pair (o ()) q1))
      ||| (y4 === Pair.pair (empty ()) (snd_ ()) &&& (y3 === Pair.pair q2 q3 &&& fancyEq q3)) )
  and doStepCheckAnswer_ y5 y6 y7 y8 = fresh (q1) (doStep y5 y7 q1 &&& checkAnswer_ y5 y6 y8 q1)
  and fancyEq y10 = y10 === o ()
  and doStep y11 y12 y13 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8)
      ( y12
      === Pair.pair (fill ()) (fst_ ())
      &&& (y11 === Pair.pair q1 q2 &&& (y13 === Pair.pair q1 (o ())))
      ||| (y12 === Pair.pair (fill ()) (snd_ ()) &&& (y11 === Pair.pair q3 q1 &&& (y13 === Pair.pair (o ()) q1)))
      ||| (y12 === Pair.pair (empty ()) (fst_ ()) &&& (y13 === Pair.pair (o ()) (o ())))
      ||| (y12 === Pair.pair (empty ()) (snd_ ()) &&& (y13 === Pair.pair (o ()) (o ())))
      ||| ( y12
          === Pair.pair (pour ()) q4
          &&& ( q4 === fst_ ()
              &&& (y11 === Pair.pair q5 q6 &&& (y13 === Pair.pair (o ()) (o ())))
              ||| (q4 === snd_ () &&& (y11 === Pair.pair q6 q7 &&& (y13 === Pair.pair q8 (o ())) &&& addGreaterAdd q8 q6)) ) ) )
  and checkAnswer_ y14 y15 y16 y17 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13)
      ( y16 === Std.List.nil ()
      &&& ( y17 === Pair.pair q1 q2
          &&& (_fancyEq y15 q1 &&& __fancyEq y15 q2)
          ||| ( y17 === Pair.pair q1 q2
              &&& ( q1 === o ()
                  &&& (y15 === s q3)
                  &&& (q2 === s q4 &&& ___fancyEq q4 q3)
                  ||| (q1 === s q5 &&& (y15 === o ()) &&& (q2 === o ()))
                  ||| (q1 === s (o ()) &&& (y15 === s (s q6)) &&& (q2 === s q7 &&& ____fancyEq q7 q6))
                  ||| (q1 === s (s q8) &&& (y15 === s (o ())) &&& (q2 === s q9 &&& _____fancyEq q9))
                  ||| (q1 === s (s q8) &&& (y15 === s (s q10)) &&& (______fancyEq q8 q10 &&& _______fancyEq q2 q10)) ) ) )
      ||| (y16 === Std.( % ) q11 q12 &&& (_checkStep y14 y17 q11 &&& _doStep y14 y17 q11 q13 &&& checkAnswer_ y14 y15 q12 q13)) )
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
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9)
      ( y36 === Pair.pair q1 q2
      &&& (y37 === Pair.pair (fill ()) (fst_ ()))
      &&& _____fancyEq q1
      ||| (y36 === Pair.pair q1 q2 &&& (y37 === Pair.pair (fill ()) (snd_ ())) &&& _____fancyEq q2)
      ||| (y36 === Pair.pair q1 q2 &&& (y37 === Pair.pair (empty ()) (fst_ ())) &&& (y35 === Pair.pair q3 q4 &&& ___fancyEq q1 q3))
      ||| (y36 === Pair.pair q1 q2 &&& (y37 === Pair.pair (empty ()) (snd_ ())) &&& (y35 === Pair.pair q5 q3 &&& ___fancyEq q2 q3))
      ||| (y36 === Pair.pair (s q6) q2 &&& (y37 === Pair.pair (pour ()) (fst_ ())) &&& (y35 === Pair.pair q7 q8 &&& ______fancyEq q2 q8))
      ||| (y36 === Pair.pair q1 q2 &&& (y37 === Pair.pair (pour ()) (snd_ ())) &&& (y35 === Pair.pair q8 q9 &&& fancyEqFancyEq q2 q1 q8)) )
  and _doStep y38 y39 y40 y41 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14)
      ( y39 === Pair.pair q1 q2
      &&& (y40 === Pair.pair (fill ()) (fst_ ()))
      &&& (y38 === Pair.pair q3 q4 &&& (y41 === Pair.pair q3 q2))
      ||| (y39 === Pair.pair q1 q2 &&& (y40 === Pair.pair (fill ()) (snd_ ())) &&& (y38 === Pair.pair q5 q3 &&& (y41 === Pair.pair q1 q3)))
      ||| (y39 === Pair.pair q1 q2 &&& (y40 === Pair.pair (empty ()) (fst_ ())) &&& (y41 === Pair.pair (o ()) q2))
      ||| (y39 === Pair.pair q1 q2 &&& (y40 === Pair.pair (empty ()) (snd_ ())) &&& (y41 === Pair.pair q1 (o ())))
      ||| ( y39 === Pair.pair q1 q2
          &&& (y40 === Pair.pair (pour ()) q6)
          &&& ( q6 === fst_ ()
              &&& (y38 === Pair.pair q7 q8 &&& (addGreaterAddSub q1 q2 q9 q8 &&& createState y41 q8 q9))
              ||| (q6 === snd_ () &&& (y38 === Pair.pair q8 q10 &&& (_addGreaterAddSub q1 q2 q9 q8 &&& _createState y41 q8 q9))) ) )
      ||| ( y39 === Pair.pair q1 q2
          &&& (y40 === Pair.pair (pour ()) q6)
          &&& ( q6 === fst_ ()
              &&& (y38 === Pair.pair q11 q12 &&& (_addGreaterAdd q1 q2 q13 q12 &&& __createState y41 q13))
              ||| (q6 === snd_ () &&& (y38 === Pair.pair q12 q14 &&& (__addGreaterAdd q1 q2 q13 q12 &&& ___createState y41 q13))) ) ) )
  and fancyEqFancyEq y42 y43 y44 = fresh (q1) (y42 === s q1 &&& ______fancyEq y43 y44)
  and addGreaterAddSub y45 y46 y48 y50 = fresh (q1 q2) (add y45 y46 q1 &&& greater q1 y50 &&& add y45 y46 q2 &&& sub y48 q2 y50)
  and add y51 y52 y53 = fresh (q1 q2) (y51 === o () &&& (y52 === y53) ||| (y51 === s q1 &&& (y53 === s q2) &&& add q1 y52 q2))
  and greater y54 y55 = fresh (q1 q2) (y54 === s q1 &&& (y55 === o ()) ||| (y54 === s q1 &&& (y55 === s q2) &&& greater q1 q2))
  and sub y56 y57 y58 =
    fresh (q1 q2)
      (y58 === o () &&& (y56 === y57) ||| (y58 === s q1 &&& (y57 === o ()) &&& (y56 === o ())) ||| (y58 === s q1 &&& (y57 === s q2) &&& sub y56 q2 q1))
  and createState y59 y60 y61 = y59 === Pair.pair y61 y60
  and _addGreaterAddSub y62 y63 y65 y67 = fresh (q1 q2) (add y62 y63 q1 &&& greater q1 y67 &&& add y62 y63 q2 &&& sub y65 q2 y67)
  and _createState y68 y69 y70 = y68 === Pair.pair y69 y70
  and _addGreaterAdd y71 y72 y74 y75 = fresh (q1) (add y71 y72 q1 &&& _greater q1 y75 &&& add y71 y72 y74)
  and _greater y76 y77 = fresh (q1 q2) (y76 === o () ||| (y76 === s q1 &&& (y77 === s q2) &&& _greater q1 q2))
  and __createState y78 y79 = y78 === Pair.pair (o ()) y79
  and __addGreaterAdd y80 y81 y83 y84 = fresh (q1) (add y80 y81 q1 &&& _greater q1 y84 &&& add y80 y81 y83)
  and ___createState y85 y86 = y85 === Pair.pair y86 (o ()) in
  checkAnswer x0 x1 x2 *)