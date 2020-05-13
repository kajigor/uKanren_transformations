open GT
open OCanren
open OCanren.Std

let fst_ () = !! Fst
let snd_ () = !! Snd
type stepType =
  | Fill
  | Empty
  | Pour
let fill () = !! Fill
let empty () = !! Empty
let pour () = !! Pour
type 'a0 gnat =
  | O
  | S of 'a0
module For_gnat = (Fmap)(struct let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec o () = inj (For_gnat.distrib O)
and s x__0 = inj (For_gnat.distrib (S x__0))

let topLevel x0 x1 x2 =
  let rec checkAnswer y0 y1 y2 =
    fresh (q1 q2) (y0 === Std.List.nil () &&& (y2 === o ()) ||| (y0 === Std.( % ) q1 q2 &&& (checkStep y1 q1 &&& doStepCheckAnswer_ y1 y2 q1 q2)))
  and checkStep y3 y4 =
    fresh (q1 q2 q3)
      ( y4 === Std.Pair.pair fill fst
      ||| (y4 === Std.Pair.pair fill snd)
      ||| (y4 === Std.Pair.pair empty fst &&& (y3 === Std.Pair.pair o () q1))
      ||| (y4 === Std.Pair.pair empty snd &&& (y3 === Std.Pair.pair q2 q3 &&& fancyEq q3)) )
  and doStepCheckAnswer_ y5 y6 y7 y8 = fresh (q1) (doStep y5 y7 q1 &&& checkAnswer_ y5 y6 y8 q1)
  and fancyEq y10 = y10 === o ()
  and doStep y11 y12 y13 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( y12 === Std.Pair.pair fill fst
      &&& (y11 === Std.Pair.pair q1 q2 &&& (y13 === Std.Pair.pair q1 o ()))
      ||| (y12 === Std.Pair.pair fill snd &&& (y11 === Std.Pair.pair q3 q1 &&& (y13 === Std.Pair.pair o () q1)))
      ||| (y12 === Std.Pair.pair empty fst &&& (y13 === Std.Pair.pair o () o ()))
      ||| (y12 === Std.Pair.pair empty snd &&& (y13 === Std.Pair.pair o () o ()))
      ||| ( y12 === Std.Pair.pair pour q4
          &&& ( q4 === fst
              &&& (y11 === Std.Pair.pair q3 q5 &&& (y13 === Std.Pair.pair o () o ()))
              ||| (q4 === snd &&& (y11 === Std.Pair.pair q5 q2 &&& (y13 === Std.Pair.pair q6 o () &&& addGreaterAdd q6 q5))) ) ) )
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
                  ||| (q1 === s o () &&& (y15 === s (s q6)) &&& (q2 === s q7 &&& ____fancyEq q7 q6))
                  ||| (q1 === s (s q4) &&& (y15 === s o ()) &&& (q2 === s q7 &&& _____fancyEq q7))
                  ||| (q1 === s (s q4) &&& (y15 === s (s q8)) &&& (______fancyEq q4 q8 &&& _______fancyEq q2 q8))
                  ) ) )
      ||| (y16 === Std.( % ) q9 q10 &&& (_checkStep y14 y17 q9 &&& _doStep y14 y17 q9 q11 &&& checkAnswer_ y14 y15 q10 q11)) )
  and addGreaterAdd y19 y20 = y19 === o ()
  and _fancyEq y21 y22 =
    fresh (q1 q2 q3 q4)
      ( y22 === o () &&& (y21 === o ())
      ||| ( y22 === s q1
          &&& (y21 === s q2)
          &&& (q1 === o () &&& (q2 === o ()) ||| (q1 === s q3 &&& (q2 === s q4) &&& ___fancyEq q3 q4)) ) )
  and __fancyEq y23 y24 = __fancyEq y23 y24
  and ___fancyEq y26 y27 =
    fresh (q1 q2) (y26 === o () &&& (y27 === o ()) ||| (y26 === s q1 &&& (y27 === s q2) &&& ___fancyEq q1 q2))
  and ____fancyEq y28 y29 = fresh (q1) (y28 === s q1 &&& ___fancyEq q1 y29)
  and _____fancyEq y30 = y30 === o ()
  and ______fancyEq y31 y32 =
    fresh (q1 q2 q3)
      ( y31 === o ()
      &&& (y32 === s q1)
      ||| (y31 === s q2 &&& (y32 === o ()))
      ||| (y31 === s q2 &&& (y32 === s q3) &&& ______fancyEq q2 q3) )
  and _______fancyEq y33 y34 = fresh (q1) (y33 === s q1 &&& ____fancyEq q1 y34)
  and _checkStep y35 y36 y37 =
    fresh (q1 q2 q3 q4 q5 q6 q7)
      ( y36 === Std.Pair.pair q1 q2
      &&& (y37 === Std.Pair.pair fill fst)
      &&& _____fancyEq q1
      ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair fill snd) &&& _____fancyEq q2)
      ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair empty fst) &&& (y35 === Std.Pair.pair q3 q4 &&& ___fancyEq q1 q3))
      ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair empty snd) &&& (y35 === Std.Pair.pair q5 q3 &&& ___fancyEq q2 q3))
      ||| (y36 === Std.Pair.pair (s q6) q2 &&& (y37 === Std.Pair.pair pour fst) &&& (y35 === Std.Pair.pair q5 q7 &&& ______fancyEq q2 q7))
      ||| (y36 === Std.Pair.pair q1 q2 &&& (y37 === Std.Pair.pair pour snd) &&& (y35 === Std.Pair.pair q7 q4 &&& fancyEqFancyEq q2 q1 q7)) )
  and _doStep y38 y39 y40 y41 =
    fresh (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10)
      ( y39 === Std.Pair.pair q1 q2
      &&& (y40 === Std.Pair.pair fill fst)
      &&& (y38 === Std.Pair.pair q3 q4 &&& (y41 === Std.Pair.pair q3 q2))
      ||| (y39 === Std.Pair.pair q1 q2 &&& (y40 === Std.Pair.pair fill snd) &&& (y38 === Std.Pair.pair q5 q3 &&& (y41 === Std.Pair.pair q1 q3)))
      ||| (y39 === Std.Pair.pair q1 q2 &&& (y40 === Std.Pair.pair empty fst) &&& (y41 === Std.Pair.pair o () q2))
      ||| (y39 === Std.Pair.pair q1 q2 &&& (y40 === Std.Pair.pair empty snd) &&& (y41 === Std.Pair.pair q1 o ()))
      ||| ( y39 === Std.Pair.pair q1 q2
          &&& (y40 === Std.Pair.pair pour q6)
          &&& ( q6 === fst
              &&& (y38 === Std.Pair.pair q5 q7 &&& (addGreaterAddSub q1 q2 q8 q7 &&& createState y41 q8 q7))
              ||| (q6 === snd &&& (y38 === Std.Pair.pair q7 q4 &&& (_addGreaterAddSub q1 q2 q8 q7 &&& __createState y41 q8 q7))) ) )
      ||| ( y39 === Std.Pair.pair q1 q2
          &&& (y40 === Std.Pair.pair pour q6)
          &&& ( q6 === fst
              &&& (y38 === Std.Pair.pair q5 q9 &&& (_addGreaterAdd q1 q2 q10 q9 &&& ____createState y41 q10))
              ||| (q6 === snd &&& (y38 === Std.Pair.pair q9 q4 &&& (__addGreaterAdd q1 q2 q10 q9 &&& ______createState y41 q10))) ) ) )
  and fancyEqFancyEq y42 y43 y44 = fresh (q1) (y42 === s q1 &&& ______fancyEq y43 y44)
  and addGreaterAddSub y45 y46 y48 y50 = fresh (q1 q2) (add y45 y46 q1 &&& greater q1 y50 &&& add y45 y46 q2 &&& sub y48 q2 y50)
  and createState y51 y52 y53 = createState y51 y52 y53
  and add y54 y55 y56 = fresh (q1 q2) (y54 === o () &&& (y55 === y56) ||| (y54 === s q1 &&& (y56 === s q2) &&& add q1 y55 q2))
  and greater y57 y58 =
    fresh (q1 q2) (y57 === s q1 &&& (y58 === o ()) ||| (y57 === s q1 &&& (y58 === s q2) &&& greater q1 q2))
  and sub y59 y60 y61 =
    fresh (q1 q2)
      ( y61 === o () &&& (y59 === y60)
      ||| (y61 === s q1 &&& (y60 === o ()) &&& (y59 === o ()))
      ||| (y61 === s q1 &&& (y60 === s q2) &&& sub y59 q2 q1) )
  and _addGreaterAddSub y65 y66 y68 y70 = fresh (q1 q2) (add y65 y66 q1 &&& greater q1 y70 &&& add y65 y66 q2 &&& sub y68 q2 y70)
  and __createState y71 y72 y73 = __createState y71 y72 y73
  and _addGreaterAdd y77 y78 y80 y81 = fresh (q1) (add y77 y78 q1 &&& _greater q1 y81 &&& add y77 y78 y80)
  and ____createState y82 y83 = ____createState y82 y83
  and _greater y84 y85 = fresh (q1 q2) (y84 === o () ||| (y84 === s q1 &&& (y85 === s q2) &&& _greater q1 q2))
  and __addGreaterAdd y88 y89 y91 y92 = fresh (q1) (add y88 y89 q1 &&& _greater q1 y92 &&& add y88 y89 y91)
  and ______createState y93 y94 = ______createState y93 y94 in
  checkAnswer x0 x1 x2