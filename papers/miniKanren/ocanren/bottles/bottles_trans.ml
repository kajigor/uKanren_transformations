open GT
open OCanren
open OCanren.Std
open Helper


let topLevel x0 x1 x2 =
    let rec checkAnswer y0 y1 y2 =
      fresh
        (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40
           q41 q42 q43 q44 q45 q46 q47 q48 q49 q50 q51 q52 q53 q54 q55 q56 q57 q58 q59 q60 q61 q62 q63 q64 q65 q66 q67 q68)
        ( y0 === Std.List.nil ()
        &&& (y2 === o ())
        ||| ( y0 === Std.( % ) q1 q2
            &&& ( q1
                === Std.Pair.pair (fill ()) (fst_ ())
                ||| (q1 === Std.Pair.pair (fill ()) (snd_ ()))
                ||| (q1 === Std.Pair.pair (empty ()) (fst_ ()) &&& (y1 === Std.Pair.pair (o ()) q3))
                ||| (q1 === Std.Pair.pair (empty ()) (snd_ ()) &&& (y1 === Std.Pair.pair q4 q5 &&& fancyEq q5))
                &&& ( q1
                    === Std.Pair.pair (fill ()) (fst_ ())
                    &&& (y1 === Std.Pair.pair q6 q7 &&& (q8 === Std.Pair.pair q6 (o ())))
                    ||| (q1 === Std.Pair.pair (fill ()) (snd_ ()) &&& (y1 === Std.Pair.pair q9 q6 &&& (q8 === Std.Pair.pair (o ()) q6)))
                    ||| (q1 === Std.Pair.pair (empty ()) (fst_ ()) &&& (q8 === Std.Pair.pair (o ()) (o ())))
                    ||| (q1 === Std.Pair.pair (empty ()) (snd_ ()) &&& (q8 === Std.Pair.pair (o ()) (o ())))
                    ||| ( q1
                        === Std.Pair.pair (pour ()) q10
                        &&& ( q10 === fst_ ()
                            &&& (y1 === Std.Pair.pair q11 q12 &&& (q8 === Std.Pair.pair (o ()) (o ())))
                            ||| (q10 === snd_ () &&& (y1 === Std.Pair.pair q12 q13 &&& (q8 === Std.Pair.pair q14 (o ()) &&& addGreaterAdd q12))) ) )
                    &&& ( q2 === Std.List.nil ()
                        &&& ( q8 === Std.Pair.pair q15 q16
                            &&& ( q15 === o ()
                                &&& (y2 === o ())
                                ||| ( q15 === s q17
                                    &&& (y2 === s q18)
                                    &&& (q17 === o () &&& (q18 === o ()) ||| (q17 === s q19 &&& (q18 === s q20) &&& _fancyEq q19 q20)) )
                                &&& ( q16 === o ()
                                    &&& (y2 === o ())
                                    ||| (q16 === o () &&& (y2 === s q21))
                                    ||| (q16 === s q22 &&& (y2 === o ()))
                                    ||| (q16 === s q22 &&& (y2 === s q23) &&& __fancyEq q22) ) )
                            ||| ( q8 === Std.Pair.pair q15 q16
                                &&& ( q15 === o ()
                                    &&& (y2 === s q24)
                                    &&& (q16 === s q25 &&& _fancyEq q25 q24)
                                    ||| (q15 === s q26 &&& (y2 === o ()) &&& (q16 === o ()))
                                    ||| (q15 === s (o ()) &&& (y2 === s (s q27)) &&& (q16 === s q28 &&& ___fancyEq q28 q27))
                                    ||| (q15 === s (s q29) &&& (y2 === s (o ())) &&& (q16 === s q30 &&& ____fancyEq q30))
                                    ||| ( q15
                                        === s (s q29)
                                        &&& (y2 === s (s q31))
                                        &&& ( q29 === o ()
                                            &&& (q31 === s q32)
                                            ||| (q29 === s q33 &&& (q31 === o ()))
                                            ||| (q29 === s q33 &&& (q31 === s q34) &&& _____fancyEq q33 q34)
                                            &&& ______fancyEq q16 q31 ) ) ) ) )
                        ||| ( q2 === Std.( % ) q35 q36
                            &&& ( q8 === Std.Pair.pair q37 q38
                                &&& (q35 === Std.Pair.pair (fill ()) (fst_ ()))
                                &&& ____fancyEq q37
                                ||| (q8 === Std.Pair.pair q37 q38 &&& (q35 === Std.Pair.pair (fill ()) (snd_ ())) &&& ____fancyEq q38)
                                ||| ( q8 === Std.Pair.pair q37 q38
                                    &&& (q35 === Std.Pair.pair (empty ()) (fst_ ()))
                                    &&& (y1 === Std.Pair.pair q39 q40 &&& _fancyEq q37 q39) )
                                ||| ( q8 === Std.Pair.pair q37 q38
                                    &&& (q35 === Std.Pair.pair (empty ()) (snd_ ()))
                                    &&& (y1 === Std.Pair.pair q41 q39 &&& _fancyEq q38 q39) )
                                ||| ( q8
                                    === Std.Pair.pair (s q42) q38
                                    &&& (q35 === Std.Pair.pair (pour ()) (fst_ ()))
                                    &&& (y1 === Std.Pair.pair q43 q44 &&& _____fancyEq q38 q44) )
                                ||| ( q8 === Std.Pair.pair q37 q38
                                    &&& (q35 === Std.Pair.pair (pour ()) (snd_ ()))
                                    &&& (y1 === Std.Pair.pair q44 q45 &&& fancyEqFancyEq q38 q37 q44) )
                                &&& ( q8 === Std.Pair.pair q46 q47
                                    &&& (q35 === Std.Pair.pair (fill ()) (fst_ ()))
                                    &&& (y1 === Std.Pair.pair q48 q49 &&& (q50 === Std.Pair.pair q48 q47))
                                    ||| ( q8 === Std.Pair.pair q46 q47
                                        &&& (q35 === Std.Pair.pair (fill ()) (snd_ ()))
                                        &&& (y1 === Std.Pair.pair q51 q48 &&& (q50 === Std.Pair.pair q46 q48)) )
                                    ||| (q8 === Std.Pair.pair q46 q47 &&& (q35 === Std.Pair.pair (empty ()) (fst_ ())) &&& (q50 === Std.Pair.pair (o ()) q47))
                                    ||| (q8 === Std.Pair.pair q46 q47 &&& (q35 === Std.Pair.pair (empty ()) (snd_ ())) &&& (q50 === Std.Pair.pair q46 (o ())))
                                    ||| ( q8 === Std.Pair.pair q46 q47
                                        &&& (q35 === Std.Pair.pair (pour ()) q52)
                                        &&& ( q52 === fst_ ()
                                            &&& ( y1 === Std.Pair.pair q53 q54
                                                &&& ( add q46 q47 q55
                                                    &&& (q55 === s q56 &&& (q54 === o ()) ||| (q55 === s q56 &&& (q54 === s q57) &&& greater q56 q57))
                                                    &&& add q46 q47 q58
                                                    &&& ( q54 === o () &&& (q59 === q58)
                                                        ||| (q54 === s q60 &&& (q58 === o ()) &&& (q59 === o ()))
                                                        ||| (q54 === s q60 &&& (q58 === s q61) &&& sub q59 q61 q60) )
                                                    &&& createState q50 q54 q59 ) )
                                            ||| ( q52 === snd_ ()
                                                &&& ( y1 === Std.Pair.pair q54 q62
                                                    &&& (add q46 q47 q55 &&& greater q55 q54 &&& add q46 q47 q58 &&& sub q59 q58 q54 &&& _createState q50 q54 q59)
                                                    ) ) ) )
                                    ||| ( q8 === Std.Pair.pair q46 q47
                                        &&& (q35 === Std.Pair.pair (pour ()) q52)
                                        &&& ( q52 === fst_ ()
                                            &&& ( y1 === Std.Pair.pair q63 q64
                                                &&& ( add q46 q47 q55
                                                    &&& (q55 === o () ||| (q55 === s q65 &&& (q64 === s q66) &&& _greater q65 q66))
                                                    &&& add q46 q47 q67 &&& __createState q50 q67 ) )
                                            ||| ( q52 === snd_ ()
                                                &&& ( y1 === Std.Pair.pair q64 q68
                                                    &&& (add q46 q47 q55 &&& _greater q55 q64 &&& add q46 q47 q67 &&& ___createState q50 q67) ) ) ) ) )
                                &&& checkAnswer_ y1 y2 q36 q50 ) ) ) ) ) ) )
    and fancyEq y3 = y3 === o ()
    and addGreaterAdd y6 = success
    and _fancyEq y7 y8 = fresh (q1 q2) (y7 === o () &&& (y8 === o ()) ||| (y7 === s q1 &&& (y8 === s q2) &&& _fancyEq q1 q2))
    and __fancyEq y10 = fresh (q1) (y10 === o () ||| (y10 === o ()) ||| (y10 === s q1) ||| (y10 === s q1 &&& __fancyEq q1))
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
        (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40
           q41 q42 q43 q44 q45 q46 q47 q48 q49 q50 q51 q52 q53 q54)
        ( y44 === Std.List.nil ()
        &&& ( y45 === Std.Pair.pair q1 q2
            &&& ( q1 === o ()
                &&& (y43 === o ())
                ||| (q1 === s q3 &&& (y43 === s q4) &&& (q3 === o () &&& (q4 === o ()) ||| (q3 === s q5 &&& (q4 === s q6) &&& _fancyEq q5 q6)))
                &&& ( q2 === o ()
                    &&& (y43 === o ())
                    ||| (q2 === o () &&& (y43 === s q7))
                    ||| (q2 === s q8 &&& (y43 === o ()))
                    ||| (q2 === s q8 &&& (y43 === s q9) &&& __fancyEq q8) ) )
            ||| ( y45 === Std.Pair.pair q1 q2
                &&& ( q1 === o ()
                    &&& (y43 === s q10)
                    &&& (q2 === s q11 &&& _fancyEq q11 q10)
                    ||| (q1 === s q12 &&& (y43 === o ()) &&& (q2 === o ()))
                    ||| (q1 === s (o ()) &&& (y43 === s (s q13)) &&& (q2 === s q14 &&& ___fancyEq q14 q13))
                    ||| (q1 === s (s q15) &&& (y43 === s (o ())) &&& (q2 === s q16 &&& ____fancyEq q16))
                    ||| ( q1
                        === s (s q15)
                        &&& (y43 === s (s q17))
                        &&& ( q15 === o ()
                            &&& (q17 === s q18)
                            ||| (q15 === s q19 &&& (q17 === o ()))
                            ||| (q15 === s q19 &&& (q17 === s q20) &&& _____fancyEq q19 q20)
                            &&& ______fancyEq q2 q17 ) ) ) ) )
        ||| ( y44 === Std.( % ) q21 q22
            &&& ( y45 === Std.Pair.pair q23 q24
                &&& (q21 === Std.Pair.pair (fill ()) (fst_ ()))
                &&& ____fancyEq q23
                ||| (y45 === Std.Pair.pair q23 q24 &&& (q21 === Std.Pair.pair (fill ()) (snd_ ())) &&& ____fancyEq q24)
                ||| (y45 === Std.Pair.pair q23 q24 &&& (q21 === Std.Pair.pair (empty ()) (fst_ ())) &&& (y42 === Std.Pair.pair q25 q26 &&& _fancyEq q23 q25))
                ||| (y45 === Std.Pair.pair q23 q24 &&& (q21 === Std.Pair.pair (empty ()) (snd_ ())) &&& (y42 === Std.Pair.pair q27 q25 &&& _fancyEq q24 q25))
                ||| ( y45
                    === Std.Pair.pair (s q28) q24
                    &&& (q21 === Std.Pair.pair (pour ()) (fst_ ()))
                    &&& (y42 === Std.Pair.pair q29 q30 &&& _____fancyEq q24 q30) )
                ||| ( y45 === Std.Pair.pair q23 q24
                    &&& (q21 === Std.Pair.pair (pour ()) (snd_ ()))
                    &&& (y42 === Std.Pair.pair q30 q31 &&& fancyEqFancyEq q24 q23 q30) )
                &&& ( y45 === Std.Pair.pair q32 q33
                    &&& (q21 === Std.Pair.pair (fill ()) (fst_ ()))
                    &&& (y42 === Std.Pair.pair q34 q35 &&& (q36 === Std.Pair.pair q34 q33))
                    ||| ( y45 === Std.Pair.pair q32 q33
                        &&& (q21 === Std.Pair.pair (fill ()) (snd_ ()))
                        &&& (y42 === Std.Pair.pair q37 q34 &&& (q36 === Std.Pair.pair q32 q34)) )
                    ||| (y45 === Std.Pair.pair q32 q33 &&& (q21 === Std.Pair.pair (empty ()) (fst_ ())) &&& (q36 === Std.Pair.pair (o ()) q33))
                    ||| (y45 === Std.Pair.pair q32 q33 &&& (q21 === Std.Pair.pair (empty ()) (snd_ ())) &&& (q36 === Std.Pair.pair q32 (o ())))
                    ||| ( y45 === Std.Pair.pair q32 q33
                        &&& (q21 === Std.Pair.pair (pour ()) q38)
                        &&& ( q38 === fst_ ()
                            &&& ( y42 === Std.Pair.pair q39 q40
                                &&& ( add q32 q33 q41
                                    &&& (q41 === s q42 &&& (q40 === o ()) ||| (q41 === s q42 &&& (q40 === s q43) &&& greater q42 q43))
                                    &&& add q32 q33 q44
                                    &&& ( q40 === o () &&& (q45 === q44)
                                        ||| (q40 === s q46 &&& (q44 === o ()) &&& (q45 === o ()))
                                        ||| (q40 === s q46 &&& (q44 === s q47) &&& sub q45 q47 q46) )
                                    &&& createState q36 q40 q45 ) )
                            ||| ( q38 === snd_ ()
                                &&& ( y42 === Std.Pair.pair q40 q48
                                    &&& (add q32 q33 q41 &&& greater q41 q40 &&& add q32 q33 q44 &&& sub q45 q44 q40 &&& _createState q36 q40 q45) ) ) ) )
                    ||| ( y45 === Std.Pair.pair q32 q33
                        &&& (q21 === Std.Pair.pair (pour ()) q38)
                        &&& ( q38 === fst_ ()
                            &&& ( y42 === Std.Pair.pair q49 q50
                                &&& ( add q32 q33 q41
                                    &&& (q41 === o () ||| (q41 === s q51 &&& (q50 === s q52) &&& _greater q51 q52))
                                    &&& add q32 q33 q53 &&& __createState q36 q53 ) )
                            ||| ( q38 === snd_ ()
                                &&& (y42 === Std.Pair.pair q50 q54 &&& (add q32 q33 q41 &&& _greater q41 q50 &&& add q32 q33 q53 &&& ___createState q36 q53)) )
                            ) ) )
                &&& checkAnswer_ y42 y43 q22 q36 ) ) )
    in
    checkAnswer x0 x1 x2
