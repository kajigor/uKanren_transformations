open GT
open OCanren
open OCanren.Std
open Helper


let topLevel x0 x1 x2 =
    let rec checkAnswer y0 y1 y2 =
      fresh
        (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40
           q41 q42 q43 q44 q45 q46 q47 q48 q49 q50 q51 q52 q53 q54 q55 q56 q57 q58 q59 q60 q61 q62 q63 q64 q65 q66 q67 q68)
        (defer
           ( defer (y0 === Std.List.nil () &&& (y2 === o ()))
           ||| defer
                 ( y0 === Std.( % ) q1 q2
                 &&& defer
                       ( defer
                           ( defer
                               ( defer (q1 === Std.Pair.pair (fill ()) (fst_ ()) ||| (q1 === Std.Pair.pair (fill ()) (snd_ ())))
                               ||| defer (q1 === Std.Pair.pair (empty ()) (fst_ ()) &&& (y1 === Std.Pair.pair (o ()) q3)) )
                           ||| defer (q1 === Std.Pair.pair (empty ()) (snd_ ()) &&& defer (y1 === Std.Pair.pair q4 q5 &&& defer (fancyEq q5))) )
                       &&& defer
                             ( defer
                                 ( defer
                                     ( defer
                                         ( defer
                                             ( defer
                                                 ( q1
                                                 === Std.Pair.pair (fill ()) (fst_ ())
                                                 &&& defer (y1 === Std.Pair.pair q6 q7 &&& (q8 === Std.Pair.pair q6 (o ()))) )
                                             ||| defer
                                                   ( q1
                                                   === Std.Pair.pair (fill ()) (snd_ ())
                                                   &&& defer (y1 === Std.Pair.pair q9 q6 &&& (q8 === Std.Pair.pair (o ()) q6)) ) )
                                         ||| defer (q1 === Std.Pair.pair (empty ()) (fst_ ()) &&& (q8 === Std.Pair.pair (o ()) (o ()))) )
                                     ||| defer (q1 === Std.Pair.pair (empty ()) (snd_ ()) &&& (q8 === Std.Pair.pair (o ()) (o ()))) )
                                 ||| defer
                                       ( q1
                                       === Std.Pair.pair (pour ()) q10
                                       &&& defer
                                             ( defer (q10 === fst_ () &&& defer (y1 === Std.Pair.pair q11 q12 &&& (q8 === Std.Pair.pair (o ()) (o ()))))
                                             ||| defer
                                                   ( q10 === snd_ ()
                                                   &&& defer
                                                         (y1 === Std.Pair.pair q12 q13 &&& defer (q8 === Std.Pair.pair q14 (o ()) &&& defer (addGreaterAdd q12)))
                                                   ) ) ) )
                             &&& defer
                                   ( defer
                                       ( q2 === Std.List.nil ()
                                       &&& defer
                                             ( defer
                                                 ( q8 === Std.Pair.pair q15 q16
                                                 &&& defer
                                                       ( defer
                                                           ( defer (q15 === o () &&& (y2 === o ()))
                                                           ||| defer
                                                                 ( defer (q15 === s q17 &&& (y2 === s q18))
                                                                 &&& defer
                                                                       ( defer (q17 === o () &&& (q18 === o ()))
                                                                       ||| defer (defer (q17 === s q19 &&& (q18 === s q20)) &&& defer (_fancyEq q19 q20)) ) ) )
                                                       &&& defer
                                                             ( defer
                                                                 ( defer (defer (q16 === o () &&& (y2 === o ())) ||| defer (q16 === o () &&& (y2 === s q21)))
                                                                 ||| defer (q16 === s q22 &&& (y2 === o ())) )
                                                             ||| defer (defer (q16 === s q22 &&& (y2 === s q23)) &&& defer (__fancyEq q22)) ) ) )
                                             ||| defer
                                                   ( q8 === Std.Pair.pair q15 q16
                                                   &&& defer
                                                         ( defer
                                                             ( defer
                                                                 ( defer
                                                                     ( defer
                                                                         ( defer (q15 === o () &&& (y2 === s q24))
                                                                         &&& defer (q16 === s q25 &&& defer (_fancyEq q25 q24)) )
                                                                     ||| defer (defer (q15 === s q26 &&& (y2 === o ())) &&& (q16 === o ())) )
                                                                 ||| defer
                                                                       ( defer (q15 === s (o ()) &&& (y2 === s (s q27)))
                                                                       &&& defer (q16 === s q28 &&& defer (___fancyEq q28 q27)) ) )
                                                             ||| defer
                                                                   ( defer (q15 === s (s q29) &&& (y2 === s (o ())))
                                                                   &&& defer (q16 === s q30 &&& defer (____fancyEq q30)) ) )
                                                         ||| defer
                                                               ( defer (q15 === s (s q29) &&& (y2 === s (s q31)))
                                                               &&& defer
                                                                     ( defer
                                                                         ( defer
                                                                             ( defer (q29 === o () &&& (q31 === s q32))
                                                                             ||| defer (q29 === s q33 &&& (q31 === o ())) )
                                                                         ||| defer (defer (q29 === s q33 &&& (q31 === s q34)) &&& defer (_____fancyEq q33 q34))
                                                                         )
                                                                     &&& defer (______fancyEq q16 q31) ) ) ) ) ) )
                                   ||| defer
                                         ( q2 === Std.( % ) q35 q36
                                         &&& defer
                                               ( defer
                                                   ( defer
                                                       ( defer
                                                           ( defer
                                                               ( defer
                                                                   ( defer
                                                                       ( defer
                                                                           ( defer (q8 === Std.Pair.pair q37 q38 &&& (q35 === Std.Pair.pair (fill ()) (fst_ ())))
                                                                           &&& defer (____fancyEq q37) )
                                                                       ||| defer
                                                                             ( defer
                                                                                 (q8 === Std.Pair.pair q37 q38 &&& (q35 === Std.Pair.pair (fill ()) (snd_ ())))
                                                                             &&& defer (____fancyEq q38) ) )
                                                                   ||| defer
                                                                         ( defer (q8 === Std.Pair.pair q37 q38 &&& (q35 === Std.Pair.pair (empty ()) (fst_ ())))
                                                                         &&& defer (y1 === Std.Pair.pair q39 q40 &&& defer (_fancyEq q37 q39)) ) )
                                                               ||| defer
                                                                     ( defer (q8 === Std.Pair.pair q37 q38 &&& (q35 === Std.Pair.pair (empty ()) (snd_ ())))
                                                                     &&& defer (y1 === Std.Pair.pair q41 q39 &&& defer (_fancyEq q38 q39)) ) )
                                                           ||| defer
                                                                 ( defer (q8 === Std.Pair.pair (s q42) q38 &&& (q35 === Std.Pair.pair (pour ()) (fst_ ())))
                                                                 &&& defer (y1 === Std.Pair.pair q43 q44 &&& defer (_____fancyEq q38 q44)) ) )
                                                       ||| defer
                                                             ( defer (q8 === Std.Pair.pair q37 q38 &&& (q35 === Std.Pair.pair (pour ()) (snd_ ())))
                                                             &&& defer (y1 === Std.Pair.pair q44 q45 &&& defer (fancyEqFancyEq q38 q37 q44)) ) )
                                                   &&& defer
                                                         ( defer
                                                             ( defer
                                                                 ( defer
                                                                     ( defer
                                                                         ( defer
                                                                             ( defer
                                                                                 (q8 === Std.Pair.pair q46 q47 &&& (q35 === Std.Pair.pair (fill ()) (fst_ ())))
                                                                             &&& defer (y1 === Std.Pair.pair q48 q49 &&& (q50 === Std.Pair.pair q48 q47)) )
                                                                         ||| defer
                                                                               ( defer
                                                                                   (q8 === Std.Pair.pair q46 q47 &&& (q35 === Std.Pair.pair (fill ()) (snd_ ())))
                                                                               &&& defer (y1 === Std.Pair.pair q51 q48 &&& (q50 === Std.Pair.pair q46 q48)) ) )
                                                                     ||| defer
                                                                           ( defer (q8 === Std.Pair.pair q46 q47 &&& (q35 === Std.Pair.pair (empty ()) (fst_ ())))
                                                                           &&& (q50 === Std.Pair.pair (o ()) q47) ) )
                                                                 ||| defer
                                                                       ( defer (q8 === Std.Pair.pair q46 q47 &&& (q35 === Std.Pair.pair (empty ()) (snd_ ())))
                                                                       &&& (q50 === Std.Pair.pair q46 (o ())) ) )
                                                             ||| defer
                                                                   ( defer (q8 === Std.Pair.pair q46 q47 &&& (q35 === Std.Pair.pair (pour ()) q52))
                                                                   &&& defer
                                                                         ( defer
                                                                             ( q52 === fst_ ()
                                                                             &&& defer
                                                                                   ( y1 === Std.Pair.pair q53 q54
                                                                                   &&& defer
                                                                                         ( defer
                                                                                             ( defer
                                                                                                 ( defer
                                                                                                     ( defer (add q46 q47 q55)
                                                                                                     &&& defer
                                                                                                           ( defer (q55 === s q56 &&& (q54 === o ()))
                                                                                                           ||| defer
                                                                                                                 ( defer (q55 === s q56 &&& (q54 === s q57))
                                                                                                                 &&& defer (greater q56 q57) ) ) )
                                                                                                 &&& defer (add q46 q47 q58) )
                                                                                             &&& defer
                                                                                                   ( defer
                                                                                                       ( defer (q54 === o () &&& (q59 === q58))
                                                                                                       ||| defer
                                                                                                             ( defer (q54 === s q60 &&& (q58 === o ()))
                                                                                                             &&& (q59 === o ()) ) )
                                                                                                   ||| defer
                                                                                                         ( defer (q54 === s q60 &&& (q58 === s q61))
                                                                                                         &&& defer (sub q59 q61 q60) ) ) )
                                                                                         &&& defer (createState q50 q54 q59) ) ) )
                                                                         ||| defer
                                                                               ( q52 === snd_ ()
                                                                               &&& defer
                                                                                     ( y1 === Std.Pair.pair q54 q62
                                                                                     &&& defer
                                                                                           ( defer
                                                                                               ( defer
                                                                                                   ( defer (defer (add q46 q47 q55) &&& defer (greater q55 q54))
                                                                                                   &&& defer (add q46 q47 q58) )
                                                                                               &&& defer (sub q59 q58 q54) )
                                                                                           &&& defer (_createState q50 q54 q59) ) ) ) ) ) )
                                                         ||| defer
                                                               ( defer (q8 === Std.Pair.pair q46 q47 &&& (q35 === Std.Pair.pair (pour ()) q52))
                                                               &&& defer
                                                                     ( defer
                                                                         ( q52 === fst_ ()
                                                                         &&& defer
                                                                               ( y1 === Std.Pair.pair q63 q64
                                                                               &&& defer
                                                                                     ( defer
                                                                                         ( defer
                                                                                             ( defer (add q46 q47 q55)
                                                                                             &&& defer
                                                                                                   ( q55 === o ()
                                                                                                   ||| defer
                                                                                                         ( defer (q55 === s q65 &&& (q64 === s q66))
                                                                                                         &&& defer (_greater q65 q66) ) ) )
                                                                                         &&& defer (add q46 q47 q67) )
                                                                                     &&& defer (__createState q50 q67) ) ) )
                                                                     ||| defer
                                                                           ( q52 === snd_ ()
                                                                           &&& defer
                                                                                 ( y1 === Std.Pair.pair q64 q68
                                                                                 &&& defer
                                                                                       ( defer
                                                                                           ( defer (defer (add q46 q47 q55) &&& defer (_greater q55 q64))
                                                                                           &&& defer (add q46 q47 q67) )
                                                                                       &&& defer (___createState q50 q67) ) ) ) ) ) ) )
                                               &&& defer (checkAnswer_ y1 y2 q36 q50) ) ) ) ) ) ) ))
    and fancyEq y3 = y3 === o ()
    and addGreaterAdd y6 = defer success
    and _fancyEq y7 y8 =
      fresh (q1 q2) (defer (defer (y7 === o () &&& (y8 === o ())) ||| defer (defer (y7 === s q1 &&& (y8 === s q2)) &&& defer (_fancyEq q1 q2))))
    and __fancyEq y10 = fresh (q1) (defer (defer (defer (y10 === o () ||| (y10 === o ())) ||| (y10 === s q1)) ||| defer (y10 === s q1 &&& defer (__fancyEq q1))))
    and ___fancyEq y12 y13 = fresh (q1) (defer (y12 === s q1 &&& defer (_fancyEq q1 y13)))
    and ____fancyEq y14 = y14 === o ()
    and _____fancyEq y15 y16 =
      fresh (q1 q2 q3)
        (defer
           ( defer (defer (y15 === o () &&& (y16 === s q1)) ||| defer (y15 === s q2 &&& (y16 === o ())))
           ||| defer (defer (y15 === s q2 &&& (y16 === s q3)) &&& defer (_____fancyEq q2 q3)) ))
    and ______fancyEq y17 y18 = fresh (q1) (defer (y17 === s q1 &&& defer (___fancyEq q1 y18)))
    and fancyEqFancyEq y19 y20 y21 = fresh (q1) (defer (y19 === s q1 &&& defer (_____fancyEq y20 y21)))
    and add y22 y23 y24 =
      fresh (q1 q2) (defer (defer (y22 === o () &&& (y23 === y24)) ||| defer (defer (y22 === s q1 &&& (y24 === s q2)) &&& defer (add q1 y23 q2))))
    and greater y25 y26 =
      fresh (q1 q2) (defer (defer (y25 === s q1 &&& (y26 === o ())) ||| defer (defer (y25 === s q1 &&& (y26 === s q2)) &&& defer (greater q1 q2))))
    and sub y27 y28 y29 =
      fresh (q1 q2)
        (defer
           ( defer (defer (y29 === o () &&& (y27 === y28)) ||| defer (defer (y29 === s q1 &&& (y28 === o ())) &&& (y27 === o ())))
           ||| defer (defer (y29 === s q1 &&& (y28 === s q2)) &&& defer (sub y27 q2 q1)) ))
    and createState y30 y31 y32 = y30 === Std.Pair.pair y32 y31
    and _createState y33 y34 y35 = y33 === Std.Pair.pair y34 y35
    and _greater y36 y37 = fresh (q1 q2) (defer (y36 === o () ||| defer (defer (y36 === s q1 &&& (y37 === s q2)) &&& defer (_greater q1 q2))))
    and __createState y38 y39 = y38 === Std.Pair.pair (o ()) y39
    and ___createState y40 y41 = y40 === Std.Pair.pair y41 (o ())
    and checkAnswer_ y42 y43 y44 y45 =
      fresh
        (q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 q19 q20 q21 q22 q23 q24 q25 q26 q27 q28 q29 q30 q31 q32 q33 q34 q35 q36 q37 q38 q39 q40
           q41 q42 q43 q44 q45 q46 q47 q48 q49 q50 q51 q52 q53 q54)
        (defer
           ( defer
               ( y44 === Std.List.nil ()
               &&& defer
                     ( defer
                         ( y45 === Std.Pair.pair q1 q2
                         &&& defer
                               ( defer
                                   ( defer (q1 === o () &&& (y43 === o ()))
                                   ||| defer
                                         ( defer (q1 === s q3 &&& (y43 === s q4))
                                         &&& defer
                                               ( defer (q3 === o () &&& (q4 === o ()))
                                               ||| defer (defer (q3 === s q5 &&& (q4 === s q6)) &&& defer (_fancyEq q5 q6)) ) ) )
                               &&& defer
                                     ( defer
                                         ( defer (defer (q2 === o () &&& (y43 === o ())) ||| defer (q2 === o () &&& (y43 === s q7)))
                                         ||| defer (q2 === s q8 &&& (y43 === o ())) )
                                     ||| defer (defer (q2 === s q8 &&& (y43 === s q9)) &&& defer (__fancyEq q8)) ) ) )
                     ||| defer
                           ( y45 === Std.Pair.pair q1 q2
                           &&& defer
                                 ( defer
                                     ( defer
                                         ( defer
                                             ( defer (defer (q1 === o () &&& (y43 === s q10)) &&& defer (q2 === s q11 &&& defer (_fancyEq q11 q10)))
                                             ||| defer (defer (q1 === s q12 &&& (y43 === o ())) &&& (q2 === o ())) )
                                         ||| defer (defer (q1 === s (o ()) &&& (y43 === s (s q13))) &&& defer (q2 === s q14 &&& defer (___fancyEq q14 q13))) )
                                     ||| defer (defer (q1 === s (s q15) &&& (y43 === s (o ()))) &&& defer (q2 === s q16 &&& defer (____fancyEq q16))) )
                                 ||| defer
                                       ( defer (q1 === s (s q15) &&& (y43 === s (s q17)))
                                       &&& defer
                                             ( defer
                                                 ( defer (defer (q15 === o () &&& (q17 === s q18)) ||| defer (q15 === s q19 &&& (q17 === o ())))
                                                 ||| defer (defer (q15 === s q19 &&& (q17 === s q20)) &&& defer (_____fancyEq q19 q20)) )
                                             &&& defer (______fancyEq q2 q17) ) ) ) ) ) )
           ||| defer
                 ( y44 === Std.( % ) q21 q22
                 &&& defer
                       ( defer
                           ( defer
                               ( defer
                                   ( defer
                                       ( defer
                                           ( defer
                                               ( defer
                                                   ( defer (y45 === Std.Pair.pair q23 q24 &&& (q21 === Std.Pair.pair (fill ()) (fst_ ())))
                                                   &&& defer (____fancyEq q23) )
                                               ||| defer
                                                     ( defer (y45 === Std.Pair.pair q23 q24 &&& (q21 === Std.Pair.pair (fill ()) (snd_ ())))
                                                     &&& defer (____fancyEq q24) ) )
                                           ||| defer
                                                 ( defer (y45 === Std.Pair.pair q23 q24 &&& (q21 === Std.Pair.pair (empty ()) (fst_ ())))
                                                 &&& defer (y42 === Std.Pair.pair q25 q26 &&& defer (_fancyEq q23 q25)) ) )
                                       ||| defer
                                             ( defer (y45 === Std.Pair.pair q23 q24 &&& (q21 === Std.Pair.pair (empty ()) (snd_ ())))
                                             &&& defer (y42 === Std.Pair.pair q27 q25 &&& defer (_fancyEq q24 q25)) ) )
                                   ||| defer
                                         ( defer (y45 === Std.Pair.pair (s q28) q24 &&& (q21 === Std.Pair.pair (pour ()) (fst_ ())))
                                         &&& defer (y42 === Std.Pair.pair q29 q30 &&& defer (_____fancyEq q24 q30)) ) )
                               ||| defer
                                     ( defer (y45 === Std.Pair.pair q23 q24 &&& (q21 === Std.Pair.pair (pour ()) (snd_ ())))
                                     &&& defer (y42 === Std.Pair.pair q30 q31 &&& defer (fancyEqFancyEq q24 q23 q30)) ) )
                           &&& defer
                                 ( defer
                                     ( defer
                                         ( defer
                                             ( defer
                                                 ( defer
                                                     ( defer (y45 === Std.Pair.pair q32 q33 &&& (q21 === Std.Pair.pair (fill ()) (fst_ ())))
                                                     &&& defer (y42 === Std.Pair.pair q34 q35 &&& (q36 === Std.Pair.pair q34 q33)) )
                                                 ||| defer
                                                       ( defer (y45 === Std.Pair.pair q32 q33 &&& (q21 === Std.Pair.pair (fill ()) (snd_ ())))
                                                       &&& defer (y42 === Std.Pair.pair q37 q34 &&& (q36 === Std.Pair.pair q32 q34)) ) )
                                             ||| defer
                                                   ( defer (y45 === Std.Pair.pair q32 q33 &&& (q21 === Std.Pair.pair (empty ()) (fst_ ())))
                                                   &&& (q36 === Std.Pair.pair (o ()) q33) ) )
                                         ||| defer
                                               ( defer (y45 === Std.Pair.pair q32 q33 &&& (q21 === Std.Pair.pair (empty ()) (snd_ ())))
                                               &&& (q36 === Std.Pair.pair q32 (o ())) ) )
                                     ||| defer
                                           ( defer (y45 === Std.Pair.pair q32 q33 &&& (q21 === Std.Pair.pair (pour ()) q38))
                                           &&& defer
                                                 ( defer
                                                     ( q38 === fst_ ()
                                                     &&& defer
                                                           ( y42 === Std.Pair.pair q39 q40
                                                           &&& defer
                                                                 ( defer
                                                                     ( defer
                                                                         ( defer
                                                                             ( defer (add q32 q33 q41)
                                                                             &&& defer
                                                                                   ( defer (q41 === s q42 &&& (q40 === o ()))
                                                                                   ||| defer
                                                                                         (defer (q41 === s q42 &&& (q40 === s q43)) &&& defer (greater q42 q43))
                                                                                   ) )
                                                                         &&& defer (add q32 q33 q44) )
                                                                     &&& defer
                                                                           ( defer
                                                                               ( defer (q40 === o () &&& (q45 === q44))
                                                                               ||| defer (defer (q40 === s q46 &&& (q44 === o ())) &&& (q45 === o ())) )
                                                                           ||| defer (defer (q40 === s q46 &&& (q44 === s q47)) &&& defer (sub q45 q47 q46)) ) )
                                                                 &&& defer (createState q36 q40 q45) ) ) )
                                                 ||| defer
                                                       ( q38 === snd_ ()
                                                       &&& defer
                                                             ( y42 === Std.Pair.pair q40 q48
                                                             &&& defer
                                                                   ( defer
                                                                       ( defer
                                                                           ( defer (defer (add q32 q33 q41) &&& defer (greater q41 q40))
                                                                           &&& defer (add q32 q33 q44) )
                                                                       &&& defer (sub q45 q44 q40) )
                                                                   &&& defer (_createState q36 q40 q45) ) ) ) ) ) )
                                 ||| defer
                                       ( defer (y45 === Std.Pair.pair q32 q33 &&& (q21 === Std.Pair.pair (pour ()) q38))
                                       &&& defer
                                             ( defer
                                                 ( q38 === fst_ ()
                                                 &&& defer
                                                       ( y42 === Std.Pair.pair q49 q50
                                                       &&& defer
                                                             ( defer
                                                                 ( defer
                                                                     ( defer (add q32 q33 q41)
                                                                     &&& defer
                                                                           ( q41 === o ()
                                                                           ||| defer (defer (q41 === s q51 &&& (q50 === s q52)) &&& defer (_greater q51 q52)) )
                                                                     )
                                                                 &&& defer (add q32 q33 q53) )
                                                             &&& defer (__createState q36 q53) ) ) )
                                             ||| defer
                                                   ( q38 === snd_ ()
                                                   &&& defer
                                                         ( y42 === Std.Pair.pair q50 q54
                                                         &&& defer
                                                               ( defer (defer (defer (add q32 q33 q41) &&& defer (_greater q41 q50)) &&& defer (add q32 q33 q53))
                                                               &&& defer (___createState q36 q53) ) ) ) ) ) ) )
                       &&& defer (checkAnswer_ y42 y43 q22 q36) ) ) ))
    in
    defer (checkAnswer x0 x1 x2)