open GT
open OCanren
open OCanren.Std

open Helper

let rec add a b q5 = fresh (q1) (a q1) (((q1 === (o ())) &&& (b q5)) ||| (fresh (x) (q1 === (s x)) (add (fun q4 -> x === q4) (fun q3 -> fresh (q2) (q3 === (s q2)) (b q2)) q5)))
let rec greater a b q14 =
  fresh (q7) (a q7)
    (((q7 === (o ())) &&& (q14 === (!! false))) |||
       (fresh (x q10) (q7 === (s x)) (b q10)
          (((q10 === (o ())) &&& (q14 === (!! true))) ||| (fresh (y) (q10 === (s y)) (greater (fun q13 -> x === q13) (fun q12 -> y === q12) q14)))))
let rec sub a b q22 =
  fresh (q16) (b q16)
    (((q16 === (o ())) &&& (a q22)) |||
       (fresh (y q18) (q16 === (s y)) (a q18) (((q18 === (o ())) &&& (q22 === (o ()))) ||| (fresh (x) (q18 === (s x)) (sub (fun q20 -> x === q20) (fun q21 -> y === q21) q22)))))
let anotherBottle b q27 = fresh (q24) (b q24) (((q24 === (fst_ ())) &&& (q27 === (snd_ ()))) ||| ((q24 === (snd_ ())) &&& (q27 === (fst_ ()))))
let createState bottle lvl1 lvl2 q36 =
  fresh (q29) (bottle q29)
    ((fresh (q30 q31) (q29 === (fst_ ())) (q36 === (pair q30 q31)) (lvl1 q30) (lvl2 q31)) ||| (fresh (q33 q34) (q29 === (snd_ ())) (q36 === (pair q33 q34)) (lvl2 q33) (lvl1 q34)))
let checkStep state0 step0 capacities q80 =
  fresh (q38 f s q40 t b) (q38 === (pair f s)) (q40 === (pair t b)) (
    state0 q38) (step0 q40)
    (let lvl1 q41 = fresh (q42) (b === q42) (((q42 === (fst_ ())) &&& (f === q41)) ||| ((q42 === (snd_ ())) &&& (s === q41))) in
     let lvl2 q43 = fresh (q44) (b === q44) (((q44 === (fst_ ())) &&& (s === q43)) ||| ((q44 === (snd_ ())) &&& (f === q43))) in
     fresh (q46) (t === q46)
       (conde
          [fresh (q50 q51) (q46 === (fill ())) (q51 === (o ())) (lvl1 q50) (conde [(q50 === q51) &&& (q80 === (!! true)); (q80 === (!! false)) &&& (q50 =/= q51)]);
          fresh (q55 q56) (q46 === (empty ())) (lvl1 q55) (capacities (fun q77 -> b === q77) q56)
            (conde [(q55 === q56) &&& (q80 === (!! true)); (q80 === (!! false)) &&& (q55 =/= q56)]);
          (q46 === (pour ())) &&&
            ((let b' = anotherBottle (fun q77 -> b === q77) in
              fresh (q74 q71 q61 q62) (q62 === (o ())) (lvl1 q61) (conde [(q61 === q62) &&& (q71 === (!! true)); (q71 === (!! false)) &&& (q61 =/= q62)])
                (conde
                   [(q71 === (!! true)) &&& (q74 === (!! true));
                   fresh (q66 q67) (q71 === (!! false)) (lvl2 q66) (capacities b' q67) (conde [(q66 === q67) &&& (q74 === (!! true)); (q74 === (!! false)) &&& (q66 =/= q67)])])
                (conde [(q74 === (!! true)) &&& (q80 === (!! false)); (q74 === (!! false)) &&& (q80 === (!! true))])))]))
let doStep state0 step0 capacities q97 =
  fresh (q82 f s q84 t b) (q82 === (pair f s)) (q84 === (pair t b)) (
    state0 q82) (step0 q84)
    (let lvl2 q85 = fresh (q86) (b === q86) (((q86 === (fst_ ())) &&& (s === q85)) ||| ((q86 === (snd_ ())) &&& (f === q85))) in
     fresh (q88) (t === q88)
       (conde
          [(q88 === (fill ())) &&& (createState (fun q94 -> b === q94) (capacities (fun q94 -> b === q94)) lvl2 q97);
          (q88 === (empty ())) &&& (createState (fun q94 -> b === q94) (fun q89 -> q89 === (o ())) lvl2 q97);
          (q88 === (pour ())) &&&
            ((let sum = add (fun q95 -> f === q95) (fun q96 -> s === q96) in
              let cap2 = capacities (anotherBottle (fun q94 -> b === q94)) in
              fresh (q90) (greater sum cap2 q90)
                (conde
                   [(q90 === (!! true)) &&& (createState (fun q94 -> b === q94) (sub sum cap2) cap2 q97);
                   (q90 === (!! false)) &&& (createState (fun q94 -> b === q94) (fun q92 -> q92 === (o ())) sum q97)])))]))
let isFinishState state0 reqLvl q116 =
  fresh (q117 q99 f s q112 q102 q103) (q99 === (pair f s)) (f === q102) (
    q103 === q117) (reqLvl q117) (state0 q99) (conde [(q102 === q103) &&& (q112 === (!! true)); (q112 === (!! false)) &&& (q102 =/= q103)])
    (conde
       [(q112 === (!! true)) &&& (q116 === (!! true));
       fresh (q107 q108) (q112 === (!! false)) (s === q107) (q108 === q117) (conde [(q107 === q108) &&& (q116 === (!! true)); (q116 === (!! false)) &&& (q107 =/= q108)])])
let checkAnswer answer capacities reqLvl q130 =
  let rec checkAnswer state0 answer q126 =
    fresh (q127 q120) (state0 q127) (answer q120)
      (((q120 === (nil ())) &&& (isFinishState (fun q128 -> q128 === q127) reqLvl q126)) |||
         (fresh (x xs q121) (q120 === (x % xs)) (checkStep (fun q128 -> q128 === q127) (fun q124 -> x === q124) capacities q121)
            (conde
               [(q121 === (!! true)) &&& (checkAnswer (doStep (fun q128 -> q128 === q127) (fun q124 -> x === q124) capacities) (fun q125 -> xs === q125) q126);
               (q121 === (!! false)) &&& (q126 === (!! false))]))) in
  let startState q129 = q129 === (pair (o ()) (o ())) in checkAnswer startState answer q130
let capacities1 b q135 =
  fresh (q132) (b q132) (((q132 === (fst_ ())) &&& (q135 === (s (s (s (s (o ()))))))) ||| ((q132 === (snd_ ())) &&& (q135 === (s (s (s (s (s (s (s (s (s (o ())))))))))))))