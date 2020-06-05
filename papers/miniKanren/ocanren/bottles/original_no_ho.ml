open GT
open OCanren
open OCanren.Std
open Helper



let topLevel a b c = let rec add x y z = (((x === (o ())) &&& (z === y)) ||| (fresh (z' x') (((x === (s (x'))) &&& ((z === (s (z'))) &&& (add x' y z')))))) and greater a b q109 = (((a === (o ())) &&& (q109 === !!false)) ||| (fresh (x) (((a === (s (x))) &&& (((b === (o ())) &&& (q109 === !!true)) ||| (fresh (y) (((b === (s (y))) &&& (greater x y q109))))))))) and sub a b q105 = (((b === (o ())) &&& (a === q105)) ||| (fresh (y) (((b === (s (y))) &&& (((a === (o ())) &&& (q105 === (o ()))) ||| (fresh (x) (((a === (s (x))) &&& (sub x y q105))))))))) and anotherBottle b q102 = (((b === (fst_ ())) &&& (q102 === (snd_ ()))) ||| ((b === (snd_ ())) &&& (q102 === (fst_ ())))) and createState bottle lvl1 lvl2 q99 = (((bottle === (fst_ ())) &&& (q99 === (Pair.pair (lvl1) (lvl2)))) ||| ((bottle === (snd_ ())) &&& (q99 === (Pair.pair (lvl2) (lvl1))))) and fst' x q96 = (fresh (q97 a) (((x === (Pair.pair (a) (q97))) &&& (a === q96)))) and snd' x q93 = (fresh (a q94) (((x === (Pair.pair (q94) (a))) &&& (a === q93)))) and get_capacity capacities bottle q92 = (((bottle === (fst_ ())) &&& (fst' capacities q92)) ||| ((bottle === (snd_ ())) &&& (snd' capacities q92))) and fancyEq a b q85 = (((a === (o ())) &&& (((b === (o ())) &&& (q85 === !!true)) ||| (fresh (q88) (((b === (s (q88))) &&& (q85 === !!false)))))) ||| (fresh (x) (((a === (s (x))) &&& (((b === (o ())) &&& (q85 === !!false)) ||| (fresh (y) (((b === (s (y))) &&& (fancyEq x y q85))))))))) and checkStep state0 step0 capacities q48 = (fresh (s f) (((state0 === (Pair.pair (f) (s))) &&& (fresh (b t) (((step0 === (Pair.pair (t) (b))) &&& (((t === (fill ())) &&& (fresh (q51) (((((b === (fst_ ())) &&& (f === q51)) ||| ((b === (snd_ ())) &&& (s === q51))) &&& (fancyEq q51 ((o ())) q48))))) ||| (((t === (empty ())) &&& (fresh (q57 q56) (((((b === (fst_ ())) &&& (f === q56)) ||| ((b === (snd_ ())) &&& (s === q56))) &&& ((get_capacity capacities b q57) &&& (fancyEq q56 q57 q48)))))) ||| ((t === (pour ())) &&& (fresh (q62) (((fresh (q67 q66) (((fresh (q72) (((((b === (fst_ ())) &&& (f === q72)) ||| ((b === (snd_ ())) &&& (s === q72))) &&& (fancyEq q72 ((o ())) q66)))) &&& ((fresh (q78 q77) (((((b === (fst_ ())) &&& (s === q77)) ||| ((b === (snd_ ())) &&& (f === q77))) &&& ((fresh (q83) (((anotherBottle b q83) &&& (get_capacity capacities q83 q78)))) &&& (fancyEq q77 q78 q67))))) &&& (((q66 === !!true) &&& (q62 === !!true)) ||| ((q66 === !!false) &&& (q62 === q67))))))) &&& (((q62 === !!true) &&& (q48 === !!false)) ||| ((q62 === !!false) &&& (q48 === !!true))))))))))))))) and doStep state0 step0 capacities q15 = (fresh (s f) (((state0 === (Pair.pair (f) (s))) &&& (fresh (b t) (((step0 === (Pair.pair (t) (b))) &&& (((t === (fill ())) &&& (fresh (q19 q18) (((get_capacity capacities b q18) &&& ((((b === (fst_ ())) &&& (s === q19)) ||| ((b === (snd_ ())) &&& (f === q19))) &&& (createState b q18 q19 q15)))))) ||| (((t === (empty ())) &&& (fresh (q24) (((((b === (fst_ ())) &&& (s === q24)) ||| ((b === (snd_ ())) &&& (f === q24))) &&& (createState b ((o ())) q24 q15))))) ||| ((t === (pour ())) &&& (fresh (q30) (((fresh (q44 q43) (((add f s q43) &&& ((fresh (q46) (((anotherBottle b q46) &&& (get_capacity capacities q46 q44)))) &&& (greater q43 q44 q30))))) &&& (((q30 === !!true) &&& (fresh (q32 q31) (((fresh (q35 q34) (((add f s q34) &&& ((fresh (q37) (((anotherBottle b q37) &&& (get_capacity capacities q37 q35)))) &&& (sub q34 q35 q31))))) &&& ((fresh (q39) (((anotherBottle b q39) &&& (get_capacity capacities q39 q32)))) &&& (createState b q31 q32 q15)))))) ||| ((q30 === !!false) &&& (fresh (q41) (((add f s q41) &&& (createState b ((o ())) q41 q15)))))))))))))))))) and isFinishState state0 reqLvl q8 = (fresh (s f) (((state0 === (Pair.pair (f) (s))) &&& (fresh (q10 q9) (((fancyEq f reqLvl q9) &&& ((fancyEq s reqLvl q10) &&& (((q9 === !!true) &&& (q8 === !!true)) ||| ((q9 === !!false) &&& (q8 === q10)))))))))) and checkAnswer answer capacities reqLvl q7 = (checkAnswer' ((Pair.pair ((o ())) ((o ())))) answer capacities reqLvl q7) and checkAnswer' state0 answer capacities reqLvl q1 = (((answer === (Std.List.nil ())) &&& (isFinishState state0 reqLvl q1)) ||| (fresh (xs x) (((answer === (Std.(%) (x) (xs))) &&& (fresh (q3) (((checkStep state0 x capacities q3) &&& (((q3 === !!true) &&& (fresh (q4) (((doStep state0 x capacities q4) &&& (checkAnswer' q4 xs capacities reqLvl q1))))) ||| ((q3 === !!false) &&& (q1 === !!false)))))))))) and capacities1 q0 = (q0 === (Pair.pair ((s ((s ((s ((s ((o ())))))))))) ((s ((s ((s ((s ((s ((s ((s ((s ((s ((o ())))))))))))))))))))))) in                ( ((checkAnswer a b c !!true)))

(*
let rec add x y z = x === o () &&& (z === y) ||| fresh (z' x') (x === s x' &&& (z === s z' &&& add x' y z'))
and greater a b q109 =
  a === o () &&& (q109 === !!false) ||| fresh (x) (a === s x &&& (b === o () &&& (q109 === !!true) ||| fresh (y) (b === s y &&& greater x y q109)))
and sub a b q105 = b === o () &&& (a === q105) ||| fresh (y) (b === s y &&& (a === o () &&& (q105 === o ()) ||| fresh (x) (a === s x &&& sub x y q105)))
and anotherBottle b q102 = b === fst_ () &&& (q102 === snd_ ()) ||| (b === snd_ () &&& (q102 === fst_ ()))
and createState bottle lvl1 lvl2 q99 = bottle === fst_ () &&& (q99 === Pair.pair lvl1 lvl2) ||| (bottle === snd_ () &&& (q99 === Pair.pair lvl2 lvl1))
and fst' x q96 = fresh (q97 a) (x === Pair.pair a q97 &&& (a === q96))
and snd' x q93 = fresh (a q94) (x === Pair.pair q94 a &&& (a === q93))
and get_capacity capacities bottle q92 = bottle === fst_ () &&& fst' capacities q92 ||| (bottle === snd_ () &&& snd' capacities q92)
and fancyEq a b q85 =
  a === o ()
  &&& (b === o () &&& (q85 === !!true) ||| fresh (q88) (b === s q88 &&& (q85 === !!false)))
  ||| fresh (x) (a === s x &&& (b === o () &&& (q85 === !!false) ||| fresh (y) (b === s y &&& fancyEq x y q85)))
and checkStep state0 step0 capacities q48 =
  fresh (s f)
    ( state0 === Pair.pair f s
    &&& fresh (b t)
          ( step0 === Pair.pair t b
          &&& ( t === fill ()
              &&& fresh (q51) (b === fst_ () &&& (f === q51) ||| (b === snd_ () &&& (s === q51)) &&& fancyEq q51 (o ()) q48)
              ||| ( t === empty ()
                  &&& fresh (q57 q56)
                        (b === fst_ () &&& (f === q56) ||| (b === snd_ () &&& (s === q56)) &&& (get_capacity capacities b q57 &&& fancyEq q56 q57 q48))
                  ||| ( t === pour ()
                      &&& fresh (q62)
                            ( fresh (q67 q66)
                                ( fresh (q72) (b === fst_ () &&& (f === q72) ||| (b === snd_ () &&& (s === q72)) &&& fancyEq q72 (o ()) q66)
                                &&& ( fresh (q78 q77)
                                        (( b === fst_ () &&& (s === q77)
                                        ||| (b === snd_ () &&& (f === q77)))
                                        &&& (fresh (q83) (anotherBottle b q83 &&& get_capacity capacities q83 q78) &&& fancyEq q77 q78 q67) )
                                    &&& (q66 === !!true &&& (q62 === !!true) ||| (q66 === !!false &&& (q62 === q67))) ) )
                            &&& (q62 === !!true &&& (q48 === !!false) ||| (q62 === !!false &&& (q48 === !!true))) ) ) ) ) ) )
and doStep state0 step0 capacities q15 =
  fresh (s f)
    ( state0 === Pair.pair f s
    &&& fresh (b t)
          ( step0 === Pair.pair t b
          &&& ( t === fill ()
              &&& fresh (q19 q18)
                    (get_capacity capacities b q18 &&& (b === fst_ () &&& (s === q19) ||| (b === snd_ () &&& (f === q19)) &&& createState b q18 q19 q15))
              ||| ( t === empty ()
                  &&& fresh (q24) (b === fst_ () &&& (s === q24) ||| (b === snd_ () &&& (f === q24)) &&& createState b (o ()) q24 q15)
                  ||| ( t === pour ()
                      &&& fresh (q30)
                            ( fresh (q44 q43) (add f s q43 &&& (fresh (q46) (anotherBottle b q46 &&& get_capacity capacities q46 q44) &&& greater q43 q44 q30))
                            &&& ( q30 === !!true
                                &&& fresh (q32 q31)
                                      ( fresh (q35 q34)
                                          (add f s q34 &&& (fresh (q37) (anotherBottle b q37 &&& get_capacity capacities q37 q35) &&& sub q34 q35 q31))
                                      &&& (fresh (q39) (anotherBottle b q39 &&& get_capacity capacities q39 q32) &&& createState b q31 q32 q15) )
                                ||| (q30 === !!false &&& fresh (q41) (add f s q41 &&& createState b (o ()) q41 q15)) ) ) ) ) ) ) )
and isFinishState state0 reqLvl q8 =
  fresh (s f)
    ( state0 === Pair.pair f s
    &&& fresh (q10 q9) (fancyEq f reqLvl q9 &&& (fancyEq s reqLvl q10 &&& (q9 === !!true &&& (q8 === !!true) ||| (q9 === !!false &&& (q8 === q10))))) )
and checkAnswer answer capacities reqLvl q7 = checkAnswer1 (Pair.pair (o ()) (o ())) answer capacities reqLvl q7
and checkAnswer1 state0 answer capacities reqLvl q1 =
  answer === Std.List.nil () &&& isFinishState state0 reqLvl q1
  ||| fresh (xs x)
        ( answer === Std.( % ) x xs
        &&& fresh (q3)
              ( checkStep state0 x capacities q3
              &&& ( q3 === !!true
                  &&& fresh (q4) (doStep state0 x capacities q4 &&& checkAnswer1 q4 xs capacities reqLvl q1)
                  ||| (q3 === !!false &&& (q1 === !!false)) ) ) )

let topLevel a b c = checkAnswer a b c !!true
 *)
