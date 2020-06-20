open GT
open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 x3 =
  let rec add x y z = x === o () &&& (z === y) ||| fresh (z' x') (x === s x' &&& (z === s z' &&& add x' y z'))
  and goe a b q186 =
    a === o ()
    &&& (b === o () &&& (q186 === !!true) ||| fresh q189 (b === s q189 &&& (q186 === !!false)))
    ||| fresh x (a === s x &&& (b === o () &&& (q186 === !!true) ||| fresh y (b === s y &&& goe x y q186)))
  and sub a b q182 = b === o () &&& (a === q182) ||| fresh y (b === s y &&& (a === o () &&& (q182 === o ()) ||| fresh x (a === s x &&& sub x y q182)))
  and elem l n q179 = fresh (xs x) (l === Std.( % ) x xs &&& (n === o () &&& (x === q179) ||| fresh m (n === s m &&& elem xs m q179)))
  and eqNat a b q172 =
    a === o ()
    &&& (b === o () &&& (q172 === !!true) ||| fresh q175 (b === s q175 &&& (q172 === !!false)))
    ||| fresh x (a === s x &&& (b === o () &&& (q172 === !!false) ||| fresh y (b === s y &&& eqNat x y q172)))
  and checkStep step state len cop q85 =
    fresh (sts fuel pos)
      ( state === st pos fuel sts
      &&& ( fresh d
              ( step === left d
              &&& fresh (q88 q87)
                    ( goe pos d q87
                    &&& ( fresh (q94 q93)
                            ( goe fuel d q93
                            &&& ( fresh q99 (eqNat d (o ()) q99 &&& (q99 === !!true &&& (q94 === !!false) ||| (q99 === !!false &&& (q94 === !!true))))
                                &&& (q93 === !!false &&& (q88 === !!false) ||| (q93 === !!true &&& (q88 === q94))) ) )
                        &&& (q87 === !!false &&& (q85 === !!false) ||| (q87 === !!true &&& (q85 === q88))) ) ) )
          ||| ( fresh d
                  ( step === right d
                  &&& fresh (q104 q103)
                        ( fresh q109 (add pos d q109 &&& goe len q109 q103)
                        &&& ( fresh (q112 q111)
                                ( goe fuel d q111
                                &&& ( fresh q117 (eqNat d (o ()) q117 &&& (q117 === !!true &&& (q112 === !!false) ||| (q117 === !!false &&& (q112 === !!true))))
                                    &&& (q111 === !!false &&& (q104 === !!false) ||| (q111 === !!true &&& (q104 === q112))) ) )
                            &&& (q103 === !!false &&& (q85 === !!false) ||| (q103 === !!true &&& (q85 === q104))) ) ) )
              ||| ( fresh f
                      ( step === pour f
                      &&& fresh (q122 q121)
                            ( fresh q127 (eqNat pos len q127 &&& (q127 === !!true &&& (q121 === !!false) ||| (q127 === !!false &&& (q121 === !!true))))
                            &&& ( fresh (q132 q131)
                                    ( fresh q137
                                        (eqNat pos (o ()) q137 &&& (q137 === !!true &&& (q131 === !!false) ||| (q137 === !!false &&& (q131 === !!true))))
                                    &&& ( fresh (q142 q141)
                                            ( fresh q147
                                                (eqNat f (o ()) q147 &&& (q147 === !!true &&& (q141 === !!false) ||| (q147 === !!false &&& (q141 === !!true))))
                                            &&& (goe fuel f q142 &&& (q141 === !!false &&& (q132 === !!false) ||| (q141 === !!true &&& (q132 === q142)))) )
                                        &&& (q131 === !!false &&& (q122 === !!false) ||| (q131 === !!true &&& (q122 === q132))) ) )
                                &&& (q121 === !!false &&& (q85 === !!false) ||| (q121 === !!true &&& (q85 === q122))) ) ) )
                  ||| ( step === fill ()
                      &&& ( pos === o ()
                          &&& fresh q152 (eqNat fuel cop q152 &&& (q152 === !!true &&& (q85 === !!false) ||| (q152 === !!false &&& (q85 === !!true))))
                          ||| fresh x
                                ( pos === s x
                                &&& fresh (q157 q156)
                                      ( fresh q162
                                          (eqNat fuel cop q162 &&& (q162 === !!true &&& (q156 === !!false) ||| (q162 === !!false &&& (q156 === !!true))))
                                      &&& ( fresh q166
                                              ( fresh q170 (elem sts x q170 &&& eqNat q170 (o ()) q166)
                                              &&& (q166 === !!true &&& (q157 === !!false) ||| (q166 === !!false &&& (q157 === !!true))) )
                                          &&& (q156 === !!false &&& (q85 === !!false) ||| (q156 === !!true &&& (q85 === q157))) ) ) ) ) ) ) ) ) )
  and addForElem l n v q79 =
    fresh (xs x)
      ( l === Std.( % ) x xs
      &&& ( n === o ()
          &&& fresh q81 (q79 === Std.( % ) q81 xs &&& add v x q81)
          ||| fresh m (n === s m &&& fresh q83 (q79 === Std.( % ) x q83 &&& addForElem xs m v q83)) ) )
  and setForElem l n v q74 =
    fresh (xs x)
      ( l === Std.( % ) x xs
      &&& (n === o () &&& (q74 === Std.( % ) v xs) ||| fresh m (n === s m &&& fresh q77 (q74 === Std.( % ) x q77 &&& addForElem xs m v q77))) )
  and step step state len cop q50 =
    fresh (sts fuel pos)
      ( state === st pos fuel sts
      &&& ( fresh d (step === left d &&& fresh (q53 q52) (q50 === st q52 q53 sts &&& (sub pos d q52 &&& sub fuel d q53)))
          ||| ( fresh d (step === right d &&& fresh (q56 q55) (q50 === st q55 q56 sts &&& (add pos d q55 &&& sub fuel d q56)))
              ||| ( fresh f
                      (step === pour f &&& fresh x (pos === s x &&& fresh (q60 q59) (q50 === st pos q59 q60 &&& (sub fuel f q59 &&& addForElem sts x f q60))))
                  ||| ( step === fill ()
                      &&& ( pos === o ()
                          &&& (q50 === st pos cop sts)
                          ||| fresh x
                                ( pos === s x
                                &&& fresh stationFuel
                                      ( elem sts x stationFuel
                                      &&& fresh totalFuel
                                            ( add fuel stationFuel totalFuel
                                            &&& fresh q67
                                                  ( goe totalFuel cop q67
                                                  &&& ( q67 === !!true
                                                      &&& fresh q68 (q50 === st pos cop q68 &&& fresh q70 (sub totalFuel cop q70 &&& setForElem sts x q70 q68))
                                                      ||| (q67 === !!false &&& fresh q72 (q50 === st pos totalFuel q72 &&& setForElem sts x (o ()) q72)) ) ) )
                                      ) ) ) ) ) ) ) )
  and isFinishState state len q49 = fresh (sts fuel pos) (state === st pos fuel sts &&& eqNat pos len q49)
  and getFuel step state cop q42 =
    fresh d (step === left d &&& (q42 === o ()))
    ||| ( fresh d (step === right d &&& (q42 === o ()))
        ||| ( fresh f (step === pour f &&& (q42 === o ()))
            ||| ( step === fill ()
                &&& fresh (sts fuel pos) (state === st pos fuel sts &&& (pos === o () &&& sub cop fuel q42 ||| fresh x (pos === s x &&& (q42 === o ())))) ) )
        )
  and isMove step q34 =
    fresh q35 (step === left q35 &&& (q34 === !!true))
    ||| ( fresh q37 (step === right q37 &&& (q34 === !!true))
        ||| (step === fill () &&& (q34 === !!false) ||| fresh q40 (step === pour q40 &&& (q34 === !!false))) )
  and eqBool a b q30 = a === !!true &&& (b === q30) ||| (a === !!false &&& (b === !!true &&& (q30 === !!false) ||| (b === !!false &&& (q30 === !!true))))
  and startState len cop q29 = fresh q27 (q29 === st (o ()) cop q27 &&& stations len q27)
  and stations n q23 = n === o () &&& (q23 === Std.List.nil ()) ||| fresh m (n === s m &&& fresh q25 (q23 === Std.( % ) (o ()) q25 &&& stations m q25))
  and calcFuel state ans len cop prevIsMove q2 =
    ans === Std.List.nil ()
    &&& fresh q4 (isFinishState state len q4 &&& (q4 === !!true &&& (q2 === Option.some cop) ||| (q4 === !!false &&& (q2 === Option.none ()))))
    ||| fresh (xs x)
          ( ans === Std.( % ) x xs
          &&& fresh currIsMove
                ( isMove x currIsMove
                &&& fresh q9
                      ( eqBool prevIsMove currIsMove q9
                      &&& ( q9 === !!true
                          &&& (q2 === Option.none ())
                          ||| ( q9 === !!false
                              &&& fresh q12
                                    ( checkStep x state len cop q12
                                    &&& ( q12 === !!true
                                        &&& fresh q14
                                              ( fresh q20 (step x state len cop q20 &&& calcFuel q20 xs len cop currIsMove q14)
                                              &&& ( q14 === Option.none ()
                                                  &&& (q2 === Option.none ())
                                                  ||| fresh res
                                                        ( q14 === Option.some res
                                                        &&& fresh q16 (q2 === Option.some q16 &&& fresh q18 (getFuel x state cop q18 &&& add q18 res q16)) ) )
                                              )
                                        ||| (q12 === !!false &&& (q2 === Option.none ())) ) ) ) ) ) ) )
  and checkAnswer answer len cop q1 = fresh q0 (startState len cop q0 &&& calcFuel q0 answer len cop !!false q1) in
  fresh (d c b a) (checkAnswer a b c (Option.some d))
