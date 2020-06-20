open GT
open OCanren
open OCanren.Std
type 'a0 gnat =
  | O 
  | S of 'a0 
module For_gnat = (Fmap)(struct let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)
                                type 'a0 t = 'a0 gnat end)
let rec o () = inj (For_gnat.distrib O)
and s x__0 = inj (For_gnat.distrib (S x__0))
type 'a0 gstep =
  | Left of 'a0 
  | Right of 'a0 
  | Fill 
  | Pour of 'a0 
module For_gstep =
  (Fmap)(struct let rec fmap fa0 = function | Left a0 -> Left (fa0 a0) | Right a0 -> Right (fa0 a0) | Fill -> Fill | Pour a0 -> Pour (fa0 a0)
                type 'a0 t = 'a0 gstep end)
let rec left x__0 = inj (For_gstep.distrib (Left x__0))
and right x__0 = inj (For_gstep.distrib (Right x__0))
and fill () = inj (For_gstep.distrib Fill)
and pour x__0 = inj (For_gstep.distrib (Pour x__0))
type ('a1, 'a0) gstate =
  | St of 'a1 * 'a1 * 'a0 
module For_gstate = (Fmap2)(struct let rec fmap fa1 fa0 = function | St (a1_0, a1_1, a0_2) -> St ((fa1 a1_0), (fa1 a1_1), (fa0 a0_2))
                                   type ('a1, 'a0) t = ('a1, 'a0) gstate end)
let rec st x__0 x__1 x__2 = inj (For_gstate.distrib (St (x__0, x__1, x__2)))
let rec (|+|) a b q5 =
  fresh (q1) (a q1) (((q1 === (o ())) &&& (b q5)) ||| (fresh (x) (q1 === (s x)) ((|+|) (fun q4 -> x === q4) (fun q3 -> fresh (q2) (q3 === (s q2)) (b q2)) q5)))
let rec (|>=|) a b q19 =
  fresh (q7) (a q7)
    ((fresh (q11 q12) (q7 === (o ())) (q12 === (o ())) (b q11) (conde [(q11 === q12) &&& (q19 === (!! true)); (q19 === (!! false)) &&& (q11 =/= q12)])) |||
       (fresh (x q15) (q7 === (s x)) (b q15)
          (((q15 === (o ())) &&& (q19 === (!! true))) ||| (fresh (y) (q15 === (s y)) ((|>=|) (fun q18 -> x === q18) (fun q17 -> y === q17) q19)))))
let rec (|-|) a b q27 =
  fresh (q21) (b q21)
    (((q21 === (o ())) &&& (a q27)) |||
       (fresh (y q23) (q21 === (s y)) (a q23) (((q23 === (o ())) &&& (q27 === (o ()))) ||| (fresh (x) (q23 === (s x)) ((|-|) (fun q25 -> x === q25) (fun q26 -> y === q26) q27)))))
let rec elem l n q35 =
  fresh (q29 x xs q31) (q29 === (x % xs)) (l q29) (n q31)
    (((q31 === (o ())) &&& (x === q35)) ||| (fresh (m) (q31 === (s m)) (elem (fun q34 -> xs === q34) (fun q32 -> m === q32) q35)))
let rec changeElem l n f q49 =
  fresh (q37 x xs q39) (q37 === (x % xs)) (l q37) (n q39)
    ((fresh (q40 q41) (q39 === (o ())) (q49 === (q40 % q41)) (xs === q41) (f (fun q47 -> x === q47) q40)) |||
       (fresh (m q43 q44) (q39 === (s m)) (q49 === (q43 % q44)) (x === q43) (changeElem (fun q48 -> xs === q48) (fun q46 -> m === q46) f q44)))
let checkStep step state len cop q140 =
  fresh (q51 pos fuel sts q53) (q51 === (st pos fuel sts)) (state q51) (
    step q53)
    (conde
       [fresh (d q66) (q53 === (left d)) ((|>=|) (fun q137 -> pos === q137) (fun q68 -> d === q68) q66)
          (conde
             [(q66 === (!! false)) &&& (q140 === (!! false));
             fresh (q62) (q66 === (!! true)) ((|>=|) (fun q138 -> fuel === q138) (fun q68 -> d === q68) q62)
               (conde
                  [(q62 === (!! false)) &&& (q140 === (!! false));
                  fresh (q57 q58) (q62 === (!! true)) (d === q57) (q58 === (o ())) (conde [(q57 === q58) &&& (q140 === (!! false)); (q140 === (!! true)) &&& (q57 =/= q58)])])]);
       fresh (d q81) (q53 === (right d)) ((|>=|) len ((fun q137 -> pos === q137) |+| (fun q83 -> d === q83)) q81)
         (conde
            [(q81 === (!! false)) &&& (q140 === (!! false));
            fresh (q77) (q81 === (!! true)) ((|>=|) (fun q138 -> fuel === q138) (fun q83 -> d === q83) q77)
              (conde
                 [(q77 === (!! false)) &&& (q140 === (!! false));
                 fresh (q72 q73) (q77 === (!! true)) (d === q72) (q73 === (o ())) (conde [(q72 === q73) &&& (q140 === (!! false)); (q140 === (!! true)) &&& (q72 =/= q73)])])]);
       fresh (f q111 q86 q87) (q53 === (pour f)) (pos === q86) (len q87) (
         conde [(q86 === q87) &&& (q111 === (!! false)); (q111 === (!! true)) &&& (q86 =/= q87)])
         (conde
            [(q111 === (!! false)) &&& (q140 === (!! false));
            fresh (q107 q92 q93) (q111 === (!! true)) (pos === q92) (
              q93 === (o ())) (conde [(q92 === q93) &&& (q107 === (!! false)); (q107 === (!! true)) &&& (q92 =/= q93)])
              (conde
                 [(q107 === (!! false)) &&& (q140 === (!! false));
                 fresh (q103 q98 q99) (q107 === (!! true)) (f === q98) (
                   q99 === (o ())) (conde [(q98 === q99) &&& (q103 === (!! false)); (q103 === (!! true)) &&& (q98 =/= q99)])
                   (conde [(q103 === (!! false)) &&& (q140 === (!! false)); (q103 === (!! true)) &&& ((|>=|) ((fun q138 -> fuel === q138)) ((fun q113 -> f === q113)) q140)])])]);
       fresh (q115) (q53 === (fill ())) (pos === q115)
         ((fresh (q118 q119) (q115 === (o ())) (fuel === q118) (cop q119) (conde [(q118 === q119) &&& (q140 === (!! false)); (q140 === (!! true)) &&& (q118 =/= q119)])) |||
            (fresh (x q134 q123 q124) (q115 === (s x)) (fuel === q123) (
               cop q124) (conde [(q123 === q124) &&& (q134 === (!! false)); (q134 === (!! true)) &&& (q123 =/= q124)])
               (conde
                  [(q134 === (!! false)) &&& (q140 === (!! false));
                  fresh (q129 q130) (q134 === (!! true)) (q130 === (o ())) (
                    elem (fun q139 -> sts === q139) (fun q136 -> x === q136) q129) (
                    conde [(q129 === q130) &&& (q140 === (!! false)); (q140 === (!! true)) &&& (q129 =/= q130)])])))])
let step step state len cop q187 =
  fresh (q188 q142 pos fuel sts q144) (q142 === (st pos fuel sts)) (cop q188) (
    state q142) (step q144)
    (conde
       [fresh (d q145 q146 q147) (q144 === (left d)) (q187 === (st q145 q146 q147)) (
          sts === q147) ((|-|) (fun q184 -> pos === q184) (fun q149 -> d === q149) q145) (
          (|-|) (fun q185 -> fuel === q185) (fun q149 -> d === q149) q146);
       fresh (d q150 q151 q152) (q144 === (right d)) (q187 === (st q150 q151 q152)) (
         sts === q152) ((|+|) (fun q184 -> pos === q184) (fun q154 -> d === q154) q150) (
         (|-|) (fun q185 -> fuel === q185) (fun q154 -> d === q154) q151);
       fresh (f q156 x q157 q158 q159) (q144 === (pour f)) (pos === q156) (
         q156 === (s x)) (q187 === (st q157 q158 q159)) (pos === q157) (
         (|-|) (fun q185 -> fuel === q185) (fun q163 -> f === q163) q158)
         (changeElem (fun q186 -> sts === q186) (fun q162 -> x === q162) (fun e -> fun q161 -> (|+|) (fun q163 -> f === q163) e q161) q159);
       fresh (q165) (q144 === (fill ())) (pos === q165)
         ((fresh (q166 q167 q168) (q165 === (o ())) (q187 === (st q166 q167 q168)) (pos === q166) (q167 === q188) (sts === q168)) |||
            (fresh (x) (q165 === (s x))
               (let stationFuel = elem (fun q186 -> sts === q186) (fun q183 -> x === q183) in
                let totalFuel = (fun q185 -> fuel === q185) |+| stationFuel in
                fresh (q170) ((|>=|) totalFuel (fun q189 -> q189 === q188) q170)
                  (conde
                     [fresh (q178 q179 q180) (q170 === (!! true)) (q187 === (st q178 q179 q180)) (
                        pos === q178) (q179 === q188)
                        (changeElem (fun q186 -> sts === q186) (fun q183 -> x === q183) (fun e -> fun q182 -> (|-|) totalFuel (fun q189 -> q189 === q188) q182) q180);
                     fresh (q172 q173 q174) (q170 === (!! false)) (q187 === (st q172 q173 q174)) (
                       pos === q172) (totalFuel q173) (changeElem (fun q186 -> sts === q186) (fun q183 -> x === q183) (fun e -> fun q177 -> q177 === (o ())) q174)]))))])
let isFinishState state len q200 =
  fresh (q191 pos fuel sts q194 q195) (q191 === (st pos fuel sts)) (pos === q194) (
    state q191) (len q195) (conde [(q194 === q195) &&& (q200 === (!! true)); (q200 === (!! false)) &&& (q194 =/= q195)])
let getFuel step state cop q218 =
  fresh (q202) (step q202)
    (conde
       [fresh (d) (q202 === (left d)) (q218 === (o ()));
       fresh (d) (q202 === (right d)) (q218 === (o ()));
       fresh (f) (q202 === (pour f)) (q218 === (o ()));
       fresh (q210 pos fuel sts q212) (q202 === (fill ())) (q210 === (st pos fuel sts)) (
         pos === q212) (state q210) (((q212 === (o ())) &&& ((|-|) cop (fun q216 -> fuel === q216) q218)) ||| (fresh (x) (q212 === (s x)) (q218 === (o ()))))])
let isMove step q228 =
  fresh (q220) (step q220)
    (conde
       [fresh (x) (q220 === (left x)) (q228 === (!! true));
       fresh (x) (q220 === (right x)) (q228 === (!! true));
       (q220 === (fill ())) &&& (q228 === (!! false));
       fresh (x) (q220 === (pour x)) (q228 === (!! false))])
let checkAnswer answer len cop q269 =
  fresh (q270 q271) (len q270) (cop q271)
    (let rec calcFuel state ans prevIsMove q255 =
       fresh (q256 q230) (state q256) (ans q230)
         ((fresh (q231) (q230 === (nil ())) (isFinishState (fun q257 -> q257 === q256) (fun q272 -> q272 === q270) q231)
             (conde [fresh (q234) (q231 === (!! true)) (q255 === (some q234)) (q234 === q271); (q231 === (!! false)) &&& (q255 === (none ()))]))
            |||
            (fresh (x xs) (q230 === (x % xs))
               (let currIsMove = isMove (fun q253 -> x === q253) in
                fresh (q236 q250 q251) (prevIsMove q250) (currIsMove q251) (
                  conde [(q250 === q251) &&& (q236 === (!! true)); (q236 === (!! false)) &&& (q250 =/= q251)])
                  (conde
                     [(q236 === (!! true)) &&& (q255 === (none ()));
                     fresh (q238) (q236 === (!! false))
                       (checkStep (fun q253 -> x === q253) (fun q257 -> q257 === q256) (fun q272 -> q272 === q270) (fun q273 -> q273 === q271) q238)
                       (conde
                          [fresh (q242) (q238 === (!! true))
                             (calcFuel (step (fun q253 -> x === q253) (fun q257 -> q257 === q256) (fun q272 -> q272 === q270) (fun q273 -> q273 === q271))
                                (fun q254 -> xs === q254) currIsMove q242)
                             (((q242 === (none ())) &&& (q255 === (none ()))) |||
                                (fresh (res q244) (q242 === (some res)) (
                                   q255 === (some q244))
                                   ((|+|) (getFuel (fun q253 -> x === q253) (fun q257 -> q257 === q256) (fun q273 -> q273 === q271)) (fun q246 -> res === q246) q244)));
                          (q238 === (!! false)) &&& (q255 === (none ()))])])))) in
     let startState =
       let rec stations n q264 =
         fresh (q259) (n q259)
           (((q259 === (o ())) &&& (q264 === (nil ()))) ||| (fresh (m q261) (q259 === (s m)) (q264 === ((o ()) % q261)) (stations (fun q263 -> m === q263) q261))) in
       fun q267 -> fresh (q265 q266) (q267 === (st (o ()) q265 q266)) (q265 === q271) (stations (fun q272 -> q272 === q270) q266) in
     calcFuel startState answer (fun q268 -> q268 === (!! false)) q269)