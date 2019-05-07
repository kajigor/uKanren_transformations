module Desert where

import Syntax

some x = C "some" [x]

ofInt = peanify
  where
    peanify n | n <= 0 = zero
    peanify n          = succ (peanify $ n - 1)

    zero = C "o" []
    succ x = C "s" [x]

query = fst tree $ fresh ["q"] (call "checkAnswer" [V "q", ofInt 8, ofInt 5, some $ ofInt 22])
query' = fst tree $ fresh ["a", "b"] (call "checkAnswer" [V "a", ofInt 6, ofInt 4, some $ V "b"])
query'' = fst tree $ fresh ["a", "b"] (call "checkAnswer" [V "a", ofInt 5, ofInt 4, some $ V "b"])

env = snd tree

tree =
 (\last_goal ->
  Let (def "add" ["a", "b", "q193"] (
    ((V "a" === C "o" []) &&&
    (V "b" === V "q193")) |||
    (fresh ["x"] (
       (V "a" === C "s" [V "x"]) &&&
       (call "add" [V "x", C "s" [V "b"], V "q193"])))
  )) (
  Let (def "goe" ["a", "b", "q186"] (
    ((V "a" === C "o" []) &&&
    (((V "b" === C "o" []) &&&
    (V "q186" === C "true" [])) |||
    (fresh ["q189"] (
       (V "b" === C "s" [V "q189"]) &&&
       (V "q186" === C "false" []))))) |||
    (fresh ["x"] (
       (V "a" === C "s" [V "x"]) &&&
       (((V "b" === C "o" []) &&&
       (V "q186" === C "true" [])) |||
       (fresh ["y"] (
          (V "b" === C "s" [V "y"]) &&&
          (call "goe" [V "x", V "y", V "q186"]))))))
  )) (
  Let (def "sub" ["a", "b", "q182"] (
    ((V "b" === C "o" []) &&&
    (V "a" === V "q182")) |||
    (fresh ["y"] (
       (V "b" === C "s" [V "y"]) &&&
       (((V "a" === C "o" []) &&&
       (V "q182" === C "o" [])) |||
       (fresh ["x"] (
          (V "a" === C "s" [V "x"]) &&&
          (call "sub" [V "x", V "y", V "q182"]))))))
  )) (
  Let (def "elem" ["l", "n", "q179"] (
    fresh ["x", "xs"] (
      (V "l" === C "%" [V "x", V "xs"]) &&&
      (((V "n" === C "o" []) &&&
      (V "x" === V "q179")) |||
      (fresh ["m"] (
         (V "n" === C "s" [V "m"]) &&&
         (call "elem" [V "xs", V "m", V "q179"])))))
  )) (
  Let (def "eqNat" ["a", "b", "q172"] (
    ((V "a" === C "o" []) &&&
    (((V "b" === C "o" []) &&&
    (V "q172" === C "true" [])) |||
    (fresh ["q175"] (
       (V "b" === C "s" [V "q175"]) &&&
       (V "q172" === C "false" []))))) |||
    (fresh ["x"] (
       (V "a" === C "s" [V "x"]) &&&
       (((V "b" === C "o" []) &&&
       (V "q172" === C "false" [])) |||
       (fresh ["y"] (
          (V "b" === C "s" [V "y"]) &&&
          (call "eqNat" [V "x", V "y", V "q172"]))))))
  )) (
  Let (def "checkStep" ["step", "state", "len", "cop", "q85"] (
    fresh ["pos", "fuel", "sts"] (
      (V "state" === C "st" [V "pos", V "fuel", V "sts"]) &&&
      ((fresh ["d"] (
          (V "step" === C "left" [V "d"]) &&&
          (fresh ["q87", "q88"] (
             (call "goe" [V "pos", V "d", V "q87"]) &&&
             (fresh ["q93", "q94"] (
                (call "goe" [V "fuel", V "d", V "q93"]) &&&
                (fresh ["q99"] (
                   (call "eqNat" [V "d", C "o" [], V "q99"]) &&&
                   (((V "q99" === C "true" []) &&&
                   (V "q94" === C "false" [])) |||
                   ((V "q99" === C "false" []) &&&
                   (V "q94" === C "true" []))))) &&&
                (((V "q93" === C "false" []) &&&
                (V "q88" === C "false" [])) |||
                ((V "q93" === C "true" []) &&&
                (V "q88" === V "q94"))))) &&&
             (((V "q87" === C "false" []) &&&
             (V "q85" === C "false" [])) |||
             ((V "q87" === C "true" []) &&&
             (V "q85" === V "q88"))))))) |||
      (fresh ["d"] (
         (V "step" === C "right" [V "d"]) &&&
         (fresh ["q103", "q104"] (
            (fresh ["q109"] (
               (call "add" [V "pos", V "d", V "q109"]) &&&
               (call "goe" [V "len", V "q109", V "q103"]))) &&&
            (fresh ["q111", "q112"] (
               (call "goe" [V "fuel", V "d", V "q111"]) &&&
               (fresh ["q117"] (
                  (call "eqNat" [V "d", C "o" [], V "q117"]) &&&
                  (((V "q117" === C "true" []) &&&
                  (V "q112" === C "false" [])) |||
                  ((V "q117" === C "false" []) &&&
                  (V "q112" === C "true" []))))) &&&
               (((V "q111" === C "false" []) &&&
               (V "q104" === C "false" [])) |||
               ((V "q111" === C "true" []) &&&
               (V "q104" === V "q112"))))) &&&
            (((V "q103" === C "false" []) &&&
            (V "q85" === C "false" [])) |||
            ((V "q103" === C "true" []) &&&
            (V "q85" === V "q104"))))))) |||
      (fresh ["f"] (
         (V "step" === C "pour" [V "f"]) &&&
         (fresh ["q121", "q122"] (
            (fresh ["q127"] (
               (call "eqNat" [V "pos", V "len", V "q127"]) &&&
               (((V "q127" === C "true" []) &&&
               (V "q121" === C "false" [])) |||
               ((V "q127" === C "false" []) &&&
               (V "q121" === C "true" []))))) &&&
            (fresh ["q131", "q132"] (
               (fresh ["q137"] (
                  (call "eqNat" [V "pos", C "o" [], V "q137"]) &&&
                  (((V "q137" === C "true" []) &&&
                  (V "q131" === C "false" [])) |||
                  ((V "q137" === C "false" []) &&&
                  (V "q131" === C "true" []))))) &&&
               (fresh ["q141", "q142"] (
                  (fresh ["q147"] (
                     (call "eqNat" [V "f", C "o" [], V "q147"]) &&&
                     (((V "q147" === C "true" []) &&&
                     (V "q141" === C "false" [])) |||
                     ((V "q147" === C "false" []) &&&
                     (V "q141" === C "true" []))))) &&&
                  (call "goe" [V "fuel", V "f", V "q142"]) &&&
                  (((V "q141" === C "false" []) &&&
                  (V "q132" === C "false" [])) |||
                  ((V "q141" === C "true" []) &&&
                  (V "q132" === V "q142"))))) &&&
               (((V "q131" === C "false" []) &&&
               (V "q122" === C "false" [])) |||
               ((V "q131" === C "true" []) &&&
               (V "q122" === V "q132"))))) &&&
            (((V "q121" === C "false" []) &&&
            (V "q85" === C "false" [])) |||
            ((V "q121" === C "true" []) &&&
            (V "q85" === V "q122"))))))) |||
      ((V "step" === C "fill" []) &&&
      (((V "pos" === C "o" []) &&&
      (fresh ["q152"] (
         (call "eqNat" [V "fuel", V "cop", V "q152"]) &&&
         (((V "q152" === C "true" []) &&&
         (V "q85" === C "false" [])) |||
         ((V "q152" === C "false" []) &&&
         (V "q85" === C "true" [])))))) |||
      (fresh ["x"] (
         (V "pos" === C "s" [V "x"]) &&&
         (fresh ["q156", "q157"] (
            (fresh ["q162"] (
               (call "eqNat" [V "fuel", V "cop", V "q162"]) &&&
               (((V "q162" === C "true" []) &&&
               (V "q156" === C "false" [])) |||
               ((V "q162" === C "false" []) &&&
               (V "q156" === C "true" []))))) &&&
            (fresh ["q166"] (
               (fresh ["q170"] (
                  (call "elem" [V "sts", V "x", V "q170"]) &&&
                  (call "eqNat" [V "q170", C "o" [], V "q166"]))) &&&
               (((V "q166" === C "true" []) &&&
               (V "q157" === C "false" [])) |||
               ((V "q166" === C "false" []) &&&
               (V "q157" === C "true" []))))) &&&
            (((V "q156" === C "false" []) &&&
            (V "q85" === C "false" [])) |||
            ((V "q156" === C "true" []) &&&
            (V "q85" === V "q157")))))))))))
  )) (
  Let (def "addForElem" ["l", "n", "v", "q79"] (
    fresh ["x", "xs"] (
      (V "l" === C "%" [V "x", V "xs"]) &&&
      (((V "n" === C "o" []) &&&
      (fresh ["q81"] (
         (V "q79" === C "%" [V "q81", V "xs"]) &&&
         (call "add" [V "v", V "x", V "q81"])))) |||
      (fresh ["m"] (
         (V "n" === C "s" [V "m"]) &&&
         (fresh ["q83"] (
            (V "q79" === C "%" [V "x", V "q83"]) &&&
            (call "addForElem" [V "xs", V "m", V "v", V "q83"])))))))
  )) (
  Let (def "setForElem" ["l", "n", "v", "q74"] (
    fresh ["x", "xs"] (
      (V "l" === C "%" [V "x", V "xs"]) &&&
      (((V "n" === C "o" []) &&&
      (V "q74" === C "%" [V "v", V "xs"])) |||
      (fresh ["m"] (
         (V "n" === C "s" [V "m"]) &&&
         (fresh ["q77"] (
            (V "q74" === C "%" [V "x", V "q77"]) &&&
            (call "addForElem" [V "xs", V "m", V "v", V "q77"])))))))
  )) (
  Let (def "step" ["step", "state", "len", "cop", "q50"] (
    fresh ["pos", "fuel", "sts"] (
      (V "state" === C "st" [V "pos", V "fuel", V "sts"]) &&&
      ((fresh ["d"] (
          (V "step" === C "left" [V "d"]) &&&
          (fresh ["q52", "q53"] (
             (V "q50" === C "st" [V "q52", V "q53", V "sts"]) &&&
             (call "sub" [V "pos", V "d", V "q52"]) &&&
             (call "sub" [V "fuel", V "d", V "q53"]))))) |||
      (fresh ["d"] (
         (V "step" === C "right" [V "d"]) &&&
         (fresh ["q55", "q56"] (
            (V "q50" === C "st" [V "q55", V "q56", V "sts"]) &&&
            (call "add" [V "pos", V "d", V "q55"]) &&&
            (call "sub" [V "fuel", V "d", V "q56"]))))) |||
      (fresh ["f"] (
         (V "step" === C "pour" [V "f"]) &&&
         (fresh ["x"] (
            (V "pos" === C "s" [V "x"]) &&&
            (fresh ["q59", "q60"] (
               (V "q50" === C "st" [V "pos", V "q59", V "q60"]) &&&
               (call "sub" [V "fuel", V "f", V "q59"]) &&&
               (call "addForElem" [V "sts", V "x", V "f", V "q60"]))))))) |||
      ((V "step" === C "fill" []) &&&
      (((V "pos" === C "o" []) &&&
      (V "q50" === C "st" [V "pos", V "cop", V "sts"])) |||
      (fresh ["x"] (
         (V "pos" === C "s" [V "x"]) &&&
         (fresh ["stationFuel"] (
            (call "elem" [V "sts", V "x", V "stationFuel"]) &&&
            (fresh ["totalFuel"] (
               (call "add" [V "fuel", V "stationFuel", V "totalFuel"]) &&&
               (fresh ["q67"] (
                  (call "goe" [V "totalFuel", V "cop", V "q67"]) &&&
                  (((V "q67" === C "true" []) &&&
                  (fresh ["q68"] (
                     (V "q50" === C "st" [V "pos", V "cop", V "q68"]) &&&
                     (fresh ["q70"] (
                        (call "sub" [V "totalFuel", V "cop", V "q70"]) &&&
                        (call "setForElem" [V "sts", V "x", V "q70", V "q68"])))))) |||
                  ((V "q67" === C "false" []) &&&
                  (fresh ["q72"] (
                     (V "q50" === C "st" [V "pos", V "totalFuel", V "q72"]) &&&
                     (call "setForElem" [V "sts", V "x", C "o" [], V "q72"])))))))))))))))))
  )) (
  Let (def "isFinishState" ["state", "len", "q49"] (
    fresh ["pos", "fuel", "sts"] (
      (V "state" === C "st" [V "pos", V "fuel", V "sts"]) &&&
      (call "eqNat" [V "pos", V "len", V "q49"]))
  )) (
  Let (def "getFuel" ["step", "state", "cop", "q42"] (
    (fresh ["d"] (
       (V "step" === C "left" [V "d"]) &&&
       (V "q42" === C "o" []))) |||
    (fresh ["d"] (
       (V "step" === C "right" [V "d"]) &&&
       (V "q42" === C "o" []))) |||
    (fresh ["f"] (
       (V "step" === C "pour" [V "f"]) &&&
       (V "q42" === C "o" []))) |||
    ((V "step" === C "fill" []) &&&
    (fresh ["pos", "fuel", "sts"] (
       (V "state" === C "st" [V "pos", V "fuel", V "sts"]) &&&
       (((V "pos" === C "o" []) &&&
       (call "sub" [V "cop", V "fuel", V "q42"])) |||
       (fresh ["x"] (
          (V "pos" === C "s" [V "x"]) &&&
          (V "q42" === C "o" [])))))))
  )) (
  Let (def "isMove" ["step", "q34"] (
    (fresh ["q35"] (
       (V "step" === C "left" [V "q35"]) &&&
       (V "q34" === C "true" []))) |||
    (fresh ["q37"] (
       (V "step" === C "right" [V "q37"]) &&&
       (V "q34" === C "true" []))) |||
    ((V "step" === C "fill" []) &&&
    (V "q34" === C "false" [])) |||
    (fresh ["q40"] (
       (V "step" === C "pour" [V "q40"]) &&&
       (V "q34" === C "false" [])))
  )) (
  Let (def "eqBool" ["a", "b", "q30"] (
    ((V "a" === C "true" []) &&&
    (V "b" === V "q30")) |||
    ((V "a" === C "false" []) &&&
    (((V "b" === C "true" []) &&&
    (V "q30" === C "false" [])) |||
    ((V "b" === C "false" []) &&&
    (V "q30" === C "true" []))))
  )) (
  Let (def "startState" ["len", "cop", "q29"] (
    Let (def "stations" ["n", "q23"] (
      ((V "n" === C "o" []) &&&
      (V "q23" === C "nil" [])) |||
      (fresh ["m"] (
         (V "n" === C "s" [V "m"]) &&&
         (fresh ["q25"] (
            (V "q23" === C "%" [C "o" [], V "q25"]) &&&
            (call "stations" [V "m", V "q25"])))))
    )) (
    fresh ["q27"] (
      (V "q29" === C "st" [C "o" [], V "cop", V "q27"]) &&&
      (call "stations" [V "len", V "q27"])))
  )) (
  Let (def "calcFuel" ["state", "ans", "len", "cop", "prevIsMove", "q2"] (
    ((V "ans" === C "nil" []) &&&
    (fresh ["q4"] (
       (call "isFinishState" [V "state", V "len", V "q4"]) &&&
       (((V "q4" === C "true" []) &&&
       (V "q2" === C "some" [V "cop"])) |||
       ((V "q4" === C "false" []) &&&
       (V "q2" === C "none" [])))))) |||
    (fresh ["x", "xs"] (
       (V "ans" === C "%" [V "x", V "xs"]) &&&
       (fresh ["currIsMove"] (
          (call "isMove" [V "x", V "currIsMove"]) &&&
          (fresh ["q9"] (
             (call "eqBool" [V "prevIsMove", V "currIsMove", V "q9"]) &&&
             (((V "q9" === C "true" []) &&&
             (V "q2" === C "none" [])) |||
             ((V "q9" === C "false" []) &&&
             (fresh ["q12"] (
                (call "checkStep" [V "x", V "state", V "len", V "cop", V "q12"]) &&&
                (((V "q12" === C "true" []) &&&
                (fresh ["q14"] (
                   (fresh ["q20"] (
                      (call "step" [V "x", V "state", V "len", V "cop", V "q20"]) &&&
                      (call "calcFuel" [V "q20", V "xs", V "len", V "cop", V "currIsMove", V "q14"]))) &&&
                   (((V "q14" === C "none" []) &&&
                   (V "q2" === C "none" [])) |||
                   (fresh ["res"] (
                      (V "q14" === C "some" [V "res"]) &&&
                      (fresh ["q16"] (
                         (V "q2" === C "some" [V "q16"]) &&&
                         (fresh ["q18"] (
                            (call "getFuel" [V "x", V "state", V "cop", V "q18"]) &&&
                            (call "add" [V "q18", V "res", V "q16"]))))))))))) |||
                ((V "q12" === C "false" []) &&&
                (V "q2" === C "none" [])))))))))))))
  )) (
  Let (def "checkAnswer" ["answer", "len", "cop", "q1"] (
    fresh ["q0"] (
      (call "startState" [V "len", V "cop", V "q0"]) &&&
      (call "calcFuel" [V "q0", V "answer", V "len", V "cop", C "false" [], V "q1"]))
  )) (
  last_goal))))))))))))))))

  ,

  "open MiniKanren\nopen MiniKanrenStd\ntype 'a0 gnat =\n  | O \n  | S of 'a0 \nlet rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\nmodule For_gnat =\n  (Fmap)(struct\n           let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gnat\n         end)\nlet rec o () = inj (For_gnat.distrib O)\nand s x__0 = inj (For_gnat.distrib (S x__0))\ntype 'a0 gstep =\n  | Left of 'a0 \n  | Right of 'a0 \n  | Fill \n  | Pour of 'a0 \nlet rec fmap fa0 =\n  function\n  | Left a0 -> Left (fa0 a0)\n  | Right a0 -> Right (fa0 a0)\n  | Fill -> Fill\n  | Pour a0 -> Pour (fa0 a0)\nmodule For_gstep =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | Left a0 -> Left (fa0 a0)\n             | Right a0 -> Right (fa0 a0)\n             | Fill -> Fill\n             | Pour a0 -> Pour (fa0 a0)\n           type 'a0 t = 'a0 gstep\n         end)\nlet rec left x__0 = inj (For_gstep.distrib (Left x__0))\nand right x__0 = inj (For_gstep.distrib (Right x__0))\nand fill () = inj (For_gstep.distrib Fill)\nand pour x__0 = inj (For_gstep.distrib (Pour x__0))\ntype ('a1, 'a0) gstate =\n  | St of 'a1 * 'a1 * 'a0 \nlet rec fmap fa1 fa0 =\n  function | St (a1_0, a1_1, a0_2) -> St ((fa1 a1_0), (fa1 a1_1), (fa0 a0_2))\nmodule For_gstate =\n  (Fmap2)(struct\n            let rec fmap fa1 fa0 =\n              function\n              | St (a1_0, a1_1, a0_2) ->\n                  St ((fa1 a1_0), (fa1 a1_1), (fa0 a0_2))\n            type ('a1, 'a0) t = ('a1, 'a0) gstate\n          end)\nlet rec st x__0 x__1 x__2 = inj (For_gstate.distrib (St (x__0, x__1, x__2)))")
