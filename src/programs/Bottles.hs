module Bottles where

import Syntax

true = C "true" []

six = ofInt 6
  where
    ofInt 0 = C "zero" []
    ofInt n = C "succ" [ofInt $ n - 1]

query = fst bottles $ fresh ["a", "b", "c"] (call "checkAnswer" [V "a", V "b", V "c", true])

query' =
  definition $ fresh ["q", "res"] (call "query" [V "q", V "res"])
  where
    definition = Let (def "query" ["q", "res"] (
                      call "capacities1" [V "1"] &&& call "checkAnswer" [V "res", V "q", six, true]))
                 . fst bottles

env = snd bottles

bottles =
 (\last_goal ->
  Let (def "add" ["a", "b", "q113"] (
    ((V "a" === C "o" []) &&&
    (V "b" === V "q113")) |||
    (fresh ["x"] (
       (V "a" === C "s" [V "x"]) &&&
       (call "add" [V "x", C "s" [V "b"], V "q113"])))
  )) (
  Let (def "greater" ["a", "b", "q109"] (
    ((V "a" === C "o" []) &&&
    (V "q109" === C "false" [])) |||
    (fresh ["x"] (
       (V "a" === C "s" [V "x"]) &&&
       (((V "b" === C "o" []) &&&
       (V "q109" === C "true" [])) |||
       (fresh ["y"] (
          (V "b" === C "s" [V "y"]) &&&
          (call "greater" [V "x", V "y", V "q109"]))))))
  )) (
  Let (def "sub" ["a", "b", "q105"] (
    ((V "b" === C "o" []) &&&
    (V "a" === V "q105")) |||
    (fresh ["y"] (
       (V "b" === C "s" [V "y"]) &&&
       (((V "a" === C "o" []) &&&
       (V "q105" === C "o" [])) |||
       (fresh ["x"] (
          (V "a" === C "s" [V "x"]) &&&
          (call "sub" [V "x", V "y", V "q105"]))))))
  )) (
  Let (def "anotherBottle" ["b", "q102"] (
    ((V "b" === C "fst" []) &&&
    (V "q102" === C "snd" [])) |||
    ((V "b" === C "snd" []) &&&
    (V "q102" === C "fst" []))
  )) (
  Let (def "createState" ["bottle", "lvl1", "lvl2", "q99"] (
    ((V "bottle" === C "fst" []) &&&
    (V "q99" === C "pair" [V "lvl1", V "lvl2"])) |||
    ((V "bottle" === C "snd" []) &&&
    (V "q99" === C "pair" [V "lvl2", V "lvl1"]))
  )) (
  Let (def "fst'" ["x", "q96"] (
    fresh ["a", "q97"] (
      (V "x" === C "pair" [V "a", V "q97"]) &&&
      (V "a" === V "q96"))
  )) (
  Let (def "snd'" ["x", "q93"] (
    fresh ["q94", "a"] (
      (V "x" === C "pair" [V "q94", V "a"]) &&&
      (V "a" === V "q93"))
  )) (
  Let (def "get_capacity" ["capacities", "bottle", "q92"] (
    ((V "bottle" === C "fst" []) &&&
    (call "fst'" [V "capacities", V "q92"])) |||
    ((V "bottle" === C "snd" []) &&&
    (call "snd'" [V "capacities", V "q92"]))
  )) (
  Let (def "|=|" ["a", "b", "q85"] (
    ((V "a" === C "o" []) &&&
    (((V "b" === C "o" []) &&&
    (V "q85" === C "true" [])) |||
    (fresh ["q88"] (
       (V "b" === C "s" [V "q88"]) &&&
       (V "q85" === C "false" []))))) |||
    (fresh ["x"] (
       (V "a" === C "s" [V "x"]) &&&
       (((V "b" === C "o" []) &&&
       (V "q85" === C "false" [])) |||
       (fresh ["y"] (
          (V "b" === C "s" [V "y"]) &&&
          (call "|=|" [V "x", V "y", V "q85"]))))))
  )) (
  Let (def "checkStep" ["state0", "step0", "capacities", "q48"] (
    fresh ["f", "s"] (
      (V "state0" === C "pair" [V "f", V "s"]) &&&
      (fresh ["t", "b"] (
         (V "step0" === C "pair" [V "t", V "b"]) &&&
         (((V "t" === C "fill" []) &&&
         (fresh ["q51"] (
            (((V "b" === C "fst" []) &&&
            (V "f" === V "q51")) |||
            ((V "b" === C "snd" []) &&&
            (V "s" === V "q51"))) &&&
            (call "|=|" [V "q51", C "o" [], V "q48"])))) |||
         ((V "t" === C "empty" []) &&&
         (fresh ["q56", "q57"] (
            (((V "b" === C "fst" []) &&&
            (V "f" === V "q56")) |||
            ((V "b" === C "snd" []) &&&
            (V "s" === V "q56"))) &&&
            (call "get_capacity" [V "capacities", V "b", V "q57"]) &&&
            (call "|=|" [V "q56", V "q57", V "q48"])))) |||
         ((V "t" === C "pour" []) &&&
         (fresh ["q62"] (
            (fresh ["q66", "q67"] (
               (fresh ["q72"] (
                  (((V "b" === C "fst" []) &&&
                  (V "f" === V "q72")) |||
                  ((V "b" === C "snd" []) &&&
                  (V "s" === V "q72"))) &&&
                  (call "|=|" [V "q72", C "o" [], V "q66"]))) &&&
               (fresh ["q77", "q78"] (
                  (((V "b" === C "fst" []) &&&
                  (V "s" === V "q77")) |||
                  ((V "b" === C "snd" []) &&&
                  (V "f" === V "q77"))) &&&
                  (fresh ["q83"] (
                     (call "anotherBottle" [V "b", V "q83"]) &&&
                     (call "get_capacity" [V "capacities", V "q83", V "q78"]))) &&&
                  (call "|=|" [V "q77", V "q78", V "q67"]))) &&&
               (((V "q66" === C "true" []) &&&
               (V "q62" === C "true" [])) |||
               ((V "q66" === C "false" []) &&&
               (V "q62" === V "q67"))))) &&&
            (((V "q62" === C "true" []) &&&
            (V "q48" === C "false" [])) |||
            ((V "q62" === C "false" []) &&&
            (V "q48" === C "true" []))))))))))
  )) (
  Let (def "doStep" ["state0", "step0", "capacities", "q15"] (
    fresh ["f", "s"] (
      (V "state0" === C "pair" [V "f", V "s"]) &&&
      (fresh ["t", "b"] (
         (V "step0" === C "pair" [V "t", V "b"]) &&&
         (((V "t" === C "fill" []) &&&
         (fresh ["q18", "q19"] (
            (call "get_capacity" [V "capacities", V "b", V "q18"]) &&&
            (((V "b" === C "fst" []) &&&
            (V "s" === V "q19")) |||
            ((V "b" === C "snd" []) &&&
            (V "f" === V "q19"))) &&&
            (call "createState" [V "b", V "q18", V "q19", V "q15"])))) |||
         ((V "t" === C "empty" []) &&&
         (fresh ["q24"] (
            (((V "b" === C "fst" []) &&&
            (V "s" === V "q24")) |||
            ((V "b" === C "snd" []) &&&
            (V "f" === V "q24"))) &&&
            (call "createState" [V "b", C "o" [], V "q24", V "q15"])))) |||
         ((V "t" === C "pour" []) &&&
         (fresh ["q30"] (
            (fresh ["q43", "q44"] (
               (call "add" [V "f", V "s", V "q43"]) &&&
               (fresh ["q46"] (
                  (call "anotherBottle" [V "b", V "q46"]) &&&
                  (call "get_capacity" [V "capacities", V "q46", V "q44"]))) &&&
               (call "greater" [V "q43", V "q44", V "q30"]))) &&&
            (((V "q30" === C "true" []) &&&
            (fresh ["q31", "q32"] (
               (fresh ["q34", "q35"] (
                  (call "add" [V "f", V "s", V "q34"]) &&&
                  (fresh ["q37"] (
                     (call "anotherBottle" [V "b", V "q37"]) &&&
                     (call "get_capacity" [V "capacities", V "q37", V "q35"]))) &&&
                  (call "sub" [V "q34", V "q35", V "q31"]))) &&&
               (fresh ["q39"] (
                  (call "anotherBottle" [V "b", V "q39"]) &&&
                  (call "get_capacity" [V "capacities", V "q39", V "q32"]))) &&&
               (call "createState" [V "b", V "q31", V "q32", V "q15"])))) |||
            ((V "q30" === C "false" []) &&&
            (fresh ["q41"] (
               (call "add" [V "f", V "s", V "q41"]) &&&
               (call "createState" [V "b", C "o" [], V "q41", V "q15"]))))))))))))
  )) (
  Let (def "isFinishState" ["state0", "reqLvl", "q8"] (
    fresh ["f", "s"] (
      (V "state0" === C "pair" [V "f", V "s"]) &&&
      (fresh ["q9", "q10"] (
         (call "|=|" [V "f", V "reqLvl", V "q9"]) &&&
         (call "|=|" [V "s", V "reqLvl", V "q10"]) &&&
         (((V "q9" === C "true" []) &&&
         (V "q8" === C "true" [])) |||
         ((V "q9" === C "false" []) &&&
         (V "q8" === V "q10"))))))
  )) (
  Let (def "checkAnswer" ["answer", "capacities", "reqLvl", "q7"] (
    Let (def "checkAnswer'" ["state0", "answer", "capacities", "reqLvl", "q1"] (
      ((V "answer" === C "nil" []) &&&
      (call "isFinishState" [V "state0", V "reqLvl", V "q1"])) |||
      (fresh ["x", "xs"] (
         (V "answer" === C "%" [V "x", V "xs"]) &&&
         (fresh ["q3"] (
            (call "checkStep" [V "state0", V "x", V "capacities", V "q3"]) &&&
            (((V "q3" === C "true" []) &&&
            (fresh ["q4"] (
               (call "doStep" [V "state0", V "x", V "capacities", V "q4"]) &&&
               (call "checkAnswer'" [V "q4", V "xs", V "capacities", V "reqLvl", V "q1"])))) |||
            ((V "q3" === C "false" []) &&&
            (V "q1" === C "false" [])))))))
    )) (
    call "checkAnswer'" [C "pair" [C "o" [], C "o" []], V "answer", V "capacities", V "reqLvl", V "q7"])
  )) (
  Let (def "capacities1" ["q0"] (
    V "q0" === C "pair" [C "s" [C "s" [C "s" [C "s" [C "o" []]]]], C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "o" []]]]]]]]]]]
  )) (
  last_goal))))))))))))))

  ,

  "open MiniKanren\nopen MiniKanrenStd\ntype bottle =\n  | Fst \n  | Snd \nlet fst () = !! Fst\nlet snd () = !! Snd\ntype stepType =\n  | Fill \n  | Empty \n  | Pour \nlet fill () = !! Fill\nlet empty () = !! Empty\nlet pour () = !! Pour\ntype 'a0 gnat =\n  | O \n  | S of 'a0 \nlet rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\nmodule For_gnat =\n  (Fmap)(struct\n           let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gnat\n         end)\nlet rec o () = inj (For_gnat.distrib O)\nand s x__0 = inj (For_gnat.distrib (S x__0))")
