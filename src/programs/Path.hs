module Path where

import Syntax

query = fst pathTree $ fresh ["c", "g", "q"] (call "isPath" [V "c", V "g", V "q"])

query1 = fst pathTree $ fresh ["c", "g"] (call "isPath" [V "c", V "g", C "true" []])

queryNat = fst pathTree $ fresh ["a", "b", "q"] (call "eqNat" [V "a", V "b", V "q"])

queryPair = fst pathTree $ fresh ["a", "b", "q"] (call "eqPair" [V "a", V "b", V "q"])

queryPair1 = fst pathTree $ fresh ["a", "b"] (call "eqPair" [V "a", V "b", C "true" [] ])

queryElem = fst pathTree $ fresh ["x", "g", "q"] (call "elem" [V "x", V "g", V "q"])

queryElem1 = fst pathTree $ fresh ["x", "g"] (call "elem" [V "x", V "g", C "true" []])

queryQueer = fst pathTree $ fresh ["x", "y", "z"] (call "eqPair" [V "x", V "y", C "false" []] &&& call "elem" [V "x", V "z", C "true" []])

env = snd pathTree

pathTree =
 (\last_goal ->
  Let (def "eqNat" ["a", "b", "q24"] (
    fresh ["q25"] (
      (V "q25" === C "pair" [V "a", V "b"]) &&&
      (((V "q25" === C "pair" [C "z" [], C "z" []]) &&&
      (V "q24" === C "true" [])) |||
      (fresh ["q27"] (
         (V "q25" === C "pair" [C "s" [V "q27"], C "z" []]) &&&
         (V "q24" === C "false" []))) |||
      (fresh ["q29"] (
         (V "q25" === C "pair" [C "z" [], C "s" [V "q29"]]) &&&
         (V "q24" === C "false" []))) |||
      (fresh ["x", "y"] (
         (V "q25" === C "pair" [C "s" [V "x"], C "s" [V "y"]]) &&&
         (call "eqNat" [V "x", V "y", V "q24"])))))
  )) (
  Let (def "eqPair" ["a", "b", "q15"] (
    fresh ["q16", "a1", "a2", "b1", "b2", "q17", "q18"] (
      (V "q16" === C "pair" [V "a", V "b"]) &&&
      (V "q16" === C "pair" [C "pair" [V "a1", V "a2"], C "pair" [V "b1", V "b2"]]) &&&
      (call "eqNat" [V "a1", V "b1", V "q17"]) &&&
      (call "eqNat" [V "a2", V "b2", V "q18"]) &&&
      (((V "q17" === C "false" []) &&&
      (V "q15" === C "false" [])) |||
      ((V "q17" === C "true" []) &&&
      (V "q15" === V "q18"))))
  )) (
  Let (def "elem" ["x", "g", "q10"] (
    ((V "g" === C "nil" []) &&&
    (V "q10" === C "false" [])) |||
    (fresh ["y", "ys", "q13"] (
       (V "g" === C "%" [V "y", V "ys"]) &&&
       (call "eqPair" [V "x", V "y", V "q13"]) &&&
       (((V "q13" === C "true" []) &&&
       (V "q10" === C "true" [])) |||
       ((V "q13" === C "false" []) &&&
       (call "elem" [V "x", V "ys", V "q10"])))))
  )) (
  Let (def "isPath" ["c", "g", "q0"] (
    ((V "c" === C "nil" []) &&&
    (V "q0" === C "true" [])) |||
    (fresh ["q2"] (
       (V "c" === C "%" [V "q2", C "nil" []]) &&&
       (V "q0" === C "true" []))) |||
    (fresh ["x1", "x2", "xs", "q4", "q5"] (
       (V "c" === C "%" [V "x1", C "%" [V "x2", V "xs"]]) &&&
       (call "elem" [C "pair" [V "x1", V "x2"], V "g", V "q4"]) &&&
       (call "isPath" [C "%" [V "x2", V "xs"], V "g", V "q5"]) &&&
       (((V "q4" === C "false" []) &&&
       (V "q0" === C "false" [])) |||
       ((V "q4" === C "true" []) &&&
       (V "q0" === V "q5")))))
  )) (
  last_goal))))

  ,

  "open MiniKanren\nopen MiniKanrenStd\ntype 'a0 gnat =\n  | Z \n  | S of 'a0 \nlet rec fmap fa0 = function | Z -> Z | S a0 -> S (fa0 a0)\nmodule For_gnat =\n  (Fmap)(struct\n           let rec fmap fa0 = function | Z -> Z | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gnat\n         end)\nlet rec z () = inj (For_gnat.distrib Z)\nand s x__0 = inj (For_gnat.distrib (S x__0))")
