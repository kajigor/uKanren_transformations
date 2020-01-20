module Program.TreeGen where

import Syntax
import Prelude hiding (repeat)

eqNat :: [Def]
eqNat = [eqNatDef]

eqNatDef :: Def
eqNatDef = 
    Def "eq_nat" ["n1", "n2", "q11"] (
      fresh ["q12"] (
        (V "q12" === C "pair" [V "n1", V "n2"]) &&&
        (((V "q12" === C "pair" [C "o" [], C "o" []]) &&&
        (V "q11" === C "true" [])) |||
        (fresh ["q14"] (
           (V "q12" === C "pair" [C "s" [V "q14"], C "o" []]) &&&
           (V "q11" === C "false" []))) |||
        (fresh ["q16"] (
           (V "q12" === C "pair" [C "o" [], C "s" [V "q16"]]) &&&
           (V "q11" === C "false" []))) |||
        (fresh ["x", "y"] (
           (V "q12" === C "pair" [C "s" [V "x"], C "s" [V "y"]]) &&&
           (call "eq_nat" [V "x", V "y", V "q11"])))))
    )

eqOption :: [Def]
eqOption = eqOptionDef : eqNat 

eqOptionDef :: Def 
eqOptionDef = 
    Def "eq_option" ["a", "b", "q19"] (
      fresh ["q20"] (
        (V "q20" === C "pair" [V "a", V "b"]) &&&
        ((fresh ["x", "y"] (
            (V "q20" === C "pair" [C "some" [V "x"], C "some" [V "y"]]) &&&
            (call "eq_nat" [V "x", V "y", V "q19"]))) |||
        (fresh ["q21"] (
           (V "q20" === C "pair" [C "some" [V "q21"], C "none" []]) &&&
           (V "q19" === C "false" []))) |||
        (fresh ["q23"] (
           (V "q20" === C "pair" [C "none" [], C "some" [V "q23"]]) &&&
           (V "q19" === C "false" []))) |||
        ((V "q20" === C "pair" [C "none" [], C "none" []]) &&&
        (V "q19" === C "true" []))))
    )

eqLists :: [Def]
eqLists = eqListsDef : eqTree 

eqListsDef :: Def 
eqListsDef = 
    Def "eq_lists" ["l1", "l2", "q27"] (
      fresh ["q28"] (
        (V "q28" === C "pair" [V "l1", V "l2"]) &&&
        ((fresh ["x", "xs", "y", "ys"] (
            (V "q28" === C "pair" [C "%" [V "x", V "xs"], C "%" [V "y", V "ys"]]) &&&
            (fresh ["q29", "q30"] (
               (call "eq_tree" [V "x", V "y", V "q29"]) &&&
               (call "eq_lists" [V "xs", V "ys", V "q30"]) &&&
               (((V "q29" === C "false" []) &&&
               (V "q27" === C "false" [])) |||
               ((V "q29" === C "true" []) &&&
               (V "q27" === V "q30"))))))) |||
        (fresh ["q35", "q36"] (
           (V "q28" === C "pair" [C "%" [V "q35", V "q36"], C "nil" []]) &&&
           (V "q27" === C "false" []))) |||
        (fresh ["q38", "q39"] (
           (V "q28" === C "pair" [C "nil" [], C "%" [V "q38", V "q39"]]) &&&
           (V "q27" === C "false" []))) |||
        ((V "q28" === C "pair" [C "nil" [], C "nil" []]) &&&
        (V "q27" === C "true" []))))
    )

eqTree :: [Def]
eqTree = eqTreeDef : eqOption ++ eqLists

eqTreeDef :: Def
eqTreeDef =
  Def "eq_tree" ["t1", "t2", "q53"] (
    fresh ["a1", "c1"] (
      (V "t1" === C "node" [V "a1", V "c1"]) &&&
      (fresh ["a2", "c2"] (
         (V "t2" === C "node" [V "a2", V "c2"]) &&&
         (fresh ["q45", "q46"] (
            (call "eq_option" [V "a1", V "a2", V "q45"]) &&&
            (call "eq_lists" [V "c1", V "c2", V "q46"]) &&&
            (((V "q45" === C "false" []) &&&
            (V "q53" === C "false" [])) |||
            ((V "q45" === C "true" []) &&&
            (V "q53" === V "q46")))))))))

repeat :: [Def]
repeat = [repeatDef]

repeatDef :: Def 
repeatDef = 
    Def "repeat" ["e", "n", "q0"] (
      ((V "n" === C "o" []) &&&
      (V "q0" === C "nil" [])) |||
      (fresh ["x"] (
         (V "n" === C "s" [V "x"]) &&&
         (fresh ["q2"] (
            (V "q0" === C "%" [V "e", V "q2"]) &&&
            (call "repeat" [V "e", V "x", V "q2"])))))
    )

treeGenerator :: [Def]
treeGenerator = treeGeneratorDef : repeat

treeGeneratorDef :: Def 
treeGeneratorDef = 
    Def "tree_generator" ["n", "q10"] (
      ((V "n" === C "o" []) &&&
      (V "q10" === C "node" [C "none" [], C "nil" []])) |||
      (fresh ["x"] (
        (V "n" === C "s" [V "x"]) &&&
        (fresh ["q6"] (
            (V "q10" === C "node" [C "some" [V "x"], V "q6"]) &&&
            (fresh ["q8"] (
              (call "tree_generator" [V "x", V "q8"]) &&&
              (call "repeat" [V "q8", V "n", V "q6"])))))))
    )
  

treeGen :: [Def]
treeGen = treeGenerator ++ eqTree 

env :: String 
env = 
  "open MiniKanren\nopen MiniKanrenStd\ntype 'a0 gnat =\n  | O \n  | S of 'a0 \nlet rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\nmodule For_gnat =\n  (Fmap)(struct\n           let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gnat\n         end)\nlet rec o () = inj (For_gnat.distrib O)\nand s x__0 = inj (For_gnat.distrib (S x__0))\ntype ('a1, 'a0) gtree =\n  | Node of 'a1 * 'a0 \nlet rec fmap fa1 fa0 =\n  function | Node (a1_0, a0_1) -> Node ((fa1 a1_0), (fa0 a0_1))\nmodule For_gtree =\n  (Fmap2)(struct\n            let rec fmap fa1 fa0 =\n              function | Node (a1_0, a0_1) -> Node ((fa1 a1_0), (fa0 a0_1))\n            type ('a1, 'a0) t = ('a1, 'a0) gtree\n          end)\nlet rec node x__0 x__1 = inj (For_gtree.distrib (Node (x__0, x__1)))"



-- badAppendo =
--   (\last_goal ->
--    Let (Def "badAppendo" ["x", "y", "q12"] (
--      Let (Def "r" ["q5"] (
--        V "q5" === C "%" [C "pair" [C "true" [], C "true" []], C "%" [C "pair" [C "false" [], C "true" []], C "nil" []]]
--      )) (
--      fresh ["q7"] (
--        (call "r" [V "q7"]) &&&
--        (fresh ["a", "b", "c"] (
--           (V "q7" === C "%" [C "pair" [C "true" [], C "true" []], C "%" [C "pair" [V "a", V "b"], V "c"]]) &&&
--           (((V "x" === C "nil" []) &&&
--           (V "y" === V "q12")) |||
--           (fresh ["w", "ws"] (
--              (V "x" === C "%" [V "w", V "ws"]) &&&
--              (fresh ["q10"] (
--                 (V "q12" === C "%" [V "w", V "q10"]) &&&
--                 (call "badAppendo" [V "ws", V "y", V "q10"]))))))))))
--    )) (
--    Let (Def "append" ["x", "y", "q4"] (
--      Let (Def "f" ["x", "q0"] (
--        ((V "x" === C "nil" []) &&&
--        (V "y" === V "q0")) |||
--        (fresh ["r", "rs"] (
--           (V "x" === C "%" [V "r", V "rs"]) &&&
--           (fresh ["q2"] (
--              (V "q0" === C "%" [V "r", V "q2"]) &&&
--              (call "f" [V "rs", V "q2"])))))
--      )) (
--      call "f" [V "x", V "q4"])
--    )) (
--    last_goal))

--    ,

--    "open MiniKanren\nopen MiniKanrenStd")
