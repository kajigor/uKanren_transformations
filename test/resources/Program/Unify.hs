module Program.Unify where

import Syntax

env = snd unify

query = fst unify $ fresh ["x", "y", "s"] (call "check_uni" [V "x", V "y", V "s", true])

querySame = fst unify $ fresh ["x", "s"] (call "check_uni" [V "x", V "x", V "s", true])

queryAll = fst unify $ fresh ["x", "y", "s"] (call "all_check_uni" [V "5", V "7", V "2", true])

queryGet = fst unify $ fresh ["var", "subst", "q25"] (call "get_term" [V "var", V "subst",  C "some" [V "q25"]])

true = C "true" []

unify = unifyTree

unifyTree =
 (\last_goal ->
  Let (Def "eq_nat" ["a", "b", "q36"] (
    fresh ["q37"] (
      (V "q37" === C "pair" [V "a", V "b"]) &&&
      (((V "q37" === C "pair" [C "z" [], C "z" []]) &&&
      (V "q36" === C "true" [])) |||
      (fresh ["q39"] (
         (V "q37" === C "pair" [C "s" [V "q39"], C "z" []]) &&&
         (V "q36" === C "false" []))) |||
      (fresh ["q41"] (
         (V "q37" === C "pair" [C "z" [], C "s" [V "q41"]]) &&&
         (V "q36" === C "false" []))) |||
      (fresh ["x", "y"] (
         (V "q37" === C "pair" [C "s" [V "x"], C "s" [V "y"]]) &&&
         (call "eq_nat" [V "x", V "y", V "q36"])))))
  )) (
  Let (Def "get_term" ["var", "subst", "q32"] (
    ((V "subst" === C "nil" []) &&&
    (V "q32" === C "none" [])) |||
    (fresh ["x", "xs"] (
       (V "subst" === C "%" [V "x", V "xs"]) &&&
       (((V "var" === C "z" []) &&&
       (V "x" === V "q32")) |||
       (fresh ["n"] (
          (V "var" === C "s" [V "n"]) &&&
          (call "get_term" [V "n", V "xs", V "q32"]))))))
  )) (
  Let (Def "check_uni" ["subst", "t1", "t2", "q31"] (
    Let (Def "forall2" ["subst", "l1", "l2", "q0"] (
      fresh ["q1"] (
        (V "q1" === C "pair" [V "l1", V "l2"]) &&&
        (((V "q1" === C "pair" [C "nil" [], C "nil" []]) &&&
        (V "q0" === C "true" [])) |||
        (fresh ["x", "xs", "y", "ys", "q3", "q4"] (
           (V "q1" === C "pair" [C "%" [V "x", V "xs"], C "%" [V "y", V "ys"]]) &&&
           (call "check_uni" [V "subst", V "x", V "y", V "q3"]) &&&
           (call "forall2" [V "subst", V "xs", V "ys", V "q4"]) &&&
           (((V "q3" === C "false" []) &&&
           (V "q0" === C "false" [])) |||
           ((V "q3" === C "true" []) &&&
           (V "q0" === V "q4")))))))
    )) (
    fresh ["q11"] (
      (V "q11" === C "pair" [V "t1", V "t2"]) &&&
      ((fresh ["n1", "a1", "n2", "a2", "q12", "q13"] (
          (V "q11" === C "pair" [C "constr" [V "n1", V "a1"], C "constr" [V "n2", V "a2"]]) &&&
          (call "eq_nat" [V "n1", V "n2", V "q12"]) &&&
          (call "forall2" [V "subst", V "a1", V "a2", V "q13"]) &&&
          (((V "q12" === C "false" []) &&&
          (V "q31" === C "false" [])) |||
          ((V "q12" === C "true" []) &&&
          (V "q31" === V "q13"))))) |||
      (fresh ["v", "n", "a", "q19"] (
         (V "q11" === C "pair" [C "var_" [V "v"], C "constr" [V "n", V "a"]]) &&&
         (call "get_term" [V "v", V "subst", V "q19"]) &&&
         (((V "q19" === C "none" []) &&&
         (V "q31" === C "false" [])) |||
         (fresh ["t"] (
            (V "q19" === C "some" [V "t"]) &&&
            (call "check_uni" [V "subst", V "t", V "t2", V "q31"])))))) |||
      (fresh ["n", "a", "v", "q22"] (
         (V "q11" === C "pair" [C "constr" [V "n", V "a"], C "var_" [V "v"]]) &&&
         (call "get_term" [V "v", V "subst", V "q22"]) &&&
         (((V "q22" === C "none" []) &&&
         (V "q31" === C "false" [])) |||
         (fresh ["t"] (
            (V "q22" === C "some" [V "t"]) &&&
            (call "check_uni" [V "subst", V "t1", V "t", V "q31"])))))) |||
      (fresh ["v1", "v2", "q25"] (
         (V "q11" === C "pair" [C "var_" [V "v1"], C "var_" [V "v2"]]) &&&
         (call "get_term" [V "v1", V "subst", V "q25"]) &&&
         ((fresh ["t1'"] (
             (V "q25" === C "some" [V "t1'"]) &&&
             (call "check_uni" [V "subst", V "t1'", V "t2", V "q31"]))) |||
         (fresh ["q27"] (
            (V "q25" === C "none" []) &&&
            (call "get_term" [V "v2", V "subst", V "q27"]) &&&
            ((fresh ["q28"] (
                (V "q27" === C "some" [V "q28"]) &&&
                (V "q31" === C "false" []))) |||
            ((V "q27" === C "none" []) &&&
            (call "eq_nat" [V "v1", V "v2", V "q31"])))))))))))
  )) (
  last_goal)))

  ,

  "open MiniKanren\nopen MiniKanrenStd\ntype 'a0 gnat =\n  | Z \n  | S of 'a0 \nlet rec fmap fa0 = function | Z -> Z | S a0 -> S (fa0 a0)\nmodule For_gnat =\n  (Fmap)(struct\n           let rec fmap fa0 = function | Z -> Z | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gnat\n         end)\nlet rec z () = inj (For_gnat.distrib Z)\nand s x__0 = inj (For_gnat.distrib (S x__0))\ntype ('a1, 'a0) gterm =\n  | Var_ of 'a1 \n  | Constr of 'a1 * 'a0 \nlet rec fmap fa1 fa0 =\n  function\n  | Var_ a1 -> Var_ (fa1 a1)\n  | Constr (a1_0, a0_1) -> Constr ((fa1 a1_0), (fa0 a0_1))\nmodule For_gterm =\n  (Fmap2)(struct\n            let rec fmap fa1 fa0 =\n              function\n              | Var_ a1 -> Var_ (fa1 a1)\n              | Constr (a1_0, a0_1) -> Constr ((fa1 a1_0), (fa0 a0_1))\n            type ('a1, 'a0) t = ('a1, 'a0) gterm\n          end)\nlet rec var_ x__0 = inj (For_gterm.distrib (Var_ x__0))\nand constr x__0 x__1 = inj (For_gterm.distrib (Constr (x__0, x__1)))")

--
-- unify =
--  (\last_goal ->
--   Let (Def "eq_nat" ["a", "b", "q29"] (
--     (V "a" === V "b") &&& (V "q29" === C "true" [])
--   )) (
--   Let (Def "get_term" ["var", "subst", "q25"] (
--     ((V "subst" === C "nil" []) &&&
--     (V "q25" === C "none" [])) |||
--     (fresh ["x", "xs"] (
--        (V "subst" === C "%" [V "x", V "xs"]) &&&
--        (((V "var" === C "o" []) &&&
--        (V "x" === V "q25")) |||
--        (fresh ["n"] (
--           (V "var" === C "s" [V "n"]) &&&
--           (call "get_term" [V "n", V "xs", V "q25"]))))))
--   )) (
--   Let (Def "check_uni" ["t1", "t2", "subst", "q24"] (
--     Let (Def "all_check_uni" ["a1", "a2", "subst", "q0"] (
--       fresh ["q1"] (
--         (V "q1" === C "pair" [V "a1", V "a2"]) &&&
--         (((V "q1" === C "pair" [C "nil" [], C "nil" []]) &&&
--         (V "q0" === C "true" [])) |||
--         (fresh ["x", "xs", "y", "ys"] (
--            (V "q1" === C "pair" [C "%" [V "x", V "xs"], C "%" [V "y", V "ys"]]) &&&
--            (fresh ["q3", "q4"] (
--               (call "check_uni" [V "x", V "y", V "subst", V "q3"]) &&&
--               (call "all_check_uni" [V "xs", V "ys", V "subst", V "q4"]) &&&
--               (((V "q3" === C "false" []) &&&
--               (V "q0" === C "false" [])) |||
--               ((V "q3" === C "true" []) &&&
--               (V "q0" === V "q4")))))))))
--     )) (
--     fresh ["q11"] (
--       (V "q11" === C "pair" [V "t1", V "t2"]) &&&
--       ((fresh ["n1", "a1", "n2", "a2"] (
--           (V "q11" === C "pair" [C "constr" [V "n1", V "a1"], C "constr" [V "n2", V "a2"]]) &&&
--           (fresh ["q13"] (
--              (call "eq_nat" [V "n1", V "n2", V "q13"]) &&&
--              (((V "q13" === C "true" []) &&&
--              (call "all_check_uni" [V "a1", V "a2", V "subst", V "q24"])) |||
--              ((V "q13" === C "false" []) &&&
--              (V "q24" === C "false" [])))))))  |||
--       (fresh ["v1", "n2", "a2"] (
--          (V "q11" === C "pair" [C "var" [V "v1"], C "constr" [V "n2", V "a2"]]) &&&
--          (fresh ["q16"] (
--             (call "get_term" [V "v1", V "subst", V "q16"]) &&&
--             (fresh ["t1'"] (
--                (V "q16" === C "some" [V "t1'"]) &&&
--                (call "check_uni" [V "t1'", V "t2", V "subst", V "q24"]))))))) |||
--       (fresh ["n1", "a1", "v2"] (
--          (V "q11" === C "pair" [C "constr" [V "n1", V "a1"], C "var" [V "v2"]]) &&&
--          (fresh ["q18"] (
--             (call "get_term" [V "v2", V "subst", V "q18"]) &&&
--             (fresh ["t2'"] (
--                (V "q18" === C "some" [V "t2'"]) &&&
--                (call "check_uni" [V "t1", V "t2'", V "subst", V "q24"])))))))  |||
--       (fresh ["v1", "v2"] (
--          (V "q11" === C "pair" [C "var" [V "v1"], C "var" [V "v2"]]) &&&
--          (fresh ["q20"] (
--             (call "get_term" [V "v1", V "subst", V "q20"]) &&&
--             ((fresh ["t1'"] (
--                 (V "q20" === C "some" [V "t1'"]) &&&
--                 (call "check_uni" [V "t1'", V "t2", V "subst", V "q24"]))) |||
--             ((V "q20" === C "none" []) &&&
--             (fresh ["q22"] (
--                (call "get_term" [V "v2", V "subst", V "q22"]) &&&
--                ((fresh ["t2'"] (
--                    (V "q22" === C "some" [V "t2'"]) &&&
--                    (call "check_uni" [V "t1", V "t2'", V "subst", V "q24"]))) |||
--                ((V "q22" === C "none" []) &&&
--                (call "eq_nat" [V "v1", V "v2", V "q24"])))))))))))  )))
--   )) (
--   last_goal)))
--
--   ,
--
--   "open MiniKanren\nopen MiniKanrenStd\ntype 'a0 gnat =\n  | O \n  | S of 'a0 \nlet rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\nmodule For_gnat =\n  (Fmap)(struct\n           let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gnat\n         end)\nlet rec o () = inj (For_gnat.distrib O)\nand s x__0 = inj (For_gnat.distrib (S x__0))\ntype ('a1, 'a0) gterm =\n  | Var of 'a1 \n  | Constr of 'a1 * 'a0 \nlet rec fmap fa1 fa0 =\n  function\n  | Var a1 -> Var (fa1 a1)\n  | Constr (a1_0, a0_1) -> Constr ((fa1 a1_0), (fa0 a0_1))\nmodule For_gterm =\n  (Fmap2)(struct\n            let rec fmap fa1 fa0 =\n              function\n              | Var a1 -> Var (fa1 a1)\n              | Constr (a1_0, a0_1) -> Constr ((fa1 a1_0), (fa0 a0_1))\n            type ('a1, 'a0) t = ('a1, 'a0) gterm\n          end)\nlet rec var x__0 = inj (For_gterm.distrib (Var x__0))\nand constr x__0 x__1 = inj (For_gterm.distrib (Constr (x__0, x__1)))")
-- --
