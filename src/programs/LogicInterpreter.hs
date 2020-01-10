module LogicInterpreter where

import Bool
import CPD
import Control.Monad
import Data.Maybe
import Data.List
import Printer.Dot
import qualified Eval as E
import qualified GlobalControl as GC
import Printer.GlobalTree
import List
import Num
import Programs
import Purification
import Printer.SldTree
import Syntax
import Text.Printf
import Debug.Trace
import Prelude hiding (succ)

{-
test_check_subst =
  runTestWithEnv "check_subst" (snd logic_interpreter) $ (fst logic_interpreter) $
    fresh ["subst", "fml", "res"] (call "check_subst" [V "subst", V "fml", V "res"])
-}

logic_interpreter =
 (\last_goal ->
  Let (def "eq_nat" ["a", "b", "q110"] (
    ((V "a" === C "z" []) &&&
    (((V "b" === C "z" []) &&&
    (V "q110" === C "true" [])) |||
    (fresh ["q113"] (
       (V "b" === C "s" [V "q113"]) &&&
       (V "q110" === C "false" []))))) |||
    (fresh ["x"] (
       (V "a" === C "s" [V "x"]) &&&
       (((V "b" === C "z" []) &&&
       (V "q110" === C "false" [])) |||
       (fresh ["y"] (
          (V "b" === C "s" [V "y"]) &&&
          (call "eq_nat" [V "x", V "y", V "q110"]))))))
  )) (
  Let (def "lookup" ["s", "n", "q105"] (
    ((V "s" === C "nil" []) &&&
    (V "q105" === C "none" [])) |||
    (fresh ["m", "e", "xs"] (
       (V "s" === C "%" [C "pair" [V "m", V "e"], V "xs"]) &&&
       (fresh ["q108"] (
          (call "eq_nat" [V "n", V "m", V "q108"]) &&&
          (((V "q108" === C "true" []) &&&
          (V "q105" === C "some" [V "e"])) |||
          ((V "q108" === C "false" []) &&&
          (call "lookup" [V "xs", V "n", V "q105"])))))))
  )) (
  Let (def "total_disj" ["a", "b", "q98"] (
    ((V "a" === C "true" []) &&&
    (((V "b" === C "true" []) &&&
    (V "q98" === C "true" [])) |||
    ((V "b" === C "false" []) &&&
    (V "q98" === C "false" [])))) |||
    ((V "a" === C "false" []) &&&
    (((V "b" === C "true" []) &&&
    (V "q98" === C "false" [])) |||
    ((V "b" === C "false" []) &&&
    (V "q98" === C "false" []))))
  )) (
  Let (def "total_conj" ["a", "b", "q91"] (
    ((V "a" === C "true" []) &&&
    (((V "b" === C "true" []) &&&
    (V "q91" === C "true" [])) |||
    ((V "b" === C "false" []) &&&
    (V "q91" === C "true" [])))) |||
    ((V "a" === C "false" []) &&&
    (((V "b" === C "true" []) &&&
    (V "q91" === C "true" [])) |||
    ((V "b" === C "false" []) &&&
    (V "q91" === C "false" []))))
  )) (
  Let (def "eval" ["subst", "expr", "q65"] (
    ((V "expr" === C "i" []) &&&
    (V "q65" === C "some" [C "true" []])) |||
    ((V "expr" === C "o" []) &&&
    (V "q65" === C "some" [C "false" []])) |||
    (fresh ["x"] (
       (V "expr" === C "var" [V "x"]) &&&
       (call "lookup" [V "subst", V "x", V "q65"]))) |||
    (fresh ["e"] (
       (V "expr" === C "not_" [V "e"]) &&&
       (fresh ["q69"] (
          (call "eval" [V "subst", V "e", V "q69"]) &&&
          (((V "q69" === C "none" []) &&&
          (V "q65" === C "none" [])) |||
          (fresh ["b"] (
             (V "q69" === C "some" [V "b"]) &&&
             (fresh ["q71"] (
                (V "q65" === C "some" [V "q71"]) &&&
                (((V "b" === C "true" []) &&&
                (V "q71" === C "false" [])) |||
                ((V "b" === C "false" []) &&&
                (V "q71" === C "true" [])))))))))))) |||
    (fresh ["l", "r"] (
       (V "expr" === C "conj_" [V "l", V "r"]) &&&
       (fresh ["q76"] (
          (call "eval" [V "subst", V "l", V "q76"]) &&&
          (((V "q76" === C "none" []) &&&
          (V "q65" === C "none" [])) |||
          (fresh ["a"] (
             (V "q76" === C "some" [V "a"]) &&&
             (fresh ["q79"] (
                (call "eval" [V "subst", V "r", V "q79"]) &&&
                (((V "q79" === C "none" []) &&&
                (V "q65" === C "none" [])) |||
                (fresh ["b"] (
                   (V "q79" === C "some" [V "b"]) &&&
                   (fresh ["q81"] (
                      (V "q65" === C "some" [V "q81"]) &&&
                      (call "total_conj" [V "a", V "b", V "q81"]))))))))))))))) |||
    (fresh ["l", "r"] (
       (V "expr" === C "disj" [V "l", V "r"]) &&&
       (fresh ["q84"] (
          (call "eval" [V "subst", V "l", V "q84"]) &&&
          (((V "q84" === C "none" []) &&&
          (V "q65" === C "none" [])) |||
          (fresh ["a"] (
             (V "q84" === C "some" [V "a"]) &&&
             (fresh ["q87"] (
                (call "eval" [V "subst", V "r", V "q87"]) &&&
                (((V "q87" === C "none" []) &&&
                (V "q65" === C "none" [])) |||
                (fresh ["b"] (
                   (V "q87" === C "some" [V "b"]) &&&
                   (fresh ["q89"] (
                      (V "q65" === C "some" [V "q89"]) &&&
                      (call "total_disj" [V "a", V "b", V "q89"])))))))))))))))
  )) (
  Let (def "append" ["a", "b", "q61"] (
    ((V "a" === C "nil" []) &&&
    (V "b" === V "q61")) |||
    (fresh ["x", "xs"] (
       (V "a" === C "%" [V "x", V "xs"]) &&&
       (fresh ["q63"] (
          (V "q61" === C "%" [V "x", V "q63"]) &&&
          (call "append" [V "xs", V "b", V "q63"])))))
  )) (
  Let (def "remove" ["v", "l", "q54"] (
    ((V "l" === C "nil" []) &&&
    (V "q54" === C "nil" [])) |||
    (fresh ["x", "xs"] (
       (V "l" === C "%" [V "x", V "xs"]) &&&
       (fresh ["nl"] (
          (call "remove" [V "v", V "xs", V "nl"]) &&&
          (fresh ["q58"] (
             (call "eq_nat" [V "v", V "x", V "q58"]) &&&
             (((V "q58" === C "true" []) &&&
             (V "nl" === V "q54")) |||
             ((V "q58" === C "false" []) &&&
             (V "q54" === C "%" [V "x", V "nl"])))))))))
  )) (
  Let (def "remove_repeats" ["l", "q48"] (
    ((V "l" === C "nil" []) &&&
    (V "q48" === C "nil" [])) |||
    (fresh ["x", "xs"] (
       (V "l" === C "%" [V "x", V "xs"]) &&&
       (fresh ["q50"] (
          (V "q48" === C "%" [V "x", V "q50"]) &&&
          (fresh ["q52"] (
             (call "remove" [V "x", V "xs", V "q52"]) &&&
             (call "remove_repeats" [V "q52", V "q50"])))))))
  )) (
  Let (def "all_vars" ["e", "q37"] (
    ((V "e" === C "i" []) &&&
    (V "q37" === C "nil" [])) |||
    ((V "e" === C "o" []) &&&
    (V "q37" === C "nil" [])) |||
    (fresh ["x"] (
       (V "e" === C "var" [V "x"]) &&&
       (V "q37" === C "%" [V "x", C "nil" []]))) |||
    (fresh ["q41"] (
       (V "e" === C "not_" [V "q41"]) &&&
       (call "all_vars" [V "q41", V "q37"]))) |||
    (fresh ["l", "r"] (
       (V "e" === C "conj_" [V "l", V "r"]) &&&
       (fresh ["q42", "q43"] (
          (call "all_vars" [V "l", V "q42"]) &&&
          (call "all_vars" [V "r", V "q43"]) &&&
          (call "append" [V "q42", V "q43", V "q37"]))))) |||
    (fresh ["l", "r"] (
       (V "e" === C "disj" [V "l", V "r"]) &&&
       (fresh ["q45", "q46"] (
          (call "all_vars" [V "l", V "q45"]) &&&
          (call "all_vars" [V "r", V "q46"]) &&&
          (call "append" [V "q45", V "q46", V "q37"])))))
  )) (
  Let (def "check_subst" ["subst", "expr", "q36"] (
    Let (def "check" ["subst", "vars", "q18"] (
      ((V "vars" === C "nil" []) &&&
      (((V "subst" === C "nil" []) &&&
      (V "q18" === C "true" [])) |||
      (fresh ["q21", "q22"] (
         (V "subst" === C "%" [V "q21", V "q22"]) &&&
         (V "q18" === C "false" []))))) |||
      (fresh ["x", "xs"] (
         (V "vars" === C "%" [V "x", V "xs"]) &&&
         (((V "subst" === C "nil" []) &&&
         (V "q18" === C "false" [])) |||
         (fresh ["a", "b", "ys"] (
            (V "subst" === C "%" [C "pair" [V "a", V "b"], V "ys"]) &&&
            (fresh ["q26", "q27"] (
               (call "eq_nat" [V "x", V "a", V "q26"]) &&&
               (call "check" [V "ys", V "xs", V "q27"]) &&&
               (((V "q26" === C "false" []) &&&
               (V "q18" === C "false" [])) |||
               ((V "q26" === C "true" []) &&&
               (V "q18" === V "q27"))))))))))
    )) (
    fresh ["q32"] (
      (fresh ["q34"] (
         (call "all_vars" [V "expr", V "q34"]) &&&
         (call "remove_repeats" [V "q34", V "q32"]))) &&&
      (call "check" [V "subst", V "q32", V "q36"])))
  )) (
  Let (def "eval_and_check" ["subst", "expr", "q11"] (
    fresh ["q12"] (
      (call "eval" [V "subst", V "expr", V "q12"]) &&&
      (((V "q12" === C "none" []) &&&
      (V "q11" === C "none" [])) |||
      (fresh ["y"] (
         (V "q12" === C "some" [V "y"]) &&&
         (fresh ["q15"] (
            (call "check_subst" [V "subst", V "expr", V "q15"]) &&&
            (((V "q15" === C "true" []) &&&
            (V "q11" === C "some" [V "y"])) |||
            ((V "q15" === C "false" []) &&&
            (V "q11" === C "none" [])))))))))
  )) (
  Let (def "check_and_eval" ["subst", "expr", "q4"] (
    fresh ["q5"] (
      (call "check_subst" [V "subst", V "expr", V "q5"]) &&&
      (((V "q5" === C "true" []) &&&
      (fresh ["q7"] (
         (call "eval" [V "subst", V "expr", V "q7"]) &&&
         (((V "q7" === C "none" []) &&&
         (V "q4" === C "none" [])) |||
         (fresh ["y"] (
            (V "q7" === C "some" [V "y"]) &&&
            (V "q4" === C "some" [V "y"]))))))) |||
      ((V "q5" === C "false" []) &&&
      (V "q4" === C "none" []))))
  )) (
  Let (def "expr1" ["q3"] (
    V "q3" === C "o" []
  )) (
  Let (def "expr2" ["q2"] (
    V "q2" === C "disj" [C "var" [C "z" []], C "i" []]
  )) (
  Let (def "expr3" ["q1"] (
    V "q1" === C "conj_" [C "var" [C "z" []], C "i" []]
  )) (
  Let (def "expr4" ["q0"] (
    V "q0" === C "disj" [C "conj_" [C "var" [C "z" []], C "var" [C "s" [C "z" []]]], C "conj_" [C "var" [C "s" [C "s" [C "z" []]]], C "not_" [C "var" [C "s" [C "s" [C "s" [C "z" []]]]]]]]
  )) (
  last_goal)))))))))))))))))
