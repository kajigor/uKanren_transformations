module Program.PairBridge where

import Prelude hiding (succ, max)
import Syntax
import Program
import Def

greater :: [Def G X]
greater = [greaterDef]

greaterDef :: Def G X
greaterDef =
    Def "greater" ["a0", "b0", "q113"] (
      ((V "a0" === C "o" []) &&&
      (V "q113" === C "false" [])) |||
      (fresh ["x"] (
        (V "a0" === C "s" [V "x"]) &&&
        (((V "b0" === C "o" []) &&&
        (V "q113" === C "true" [])) |||
        (fresh ["y"] (
            (V "b0" === C "s" [V "y"]) &&&
            (call "greater" [V "x", V "y", V "q113"]))))))
    )

grForPerson :: [Def G X]
grForPerson = [grForPersonDef]

grForPersonDef :: Def G X
grForPersonDef =
    Def "grForPerson" ["x", "y", "q106"] (
      ((V "x" === C "a" []) &&&
      (((V "y" === C "a" []) &&&
      (V "q106" === C "false" [])) |||
      ((V "y" === C "b" []) &&&
      (V "q106" === C "true" [])))) |||
      ((V "x" === C "b" []) &&&
      (((V "y" === C "a" []) &&&
      (V "q106" === C "false" [])) |||
      ((V "y" === C "b" []) &&&
      (V "q106" === C "false" []))))
    )

max :: [Def G X]
max = maxDef : greater

maxDef :: Def G X
maxDef =
    Def "max" ["a0", "b0", "q102"] (
      fresh ["q103"] (
        (call "greater" [V "a0", V "b0", V "q103"]) &&&
        (((V "q103" === C "true" []) &&&
        (V "a0" === V "q102")) |||
        ((V "q103" === C "false" []) &&&
        (V "b0" === V "q102"))))
    )

add :: [Def G X]
add = [addDef]

addDef :: Def G X
addDef =
    Def "add" ["a0", "b0", "q100"] (
      ((V "a0" === C "o" []) &&&
      (V "b0" === V "q100")) |||
      (fresh ["x"] (
        (V "a0" === C "s" [V "x"]) &&&
        (call "add" [V "x", C "s" [V "b0"], V "q100"])))
    )

eqForBool :: [Def G X]
eqForBool = [eqForBoolDef]

eqForBoolDef :: Def G X
eqForBoolDef =
    Def "eqForBool" ["a", "b", "q86"] (
      fresh ["q84", "q85"] (
        (((V "a" === C "false" []) &&&
        (V "q84" === C "false" [])) |||
        ((V "a" === C "true" []) &&&
        (V "q84" === V "b"))) &&&
        (fresh ["q93"] (
          (((V "a" === C "true" []) &&&
          (V "q93" === C "true" [])) |||
          ((V "a" === C "false" []) &&&
          (V "q93" === V "b"))) &&&
          (((V "q93" === C "true" []) &&&
          (V "q85" === C "false" [])) |||
          ((V "q93" === C "false" []) &&&
          (V "q85" === C "true" []))))) &&&
        (((V "q84" === C "true" []) &&&
        (V "q86" === C "true" [])) |||
        ((V "q84" === C "false" []) &&&
        (V "q86" === V "q85"))))
    )

eqForEnv :: [Def G X]
eqForEnv = eqForEnvDef : eqForBool

eqForEnvDef :: Def G X
eqForEnvDef =
    Def "eqForEnv" ["x", "y", "q70"] (
      fresh ["l1", "a1", "b1"] (
        (V "x" === C "st" [V "l1", V "a1", V "b1"]) &&&
        (fresh ["l2", "a2", "b2"] (
          (V "y" === C "st" [V "l2", V "a2", V "b2"]) &&&
          (fresh ["q72", "q73"] (
              (call "eqForBool" [V "l1", V "l2", V "q72"]) &&&
              (fresh ["q78", "q79"] (
                (call "eqForBool" [V "a1", V "a2", V "q78"]) &&&
                (call "eqForBool" [V "b1", V "b2", V "q79"]) &&&
                (((V "q78" === C "false" []) &&&
                (V "q73" === C "false" [])) |||
                ((V "q78" === C "true" []) &&&
                (V "q73" === V "q79"))))) &&&
              (((V "q72" === C "false" []) &&&
              (V "q70" === C "false" [])) |||
              ((V "q72" === C "true" []) &&&
              (V "q70" === V "q73"))))))))
    )

checkPerson :: [Def G X]
checkPerson = checkPersonDef : eqForBool

checkPersonDef :: Def G X
checkPersonDef =
    Def "checkPerson" ["Env", "person", "q68"] (
      fresh ["l", "a0", "b0"] (
        (V "Env" === C "st" [V "l", V "a0", V "b0"]) &&&
        (((V "person" === C "a" []) &&&
        (call "eqForBool" [V "a0", V "l", V "q68"])) |||
        ((V "person" === C "b" []) &&&
        (call "eqForBool" [V "b0", V "l", V "q68"]))))
    )

checkStep :: [Def G X]
checkStep = checkStepDef : checkPerson ++ grForPerson

checkStepDef :: Def G X
checkStepDef =
    Def "checkStep" ["Env", "step", "q55"] (
      (fresh ["p"] (
        (V "step" === C "one" [V "p"]) &&&
        (call "checkPerson" [V "Env", V "p", V "q55"]))) |||
      (fresh ["p", "q"] (
        (V "step" === C "two" [V "p", V "q"]) &&&
        (fresh ["q56", "q57"] (
            (call "checkPerson" [V "Env", V "p", V "q56"]) &&&
            (fresh ["q62", "q63"] (
              (call "checkPerson" [V "Env", V "q", V "q62"]) &&&
              (call "grForPerson" [V "p", V "q", V "q63"]) &&&
              (((V "q62" === C "false" []) &&&
              (V "q57" === C "false" [])) |||
              ((V "q62" === C "true" []) &&&
              (V "q57" === V "q63"))))) &&&
            (((V "q56" === C "false" []) &&&
            (V "q55" === C "false" [])) |||
            ((V "q56" === C "true" []) &&&
            (V "q55" === V "q57")))))))
    )

moveLight :: [Def G X]
moveLight = [moveLightDef]

moveLightDef :: Def G X
moveLightDef =
    Def "moveLight" ["Env", "q50"] (
      fresh ["l", "a0", "b0"] (
        (V "Env" === C "st" [V "l", V "a0", V "b0"]) &&&
        (fresh ["q51"] (
          (V "q50" === C "st" [V "q51", V "a0", V "b0"]) &&&
          (((V "l" === C "true" []) &&&
          (V "q51" === C "false" [])) |||
          ((V "l" === C "false" []) &&&
          (V "q51" === C "true" []))))))
    )


movePerson :: [Def G X]
movePerson = [movePersonDef]

movePersonDef :: Def G X
movePersonDef =
    Def "movePerson" ["Env", "person", "q40"] (
      fresh ["l", "a0", "b0"] (
        (V "Env" === C "st" [V "l", V "a0", V "b0"]) &&&
        (((V "person" === C "a" []) &&&
        (fresh ["q42"] (
            (V "q40" === C "st" [V "l", V "q42", V "b0"]) &&&
            (((V "a0" === C "true" []) &&&
            (V "q42" === C "false" [])) |||
            ((V "a0" === C "false" []) &&&
            (V "q42" === C "true" [])))))) |||
        ((V "person" === C "b" []) &&&
        (fresh ["q46"] (
            (V "q40" === C "st" [V "l", V "a0", V "q46"]) &&&
            (((V "b0" === C "true" []) &&&
            (V "q46" === C "false" [])) |||
            ((V "b0" === C "false" []) &&&
            (V "q46" === C "true" []))))))))
    )

step :: [Def G X]
step = stepDef : movePerson ++ moveLight

stepDef :: Def G X
stepDef =
    Def "step" ["Env", "step", "q33"] (
      (fresh ["p"] (
        (V "step" === C "one" [V "p"]) &&&
        (fresh ["q34"] (
            (call "movePerson" [V "Env", V "p", V "q34"]) &&&
            (call "moveLight" [V "q34", V "q33"]))))) |||
      (fresh ["p", "q"] (
        (V "step" === C "two" [V "p", V "q"]) &&&
        (fresh ["q36"] (
            (fresh ["q38"] (
              (call "movePerson" [V "Env", V "p", V "q38"]) &&&
              (call "movePerson" [V "q38", V "q", V "q36"]))) &&&
            (call "moveLight" [V "q36", V "q33"])))))
    )

times :: [Def G X]
times = [timesDef]

timesDef :: Def G X
timesDef =
    Def "times" ["p", "q30"] (
      ((V "p" === C "a" []) &&&
      (V "q30" === C "s" [C "o" []])) |||
      ((V "p" === C "b" []) &&&
      (V "q30" === C "s" [C "s" [C "o" []]]))
    )

getTime :: [Def G X]
getTime = getTimeDef : times ++ max

getTimeDef :: Def G X
getTimeDef =
    Def "getTime" ["Env", "q26"] (
      (fresh ["p"] (
        (V "Env" === C "one" [V "p"]) &&&
        (call "times" [V "p", V "q26"]))) |||
      (fresh ["p", "q"] (
        (V "Env" === C "two" [V "p", V "q"]) &&&
        (fresh ["q27", "q28"] (
            (call "times" [V "p", V "q27"]) &&&
            (call "times" [V "q", V "q28"]) &&&
            (call "max" [V "q27", V "q28", V "q26"])))))
    )

start :: [Def G X]
start = [startDef]

startDef :: Def G X
startDef =
    Def "start" ["q0"] (
      V "q0" === C "st" [C "true" [], C "true" [], C "true" []]
    )

finish :: [Def G X]
finish = [finishDef]

finishDef :: Def G X
finishDef =
    Def "finish" ["q1"] (
      V "q1" === C "st" [C "false" [], C "false" [], C "false" []]
    )

getAnswer' :: [Def G X]
getAnswer' = getAnswer'Def : checkStep ++ step ++ getTime ++ add ++ finish ++ eqForEnv

getAnswer'Def :: Def G X
getAnswer'Def =
    Def "getAnswer'" ["answer", "Env", "q2"] (
      (fresh ["x", "xs"] (
         (V "answer" === C "%" [V "x", V "xs"]) &&&
         (fresh ["q4"] (
            (call "checkStep" [V "Env", V "x", V "q4"]) &&&
            (((V "q4" === C "true" []) &&&
            (fresh ["q6"] (
               (fresh ["q12"] (
                  (call "step" [V "Env", V "x", V "q12"]) &&&
                  (call "getAnswer'" [V "xs", V "q12", V "q6"]))) &&&
               (((V "q6" === C "none" []) &&&
               (V "q2" === C "none" [])) |||
               (fresh ["t1"] (
                  (V "q6" === C "some" [V "t1"]) &&&
                  (fresh ["q8"] (
                     (V "q2" === C "some" [V "q8"]) &&&
                     (fresh ["q10"] (
                        (call "getTime" [V "x", V "q10"]) &&&
                        (call "add" [V "q10", V "t1", V "q8"]))))))))))) |||
            ((V "q4" === C "false" []) &&&
            (V "q2" === C "none" []))))))) |||
      ((V "answer" === C "nil" []) &&&
      (fresh ["q16"] (
         (fresh ["q19"] (
            (call "finish" [V "q19"]) &&&
            (call "eqForEnv" [V "Env", V "q19", V "q16"]))) &&&
         (((V "q16" === C "true" []) &&&
         (V "q2" === C "some" [C "o" []])) |||
         ((V "q16" === C "false" []) &&&
         (V "q2" === C "none" []))))))
    )

getAnswer :: [Def G X]
getAnswer = getAnswerDef : start ++ getAnswer'

getAnswerDef :: Def G X
getAnswerDef =
    Def "getAnswer" ["answer", "q25"] (
      fresh ["q21"] (
        (call "start" [V "q21"]) &&&
        (call "getAnswer'" [V "answer", V "q21", V "q25"]))
    )

pair_bridge :: [Def G X]
pair_bridge =
  [ greaterDef
  , grForPersonDef
  , maxDef
  , addDef
  , eqForBoolDef
  , eqForEnvDef
  , checkPersonDef
  , checkStepDef
  , moveLightDef
  , movePersonDef
  , stepDef
  , timesDef
  , getTimeDef
  , getAnswerDef
  , startDef
  , finishDef
  , getAnswerDef
  ]

env :: String
env =
  "open MiniKanren\nopen MiniKanrenStd\ntype 'a0 gpeano =\n  | O \n  | S of 'a0 \nlet rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\nmodule For_gpeano =\n  (Fmap)(struct\n           let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gpeano\n         end)\nlet rec o () = inj (For_gpeano.distrib O)\nand s x__0 = inj (For_gpeano.distrib (S x__0))\ntype person =\n  | A \n  | B \nlet a () = !! A\nlet b () = !! B\ntype 'a0 gstep =\n  | One of 'a0 \n  | Two of 'a0 * 'a0 \nlet rec fmap fa0 =\n  function\n  | One a0 -> One (fa0 a0)\n  | Two (a0_0, a0_1) -> Two ((fa0 a0_0), (fa0 a0_1))\nmodule For_gstep =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | One a0 -> One (fa0 a0)\n             | Two (a0_0, a0_1) -> Two ((fa0 a0_0), (fa0 a0_1))\n           type 'a0 t = 'a0 gstep\n         end)\nlet rec one x__0 = inj (For_gstep.distrib (One x__0))\nand two x__0 x__1 = inj (For_gstep.distrib (Two (x__0, x__1)))\ntype 'a0 gEnv =\n  | St of 'a0 * 'a0 * 'a0 \nlet rec fmap fa0 =\n  function | St (a0_0, a0_1, a0_2) -> St ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2))\nmodule For_gEnv =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | St (a0_0, a0_1, a0_2) ->\n                 St ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2))\n           type 'a0 t = 'a0 gEnv\n         end)\nlet rec st x__0 x__1 x__2 = inj (For_gEnv.distrib (St (x__0, x__1, x__2)))"
