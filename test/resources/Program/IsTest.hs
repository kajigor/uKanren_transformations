module Program.IsTest where

import           Def
import           Prelude hiding (max)
import           Syntax

greater :: [Def G X]
greater = [greaterDef]

greaterDef :: Def G X
greaterDef =
    Def "greater" ["a0", "b0", "q146"] (
      ((V "a0" === C "o" []) &&&
      (V "q146" === C "false" [])) |||
      (fresh ["x"] (
        (V "a0" === C "s" [V "x"]) &&&
        (((V "b0" === C "o" []) &&&
        (V "q146" === C "true" [])) |||
        (fresh ["y"] (
            (V "b0" === C "s" [V "y"]) &&&
            (call "greater" [V "x", V "y", V "q146"]))))))
    )

grForPerson :: [Def G X]
grForPerson = [grForPersonDef]

grForPersonDef :: Def G X
grForPersonDef =
    Def "grForPerson" ["x", "y", "q129"] (
      ((V "x" === C "a" []) &&&
      (((V "y" === C "a" []) &&&
      (V "q129" === C "false" [])) |||
      ((V "y" === C "b" []) &&&
      (V "q129" === C "true" [])) |||
      ((V "y" === C "c" []) &&&
      (V "q129" === C "true" [])) |||
      ((V "y" === C "d" []) &&&
      (V "q129" === C "true" [])))) |||
      ((V "x" === C "b" []) &&&
      (((V "y" === C "a" []) &&&
      (V "q129" === C "false" [])) |||
      ((V "y" === C "b" []) &&&
      (V "q129" === C "false" [])) |||
      ((V "y" === C "c" []) &&&
      (V "q129" === C "false" [])) |||
      ((V "y" === C "d" []) &&&
      (V "q129" === C "true" [])))) |||
      ((V "x" === C "c" []) &&&
      (((V "y" === C "a" []) &&&
      (V "q129" === C "false" [])) |||
      ((V "y" === C "b" []) &&&
      (V "q129" === C "false" [])) |||
      ((V "y" === C "c" []) &&&
      (V "q129" === C "false" [])) |||
      ((V "y" === C "d" []) &&&
      (V "q129" === C "true" [])))) |||
      ((V "x" === C "d" []) &&&
      (V "q129" === C "false" []))
    )

max :: [Def G X]
max = maxDef : greater

maxDef :: Def G X
maxDef =
    Def "max" ["a0", "b0", "q125"] (
      fresh ["q126"] (
        (call "greater" [V "a0", V "b0", V "q126"]) &&&
        (((V "q126" === C "true" []) &&&
        (V "a0" === V "q125")) |||
        ((V "q126" === C "false" []) &&&
        (V "b0" === V "q125"))))
    )

add :: [Def G X]
add = [addDef]

addDef :: Def G X
addDef =
    Def "add" ["a0", "b0", "q123"] (
      ((V "a0" === C "o" []) &&&
      (V "b0" === V "q123")) |||
      (fresh ["x"] (
        (V "a0" === C "s" [V "x"]) &&&
        (call "add" [V "x", C "s" [V "b0"], V "q123"])))
    )

eqForBool :: [Def G X]
eqForBool = [eqForBoolDef]

eqForBoolDef :: Def G X
eqForBoolDef =
    Def "eqForBool" ["a", "b", "q109"] (
      fresh ["q107", "q108"] (
        (((V "a" === C "false" []) &&&
        (V "q107" === C "false" [])) |||
        ((V "a" === C "true" []) &&&
        (V "q107" === V "b"))) &&&
        (fresh ["q116"] (
          (((V "a" === C "true" []) &&&
          (V "q116" === C "true" [])) |||
          ((V "a" === C "false" []) &&&
          (V "q116" === V "b"))) &&&
          (((V "q116" === C "true" []) &&&
          (V "q108" === C "false" [])) |||
          ((V "q116" === C "false" []) &&&
          (V "q108" === C "true" []))))) &&&
        (((V "q107" === C "true" []) &&&
        (V "q109" === C "true" [])) |||
        ((V "q107" === C "false" []) &&&
        (V "q109" === V "q108"))))
    )

eqForEnv :: [Def G X]
eqForEnv = eqForEnvDef : eqForBool

eqForEnvDef :: Def G X
eqForEnvDef =
    Def "eqForEnv" ["x", "y", "q81"] (
      fresh ["l1", "a1", "b1", "c1", "d1"] (
        (V "x" === C "st" [V "l1", V "a1", V "b1", V "c1", V "d1"]) &&&
        (fresh ["l2", "a2", "b2", "c2", "d2"] (
          (V "y" === C "st" [V "l2", V "a2", V "b2", V "c2", V "d2"]) &&&
          (fresh ["q83", "q84"] (
              (call "eqForBool" [V "l1", V "l2", V "q83"]) &&&
              (fresh ["q89", "q90"] (
                (call "eqForBool" [V "a1", V "a2", V "q89"]) &&&
                (fresh ["q95", "q96"] (
                    (call "eqForBool" [V "b1", V "b2", V "q95"]) &&&
                    (fresh ["q101", "q102"] (
                      (call "eqForBool" [V "c1", V "c2", V "q101"]) &&&
                      (call "eqForBool" [V "d1", V "d2", V "q102"]) &&&
                      (((V "q101" === C "false" []) &&&
                      (V "q96" === C "false" [])) |||
                      ((V "q101" === C "true" []) &&&
                      (V "q96" === V "q102"))))) &&&
                    (((V "q95" === C "false" []) &&&
                    (V "q90" === C "false" [])) |||
                    ((V "q95" === C "true" []) &&&
                    (V "q90" === V "q96"))))) &&&
                (((V "q89" === C "false" []) &&&
                (V "q84" === C "false" [])) |||
                ((V "q89" === C "true" []) &&&
                (V "q84" === V "q90"))))) &&&
              (((V "q83" === C "false" []) &&&
              (V "q81" === C "false" [])) |||
              ((V "q83" === C "true" []) &&&
              (V "q81" === V "q84"))))))))
    )

checkPerson :: [Def G X]
checkPerson = checkPersonDef : eqForBool

checkPersonDef :: Def G X
checkPersonDef =
    Def "checkPerson" ["Env", "person", "q79"] (
      fresh ["l", "a0", "b0", "c0", "d0"] (
        (V "Env" === C "st" [V "l", V "a0", V "b0", V "c0", V "d0"]) &&&
        (((V "person" === C "a" []) &&&
        (call "eqForBool" [V "a0", V "l", V "q79"])) |||
        ((V "person" === C "b" []) &&&
        (call "eqForBool" [V "b0", V "l", V "q79"])) |||
        ((V "person" === C "c" []) &&&
        (call "eqForBool" [V "c0", V "l", V "q79"])) |||
        ((V "person" === C "d" []) &&&
        (call "eqForBool" [V "d0", V "l", V "q79"]))))
    )

checkStep :: [Def G X]
checkStep = checkStepDef : checkPerson ++ grForPerson

checkStepDef :: Def G X
checkStepDef =
    Def "checkStep" ["Env", "step", "q66"] (
      (fresh ["p"] (
        (V "step" === C "one" [V "p"]) &&&
        (call "checkPerson" [V "Env", V "p", V "q66"]))) |||
      (fresh ["p", "q"] (
        (V "step" === C "two" [V "p", V "q"]) &&&
        (fresh ["q67", "q68"] (
            (call "checkPerson" [V "Env", V "p", V "q67"]) &&&
            (fresh ["q73", "q74"] (
              (call "checkPerson" [V "Env", V "q", V "q73"]) &&&
              (call "grForPerson" [V "p", V "q", V "q74"]) &&&
              (((V "q73" === C "false" []) &&&
              (V "q68" === C "false" [])) |||
              ((V "q73" === C "true" []) &&&
              (V "q68" === V "q74"))))) &&&
            (((V "q67" === C "false" []) &&&
            (V "q66" === C "false" [])) |||
            ((V "q67" === C "true" []) &&&
            (V "q66" === V "q68")))))))
    )

moveLight :: [Def G X]
moveLight = [moveLightDef]

moveLightDef :: Def G X
moveLightDef =
    Def "moveLight" ["Env", "q61"] (
      fresh ["l", "a0", "b0", "c0", "d0"] (
        (V "Env" === C "st" [V "l", V "a0", V "b0", V "c0", V "d0"]) &&&
        (fresh ["q62"] (
          (V "q61" === C "st" [V "q62", V "a0", V "b0", V "c0", V "d0"]) &&&
          (((V "l" === C "true" []) &&&
          (V "q62" === C "false" [])) |||
          ((V "l" === C "false" []) &&&
          (V "q62" === C "true" []))))))
    )

movePerson :: [Def G X]
movePerson = [movePersonDef]

movePersonDef :: Def G X
movePersonDef =
    Def "movePerson" ["Env", "person", "q43"] (
      fresh ["l", "a0", "b0", "c0", "d0"] (
        (V "Env" === C "st" [V "l", V "a0", V "b0", V "c0", V "d0"]) &&&
        (((V "person" === C "a" []) &&&
        (fresh ["q45"] (
          (V "q43" === C "st" [V "l", V "q45", V "b0", V "c0", V "d0"]) &&&
          (((V "a0" === C "true" []) &&&
          (V "q45" === C "false" [])) |||
          ((V "a0" === C "false" []) &&&
          (V "q45" === C "true" [])))))) |||
        ((V "person" === C "b" []) &&&
        (fresh ["q49"] (
          (V "q43" === C "st" [V "l", V "a0", V "q49", V "c0", V "d0"]) &&&
          (((V "b0" === C "true" []) &&&
          (V "q49" === C "false" [])) |||
          ((V "b0" === C "false" []) &&&
          (V "q49" === C "true" [])))))) |||
        ((V "person" === C "c" []) &&&
        (fresh ["q53"] (
          (V "q43" === C "st" [V "l", V "a0", V "b0", V "q53", V "d0"]) &&&
          (((V "c0" === C "true" []) &&&
          (V "q53" === C "false" [])) |||
          ((V "c0" === C "false" []) &&&
          (V "q53" === C "true" [])))))) |||
        ((V "person" === C "d" []) &&&
        (fresh ["q57"] (
          (V "q43" === C "st" [V "l", V "a0", V "b0", V "c0", V "q57"]) &&&
          (((V "d0" === C "true" []) &&&
          (V "q57" === C "false" [])) |||
          ((V "d0" === C "false" []) &&&
          (V "q57" === C "true" []))))))))
    )

step :: [Def G X]
step = stepDef : movePerson ++ moveLight

stepDef :: Def G X
stepDef =
    Def "step" ["Env", "step", "q36"] (
      (fresh ["p"] (
        (V "step" === C "one" [V "p"]) &&&
        (fresh ["q37"] (
            (call "movePerson" [V "Env", V "p", V "q37"]) &&&
            (call "moveLight" [V "q37", V "q36"]))))) |||
      (fresh ["p", "q"] (
        (V "step" === C "two" [V "p", V "q"]) &&&
        (fresh ["q39"] (
            (fresh ["q41"] (
              (call "movePerson" [V "Env", V "p", V "q41"]) &&&
              (call "movePerson" [V "q41", V "q", V "q39"]))) &&&
            (call "moveLight" [V "q39", V "q36"])))))
    )

times :: [Def G X]
times = [timesDef]

timesDef :: Def G X
timesDef =
    Def "times" ["p", "q31"] (
      ((V "p" === C "a" []) &&&
      (V "q31" === C "s" [C "o" []])) |||
      ((V "p" === C "b" []) &&&
      (V "q31" === C "s" [C "s" [C "o" []]])) |||
      ((V "p" === C "c" []) &&&
      (V "q31" === C "s" [C "s" [C "s" [C "s" [C "s" [C "o" []]]]]])) |||
      ((V "p" === C "d" []) &&&
      (V "q31" === C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "o" []]]]]]]]]]]))
    )

getTime :: [Def G X]
getTime = getTimeDef : times ++ max

getTimeDef :: Def G X
getTimeDef =
    Def "getTime" ["Env", "q27"] (
      (fresh ["p"] (
        (V "Env" === C "one" [V "p"]) &&&
        (call "times" [V "p", V "q27"]))) |||
      (fresh ["p", "q"] (
        (V "Env" === C "two" [V "p", V "q"]) &&&
        (fresh ["q28", "q29"] (
            (call "times" [V "p", V "q28"]) &&&
            (call "times" [V "q", V "q29"]) &&&
            (call "max" [V "q28", V "q29", V "q27"])))))
    )

start :: [Def G X]
start = [startDef]

startDef :: Def G X
startDef =
    Def "start" ["q1"] (V "q1" === C "st" [C "true" [], C "true" [], C "true" [], C "true" [], C "true" []])

finish :: [Def G X]
finish = [finishDef]

finishDef :: Def G X
finishDef =
    Def "finish" ["q2"] (V "q2" === C "st" [C "false" [], C "false" [], C "false" [], C "false" [], C "false" []])

getAnswer' :: [Def G X]
getAnswer' = getAnswer'Def : checkStep ++ step ++ getTime ++ add ++ finish ++ eqForEnv

getAnswer'Def :: Def G X
getAnswer'Def =
    Def "getAnswer'" ["answer", "Env", "q3"] (
      (fresh ["x", "xs"] (
        (V "answer" === C "%" [V "x", V "xs"]) &&&
        (fresh ["q5"] (
            (call "checkStep" [V "Env", V "x", V "q5"]) &&&
            (((V "q5" === C "true" []) &&&
            (fresh ["q7"] (
              (fresh ["q13"] (
                  (call "step" [V "Env", V "x", V "q13"]) &&&
                  (call "getAnswer'" [V "xs", V "q13", V "q7"]))) &&&
              (((V "q7" === C "none" []) &&&
              (V "q3" === C "none" [])) |||
              (fresh ["t1"] (
                  (V "q7" === C "some" [V "t1"]) &&&
                  (fresh ["q9"] (
                    (V "q3" === C "some" [V "q9"]) &&&
                    (fresh ["q11"] (
                        (call "getTime" [V "x", V "q11"]) &&&
                        (call "add" [V "q11", V "t1", V "q9"]))))))))))) |||
            ((V "q5" === C "false" []) &&&
            (V "q3" === C "none" []))))))) |||
      ((V "answer" === C "nil" []) &&&
      (fresh ["q17"] (
        (fresh ["q20"] (
            (call "finish" [V "q20"]) &&&
            (call "eqForEnv" [V "Env", V "q20", V "q17"]))) &&&
        (((V "q17" === C "true" []) &&&
        (V "q3" === C "some" [C "o" []])) |||
        ((V "q17" === C "false" []) &&&
        (V "q3" === C "none" []))))))
    )

getAnswer :: [Def G X]
getAnswer = getAnswerDef : start ++ getAnswer'

getAnswerDef :: Def G X
getAnswerDef =
    Def "getAnswer" ["answer", "q26"] (
      fresh ["q22"] (
        (call "start" [V "q22"]) &&&
        (call "getAnswer'" [V "answer", V "q22", V "q26"])))

result :: [Def G X]
result = [resultDef]

resultDef :: Def G X
resultDef =
    Def "result" ["q0"] (
      V "q0" === C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "s" [C "o" []]]]]]]]]]]]]]]]]]
    )

tree1 :: [Def G X]
tree1 =
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
    , resultDef
    ]

env :: String
env =
  "open MiniKanren\nopen MiniKanrenStd\ntype 'a0 gpeano =\n  | O \n  | S of 'a0 \nlet rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\nmodule For_gpeano =\n  (Fmap)(struct\n           let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gpeano\n         end)\nlet rec o () = inj (For_gpeano.distrib O)\nand s x__0 = inj (For_gpeano.distrib (S x__0))\ntype person =\n  | A \n  | B \n  | C \n  | D \nlet a () = !! A\nlet b () = !! B\nlet c () = !! C\nlet d () = !! D\ntype 'a0 gstep =\n  | One of 'a0 \n  | Two of 'a0 * 'a0 \nlet rec fmap fa0 =\n  function\n  | One a0 -> One (fa0 a0)\n  | Two (a0_0, a0_1) -> Two ((fa0 a0_0), (fa0 a0_1))\nmodule For_gstep =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | One a0 -> One (fa0 a0)\n             | Two (a0_0, a0_1) -> Two ((fa0 a0_0), (fa0 a0_1))\n           type 'a0 t = 'a0 gstep\n         end)\nlet rec one x__0 = inj (For_gstep.distrib (One x__0))\nand two x__0 x__1 = inj (For_gstep.distrib (Two (x__0, x__1)))\ntype 'a0 gEnv =\n  | St of 'a0 * 'a0 * 'a0 * 'a0 * 'a0 \nlet rec fmap fa0 =\n  function\n  | St (a0_0, a0_1, a0_2, a0_3, a0_4) ->\n      St ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3), (fa0 a0_4))\nmodule For_gEnv =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | St (a0_0, a0_1, a0_2, a0_3, a0_4) ->\n                 St\n                   ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3),\n                     (fa0 a0_4))\n           type 'a0 t = 'a0 gEnv\n         end)\nlet rec st x__0 x__1 x__2 x__3 x__4 =\n  inj (For_gEnv.distrib (St (x__0, x__1, x__2, x__3, x__4)))"
