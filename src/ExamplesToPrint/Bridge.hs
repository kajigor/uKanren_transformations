module ExamplesToPrint.Bridge where

import Prelude hiding (succ, max)
import Syntax
import Program
import Def

query = Program topLevelBigBridge (fresh ["a", "b"] (call "tlBigBridge" [V "a", V "b"]))

env = "open MiniKanren\nopen MiniKanrenStd\ntype 'a0 gpeano =\n  | O \n  | S of 'a0 \nlet rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\nmodule For_gpeano =\n  (Fmap)(struct\n           let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gpeano\n         end)\nlet rec o () = inj (For_gpeano.distrib O)\nand s x__0 = inj (For_gpeano.distrib (S x__0))\ntype person =\n  | A \n  | B \n  | C \n  | D \nlet a () = !! A\nlet b () = !! B\nlet c () = !! C\nlet d () = !! D\ntype 'a0 gstep =\n  | One of 'a0 \n  | Two of 'a0 * 'a0 \nlet rec fmap fa0 =\n  function\n  | One a0 -> One (fa0 a0)\n  | Two (a0_0, a0_1) -> Two ((fa0 a0_0), (fa0 a0_1))\nmodule For_gstep =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | One a0 -> One (fa0 a0)\n             | Two (a0_0, a0_1) -> Two ((fa0 a0_0), (fa0 a0_1))\n           type 'a0 t = 'a0 gstep\n         end)\nlet rec One x__0 = inj (For_gstep.distrib (One x__0))\nand Two x__0 x__1 = inj (For_gstep.distrib (Two (x__0, x__1)))\ntype 'a0 gEnv =\n  | St of 'a0 * 'a0 * 'a0 * 'a0 * 'a0 \nlet rec fmap fa0 =\n  function\n  | St (a0_0, a0_1, a0_2, a0_3, a0_4) ->\n      St ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3), (fa0 a0_4))\nmodule For_gEnv =\n  (Fmap)(struct\n           let rec fmap fa0 =\n             function\n             | St (a0_0, a0_1, a0_2, a0_3, a0_4) ->\n                 St\n                   ((fa0 a0_0), (fa0 a0_1), (fa0 a0_2), (fa0 a0_3),\n                     (fa0 a0_4))\n           type 'a0 t = 'a0 gEnv\n         end)\nlet rec st x__0 x__1 x__2 x__3 x__4 =\n  inj (For_gEnv.distrib (St (x__0, x__1, x__2, x__3, x__4)))"

topLevelBigBridge :: [Def G X]
topLevelBigBridge = topLevelBigBridgeDef : result ++ getAnswer

topLevelBigBridgeDef :: Def G X
topLevelBigBridgeDef =
    Def "tlBigBridge" ["a", "b"] (
      call "result" [V "b"] &&& call "getAnswer" [V "a", C "Some" [V "b"]]
    )

-- fresh ["a", "b"]  (call "result" [V "b"] &&& call "getAnswer" [V "a", C "Some" [V "b"]])

greater :: [Def G X]
greater = [greaterDef]

greaterDef :: Def G X
greaterDef =
    Def "greater" ["a0", "b0", "q146"] (
      ((V "a0" === C "Zero" []) &&&
      (V "q146" === C "False" [])) |||
      (fresh ["x"] (
        (V "a0" === C "Succ" [V "x"]) &&&
        (((V "b0" === C "Zero" []) &&&
        (V "q146" === C "True" [])) |||
        (fresh ["y"] (
            (V "b0" === C "Succ" [V "y"]) &&&
            (call "greater" [V "x", V "y", V "q146"]))))))
    )


grForPerson :: [Def G X]
grForPerson = [grForPersonDef]

grForPersonDef :: Def G X
grForPersonDef =
    Def "grForPerson" ["x", "y", "q129"] (
      ((V "x" === C "A" []) &&&
      (((V "y" === C "A" []) &&&
      (V "q129" === C "False" [])) |||
      ((V "y" === C "B" []) &&&
      (V "q129" === C "True" [])) |||
      ((V "y" === C "C" []) &&&
      (V "q129" === C "True" [])) |||
      ((V "y" === C "D" []) &&&
      (V "q129" === C "True" [])))) |||
      ((V "x" === C "B" []) &&&
      (((V "y" === C "A" []) &&&
      (V "q129" === C "False" [])) |||
      ((V "y" === C "B" []) &&&
      (V "q129" === C "False" [])) |||
      ((V "y" === C "C" []) &&&
      (V "q129" === C "False" [])) |||
      ((V "y" === C "D" []) &&&
      (V "q129" === C "True" [])))) |||
      ((V "x" === C "C" []) &&&
      (((V "y" === C "A" []) &&&
      (V "q129" === C "False" [])) |||
      ((V "y" === C "B" []) &&&
      (V "q129" === C "False" [])) |||
      ((V "y" === C "C" []) &&&
      (V "q129" === C "False" [])) |||
      ((V "y" === C "D" []) &&&
      (V "q129" === C "True" [])))) |||
      ((V "x" === C "D" []) &&&
      (V "q129" === C "False" []))
    )

max :: [Def G X]
max = maxDef : greater

maxDef :: Def G X
maxDef =
    Def "max" ["a0", "b0", "q125"] (
      fresh ["q126"] (
        (call "greater" [V "a0", V "b0", V "q126"]) &&&
        (((V "q126" === C "True" []) &&&
        (V "a0" === V "q125")) |||
        ((V "q126" === C "False" []) &&&
        (V "b0" === V "q125"))))
    )

add :: [Def G X]
add = [addDef]

-- addDef :: Def G X
-- addDef =
--     Def "add" ["a0", "b0", "q123"] (
--       ((V "a0" === C "Zero" []) &&&
--       (V "b0" === V "q123")) |||
--       (fresh ["x"] (
--           (V "a0" === C "Succ" [V "x"]) &&&
--           (call "add" [V "x", C "Succ" [V "b0"], V "q123"])))
--     )


addDef :: Def G X
addDef =
    ( Def "add" ["x", "y", "z"]
        (
          x === C "Zero" []  &&& z === y |||
          fresh ["x'", "z'"]
            (x === C "Succ" [x'] &&& z === C "Succ" [z'] &&& call "add" [x', y, z'])
        )
    )
  where
    [x, y, z, x', z'] = map V ["x", "y", "z", "x'", "z'"]

eqForBool :: [Def G X]
eqForBool = [eqForBoolDef]

eqForBoolDef :: Def G X
eqForBoolDef =
    Def "eqForBool" ["a", "b", "q109"] (
      fresh ["q107", "q108"] (
        (((V "a" === C "False" []) &&&
        (V "q107" === C "False" [])) |||
        ((V "a" === C "True" []) &&&
        (V "q107" === V "b"))) &&&
        (fresh ["q116"] (
          (((V "a" === C "True" []) &&&
          (V "q116" === C "True" [])) |||
          ((V "a" === C "False" []) &&&
          (V "q116" === V "b"))) &&&
          (((V "q116" === C "True" []) &&&
          (V "q108" === C "False" [])) |||
          ((V "q116" === C "False" []) &&&
          (V "q108" === C "True" []))))) &&&
        (((V "q107" === C "True" []) &&&
        (V "q109" === C "True" [])) |||
        ((V "q107" === C "False" []) &&&
        (V "q109" === V "q108"))))
    )

eqForEnv :: [Def G X]
eqForEnv = eqForEnvDef : eqForBool

eqForEnvDef :: Def G X
eqForEnvDef =
    Def "eqForEnv" ["x", "y", "q81"] (
      fresh ["l1", "a1", "b1", "c1", "d1"] (
        (V "x" === C "St" [V "l1", V "a1", V "b1", V "c1", V "d1"]) &&&
        (fresh ["l2", "a2", "b2", "c2", "d2"] (
          (V "y" === C "St" [V "l2", V "a2", V "b2", V "c2", V "d2"]) &&&
          (fresh ["q83", "q84"] (
              (call "eqForBool" [V "l1", V "l2", V "q83"]) &&&
              (fresh ["q89", "q90"] (
                (call "eqForBool" [V "a1", V "a2", V "q89"]) &&&
                (fresh ["q95", "q96"] (
                    (call "eqForBool" [V "b1", V "b2", V "q95"]) &&&
                    (fresh ["q101", "q102"] (
                      (call "eqForBool" [V "c1", V "c2", V "q101"]) &&&
                      (call "eqForBool" [V "d1", V "d2", V "q102"]) &&&
                      (((V "q101" === C "False" []) &&&
                      (V "q96" === C "False" [])) |||
                      ((V "q101" === C "True" []) &&&
                      (V "q96" === V "q102"))))) &&&
                    (((V "q95" === C "False" []) &&&
                    (V "q90" === C "False" [])) |||
                    ((V "q95" === C "True" []) &&&
                    (V "q90" === V "q96"))))) &&&
                (((V "q89" === C "False" []) &&&
                (V "q84" === C "False" [])) |||
                ((V "q89" === C "True" []) &&&
                (V "q84" === V "q90"))))) &&&
              (((V "q83" === C "False" []) &&&
              (V "q81" === C "False" [])) |||
              ((V "q83" === C "True" []) &&&
              (V "q81" === V "q84"))))))))
    )


checkPerson :: [Def G X]
checkPerson = checkPersonDef : eqForBool

checkPersonDef :: Def G X
checkPersonDef =
    Def "checkPerson" ["env", "person", "q79"] (
      fresh ["l", "a0", "b0", "c0", "d0"] (
        (V "env" === C "St" [V "l", V "a0", V "b0", V "c0", V "d0"]) &&&
        (((V "person" === C "A" []) &&&
        (call "eqForBool" [V "a0", V "l", V "q79"])) |||
        ((V "person" === C "B" []) &&&
        (call "eqForBool" [V "b0", V "l", V "q79"])) |||
        ((V "person" === C "C" []) &&&
        (call "eqForBool" [V "c0", V "l", V "q79"])) |||
        ((V "person" === C "D" []) &&&
        (call "eqForBool" [V "d0", V "l", V "q79"]))))
    )

checkStep :: [Def G X]
checkStep = checkStepDef : checkPerson ++ grForPerson

checkStepDef :: Def G X
checkStepDef =
    Def "checkStep" ["env", "step", "q66"] (
      (fresh ["p"] (
        (V "step" === C "One" [V "p"]) &&&
        (call "checkPerson" [V "env", V "p", V "q66"]))) |||
      (fresh ["p", "q"] (
        (V "step" === C "Two" [V "p", V "q"]) &&&
        (fresh ["q67", "q68"] (
            (call "checkPerson" [V "env", V "p", V "q67"]) &&&
            (fresh ["q73", "q74"] (
              (call "checkPerson" [V "env", V "q", V "q73"]) &&&
              (call "grForPerson" [V "p", V "q", V "q74"]) &&&
              (((V "q73" === C "False" []) &&&
              (V "q68" === C "False" [])) |||
              ((V "q73" === C "True" []) &&&
              (V "q68" === V "q74"))))) &&&
            (((V "q67" === C "False" []) &&&
            (V "q66" === C "False" [])) |||
            ((V "q67" === C "True" []) &&&
            (V "q66" === V "q68")))))))
    )

moveLight :: [Def G X]
moveLight = [moveLightDef]

moveLightDef :: Def G X
moveLightDef =
    Def "moveLight" ["env", "q61"] (
      fresh ["l", "a0", "b0", "c0", "d0"] (
        (V "env" === C "St" [V "l", V "a0", V "b0", V "c0", V "d0"]) &&&
        (fresh ["q62"] (
          (V "q61" === C "St" [V "q62", V "a0", V "b0", V "c0", V "d0"]) &&&
          (((V "l" === C "True" []) &&&
          (V "q62" === C "False" [])) |||
          ((V "l" === C "False" []) &&&
          (V "q62" === C "True" []))))))
    )

movePerson :: [Def G X]
movePerson = [movePersonDef]

movePersonDef :: Def G X
movePersonDef =
    Def "movePerson" ["env", "person", "q43"] (
      fresh ["l", "a0", "b0", "c0", "d0"] (
        (V "env" === C "St" [V "l", V "a0", V "b0", V "c0", V "d0"]) &&&
        (((V "person" === C "A" []) &&&
        (fresh ["q45"] (
          (V "q43" === C "St" [V "l", V "q45", V "b0", V "c0", V "d0"]) &&&
          (((V "a0" === C "True" []) &&&
          (V "q45" === C "False" [])) |||
          ((V "a0" === C "False" []) &&&
          (V "q45" === C "True" [])))))) |||
        ((V "person" === C "B" []) &&&
        (fresh ["q49"] (
          (V "q43" === C "St" [V "l", V "a0", V "q49", V "c0", V "d0"]) &&&
          (((V "b0" === C "True" []) &&&
          (V "q49" === C "False" [])) |||
          ((V "b0" === C "False" []) &&&
          (V "q49" === C "True" [])))))) |||
        ((V "person" === C "C" []) &&&
        (fresh ["q53"] (
          (V "q43" === C "St" [V "l", V "a0", V "b0", V "q53", V "d0"]) &&&
          (((V "c0" === C "True" []) &&&
          (V "q53" === C "False" [])) |||
          ((V "c0" === C "False" []) &&&
          (V "q53" === C "True" [])))))) |||
        ((V "person" === C "D" []) &&&
        (fresh ["q57"] (
          (V "q43" === C "St" [V "l", V "a0", V "b0", V "c0", V "q57"]) &&&
          (((V "d0" === C "True" []) &&&
          (V "q57" === C "False" [])) |||
          ((V "d0" === C "False" []) &&&
          (V "q57" === C "True" []))))))))
    )

step :: [Def G X]
step = stepDef : movePerson ++ moveLight

stepDef :: Def G X
stepDef =
    Def "step" ["env", "step", "q36"] (
      (fresh ["p"] (
        (V "step" === C "One" [V "p"]) &&&
        (fresh ["q37"] (
            (call "movePerson" [V "env", V "p", V "q37"]) &&&
            (call "moveLight" [V "q37", V "q36"]))))) |||
      (fresh ["p", "q"] (
        (V "step" === C "Two" [V "p", V "q"]) &&&
        (fresh ["q39"] (
            (fresh ["q41"] (
              (call "movePerson" [V "env", V "p", V "q41"]) &&&
              (call "movePerson" [V "q41", V "q", V "q39"]))) &&&
            (call "moveLight" [V "q39", V "q36"])))))
    )

times :: [Def G X]
times = [timesDef]

timesDef :: Def G X
timesDef =
    Def "times" ["p", "q31"] (
      ((V "p" === C "A" []) &&&
      (V "q31" === C "Succ" [C "Zero" []])) |||
      ((V "p" === C "B" []) &&&
      (V "q31" === C "Succ" [C "Succ" [C "Zero" []]])) |||
      ((V "p" === C "C" []) &&&
      (V "q31" === C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Zero" []]]]]])) |||
      ((V "p" === C "D" []) &&&
      (V "q31" === C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Zero" []]]]]]]]]]]))
    )

getTime :: [Def G X]
getTime = getTimeDef : times ++ max

getTimeDef :: Def G X
getTimeDef =
    Def "getTime" ["env", "q27"] (
      (fresh ["p"] (
        (V "env" === C "One" [V "p"]) &&&
        (call "times" [V "p", V "q27"]))) |||
      (fresh ["p", "q"] (
        (V "env" === C "Two" [V "p", V "q"]) &&&
        (fresh ["q28", "q29"] (
            (call "times" [V "p", V "q28"]) &&&
            (call "times" [V "q", V "q29"]) &&&
            (call "max" [V "q28", V "q29", V "q27"])))))
    )

start :: [Def G X]
start = [startDef]

startDef :: Def G X
startDef =
    Def "start" ["q1"] (
        V "q1" === C "St" [C "True" [], C "True" [], C "True" [], C "True" [], C "True" []]
    )

finish :: [Def G X]
finish = [finishDef]

finishDef :: Def G X
finishDef =
    Def "finish" ["q2"] (
      V "q2" === C "St" [C "False" [], C "False" [], C "False" [], C "False" [], C "False" []]
    )

getAnswer' :: [Def G X]
getAnswer' = getAnswer'Def : checkStep ++ finish ++ eqForEnv ++ add ++ step ++ getTime

getAnswer'Def :: Def G X
getAnswer'Def =
    Def "getAnswer'" ["answer", "env", "q3"] (
      (fresh ["x", "xs"] (
         (V "answer" === C "%" [V "x", V "xs"]) &&&
         (fresh ["q5"] (
            (call "checkStep" [V "env", V "x", V "q5"]) &&&
            (((V "q5" === C "True" []) &&&
            (fresh ["q7"] (
               (fresh ["q13"] (
                  (call "step" [V "env", V "x", V "q13"]) &&&
                  (call "getAnswer'" [V "xs", V "q13", V "q7"]))) &&&
               (((V "q7" === C "NOne" []) &&&
               (V "q3" === C "NOne" [])) |||
               (fresh ["t1"] (
                  (V "q7" === C "Some" [V "t1"]) &&&
                  (fresh ["q9"] (
                     (V "q3" === C "Some" [V "q9"]) &&&
                     (fresh ["q11"] (
                        (call "getTime" [V "x", V "q11"]) &&&
                        (call "add" [V "q11", V "t1", V "q9"]))))))))))) |||
            ((V "q5" === C "False" []) &&&
            (V "q3" === C "NOne" []))))))) |||
      ((V "answer" === C "nil" []) &&&
      (fresh ["q17"] (
         (fresh ["q20"] (
            (call "finish" [V "q20"]) &&&
            (call "eqForEnv" [V "env", V "q20", V "q17"]))) &&&
         (((V "q17" === C "True" []) &&&
         (V "q3" === C "Some" [C "Zero" []])) |||
         ((V "q17" === C "False" []) &&&
         (V "q3" === C "NOne" []))))))
    )

getAnswer :: [Def G X]
getAnswer = getAnswerDef : start ++ getAnswer'

getAnswerDef :: Def G X
getAnswerDef =
    Def "getAnswer" ["answer", "q26"] (
      fresh ["q22"] (
        (call "start" [V "q22"]) &&&
        (call "getAnswer'" [V "answer", V "q22", V "q26"]))
    )

result :: [Def G X]
result = [resultDef]

resultDef :: Def G X
resultDef =
    Def "result" ["q0"] (
      V "q0" === C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Succ" [C "Zero" []]]]]]]]]]]]]]]]]]
    )

game2Big :: [Def G X]
game2Big =
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
  , getAnswer'Def
  , resultDef
  ]