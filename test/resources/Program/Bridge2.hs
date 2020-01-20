module Program.Bridge2 where

import qualified Program.Bridge 
import Prelude hiding (succ, max)
import Syntax


env :: String 
env = Program.Bridge.env 

greater :: [Def] 
greater = [greaterDef]

greaterDef :: Def 
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

grForPerson :: [Def] 
grForPerson = [grForPersonDef]

grForPersonDef :: Def 
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

max :: [Def] 
max = maxDef : greater 

maxDef :: Def 
maxDef =
    Def "max" ["a0", "b0", "q102"] (
      fresh ["q103"] (
        (call "greater" [V "a0", V "b0", V "q103"]) &&&
        (((V "q103" === C "true" []) &&&
        (V "a0" === V "q102")) |||
        ((V "q103" === C "false" []) &&&
        (V "b0" === V "q102"))))
    )

add :: [Def] 
add = [addDef]

addDef :: Def 
addDef =
    Def "add" ["a0", "b0", "q100"] (
      ((V "a0" === C "o" []) &&&
      (V "b0" === V "q100")) |||
      (fresh ["x"] (
        (V "a0" === C "s" [V "x"]) &&&
        (call "add" [V "x", C "s" [V "b0"], V "q100"])))
    )

eqForBool :: [Def] 
eqForBool = [eqForBoolDef]

eqForBoolDef :: Def 
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

eqForState :: [Def] 
eqForState = eqForStateDef : eqForBool 

eqForStateDef :: Def 
eqForStateDef = 
    Def "eqForState" ["x", "y", "q70"] (
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

checkPerson :: [Def] 
checkPerson = checkPersonDef : eqForBool

checkPersonDef :: Def 
checkPersonDef = 
    Def "checkPerson" ["state", "person", "q68"] (
      fresh ["l", "a0", "b0"] (
        (V "state" === C "st" [V "l", V "a0", V "b0"]) &&&
        (((V "person" === C "a" []) &&&
        (call "eqForBool" [V "a0", V "l", V "q68"])) |||
        ((V "person" === C "b" []) &&&
        (call "eqForBool" [V "b0", V "l", V "q68"]))))
    )

checkStep :: [Def]
checkStep = checkStepDef : grForPerson ++ checkPerson

checkStepDef :: Def
checkStepDef =
    Def "checkStep" ["state", "step", "q55"] (
      (fresh ["p"] (
          (V "step" === C "one" [V "p"]) &&&
          (call "checkPerson" [V "state", V "p", V "q55"]))) |||
      (fresh ["p", "q"] (
          (V "step" === C "two" [V "p", V "q"]) &&&
          (fresh ["q56", "q57"] (
            (call "checkPerson" [V "state", V "p", V "q56"]) &&&
            (fresh ["q62", "q63"] (
                (call "checkPerson" [V "state", V "q", V "q62"]) &&&
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

moveLight :: [Def]
moveLight = [moveLightDef]

moveLightDef :: Def 
moveLightDef = 
    Def "moveLight" ["state", "q50"] (
      fresh ["l", "a0", "b0"] (
        (V "state" === C "st" [V "l", V "a0", V "b0"]) &&&
        (fresh ["q51"] (
          (V "q50" === C "st" [V "q51", V "a0", V "b0"]) &&&
          (((V "l" === C "true" []) &&&
          (V "q51" === C "false" [])) |||
          ((V "l" === C "false" []) &&&
          (V "q51" === C "true" []))))))
    )

movePerson :: [Def] 
movePerson = [movePersonDef] 

movePersonDef :: Def 
movePersonDef = 
    Def "movePerson" ["state", "person", "q40"] (
      fresh ["l", "a0", "b0"] (
        (V "state" === C "st" [V "l", V "a0", V "b0"]) &&&
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

step :: [Def]
step = stepDef : movePerson ++ moveLight

stepDef :: Def 
stepDef = 
    Def "step" ["state", "step", "q33"] (
      (fresh ["p"] (
        (V "step" === C "one" [V "p"]) &&&
        (fresh ["q34"] (
            (call "movePerson" [V "state", V "p", V "q34"]) &&&
            (call "moveLight" [V "q34", V "q33"]))))) |||
      (fresh ["p", "q"] (
        (V "step" === C "two" [V "p", V "q"]) &&&
        (fresh ["q36"] (
            (fresh ["q38"] (
              (call "movePerson" [V "state", V "p", V "q38"]) &&&
              (call "movePerson" [V "q38", V "q", V "q36"]))) &&&
            (call "moveLight" [V "q36", V "q33"])))))
    )

times :: [Def]
times = [timesDef]

timesDef :: Def 
timesDef = 
    Def "times" ["p", "q30"] (
      ((V "p" === C "a" []) &&&
      (V "q30" === C "s" [C "o" []])) |||
      ((V "p" === C "b" []) &&&
      (V "q30" === C "s" [C "s" [C "o" []]]))
    )

getTime :: [Def] 
getTime = getTimeDef : times ++ max 

getTimeDef :: Def 
getTimeDef = 
    Def "getTime" ["state", "q26"] (
      (fresh ["p"] (
        (V "state" === C "one" [V "p"]) &&&
        (call "times" [V "p", V "q26"]))) |||
      (fresh ["p", "q"] (
        (V "state" === C "two" [V "p", V "q"]) &&&
        (fresh ["q27", "q28"] (
            (call "times" [V "p", V "q27"]) &&&
            (call "times" [V "q", V "q28"]) &&&
            (call "max" [V "q27", V "q28", V "q26"])))))
    )

start :: [Def] 
start = [startDef]

startDef :: Def 
startDef = 
    Def "start" ["q0"] (
      V "q0" === C "st" [C "true" [], C "true" [], C "true" []]
    )

finish :: [Def] 
finish = [finishDef]

finishDef :: Def 
finishDef = 
    Def "finish" ["q1"] (
      V "q1" === C "st" [C "false" [], C "false" [], C "false" []]
    )

getAnswer' :: [Def] 
getAnswer' = getAnswer'Def : checkStep ++ step ++ getTime ++ add ++ finish ++ eqForState 

getAnswer'Def :: Def 
getAnswer'Def = 
    Def "getAnswer'" ["answer", "state", "q2"] (
      (fresh ["x", "xs"] (
         (V "answer" === C "%" [V "x", V "xs"]) &&&
         (fresh ["q4"] (
            (call "checkStep" [V "state", V "x", V "q4"]) &&&
            (((V "q4" === C "true" []) &&&
            (fresh ["q6"] (
               (fresh ["q12"] (
                  (call "step" [V "state", V "x", V "q12"]) &&&
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
            (call "eqForState" [V "state", V "q19", V "q16"]))) &&&
         (((V "q16" === C "true" []) &&&
         (V "q2" === C "some" [C "o" []])) |||
         ((V "q16" === C "false" []) &&&
         (V "q2" === C "none" []))))))
    )

getAnswer :: [Def] 
getAnswer = getAnswerDef : start ++ getAnswer'

getAnswerDef :: Def 
getAnswerDef = 
    Def "getAnswer" ["answer", "q25"] (
      fresh ["q21"] (
        (call "start" [V "q21"]) &&&
        (call "getAnswer'" [V "answer", V "q21", V "q25"]))
    )

game2 :: [Def]
game2 =
  [ greaterDef 
  , grForPersonDef 
  , maxDef 
  , addDef 
  , eqForBoolDef 
  , eqForStateDef 
  , checkPersonDef 
  , checkStepDef 
  , moveLightDef 
  , movePersonDef 
  , stepDef 
  , timesDef 
  , getTimeDef 
  , getAnswerDef 
  ]
