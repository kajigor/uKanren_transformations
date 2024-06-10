module Program.Bottles where

import           Def
import           Program
import           Syntax

true = C "true" []

six = ofInt 6
four = ofInt 4
nine = ofInt 9
seven = ofInt 7

ofInt 0 = C "o" []
ofInt n = C "s" [ofInt $ n - 1]

query = Program bottles $ fresh ["a", "b", "c"] (call "checkAnswer" [V "a", V "b", V "c", true])

query' =
    Program definition $ fresh ["q", "res"] (call "query" [V "q", V "res"])

queryEq = Program fancyEq $ fresh ["x", "y"] (call "fancyEq" [V "x", V "y", true])

definition :: [Def G X]
definition = definitionDef : bottles

definitionDef :: Def G X
definitionDef =
    Def "query" ["q", "res"] (
      call "capacities1" [V "q"] &&& call "checkAnswer" [V "res", V "q", seven, true])


add :: [Def G X]
add = [addDef]

-- addDef :: Def G X
-- addDef =
--     Def "add" ["a", "b", "q113"] (
--       ((V "a" === C "o" []) &&&
--       (V "b" === V "q113")) |||
--       (fresh ["x"] (
--         (V "a" === C "s" [V "x"]) &&&
--         (call "add" [V "x", C "s" [V "b"], V "q113"])))
--     )

addDef :: Def G X
addDef =
    ( Def "add" ["x", "y", "z"]
        (
          x === C "o" []  &&& z === y |||
          fresh ["x'", "z'"]
            (x === C "s" [x'] &&& z === C "s" [z'] &&& call "add" [x', y, z'])
        )
    )
  where
    [x, y, z, x', z'] = map V ["x", "y", "z", "x'", "z'"]

greater :: [Def G X]
greater = [greaterDef]

greaterDef :: Def G X
greaterDef =
    Def "greater" ["a", "b", "q109"] (
      ((V "a" === C "o" []) &&&
      (V "q109" === C "false" [])) |||
      (fresh ["x"] (
        (V "a" === C "s" [V "x"]) &&&
        (((V "b" === C "o" []) &&&
        (V "q109" === C "true" [])) |||
        (fresh ["y"] (
            (V "b" === C "s" [V "y"]) &&&
            (call "greater" [V "x", V "y", V "q109"]))))))
    )

sub :: [Def G X]
sub = [subDef]

subDef :: Def G X
subDef =
    Def "sub" ["a", "b", "q105"] (
      ((V "b" === C "o" []) &&&
      (V "a" === V "q105")) |||
      (fresh ["y"] (
        (V "b" === C "s" [V "y"]) &&&
        (((V "a" === C "o" []) &&&
        (V "q105" === C "o" [])) |||
        (fresh ["x"] (
            (V "a" === C "s" [V "x"]) &&&
            (call "sub" [V "x", V "y", V "q105"]))))))
    )


anotherBottle :: [Def G X]
anotherBottle = [anotherBottleDef]

anotherBottleDef :: Def G X
anotherBottleDef =
    Def "anotherBottle" ["b", "q102"] (
      ((V "b" === C "fst" []) &&&
      (V "q102" === C "snd" [])) |||
      ((V "b" === C "snd" []) &&&
      (V "q102" === C "fst" []))
    )

createEnv :: [Def G X]
createEnv = [createEnvDef]

createEnvDef :: Def G X
createEnvDef =
    Def "createEnv" ["bottle", "lvl1", "lvl2", "q99"] (
      ((V "bottle" === C "fst" []) &&&
      (V "q99" === C "pair" [V "lvl1", V "lvl2"])) |||
      ((V "bottle" === C "snd" []) &&&
      (V "q99" === C "pair" [V "lvl2", V "lvl1"]))
    )

fst' :: [Def G X]
fst' = [fst'Def]

fst'Def :: Def G X
fst'Def =
    Def "fst'" ["x", "q96"] (
      fresh ["a", "q97"] (
        (V "x" === C "pair" [V "a", V "q97"]) &&&
        (V "a" === V "q96"))
    )

snd' :: [Def G X]
snd' = [snd'Def]

snd'Def :: Def G X
snd'Def =
    Def "snd'" ["x", "q93"] (
      fresh ["q94", "a"] (
        (V "x" === C "pair" [V "q94", V "a"]) &&&
        (V "a" === V "q93"))
    )

getCapacity :: [Def G X]
getCapacity = getCapacityDef : fst' ++ snd'

getCapacityDef :: Def G X
getCapacityDef =
    Def "get_capacity" ["capacities", "bottle", "q92"] (
      ((V "bottle" === C "fst" []) &&&
      (call "fst'" [V "capacities", V "q92"])) |||
      ((V "bottle" === C "snd" []) &&&
      (call "snd'" [V "capacities", V "q92"]))
    )

fancyEq :: [Def G X]
fancyEq = [fancyEqDef]

fancyEqDef :: Def G X
fancyEqDef =
    Def "fancyEq" ["a", "b", "q85"] (
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
            (call "fancyEq" [V "x", V "y", V "q85"]))))))
    )


checkStep :: [Def G X]
checkStep = checkStepDef : fancyEq ++ getCapacity ++ anotherBottle

checkStepDef :: Def G X
checkStepDef =
    Def "checkStep" ["Env0", "step0", "capacities", "q48"] (
      fresh ["f", "s"] (
        (V "Env0" === C "pair" [V "f", V "s"]) &&&
        (fresh ["t", "b"] (
          (V "step0" === C "pair" [V "t", V "b"]) &&&
          (((V "t" === C "fill" []) &&&
          (fresh ["q51"] (
              (((V "b" === C "fst" []) &&&
              (V "f" === V "q51")) |||
              ((V "b" === C "snd" []) &&&
              (V "s" === V "q51"))) &&&
              (call "fancyEq" [V "q51", C "o" [], V "q48"])))) |||
          ((V "t" === C "empty" []) &&&
          (fresh ["q56", "q57"] (
              (((V "b" === C "fst" []) &&&
              (V "f" === V "q56")) |||
              ((V "b" === C "snd" []) &&&
              (V "s" === V "q56"))) &&&
              (call "get_capacity" [V "capacities", V "b", V "q57"]) &&&
              (call "fancyEq" [V "q56", V "q57", V "q48"])))) |||
          ((V "t" === C "pour" []) &&&
          (fresh ["q62"] (
              (fresh ["q66", "q67"] (
                (fresh ["q72"] (
                    (((V "b" === C "fst" []) &&&
                    (V "f" === V "q72")) |||
                    ((V "b" === C "snd" []) &&&
                    (V "s" === V "q72"))) &&&
                    (call "fancyEq" [V "q72", C "o" [], V "q66"]))) &&&
                (fresh ["q77", "q78"] (
                    (((V "b" === C "fst" []) &&&
                    (V "s" === V "q77")) |||
                    ((V "b" === C "snd" []) &&&
                    (V "f" === V "q77"))) &&&
                    (fresh ["q83"] (
                      (call "anotherBottle" [V "b", V "q83"]) &&&
                      (call "get_capacity" [V "capacities", V "q83", V "q78"]))) &&&
                    (call "fancyEq" [V "q77", V "q78", V "q67"]))) &&&
                (((V "q66" === C "true" []) &&&
                (V "q62" === C "true" [])) |||
                ((V "q66" === C "false" []) &&&
                (V "q62" === V "q67"))))) &&&
              (((V "q62" === C "true" []) &&&
              (V "q48" === C "false" [])) |||
              ((V "q62" === C "false" []) &&&
              (V "q48" === C "true" []))))))))))
    )

doStep :: [Def G X]
doStep = doStepDef : getCapacity ++ createEnv ++ add ++ anotherBottle ++ greater

doStepDef :: Def G X
doStepDef =
    Def "doStep" ["Env0", "step0", "capacities", "q15"] (
      fresh ["f", "s"] (
        (V "Env0" === C "pair" [V "f", V "s"]) &&&
        (fresh ["t", "b"] (
          (V "step0" === C "pair" [V "t", V "b"]) &&&
          (((V "t" === C "fill" []) &&&
          (fresh ["q18", "q19"] (
              (call "get_capacity" [V "capacities", V "b", V "q18"]) &&&
              (((V "b" === C "fst" []) &&&
              (V "s" === V "q19")) |||
              ((V "b" === C "snd" []) &&&
              (V "f" === V "q19"))) &&&
              (call "createEnv" [V "b", V "q18", V "q19", V "q15"])))) |||
          ((V "t" === C "empty" []) &&&
          (fresh ["q24"] (
              (((V "b" === C "fst" []) &&&
              (V "s" === V "q24")) |||
              ((V "b" === C "snd" []) &&&
              (V "f" === V "q24"))) &&&
              (call "createEnv" [V "b", C "o" [], V "q24", V "q15"])))) |||
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
                (call "createEnv" [V "b", V "q31", V "q32", V "q15"])))) |||
              ((V "q30" === C "false" []) &&&
              (fresh ["q41"] (
                (call "add" [V "f", V "s", V "q41"]) &&&
                (call "createEnv" [V "b", C "o" [], V "q41", V "q15"]))))))))))))
    )


isFinishEnv :: [Def G X]
isFinishEnv = isFinishEnvDef : fancyEq

isFinishEnvDef :: Def G X
isFinishEnvDef =
    Def "isFinishEnv" ["Env0", "reqLvl", "q8"] (
      fresh ["f", "s"] (
        (V "Env0" === C "pair" [V "f", V "s"]) &&&
        (fresh ["q9", "q10"] (
          (call "fancyEq" [V "f", V "reqLvl", V "q9"]) &&&
          (call "fancyEq" [V "s", V "reqLvl", V "q10"]) &&&
          (((V "q9" === C "true" []) &&&
          (V "q8" === C "true" [])) |||
          ((V "q9" === C "false" []) &&&
          (V "q8" === V "q10"))))))
    )

checkAnswer' :: [Def G X]
checkAnswer' = checkAnswer'Def : isFinishEnv ++ checkStep ++ doStep

checkAnswer'Def :: Def G X
checkAnswer'Def =
    Def "checkAnswer'" ["Env0", "answer", "capacities", "reqLvl", "q1"] (
      ((V "answer" === C "nil" []) &&&
      (call "isFinishEnv" [V "Env0", V "reqLvl", V "q1"])) |||
      (fresh ["x", "xs"] (
         (V "answer" === C "%" [V "x", V "xs"]) &&&
         (fresh ["q3"] (
            (call "checkStep" [V "Env0", V "x", V "capacities", V "q3"]) &&&
            (((V "q3" === C "true" []) &&&
            (fresh ["q4"] (
               (call "doStep" [V "Env0", V "x", V "capacities", V "q4"]) &&&
               (call "checkAnswer'" [V "q4", V "xs", V "capacities", V "reqLvl", V "q1"])))) |||
            ((V "q3" === C "false" []) &&&
            (V "q1" === C "false" [])))))))
    )

checkAnswer :: [Def G X]
checkAnswer = checkAnswerDef : checkAnswer'

checkAnswerDef :: Def G X
checkAnswerDef =
    Def "checkAnswer" ["answer", "capacities", "reqLvl", "q7"] (
      call "checkAnswer'" [C "pair" [C "o" [], C "o" []], V "answer", V "capacities", V "reqLvl", V "q7"]
    )

capacities1 :: [Def G X]
capacities1 = [capacities1Def]

capacities1Def :: Def G X
capacities1Def =
    Def "capacities1" ["q0"] (
      V "q0" === C "pair" [four, nine]
    )

bottles :: [Def G X]
bottles =
  [ addDef
  , greaterDef
  , subDef
  , anotherBottleDef
  , createEnvDef
  , fst'Def
  , snd'Def
  , getCapacityDef
  , fancyEqDef
  , checkStepDef
  , doStepDef
  , isFinishEnvDef
  , checkAnswerDef
  , checkAnswer'Def
  , capacities1Def
  ]



env :: String
env =
  "open MiniKanren\nopen MiniKanrenStd\ntype bottle =\n  | Fst \n  | Snd \nlet fst () = !! Fst\nlet snd () = !! Snd\ntype stepType =\n  | Fill \n  | Empty \n  | Pour \nlet fill () = !! Fill\nlet empty () = !! Empty\nlet pour () = !! Pour\ntype 'a0 gnat =\n  | O \n  | S of 'a0 \nlet rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\nmodule For_gnat =\n  (Fmap)(struct\n           let rec fmap fa0 = function | O -> O | S a0 -> S (fa0 a0)\n           type 'a0 t = 'a0 gnat\n         end)\nlet rec o () = inj (For_gnat.distrib O)\nand s x__0 = inj (For_gnat.distrib (S x__0))"
