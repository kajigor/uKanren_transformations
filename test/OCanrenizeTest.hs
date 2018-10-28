module OCanrenizeTest where

import OCanrenize
import DrivingTest
import Syntax
import Num
import Sort
import Residualize
import Driving
import Stlc
import Programs
import IsTest
import Bridge
import List
import TreeGen
import qualified BridgeTest as BT
import qualified SomeTest as ST
import qualified PrintingTest as PT

import Debug.Trace
import Purification

runTest name goal = do
  PT.runTestSimplified name goal
  let res = residualize $ drive goal
  toOCanren (name ++ ".ml") name Nothing res
  toOCanren (name ++ "_pur.ml") name Nothing $ purification res

runTestWithEnv name env goal = do
  PT.runTestSimplified name goal
  let res = residualize $ drive goal
  --toOCanren (name ++ ".ml") name (Just env) res
  toOCanren (name ++ "_pur.ml") name (Just env) $ purification res

test   = toOCanren "appendo2.ml" "appendo2" Nothing $ residualize tc
test'  = toOCanren "reverso.ml"  "reverso"  Nothing $ residualize tc'
test'' = toOCanren "revacco.ml"  "revacco"  Nothing $ residualize tc''

test_gto = runTest "gto" $ gto $ fresh ["q", "p", "r"] (call "gto" [V "p", V "r", V "q"])
test_leo = runTest "leo" $ leo $ fresh ["q", "p", "r"] (call "leo" [V "p", V "r", V "q"])

test_smallesto = runTest "smallesto" $ smallesto $ fresh ["q", "p", "r"] (call "smallesto" [V "q", V "p", V "r"])

test_sorto = runTest "sorto" $ sorto $ fresh ["q", "r"] (call "sorto" [V "q", V "r"])

test_minmax = runTest "minmaxo" $ minmaxo $ fresh ["q", "p", "r", "s"] (call "minmaxo" [V "q", V "p", V "r", V "s"])

test_evalo = runTest "evalo" $ evalo $ fresh ["q", "p"] (call "evalo" [V "q", V "p"])

testAppendo123 = runTest "appendo123" $ appendo123 $ fresh ["x", "y"] (call "appendo123" [V "x", V "y"])


testAppendoXyz = runTest "appendoXyz" $ appendoXyz $ fresh ["x", "y", "z", "t", "q"] (call "appendoXyz" [V "x", V "y", V "z", V "t", V "q"])

toNat i = if i == 0 then C "o" [] else C "s" [toNat $ i - 1]

testTreeGen = runTestWithEnv "treeGen" (snd treeGen) $ fst treeGen $ fresh ["x", "y"] (call "tree_generator" [toNat 4, V "x"] &&& call "eq_tree" [V "x", V "y", C "true" []])

testBadAppendo =
  let tlist rest = C "%" [C "true" [], C "%" [C "false" [], rest]] in
  runTestWithEnv "badAppendo" (snd badAppendo) $ fst badAppendo $ fresh ["x", "y", "z"] (call "badAppendo" [tlist (V "x"), tlist (V "y"), V "z"])

testUnclosedLet =
  let tlist rest = C "%" [C "true" [], C "%" [C "false" [], rest]] in
  runTestWithEnv "unclosedLet" (snd badAppendo) $ fst badAppendo $ fresh ["x", "y", "z"] (call "append" [tlist (V "x"), tlist (V "y"), V "z"])


test_bridge = runTestWithEnv "bridge" (snd pair_bridge) $ fst pair_bridge $ fresh ["a", "b"] (call "getAnswer" [V "a", C "some" [V "b"]])

test_sud4x4 = runTestWithEnv "sudoku" (snd sudoku4x4) $ fst sudoku4x4 $ fresh ["a"] (call "check_sudoku" [V "a", C "true" []])


{-test_palindromo = runTest "palindromo" $ palindromo $ fresh ["x"] (call "palindromo" [V "x"])
test_doubleAppendo = runTest "doubleAppendo" $ doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
test_eveno = runTest "eveno" $ eveno $ fresh ["x"] (call "eveno" [V "x"])
test_doubleo = runTest "doubleo" $ doubleo $ fresh ["x"] (call "doubleo" [V "x"])
test_empty_appendo = runTest "emptyAppendo" $ emptyAppendo $ fresh ["x", "y"] (call "emptyAppendo" [V "x", V "y"])
test_singletonReverso = runTest "singletonReverso" $ singletonReverso $ fresh ["x", "y"] (call "singletonReverso" [V "x", V "y"])
-}
main = do
  --testAppendo123
  --testBadAppendo
  --testUnclosedLet
  --runTest "bridge" BT.game2Goal
  test_bridge
  --test_sud4x4
  --testAppendoXyz
  --testTreeGen
{-  runTest "sum" ST.someGoal
  runTest "biggerSum" ST.someGoal'

  --runTest "bigBridge" BT.game2'goal
  test
  test'
  test''
  test_gto
  test_leo
  test_smallesto
  test_sorto
  test_minmax-}
