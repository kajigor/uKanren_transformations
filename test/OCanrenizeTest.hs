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
import qualified BridgeTest as BT
import qualified SomeTest as ST
import qualified PrintingTest as PT

runTest name goal = do
  PT.runTestSimplified name goal
  toOCanren (name ++ ".ml") name Nothing $ residualize $ drive goal

runTestWithEnv name env goal = do
  PT.runTestSimplified name goal
  toOCanren (name ++ ".ml") name (Just env) $ residualize $ drive goal

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


{-test_palindromo = runTest "palindromo" $ palindromo $ fresh ["x"] (call "palindromo" [V "x"])
test_doubleAppendo = runTest "doubleAppendo" $ doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
test_eveno = runTest "eveno" $ eveno $ fresh ["x"] (call "eveno" [V "x"])
test_doubleo = runTest "doubleo" $ doubleo $ fresh ["x"] (call "doubleo" [V "x"])
test_empty_appendo = runTest "emptyAppendo" $ emptyAppendo $ fresh ["x", "y"] (call "emptyAppendo" [V "x", V "y"])
test_singletonReverso = runTest "singletonReverso" $ singletonReverso $ fresh ["x", "y"] (call "singletonReverso" [V "x", V "y"])
-}
main = do
  --testAppendo123
  testAppendoXyz
{-  runTest "sum" ST.someGoal
  runTest "biggerSum" ST.someGoal'
  --runTest "bridge" BT.game2goal
  --runTest "bigBridge" BT.game2'goal
  test
  test'
  test''
  test_gto
  test_leo
  test_smallesto
  test_sorto
  test_minmax-}
