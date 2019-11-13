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
import Something
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
test_palindromo = runTest "palindromo" $ palindromo $ fresh ["x"] (call "palindromo" [V "x"])
test_doubleAppendo = runTest "doubleAppendo" $ doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
test_eveno = runTest "eveno" $ eveno $ fresh ["x"] (call "eveno" [V "x"])
test_doubleo = runTest "doubleo" $ doubleo $ fresh ["x", "y"] (call "doubleo" [V "x", V "y"])
test_empty_appendo = runTest "emptyAppendo" $ emptyAppendo $ fresh ["x", "y"] (call "emptyAppendo" [V "x", V "y"])
test_singletonReverso = runTest "singletonReverso" $ singletonReverso $ fresh ["x", "y"] (call "singletonReverso" [V "x", V "y"])
-- test_fgh = runTest "fgh" $ fgh $ fresh ["x"] (call "f" [V "x"] &&& call "g" [V "x"] &&& call "h" [V "x"])
test_additionIsOne = runTest "addIsOne" $ add $ fresh ["x", "y"] (call "add" [C "s" [C "s" [C "s" [C "s" [V "x"]]]], V "y", C "s" [C "o" []]])

test_dumbConstructors = runTest "dumbConstructors" $ dumbConstructors $ (call "dumbConstructors" [C "o" []])


dumbConstructors g =
  Let (def "dumbConstructors" ["x"] (
        call "dumbConstructors" [C "s" [V "x"]]
    )) g

add g =
  Let (def "add" ["a0", "b0", "q123"] (
    ((V "a0" === C "o" []) &&&
    (V "b0" === V "q123")) |||
    (fresh ["x"] (
       (V "a0" === C "s" [V "x"]) &&&
       (call "add" [V "x", C "s" [V "b0"], V "q123"])))
  )) g


main = do
  test_dumbConstructors
  --test_additionIsOne
  --test_sorto
  {-test
  test'
  test''
  test_gto
  test_leo
  test_smallesto
  test_sorto
  test_minmax
  test_evalo
  testAppendo123
  testAppendoXyz
  --test_palindromo
  test_doubleAppendo
  test_eveno
  test_doubleo
  test_empty_appendo
  test_singletonReverso
  test_fgh-}
{-  runTest "sum" ST.someGoal
  runTest "biggerSum" ST.someGoal'
  --runTest "bridge" BT.game2goal
  --runTest "bigBridge" BT.game2'goal -}
