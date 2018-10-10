module OCanrenizeTest where

import OCanrenize
import DrivingTest
import Syntax
import Num
import Sort
import Residualize
import Driving
import Stlc

runTest name goal =
  toOCanren (name ++ ".ml") name $ residualize $ drive goal

test   = toOCanren "appendo2.ml" "appendo2" $ residualize tc
test'  = toOCanren "reverso.ml"  "reverso"  $ residualize tc'
test'' = toOCanren "revacco.ml"  "revacco"  $ residualize tc''

test_gto = runTest "gto" $ gto $ fresh ["q", "p", "r"] (call "gto" [V "p", V "r", V "q"])
test_leo = runTest "leo" $ leo $ fresh ["q", "p", "r"] (call "leo" [V "p", V "r", V "q"])

test_smallesto = runTest "smallesto" $ smallesto $ fresh ["q", "p", "r"] (call "smallesto" [V "q", V "p", V "r"])

test_sorto = runTest "sorto" $ sorto $ fresh ["q", "r"] (call "sorto" [V "q", V "r"])

test_minmax = runTest "minmaxo" $ minmaxo $ fresh ["q", "p", "r", "s"] (call "minmaxo" [V "q", V "p", V "r", V "s"])

test_evalo = runTest "evalo" $ evalo $ fresh ["q", "p"] (call "evalo" [V "q", V "p"])

{-test_palindromo = runTest "palindromo" $ palindromo $ fresh ["x"] (call "palindromo" [V "x"])
test_doubleAppendo = runTest "doubleAppendo" $ doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
test_eveno = runTest "eveno" $ eveno $ fresh ["x"] (call "eveno" [V "x"])
test_doubleo = runTest "doubleo" $ doubleo $ fresh ["x"] (call "doubleo" [V "x"])
test_empty_appendo = runTest "emptyAppendo" $ emptyAppendo $ fresh ["x", "y"] (call "emptyAppendo" [V "x", V "y"])
test_singletonReverso = runTest "singletonReverso" $ singletonReverso $ fresh ["x", "y"] (call "singletonReverso" [V "x", V "y"])
-}
main = do
  test
  -- test'
  -- test''
  -- test_gto
  -- test_leo
--  test_smallesto
  --test_sorto
  --test_minmax
