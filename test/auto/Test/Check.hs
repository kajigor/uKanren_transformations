module Test.Check where

import qualified Eval   as E
import           Program.List
import           Program.Programs
import qualified Subst
import           Syntax
import           Test.Helper      (test2)
import           Text.Printf
import           Util.Check

unit_checkConj = do
  test2 checkConj [call "f" [V 1, V 2], call "g" [V 2, V 3]] [call "f" [V 5, V 6], call "g" [C "" [V 2], V 3]] True
  test2 checkConj [call "f" [C "" [V 1], V 2], call "g" [V 2, V 3]] [call "f" [V 1, V 2], call "g" [C "" [V 2], V 3]] True
  test2 checkConj [call "f" [V 1, V 2], call "g" [V 2, V 3]] [call "f" [V 1, V 2], call "g" [V 2, V 3]] False
  test2 checkConj [call "f" [V 1, V 2], call "g" [C "" [V 2], V 3]] [call "f" [V 1, V 2], call "g" [V 2, V 3]] False
  test2 checkConj [call "f" [V 1, V 2]] [call "f" [V 1, V 2], call "g" [C "" [V 2], V 3]] False
  test2 checkConj [call "g" [V 2, V 3], call "f" [V 1, V 2]] [call "f" [V 1, V 2], call "g" [C "" [V 2], V 3]] False

  test2 checkConj [call "|=|" [V 121, V 122, C "true" []], call "|=|" [V 12, C "Succ" [C "Succ" [V 122]], V 14]]
                  [call "|=|" [V 127, V 128, C "true"[]], call "|=|" [V 12, C "Succ" [C "Succ" [C "Succ" [C "Succ" [V 128]]]], V 14]]
                  False

checkTest :: Program -> Bool
checkTest (Program defs goal) =
    let gamma = E.gammaFromDefs defs in
    let (logicGoal, gamma', names) = E.preEval gamma goal in
    checkWhenUnfolding logicGoal gamma' Subst.empty

app = Program appendo $ fresh ["x", "y", "z"] (call "appendo" [V "x", V "y", V "z"])
dA = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
revAcco' = Program revAcco $ fresh ["x", "a", "y"] (call "revacco" [V "x", V "a", V "y"])
rev = Program reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])
maxLen = Program maxLengtho $ fresh ["xs", "m", "l"] (call "maxLengtho" [V "xs", V "m", V "l"])

runTest :: String -> Program -> IO ()
runTest name prog = do
  putStrLn $ printf "%s should%s be unfolded further" name (if checkTest prog then "" else " not")

run :: IO ()
run = do
  runTest "appendo" app
  runTest "doubleAppendo" dA
  runTest "revAcco" revAcco'
  runTest "reverso" rev
  runTest "maxLen" maxLen
