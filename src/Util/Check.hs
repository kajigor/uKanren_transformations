  module Util.Check where

  import Syntax
  import qualified Eval as E
  import Unfold
  import Program.List
  import Program.Programs
  import Text.Printf

  -- Checks if the first invocation is less than the second one.
  -- r x1 .. xn < r y1 .. yn <=> \exists i : xi -- strict subterm of yi
  check :: G S -> G S -> Bool
  check (Invoke n xs) (Invoke m ys) | n == m && length xs == length ys =
    any (\(x, y) -> x /= y && x `subTerm` y) (zip xs ys)
  check _ _ = False

  -- Checks if the first term occurs as a subterm within the second one.
  subTerm t s@(C _ ys) = t == s || any (subTerm t) ys
  subTerm (V x) (V y) = x == y

  -- Checks if the current goal should be unfolded more than once.
  checkWhenUnfolding :: G S -> E.Gamma -> E.Sigma -> Bool
  checkWhenUnfolding g@(Invoke n xs) gamma@(p,_,_) state =
    let (gs, gamma') = oneStep g gamma state in
    not $ any (\(goals, s) -> any (\g' -> check g (E.substitute s g')) goals) gs

  checkTest :: Program -> Bool
  checkTest (Program defs goal) =
      let gamma = E.updateDefsInGamma E.env0 defs in
      let (logicGoal, gamma', names) = E.preEval gamma goal in
      checkWhenUnfolding logicGoal gamma' E.s0

  app = Program appendo $ fresh ["x", "y", "z"] (call "appendo" [V "x", V "y", V "z"])
  dA = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
  revAcco' = Program revAcco $ fresh ["x", "a", "y"] (call "revacco" [V "x", V "a", V "y"])
  rev = Program reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])
  maxLen = Program maxLengtho $ fresh ["xs", "m", "l"] (call "maxLengtho" [V "xs", V "m", V "l"])

  test :: String -> Program -> IO ()
  test name prog = do
    putStrLn $ printf "%s should%s be unfolded further" name (if checkTest prog then "" else " not")

  run :: IO ()
  run = do
    test "appendo" app
    test "doubleAppendo" dA
    test "revAcco" revAcco'
    test "reverso" rev
    test "maxLen" maxLen