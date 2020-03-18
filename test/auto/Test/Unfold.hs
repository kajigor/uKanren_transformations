module Test.Unfold where

import           Test.Helper  (manyAssert, test, test2)

import           Program.Bool (nandoDef, andoDef)
import           Program.List (appendoDef, revAccoDef, reversoDef)
import qualified Program.Prop
import           Syntax
import           Unfold       (maximumBranches)

unit_maximumBranches = do
  runTest appendoDef 2
  runTest revAccoDef 2
  runTest reversoDef 2
  runTest nandoDef 4
  runTest andoDef 1
  runTest Program.Prop.evaloDef 3

runTest =
  test maximumBranches
