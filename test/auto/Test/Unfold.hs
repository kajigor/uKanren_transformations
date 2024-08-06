module Test.Unfold where

import           Test.Helper  (test)

import           Program.Bool (andoDef, nandoDef)
import           Program.List (appendoDef, revAccoDef, reversoDef)
import qualified Program.Prop
import           Unfold       (maximumBranches)

unit_maximumBranches = do
  runTest appendoDef 2
  runTest revAccoDef 2
  runTest reversoDef 2
  runTest nandoDef 4
  runTest andoDef 1
  runTest Program.Prop.evaloDef 5

runTest =
  test maximumBranches
