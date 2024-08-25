module Test.Unfold where

import           Test.Helper  (test)

import qualified Environment  as Env 
import           Program.Bool (andoDef, nandoDef, ando)
import           Program.List (appendoDef, revAccoDef, reversoDef, doubleReverso)
import qualified Program.Prop
import           Text.Printf  (printf)
import           Unfold       (maximumBranches, static)

unit_maximumBranches = do
  runTest appendoDef 2
  runTest revAccoDef 2
  runTest reversoDef 2
  runTest nandoDef 4
  runTest andoDef 1
  runTest Program.Prop.evaloDef 5

runTest =
  test maximumBranches

unit_static = do 
  testStatic Program.Bool.ando "ando" True 
  testStatic Program.Bool.ando "nando" True 
  testStatic Program.List.doubleReverso "appendo" False 
  testStatic Program.List.doubleReverso "reverso" False 
  testStatic Program.List.doubleReverso "doubleReverso" False 

testStatic defs relName exp = do 
  let env = Env.fromDefs defs 
  test (static env) relName exp 
