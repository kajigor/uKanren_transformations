module BridgeTest where

import           Bridge
import qualified PrintingTest as PT
import           Syntax

game2BigGoal = game2Big $ fresh ["a", "b"] (call "result" [V "b"] &&& call "getAnswer" [V "a", C "some" [V "b"]])
game2Goal = game2 $ fresh ["a", "b"] (call "getAnswer" [V "a", C "some" [V "b"]])

eqForBoolTest = game2 $ call "eqForBool" [C "true" [], C "true" [], C "true" []]

testBig = PT.runTestSimplified "bigbridge" $ game2BigGoal
test    = PT.runTestSimplified "bridge" $ game2Goal
testEq  = PT.runTestSimplified "eqForBool" $ eqForBoolTest
