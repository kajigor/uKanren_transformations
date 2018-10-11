module BridgeTest where

import Bridge
import Syntax
import qualified PrintingTest as PT

game2goal = game2 $ fresh ["a", "b"] (call "result" [V "b"] &&& call "getAnswer" [V "a", C "some" [V "b"]])
game2'goal = game2' $ fresh ["a", "b"] (call "getAnswer" [V "a", C "some" [V "b"]])

test = PT.runTestSimplified "bigbridge" $ game2goal
test' = PT.runTestSimplified "bridge" $ game2'goal