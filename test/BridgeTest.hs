module BridgeTest where

import Bridge
import Syntax
import qualified PrintingTest as PT

test = PT.runTestSimplified "bridge" $ game2 $ fresh ["a", "b"] (call "result" [V "b"] &&& call "getAnswer" [V "a", C "some" [V "b"]])
test' = PT.runTestSimplified "bridge" $ game2' $ fresh ["a", "b"] (call "getAnswer" [V "a", C "some" [V "b"]])