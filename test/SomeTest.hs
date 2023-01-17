module SomeTest where

import           Something
import           Syntax

import qualified PrintingTest as PT

someGoal  = tree $ fresh ["q0", "q1"] (call "sum" [V "q0", C "some" [V "q1"]])
someGoal' = tree $ fresh ["q0"]       (call "sum" [V "q0", C "some" [C "s" [C "s" [C "o" []]]]])

test  = PT.runTestSimplified "sum"       someGoal
test' = PT.runTestSimplified "biggerSum" someGoal'


