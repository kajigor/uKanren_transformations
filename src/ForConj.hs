module ForConj  where


import           NonConjunctive.Residualization
import qualified NonConjunctive.Unfold          as NC
import qualified Program.Bottles
import           Purification
import           Residualize                    (vident)
import           Syntax

runNc l = test (NC.nonConjunctive l)

runBottles = do
    runNc (-1) "bottles" Program.Bottles.query

unit_nonConjunctiveTest = do
  runBottles

test function filename goal =
  let transformed@(tree, logicGoal, names) = function goal in
  if NC.noPrune tree
  then
    let prog = residualize transformed in
    let pur@(goal, xs, defs) = purification (prog, vident <$> reverse names) in
    Program defs goal
  else error "Failed to residualize: prune nodes in tree"
