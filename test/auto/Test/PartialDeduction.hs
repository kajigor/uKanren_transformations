module Test.PartialDeduction where

import           Program
import           Program.List     (nil, revAcco, reverso)
import           Program.Programs (doubleAppendo)
import qualified Program.Prop
import           Syntax
import           Transformer.PD

dA = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
revAcco' = Program revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
rev = Program reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])
prop = Program.Prop.query3

unit_partialDeductionTest = do
  -- transform "da" "out" dA
  -- transform "rev" "out"  rev
  -- transform "revAcco" "out" revAcco'
  transform "prop" "out" prop


