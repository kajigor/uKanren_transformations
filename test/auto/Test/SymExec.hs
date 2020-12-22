module Test.SymExec where


import           Program.List                  (nil, revAcco, reverso)
import           Program.Programs              (doubleAppendo)
import           Syntax
import           Transformer.SymbolicExecution

dA = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
revAcco' = Program revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
rev = Program reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])

unit_symExec = do
  transform 5 "da" dA
  transform 5 "rev" rev
  transform 5 "revAcco" revAcco'
