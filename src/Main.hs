module Main where

import MuKanren
import Programs
import Data.List (find)

i x = Ctor x []

appSpec = Spec { defs = [appendo]
               , goal =  Fresh "q" (Invoke "appendo" [
                                                      i "A" `cons` (i "B" `cons` (i "C" `cons` nil)),
                                                      i "D" `cons` (i "E" `cons` (i "F" `cons` nil)),
                                                      var "q"])
               }

env :: Spec -> String -> Def
env spec name =
  case find (\(Def n _ _) -> n == name) (defs spec) of
    Just d -> d
    Nothing -> error $ "No definition with name " ++ name ++ " in specification!"

main = do
--  print $ unify emptyState (Free 0) (Ctor "ctor" [Var "v"])
    print $ eval (env appSpec) emptyState (goal appSpec)

