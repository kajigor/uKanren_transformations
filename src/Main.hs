module Main where

import MiniKanren
import Programs
import Data
import DataShow
import State
import Data.List (find)

i x = Ctor x []

appSpec = Spec { defs = [appendo]
               , goal =  Fresh "q" (Invoke "appendo" [
                                                      i "A" `cons` (i "B" `cons` (i "C" `cons` nil)),
                                                      i "D" `cons` (i "E" `cons` (i "F" `cons` nil)),
                                                      var "q"])
               }

appSpec1 = Spec { defs = [appendo]
               , goal =  Fresh "q"
                           (Fresh "p"
                             (Invoke "appendo" [
                                                var "p",
                                                i "D" `cons` (i "E" `cons` (i "F" `cons` nil)),
                                                var "q"]))
               }

appSpec2 = Spec { defs = [appendo]
                , goal = Fresh "q" $
                           Fresh "p" $
                             Fresh "r" $
                               Invoke "appendo" [var "q", var "p", var "r"]
                }

env :: Spec -> String -> Def
env spec name =
  case find (\(Def n _ _) -> n == name) (defs spec) of
    Just d -> d
    Nothing -> error $ "No definition with name " ++ name ++ " in specification!"

run k spec =
  let
      take k (Immature s) | k > 0 = take k s
      take k (Mature h t) | k > 0 = Mature h $ take (k-1) t
      take k _ = Empty
  in  take k $ eval (env spec) emptyState (goal spec)

main = do
--  print $ unify emptyState (Free 0) (Ctor "ctor" [Var "v"])
--    print $ run 3 appSpec1
--  print $ run 5 appSpec2
  print $ reify (Free 0) (run 3 appSpec)

