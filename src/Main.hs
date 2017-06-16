module Main where

import MiniKanren
import Programs
import Data
import DataShow
import State
import Driver

i x = Ctor x []

a = i "A" `cons` nil
abc = i "A" `cons` (i "B" `cons` (i "C" `cons` nil))
def = i "D" `cons` (i "E" `cons` (i "F" `cons` nil))

appSpec = Spec { defs = [appendo]
               , goal =  Fresh "q" (Invoke "appendo" [abc, def, var "q"])
               }

appSpec1 = Spec { defs = [appendo]
               , goal =  Fresh "q"
                           (Fresh "p"
                             (Invoke "appendo" [var "p", def, var "q"]))
               }

appSpec2 = Spec { defs = [appendo]
                , goal = Fresh "q" $
                           Fresh "p" $
                             Fresh "r" $
                               Invoke "appendo" [var "q", var "p", var "r"]
                }

revSpec = Spec { defs = [appendo, reverso]
               , goal = Fresh "q" (Invoke "reverso" [a, Var "q"])
               }

revSpec1 = Spec { defs = [appendo, reverso]
                , goal = Fresh "q" (Invoke "reverso" [abc, Var "q"])
                }

revSpec2 = Spec { defs = [appendo, reverso]
                , goal = Fresh "q" $ Fresh "p" (Invoke "reverso" [Var "p", Var "q"])
                }

run k spec =
  let
      take k (Immature s) | k > 0 = take k s
      take k (Mature h t) | k > 0 = Mature h $ take (k-1) t
      take k _ = Empty
  in  take k $ eval (env spec) emptyState (goal spec)

main = do
--  print $ unify emptyState (Free 0) (Ctor "ctor" [Var "v"])
  print $ reify (Free 0) $ run 3 appSpec
  print ""
  print $ reify (Free 0) $ run 3 appSpec1
  print ""
  print $ reify (Free 0) $ run 3 appSpec2
  print ""
  print $ reify (Free 0) $ run 5 revSpec
  print ""
  print $ reify (Free 0) $ run 5 revSpec1
  print ""
  print $ reify (Free 0) $ run 5 revSpec2
  print ""

