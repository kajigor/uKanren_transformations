module Main where

import MiniKanren
import Programs
import Data
import DataShow
import State
import Driver
import Residualization
import Data.List (intercalate)
import Debug.Trace

i x = Ctor x []

a   = i "A" `cons` nil
ab  = i "A" `cons` (i "B" `cons` nil)
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

appAppSpec = Spec { defs = [appendo, doubleAppendo]
                  , goal = let args = ["x", "y", "t", "z", "r"]
                           in  fresh args $ Invoke "doubleAppendo" (map Var args)
                  }

revSpec = Spec { defs = [appendo, reverso]
               , goal = Fresh "q" (Invoke "reverso" [a, Var "q"])
               }

revSpec1 = Spec { defs = [appendo, reverso]
                , goal = Fresh "q" (Invoke "reverso" [abc, Var "q"])
                }

revSpec2 = Spec { defs = [appendo, reverso]
                , goal = Fresh "q" $ Fresh "p" (Invoke "reverso" [Var "q", Var "p"])
                }

revAccoSpec = Spec { defs = [revAcco]
                   , goal = fresh ["q"] (Invoke "revAcco" [abc, nil, Var "q"])
                   }

revAccoSpec1 = Spec { defs = [revAcco]
                    , goal = fresh ["q"] (Invoke "revAcco" [var "q", nil, Var "q"])
                    }

revAccoSpec2 = Spec { defs = [revAcco]
                    , goal = fresh ["q"] (Invoke "revAcco" [Var "q", nil, abc])
                    }


revAccoSpec3 = Spec { defs = [revAcco]
                    , goal = fresh ["q", "p"] (Invoke "revAcco" [Var "q", nil, Var "p"])
                    }

revAcco'Spec = Spec { defs = [revAcco, revAcco']
                    , goal = fresh ["xs", "sx"] (Invoke "revAcco'" [var "xs", var "sx"])
                    }

run k spec =
  let
      take k (Immature s) | k > 0 = take k s
      take k (Mature h t) | k > 0 = Mature h $ take (k-1) t
      take k _ = Empty
  in  take k $ eval (env spec) emptyState (goal spec)

test :: (Ord a, Num a) => a -> Spec -> String -> Bool
test k spec name =
  let goal' = rename tlName $ goal spec
      rename tlName goal =
        case goal of
          Invoke name args | name == name -> Invoke tlName args
          Fresh v g -> Fresh v (rename tlName g)
          Conj l r -> Conj (rename tlName l) (rename tlName r)
          Disj l r -> Disj (rename tlName l) (rename tlName r)
          Zzz g -> Zzz (rename tlName g)
          x -> x
      x@(tlName, transformedDefs) = transform (defs spec) name
      original = run k spec
      transformed = run k Spec{goal = goal', defs = transformedDefs}
      expected = reify (Free 0) original
      actual = reify (Free 0) transformed
  in  trace ("\nExpected:\n" ++ show expected ++ "\nActual:\n" ++ show actual) $
      reify (Free 0) original == reify (Free 0) transformed


main = do
--  putStrLn "\nAppendo:\n"
--  print $ goal appSpec2
--  print $ transform (defs appSpec2) "appendo"
--
--  putStrLn "\nDouble appendo:\n"
--  print $ transform (defs appAppSpec) "doubleAppendo"
--
--  putStrLn "\nNaive reverso:\n"
--  print $ transform (defs revSpec2) "reverso"
--
--  putStrLn "\nAccumulative reverso:\n"
--  putStrLn "\nDriven:\n"
--  print $ drive revAcco'Spec
--
--  print $ transform (defs revAcco'Spec) "revAcco'"
--
--  putStrLn "\nAccumulative reverso (no nil):\n"
--  print $ transform (defs revAccoSpec3) "revAcco"
--
--  putStrLn "\nAppendo"
--  print $ run 10 appSpec
--
  print $ appendo

  let (tlName, transformed) = transform (defs appSpec) "appendo"
  print $ transformed
  putStrLn "\nTrying to eval transformed appendo\n"

--  let spec1 = Spec{goal = Fresh "q" (Invoke tlName [nil, def, var "q"]) , defs = transformed}
--  let spec2 = Spec{goal = Fresh "q" (Invoke "topLevel" [nil, def, var "q"]) , defs = [appendo', tl]}
--  print spec1
--  print spec2
--  print $ run 1 spec1
--  putStrLn "============================================================"
--  print $ run 1 spec2


  print $ test 2 appSpec "appendo"
  print $ test 1 appSpec1 "appendo"

--
--  putStrLn "\nAppendo:\n"
--  print $ residualize' $ drive appSpec2
--
--  putStrLn "\nDouble appendo:\n"
--  print $ residualize' $ drive appAppSpec
--
--  putStrLn "\nNaive reverso:\n"
--  print $ residualize' $ drive revSpec2
--
--  putStrLn "\nAccumulative reverso:\n"
--  print $ residualize' $ drive revAccoSpec3
