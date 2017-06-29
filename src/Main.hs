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

list :: [Term] -> Term
list = foldr cons nil

list' = list . map i

pair   [x,y]         = Ctor "Pair"   [x,y]
triple [x,y,z]       = Ctor "Triple" [x,y,z]
tuple4 [x,y,z,r]     = Ctor "Tuple4" [x,y,z,r]
tuple5 [x,y,z,r,s]   = Ctor "Tuple5" [x,y,z,r,s]
tuple6 [x,y,z,r,s,t] = Ctor "Tuple6" [x,y,z,r,s,t]

tuple xs
  | length xs == 2 = pair   (map var xs)
  | length xs == 3 = triple (map var xs)
  | length xs == 4 = tuple4 (map var xs)
  | length xs == 5 = tuple5 (map var xs)
  | length xs == 6 = tuple6 (map var xs)

a   = list' ["A"]           -- i "A" `cons` nil
ab  = list' ["A", "B"]      -- i "A" `cons` (i "B" `cons` nil)
abc = list' ["A", "B", "C"] -- i "A" `cons` (i "B" `cons` (i "C" `cons` nil))
def = list' ["D", "E", "F"] -- i "D" `cons` (i "E" `cons` (i "F" `cons` nil))

appSpec = Spec { defs = [appendo]
               , goal =  Fresh "q" (Invoke "appendo" [abc, def, var "q"])
               }

appSpec1 = Spec { defs = [appendo]
               , goal =  fresh ["q", "p", "r"]
                           (var "q" === tuple ["p","r"]
                           &&& Invoke "appendo" [var "p", def, var "r"])
               }

appSpec2 = Spec { defs = [appendo]
                , goal = fresh ["q0", "q", "p", "r"]
                           (var "q0" === tuple ["q","p","r"]
                           &&& Invoke "appendo" [var "q", var "p", var "r"])
                }

appAppSpec = Spec { defs = [appendo, doubleAppendo]
                  , goal = let args = ["x", "y", "t", "z", "r"]
                           in  fresh args $ Invoke "doubleAppendo" (map Var args)
                  }

appAppSpec1 = Spec { defs = [appendo, doubleAppendo]
                   , goal = let args = ["x", "y", "t", "z", "r"]
                            in  fresh ("q" : args)
                                  ( var "q" === tuple args
                                  &&& Invoke "doubleAppendo" (map Var args))
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

revSpec3 = Spec { defs = [appendo, reverso]
                , goal = let args = ["xs", "sx"]
                         in  fresh ("q" : args)
                               ( var "q" === tuple args
                                 &&& Invoke "reverso" (map var args))
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

revAccoSpec4 = Spec { defs = [revAcco, revAcco']
                    , goal = let args = ["xs", "sx"]
                             in  fresh ("q":args)
                                 (var "q" === tuple args
                                 &&& Invoke "revAcco'" (map var args))
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

  print $ test 2 appSpec "appendo"
  print $ test 3 appSpec1 "appendo"
  print $ test 3 appSpec2 "appendo"
  print $ test 13 appAppSpec1 "doubleAppendo"
  print $ test 5 revSpec3 "reverso"

  putStrLn "revAcco"
  print $ test 10 revAccoSpec4 "revAcco'"

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
