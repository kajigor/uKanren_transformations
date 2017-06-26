module Main where

import MiniKanren
import Programs
import Data
import DataShow
import State
import Driver
import Residualization
import Data.List (intercalate)

i x = Ctor x []

a = i "A" `cons` nil
ab = i "A" `cons` (i "B" `cons` nil)
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

appAppSpec = Spec { defs = [appendo]
                  , goal = fresh ["x", "y", "t", "z", "r"] $
                             Conj (Invoke "appendo" [var "x", var "y", var "t"])
                                  (Invoke "appendo" [var "t", var "z", var "r"])
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

run k spec =
  let
      take k (Immature s) | k > 0 = take k s
      take k (Mature h t) | k > 0 = Mature h $ take (k-1) t
      take k _ = Empty
  in  take k $ eval (env spec) emptyState (goal spec)

main = do
--  print $ unify emptyState (Free 0) (Ctor "ctor" [Var "v"])
--  print $ reify (Free 0) $ run 3 appSpec
--  print ""
--  print $ reify (Free 0) $ run 3 appSpec1
--  print ""
--  print $ reify (Free 0) $ run 3 appSpec2
--  print ""
--  print $ reify (Free 0) $ run 5 revSpec
--  print ""
--  print $ reify (Free 0) $ run 5 revSpec1
--  print ""
--  print $ reify (Free 0) $ run 5 revSpec2
--  print ""
--  print $ reify (Free 4) $ run 5 appAppSpec
--  print $ drive revSpec2

--  putStrLn $ intercalate "\n" $ map show $ reify (Free 0) $ run 10 revAccoSpec
--  putStrLn $ intercalate "\n" $ map show $ reify (Free 0) $ run 10 revAccoSpec1
--  putStrLn $ intercalate "\n" $ map show $ reify (Free 0) $ run 1  revAccoSpec2


--  print $ drive appSpec2
--  putStrLn ""
--  print $ drive appAppSpec
--  putStrLn ""
--  print $ drive revSpec2
--  putStrLn ""
--  print $ drive revAccoSpec3
--  putStrLn ""
--  print $ drive testSpec
--  putStrLn ""
--

  putStrLn "\nAppendo:\n"
  print $ residualize' $ drive appSpec2

  putStrLn "\nDouble appendo:\n"
  print $ residualize' $ drive appAppSpec

  putStrLn "\nNaive reverso:\n"
  print $ residualize' $ drive revSpec2

  putStrLn "\nAccumulative reverso:\n"
  print $ drive revAccoSpec3

  putStrLn "\nAccumulative reverso transformed:\n"
  print $ residualize' $ drive revAccoSpec3
