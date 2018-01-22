module Test where

import Syntax
import Stream
import Eval
import Driving
import Tree
import List
import Num
import Prelude hiding (succ)
import qualified Debug.Trace as T 

reify s x@(V v) =
  case lookup v s of
    Nothing -> x
    Just t  -> reify s t
reify s (C n ts) = C n $ map (reify s) ts

toplevel n printer g =
  map (\s -> printer $ reify s (V 0)) $ takeS n $ (run g)

main = 
  do
  {-
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [zero, zero, V "q"])))
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [zero, V "q", zero])))
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [V "q", zero, zero])))
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [peanify 2, zero, V "q"])))
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [zero, peanify 2, V "q"])))
    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [zero, V "q", V "q"])))
--    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [succ zero, V "q", V "q"])))
--    putStrLn $ show (toplevel 1 num (addo $ fresh ["q"] (call "addo" [V "q", succ zero, V "q"])))

    putStrLn "and now mulo"
    putStrLn $ show (toplevel 1 num (mulo $ fresh ["q"] (call "mulo" [zero, zero, V "q"])))
    putStrLn $ show (toplevel 1 num (mulo $ fresh ["q"] (call "mulo" [zero, V "q", zero])))
    putStrLn $ show (toplevel 1 num (mulo $ fresh ["q"] (call "mulo" [V "q", zero, zero])))
    putStrLn $ show (toplevel 1 num (mulo $ fresh ["q"] (call "mulo" [peanify 2, zero, V "q"])))
    putStrLn $ show (toplevel 1 num (mulo $ fresh ["q"] (call "mulo" [zero, peanify 2, V "q"])))
    putStrLn $ show (toplevel 1 num (mulo $ fresh ["q"] (call "mulo" [zero, V "q", V "q"])))
    putStrLn $ show (toplevel 1 num (mulo $ fresh ["q"] (call "mulo" [peanify 2, peanify 3, V "q"])))
    putStrLn $ show (toplevel 1 num (mulo $ fresh ["q"] (call "mulo" [peanify 2, V "q", peanify 6])))
    putStrLn $ show (toplevel 1 num (mulo $ fresh ["q"] (call "mulo" [V "q", V "q", peanify 16])))
-}
{-
--    putStrLn $ show (fresh ["q"] (call "appendo" [ V "q", i "B" % nil, i "A" % (i "B" % nil)]))
    putStrLn $ show (toplevel 1 show (appendo $ fresh ["q"] (call "appendo" [nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 show (appendo $ fresh ["q"] (call "appendo" [a % nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 show (appendo $ fresh ["q"] (call "appendo" [a % b % nil, nil, V "q"])))
    putStrLn $ show (toplevel 1 show (appendo $ fresh ["q"] (call "appendo" [ V "q", b % nil, a % b % nil])))

    putStrLn $ show (toplevel 1 show (reverso $ fresh ["q"] (call "reverso" [nil, V "q"])))
    putStrLn $ show (toplevel 1 show (reverso $ fresh ["q"] (call "reverso" [a % nil, V "q"])))
    putStrLn $ show (toplevel 1 show (reverso $ fresh ["q"] (call "reverso" [a % b % nil, V "q"])))
    putStrLn $ show (toplevel 1 show (reverso $ fresh ["q"] (call "reverso" [V "q", a % b % nil])))
-}
    putStrLn $ show $ snd' $ drive (reverso $ fresh ["q", "r", "s"] (call "reverso" [V "q", V "r", V "s"]))

