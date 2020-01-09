module OldTest where

import Syntax
import Stream
import Eval
import Driving
import Tree
import List
import Num
import Bool
import Sort
import Prelude hiding (succ)

reify :: Eq a => [(a, Term a)] -> Term a -> Term a
reify s x@(V v) =
  case lookup v s of
    Nothing -> x
    Just t  -> reify s t
reify s (C n ts) = C n $ map (reify s) ts

toplevel :: Integer -> (Term S -> String) -> G X -> [String]
toplevel n printer g =
  map (\s -> printer $ reify s (V 0)) $ takeS n $ run g

main :: IO ()
main =
  do
    print (toplevel 1 bool (nando $ fresh ["q"] (call "nando" [falso, falso, V "q"])))
    print (toplevel 1 bool (nando $ fresh ["q"] (call "nando" [V "q", V "q", V "q"])))
    print (toplevel 1 show (nando $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "nando" [V "r", V "r", V "p"])))

    print (toplevel 1 bool (noto $ fresh ["q"] (call "noto" [falso, V "q"])))
    print (toplevel 1 bool (noto $ fresh ["q"] (call "noto" [V "q", V "q"])))
    print (toplevel 2 show (noto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "noto" [V "p", V "r"])))

    print (toplevel 3 show (oro $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "oro" [V "p", V "r", C "true" []])))
    print (toplevel 4 show (ando $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "ando" [V "p", V "r", C "false" []])))


    print (toplevel 4 show (leo $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "leo" [V "p", V "p", V "r"] )))
    print (toplevel 4 show (leo $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "leo" [V "p", V "r", C "false" []])))

    print (toplevel 4 show (geo $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "geo" [V "p", V "r", C "false" []])))

    print (toplevel 4 show (gto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "gto" [zero, V "p", V "r"] )))

    print (toplevel 4 show (gto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "gto" [V "p", V "p", V "r"] )))
    print (toplevel 4 show (gto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "gto" [V "p", V "r", C "false" []])))

    print (toplevel 4 show (gto $ fresh ["q"] (call "gto" [succ zero, zero, V "q"])))


    print (toplevel 4 show (lto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "lto" [V "p", V "r", C "false" []])))

    print (toplevel 4 list (singletono $ fresh ["q"] (call "singletono" [V "q", peanify 1])))
    print (toplevel 4 show (singletono $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "singletono" [V "p", V "r"])))

    print (toplevel 4 show (smallesto $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "smallesto" [peanify 2 % (peanify 1 % nil), V "p", V "r"])))

    print (toplevel 4 show (minmaxo $ fresh ["q", "p", "r"] (V "q" === C "pair" [V "p", V "r"] &&& call "minmaxo" [peanify 1, peanify 0, V "p", V "r"])))

--    print (toplevel 1 show (sorto $ fresh ["q"] $ call "sorto" [peanify 3 % (peanify 1 % (peanify 0 % nil)), V "q"]))

    print (toplevel 1 show (sorto $ fresh ["q"] $ call "sorto" [peanify 0 % (peanify 1 % (peanify 1 % (peanify 0 % nil))), V "q"]))


--    putStrLn $ show (toplevel 1 show (sorto $ fresh ["q", "q1", "q2", "q3", "r"] (V "q" === C "pair" [C "triple" [V "q1", V "q2", V "q3"], V "r"] &&& call "sorto" [(V "q1" % (V "q2" % (V "q3" % nil))), V "r"])))

{-


    putStrLn $ show (toplevel 1 num (notZero $ (call "notZero" [zero])))
    putStrLn $ show (toplevel 1 num (notZero $ (call "notZero" [succ zero])))
    putStrLn $ show (toplevel 1 num (notZero $ (call "notZero" [succ $ succ zero])))

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
   {- putStrLn $ show $ snd' $ drive (reverso $ fresh ["q", "r", "s"] (call "reverso" [V "q", V "r", V "s"]))
-}




