module PrintingTest where

import           Data.List
import           Debug.Trace
import           Driving
import           Eval
import           List
import           Num
import           Prelude     hiding (succ)
import           Residualize
import           Sort
import           Stream
import           Syntax
import           Test        hiding (main)
import           Tree
import           Printer.Tree
import           Stlc
import           Programs
import           Text.Printf
import           ConjRetriever
import Miscellaneous
import Printer.Dot

upTo d x =
  trace (printf "\nDepth: %s" $ show d) $
  if d == 0 then Fail
  else upToDepth x
  where
    upToDepth (Gen id gen t goal sigma) = Gen id gen (upTo (d-1) t) goal sigma
    upToDepth (Call id t goal sigma) = Call id (upTo (d-1) t) goal sigma
    upToDepth (Split id ts goal sigma) = Split id (map (\t -> upTo (d-1) t) ts) goal sigma
    upToDepth (Or t1 t2 goal sigma) = Or (upTo (d-1) t1) (upTo (d-1) t2)  goal sigma
    upToDepth (Fail) = Fail
    upToDepth (Success s) = Success s
    upToDepth (Rename id g s r ts) = Rename id g s r ts

accumCalls :: Tree -> [[G S]]
accumCalls (Or t1 t2 _ _)    = accumCalls t1 ++ accumCalls t2
accumCalls (Split _ ts _ _)  = concatMap accumCalls ts
accumCalls (Gen _ _ t _ _)   = accumCalls t
accumCalls (Call _ t goal _) = map (\xs -> goal : xs) $ accumCalls t
accumCalls (Prune [g])       = [[g]]
--accumCalls (Prune gs)          = [[conj gs]]
accumCalls _                 = []

showConjStack :: [[G S]] -> String
showConjStack gss = unlines $ map (\gs -> '\n' : '\n' : (unlines $ map (\g -> '\n' : (unwords $ map show $ unconj g)) gs) ) gss where
  unconj (g1 :/\: g2) = unconj g1 ++ unconj g2
  unconj g@(Invoke _ _) = [g]
  unconj _ = error "Here could only be conjunctions"

runTest name goal =
  do
    let tree = snd3 $ drive $ goal
    printTree (printf "%s.dot" name) tree

runTestSimplified name goal =
  do
    putStr $ printf "Running %s\n" name
    let tree = simpl $ snd3 $ drive $ goal
    writeFile (printf "%s.log" name) $ showConjStack $ retrieve tree
    printTree (printf "%s.dot" name) tree
    putStr $ printf "Finished %s\n" name

test_palindromo       = runTestSimplified "palindromo" $ palindromo $ fresh ["x"] (call "palindromo" [V "x"])
test_doubleAppendo    = runTestSimplified "doubleAppendo" $ doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
test_eveno            = runTestSimplified "eveno" $ eveno $ fresh ["x"] (call "eveno" [V "x"])
test_doubleo          = runTestSimplified "doubleo" $ doubleo $ fresh ["x", "xx"] (call "doubleo" [V "x", V "xx"])
test_empty_appendo    = runTestSimplified "emptyAppendo" $ emptyAppendo $ fresh ["x", "y"] (call "emptyAppendo" [V "x", V "y"])
test_singletonReverso = runTestSimplified "singletonReverso" $ singletonReverso $ fresh ["x", "y"] (call "singletonReverso" [V "x", V "y"])

tc = drive (appendo
              (fresh ["q", "r", "s", "t", "p"]
                 (call "appendo" [V "q", V "r", V "s"] &&&
                  call "appendo" [V "s", V "t", V "p"])
              )
           )

tc'  = drive (reverso $ fresh ["q", "r"] (call "reverso" [V "q", V "r"]))
tc'' = drive (revAcco $ fresh ["q", "s"] (call "revacco" [V "q", nil, V "s"]))


tree   = snd3 tc
tree'  = snd3 tc'
tree'' = snd3 tc''

main =
  do
--    test_doubleAppendo


    {-printTree "appendo2.dot" tree
    printTree "reverso.dot" tree'
    printTree "revacco.dot" tree''
-}
    {-let (_, t, _) = drive $ sorto $ fresh ["q", "r"] $ call "sorto" [V "q", V "r"]
    printTree "sorto.dot" (simpl t)
-}


    --runTest "evalo" $ evalo $ fresh ["q", "p"] (call "evalo" [V "q", V "p"])

    --test_palindromo
    --test_doubleo

    --test_palindromo
    test_doubleAppendo
    test_eveno
    test_doubleo
    test_empty_appendo
    test_singletonReverso


    {-let (_, t, _) = drive $ smallesto $ fresh ["q", "r", "s"] $ call "smallesto" [V "q", V "r", V "s"]
    putStrLn "\n\n\n"
    putStrLn (intercalate "\n\n" $ map (\x -> intercalate "\n" $ map show x) (accumCalls t))
    printTree "smallesto.dot" (simpl t) -- (upTo 10 t)
-}
    {-
    let (_, t, _) = drive $ sorto $ fresh ["q"] $ call "sorto" [(peanify 0 % (peanify 1 % (peanify 1 % (peanify 0 % nil)))), V "q"]
    printTree "sort.dot" t

    printTree "ehm.dot" tree
-}
    {-let (_, t', _) = drive (reverso $ fresh ["q", "r"] (call "reverso" [V "q", V "r"]))
    let t'' = simpl t'
    printTree "reverso.dot" t'
    printTree "reversoSimple.dot" t''-}
{-
    let ra@(_, t', _) = drive (revAcco $ fresh ["q", "s"] (call "revacco" [V "q", nil, V "s"]))
    let t'' = simpl t'
    printTree "revaccoSimpl.dot" t''
    printTree "revacco.dot" t'
-}
