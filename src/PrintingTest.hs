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
import           TreePrinter

upTo d x =
  trace ("\nDepth: " ++ show d) $
  if d == 0 then Fail
  else upToDepth x
  where
    upToDepth (Gen id gen t goal sigma) = Gen id gen (upTo (d-1) t) goal sigma
    upToDepth (Call id t goal sigma) = Call id (upTo (d-1) t) goal sigma
    upToDepth (Split id t1 t2 goal sigma) = Split id (upTo (d-1) t1) (upTo (d-1) t2) goal sigma
    upToDepth (Or t1 t2 goal sigma) = Or (upTo (d-1) t1) (upTo (d-1) t2)  goal sigma
    upToDepth (Fail) = Fail
    upToDepth (Success s) = Success s
    upToDepth (Rename id g s r ts) = Rename id g s r ts

accumCalls :: Tree -> [[G S]]
accumCalls (Or t1 t2 _ _)      = accumCalls t1 ++ accumCalls t2
accumCalls (Split _ t1 t2 _ _) = accumCalls t1 ++ accumCalls t2
accumCalls (Gen _ _ t _ _)     = accumCalls t
accumCalls (Call _ t goal _)   = map (\xs -> goal : xs) $ accumCalls t
accumCalls (Prune [g])          = [[g]]
--accumCalls (Prune gs)          = [[conj gs]]
accumCalls _                   = []

main =
  do
    let (_, t, _) = drive $ sorto $ fresh ["q", "r"] $ call "sorto" [V "q", V "r"]
    printTree "sorto.dot" (simpl t) 
    
    {-let (_, t, _) = drive $ smallesto $ fresh ["q", "r", "s"] $ call "smallesto" [V "q", V "r", V "s"]
    putStrLn "\n\n\n"
    putStrLn (intercalate "\n\n" $ map (\x -> intercalate "\n" $ map show x) (accumCalls t))
    printTree "smallesto.dot" (simpl t) -- (upTo 10 t)
-}
    {-
    let (_, t, _) = drive $ sorto $ fresh ["q"] $ call "sorto" [(peanify 0 % (peanify 1 % (peanify 1 % (peanify 0 % nil)))), V "q"]
    printTree "sort.dot" t

    printTree "ehm.dot" tree

    let (_, t', _) = tc'
    let t'' = simpl t'
    printTree "reverso.dot" t'
    printTree "reversoSimple.dot" t''

    let ra@(_, t', _) = drive (revAcco $ fresh ["q", "s"] (call "revacco" [V "q", nil, V "s"]))
    let t'' = simpl t'
    printTree "revaccoSimpl.dot" t''
    printTree "revacco.dot" t'
-}
