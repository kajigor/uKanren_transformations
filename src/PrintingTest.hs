module PrintingTest where

import Syntax
import Stream
import Eval
import Driving
import Tree
import Test hiding (main)
import TreePrinter
import List
import Num
import Sort
import Prelude hiding (succ)
import Residualize
import qualified Debug.Trace as T 


upToDepth x d | d <= 0 = Fail
upToDepth (Gen id gen t goal sigma) d = Gen id gen (upToDepth t $ d-1) goal sigma
upToDepth (Call id t goal sigma) d = Call id (upToDepth t $ d-1) goal sigma
upToDepth (Split id t1 t2 goal sigma) d = Split id (upToDepth t1 $ d-1) (upToDepth t2 $ d-1) goal sigma
upToDepth (Or t1 t2 goal sigma) d = Or (upToDepth t1 $ d-1) (upToDepth t2 $ d-1) goal sigma
upToDepth x _ = x

main = 
  do
    let (_, t, _) = drive $ smallesto $ fresh ["q", "r", "s"] $ call "smallesto" [V "q", V "r", V "s"] 
    printTree "smallesto.dot" t
    
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
