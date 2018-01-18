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
import Prelude hiding (succ)
import Residualize
import qualified Debug.Trace as T 

main = 
  do
    printTree "ehm.dot" tree 

    let (_, t') = tc'
    let t'' = simpl t'
    printTree "reverso.dot" t'
    printTree "reversoSimple.dot" t''
    
    let ra@(_, t') = drive (revAcco $ fresh ["q", "s"] (call "revacco" [V "q", nil, V "s"]))
    let t'' = simpl t'
    printTree "revaccoSimpl.dot" t''
    printTree "revacco.dot" t'

