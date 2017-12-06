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
    let (_, t) = drive (appendo $ 
                          fresh ["q", "r", "s", "t", "p"] 
                             (call "appendo" [V "q", V "r", V "s"] &&& 
                              call "appendo" [V "s", V "t", V "p"]
                             )
                        )
    printTree "doubleapp.dot" t

    let (_, t') = drive (reverso $ fresh ["q", "r", "s"] (call "reverso" [V "q", V "r", V "s"]))
    printTree "reverso.dot" t'
    
    let ra@(_, t') = drive (revAcco $ fresh ["q", "s"] (call "revacco" [V "q", nil, V "s"]))
    printTree "revacco.dot" t'
    
    putStrLn $ show $ residualize ra

