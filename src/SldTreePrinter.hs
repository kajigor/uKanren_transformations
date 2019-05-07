
module SldTreePrinter where

import DotPrinter
import qualified Eval as E
import Syntax
import CPD
import Text.Printf

instance DotPrinter SldTree where
  labelNode t@(Conj ch _ _) = addChild    t ch
  labelNode t@(Or ch _)     = addChildren t ch
  labelNode t               = addLeaf     t

instance Dot SldTree where
  dot (Leaf gs s _) = printf "Leaf <BR/> %s <BR/> %s" (dot (map getCurr gs)) (E.dotSigma s)
  dot Fail = "_|_"
  dot (Success s) = printf "S <BR/> %s" (E.dotSigma s)
  dot (Or _ _ ) = printf "O" -- <BR/> " ++ dot curr
  dot (Conj _ gs s)  = printf "C <BR/> %s <BR/> %s" (dot $ map getCurr gs) (E.dotSigma s) -- %s <BR/> %s" (show id') (dot curr)
