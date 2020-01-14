
module Printer.PDTree where

import Printer.Dot
import qualified Eval as E
import Syntax
import PartialDeduction
import Text.Printf
import CPD (getCurr)

instance DotPrinter PDTree where
  labelNode t@(Conj ch _ _) = addChildren t ch
  labelNode t@(Or ch _ _)   = addChildren t ch
  labelNode t@(Gen ch _)    = addChild    t ch 
  labelNode t               = addLeaf     t

instance Dot PDTree where
  dot (Leaf gs s) = printf "Leaf <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s)
  dot Fail = "_|_"
  dot (Success s) = printf "S <BR/> %s" (E.dotSigma s)
  dot (Or _ g _) = printf "O <BR/> %s" (dot $ getCurr g)
  dot (Conj _ gs s)  = printf "C <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s)
  dot (Gen _ g) = printf "G <BR/> %s" (dot g)