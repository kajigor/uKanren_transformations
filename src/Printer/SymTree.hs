
module Printer.SymTree where

import Printer.Dot
import qualified Eval as E
import Syntax
import SymbolicExecution
import Text.Printf

instance DotPrinter SymTree where
  labelNode t@(Conj ch _ _)  = addChildren t ch
  labelNode t@(Or ch _ _)    = addChildren t ch
  labelNode t                = addLeaf     t

instance Dot SymTree where
  dot Fail = "_|_"
  dot (Success s) = printf "Success <BR/> %s" (E.dotSigma s)
  dot (Or _ g _) = printf "Disj <BR/> %s" (dot g)
  dot (Conj _ gs s)  = printf "Conj <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s)
  dot (Prune g s) = printf "Prune <BR/> %s <BR/> %s" (dot g) (E.dotSigma s)