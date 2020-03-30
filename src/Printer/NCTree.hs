
module Printer.NCTree where

import Printer.Dot
import qualified Eval as E
import Syntax
import NonConjunctive
import Text.Printf
import CPD.LocalControl (getCurr)

instance DotPrinter NCTree where
  labelNode t@(Conj ch _ _)  = addChildren t ch
  labelNode t@(Split ch _ _) = addChildren t ch
  labelNode t@(Or ch _ _)    = addChildren t ch
  labelNode t@(Gen ch _ _)   = addChild    t ch
  labelNode t                = addLeaf     t

instance Dot NCTree where
  dot (Leaf gs s) = printf "Leaf <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s)
  dot Fail = "_|_"
  dot (Success s) = printf "S <BR/> %s" (E.dotSigma s)
  dot (Or _ g s) = printf "O <BR/> %s <BR/> %s" (dot $ getCurr g) (E.dotSigma s)
  dot (Conj _ gs s)  = printf "C <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s)
  dot (Gen _ g gen) = printf "G <BR/> %s <BR/> %s"  (dot g) (E.dotSigma gen)
  dot (Split _ gs s) = printf "Split <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s)
  dot (Prune gs s) = printf "Prune <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s)