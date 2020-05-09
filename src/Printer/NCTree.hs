
module Printer.NCTree where

import qualified CPD.LocalControl      as LC
import           Data.List             ((\\))
import           Debug.Trace           (trace)
import qualified Eval                  as E
import           NonConjunctive.Unfold (NCTree (..), restrictSubsts)
import           Printer.Dot
import           Syntax
import           Text.Printf

instance DotPrinter NCTree where
  labelNode t@(Conj ch _ _)  = addChildren t ch
  labelNode t@(Split ch _ _) = addChildren t ch
  labelNode t@(Or ch _ _)    = addChildren t ch
  labelNode t@(Gen ch _ _)   = addChild    t ch
  labelNode t                = addLeaf     t

  simplify = restrictSubsts

instance Dot NCTree where
  dot (Leaf gs s _ v) = printf "Leaf <BR/> %s <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s) (dot v)
  dot Fail = "_|_"
  dot (Success s _) = printf "Success <BR/> %s" (E.dotSigma s)
  dot (Or _ g s) = printf "Or <BR/> %s <BR/> %s" (dot $ LC.getCurr g) (E.dotSigma s)
  dot (Conj _ gs s)  = printf "And <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s)
  dot (Gen _ g gen) = printf "Gen <BR/> %s <BR/> %s"  (dot g) (E.dotSigma gen)
  dot (Split _ gs s) = printf "Split <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s)
  dot (Prune gs s) = printf "Prune <BR/> %s <BR/> %s" (dot gs) (E.dotSigma s)
