
module Printer.ConsPDTree where

import           ConsPD.Unfold    (ConsPDTree (..), restrictSubsts)
import qualified CPD.LocalControl as LC
import           Data.List        ((\\))
import           Debug.Trace      (trace)
import qualified Eval             as E
import           Printer.Dot
import qualified Subst
import           Syntax
import           Text.Printf

instance DotPrinter ConsPDTree where
  labelNode t@(Conj ch _ _)    = addChildren t ch
  labelNode t@(Split ch _ _)   = addChildren t ch
  labelNode t@(Or ch _ _)      = addChildren t ch
  labelNode t@(Gen ch _ _ _ _) = addChild    t ch
  labelNode t                  = addLeaf     t

  simplify = restrictSubsts

instance Dot ConsPDTree where
  dot (Leaf gs s _ v) = printf "Leaf <BR/> %s <BR/> %s <BR/> %s" (dot gs) (Subst.dotSubst s) (dot v)
  dot Fail = "_|_"
  dot (Success s _) = printf "Success <BR/> %s" (Subst.dotSubst s)
  dot (Or _ g s) = printf "Or <BR/> %s <BR/> %s" (dot $ LC.getCurr g) (Subst.dotSubst s)
  dot (Conj _ gs s)  = printf "And <BR/> %s <BR/> %s" (dot gs) (Subst.dotSubst s)
  dot (Gen _ g g' gen s) = printf "Gen <BR/> %s <BR/> %s <BR/> %s <BR/> %s"  (dot g) (dot g') (Subst.dotSubst gen) (Subst.dotSubst s)
  dot (Split _ gs s) = printf "Split <BR/> %s <BR/> %s" (dot gs) (Subst.dotSubst s)
  dot (Prune gs s) = printf "Prune <BR/> %s <BR/> %s" (dot gs) (Subst.dotSubst s)
