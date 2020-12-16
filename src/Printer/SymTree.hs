
module Printer.SymTree where

import Printer.Dot
import qualified Eval as E
import Syntax
import SymbolicExecution
import Text.Printf
import qualified Subst

instance DotPrinter SymTree where
  labelNode t@(Conj ch _ _) = addChildren t ch
  labelNode t@(Disj ch _ _) = addChildren t ch
  labelNode t               = addLeaf     t

instance Dot SymTree where
  dot Fail = "_|_"
  dot (Success s) = printf "Success <BR/> %s" (Subst.dotSubst s)
  dot (Disj _ gs s) = printf "Disj <BR/> %s <BR/> %s" (dot gs) (Subst.dotSubst s)
  dot (Conj _ gs s) = printf "Conj <BR/> %s <BR/> %s" (dot gs) (Subst.dotSubst s)
  dot (Prune g s) = printf "Prune <BR/> %s <BR/> %s" (dot g) (Subst.dotSubst s)