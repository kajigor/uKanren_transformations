
module Printer.SymTree where

import Printer.Dot
import Syntax
import SymbolicExecution
import Text.Printf

instance DotPrinter SymTree where
  labelNode t@(Conj ch _ _) = addChildren t ch
  labelNode t@(Disj ch _ _) = addChildren t ch
  labelNode t               = addLeaf     t

instance Dot SymTree where
  dot Fail = "_|_"
  dot (Success s) = printf "Success <BR/> %s" (dot s)
  dot (Disj _ gs s) = printf "Disj <BR/> %s <BR/> %s" (dot gs) (dot s)
  dot (Conj _ gs s) = printf "Conj <BR/> %s <BR/> %s" (dot gs) (dot s)
  dot (Prune g s) = printf "Prune <BR/> %s <BR/> %s" (dot g) (dot s)