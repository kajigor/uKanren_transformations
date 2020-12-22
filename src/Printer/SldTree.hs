
module Printer.SldTree where

import Printer.Dot
import Syntax
import CPD.LocalControl
import Text.Printf

instance DotPrinter SldTree where
  labelNode t@(Conj ch _ _) = addChild    t ch
  labelNode t@(Or ch _ _)   = addChildren t ch
  labelNode t               = addLeaf     t

instance Dot SldTree where
  dot (Leaf gs s _) = printf "Leaf <BR/> %s <BR/> %s" (dot (map getCurr gs)) (dot s)
  dot Fail = "_|_"
  dot (Success s) = printf "S <BR/> %s" (dot s)
  dot (Or _ Nothing _) = "O"
  dot (Or _ (Just g) _) = printf "O <BR/> %s" (dot g)
  dot (Conj _ gs s)  = printf "C <BR/> %s <BR/> %s" (dot $ map getCurr gs) (dot s) -- %s <BR/> %s" (show id') (dot curr)
