
module Printer.PDTree where

import           Descend
import           PD.PartialDeduction
import           Printer.Dot
import           Syntax
import           Text.Printf

instance DotPrinter PDTree where
  labelNode t@(Conj ch _ _)  = addChildren t ch
  labelNode t@(Or ch _ _)    = addChildren t ch
  labelNode t@(Gen ch _ _)   = addChild    t ch
  labelNode t                = addLeaf     t

instance Dot PDTree where
  dot (Leaf gs s g') = printf "Leaf <BR/> %s <BR/> %s <BR/> renames: %s" (dot gs) (dot s) (dot g')
  dot Fail = "_|_"
  dot (Success s) = printf "S <BR/> %s" (dot s)
  dot (Or _ g _) = printf "O <BR/> %s" (dot $ getCurr g)
  dot (Conj _ gs s)  = printf "C <BR/> %s <BR/> %s" (dot gs) (dot s)
  dot (Gen _ g g') = printf "G <BR/> old: %s <BR/> new: %s" (dot g) (dot g')
