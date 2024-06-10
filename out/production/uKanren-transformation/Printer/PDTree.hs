
module Printer.PDTree where

import           Descend
import           PartialDeduction
import           Printer.Dot
import           Syntax
import           Text.Printf

instance DotPrinter PDTree where
  labelNode t@(Conj ch _ _)  = addChildren t ch
  labelNode t@(Split ch _ _) = addChildren t ch
  labelNode t@(Or ch _ _)    = addChildren t ch
  labelNode t@(Gen ch _)     = addChild    t ch
  labelNode t                = addLeaf     t

instance Dot PDTree where
  dot (Leaf gs s) = printf "Leaf <BR/> %s <BR/> %s" (dot gs) (dot s)
  dot Fail = "_|_"
  dot (Success s) = printf "S <BR/> %s" (dot s)
  dot (Or _ g _) = printf "O <BR/> %s" (dot $ getCurr g)
  dot (Conj _ gs s)  = printf "C <BR/> %s <BR/> %s" (dot gs) (dot s)
  dot (Gen _ g) = printf "G <BR/> %s" (dot g)
  dot (Split _ gs s) = printf "Split <BR/> %s <BR/> %s" (dot gs) (dot s)
