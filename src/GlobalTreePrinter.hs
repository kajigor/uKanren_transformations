module GlobalTreePrinter where

import DotPrinter

import qualified Eval as E
import Syntax
import CPD hiding (Leaf)
import GlobalControl
import Text.Printf

instance DotPrinter GlobalTree where
  labelNode t@(Node _ ch) = addChildren t ch
  labelNode t             = addLeaf     t

instance Dot GlobalTree where
  dot (Leaf gs s) = printf "L <BR/> %s <BR/> %s" (dot $ getCurr gs) (dot s)
  dot (Node gs _)  = printf "N <BR/> %s" (dot $ getCurr gs)
