module Printer.GlobalTreeCons where

import           Printer.Dot

import           ConsPD.GlobalControl
import           Descend
import           Syntax
import           Text.Printf

instance DotPrinter GlobalTree where
  labelNode t@(Node _ _ _ ch) i vs es ids = addChildren t ch i vs es ids 
  labelNode t                 i vs es ids = addLeaf     t    i vs es ids

isLeaf (Leaf _ _ _) = True
isLeaf _ = False

instance Dot GlobalTree where
  dot (Leaf gs _ _)  = printf "L <BR/> %s" (dot $ getCurr gs)
  dot (Node gs gen _ _) = printf "N <BR/> %s <BR/> %s" (dot $ getCurr gs) (dot gen)
  dot (Prune gs _)  = printf "P <BR/> %s" (dot $ getCurr gs)
