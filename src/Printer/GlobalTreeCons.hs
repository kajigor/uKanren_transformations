module Printer.GlobalTreeCons where

import           Printer.Dot

import           ConsPD.GlobalControl
import           Descend
import           Syntax
import           Text.Printf

instance DotPrinter GlobalTree where
  labelNode t@(Node _ _ _ _ ch) i vs es ids = addChildren t ch i vs es ids 
  labelNode t@(Split _ ch _ )      i vs es ids = addChildren t ch i vs es ids
  labelNode t@(Transient _ ch _ _) i vs es ids = addChildren t [ch] i vs es ids
  labelNode t                   i vs es ids = addLeaf     t    i vs es ids

isLeaf (Leaf _ _ _) = True
isLeaf (Success _) = True 
isLeaf _ = False

instance Dot GlobalTree where
  -- dot (Success s) = printf "S"
  -- dot (Leaf gs gen s)  = printf "L <BR/> %s <BR/> G: %s" (dot $ getCurr gs) (dot gen) 
  -- dot (Node gs gen s _ _) = printf "N <BR/> %s <BR/> G: %s" (dot $ getCurr gs) (dot gen) 
  -- dot (Split gs _ _ ) = printf "Split <BR/> %s" (dot gs)
  -- dot (Prune gs _)  = printf "P <BR/> %s" (dot $ getCurr gs)
  -- dot (Transient gs _ _ _) = printf "T <BR/> %s" (dot $ getCurr gs)

  dot Fail = "_|_"
  dot (Success s) = printf "S <BR/> %s" (dot s)
  dot (Leaf gs gen s)  = printf "L <BR/> %s <BR/> G: %s <BR/> S: %s" (dot $ getCurr gs) (dot gen) (dot s)
  dot (Node gs gen s _ _) = printf "N <BR/> %s <BR/> G: %s <BR/> S: %s" (dot $ getCurr gs) (dot gen) (dot s)
  dot (Split gs _ s) = printf "Split <BR/> %s <BR/> %s" (dot gs) (dot s)
  dot (Prune gs _)  = printf "P <BR/> %s" (dot $ getCurr gs)
  dot (Transient gs _ s _) = printf "T <BR/> %s <BR/> %s" (dot $ getCurr gs) (dot s)
