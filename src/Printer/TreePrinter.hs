module Printer.TreePrinter where

import Printer.Dot
import Tree

instance DotPrinter Tree where
  labelNode t@(Call _ ch _ _)  = addChild    t ch
  labelNode t@(Gen _ _ ch _ _) = addChild    t ch
  labelNode t@(Or ch1 ch2 _ _) = addChildren t [ch1, ch2]
  labelNode t@(Split _ ch _ _) = addChildren t ch
  labelNode t                  = addLeaf     t
