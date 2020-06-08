module Transformer.MkToOcanren where

import qualified OCanrenize as OC
import           Syntax

transform fileName (Program defs goal) names =
  OC.topLevel fileName "topLevel" Nothing (goal, names, defs)
