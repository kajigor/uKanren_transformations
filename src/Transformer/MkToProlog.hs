module Transformer.MkToProlog where

import Util.ToProlog

transform filename program = do
  writeFile filename (defsToProlog program)