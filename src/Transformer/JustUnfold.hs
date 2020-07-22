module Transformer.JustUnfold where

import qualified ConsPD.Unfold
import qualified Transformer.ConsPD

transform l = Transformer.ConsPD.transform "test/out/consPD" True Nothing (ConsPD.Unfold.justUnfold l)
