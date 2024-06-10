module Transformer.JustUnfold where

import qualified ConsPD.Unfold
import qualified Transformer.ConsPD

transform l = Transformer.ConsPD.transform "test/out/consPD" Nothing (ConsPD.Unfold.justUnfold l) Nothing
