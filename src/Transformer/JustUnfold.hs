module Transformer.JustUnfold where

import qualified Transformer.NonConj
import qualified NonConjunctive.Unfold

transform l = Transformer.NonConj.transform "test/out/nc" True Nothing (NonConjunctive.Unfold.justUnfold l)