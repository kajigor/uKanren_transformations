module VarInterpretation where

import Syntax
import qualified Data.Map as Map
import Data.List ( intercalate )
import Text.Printf ( printf )

newtype Interpretation = Interpretation { getInterpretation :: Map.Map X Ts }

---- Interpreting syntactic variables
infix 9 <@>
(<@>) :: Interpretation -> Tx -> Ts
i <@> (V x) = app i x
i <@> (C c ts) = C c $ map (i<@>) ts

instance Show Interpretation where
  show (Interpretation s) =
    intercalate ", " . map (\(x, y) -> printf "%s -> %s" x (show y)) $ Map.toList s

---- Extending variable interpretation
extend :: Interpretation -> X -> Ts -> Interpretation
extend (Interpretation interp) x ts = Interpretation $ Map.insert x ts interp

empty :: Interpretation
empty = Interpretation $ Map.empty

app :: Interpretation -> X -> Ts
app (Interpretation iota) x = iota Map.! x
