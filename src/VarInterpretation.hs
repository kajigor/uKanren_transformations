module VarInterpretation where

import           Data.List   (intercalate)
import qualified Data.Map    as Map
import           Syntax
import           Text.Printf (printf)

newtype Interpretation = Interpretation { getInterpretation :: Map.Map X Ts }
                       deriving (Ord, Eq)

---- Interpreting syntactic variables
infix 9 <@>
(<@>) :: Interpretation -> Term X -> Term S
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
app (Interpretation iota) x =
  case Map.lookup x iota of
    Just r -> r
    Nothing -> error (printf "Var %s undefined" x)
