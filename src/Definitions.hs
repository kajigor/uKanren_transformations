module Definitions where

import qualified Data.Map    as Map
import           Def
import           Syntax
import           Text.Printf (printf)

newtype Definitions = Definitions { getDefinitions :: Map.Map Name (Def G X) }
                    deriving (Show, Ord, Eq)

empty :: Definitions
empty = Definitions Map.empty

insert :: Name -> Def G X -> Definitions -> Definitions
insert k v (Definitions d) = Definitions $ Map.insert k v d

getDef :: Definitions -> Name -> Def G X
getDef (Definitions p) n =
  case Map.lookup n p of
    Just d -> d
    Nothing -> error (printf "Relation %s undefined" n)
