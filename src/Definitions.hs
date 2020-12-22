module Definitions where

import qualified Data.Map as Map
import Syntax

newtype Definitions = Definitions { getDefinitions :: Map.Map Name Def }

empty :: Definitions
empty = Definitions $ Map.empty

insert :: Name -> Def -> Definitions -> Definitions
insert k v (Definitions d) = Definitions $ Map.insert k v d

getDef :: Definitions -> Name -> Def
getDef (Definitions p) n = p Map.! n
