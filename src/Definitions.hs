module Definitions where

import qualified Data.Map as Map
import Syntax
import Def

newtype Definitions = Definitions { getDefinitions :: Map.Map Name (Def G X) }
                    deriving (Show, Ord, Eq)

empty :: Definitions
empty = Definitions Map.empty

insert :: Name -> Def G X -> Definitions -> Definitions
insert k v (Definitions d) = Definitions $ Map.insert k v d

getDef :: Definitions -> Name -> Def G X
getDef (Definitions p) n = p Map.! n
