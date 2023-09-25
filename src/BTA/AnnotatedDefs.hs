module BTA.AnnotatedDefs where

import           BTA.AnnotatedDef
import           BTA.InvokeAnnotation (AnnG)
import           Syntax
import           Text.Printf (printf)
import qualified Data.Map    as Map

newtype AnnDefinitions = AnnDefinitions { getDefinitions :: Map.Map Name (AnnotatedDef (AnnG Term) X) }
                    deriving (Show, Ord, Eq)

empty :: AnnDefinitions
empty = AnnDefinitions Map.empty

insert :: Name -> AnnotatedDef (AnnG Term) X -> AnnDefinitions -> AnnDefinitions
insert k v (AnnDefinitions d) = AnnDefinitions $ Map.insert k v d

getDef :: AnnDefinitions -> Name -> AnnotatedDef (AnnG Term) X
getDef (AnnDefinitions p) n =
  case Map.lookup n p of
    Just d -> d
    Nothing -> error (printf "Relation %s undefined" n)

