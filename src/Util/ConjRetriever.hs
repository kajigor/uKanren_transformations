module Util.ConjRetriever where

import Syntax
import Tree
import qualified CPD.GlobalControl as GC
import qualified Data.Set as Set
import qualified CPD.LocalControl as LC 
import Data.List.Extra

retrieve :: Tree -> [[G S]]
retrieve tree = map reverse $ Set.toList $ Set.fromList $ go tree [[]] where
  go Fail acc = acc
  go (Success _) acc = acc
  go (Rename _ _ _ _ _) acc = acc
  go (Gen _ _ t _ _) acc = go t acc
  go (Call _ t g _) acc = go t (map (g :) acc)
  go (Or t1 t2 _ _) acc = go t1 acc ++ go t2 acc
  go (Split _ ts _ _) acc = concatMap (\t -> go t acc) ts
  go (Prune gs) acc = acc

globalTreeRetrieve :: GC.GlobalTree -> [[[G S]]]
globalTreeRetrieve tree = map reverse $ filter notNull $ go tree [[]] where
  go (GC.Leaf _ _ _) acc = [[]]
  go (GC.Node d _ _ ch) acc = concatMap (\x -> go x (map (LC.getCurr d : ) acc)) ch
  go (GC.Prune d _) acc = map ((LC.getCurr d) :) acc
