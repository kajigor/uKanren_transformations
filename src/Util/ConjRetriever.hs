module Util.ConjRetriever where

import qualified CPD.GlobalControl as GC
import Data.List.Extra
import qualified Data.Set as Set
import           Descend
import Syntax
import Tree


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
  go (GC.Node d _ _ ch) acc = concatMap (\x -> go x (map (getCurr d : ) acc)) ch
  go (GC.Prune d _) acc = map ((getCurr d) :) acc
