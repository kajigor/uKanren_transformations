module ConjRetriever where

import Syntax
import Tree
import qualified GlobalControl as G
import qualified Data.Set as Set
import qualified CPD
import Data.List.Extra

{-data Tree =
  Prune   [G S]                             |
  Fail                                      |
  Success E.Sigma                           |
  Or      Tree Tree (G S) E.Sigma           |
  Rename  Id (G S) E.Sigma Renaming E.Sigma |
  Gen     Id Generalizer Tree (G S) E.Sigma |
  Call    Id Tree (G S) E.Sigma             |
  Split   Id [Tree] (G S) E.Sigma           deriving Show-}

retrieve :: Tree -> [[G S]]
retrieve tree = map reverse $ Set.toList $ Set.fromList $ retrieve' tree [[]] where
  retrieve' Fail acc = acc
  retrieve' (Success _) acc = acc
  retrieve' (Rename _ _ _ _ _) acc = acc
  retrieve' (Gen _ _ t _ _) acc = retrieve' t acc
  retrieve' (Call _ t g _) acc = retrieve' t (map (g :) acc)
  retrieve' (Or t1 t2 _ _) acc = retrieve' t1 acc ++ retrieve' t2 acc
  retrieve' (Split _ ts _ _) acc = concatMap (\t -> retrieve' t acc) ts
  retrieve' (Prune gs) acc = acc
  --
  -- data GlobalTree = Leaf  (Descend [G S]) T.Generalizer E.Sigma
  --                 | Node  (Descend [G S]) T.Generalizer CPD.SldTree [GlobalTree]
  --                 | Prune (Descend [G S]) E.Sigma

--globalTreeRetrieve :: G.GlobalTree -> [[G S]]
globalTreeRetrieve tree = map reverse $ filter notNull $ retrieve' tree [[]] where -- map reverse $ Set.toList $ Set.fromList $ retrieve' tree  where
  retrieve' (G.Leaf _ _ _) acc = [[]]
  retrieve' (G.Node d _ _ ch) acc = concatMap (\x -> retrieve' x (map (CPD.getCurr d : ) acc)) ch
  retrieve' (G.Prune d _) acc = map ((CPD.getCurr d) :) acc -- Set.insert (CPD.getCurr d) (CPD.getAncs d)
