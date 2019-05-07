module GlobalTreePrinter where

import DotPrinter

import qualified Eval as E
import Syntax
import CPD hiding (Leaf)
import GlobalControl
import Text.Printf

instance DotPrinter GlobalTree where
  -- labelNode t@(Node _ ch) i vs es ids = addChildren t ch i vs es ids -- (filter (\t -> not $ case t of Leaf _ _ -> True ; _ -> False) ch) i vs es ids
  -- labelNode t@(Prune _ _) i vs es ids = addLeaf     t    i vs es ids
  -- labelNode _ _ vs es ids = (vs, es, ids)

  -- labelNode t@(Node _ _ _ ch) i vs es ids = addChildren t (filter (not . isLeaf) ch) i vs es ids -- (filter (\t -> not $ case t of Leaf _ _ -> True ; _ -> False) ch) i vs es ids
  labelNode t@(Node _ _ _ ch) i vs es ids = addChildren t ch i vs es ids -- (filter (\t -> not $ case t of Leaf _ _ -> True ; _ -> False) ch) i vs es ids
  labelNode t                 i vs es ids = addLeaf     t    i vs es ids

isLeaf (Leaf _ _ _) = True
isLeaf _ = False


  -- -- addChild :: a -> a -> Id -> [Vertex] -> [Edge] -> [Id] -> ([Vertex], [Edge], [Id])
  -- addChild n ch nodeId ns es (childId : ids) =
  --     let (ns', es', ids') = labelNode ch childId ns es ids
  --     in  ((nodeId, dot n) : ns', (nodeId, childId, "") : es', ids')
  --
  -- -- addChildren :: a -> [a] -> Id -> [Vertex] -> [Edge] -> [Id] -> ([Vertex], [Edge], [Id])
  -- addChildren n ch nodeId ns es ids =
  --     let (names, otherIds) = splitAt (length ch) ids in
  --     let (nss, ess, idss) = foldl (\(ns', es', ids') (ch, key) -> labelNode ch key ns' es' ids') (ns, es, otherIds) (zip ch names) in
  --     ((nodeId, dot n) : nss, map (\x -> (nodeId, x, "")) names ++ ess, idss)

 --a -> Id -> [Vertex] -> [Edge] -> [Id] -> ([Vertex], [Edge], [Id])

instance Dot GlobalTree where
  -- dot (Leaf gs gen s)   = printf "L <BR/> %s <BR/> %s <BR/> %s" (dot $ getCurr gs) (dot gen) (dot s)
  dot (Leaf gs _ _)  = printf "L <BR/> %s" (dot $ getCurr gs)
  dot (Node gs gen _ _) = printf "N <BR/> %s <BR/> %s" (dot $ getCurr gs) (dot gen)
  dot (Prune gs _)  = printf "P <BR/> %s" (dot $ getCurr gs)
