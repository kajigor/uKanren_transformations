{-# LANGUAGE InstanceSigs #-}

module BTA.Graph where

import           Data.Bifunctor  (bimap)
import           Data.Group
import           Data.Map        hiding (map, foldl, foldr, filter, mapMaybe)
import           Data.Maybe
import           Debug.Trace
import           Prelude         hiding (lookup)
import           BTA.Condition
import           BTA.Conditions 
import           BTA.SizeConversion
import qualified Data.List       as List
import qualified Data.Set        as Set


data TypeEdge = 
    Arc | 
    WeightedArc 
    deriving (Eq, Show, Ord)

data Graph a = 
    Graph [a] (Map (a, a) (AbstractTerm a, TypeEdge))
    deriving (Eq, Show, Ord)

instance Ord a => Semigroup (Graph a) where
    (<>) :: Graph a -> Graph a -> Graph a
    graph1@(Graph vars1 mp1) <> graph2@(Graph vars2 mp2) = 
        foldl addCondDifTypes (Graph (List.union vars1 vars2) mp1) $ assocs mp2

graphfmap :: Ord b => (a -> b) -> Graph a -> Graph b 
graphfmap f (Graph vars mp) = Graph (map f vars) $ fmap (\(a, b) -> (termfmap f a, b)) $ mapKeys (bimap f f) mp

addCond :: Ord a => (Map (a, a) (AbstractTerm a, TypeEdge)) -> Condition a -> (Map (a, a) (AbstractTerm a, TypeEdge))
addCond mp (Lt a b) = insert (b, a) (Sum 1 empty, Arc) mp
addCond mp (Eq a b) = insert (a, b) (Sum 0 empty, WeightedArc) $ insert (b, a) (Sum 0 empty, WeightedArc) mp

addWeightCond :: Ord a => Graph a -> a -> a -> (AbstractTerm a) -> Graph a 
addWeightCond graph@(Graph vars mp) a b term = Graph vars (insert (a, b) (term, WeightedArc) (insert (b, a) (invert term, WeightedArc) mp))

getFromConjunction :: Ord a => Conditions a -> [a] -> Graph a
getFromConjunction initCond@(ConditionConj conjuncts) vars = 
    let graphMap = foldl (\mp -> \x@(Condition cond) -> (addCond mp cond)) (empty :: Map (a, a) (AbstractTerm a, TypeEdge)) conjuncts in
    supplement (Graph vars graphMap)


zeroPathHelp :: Ord a => AbstractTerm a -> Int -> Graph a -> a -> a -> Set.Set a -> Bool
zeroPathHelp (Sum 0 mp) n graph a b visited | a == b && mp == empty = True
zeroPathHelp curSum 0 graph a b visited = False
zeroPathHelp curSum n graph@(Graph vars graphMap) a b visited = 
    let next_pos = filter (\v -> member (a, v) graphMap && (not $ Set.member v visited)) vars in 
    any (\nxt -> zeroPathHelp (curSum <> (fst $ graphMap ! (a, nxt))) (n - 1) graph nxt b (Set.insert nxt visited)) $ filter (\v -> (snd $ graphMap ! (a, v)) == WeightedArc) next_pos


positivePathHelp :: Ord a => AbstractTerm a -> Int -> Graph a -> a -> a -> Set.Set a -> Bool
positivePathHelp curSum n graph a b visited | isPositive curSum && a == b = True
positivePathHelp curSum 0 graph a b visited = False
positivePathHelp curSum n graph@(Graph vars graphMap) a b visited = 
    let next_pos = filter (\v -> member (a, v) graphMap && (not $ Set.member v visited)) vars in 
    any (\nxt -> positivePathHelp (curSum <> (fst $ graphMap ! (a, nxt))) (n - 1) graph nxt b (Set.insert nxt visited)) next_pos

positivePath :: Ord a => a -> a -> Graph a -> Bool 
positivePath a b graph@(Graph vars graphMap) = 
    positivePathHelp (Sum 0 empty) (length vars - 1) graph a b $ Set.fromList [a]

atLeastZeroPath :: Ord a => a -> a -> Graph a -> Bool 
atLeastZeroPath a b graph@(Graph vars graphMap) = 
    positivePath a b graph || zeroPath a b graph


zeroPath :: Ord a => a -> a -> Graph a -> Bool 
zeroPath a b graph@(Graph vars graphMap) = 
    zeroPathHelp (Sum 0 empty) (length vars - 1) graph a b $ Set.fromList [a]


addEdge :: Ord a => Graph a -> (a, a) -> Graph a 
addEdge graph@(Graph vars graphMap) (a, b)  | member (a, b) graphMap = graph
                                            | positivePath a b graph = Graph vars $ insert (a, b) (Sum 1 empty, Arc) graphMap
                                            | zeroPath a b graph && a /= b = Graph vars $ insert (a, b) (Sum 0 empty, WeightedArc) graphMap
                                            | otherwise = graph
     

supplement :: Ord a => Graph a -> Graph a
supplement graph@(Graph vars _) = 
    let tuples = [(a, b) | a <- vars, b <- vars] in 
    foldl addEdge graph tuples

addVertexes :: Ord a => [a] -> Graph a -> Graph a 
addVertexes vars1 graph@(Graph vars graphMap) = Graph (List.union vars vars1) graphMap

removeVertex :: Ord a => Graph a -> a -> Graph a 
removeVertex (Graph vars graphMap) vertex = 
    let tuples = [(vertex, a) | a <- vars] ++ [(a, vertex) | a <- vars] in
    Graph (List.delete vertex vars) $ foldr delete graphMap tuples

getState :: Ord a => Graph a -> (a, a) -> [Conditions a]
getState graph (a, b) | positivePath a b graph && zeroPath a b graph = [(Condition (Lt b a)), (Condition (Eq a b))]
                      | positivePath a b graph = [Condition (Lt b a)]
                      | zeroPath a b graph = [Condition (Eq a b)]
                      | otherwise = []


getConditionsFromGraph :: Ord a => Graph a -> Conditions a 
getConditionsFromGraph graph@(Graph vars graphMap) = 
    let tuples = [(a, b) | a <- vars, b <- vars, a/= b] in 
    let conjuncts = concatMap (getState graph) tuples in 
    ConditionDisj [ConditionConj conjuncts]

addCondDifTypes :: Ord a => Graph a -> ((a, a), (AbstractTerm a, TypeEdge)) -> Graph a
addCondDifTypes graph@(Graph vars mp) ((a, b), (weight, t)) | t == WeightedArc = addWeightCond graph a b weight 
                                                            | (member (a, b) mp) = graph
                                                            | otherwise = Graph vars $ insert (a, b) (weight, t) mp

relaxPair :: Ord a => Graph a -> (a, a) -> Maybe (a, a, Bool, Bool)
relaxPair graph@(Graph vars graphMap) (a, b) = 
    let (term@(Sum n mp), t) = graphMap ! (a, b) in 
    case (all (\x -> elem x vars) $ keys mp) of 
        True -> Nothing  
        False -> Just (a, b, positivePath a b graph, zeroPath a b graph)

addWithFlags :: Ord a => Graph a -> (a, a, Bool, Bool) -> Graph a 
addWithFlags graph@(Graph vars mp) (a, b, p1, p2) | p1 = Graph vars $ insert (a, b) (Sum 1 empty, Arc) mp
                                                  | p2 = Graph vars $ insert (a, b) (Sum 0 empty, WeightedArc) mp 
                                                  | otherwise = graph

cleanGraph :: Ord a => Graph a -> [a] -> Graph a
cleanGraph graph@(Graph vars mp) vertexes = 
    let withoutVert@(Graph vars1 mp1) = foldl removeVertex graph (vars List.\\ vertexes) in 
    let pairs = mapMaybe (relaxPair withoutVert) $ keys mp1 in 
    let cleaned = foldl (\(Graph vars mp) (a, b, p1, p2) -> (Graph vars $ delete (a, b) mp)) withoutVert pairs in 
    foldl addWithFlags cleaned pairs

withoutPositiveCycle :: Ord a => Graph a -> Bool 
withoutPositiveCycle graph@(Graph vars mp) = 
    not $ any (\v -> positivePath v v graph) vars