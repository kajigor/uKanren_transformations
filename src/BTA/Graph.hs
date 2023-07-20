
module BTA.Graph where

import Prelude hiding (lookup)
import Data.Map hiding (map, foldl, foldr)
import qualified Data.List as List
import BTA.Conditions hiding ((==))
import BTA.SizeConversion
import Data.Maybe
import BTA.Condition hiding ((==))


data TypeEdge = 
    Arc | 
    WeightedArc 
    deriving (Eq, Show)

data Graph a = 
    Graph [a] (Map (a, a) (AbstractTerm String, TypeEdge))
    deriving (Eq, Show)

addCond :: Ord a => (Map (a, a) (AbstractTerm String, TypeEdge)) -> Condition a -> (Map (a, a) (AbstractTerm String, TypeEdge))
addCond mp (Lt a b) = (insert (b, a) (Sum 1 empty, Arc) mp)
addCond mp (Eq a b) = (insert (a, b) (Sum 0 empty, WeightedArc) (insert (b, a) (Sum 0 empty, WeightedArc) mp))

addWeightCond :: Ord a => Graph a -> a -> a -> (AbstractTerm String) -> Graph a 
addWeightCond graph@(Graph vars mp) a b term = Graph vars (insert (a, b) (term, WeightedArc) (insert (b, a) (negative term, WeightedArc) mp))

getFromConjunction :: Ord a => Conditions a -> [a] -> Graph a
getFromConjunction initCond@(ConditionConj conjuncts) vars = 
    let graphMap = foldl (\mp -> \x@(Condition cond) -> (addCond mp cond)) (empty :: Map (a, a) (AbstractTerm String, TypeEdge)) conjuncts in
    supplement (Graph vars graphMap)

getPathLength :: Ord a => Map (a, a) (AbstractTerm String, TypeEdge) -> [a] -> (AbstractTerm String)
getPathLength graphMap (start : path) | (any (\(a, b) -> not (member (a, b) graphMap)) (zip (start : path) path)) = (Sum (-1) empty)
                                      | otherwise = (foldl (<>) (Sum 0 empty) (map (\x -> fst $ fromJust $ lookup x graphMap) (zip (start : path) path)))


checkZeroPath :: Ord a => Map (a, a) (AbstractTerm String, TypeEdge) -> [a] -> Bool 
checkZeroPath graphMap (start : path) = 
    let pathMaybe = (map (\x -> lookup x graphMap) (zip (start : path) path)) in 
    let path = map fromJust pathMaybe in 
    (all ( ((==) WeightedArc) . snd) path)

positivePath :: Ord a => a -> a -> Graph a -> Bool 
positivePath a b graph@(Graph vars graphMap) = 
    let availableVert = List.delete a (List.delete b vars) in
    let subs = List.subsequences availableVert in 
    let perms = concatMap List.permutations subs in 
    any (isPositive . (getPathLength graphMap) . (\x -> [a] ++ x ++ [b])) perms


zeroPath :: Ord a => a -> a -> Graph a -> Bool 
zeroPath a b graph@(Graph vars graphMap) = 
    let availableVert = List.delete a (List.delete b vars) in
    let subs = List.subsequences availableVert in 
    let perms = concatMap List.permutations subs in 
    any ((\path -> (getPathLength graphMap path == (Sum 0 empty)) && (checkZeroPath graphMap path)) . (\x -> [a] ++ x ++ [b])) perms


addEdge :: Ord a => Graph a -> (a, a) -> Graph a 
addEdge graph@(Graph vars graphMap) (a, b)  | (member (a, b) graphMap) = graph
                                            | (positivePath a b graph) = Graph vars $ insert (a, b) (Sum 1 empty, Arc) graphMap
                                            | (zeroPath a b graph) && (a /= b) = Graph vars $ insert (a, b) (Sum 0 empty, WeightedArc) graphMap
                                            | otherwise = graph
     

supplement :: Ord a => Graph a -> Graph a
supplement graph@(Graph vars _) = 
    let tuples = [(a, b) | a <- vars, b <- vars] in 
    foldl addEdge graph tuples

addVertex :: Ord a => Graph a -> a -> Graph a
addVertex (Graph vars graphMap) var = (Graph (var : vars) graphMap)

removeVertex :: Ord a => Graph a -> a -> Graph a 
removeVertex (Graph vars graphMap) vertex = 
    let tuples = [(vertex, a) | a <- vars] ++ [(a, vertex) | a <- vars] in
    Graph (List.delete vertex vars) (foldr delete graphMap tuples)

getState :: Ord a => Graph a -> (a, a) -> [Conditions a]
getState graph (a, b) | (positivePath a b graph) && (zeroPath a b graph) = [(Condition (Lt b a)), (Condition (Eq a b))]
                      | (positivePath a b graph) = [Condition (Lt b a)]
                      | (zeroPath a b graph) = [Condition (Eq a b)]
                      | otherwise = []


getConditionsFromGraph :: Ord a => Graph a -> Conditions a 
getConditionsFromGraph graph@(Graph vars graphMap) = 
    let tuples = [(a, b) | a <- vars, b <- vars] in 
    let conjuncts = concatMap (getState graph) tuples in 
    ConditionDisj [ConditionConj conjuncts]

addCondDifTypes :: Ord a => Graph a -> ((a, a), (AbstractTerm String, TypeEdge)) -> Graph a
addCondDifTypes graph@(Graph vars mp) ((a, b), (weight, t)) | t == WeightedArc = addWeightCond graph a b weight 
                                                            | (member (a, b) mp) = graph
                                                            | otherwise = Graph vars $ insert (a, b) (weight, t) mp

unionGraphs :: Ord a => Graph a -> Graph a -> Graph a 
unionGraphs graph1@(Graph vars1 mp1) graph2@(Graph vars2 mp2) = 
    foldl addCondDifTypes (Graph (List.union vars1 vars2) mp1) $ assocs mp2

cleanGraph :: Ord a => Graph a -> [a] -> Graph a
cleanGraph graph@(Graph vars mp) vertexes = 
    foldl removeVertex graph (vars List.\\ vertexes)