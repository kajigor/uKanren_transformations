
module BTA.TerminationCheck where 

import AnnotatedProgram
import BTA.SizeConversion 
import AnnotatedDef
import AnnotationType
import BTA.Conditions
import qualified Data.Map as Map
import FreshNames
import Control.Monad.State
import BTA.Inequalities 
import BTA.Graph
import qualified InvokeAnnotation as Inv
import qualified Data.List as List
import qualified Data.List.Split as Split
import Data.Maybe
import Data.Map as Map hiding (foldl, foldr, map, fmap, filter)
import Data.Set as Set hiding (map, foldl, foldr, fmap, filter)
import Debug.Trace

import Control.Monad  (join)
import Data.Bifunctor (bimap)

terminationCheck :: Ord a => AnnotatedProgram AbstractG a -> Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef AbstractG a) -> Bool 
terminationCheck program@(AnnotatedProgram defs goal) mapConditions mapDefs = 
    let (defsGraphs, defsVars) = unzip $ map (getPairsDef mapConditions mapDefs) defs in 
    let defsVarsRes = Map.unions defsVars in 
    let defsGraphsRes = goGraphMap (Map.unions defsGraphs) defsVarsRes (map getName defs) in 
    all (\name -> checkDecreasing (Map.findWithDefault Set.empty (name, name) defsGraphsRes) (filterArgs (getAnnotations $ mapDefs Map.! name) $ defsVarsRes Map.! (name, name))) $ map getName defs
    where filterArgs :: [AnnotationType] -> ([String], [String]) -> ([String], [String])
          filterArgs annotations (inArgs, outArgs) = 
            let indices = List.elemIndices Static annotations in 
            (map (inArgs !!) indices, map (outArgs !!) indices)
    

checkPossibleZero :: Graph String -> Map.Map ([String], [String]) Bool -> ([String], [String]) -> Map.Map ([String], [String]) Bool 
checkPossibleZero graph curPossible (inArgs, outArgs) = 
    let isPossible = any (\(inArg, outArg) -> Map.findWithDefault False (List.delete inArg inArgs, List.delete outArg outArgs) curPossible && atLeastZeroPath inArg outArg graph) [(inArg, outArg) | inArg <- inArgs, outArg <- outArgs] in
    case isPossible of 
        True -> Map.insert (inArgs, outArgs) True curPossible 
        False -> curPossible


checkSubSequences :: ([String], [String]) -> Graph String -> Bool 
checkSubSequences ([], []) graph = False
checkSubSequences (inArgs, outArgs) graph =
    let mapZero = foldl (checkPossibleZero (trace ("subseq" ++ show graph) graph)) (Map.fromList [(([], []), True)]) $ filter (\(ins, outs) -> length ins == length outs) [(seq1, seq2) | seq1 <- List.subsequences inArgs, seq2 <- List.subsequences outArgs] in 
    any (\(inArg, outArg) -> Map.findWithDefault False (List.delete inArg inArgs, List.delete outArg outArgs) mapZero && positivePath inArg outArg graph) [(inArg, outArg) | inArg <- inArgs, outArg <- outArgs]

checkDecreasing :: Set.Set (Graph String) -> ([String], [String]) -> Bool
checkDecreasing setGraphs (inArgs, outArgs) = 
    any (\subseq -> all (checkSubSequences $ traceShow subseq subseq) $ trace ("graphs" ++ show setGraphs ++ show inArgs ++ show outArgs) setGraphs) $ zip (List.subsequences inArgs) (List.subsequences outArgs)


goGraphMap :: Map.Map (String, String) (Set.Set (Graph String)) -> Map.Map (String, String) ([String], [String]) -> [String] -> Map.Map (String, String) (Set.Set (Graph String)) 
goGraphMap defsGraphs defsVars defs = 
    let newDefsGraphs = goGraphsOneCycle defsGraphs defsVars defs in 
    case (newDefsGraphs == defsGraphs) of 
        True -> newDefsGraphs 
        False -> goGraphMap newDefsGraphs defsVars defs

goGraphsOneCycle :: Map.Map (String, String) (Set.Set (Graph String)) -> Map.Map (String, String) ([String], [String]) -> [String] -> Map.Map (String, String) (Set.Set (Graph String)) 
goGraphsOneCycle defsGraphs defsVars defs =
    foldl (relaxTriple defsVars) defsGraphs [(a, b, c) | a <- defs, b <- defs, c <- defs]


relaxTriple :: Map.Map (String, String) ([String], [String]) -> Map.Map (String, String) (Set.Set (Graph String)) -> (String, String, String) -> Map.Map (String, String) (Set.Set (Graph String)) 
relaxTriple defsVars defsGraphs (a, b, c) = 
    let graphs1 = Set.toList $ fromMaybe (Set.empty) $ Map.lookup (a, b) defsGraphs in 
    let graphs2 = Set.toList $ fromMaybe (Set.empty) $ Map.lookup (b, c) defsGraphs in 
    let curGraphs = fromMaybe (Set.empty) $ Map.lookup (a, c) defsGraphs in 
    let addGraphs = Set.fromList [newGraph graph1 graph2 | graph1 <- graphs1, graph2 <- graphs2] in 
    Map.insert (a, c) (Set.union curGraphs addGraphs) defsGraphs
    where   newGraph :: Graph String -> Graph String -> Graph String
            newGraph graph1 graph2 = 
                let (in1, out1) = fromJust $ Map.lookup (a, b) defsVars in 
                let (in2, out2) = fromJust $ Map.lookup (b, c) defsVars in 
                let (inRes, outRes) = fromJust $ Map.lookup (a, c) defsVars in  
                let (newNamesInt, fn) = getNames (length in1 + length out1 + length out2) $ defaultNames :: ([Int], FreshNames) in 
                let newNames = map show newNamesInt in 
                let mapVars1 = Map.fromList (zip (in1 ++ out1) newNames) in 
                let mapVars2 = Map.fromList (zip (in2 ++ out2) (List.drop (length in1) newNames)) in 
                let graph1New = graphfmap (\x -> fromJust $ Map.lookup x mapVars1) graph1 in 
                let graph2New = graphfmap (\x -> fromJust $ Map.lookup x mapVars2) graph2 in 
                let coreVertexes = (List.take (length in1) newNames ++ List.drop (length in1 + length out1) newNames) in 
                let graphNew = cleanGraph (supplement $ unionGraphs graph1New graph2New) coreVertexes in 
                let mapVars3 = Map.fromList (zip coreVertexes (inRes ++ outRes)) in 
                graphfmap (\x -> fromJust $ Map.lookup x mapVars3) graphNew 

renamingPair :: String -> Map.Map String [String] -> (String, Graph String, [String], [String]) -> ((String, String), Graph String)
renamingPair name mapDefsArgs (nameTo, graph, inArgs, outArgs) = 
    let newOutArgs = fromJust $ Map.lookup nameTo mapDefsArgs in 
    let curMapArgs = Map.fromList $ (zip inArgs inArgs) ++ (zip outArgs newOutArgs) in 
    let newGraph = graphfmap (\x -> fromJust $ Map.lookup x curMapArgs) graph in 
    ((name, nameTo), newGraph) 


getPairsDef :: Ord a => Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef AbstractG a) -> AnnotatedDef AbstractG a -> (Map.Map (String, String) (Set.Set (Graph String)), Map.Map (String, String) ([String], [String]))
getPairsDef mapConditions mapDefs def@(AnnotatedDef name args body annotations) = 
    let (namesInt, fn) = getNames (length args) $ defaultNames :: ([Int], FreshNames) in 
    let namesStr = map show (namesInt) :: [String] in 
    let mapVars = Map.fromList $ zip args namesStr in 
    let (namesDefsInt, fnNew) = getNames (sum $ map (length . getArgs) (Map.elems mapDefs)) fn in 
    let namesDefsStr = Split.splitPlaces (map (length . getArgs) (Map.elems mapDefs)) $ map show namesDefsInt in 
    let mapDefsArgs = Map.fromList $ zip (Map.keys mapDefs) namesDefsStr in 
    let pairs = (goPairsBody body mapConditions mapDefs namesStr namesStr mapVars fnNew) in 
    let preparedPairs = List.groupBy (\(a1, b1) (a2, b2) -> a1 == a2) $ map (renamingPair name mapDefsArgs) pairs in 
    (Map.fromList $ map (\lst -> (fst $ lst !! 0, Set.fromList $ map snd lst)) preparedPairs,
    Map.fromList $ map (\(key, val) -> ((name, key), (namesStr, val))) $ Map.toList mapDefsArgs)


formPair :: Ord a => AbstractG a -> Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef AbstractG a) -> [String] -> [String] -> Map.Map a String -> FreshNames -> [(String, Graph String, [String], [String])]
formPair goal@(Invoke name terms ann) mapConditions mapDefs baseArgs args mapVars fn | ann == Inv.Unfold = 
    let (newArgsInt, newFn) = getNames (length terms) fn in 
    let newArgsStr = map show newArgsInt in 
    let newTerms =  map (termfmap (\x -> fromJust (Map.lookup x mapVars))) terms in
    let graph = handleOneInvokeConjunct newTerms newArgsStr args (ConditionConj []) in    
    [(name, cleanGraph graph (baseArgs ++ newArgsStr), baseArgs, newArgsStr)]
formPair goal@(Invoke name terms ann) mapConditions mapDefs baseArgs args mapVars fn | otherwise = []


goPairConjunct :: Ord a => AbstractG a -> Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef AbstractG a) -> [String] -> [String] -> Map.Map a String -> State ([Graph String], [(String, Int)], FreshNames) [(String, Graph String, [String], [String])]
goPairConjunct g@(Invoke name terms ann) mapConditions mapDefs args newArgs mapVars | ann == Inv.Unfold = do 
    (graphs1, consts, fn) <- get 
    let termsNew = map (termfmap (\x -> fromJust (Map.lookup x mapVars))) (terms) 
    let (newArgsInt, fnNew) = getNames (length terms) fn 
    let newArgsStr = map show newArgsInt 
    let graphs = map (addVertexes newArgsStr) graphs1
    let mapArgs = Map.fromList $ zip (getArgs . fromJust $ Map.lookup name mapDefs) newArgsStr ---WHAT
    let conds = concatMap getCondsOneTerm (zip newArgsStr termsNew) 
    let graphsNew = map (\initGraph -> supplement $ foldl (\graph cond@(a, b, term) -> addWeightCond graph a b term) initGraph conds) graphs 
    let res = map (\graph -> (name, cleanGraph graph (args ++ newArgsStr), args, newArgsStr)) graphsNew
    let condsFun = (fromMaybe (ConditionDisj []) (Map.lookup name mapConditions))
    let newConds@(ConditionDisj disjuncts) = fmap (\x -> fromJust $ Map.lookup x mapArgs) condsFun 
    let graphsRes = [unionGraphs graph (getFromConjunction cond newArgsStr) | graph <- graphsNew, cond <- disjuncts] 
    put (graphsRes, consts, fnNew)
    return res
goPairConjunct g@(Invoke name terms ann) mapConditions mapDefs args newArgs mapVars | otherwise = do 
    return []
goPairConjunct g@(term1 :=: term2) mapConditions mapDefs args newArgs mapVars = do
    (graphs, consts, fn) <- get 
    let ((graphs1, consts1), fn1) = runState (goOneConjunct g Map.empty mapConditions args mapVars) fn 
    let resGraphs = [unionGraphs graph graph1 | graph <- graphs, graph1 <- graphs1] 
    let resGraphsWithConsts = map (\curg -> foldl (\g ((v1 ,n1), (v2, n2)) -> addWeightCond g v1 v2 (Sum (n2 - n1) Map.empty)) curg [(v1, v2) | v1 <- consts, v2 <- consts1]) resGraphs
    put (resGraphsWithConsts, List.union consts consts1, fn1)
    return []

goPairsConjunction :: Ord a => [AbstractG a] -> Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef AbstractG a) -> [String] -> [String] -> Map.Map a String -> State ([Graph String], [(String, Int)], FreshNames) [(String, Graph String, [String], [String])]
goPairsConjunction lstG mapConditions mapDefs args newArgs mapVars = do
    res <- mapM (\goal -> goPairConjunct goal mapConditions mapDefs args newArgs mapVars) lstG
    return $ concat res

goPairsBody :: Ord a => AbstractG a -> Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef AbstractG a) -> [String] -> [String] -> Map.Map a String -> FreshNames ->  [(String, Graph String, [String], [String])]
goPairsBody goal@(Conjunction g1 g2 lstG) mapConditions mapDefs args newArgs mapVars fn = 
    let (pairs, state) = runState (goPairsConjunction (g1 : g2 : lstG) mapConditions mapDefs args newArgs mapVars) ([Graph newArgs Map.empty], [], fn) in 
    pairs
goPairsBody goal@(Disjunction g1 g2 lstG) mapConditions mapDefs args newArgs mapVars fn = 
    concatMap (\goal -> goPairsBody goal mapConditions mapDefs args newArgs mapVars fn) (g1 : g2 : lstG) 
goPairsBody goal@(Fresh x g) mapConditions mapDefs args newArgs mapVars fn = 
    let (newVarInt, fnNew) = getFreshName fn in 
    let newVarStr = show newVarInt in
    let newMapVars = Map.insert x newVarStr mapVars in 
    goPairsBody g mapConditions mapDefs args (newVarStr : newArgs) newMapVars fnNew
goPairsBody goal@(Invoke name terms ann) mapConditions mapDefs args newArgs mapVars fn =
    formPair goal mapConditions mapDefs args newArgs mapVars fn
goPairsBody goal@(term1 :=: term2) mapConditions mapDefs args newArgs mapVars fn = []