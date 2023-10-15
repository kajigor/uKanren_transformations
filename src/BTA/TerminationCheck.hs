
module BTA.TerminationCheck where 

import           Control.Monad.State
import           Data.Group
import           Debug.Trace
import           BTA.AnnotatedDef
import           BTA.AnnotatedProgram
import           BTA.AnnotationType
import           BTA.SizeConversion
import           BTA.Conditions
import           BTA.Graph
import           BTA.Inequalities 
import           BTA.InvokeAnnotation 
import           FreshNames
import qualified Data.Map             as Map
import qualified Data.List            as List
import qualified Data.List.Split      as Split
import qualified Data.Set             as Set
import           Control.Exception

terminationCheck :: Ord a => AnnotatedProgram (AnnG AbstractTerm) a -> Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef (AnnG AbstractTerm) a) -> Bool 
terminationCheck (AnnotatedProgram defs _) mapConditions mapDefs = 
    let (defsGraphs, defsVars) = unzip $ map (getPairsDef mapConditions mapDefs) defs in 
    let defsVarsRes = Map.unions defsVars in 
    let defsGraphsRes = goGraphMap (Map.unions defsGraphs) defsVarsRes $ map getName defs in 
    all (\name -> checkDecreasing (Map.findWithDefault Set.empty (name, name) defsGraphsRes) (filterArgs (getAnnotations $ mapDefs Map.! name) $ defsVarsRes Map.! (name, name))) $ map getName defs
  where 
    filterArgs :: [AnnotationType] -> ([String], [String]) -> ([String], [String])
    filterArgs annotations (inArgs, outArgs) = 
        let indices = List.elemIndices Static annotations in 
        (map (inArgs !!) indices, map (outArgs !!) indices)
    

checkPossibleZero :: Graph String -> Set.Set ([String], [String]) -> ([String], [String]) -> Set.Set ([String], [String]) 
checkPossibleZero graph curPossible (inArgs, outArgs) = 
    let isPossible = or $ (\inArg outArg -> Set.member (List.delete inArg inArgs, List.delete outArg outArgs) curPossible && atLeastZeroPath inArg outArg graph) <$> inArgs <*> outArgs in
    if isPossible 
    then
      Set.insert (inArgs, outArgs) curPossible 
    else 
      curPossible


checkSubSequences :: ([String], [String]) -> Graph String -> Bool 
checkSubSequences ([], []) graph = False
checkSubSequences (inArgs, outArgs) graph =
    let setZero = foldl (checkPossibleZero graph) (Set.singleton ([], [])) $ filter (\(ins, outs) -> length ins == length outs) $ (,) <$> List.subsequences inArgs <*> List.subsequences outArgs in 
    any (\(inArg, outArg) -> Set.member (List.delete inArg inArgs, List.delete outArg outArgs) setZero && positivePath inArg outArg graph) $ (,) <$> inArgs <*> outArgs


checkDecreasing :: Set.Set (Graph String) -> ([String], [String]) -> Bool
checkDecreasing setGraphs (inArgs, outArgs) = 
    any (\subseq -> all (checkSubSequences subseq) setGraphs) $ zip (List.subsequences inArgs) (List.subsequences outArgs)


goGraphMap :: Map.Map (String, String) (Set.Set (Graph String)) -> Map.Map (String, String) ([String], [String]) -> [String] -> Map.Map (String, String) (Set.Set (Graph String)) 
goGraphMap defsGraphs defsVars defs = 
    until (\x -> goGraphsOneCycle x == x) goGraphsOneCycle defsGraphs
  where
    goGraphsOneCycle :: Map.Map (String, String) (Set.Set (Graph String)) -> Map.Map (String, String) (Set.Set (Graph String)) 
    goGraphsOneCycle defsGraphs =
        foldl (relaxTriple defsVars) defsGraphs [(a, b, c) | a <- defs, b <- defs, c <- defs]


relaxTriple :: Map.Map (String, String) ([String], [String]) -> Map.Map (String, String) (Set.Set (Graph String)) -> (String, String, String) -> Map.Map (String, String) (Set.Set (Graph String)) 
relaxTriple defsVars defsGraphs (a, b, c) = 
    let graphs1 = Set.toList $ Map.findWithDefault Set.empty (a, b) defsGraphs in 
    let graphs2 = Set.toList $ Map.findWithDefault Set.empty (b, c) defsGraphs in 
    let addGraphs = Set.fromList $ newGraph <$> graphs1 <*> graphs2 in 
    Map.insertWith Set.union (a, c) addGraphs defsGraphs
  where 
    newGraph :: Graph String -> Graph String -> Graph String
    newGraph graph1 graph2 = 
        let (in1, out1) = defsVars Map.! assert (Map.member (a, b) defsVars) (a, b) in 
        let (in2, out2) = defsVars Map.! assert (Map.member (b, c) defsVars) (b, c) in 
        let (inRes, outRes) = defsVars Map.! assert (Map.member (a, c) defsVars) (a, c) in  
        let (newNames, fn) = getNamesStr (length in1 + length out1 + length out2) $ defaultNames in 
        let mapVars1 = Map.fromList (zip (in1 ++ out1) newNames) in 
        let mapVars2 = Map.fromList (zip (in2 ++ out2) (List.drop (length in1) newNames)) in 
        let graph1New = graphfmap (mapVars1 Map.!) graph1 in 
        let graph2New = graphfmap (mapVars2 Map.!) graph2 in 
        let coreVertexes = List.take (length in1) newNames ++ List.drop (length in1 + length out1) newNames in 
        let graphNew = cleanGraph (supplement $ graph1New <> graph2New) coreVertexes in 
        let mapVars3 = Map.fromList $ zip coreVertexes (inRes ++ outRes) in 
        graphfmap (mapVars3 Map.!) graphNew 


renamingPair :: String -> Map.Map String [String] -> (String, Graph String, [String], [String]) -> ((String, String), Graph String)
renamingPair name mapDefsArgs (nameTo, graph, inArgs, outArgs) = 
    let newOutArgs = mapDefsArgs Map.! nameTo in 
    let curMapArgs = Map.fromList $ (zip inArgs inArgs) ++ (zip outArgs newOutArgs) in 
    let newGraph = graphfmap (curMapArgs Map.!) graph in 
    ((name, nameTo), newGraph) 


getPairsDef :: Ord a => Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef (AnnG AbstractTerm) a) -> AnnotatedDef (AnnG AbstractTerm) a -> (Map.Map (String, String) (Set.Set (Graph String)), Map.Map (String, String) ([String], [String]))
getPairsDef mapConditions mapDefs def@(AnnotatedDef name args body annotations) = 
    let (namesStr, fn) = getNamesStr (length args) $ defaultNames in 
    let mapVars = Map.fromList $ zip args namesStr in 
    let (namesDefsInt, fnNew) = getNames ((sum $ map (length . getArgs) $ Map.elems mapDefs) + 1) fn in 
    let namesDefsStr = Split.splitPlaces (map (length . getArgs) $ Map.elems mapDefs) $ map show namesDefsInt in 
    let mapDefsArgs = Map.fromList $ zip (Map.keys mapDefs) namesDefsStr in 
    let pairs = goPairsBody body mapConditions mapDefs namesStr namesStr mapVars fnNew in 
    let preparedPairs = List.groupBy (\(a1, b1) (a2, b2) -> a1 == a2) $ map (renamingPair name mapDefsArgs) pairs in 
    (Map.fromListWith Set.union $ map (\lst -> (fst $ lst !! 0, Set.fromList $ map snd lst)) preparedPairs,
    Map.fromList $ map (\(key, val) -> ((name, key), (namesStr, val))) $ Map.toList mapDefsArgs)


formPair :: Ord a => (AnnG AbstractTerm) a -> [String] -> [String] -> Map.Map a String -> FreshNames -> [(String, Graph String, [String], [String])]
formPair goal@(Invoke name terms ann) baseArgs args mapVars fn | ann == Unfold = 
    let (newArgsStr, newFn) = getNamesStr (length terms) fn in 
    let newTerms =  map (termfmap (mapVars Map.!)) terms in
    let graph = handleOneInvokeConjunct newTerms newArgsStr args (ConditionConj []) in  
    case withoutPositiveCycle graph of 
        True -> [(name, cleanGraph graph (baseArgs ++ newArgsStr), baseArgs, newArgsStr)]
        False -> []
formPair goal@(Invoke name terms ann) baseArgs args mapVars fn | otherwise = []


goPairConjunct :: Ord a => (AnnG AbstractTerm) a -> Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef (AnnG AbstractTerm) a) -> [String] -> [String] -> Map.Map a String -> State ([Graph String], [(String, AbstractTerm String)], FreshNames) [(String, Graph String, [String], [String])]
goPairConjunct g@(Invoke name terms ann) mapConditions mapDefs args newArgs mapVars | ann == Unfold = do 
    (graphs1, consts, fn) <- get 
    let termsNew = map (termfmap (mapVars Map.!)) terms 
    let (newArgsStr, fnNew) = getNamesStr (length terms) fn 
    let graphs = map (addVertexes newArgsStr) graphs1
    let mapArgs = Map.fromList $ zip (getArgs $ mapDefs Map.! name) newArgsStr
    let conds = concatMap getCondsOneTerm $ zip newArgsStr termsNew 
    let graphsNew = filter withoutPositiveCycle $ map (\initGraph -> supplement $ foldl (\graph cond@(a, b, term) -> addWeightCond graph a b term) initGraph conds) graphs 
    let res = map (\graph -> (name, cleanGraph graph (args ++ newArgsStr), args, newArgsStr)) graphsNew
    let condsFun = Map.findWithDefault disjEmpty name mapConditions
    let newConds@(ConditionDisj disjuncts) = fmap (mapArgs Map.!) condsFun 
    let graphsRes = filter withoutPositiveCycle $ [graph <> (getFromConjunction cond newArgsStr) | graph <- graphsNew, cond <- disjuncts] 
    put (graphsRes, consts, fnNew)
    return res
goPairConjunct g@(Invoke name terms ann) mapConditions mapDefs args newArgs mapVars | otherwise = do 
    return []
goPairConjunct g@(term1 :=: term2) mapConditions mapDefs args newArgs mapVars = do
    (graphs, consts, fn) <- get 
    let ((graphs1, consts1), fn1) = runState (goOneConjunct g Map.empty mapConditions args mapVars) fn 
    let resGraphs = [graph <> graph1 | graph <- graphs, graph1 <- graphs1] 
    let resGraphsWithConsts = filter withoutPositiveCycle $ map (\curg -> foldl (\g ((v1, t1), (v2, t2)) -> addWeightCond g v1 v2 (t2 ~~ t1)) curg [(v1, v2) | v1 <- consts, v2 <- consts1]) resGraphs
    put (resGraphsWithConsts, List.union consts consts1, fn1)
    return []


goPairsConjunction :: Ord a => [AnnG AbstractTerm a] -> Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef (AnnG AbstractTerm) a) -> [String] -> [String] -> Map.Map a String -> State ([Graph String], [(String, AbstractTerm String)], FreshNames) [(String, Graph String, [String], [String])]
goPairsConjunction lstG mapConditions mapDefs args newArgs mapVars = do
    res <- mapM (\goal -> goPairConjunct goal mapConditions mapDefs args newArgs mapVars) lstG
    return $ concat res


goPairsBody :: Ord a => AnnG AbstractTerm a -> Map.Map String (Conditions a) -> Map.Map String (AnnotatedDef (AnnG AbstractTerm) a) -> [String] -> [String] -> Map.Map a String -> FreshNames ->  [(String, Graph String, [String], [String])]
goPairsBody goal@(Conjunction g1 g2 lstG) mapConditions mapDefs args newArgs mapVars fn = 
    let (pairs, state) = runState (goPairsConjunction (g1 : g2 : lstG) mapConditions mapDefs args newArgs mapVars) ([Graph newArgs Map.empty], [], fn) in 
    pairs
goPairsBody goal@(Disjunction g1 g2 lstG) mapConditions mapDefs args newArgs mapVars fn = 
    concatMap (\goal -> goPairsBody goal mapConditions mapDefs args newArgs mapVars fn) $ g1 : g2 : lstG 
goPairsBody goal@(Fresh x g) mapConditions mapDefs args newArgs mapVars fn = 
    let (newVarInt, fnNew) = getFreshName fn in 
    let newVarStr = show newVarInt in
    let newMapVars = Map.insert x newVarStr mapVars in 
    goPairsBody g mapConditions mapDefs args (newVarStr : newArgs) newMapVars fnNew
goPairsBody goal@(Invoke name terms ann) mapConditions mapDefs args newArgs mapVars fn =
    formPair goal args newArgs mapVars fn
goPairsBody goal@(term1 :=: term2) mapConditions mapDefs args newArgs mapVars fn = []