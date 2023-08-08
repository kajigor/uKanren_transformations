{-# LANGUAGE TupleSections #-}

module BTA.Inequalities where 

import           Data.List.NonEmpty  (NonEmpty(..))
import           Data.Map            hiding (map, foldl, filter)
import           Data.Group          
import           Data.Semigroup      (sconcat)
import           Data.Tuple
import           Debug.Trace
import           Control.Monad.State
import           Prelude             hiding (lookup, null)
import           BTA.AnnotatedDef
import           BTA.Conditions
import           BTA.Graph
import           BTA.InvokeAnnotation
import           BTA.SizeConversion
import           FreshNames


go :: Ord a => [AnnotatedDef (AnnG AbstractTerm) a] -> Map String (AnnotatedDef (AnnG AbstractTerm) a) -> Map String (Conditions a) -> Map String (Conditions a)
go defs mapDefs mapConditions = 
    until (\x -> goOneCycle defs mapDefs x == x) (goOneCycle defs mapDefs) mapConditions


goOneCycle :: Ord a => [AnnotatedDef (AnnG AbstractTerm) a] -> Map String (AnnotatedDef (AnnG AbstractTerm) a) -> Map String (Conditions a) -> Map String (Conditions a)
goOneCycle [] mapDefs mapConditions = mapConditions
goOneCycle (def : otherDefs) mapDefs mapConditions = 
    goOneCycle otherDefs mapDefs $ goOneDef def mapDefs $ mapConditions


goOneDef :: Ord a => AnnotatedDef (AnnG AbstractTerm) a ->  Map String (AnnotatedDef (AnnG AbstractTerm) a) -> Map String (Conditions a) -> Map String (Conditions a)
goOneDef def@(AnnotatedDef name args body annotations) mapDefs mapConditions = 
    let (namesInt, fn) = getNames (length args) $ defaultNames :: ([Int], FreshNames) in 
    let namesStr = map show namesInt :: [String] in 
    let mapVars = fromList $ zip args namesStr in 
    let condsString = goBody body mapDefs mapConditions namesStr namesStr fn mapVars in 
    let mapInverse = fromList . map swap . toList $ mapVars in 
    let condsA = fmap (mapInverse !) condsString in 
    let resConds = getNewDisjunctionOr condsA $ findWithDefault disjEmpty name mapConditions in 
    insert name resConds mapConditions

getCondsOneTerm :: Ord a => (a, AbstractTerm a) -> [(a, a, AbstractTerm a)]
getCondsOneTerm (var, term@(Sum n mp)) = 
    filter (\(var, otherVar, term) -> findWithDefault 0 var mp == 0) [(var, otherVar, term ~~ fromVar otherVar) | otherVar <- keys mp]


getCondsTermToTerm :: Ord a => AbstractTerm a -> AbstractTerm a -> [(a, a, AbstractTerm a)]
getCondsTermToTerm term1@(Sum n1 mp1) term2@(Sum n2 mp2) = 
    concatMap (getCondsOneTerm . (\var -> (var, fromVar var <> term2 ~~ term1))) $ keys mp1 ++ keys mp2


handleOneInvokeConjunct :: [AbstractTerm String] -> [String] -> [String] -> Conditions String -> Graph String 
handleOneInvokeConjunct terms newVars oldVars conditions = 
    let initGraph = getFromConjunction conditions $ oldVars ++ newVars in 
    let conds = concatMap getCondsOneTerm $ zip newVars terms in 
    let resGraph = supplement $ foldl (\graph cond@(a, b, term) -> addWeightCond graph a b term) initGraph conds in
    resGraph 


handleInvokeUnfold :: Ord a => AnnG AbstractTerm a -> Map String (AnnotatedDef (AnnG AbstractTerm) a) -> Map String (Conditions a) -> [String] -> Map a String -> State FreshNames [Graph String] 
handleInvokeUnfold term@(Invoke name terms ann) mapDefs mapConditions args mapVars | ann == Unfold = do
    fn <- get 
    let condsFun = findWithDefault disjEmpty name mapConditions
    let termsNew = map (termfmap (mapVars !)) terms
    let (newArgsInt, fnNew) = getNames (length terms) fn 
    put fnNew
    let newArgsStr = map show newArgsInt 
    let mapArgs = fromList $ zip (getArgs $ mapDefs ! name) newArgsStr 
    let newConds@(ConditionDisj disjuncts) = fmap (mapArgs !) condsFun 
    return $ filter withoutPositiveCycle $ map (handleOneInvokeConjunct termsNew newArgsStr args) disjuncts
handleInvokeUnfold _ _ _ args _ = do 
    return [Graph args empty]

                                        
goOneConjunct :: Ord a => AnnG AbstractTerm a -> Map String (AnnotatedDef (AnnG AbstractTerm) a) -> Map String (Conditions a) -> [String] -> Map a String -> State FreshNames ([Graph String], [(String, AbstractTerm String)])
goOneConjunct invoke@(Invoke name terms ann) mapDefs mapConditions args mapVars = do
    graphs <- handleInvokeUnfold invoke mapDefs mapConditions args mapVars
    return (graphs, [])
goOneConjunct (term1 :=: term2) mapDefs mapConditions args mapVars = do 
    let term1New@(Sum n1 mp1) = termfmap (mapVars !) term1
    let term2New = termfmap (mapVars !) term2
    let constants = filter (null . getMap . snd) [(var, (fromVar var <> term2New ~~ term1New)) | var <- args]
    let conds = getCondsTermToTerm term1New term2New
    let smth = concatMap (getCondsOneTerm . (\var -> (var, fromVar var <> term2New ~~ term1New))) $ keys mp1 
    return (filter withoutPositiveCycle [supplement $ foldl (\graph cond@(a, b, term) -> addWeightCond graph a b term) (Graph args empty) smth], constants)


goConjunction :: Ord a => AnnG AbstractTerm a -> Map String (AnnotatedDef (AnnG AbstractTerm) a) -> Map String (Conditions a) -> [String] -> [String] -> Map a String -> State FreshNames (Conditions String)
goConjunction (Conjunction g1 g2 lstG) mapDefs mapConditions baseArgs args mapVars = do 
    resS <- mapM (\goal -> goOneConjunct goal mapDefs mapConditions args mapVars) (g1 : g2 : lstG)
    let (variousGraphs, constants1) = unzip resS
    let constants = concat constants1
    let variousGraphSequences = foldl (\seqs elems -> (:) <$> elems <*> seqs) [[]] variousGraphs
    let variousGraphsUnited = map (sconcat . (Graph args empty :|)) variousGraphSequences 
    let variousGraphsWithConst = map (\curg -> foldl (\g ((v1, t1), (v2, t2)) -> addWeightCond g v1 v2 (t2 ~~ t1)) curg $ (,) <$> constants <*> constants) variousGraphsUnited
    let variousGraphsCleaned = map (\graph -> cleanGraph (supplement graph) baseArgs) variousGraphsWithConst 
    return $ foldl getNewDisjunctionOr disjEmpty $ map getConditionsFromGraph $ filter withoutPositiveCycle variousGraphsCleaned
    

goBody :: Ord a => AnnG AbstractTerm a -> Map String (AnnotatedDef (AnnG AbstractTerm) a) -> Map String (Conditions a) -> [String] -> [String] -> FreshNames -> Map a String -> Conditions String
goBody conj@(Conjunction g1 g2 lstG) mapDefs mapConditions baseArgs args fn mapVars = 
    fst $ runState (goConjunction conj mapDefs mapConditions baseArgs args mapVars) fn
goBody (Disjunction g1 g2 lstG) mapDefs mapConditions baseArgs args fn mapVars = 
    let mapped = map (\goal -> goBody goal mapDefs mapConditions baseArgs args fn mapVars) (g2 : lstG) in 
    let fstGoal = goBody g1 mapDefs mapConditions baseArgs args fn mapVars in 
    foldl getNewDisjunctionOr fstGoal mapped
goBody (Fresh x g) mapDefs mapConditions baseArgs args fn mapVars = 
    let (newVarInt, fnNew) = getFreshName fn in 
    let newVarStr = show newVarInt in
    let newMapVars = insert x newVarStr mapVars in 
    goBody g mapDefs mapConditions baseArgs (newVarStr : args) fnNew newMapVars 
goBody (Delay g) mapDefs mapConditions baseArgs args fn mapVars = 
    goBody g mapDefs mapConditions baseArgs args fn mapVars
goBody goal mapDefs mapConditions baseArgs args fn mapVars = 
    let graphs = fst $ fst $ runState (goOneConjunct goal mapDefs mapConditions args mapVars) fn in 
    foldl getNewDisjunctionOr disjEmpty $ map (\graph -> getConditionsFromGraph $ cleanGraph (supplement graph) baseArgs) $ filter withoutPositiveCycle graphs
