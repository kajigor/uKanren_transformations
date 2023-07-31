{-# LANGUAGE TupleSections #-}

module BTA.Inequalities where 

import Prelude hiding (lookup, null)
import BTA.SizeConversion
import AnnotatedProgram
import Data.Map hiding (map, foldl, difference, filter)
import BTA.Conditions
import FreshNames
import Data.Maybe
import AnnotatedDef
import qualified InvokeAnnotation as Inv
import BTA.Graph
import Debug.Trace
import BTA.Condition
import Control.Monad.State



go :: Show a => Ord a => [AnnotatedDef AbstractG a] -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> Map (String) (Conditions a)
go defs mapDefs mapConditions = 
    until (\x -> goOneCycle defs mapDefs x == x) (goOneCycle defs mapDefs) mapConditions


goOneCycle :: Show a => Ord a => [AnnotatedDef AbstractG a] -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> Map (String) (Conditions a)
goOneCycle [] mapDefs mapConditions = mapConditions
goOneCycle (def : otherDefs) mapDefs mapConditions = 
    goOneCycle otherDefs mapDefs (goOneDef def mapDefs $ mapConditions)


goOneDef :: Show a => Ord a => (AnnotatedDef AbstractG a) ->  Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> Map (String) (Conditions a)
goOneDef def@(AnnotatedDef name args body annotations) mapDefs mapConditions = 
    let (namesInt, fn) = getNames (length args) $ defaultNames :: ([Int], FreshNames) in 
    let namesStr = map show (namesInt) :: [String] in 
    let mapVars = fromList $ zip args namesStr in 
    let condsString = (goBody body mapDefs mapConditions namesStr namesStr fn (mapVars)) in 
    let mapInverse = fromList . map (\(x, y) -> (y, x)) . toList $ mapVars in 
    let condsA = (fmap (\x -> fromJust $ lookup x mapInverse) condsString) in 
    let resConds = (getNewDisjunctionOr condsA (fromMaybe (ConditionDisj []) (lookup name mapConditions))) in 
    insert name resConds mapConditions


getCond :: Ord a => AbstractTerm a -> AbstractTerm a -> a -> a -> Maybe (Condition a)
getCond t1 t2 x y | x Prelude.== y = Nothing
getCond t1 t2 x y | isLess t1 t2 = Just (Lt y x)
getCond t1 t2 x y | isLess t2 t1 = Just (Lt x y)
getCond t1 t2 x y | t1 Prelude.== t2 = Just (Eq x y)
getCond _ _ _ _ = Nothing 


getCondsFromEq :: Ord a => AbstractTerm a -> AbstractTerm a -> [Conditions a]
getCondsFromEq term1@(Sum n1 mp1) term2@(Sum n2 mp2) = 
    let vars1 = keys mp1 in 
    let vars2 = keys mp2 in
    let varsPairs = filter (\(x, y) -> (checkcondiction (difference term1 (Sum 0 (fromList [(x, 1)]))) (difference term2 (Sum 0 (fromList [(y, 1)]))))) [(v1, v2) | v1 <- vars1, v2 <- vars2] in 
    let conds = map (\(x, y) -> getCond (difference term1 (Sum 0 (fromList [(x, 1)]))) (difference term2 (Sum 0 (fromList [(y, 1)]))) x y) varsPairs in 
    map (\x -> Condition x) $ catMaybes conds
    where checkcondiction term1 term2 = (isLess term1 term2) || (term1 Prelude.== term2) || (isLess term2 term1)


getCondsOneTerm :: Ord a => (a, AbstractTerm a) -> [(a, a, AbstractTerm a)]
getCondsOneTerm (var, term@(Sum n mp)) = 
    filter (\(var, otherVar, term) -> (fromMaybe 0 $ lookup var mp) Prelude.== 0) [(var, otherVar, difference term (Sum 0 (fromList [(otherVar, 1)]))) | otherVar <- (keys mp)]


getCondsTermToTerm :: Ord a => AbstractTerm a -> AbstractTerm a -> [(a, a, AbstractTerm a)]
getCondsTermToTerm term1@(Sum n1 mp1) term2@(Sum n2 mp2) = 
    concatMap (getCondsOneTerm . (\var -> (var, (Sum 0 (fromList [(var, 1)])) <> (difference term2 term1)))) $ (keys mp1 ++ keys mp2)


handleOneInvokeConjunct :: [AbstractTerm String] -> [String] -> [String] -> Conditions String -> Graph String 
handleOneInvokeConjunct terms newVars oldVars conditions = 
    let initGraph = getFromConjunction conditions (oldVars ++ newVars) in 
    let conds = concatMap getCondsOneTerm (zip newVars terms) in 
    let resGraph =  supplement $ foldl (\graph cond@(a, b, term) -> addWeightCond graph a b term) initGraph conds in
    resGraph 


handleInvokeUnfold :: Show a => Ord a => (AbstractG a) -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> [String] -> Map a String -> State FreshNames [Graph String] 
handleInvokeUnfold term@(Invoke name terms ann) mapDefs mapConditions args mapVars | ann Prelude.== Inv.Unfold = do
    fn <- get 
    let condsFun = (fromMaybe (ConditionDisj []) (lookup name mapConditions))
    let termsNew = map (termfmap (\x -> fromJust (lookup x mapVars))) terms
    let (newArgsInt, fnNew) = getNames (length terms) fn 
    put fnNew
    let newArgsStr = map show newArgsInt 
    let mapArgs = fromList $ zip (getArgs . fromJust $ lookup name mapDefs) newArgsStr ---WHAT
    let newConds@(ConditionDisj disjuncts) = fmap (\x -> fromJust $ lookup x mapArgs) condsFun 
    return $ filter withoutPositiveCycle $ map (handleOneInvokeConjunct termsNew newArgsStr args) disjuncts
handleInvokeUnfold _ _ _ args _ = do 
    return [Graph args empty]

                                        
goOneConjunct :: Show a => Ord a => (AbstractG a) -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> [String] -> Map a String -> State FreshNames ([Graph String], [(String, Int)])
goOneConjunct invoke@(Invoke name terms ann) mapDefs mapConditions args mapVars = do
    graphs <- handleInvokeUnfold invoke mapDefs mapConditions args mapVars
    return (graphs, [])
goOneConjunct (term1 :=: term2) mapDefs mapConditions args mapVars = do 
    let term1New@(Sum n1 mp1) = termfmap (\x -> fromMaybe "" (lookup x mapVars)) term1
    let term2New = termfmap (\x -> fromMaybe "" (lookup x mapVars)) term2
    let constants = map (\(var, Sum n mp) -> (var, n)) $ filter (\(var, Sum n mp) -> null mp) [(var, ((Sum 0 (fromList [(var, 1)])) <> (difference term2New term1New))) | var <- args]
    let conds = getCondsTermToTerm (term1New) (term2New)
    let smth = concatMap (getCondsOneTerm . (\var -> (var, (Sum 0 (fromList [(var, 1)])) <> (difference term2New term1New)))) $ keys mp1 
    return (filter withoutPositiveCycle [supplement $ foldl (\graph cond@(a, b, term) -> addWeightCond graph a b term) (Graph args empty) smth], constants)


goConjunction :: Show a => Ord a => (AbstractG a) -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> [String] -> [String] -> Map a String -> State FreshNames (Conditions String)
goConjunction (Conjunction g1 g2 lstG) mapDefs mapConditions baseArgs args mapVars = do 
    fn <- get 
    let graph = Graph args empty
    resS <- mapM (\goal -> (goOneConjunct goal mapDefs mapConditions args mapVars)) (g1 : g2 : lstG)
    let (variousGraphs, constants1) = unzip resS
    let constants = concat constants1
    let variousGraphSequences = foldl (\seqs elems -> [(elem : seq) | seq <- seqs, elem <- elems]) [[]] (variousGraphs)
    let variousGraphsUnited = map (\seq -> foldl unionGraphs (Graph args empty) seq) (variousGraphSequences) 
    let variousGraphsWithConst = map (\curg -> foldl (\g ((v1 ,n1), (v2, n2)) -> addWeightCond g v1 v2 (Sum (n2 - n1) empty)) curg [(v1, v2) | v1 <- constants, v2 <- constants]) variousGraphsUnited
    let variousGraphsCleaned = map (\graph -> cleanGraph (supplement graph) baseArgs) variousGraphsWithConst 
    return $ foldl getNewDisjunctionOr (ConditionDisj []) $ map getConditionsFromGraph $ filter withoutPositiveCycle variousGraphsCleaned
    

goBody :: Show a => Ord a => (AbstractG a) -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> [String] -> [String] -> FreshNames -> Map a String -> (Conditions String) 
goBody conj@(Conjunction g1 g2 lstG) mapDefs mapConditions baseArgs args fn mapVars = 
    fst $ runState (goConjunction conj mapDefs mapConditions baseArgs args mapVars) fn
goBody (Disjunction g1 g2 lstG) mapDefs mapConditions baseArgs args fn mapVars = 
    let mapped = (map (\goal -> goBody goal mapDefs mapConditions baseArgs args fn mapVars) (g2 : lstG)) in 
    let fstGoal = goBody g1 mapDefs mapConditions baseArgs args fn mapVars in 
    foldl getNewDisjunctionOr fstGoal mapped
goBody (Fresh x g) mapDefs mapConditions baseArgs args fn mapVars = 
    let (newVarInt, fnNew) = getFreshName fn in 
    let newVarStr = show newVarInt in
    let newMapVars = insert x newVarStr mapVars in 
    goBody g mapDefs mapConditions baseArgs (newVarStr : args) fnNew newMapVars --TODO
goBody (Delay g) mapDefs mapConditions baseArgs args fn mapVars = 
    goBody g mapDefs mapConditions baseArgs args fn mapVars
goBody goal@(term1 :=: term2) mapDefs mapConditions baseArgs args fn mapVars = 
    let graphs = fst $ fst $ runState (goOneConjunct goal mapDefs mapConditions args mapVars) fn in 
    foldl getNewDisjunctionOr (ConditionDisj []) $ map (\graph -> getConditionsFromGraph $ cleanGraph (supplement graph) baseArgs) $ filter withoutPositiveCycle graphs
goBody goal@(Invoke name terms ann) mapDefs mapConditions baseArgs args fn mapVars = 
    let graphs = fst $ fst $ runState (goOneConjunct goal mapDefs mapConditions args mapVars) fn in 
    foldl getNewDisjunctionOr (ConditionDisj []) $ map (\graph -> getConditionsFromGraph $ cleanGraph (supplement graph) baseArgs) $ filter withoutPositiveCycle graphs
    
