{-# LANGUAGE TupleSections #-}

module BTA.Inequalities where 

import Prelude hiding (lookup)
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
    let newMapConditions = goOneCycle defs mapDefs mapConditions in
    case (newMapConditions Prelude.== mapConditions) of 
        True -> newMapConditions
        False -> go defs mapDefs newMapConditions


goOneCycle :: Show a => Ord a => [AnnotatedDef AbstractG a] -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> Map (String) (Conditions a)
goOneCycle [] mapDefs mapConditions = trace "mapConditions" mapConditions
goOneCycle (def : otherDefs) mapDefs mapConditions = 
    goOneCycle otherDefs mapDefs (goOneDef def mapDefs $ trace "Here" mapConditions)


goOneDef :: Show a => Ord a => (AnnotatedDef AbstractG a) ->  Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> Map (String) (Conditions a)
goOneDef def@(AnnotatedDef name args body annotations) mapDefs mapConditions = 
    let (namesInt, fn) = getNames (length args) $ trace "defaultNames" defaultNames :: ([Int], FreshNames) in 
    let namesStr = map show (trace "namesInt" namesInt) :: [String] in 
    let mapVars = fromList $ zip args namesStr in 
    let condsString = (goBody body mapDefs mapConditions namesStr namesStr fn (trace "mapVars" mapVars)) in 
    let mapInverse = fromList . map (\(x, y) -> (y, x)) . toList $ mapVars in 
    let condsA = (fmap (\x -> fromJust $ lookup x mapInverse) condsString) in 
    insert name (getNewDisjunctionOr (trace "condsA" condsA) (fromMaybe (ConditionDisj []) (lookup name mapConditions))) mapConditions


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
    let resGraph =  supplement $ foldl (\graph cond@(a, b, term) -> trace (show cond) $ addWeightCond graph a b term) initGraph conds in
    resGraph 
    -- foldl removeVertex resGraph newVars


handleInvokeUnfold :: Ord a => (AbstractG a) -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> [String] -> Map a String -> State FreshNames [Graph String] 
handleInvokeUnfold term@(Invoke name terms ann) mapDefs mapConditions args mapVars | ann Prelude.== Inv.Unfold = do
    fn <- get 
    let condsFun = (fromMaybe (ConditionDisj []) (lookup name mapConditions))
    let termsNew = map (termfmap (\x -> fromJust (lookup x mapVars))) terms 
    let (newArgsInt, fnNew) = getNames (length terms) fn 
    put fnNew
    let newArgsStr = map show newArgsInt 
    let mapArgs = fromList $ zip (getArgs . fromJust $ lookup name mapDefs) newArgsStr ---WHAT
    let newConds@(ConditionDisj disjuncts) = fmap (\x -> fromJust $ lookup x mapArgs) condsFun 
    return $ map (handleOneInvokeConjunct termsNew newArgsStr args) disjuncts
handleInvokeUnfold _ _ _ args _ = do 
    return [Graph args empty]

                                        
goOneConjunct :: Ord a => (AbstractG a) -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> [String] -> Map a String -> State FreshNames [Graph String]
goOneConjunct invoke@(Invoke name terms ann) mapDefs mapConditions args mapVars = 
    handleInvokeUnfold invoke mapDefs mapConditions args mapVars 
-- goOneConjunct delay@(Delay g) mapDefs mapConditions args fn mapVars  = 
--     goOneConjunct g mapDefs mapConditions args fn mapVars 
goOneConjunct (term1 :=: term2) mapDefs mapConditions args mapVars = do 
    let term1New@(Sum n1 mp1) = termfmap (\x -> fromMaybe "" (lookup x mapVars)) term1
    let term2New = termfmap (\x -> fromMaybe "" (lookup x mapVars)) term2
    let conds = getCondsTermToTerm (trace (show term1New) term1New) (trace (show term2New) term2New)
    let smth = concatMap (getCondsOneTerm . (\var -> trace (show (var, (Sum 0 (fromList [(var, 1)])) <> (difference term2New term1New))) $ (var, (Sum 0 (fromList [(var, 1)])) <> (difference term2New term1New)))) $ keys mp1 
    return [supplement $ foldl (\graph cond@(a, b, term) -> trace (show cond) $ addWeightCond graph a b term) (Graph args empty) smth]


goConjunction :: Ord a => (AbstractG a) -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> [String] -> [String] -> Map a String -> State FreshNames (Conditions String)
goConjunction (Conjunction g1 g2 lstG) mapDefs mapConditions baseArgs args mapVars = do 
    fn <- get 
    let graph = Graph args empty
    variousGraphs <- mapM (\goal -> (goOneConjunct goal mapDefs mapConditions args mapVars)) (g1 : g2 : lstG)
    let variousGraphSequences = foldl (\seqs elems -> [(elem : seq) | seq <- seqs, elem <- elems]) [[]] (trace ("Graphs " ++ show variousGraphs) variousGraphs)
    let veriousGraphsUnited = map (\seq -> foldl unionGraphs (Graph args empty) seq) (trace (show variousGraphSequences) variousGraphSequences) 
    let variousGraphsCleaned = map (\graph -> cleanGraph (supplement graph) baseArgs) veriousGraphsUnited 
    return $ foldl getNewDisjunctionOr (ConditionDisj []) (map getConditionsFromGraph (trace (show variousGraphsCleaned) variousGraphsCleaned))
    

goBody :: Ord a => (AbstractG a) -> Map (String) (AnnotatedDef AbstractG a) -> Map (String) (Conditions a) -> [String] -> [String] -> FreshNames -> Map a String -> (Conditions String) 
goBody conj@(Conjunction g1 g2 lstG) mapDefs mapConditions baseArgs args fn mapVars = 
    fst $ runState (goConjunction conj mapDefs mapConditions baseArgs args mapVars) fn
    -- let graph = Graph args empty in 
    -- let variousGraphs = fst $ foldl (\(lst, fnCur) goal -> curList : $ runState (goOneConjunct goal mapDefs mapConditions args mapVars) fnCur) [(, fn)] (g1 : g2 : lstG) in 
    -- let variousGraphSequences = foldl (\seqs elems -> [(elem : seq) | seq <- seqs, elem <- elems]) [[]] (trace ("Graphs " ++ show variousGraphs) variousGraphs) in
    -- let veriousGraphsUnited = map (\seq -> foldl unionGraphs (Graph args empty) seq) (trace (show variousGraphSequences) variousGraphSequences) in 
    -- let variousGraphsCleaned = map (\graph -> cleanGraph (supplement graph) args) veriousGraphsUnited in 
    -- foldl getNewDisjunctionOr (ConditionDisj []) (map getConditionsFromGraph (trace (show variousGraphsCleaned) variousGraphsCleaned))
    -- foldl getNewDisjunctionAnd (goBody g1 mapDefs mapConditions args fn mapVars) (map (\goal -> goBody goal mapDefs mapConditions args fn mapVars) (g2 : lstG))
goBody (Disjunction g1 g2 lstG) mapDefs mapConditions baseArgs args fn mapVars = 
    foldl getNewDisjunctionOr (goBody g1 mapDefs mapConditions baseArgs args fn mapVars) (map (\goal -> goBody goal mapDefs mapConditions baseArgs args fn mapVars) (g2 : lstG))
goBody (Fresh x g) mapDefs mapConditions baseArgs args fn mapVars = 
    let (newVarInt, fnNew) = getFreshName fn in 
    let newVarStr = show newVarInt in
    let newMapVars = insert x newVarStr mapVars in 
    goBody g mapDefs mapConditions baseArgs (newVarStr : args) fnNew newMapVars --TODO
goBody (Delay g) mapDefs mapConditions baseArgs args fn mapVars = 
    goBody g mapDefs mapConditions baseArgs args fn mapVars
goBody goal@(term1 :=: term2) mapDefs mapConditions baseArgs args fn mapVars = 
    let graphs = fst $ runState (goOneConjunct goal mapDefs mapConditions args mapVars) fn in 
    ConditionDisj $ map (\graph -> getConditionsFromGraph $ cleanGraph (supplement graph) baseArgs) graphs
goBody goal@(Invoke name terms ann) mapDefs mapConditions baseArgs args fn mapVars = 
    let graphs = fst $ runState (goOneConjunct goal mapDefs mapConditions args mapVars) fn in 
    foldl getNewDisjunctionOr (ConditionDisj []) (map (\graph -> getConditionsFromGraph $ cleanGraph (supplement graph) baseArgs) graphs)
    
