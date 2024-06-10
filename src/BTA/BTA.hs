{-# LANGUAGE TupleSections #-}

module BTA.BTA where


import           Control.Monad.State
import qualified Data.Map                  as Map
import qualified Data.Set                  as Set
import           BTA.AnnotationType
import           BTA.AnnotatedDef
import           BTA.NormalizeAnnotated
import           Syntax
import           BTA.Conditions
import           BTA.SizeConversion
import           BTA.Graph
import qualified BTA.AnnotatedProgram      as AnnPr
import qualified BTA.InvokeAnnotation      as INV
import qualified BTA.TerminationCheck      as Tch (getPairsDef, goGraphMap)
import qualified CPD.Residualization        as Res


toUnfoldGoal :: INV.AnnG Term String -> INV.AnnG Term String 
toUnfoldGoal (INV.Conjunction g1 g2 gl) = 
  INV.Conjunction (toUnfoldGoal g1) (toUnfoldGoal g2) (map toUnfoldGoal gl)
toUnfoldGoal (INV.Disjunction g1 g2 gl) = 
  INV.Disjunction (toUnfoldGoal g1) (toUnfoldGoal g2) (map toUnfoldGoal gl)
toUnfoldGoal (INV.Fresh x body) =
  INV.Fresh x $ toUnfoldGoal body
toUnfoldGoal (INV.Delay body) =
  INV.Delay $ toUnfoldGoal body
toUnfoldGoal (INV.Invoke name terms annots) =
  INV.Invoke name terms INV.Unfold
toUnfoldGoal (a INV.:=: b) = 
  a INV.:=: b

toUnfoldPr :: AnnPr.AnnotatedProgram (INV.AnnG Term) String  -> AnnPr.AnnotatedProgram (INV.AnnG Term) String 
toUnfoldPr program@(AnnPr.AnnotatedProgram df1 gl1) = 
    AnnPr.AnnotatedProgram (map (\(AnnotatedDef name args body modes) -> (AnnotatedDef name args (toUnfoldGoal body) modes)) df1) $ toUnfoldGoal gl1
 
improveModes :: AnnPr.AnnotatedProgram (INV.AnnG Term) String -> AnnPr.AnnotatedProgram (INV.AnnG Term) String
improveModes program@(AnnPr.AnnotatedProgram df1 gl1) = 

    let initNames = map getName df1 in 
    let abstractProgram@(AnnPr.AnnotatedProgram defs goal) = convert (toUnfoldPr program) in 
    let mapDefs = Map.fromList $ zip (map getName defs) defs in
    let mapConditions = Map.fromList $ map ((, conjEmpty) . getName) defs in 
    let (defsGraphs, defsVars) = unzip $ map (Tch.getPairsDef mapConditions mapDefs) defs in 
    let defsVarsRes = Map.unions defsVars in 
    let defsGraphsRes = Tch.goGraphMap (Map.unions defsGraphs) defsVarsRes $ map getName defs in 
    
    let defsCheck = map (\def@(AnnotatedDef name _ _ _) -> (def, Map.findWithDefault Set.empty (name, name) defsGraphsRes, defsVarsRes Map.! (name, name))) df1 in 
    
    let resModes = until (\x -> (findConsistent . updateModes) x == x) (findConsistent . updateModes) defsCheck in -- until (\x -> updateModes x == x) 

    AnnPr.AnnotatedProgram (map (\(a, b, c) -> a) resModes) gl1

updateModes :: [(AnnotatedDef (INV.AnnG Term) X, (Set.Set (Graph String)), ([String], [String]))] -> [(AnnotatedDef (INV.AnnG Term) X, (Set.Set (Graph String)), ([String], [String]))]
updateModes defsCheck = 
  map (until (\x -> updateMode x == x) updateMode) defsCheck

updateMode :: (AnnotatedDef (INV.AnnG Term) X, (Set.Set (Graph String)), ([String], [String])) -> (AnnotatedDef (INV.AnnG Term) X, (Set.Set (Graph String)), ([String], [String]))
updateMode (def@(AnnotatedDef name args body modes), graphs, (inputs, outputs)) = 
    (AnnotatedDef name args body (map (\(out, mode) -> checkMode out mode inputs outputs modes graphs) $ zip outputs modes), graphs, (inputs, outputs))

checkMode :: String -> AnnotationType -> [String] -> [String] -> [AnnotationType] -> Set.Set (Graph String) -> AnnotationType
checkMode out mode inputs outputs modes graphs | mode == Dynamic = Dynamic
checkMode out mode inputs outputs modes graphs | otherwise = 
    case all check (Set.toList graphs) of 
        True -> mode 
        False -> Dynamic
    where 
        check graph = 
            (any (\(i, o, m) -> m == Static && positivePath i o graph) $ zip3 inputs outputs modes) ||
            (any (\i -> atLeastZeroPath i out graph) inputs)




findConsistent :: [(AnnotatedDef (INV.AnnG Term) X, (Set.Set (Graph String)), ([String], [String]))] -> [(AnnotatedDef (INV.AnnG Term) X, (Set.Set (Graph String)), ([String], [String]))]
findConsistent defsCheck  = 
  let (defs, graphs, verts) = unzip3 defsCheck in 
  let mapMode = Map.fromList $ map (\(AnnotatedDef name args body modes) -> (name, modes)) defs in 
  let localUpdate = \mpMode -> foldl (\st def -> snd $ runState (updateConsistent def) st) mpMode defs in  
  let resMapMode = until (\x -> localUpdate x == x) localUpdate mapMode in 
  let resDefs = map (\(AnnotatedDef name args body modes) -> AnnotatedDef name args body (resMapMode Map.! name)) defs in 
  zip3 resDefs graphs verts

-- findConsistentState :: [AnnotatedDef (INV.AnnG Term) X] -> State (Map.Map String [AnnotationType]) [AnnotatedDef (INV.AnnG Term) X]
-- findConsistentState defs = do 
--   let localUpdate = foldl (\def -> updateConsistent defs) defs 
--   let resDefs = until (\x -> localUpdate x == x) localUpdate defs
--   return resDefs 

updateConsistent :: AnnotatedDef (INV.AnnG Term) X -> State (Map.Map String [AnnotationType]) ()
updateConsistent def@(AnnotatedDef name args body _) = do
  state <- get 
  let ground = Set.fromList [arg | (arg, mode) <- (zip args (state Map.! name)), mode == Static]
  newGround <- updateBody body ground
  return ()

updateBody :: INV.AnnG Term X -> Set.Set X -> State (Map.Map String [AnnotationType]) (Set.Set X)
updateBody (INV.Conjunction g1 g2 gl) ground = do
  foldM (\ground g -> updateBody g ground) ground (g1 : g2 : gl)
updateBody (INV.Disjunction g1 g2 gl) ground = do
  resS <- mapM (\g -> updateBody g ground) (g1 : g2 : gl)
  return ground
updateBody (INV.Fresh x body) ground = do
  updateBody body ground
updateBody (INV.Delay body) ground = do
  updateBody body ground
updateBody (INV.Invoke name terms annot) ground = do
  state <- get 
  let vars = map ((!!0) . Set.toList . Res.getVars) terms -- terms are actually variables
  let newAnnots = map (\(var, annot) -> decision (Set.member var ground) annot) $ zip vars $ state Map.! name
  put $ Map.insert name newAnnots state
  return $ Set.union ground (Set.fromList vars)
  where decision False annot = Dynamic
        decision True annot = annot 
updateBody (a INV.:=: b) ground | any ((all (\x -> Set.member x ground)) . Res.getVars) [a, b] = do
  return $ Set.unions [ground, Res.getVars a, Res.getVars b]  
updateBody (a INV.:=: b) ground = do
  return ground
