module Mode.Toplevel where

import           Control.Monad.State
import           Data.List           (find)
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Def
import           FreshNames
import           Mode.Analysis
import           Mode.Inst
import qualified Mode.NormSyntax     as N
import           Mode.Syntax
import           Mode.Term
import           Program
import qualified Syntax              as S
import           VarRename
import Debug.Trace
import Text.Printf
-- analyze :: (Show a, Ord a) => Goal (a, Mode) -> StateT (AnalyzeState a) Maybe (Goal (a, Mode))

runAnalyze :: (Show a, Ord a) => AllowFree -> N.Goal a -> [a] -> Either ModeAnalysisError (N.Goal (a, Mode))
runAnalyze allowFree goal ins =
  let goal' = initMode goal (Map.fromList $ zip ins $ repeat Ground) in
  evalStateT (analyze allowFree goal') (emptyAnalyzeState Nothing)

makeDefMap :: [Def g a] -> Map.Map String (Def g a)
makeDefMap defs = Map.fromList $ map (\d -> (getName d, d)) defs

initModeForVar :: (Foldable t, Eq a) => t (Var a) -> Var a -> (a, Mode)
initModeForVar inputArgs (Var x) =
    (x, Mode before after)
  where
    before = if Var x `elem` inputArgs then Ground else Free
    after = Just Ground

prepareDefs :: (Show a, Ord a, FreshName a) => [Def S.G S.X] -> Either VarRenameError [Def N.Disj a]
prepareDefs defs = do
  (defs, nextVar) <- uniquelyRenameVarsInDefs defs
  let flatten = flattenDefs defs nextVar
  let normalized = N.normalizeDefs flatten
  return $ trace (printf "In normalization:\nFlatten\n%s\nNormalized\n%s\n" (show flatten) (show normalized)) $ normalized

topLevelManyModes :: [Def S.G S.X] -> [(String, [Int])] -> Either ModeAnalysisError [Def N.Goal (S.S, Mode)]
topLevelManyModes defs modes = do
  defs <- prepareDefs defs
  let defMap = makeDefMap defs
  let state = (emptyAnalyzeState Nothing) { getDefinitions = defMap }
  evalStateT (go defMap) state
  where
    go defMap = do
      mapM_ (\(name, ins) -> enqueueModded name (prepareArgs defMap name ins)) modes
      analyzeNewDefs

    prepareArgs defMap name ins =
      case Map.lookup name defMap of
        Just (Def name args _) -> initMode args (Map.fromList $ zip ins $ repeat Ground)
        Nothing -> fail ""


topLevel :: Program S.G S.X -> [Int] -> Either ModeAnalysisError (Program N.Goal (S.S, Mode))
topLevel program ins = do
    (program, nextVar) <- uniquelyRenameVars program
    let flattened = flatten program nextVar
    let normalized = N.normalize flattened
    let flat@(Program defs goal) = trace (printf "In normalization:\n\nInitialProgram: %s\n\nFlatten\n%s\n\nNormalized\n%s\n" (show program) (show flattened) (show normalized)) $ normalized
    case goal of
      N.Disj (N.Conj (N.Call delayed name args :| []) :| []) -> do
        let inputArgs = map (args !!) ins
        let goal' = initMode goal (Map.fromList $ zip ins $ repeat Ground)
        let initMode = map (initModeForVar inputArgs) args
        let topMode = (name, initMode)
        let curRel = N.Call N.Delayed name (map Var initMode)
        let initState = (emptyAnalyzeState (Just $ curRel))
                          { getAllModdedDefs = Map.fromList [(name, [initMode])]
                          , getQueue = Set.fromList [topMode]
                          , getDefinitions = makeDefMap defs
                          }
        (modded, state) <- runStateT (prioritizeGround goal') initState
        defs <- evalStateT analyzeNewDefs state
        return $ Program defs modded
      _ -> Left $ "Toplevel goal is not a call: " ++ show goal

topLevelWithDefaultCall :: Program S.G S.X -> String -> [Int] -> Either ModeAnalysisError (Program N.Goal (S.S, Mode))
topLevelWithDefaultCall program relName ins = do
  program <- replaceGoalWithDefaultCall program relName
  topLevel program ins

replaceGoalWithDefaultCall :: Program S.G S.X -> String -> Either ModeAnalysisError (Program S.G S.X)
replaceGoalWithDefaultCall program relName = do
    Program (getDefs program) <$> defaultCall
  where
    defaultCall = do
      vars <- defaultArgs
      return $ S.fresh vars $ S.Invoke relName $ map S.V vars
    defaultArgs =
      case find (\(Def name _ _) -> name == relName) (getDefs program) of
        Just def -> return $ getArgs def
        Nothing -> Left $ relName ++ " undefined"
