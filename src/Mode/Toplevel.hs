module Mode.Toplevel where

import           Control.Monad.State
import           Data.List           (find)
import qualified Data.Map            as Map
import qualified Data.Set            as Set
import           Def
import           Mode.Analysis
import           Mode.Inst
import qualified Mode.NormSyntax     as N
import           Mode.Syntax
import           Mode.Term
import           Program
import qualified Syntax              as S
import           VarRename

-- analyze :: (Show a, Ord a) => Goal (a, Mode) -> StateT (AnalyzeState a) Maybe (Goal (a, Mode))

runAnalyze :: (Show a, Ord a) => Goal a -> [a] -> Maybe (Goal (a, Mode))
runAnalyze goal ins =
  let goal' = initMode goal (Map.fromList $ zip ins $ repeat Ground) in
  evalStateT (analyze goal') emptyAnalyzeState

makeDefMap :: [Def g a] -> Map.Map String (Def g a)
makeDefMap defs = Map.fromList $ map (\d -> (getName d, d)) defs

initModeForVar :: (Foldable t, Eq a) => t (Var a) -> Var a -> (a, Mode)
initModeForVar inputArgs (Var x) =
    (x, Mode before after)
  where
    before = if Var x `elem` inputArgs then Ground else Free
    after = Just Ground

topLevel :: Program S.G S.X -> [Int] -> Maybe (Program Goal (S.S, Mode))
topLevel program ins = do
    (program, nextVar) <- uniquelyRenameVars program
    let flat@(Program defs goal) = N.back $ N.normalize $ flatten program nextVar
    case goal of
      Call name args -> do
        let inputArgs = map (args !!) ins
        let goal' = initMode goal (Map.fromList $ zip ins $ repeat Ground)
        let initMode = map (initModeForVar inputArgs) args
        let topMode = (name, initMode)
        let initState = emptyAnalyzeState
                          { getAllModdedDefs = Map.fromList [(name, [initMode])]
                          , getQueue = Set.fromList [topMode]
                          , getDefinitions = makeDefMap defs
                          }
        (modded, state) <- runStateT (analyze goal') initState
        defs <- evalStateT analyzeNewDefs state
        return $ Program defs modded
      _ -> Nothing

topLevelWithDefaultCall :: Program S.G S.X -> String -> [Int] -> Maybe (Program Goal (S.S, Mode))
topLevelWithDefaultCall program relName ins = do
  program <- replaceGoalWithDefaultCall program relName
  topLevel program ins

replaceGoalWithDefaultCall :: Program S.G S.X -> String -> Maybe (Program S.G S.X)
replaceGoalWithDefaultCall program relName = do
    Program (getDefs program) <$> defaultCall
  where
    defaultCall = do
      vars <- defaultArgs
      return $ S.fresh vars $ S.Invoke relName $ map S.V vars
    defaultArgs =
      getArgs <$> find (\(Def name _ _) -> name == relName) (getDefs program)
