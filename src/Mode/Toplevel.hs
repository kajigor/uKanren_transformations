module Mode.Toplevel where

import Mode.Syntax
import Mode.Inst
import Mode.Analysis
import Control.Monad.State
import qualified Syntax as S
import qualified Data.Map as Map
import qualified Data.Set as Set
import VarRename
import Debug.Trace
import Mode.Term
import Program
import Def
import qualified Mode.NormSyntax as N
import Text.Printf

-- analyze :: (Show a, Ord a) => Goal (a, Mode) -> StateT (AnalyzeState a) Maybe (Goal (a, Mode))

runAnalyze :: (Show a, Ord a) => Goal a -> [a] -> Maybe (Goal (a, Mode))
runAnalyze goal ins =
  let goal' = initMode goal (Map.fromList $ zip ins $ repeat Ground) in
  evalStateT (analyze goal') emptyAnalyzeState

makeDefMap defs = Map.fromList $ map (\d -> (getName d, d)) defs

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