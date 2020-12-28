-- miniKanren environment (previously gamma)
module Environment where

import qualified Definitions as Defs
import qualified VarInterpretation as VI
import qualified FreshNames as FN
import Syntax

data Env = Env { getDefs :: Defs.Definitions
               , getInterp :: VI.Interpretation
               , getFreshNames :: FN.FreshNames
               }
          deriving (Show, Ord, Eq)


empty :: Env
empty = Env Defs.empty VI.empty FN.defaultNames

updateDef :: Env -> Def -> Env
updateDef (Env p i d) def@(Def name _ _) = Env (Defs.insert name def p) i d

updateDefs :: Env -> [Def] -> Env
updateDefs = foldl updateDef

fromDefs :: [Def] -> Env
fromDefs = updateDefs empty

updateNames :: Env -> FN.FreshNames -> Env
updateNames (Env p i _) = Env p i

updateInterp :: Env -> VI.Interpretation -> Env
updateInterp (Env p _ d) i = Env p i d

getDef :: Env -> Name -> Def
getDef (Env p _ _) n = Defs.getDef p n