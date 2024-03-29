-- miniKanren environment (previously gamma)
module Environment where

import           Def
import qualified Definitions       as Defs
import qualified FreshNames        as FN
import           Syntax
import qualified VarInterpretation as VI

data Env = Env { getDefs       :: Defs.Definitions
               , getInterp     :: VI.Interpretation
               , getFreshNames :: FN.FreshNames
               }
          -- deriving (Show, Ord, Eq)
          deriving (Ord, Eq)

instance Show Env where
     show = const "env"

empty :: Env
empty = Env Defs.empty VI.empty FN.defaultNames

updateDef :: Env -> Def G X -> Env
updateDef (Env p i d) def@(Def name _ _) = Env (Defs.insert name def p) i d

updateDefs :: Env -> [Def G X] -> Env
updateDefs = foldl updateDef

fromDefs :: [Def G X] -> Env
fromDefs = updateDefs empty

updateNames :: Env -> FN.FreshNames -> Env
updateNames (Env p i _) = Env p i

updateInterp :: Env -> VI.Interpretation -> Env
updateInterp (Env p _ d) i = Env p i d

getDef :: Env -> Name -> Def G X
getDef (Env p _ _) = Defs.getDef p
