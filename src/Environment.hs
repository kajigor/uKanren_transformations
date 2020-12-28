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


empty :: Env
empty = Env Defs.empty VI.empty FN.defaultNames

update :: Env -> Def -> Env
update (Env p i d) def@(Def name _ _) = Env (Defs.insert name def p) i d

updateDefs :: Env -> [Def] -> Env
updateDefs = foldl update

fromDefs :: [Def] -> Env
fromDefs = updateDefs empty

updateNames :: Env -> FN.FreshNames -> Env
updateNames (Env p i _) = Env p i