module OfflinePD.AnnEnvironment where
  
import           BTA.AnnotatedDef
import           BTA.InvokeAnnotation (AnnG)
import           Syntax
import qualified BTA.AnnotatedDefs as AnnDefs
import qualified VarInterpretation as VI
import qualified FreshNames as FN

data Env = Env { getDefs       :: AnnDefs.AnnDefinitions
               , getInterp     :: VI.Interpretation
               , getFreshNames :: FN.FreshNames
               }
          -- deriving (Show, Ord, Eq)
          deriving (Ord, Eq)

instance Show Env where
     show = const "env"

empty :: Env
empty = Env AnnDefs.empty VI.empty FN.defaultNames

updateDef :: Env -> AnnotatedDef (AnnG Term) X -> Env
updateDef (Env p i d) def@(AnnotatedDef name _ _ _) = Env (AnnDefs.insert name def p) i d

updateDefs :: Env -> [AnnotatedDef (AnnG Term) X] -> Env
updateDefs = foldl updateDef

fromDefs :: [AnnotatedDef (AnnG Term) X] -> Env
fromDefs = updateDefs empty

updateNames :: Env -> FN.FreshNames -> Env
updateNames (Env p i _) = Env p i

updateInterp :: Env -> VI.Interpretation -> Env
updateInterp (Env p _ d) i = Env p i d

getDef :: Env -> Name -> AnnotatedDef (AnnG Term) X
getDef (Env p _ _) = AnnDefs.getDef p
