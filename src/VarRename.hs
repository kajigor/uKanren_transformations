module VarRename where

import           Control.Monad.State
import qualified Data.Map            as M
import           Def
import           FreshNames
import           Program
import           Syntax
import Debug.Trace

data RenameState a = RenameState
  { getVarMap       :: M.Map X a
  , getNameSource   :: PolyFreshNames a
  , getFirstNotUsed :: PolyFreshNames a
  } deriving (Show)

emptyState :: FreshName a => RenameState a
emptyState = RenameState { getVarMap = M.empty, getNameSource = defaultNames, getFirstNotUsed = defaultNames }

type VarRenameError = String

uniquelyRenameVars :: Program G X -> Either VarRenameError (Program G S, S)
uniquelyRenameVars (Program defs goal) = do
  (goal, gState) <- runStateT (enumerate goal) emptyState
  defs <- mapM (\d -> runStateT (enumerateDef d) emptyState) defs
  let highestNextVar = maximum (getFirstNotUsed gState : map (getFirstNotUsed . snd) defs)
  return (Program (map fst defs) goal, fst $ getFreshName highestNextVar)

uniquelyRenameVarsInDefs :: (Show a, FreshName a, Ord a)
                         => [Def G X]
                         -> Either VarRenameError ([Def G a], a)
uniquelyRenameVarsInDefs defs = do
  defs <- mapM (\d -> runStateT (enumerateDef d) emptyState) defs
  let highestNextVar = maximum (map (getFirstNotUsed . snd) defs)
  return (map fst defs, fst $ getFreshName highestNextVar)

enumerateDef ::  (Ord a, Show a, FreshName a) => Def G X -> StateT (RenameState a) (Either VarRenameError) (Def G a)
enumerateDef (Def name args body) = do
  args <- mapM newVar args
  body <- enumerate body
  return $ Def name args body

enumerate :: (Ord a, Show a, FreshName a) => G X -> StateT (RenameState a) (Either VarRenameError) (G a)
enumerate (x :=: y) = do
  x <- enumerateTerm x
  y <- enumerateTerm y
  return $ x :=: y
enumerate (Conjunction x y xs) = do
  x <- enumerate x
  y <- enumerate y
  xs <- mapM enumerate xs
  return $ Conjunction x y xs
enumerate (Disjunction x y xs) = do
  x <- enumerate x
  y <- enumerate y
  xs <- mapM enumerate xs
  return $ Disjunction x y xs
enumerate (Invoke name args) = do
  args <- mapM enumerateTerm args
  return $ Invoke name args
enumerate (Fresh x g) = do
  (g, v') <- withNewName x (enumerate g)
  return (Fresh v' g)
enumerate (Delay g) = do
  Delay <$> enumerate g

withNewName :: (Ord a, FreshName a, Show a, Monad m) =>
               X ->
               StateT (RenameState a) m b ->
               StateT (RenameState a) m (b, a)
withNewName x f = do
  oldState@(RenameState varMap nameSource firstNotUsed) <- get
  let (newName, newSource) = getFreshName nameSource
  let newVarMap = M.insert x newName varMap
  put (RenameState newVarMap newSource (max newSource firstNotUsed))
  r <- f
  subUsed <- gets getFirstNotUsed
  put $ oldState { getFirstNotUsed = max newSource subUsed}
  return (r, newName)

newVar :: (Ord a, FreshName a, Monad m) => X -> StateT (RenameState a) m a
newVar x = do
  oldState@(RenameState varMap nameSource firstNotUsed) <- get
  let (newName, newSource) = getFreshName nameSource
  let newVarMap = M.insert x newName varMap
  put (RenameState newVarMap newSource (max newSource firstNotUsed))
  return newName

enumerateVar :: (Show a, FreshName a) => X -> StateT (RenameState a) (Either VarRenameError) a
enumerateVar v = do
  varMap <- gets getVarMap
  case M.lookup v varMap of
    Just r -> return r
    Nothing -> lift $ Left $ v ++ " undefined"

enumerateTerm :: (Show a, FreshName a) => Term X -> StateT (RenameState a) (Either VarRenameError) (Term a)
enumerateTerm (V v) = do
  v <- enumerateVar v
  return (V v)
enumerateTerm (C name args) = do
  args <- mapM enumerateTerm args
  return $ C name args



