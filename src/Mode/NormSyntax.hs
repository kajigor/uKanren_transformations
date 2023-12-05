{-# LANGUAGE DeriveFunctor #-}
module Mode.NormSyntax where

import           Control.Monad.State
import           Data.List.NonEmpty  (NonEmpty (..))
import qualified Data.Set            as Set
import           Def
import qualified Mode.Syntax         as S
import           Mode.Term
import           Program

data Base a = Unif (Var a) (FlatTerm a)
            | Call String [Var a]
            deriving (Show, Eq, Functor)

newtype Conj a = Conj (NonEmpty (Base a))
               deriving (Show, Eq, Functor)

newtype Disj a = Disj (NonEmpty (Conj a))
               deriving (Show, Eq, Functor)

walkConj :: (Base a -> b) -> Conj a -> [b]
walkConj f (Conj (x :| xs)) = f <$> (x:xs)

walkConjM :: (Monad m) => (Base a -> m b) -> Conj a -> m [b]
walkConjM f (Conj (x :| xs)) = f `mapM` (x:xs)

walkConjM_ :: (Monad m) => (Base a -> m b) -> Conj a -> m ()
walkConjM_ f (Conj (x :| xs)) = f `mapM_` (x:xs)

walkDisj :: (Base a -> b) -> Disj a -> [b]
walkDisj f (Disj (x :| xs)) = (walkConj f) `concatMap` (x:xs)

walkDisjM :: (Monad m) => (Base a -> m b) -> Disj a -> m [b]
walkDisjM f (Disj (x :| xs)) = concat <$> (walkConjM f) `mapM` (x:xs)

walkDisjM_ :: (Monad m) => (Base a -> m b) -> Disj a -> m ()
walkDisjM_ f (Disj (x :| xs)) = (walkConjM_ f) `mapM_` (x:xs)

type Goal = Disj

data NameSource = NameSource
  { usedNames :: Set.Set String
  , counter   :: Int
  }

data NormalizationState a = NormalizationState
  { getNameSource :: NameSource
  , getNewDefs    :: Set.Set (Def S.Goal a)
  , getBoundVars  :: Set.Set a
  }

initNormalizationState :: [String] -> NormalizationState a
initNormalizationState namesTaken =
  NormalizationState
  { getNameSource = initNameSource namesTaken
  , getNewDefs = Set.empty
  , getBoundVars = Set.empty
  }

initNameSource :: [String] -> NameSource
initNameSource xs = NameSource { usedNames = Set.fromList xs, counter = 0 }

withLocalVars :: (Ord a) =>
                 Set.Set a ->
                 (b -> State (NormalizationState a) c) ->
                 b ->
                 State (NormalizationState a) c
withLocalVars boundVars f x  = do
  oldBoundVars <- gets getBoundVars
  modify $ addNewBoundVars boundVars
  r <- f x
  modify $ \s -> s { getBoundVars = oldBoundVars }
  return r

normalizeDefs :: Ord a => [Def S.Goal a] -> [Def Disj a]
normalizeDefs defs =
    let namesTaken = map getName defs in
    evalState go (initNormalizationState namesTaken)
  where
    go = do
      defs <- mapM normalizeDef defs
      newDefs <- normalizeNewDefs
      return (defs ++ newDefs)

normalizeDef :: Ord a
             => Def S.Goal a
             -> State (NormalizationState a) (Def Disj a)
normalizeDef def = do
  body <- withLocalVars (Set.fromList $ getArgs def) (normalizeGoal $ getName def) (getBody def)
  return $ def { getBody = body }

normalizeNewDefs :: (Ord a, Eq a) => State (NormalizationState a) [Def Goal a]
normalizeNewDefs = do
  defs <- gets getNewDefs
  if Set.null defs
  then
    return []
  else do
    let (def, rest) = Set.deleteFindMin defs
    modify $ \s -> s { getNewDefs = rest }
    def <- normalizeDef def
    (def :) <$> normalizeNewDefs


normalizeGoal :: (Ord a, Eq a) => String -> S.Goal a -> State (NormalizationState a) (Goal a)
normalizeGoal name =
    goDisj
  where
    newName :: State (NormalizationState a) String
    newName = do
      nameSource <- gets getNameSource
      let n = name ++ show (counter nameSource)
      if n `elem` usedNames nameSource
      then do
        modify $ \s -> s { getNameSource = nameSource { counter = counter nameSource + 1 } }
        newName
      else do
        modify $ \s -> s { getNameSource = nameSource { usedNames = Set.insert n $ usedNames nameSource }}
        return n

    newCall goal = do
      n <- newName
      boundVars <- gets getBoundVars
      let vars = Set.intersection boundVars (S.allVars goal)
      let newDef = Def { getName = n, getArgs = Set.toList vars, getBody = goal }
      modify $ \state -> state { getNewDefs = Set.insert newDef (getNewDefs state) }
      return $ Call n $ map Var (Set.toList vars)
    goDisj (S.Disj x y xs) = do
      x <- goConj x
      y <- goConj y
      xs <- mapM goConj xs
      return $ Disj $ x :| (y : xs)
    goDisj goal = do
      goal <- goConj goal
      return $ Disj $ goal :| []
    goConj (S.Conj x y xs) = do
      x <- goBase x
      y <- goBase y
      xs <- mapM goBase xs
      return $ Conj $ x :| (y : xs)
    goConj goal@(S.Disj _ _ _) = do
      call <- newCall goal
      return $ Conj $ call :| []
    goConj goal = do
      goal <- goBase goal
      return $ Conj $ goal :| []
    goBase (S.Call n as) = do
      modify $ addNewBoundVars (Set.fromList $ map getVar as)
      return $ Call n as
    goBase (S.Unif x t) = do
      modify $ addNewBoundVars (Set.insert (getVar x) (varsFromTerm t))
      return $ Unif x t
    goBase (S.EtaD (S.Call n as)) = do
      modify $ addNewBoundVars (Set.fromList $ map getVar as)
      return $ Call n as
    goBase goal =
      newCall goal

normalize :: (Ord a, Eq a, Show a) => Program S.Goal a -> Program Goal a
normalize program =
    let namesTaken = map getName (getDefs program) in
    evalState (normalizeProgram program) (initNormalizationState namesTaken)
  where
    normalizeProgram (Program defs goal) = do
      defs <- mapM normalizeDef defs
      goal <- withLocalVars (S.allVars goal) (normalizeGoal "topLevel") goal
      newDefs <- normalizeNewDefs
      return $ Program (defs ++ newDefs) goal

addNewBoundVars :: Ord a => Set.Set a -> NormalizationState a -> NormalizationState a
addNewBoundVars vars s = s { getBoundVars = Set.union vars $ getBoundVars s }

back :: Show a => Program Goal a -> Program S.Goal a
back (Program defs goal) =
    Program (map backDef defs) (backGoal goal)

backDef :: Def Disj a -> Def S.Goal a
backDef def@(Def name args body) =
    Def name args (backGoal body)

backGoal :: Disj a -> S.Goal a
backGoal (Disj (x :| (h:t))) = S.Disj (backConj x) (backConj h) (map backConj t)
backGoal goal@(Disj (x :| [])) = backConj x

backConj :: Conj a -> S.Goal a
backConj (Conj (x :| (h:t))) = S.Conj (backBase x) (backBase h) (map backBase t)
backConj (Conj (x :| [])) = backBase x

backBase :: Base a -> S.Goal a
backBase (Unif x t) = S.Unif x t
backBase (Call name args) = S.EtaD $ S.Call name args

allVars :: (Ord a, Eq a) => Goal a -> Set.Set a
allVars =
    goDisj
  where
    goDisj (Disj (x :| xs)) = Set.unions (goConj x : map goConj xs)
    goConj (Conj (x :| xs)) = Set.unions (goBase x : map goBase xs)
    goBase (Call _ args) = Set.fromList $ map getVar args
    goBase (Unif v t) = Set.insert (getVar v) $ varsFromTerm t
