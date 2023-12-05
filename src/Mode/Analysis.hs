{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Mode.Analysis where

import Control.Monad.State
    ( msum,
      zipWithM,
      StateT,
      gets,
      modify,
      evalStateT,
      MonadTrans(lift) )
import           Data.List.NonEmpty    (NonEmpty (..))
import qualified Data.Map              as Map
import Data.Maybe ( fromJust, fromMaybe, isJust )
import qualified Data.Set              as Set
import           Def (Def(..))
import Mode.Inst
    ( Mode(..),
      Inst(..),
      identicalBeforeModes,
      isBeforeGround,
      isBeforeFree,
      isAfterGround )
import Mode.NormSyntax
    ( Base(..),
      Conj(..),
      Disj(..),
      Goal )
import Mode.Term ( varsFromTerm, FlatTerm(..), Var(..) )
import Text.Printf (printf)

type ModeAnalysisError = String

initVarFromMap :: Ord a => Map.Map a Inst -> a -> (a, Mode)
initVarFromMap instMap v =
    initMode (getInitInstVar v instMap) v
  where
    initMode :: Inst -> a -> (a, Mode)
    initMode Ground x =
      (x, Mode { before = Ground, after = Just Ground })
    initMode inst x =
      (x, Mode { before = inst, after = Nothing })

getInitInstVar :: Ord a => a -> Map.Map a Inst -> Inst
getInitInstVar v instMap =
  fromMaybe Free (Map.lookup v instMap)

initMode :: (Functor f, Ord a, Show a, Show (f (a, Mode))) => f a -> Map.Map a Inst -> f (a, Mode)
initMode goal instMap =
  initVarFromMap instMap <$> goal

newMode :: (Functor f, Ord a) => f a -> Map.Map a Inst -> f (a, Mode)
newMode goal instMap =
    go <$> goal
  where
    go v = initMode (getInitInstVar v instMap) v
    initMode inst x = (x, Mode { before = inst, after = Just Ground })

afterMode :: Var (a, Mode) -> Maybe Inst
afterMode (Var (_, mode)) = after mode

suitableMode :: Show a => [Var (a, Mode)] -> [(a, Mode)] -> Bool
suitableMode args xs =
  identicalBeforeModes (map getVar args) xs

runAnalyze :: (Show a, Ord a)
           => Goal a
           -> [a]
           -> Either ModeAnalysisError (Goal (a, Mode))
runAnalyze goal ins =
  let goal' = initMode goal (Map.fromList $ map (, Ground) ins) in
  evalStateT (analyze goal') (emptyAnalyzeState Nothing)

emptyAnalyzeState :: Maybe (Base (a, Mode)) -> AnalyzeState a
emptyAnalyzeState currentRelation =
  AnalyzeState { getDefinitions = Map.empty
               , getAllModdedDefs = Map.empty
               , getInstMap = Map.empty
               , getQueue = Set.empty
               , getCurrentRelation = currentRelation
               }

data AnalyzeState a = AnalyzeState
  { getDefinitions     :: Map.Map String (Def Goal a)
  , getAllModdedDefs   :: Map.Map String [[(a, Mode)]]
  , getInstMap         :: Map.Map a Mode
  , getQueue           :: Set.Set (String, [(a, Mode)])
  , getCurrentRelation :: Maybe (Base (a, Mode))
  }
  deriving (Show)

type AnalyzeStateM a b = StateT (AnalyzeState a) (Either ModeAnalysisError) b 

updateInstMap :: (Show a, Ord a) => a -> Mode -> AnalyzeStateM a () 
updateInstMap v mode = do
  modify $ \s -> s { getInstMap = Map.insert v mode (getInstMap s) }

modifyMode :: (Ord a, Show a)
           => (Mode -> Either ModeAnalysisError Mode)
           -> Var (a, Mode)
           -> AnalyzeStateM a (Var (a, Mode))
modifyMode f (Var (v, mode)) = do
  m <- lift $ f mode
  updateInstMap v m
  return (Var (v, m))

modifyModeTerm :: (Show a, Ord a)
               => (Mode -> Either ModeAnalysisError Mode)
               -> FlatTerm (a, Mode)
               -> AnalyzeStateM a (FlatTerm (a, Mode))
modifyModeTerm f (FTCon name vars) = do
  vars <- mapM (modifyMode f) vars
  return $ FTCon name vars
modifyModeTerm f (FTVar v) = do
  FTVar <$> modifyMode f v

isGuard :: (Ord a) => Base (a, Mode) -> Bool
isGuard (Unif (Var v) (FTVar (Var t))) =
  isBeforeGround v && isBeforeGround t
isGuard (Call _ args) =
  all (isBeforeGround . getVar) args
isGuard _ = False

isDeconstruction :: (Ord a) => Base (a, Mode) -> Bool
isDeconstruction (Unif (Var v) (FTCon _ _)) = isBeforeGround v
isDeconstruction _ = False

returnsOne :: Base (a, Mode) -> Bool
returnsOne call@(Call _ args) =
  1 == length (filter (not . isBeforeGround . getVar) args)
returnsOne _ = False

isAssignment :: (Ord a) => Base (a, Mode) -> Bool
isAssignment (Unif (Var v) (FTVar (Var t))) =
  isBeforeGround v /= isBeforeGround t
isAssignment _ = False

isConstruction :: (Ord a) => Base (a, Mode) -> Bool
isConstruction (Unif (Var v) t@(FTCon _ _)) =
  not (isBeforeGround v) && all isBeforeGround (varsFromTerm t)
isConstruction _ = False

completelyFreeUnif :: Ord a => Base (a, Mode) -> Bool
completelyFreeUnif (Unif (Var v) t) =
  (not . isBeforeGround) v && not (any isBeforeGround (varsFromTerm t))
completelyFreeUnif _ = True

completelyFreeCall :: Base (a, Mode) -> Bool
completelyFreeCall call@(Call _ args) =
  not (any (isBeforeGround . getVar) args)
completelyFreeCall _ = True

groundifies :: (Ord a) => Base (a, Mode) -> Bool
groundifies (Call _ args) = True
groundifies unif@(Unif (Var v) t) =
  isAfterGround v || any isAfterGround (varsFromTerm t)

varGenerator :: (Ord a) => Base (a, Mode) -> Bool
varGenerator (Unif (Var v) (FTVar (Var t))) = isBeforeFree v && isBeforeFree t
varGenerator _ = False

-- Selects the first goal for which one of the preditates holds. 
select :: (Show a) => [a -> Bool] -> a -> [a] -> (a, [a])
select predicates x xs =
    fromMaybe (x, xs) $ msum $ map (\p -> findElem p (x:xs)) predicates
  where
    findElem p = go id
      where
        go _ [] = Nothing
        go prefix (x : xs) | p x = Just (x, prefix xs)
                           | otherwise = go (prefix . (x:)) xs

generator :: Ord a => Base (a, Mode) -> Bool
generator (Unif (Var v) t) =
  isBeforeGround v && any isBeforeGround (varsFromTerm t)
generator _ = False

directRecursion :: Maybe (Base (a, Mode)) -> Base (a, Mode) -> Bool
directRecursion (Just (Call name args)) (Call name' args') | name == name' = identicalBeforeModes (map getVar args) (map getVar args')
directRecursion _ _ = False

prioritySelection :: (Show a, Ord a) => Maybe (Base (a, Mode)) -> Base (a, Mode) -> [Base (a, Mode)] -> (Base (a, Mode), [Base (a, Mode)])
prioritySelection currentRelation =
  select [ isGuard
         , isConstruction
         , isDeconstruction
         , isAssignment
         , returnsOne
         , directRecursion currentRelation
         , not.completelyFreeCall
         , varGenerator
         , not.completelyFreeUnif
         , groundifies]

analyze :: (Show a, Ord a)
        => Goal (a, Mode)
        -> AnalyzeStateM a (Goal (a, Mode))
analyze goal = do
    goDisj goal
  where
    goDisj (Disj (x :| xs)) = do
      instMap <- gets getInstMap
      x <- goConj x
      xs <- mapM (\g -> do modify (\s -> s { getInstMap = instMap }); goConj g) xs
      return (Disj (x :| xs))

    goConj goal@(Conj (x :| xs)) = do 
      currentRelation <- gets getCurrentRelation
      case prioritySelection currentRelation x xs of
        (x, xs) -> do
          x <- goBase x
          xs <- mapM updateVars xs
          case xs of
            [] -> return $ Conj (x :| [])
            (y : ys) -> do
              Conj (y :| ys) <- goConj $ Conj (y :| ys)
              return $ Conj (x :| (y : ys))

    goBase goal@(Call name args) = do
      allModdedDefs <- gets getAllModdedDefs
      case Map.lookup name allModdedDefs of
        Just ds | (not . null) suitable -> 
            pickSuitable suitable goal
          where 
            suitable = filter (suitableMode args) ds
        _ -> newSuitable goal
    goBase goal@(Unif v@(Var (var, mode)) t) = do 
      let makeAfterModeGround m = return $ m { after = Just Ground }
      v <- modifyMode makeAfterModeGround v
      t <- modifyModeTerm makeAfterModeGround t
      return $ Unif v t

makeAfterModeGround :: Monad m => Mode -> m Mode
makeAfterModeGround m = return $ m { after = Just Ground }

getRelation :: String -> AnalyzeStateM a (Def Goal a)
getRelation name = do 
  defs <- gets getDefinitions 
  case Map.lookup name defs of 
    Nothing -> lift $ Left $ printf "Relation %s undefined" name
    Just d -> return d 

analyzeNewDefs :: (Ord a, Show a)
               => AnalyzeStateM a [Def Goal (a, Mode)]
analyzeNewDefs = do
    modify (\s -> s { getInstMap = Map.empty })
    queue <- gets getQueue
    if Set.null queue
    then return []
    else do
      (name, args) <- getNextDef
      (initGoal, args') <- initDef name args 
      modded <- analyze initGoal
      newInstMap <- gets getInstMap
      newArgs <- lift $ zipWithM (updateAfterMode newInstMap) args' args
      let def = Def name newArgs modded
      newModes <- analyzeNewDefs
      return (def : newModes)
  where
    updateAfterMode instMap var (_, before) =
      return (var, before { after = Just Ground })

    getNextDef = do 
      queue <- gets getQueue
      let ((name, args), queue') = Set.deleteFindMin queue
      modify $ \s -> s { getQueue = queue' }
      return (name, args)

    initDef name args = do 
      def@(Def name' args' goal) <- getRelation name 
      let varInsts = varInstsFromMode args
      let initGoal = initMode goal varInsts
      let curRel = initMode (Call name' (map Var args')) varInsts
      modify (\s -> s { getCurrentRelation = Just curRel })
      return (initGoal, args')

updateVars :: (Ord a, Show a)
           => Base (a, Mode)
           -> AnalyzeStateM a (Base (a, Mode))
updateVars goal =
  do
    instMap <- gets getInstMap
    go instMap goal
  where
    go instMap (Unif v t) = do
      v <- goVar instMap v
      t <- goTerm instMap t
      return (Unif v t)
    go instMap (Call name args) = do
      args <- mapM (goVar instMap) args
      return $ Call name args
    goTerm instMap (FTVar var) = do
      var <- goVar instMap var
      return $ FTVar var
    goTerm instMap (FTCon name args) = do
      args <- mapM (goVar instMap) args
      return (FTCon name args)
    goVar instMap var@(Var (v, mode)) =
      case Map.lookup v instMap of
        Nothing -> return var
        Just m | isJust $ after m ->
          return (Var (v, Mode { before = fromJust $ after m, after = Nothing }))
        _ -> lift $ Left $ printf "Var %s undefined" (show v)

enqueueModded :: (Ord a, Show a) => String -> [(a, Mode)] -> AnalyzeStateM a ()
enqueueModded name args = do
    oldQueue <- gets getQueue
    allSeenModes <- gets getAllModdedDefs
    case Map.lookup name allSeenModes of
      Just modes | hasCompatibleMode args modes -> return ()
      _ -> do
        let args' = map (\(v, m) -> (v, m {after = Just Ground})) args
        modify $ \s -> s { getAllModdedDefs = Map.insertWith (++) name [args'] allSeenModes
                         , getQueue = Set.insert (name, args') oldQueue
                         }
  where
    hasCompatibleMode args modes =
      any (identicalBeforeModes args) modes

varInstsFromMode :: Ord a => [(a, Mode)] -> Map.Map a Inst
varInstsFromMode = Map.fromList . map (before <$>)

newSuitable :: (Show a, Ord a)
            => Base (a, Mode)
            -> AnalyzeStateM a (Base (a, Mode))
newSuitable call@(Call name args) = do
    Def _ xs body <- getRelation name 
    let varInsts = Map.fromList $ map (\(Var (v, mode)) -> (v, before mode)) args
    let newModded = zipWith pushMode args xs -- HERE IS THE PROBLEM
    enqueueModded name newModded
    grounded <- mapM (modifyMode makeAfterModeGround) args
    return (Call name grounded)
  where
    pushMode (Var (x, mode)) y =
      (y, mode { after = Just Ground }) -- TODO REMOVE
newSuitable _ =
  lift $ Left "Unable to find newSuitable call"

pickSuitable :: (Ord a, Show a)
             => [[(a, Mode)]]
             -> Base (a, Mode)
             -> AnalyzeStateM a (Base (a, Mode))
pickSuitable xs (Call name args) = do
    args <- merge (pick xs) args
    return $ Call name args
  where
    pick = head
    merge =
      zipWithM (\(_, mode) (Var (a, _)) -> do
        updateInstMap a mode
        return $ Var (a, mode))
pickSuitable _ goal =
  lift $ Left $ printf "Unable to find a suitably moded call: %s" (show goal)
