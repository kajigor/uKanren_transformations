{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections    #-}
module Mode.Analysis where

import           Control.Applicative   ((<|>))
import           Control.Monad.State
import           Data.Function         (on)
import           Data.List             (permutations, sortOn)
import           Data.List.NonEmpty    (NonEmpty (..), fromList)
import qualified Data.Map              as Map
import qualified Data.Map.Merge.Strict as Merge
import           Data.Maybe
import qualified Data.Set              as Set
import           Def
import           Mode.Inst
import           Mode.NormSyntax
import           Mode.Term

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

forgetModes :: Goal (a, Mode) -> Goal a
forgetModes = (fst <$>)

updateAfterInst :: Functor f => Inst -> f (a, Mode) -> f (a, Mode)
updateAfterInst inst t =
    go inst <$> t
  where
    go :: Inst -> (a, Mode) -> (a, Mode)
    go inst (x, mode) = (x, mode { after = Just inst })

afterMode :: Var (a, Mode) -> Maybe Inst
afterMode (Var (_, mode)) = after mode

moreInstantiated :: (a, Mode) -> (a, Mode) -> Bool
moreInstantiated (_, mode1) (_, mode2) =
    go (before mode1) (before mode2)
  where
    go Free Ground = False
    go _ _ = True

suitableMode :: Show a => String -> [Var (a, Mode)] -> [(a, Mode)] -> Bool
suitableMode name args as =
  all (uncurry moreInstantiated) (zip (map getVar args) as)

suitableMode' :: Show a => String -> [Var (a, Mode)] -> [(a, Mode)] -> Bool
suitableMode' name args as =
  all (uncurry ((==) `on` snd)) (zip (map getVar args) as)

runAnalyze :: (Show a, Ord a)
           => AllowFree
           -> Goal a
           -> [a]
           -> Either ModeAnalysisError (Goal (a, Mode))
runAnalyze allowFree goal ins =
  let goal' = initMode goal (Map.fromList $ zip ins $ repeat Ground) in
  evalStateT (analyze allowFree goal') emptyAnalyzeState

emptyAnalyzeState :: AnalyzeState a
emptyAnalyzeState = AnalyzeState { getDefinitions = Map.empty
                                 , getAllModdedDefs = Map.empty
                                 , getInstMap = Map.empty
                                 , getQueue = Set.empty }

data AnalyzeState a = AnalyzeState
  { getDefinitions   :: Map.Map String (Def Goal a)
  , getAllModdedDefs :: Map.Map String [[(a, Mode)]]
  , getInstMap       :: Map.Map a Mode
  , getQueue         :: Set.Set (String, [(a, Mode)])
  }
  deriving (Show)

updateInstMap :: Ord a => a -> Mode -> StateT (AnalyzeState a) (Either ModeAnalysisError) ()
updateInstMap v mode = do
  modify $ \state -> state { getInstMap = Map.insert v mode (getInstMap state) }

modifyMode :: Ord a
           => (Mode -> Either ModeAnalysisError Mode)
           -> Var (a, Mode)
           -> StateT (AnalyzeState a) (Either ModeAnalysisError) (Var (a, Mode))
modifyMode f (Var (v, mode)) = do
  m <- lift $ f mode
  updateInstMap v m
  return (Var (v, m))

modifyModeTerm :: Ord a
               => (Mode -> Either ModeAnalysisError Mode)
               -> FlatTerm (a, Mode)
               -> StateT (AnalyzeState a) (Either ModeAnalysisError) (FlatTerm (a, Mode))
modifyModeTerm f (FTCon name vars) = do
  vars <- mapM (modifyMode f) vars
  return $ FTCon name vars
modifyModeTerm f (FTVar v) = do
  FTVar <$> modifyMode f v

data AllowFree = AllowFree | DisallowFree

analyze :: (Show a, Ord a)
        => AllowFree
        -> Goal (a, Mode)
        -> StateT (AnalyzeState a) (Either ModeAnalysisError) (Goal (a, Mode))
analyze allowFree goal = do
    go goal
  where
    go = goDisj
    goDisj (Disj (x :| xs)) = do
      instMap <- gets getInstMap
      x <- goConj x
      xs <- mapM (\g -> do modify (\s -> s { getInstMap = instMap }); goConj g) xs
      return (Disj (x :| xs))
    goConj goal@(Conj (x :| xs)) = do
        choice $ map processConj $ permuteConjs (x : xs)
      where
        processConj (Conj (x :| xs)) = do
          x <- goBase x
          xs <- mapM (updateVars >=> goBase) xs
          return (Conj (x :| xs))
    goBase goal@(Call delayed name args) = do
      state <- get
      case Map.lookup name (getAllModdedDefs state) of
        Just ds ->
          let suitable = filter (suitableMode' name args) ds in
          if null suitable
          then newSuitable goal
          else pickSuitable suitable goal
        Nothing -> newSuitable goal
    goBase goal@(Unif v@(Var (var, mode)) t) =
      let makeAfterModeFree m = return $ m { after = Just Free } in
      case before mode of
        Ground -> do
          v <- modifyMode makeAfterModeGround v
          t <- modifyModeTerm makeAfterModeGround t
          return $ Unif v t
        Free -> do
          let tVars = varsFromTerm t
          if any (\(_, m) -> before m == Free ) tVars
          then do
            case allowFree of
              DisallowFree -> lift $ Left "Free variables in both sides of unification"
              AllowFree -> do
                v <- modifyMode makeAfterModeFree v
                t <- modifyModeTerm makeAfterModeFree t
                return (Unif v t)
          else do
            v <- modifyMode makeAfterModeGround v
            t <- modifyModeTerm makeAfterModeGround t
            return (Unif v t)

makeAfterModeGround :: Monad m => Mode -> m Mode
makeAfterModeGround m = return $ m { after = Just Ground }

choice :: [StateT s (Either ModeAnalysisError) b] -> StateT s (Either ModeAnalysisError) b
choice = foldr (<|>) (lift $ Left "Failed to process a conjunction")

permuteConjs :: [Base a] -> [Conj a]
permuteConjs goals =
    let xss = permutations goals in
    map toConj xss
  where
    toConj = Conj . fromList

prioritizeGround :: (Show a, Ord a)
                 => Goal (a, Mode)
                 -> StateT (AnalyzeState a) (Either ModeAnalysisError) (Goal (a, Mode))
prioritizeGround goal =
  analyze DisallowFree goal <|> analyze AllowFree goal

analyzeNewDefs :: (Ord a, Show a)
               => StateT (AnalyzeState a) (Either ModeAnalysisError) [Def Goal (a, Mode)]
analyzeNewDefs = do
    modify (\s -> s { getInstMap = Map.empty })
    state <- get
    if Set.null $ getQueue state
    then return []
    else do
      let ((name, args), queue) = Set.deleteFindMin (getQueue state)
      put $ state { getQueue = queue }
      case Map.lookup name (getDefinitions state) of
        Nothing -> lift $ Left $ name ++ " undefined"
        Just def@(Def _ args' goal) -> do
          let initGoal = initMode goal $ varInstsFromMode args
          modded <- prioritizeGround initGoal
          newInstMap <- gets getInstMap
          newArgs <- lift $ zipWithM (updateAfterMode newInstMap) args' args
          let def = Def name newArgs modded
          newModes <- analyzeNewDefs
          return (def : newModes)
  where
    updateAfterMode instMap var (_, before) =
      case Map.lookup var instMap of
        Just x -> return (var, before { after = after x })
        Nothing -> return (var, before)

    updateMode instMap var =
      case Map.lookup var instMap of
        Just x -> return (var,x)
        Nothing -> return (var, Mode {before = Free, after = Just Free })

updateVars :: (Ord a, Show a)
           => Base (a, Mode)
           -> StateT (AnalyzeState a) (Either ModeAnalysisError) (Base (a, Mode))
updateVars goal =
  do
    instMap <- gets getInstMap
    go instMap goal
  where
    go instMap (Unif v t) = do
      v <- goVar instMap v
      t <- goTerm instMap t
      return (Unif v t)
    go instMap (Call delayed name args) = do
      args <- mapM (goVar instMap) args
      return $ Call delayed name args
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
        _ -> lift $ Left $ show v ++ " undefined"

retrieveInsts :: (Show a, Ord a)
              => Goal (a, Mode)
              -> Either ModeAnalysisError (Map.Map a Mode)
retrieveInsts goal =
    let vars = sortOn fst $ Set.toList $ allVars goal in
    if repeats vars
    then Left ("Some vars have different instantiations: " ++ show vars)
    else return $ Map.fromList vars
  where
    repeats ((k1, v1) : (k2, v2) : xs) | k1 == k2 && v1 /= v2 = True
                                       | otherwise = repeats ((k2, v2) : xs)
    repeats _ = False

checkInsts :: (Show k, Ord k) => Goal (k, Mode) -> Goal (k, Mode) -> [Goal (k, Mode)] -> (Either ModeAnalysisError) (Map.Map k Mode)
checkInsts x y xs = do
    instMap <- retrieveInsts x
    go instMap y xs
  where
    go instMap y xs = do
      yMap <- retrieveInsts y
      let merged = merge instMap yMap
      if Nothing `elem` Map.elems merged
      then Left "Instantiations are incompatible"
      else
        let newInstMap = Map.map fromJust merged in
        if null xs
        then return newInstMap
        else go newInstMap (head xs) (tail xs)

    merge = Merge.merge preserve preserve (Merge.zipWithMatched nothingIfNotEq)
    preserve = Merge.mapMissing (\k v -> Just v)
    nothingIfNotEq _ x y | x /= y = Nothing
                         | otherwise  = Just x

enqueueModded :: (Ord a, Monad m, Show a) => String -> [(a, Mode)] -> StateT (AnalyzeState a) m ()
enqueueModded name args = do
  oldQueue <- gets getQueue
  allSeenModes <- gets getAllModdedDefs
  case Map.lookup name allSeenModes of
    Just modes | args `elem` modes -> return () -- TODO not a good way to check compatibility
    _ -> modify $ \s -> s { getAllModdedDefs = Map.insertWith (++) name [args] allSeenModes
                          , getQueue = Set.insert (name, args) oldQueue
                          }

varInstsFromMode :: Ord a => [(a, Mode)] -> Map.Map a Inst
varInstsFromMode = Map.fromList . map (before <$>)

newSuitable :: (Show a, Ord a)
            => Base (a, Mode)
            -> StateT (AnalyzeState a) (Either ModeAnalysisError) (Base (a, Mode))
newSuitable call@(Call delayed name args) = do
    state <- get
    let defs = getDefinitions state
    case Map.lookup name defs of
      Just (Def _ xs body) -> do
        let varInsts = Map.fromList $ map (\(Var (v, mode)) -> (v, before mode)) args
        let newBody = newMode body varInsts
        let newModded = zipWith pushMode args xs -- HERE IS THE PROBLEM
        enqueueModded name newModded
        grounded <- mapM (modifyMode makeAfterModeGround) args
        intsMap <- gets getInstMap
        return (Call delayed name grounded)
      _ -> lift $ Left $ name ++ " undefined"
  where
    pushMode (Var (x, mode)) y =
      (y, mode { after = Just Ground }) -- TODO REMOVE
newSuitable _ =
  lift $ Left "Unable to find newSuitable call"

pickSuitable :: Ord a
             => [[(a, Mode)]]
             -> Base (a, b)
             -> StateT (AnalyzeState a) (Either ModeAnalysisError) (Base (a, Mode))
pickSuitable xs (Call delayed name args) = do
    args <- merge (pick xs) args
    return $ Call delayed name args
  where
    pick = head
    merge =
      zipWithM (\(_, mode) (Var (a, _)) -> do
        updateInstMap a mode
        return $ Var (a, mode))
pickSuitable _ _ =
  lift $ Left "Unable to find a suitably moded call"
