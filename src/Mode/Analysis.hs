{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
module Mode.Analysis where

import Mode.Inst
import Mode.Syntax
import Def
import Program

import Data.Maybe
import Data.List (sortOn, nub)
import Control.Monad.State
import qualified Data.Map as Map
import qualified Data.Map.Merge.Strict as Merge
import qualified Data.Set as Set
import Debug.Trace
import Data.List (permutations)
import Control.Applicative ((<|>))
import Mode.Term


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

initMode :: (Functor f, Ord a) => f a -> Map.Map a Inst -> f (a, Mode)
initMode goal instMap =
  initVarFromMap instMap <$> goal

forgetModes :: Goal (a, Mode) -> Goal a
forgetModes = (fst <$>)

updateAfterInst :: Functor f => Inst -> f (a, Mode) -> f (a, Mode)
updateAfterInst inst t =
    go inst <$> t
  where
    go :: Inst -> (a, Mode) -> (a, Mode)
    go inst (x, mode) = (x, Mode { before = before mode, after = Just inst })

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
  trace (surround "In suitable mode" (name, args, as) ) $
  all (uncurry moreInstantiated) (zip (map getVar args) as)

runAnalyze :: (Show a, Ord a) => Goal a -> [a] -> Maybe (Goal (a, Mode))
runAnalyze goal ins =
  let goal' = initMode goal (Map.fromList $ zip ins $ repeat Ground) in
  evalStateT (analyze goal') emptyAnalyzeState

emptyAnalyzeState :: AnalyzeState a
emptyAnalyzeState = AnalyzeState { getDefinitions = Map.empty
                                 , getAllModdedDefs = Map.empty
                                 , getInstMap = Map.empty
                                 , getQueue = Set.empty }

data AnalyzeState a = AnalyzeState
  { getDefinitions :: Map.Map String (Def Goal a)
  , getAllModdedDefs :: Map.Map String [[(a, Mode)]]
  , getInstMap :: Map.Map a Mode
  , getQueue :: Set.Set (String, [(a, Mode)])
  }
  deriving (Show)

updateInstMap :: Ord a => a -> Mode -> StateT (AnalyzeState a) Maybe ()
updateInstMap v mode = do
  state <- get
  put $ state { getInstMap = Map.insert v mode (getInstMap state) }

modifyMode :: Ord a => Var (a, Mode) -> (Mode -> Maybe Mode) -> StateT (AnalyzeState a) Maybe (Var (a, Mode))
modifyMode (Var (v, mode)) f = do
  m <- lift $ f mode
  updateInstMap v m
  return (Var (v, m))

modifyModeTerm :: Ord a => FlatTerm (a, Mode) -> (Mode -> Maybe Mode) -> StateT (AnalyzeState a) Maybe (FlatTerm (a, Mode))
modifyModeTerm (FTCon name vars) f = do
  vars <- mapM (\v -> modifyMode v f) vars
  return $ FTCon name vars
modifyModeTerm (FTVar v) f = do
  FTVar <$> modifyMode v f


analyze :: (Show a, Ord a) => Goal (a, Mode) -> StateT (AnalyzeState a) Maybe (Goal (a, Mode))
analyze goal = do
    res <- go goal
    return (trace (surround "\n--------------\nIn analyze\nGoal:\n" goal ++ "\n--------------\n" ++ surround "Result:\n" res) $ res)
  where
    go goal@(Unif v@(Var (var, mode)) t) =
      let makeAfterModeGround m = Just $ m { after = Just Ground } in
      case before mode of
        Ground -> do
          v <- modifyMode v makeAfterModeGround
          t <- modifyModeTerm t makeAfterModeGround
          return $ Unif v t
        Free -> do
          let tVars = varsFromTerm t
          if any (\(Var (_, m)) -> before m == Free ) tVars
          then
            lift Nothing
          else do
            v <- modifyMode v makeAfterModeGround
            return (Unif v t)

      -- let vGround = updateAfterInst Ground v in
      -- case after mode of
      --   Just Ground -> do
      --     let tGround = updateAfterInst Ground t
      --     updateInstMap var (snd $ getVar vGround)
      --     let tVars = varsFromTerm t
      --     mapM_ (\(Var (t, mode)) -> updateInstMap t (mode { after = Just Ground })) tVars
      --     return $ Unif vGround tGround
      --   _ -> case t of
      --     FTVar v | afterMode v == Just Ground -> do
      --       updateInstMap var (snd $ getVar vGround)
      --       return $ Unif vGround t
      --     FTCon s vars | all ((== Just Ground) . afterMode) vars -> do
      --       updateInstMap var (snd $ getVar vGround)
      --       return $ Unif vGround t
      --     _ -> return goal
    go goal@(Call name args) = do
      state <- get
      case Map.lookup name (getAllModdedDefs state) of
        Just ds ->
          let suitable = filter (suitableMode name args) ds in
          if null suitable
          then newSuitable goal
          else pickSuitable suitable goal
        Nothing -> newSuitable goal
    go (Disj x y xs) = do
      state <- get
      (x : y : xs) <- mapM (\g -> do put state; analyze g) (x : y : xs)
      return (Disj x y xs)
      -- case checkInsts x y xs of
      --   Just _ -> return $ Disj x y xs
      --   _ -> lift Nothing
    go goal@(Conj x y xs) =
        choice $ map processConj $ permuteConjs goal
      where
        processConj (Conj x y xs) = do
          x <- analyze x
          y <- (updateVars >=> analyze) y
          xs <- mapM (updateVars >=> analyze) xs
          return (Conj x y xs)
        processConj _ = lift Nothing

choice t = foldr (<|>) (lift Nothing) t

permuteConjs (Conj x y xs) =
    let xss = permutations (x : y : xs) in
    map toConj xss
  where
    toConj (x : y : xs) = Conj x y xs

surround y x = y ++ "\n=========\n" ++ show x ++ "\n=========\n"


analyzeNewDefs :: (Ord a, Show a) => StateT (AnalyzeState a) Maybe [Def Goal (a, Mode)]
analyzeNewDefs = do
    state <- trace "analyze new def!" $ get
    if Set.null $ getQueue state
    then return []
    else do
      let ((name, args), queue) = Set.deleteFindMin (getQueue state)
      put $ state { getQueue = queue }
      def@(Def _ args' goal) <- lift $ Map.lookup name (getDefinitions state)
      modded <- analyze (initMode goal $ varInstsFromMode args)
      newInstMap <- gets getInstMap
      newArgs <- lift $ mapM (uncurry $ updateAfterMode newInstMap) $ zip args' args
      -- newArgs <- lift $ mapM (updateMode newInstMap) args'
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

updateVars :: Ord a => Goal (a, Mode) -> StateT (AnalyzeState a) Maybe (Goal (a, Mode))
updateVars goal = do
    state <- get
    let instMap = getInstMap state
    lift $ go instMap goal
  where
    go instMap (Unif v t) = do
      v <- goVar instMap v
      t <- goTerm instMap t
      return (Unif v t)
    go instMap (Call name args) = do
      args <- mapM (goVar instMap) args
      return $ Call name args
    go instMap (Conj x y xs) = do
      x <- go instMap x
      y <- go instMap y
      xs <- mapM (go instMap) xs
      return $ Conj x y xs
    go instMap (Disj x y xs) = do
      x <- go instMap x
      y <- go instMap y
      xs <- mapM (go instMap) xs
      return $ Disj x y xs
    goVar instMap var@(Var (v, mode)) =
      case Map.lookup v instMap of
        Nothing -> return var
        Just m | isJust $ after m-> return (Var (v, Mode { before = fromJust $ after m, after = Nothing }))
        _ -> Nothing
        -- Just m | before m == before mode && (isNothing (after mode) || after mode == Just Free || after mode == after m) ->
        --   return (Var (v, mode { after = after m} ))
        -- _ -> Nothing
    goTerm instMap (FTVar var) = do
      var <- goVar instMap var
      return $ FTVar var
    goTerm instMap (FTCon name args) = do
      args <- mapM (goVar instMap) args
      return (FTCon name args)


retrieveInsts :: Ord a => Goal (a, Mode) -> Maybe (Map.Map a Mode)
retrieveInsts goal =
    let vars = sortOn fst $ nub $ allVars goal in
    if repeats vars
    then Nothing
    else Just $ Map.fromList vars
  where
    repeats ((k1, v1) : (k2, v2) : xs) | k1 == k2 && v1 /= v2 = True
                                       | otherwise = repeats ((k2, v2) : xs)
    repeats _ = False

checkInsts :: (Show k, Ord k) => Goal (k, Mode) -> Goal (k, Mode) -> [Goal (k, Mode)] -> Maybe (Map.Map k Mode)
checkInsts x y xs = do
    instMap <- retrieveInsts x
    go instMap y xs
  where
    go instMap y xs = do
      yMap <- retrieveInsts y
      let merged = merge instMap yMap
      if Nothing `elem` Map.elems merged
      then Nothing
      else
        let newInstMap = Map.map fromJust merged in
        if null xs
        then Just $ newInstMap
        else go newInstMap (head xs) (tail xs)

    merge = Merge.merge preserve preserve (Merge.zipWithMatched nothingIfNotEq)
    preserve = Merge.mapMissing (\k v -> Just v)
    nothingIfNotEq _ x y | x /= y = Nothing
                         | otherwise  = Just x

enqueueModded :: (Ord a, Monad m, Show a) => String -> [(a, Mode)] -> StateT (AnalyzeState a) m ()
enqueueModded name args = do
  state <- get
  let allSeenModes = getAllModdedDefs state
  case Map.lookup name allSeenModes of
    Just modes | args `elem` modes -> return () -- TODO not a good way to check compatibility
    _ -> put $ state { getAllModdedDefs = Map.insertWith (++) name [args] allSeenModes
                     , getQueue = Set.insert (name, args) $ getQueue state
                     }

varInstsFromMode :: Ord a => [(a, Mode)] -> Map.Map a Inst
varInstsFromMode = Map.fromList . map (\(v, mode) -> (v, before mode))

newSuitable :: (Show a, Ord a) => Goal (a, Mode) -> StateT (AnalyzeState a) Maybe (Goal (a, Mode))
newSuitable call@(Call name args) = do
    state <- get
    let defs = getDefinitions state
    case Map.lookup name defs of
      Just (Def _ xs body) -> do
        let varInsts = Map.fromList $ map (\(Var (v, mode)) -> (v, before mode)) args
        let newBody = initMode body varInsts
        let newModded = zipWith pushMode args xs
        enqueueModded name newModded
        return call
      _ -> lift Nothing
  where
    pushMode (Var (_, mode)) y =
      (y, mode)
newSuitable _ =
  lift Nothing

pickSuitable xs (Call name args) = do
    args <- merge (pick xs) args
    return $ Call name args
  where
    pick = head
    merge =
      zipWithM (\(_, mode) (Var (a, _)) -> do
        updateInstMap a mode
        return $ Var (a, mode))
pickSuitable _ _ =
  lift Nothing