{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# LANGUAGE FlexibleContexts    #-}
module ConsPD.Unfold where

import           Control.Monad.State
import           ConsPD.State
import           Data.Foldable       (foldlM)
import           Data.List           (delete, partition)
import qualified Data.Map.Strict     as M
import           Data.Maybe          (catMaybes, fromJust, fromMaybe, isNothing, mapMaybe)
import           Descend
import           Embed
import qualified Environment         as Env
import qualified Eval                as E
import qualified FreshNames          as FN
import           Generalization      (Generalizer, generalizeAllVarsToFree)
import           Prelude             hiding (or)
import           Program
import qualified Subst
import           Syntax
import           Unfold              (findBestByComplexity, findTupling, isGoalStatic, oneStep)
import           Util.ListZipper
import qualified Data.Set as Set

-- import Debug.Trace

data ConsPDTree = Fail
                | Success (Subst.Subst S) Env.Env
                | Or [ConsPDTree] (Descend [G S]) (Subst.Subst S)
                | Conj [ConsPDTree] [G S] (Subst.Subst S)
                | Gen ConsPDTree [G S] [G S] Generalizer (Subst.Subst S)
                | Leaf [G S] (Subst.Subst S) Env.Env [G S] -- last argument is a goal renaming of which current node is
                | Split [ConsPDTree] [G S] (Subst.Subst S)
                | Prune [G S] (Subst.Subst S)
                deriving (Show, Eq)

statesConcat :: [Subst.Subst S] -> Maybe (Subst.Subst S)
statesConcat = unifySubsts

goalsConcat :: [([a], Subst.Subst S)] -> Maybe ([a], Subst.Subst S)
goalsConcat x = do
  let (goals, states) = unzip x
  cStates <- statesConcat states
  return (reverse $ concat goals, cStates)

productList :: [[a]] -> [[a]]
productList []       = [[]]
productList (xs:xss) = [ h : t | h <- xs, t <- productList xss]

restrictSubsts :: ConsPDTree -> ConsPDTree
restrictSubsts =
    go Subst.empty
  where
    go subst (Conj ch gs s)  = Conj (map (go s) ch) gs (Subst.difference s subst)
    go subst (Or ch gs s)    = Or (map (go s) ch) gs (Subst.difference s subst)
    go subst (Gen ch gs gs' gen s) = Gen (go subst ch) gs gs' gen (Subst.difference s subst)
    go subst (Leaf gs s e v) = Leaf gs (Subst.difference s subst) e v
    go subst (Split ch gs s) = Split (map (go s) ch) gs (Subst.difference s subst)
    go subst (Prune gs s)    = Prune gs (Subst.difference s subst)
    go subst (Success s e)   = Success (Subst.difference s subst) e
    go _ Fail                = Fail


incrDeepOneStep :: Int -> Int -> G S -> Env.Env -> Subst.Subst S -> Maybe ([([G S], Subst.Subst S)], Env.Env)
incrDeepOneStep limit depth _ _ _ | limit < depth = Nothing
incrDeepOneStep limit depth goal env state =
    let (unified, env') = runState (oneStep goal state) env in
    if all (not . null . fst) unified
    then do
      (goals, newEnv) <-
        foldlM
          (\(acc, e) (gs, sigma) -> do
            (x, e') <-
              foldlM
                (\(acc, e) g -> do
                    (x, e') <- incrDeepOneStep limit (depth + 1) g e sigma
                    return (x : acc, e')
                )
                ([], e)
                gs
            return (acc ++ x, e')
          )
          ([], env')
          unified
      return (mapMaybe goalsConcat $ productList goals, newEnv)
    else
      return (unified, env')

realIncrDeep :: Int -> (a -> a) -> a -> a
realIncrDeep 0 _ x = x
realIncrDeep globalLimit f x =
    go 1 1 f (f x)
  where
    go _ curr f x | globalLimit <= curr = x
    go localLimit curr f x | localLimit <= curr =
      go (max (localLimit * 2) globalLimit) (curr + 1) f (f x)
    go localLimit curr f x =
      go localLimit (curr + 1) f (f x)

leaf :: Subst.Subst S -> Env.Env -> ConsPDTree
leaf s _ | Subst.null s = Fail
leaf s e = Success s e

unifySubsts :: [Subst.Subst S] -> Maybe (Subst.Subst S)
unifySubsts [] = return Subst.empty
unifySubsts [s] = return s
unifySubsts (x : y : xs) = do
    s <- go x (Subst.toList y)
    unifySubsts (s : xs)
  where
    go s [] = Just s
    go s ((v, t) : rest) = do
      s' <- E.unify (Just s) (V v) t
      go s' rest

conjToList :: G a -> [G a]
conjToList (Conjunction x y gs) = (x : y : gs)
conjToList x = [x]

globalLimit :: Int
globalLimit = 8

justUnfold :: Int -> Program G X -> (ConsPDTree, G S, [S])
justUnfold limit (Program defs goal) =
    let env = Env.fromDefs defs in
    let ((logicGoal, names), env') = runState (E.preEval goal) env in
    (go 0 (Descend (conjToList logicGoal) []) env' Subst.empty, logicGoal, names)
  where
    go n (Descend gs ancs) env subst | n > limit || length gs > 1 =
      case findVariant gs ancs of
        Nothing -> Prune gs subst
        Just v -> Leaf gs subst env v
    go n d@(Descend gs ancs) env subst =
      let [goal] = Subst.substituteList subst gs in
      let addDescend g = Descend g ([goal] : ancs) in
      let (unified, env') = runState (oneStep goal subst) env in
      let children = map (\(gs, s) -> if null gs
                                      then Success s env'
                                      else go (n+1) (addDescend gs) env' s
                         ) unified in
      Or children d subst

trace _ x = x 
traceShow _ x = x 

topLevel :: Int -> Program G X -> (ConsPDTree, G S, [S])
topLevel limit (Program defs goal) =
    let env = Env.fromDefs defs in
    let ((logicGoal, names), env') = runState (E.preEval goal) env in
    let descend = Descend.init (conjToList logicGoal) in
    let tree = evalState (go descend Subst.empty) (ConsPD.State.init env') in
    (tree, logicGoal, names)
  where
    report goal seen = 
      trace "\nGo\nGoal" $ 
      traceShow goal $ 
      trace "Seen" $ 
      traceShow seen 

    go :: Descend [G S] -> Subst.Subst S -> State ConsPDState ConsPDTree
    go (Descend goal' ancs') state = do
      (ConsPDState seen failed env) <- get
      let (hd, _) = FN.getFreshName (Env.getFreshNames env)
      let goal = Subst.substituteList state goal'
      let seen' = Set.insert goal seen
      
      let ancs = goal : ancs'
      let addAnc x = Descend x ancs
      if report goal seen $ variantCheck goal failed
      then
        return Fail
      else
        if limit > 0 && hd > limit
        then
          return $ Prune goal state
        else
          case if isGround goal then Nothing else findVariant goal seen of
            Just v ->
              return $ trace "Is variant!" $ traceShow v $ Leaf goal state env v
            Nothing | any (`embed` goal) ancs' ->
              -- A test for accumulating parameter
              -- f (x, y) /\ f (z, y) -> f (m, n) /\ f (z, C (n))
              if length goal == 1
              then
                case findInstance goal seen of
                  Just v ->
                    return $ trace "Is instance!" $ traceShow v $ Leaf goal state env v
                  Nothing -> do
                    let (allFree, generalizer, env') = generalizeAllVarsToFree goal env
                    put (ConsPDState seen' failed env')
                    ch <- go (addAnc allFree) state
                    return $ trace "Is gen!" $ Gen ch goal allFree generalizer state
              else do
                put (ConsPDState seen' failed env)
                let unified = zip (map (:[]) goal) (repeat state)
                children <- unfoldSequentially unified addAnc
                split children goal state
            Nothing ->
              if length goal == 1
              then do
                put (ConsPDState seen' failed env)
                unified <- adaptState (oneStep (head goal) state)
                ch <- trace "unified: " $ traceShow unified $ trace "ancs" $ traceShow ancs $ unfoldSequentially unified addAnc
                or (trace "4" $ ch) (addAnc goal) state
              else
                case findTupling env state goal ancs of
                  Just (unfolded, newEnv) -> do
                    put (ConsPDState seen' failed newEnv)
                    ch <- unfoldSequentially unfolded addAnc
                    or (trace "3" $ ch) (addAnc goal) state
                  Nothing ->
                    case findBestByComplexity env state goal of
                      -- Either ls or rs is not empty!
                      Just zipper ->
                        let ls = left zipper in
                        let x  = cursor zipper in
                        let rs = right zipper in
                        case if isGoalStatic env x then Nothing else findVariant [x] seen' of
                          Just v -> do
                            modifySeen (const seen')
                            let x' = Leaf [x] state env v
                            ls' <- unfoldNotNull ls state addAnc
                            rs' <- unfoldNotNull rs state addAnc
                            split (catMaybes [ls', Just x', rs']) goal state
                          Nothing -> do
                            unified <- adaptState (oneStep x state)
                            modifySeen (const seen')
                            ch <- unfoldSequentially unified addAnc
                            x' <- or (trace "2\nChildren: " $ traceShow ch $ ch) (addAnc [x]) state
                            -- ls' <- unfoldNotNull ls state addAnc
                            -- rs' <- unfoldNotNull rs state addAnc
                            -- split (catMaybes [ls', Just x', rs']) goal state
                            case computedAnswers x' of
                              Just xs ->
                                if all (\(gs, _, _) -> null gs || instanceCheck gs seen) xs
                                then do
                                  -- WHAT IF IT RENAMES WITHIN THIS SUBTREE???
                                  children <- mapM (\(goals, subst, newEnv) -> do
                                      let toUnfold = wrap ls rs goals
                                      if null toUnfold
                                      then
                                        return $ leaf subst newEnv
                                      else do
                                        modifyEnv (const newEnv)
                                        go (addAnc toUnfold) subst
                                    ) xs
                                  modifySeen (const seen)
                                  or (trace "1" $ trace "computed" $ traceShow xs $ reverse children) (addAnc $ wrap ls rs [x]) state
                                else do
                                  ls' <- unfoldNotNull ls state addAnc
                                  rs' <- unfoldNotNull rs state addAnc
                                  split (catMaybes [ls', Just x', rs']) goal state
                              Nothing -> do
                                ls' <- unfoldNotNull ls state addAnc
                                rs' <- unfoldNotNull rs state addAnc
                                split (catMaybes [ls', Just x', rs']) goal state
                      Nothing -> do
                        put (ConsPDState seen' failed env)
                        children <- unfoldSequentially (zip (map (:[]) goal) (repeat state)) addAnc
                        split children goal state

    -- go :: Descend [G S] -> Subst.Subst S -> State ConsPDState ConsPDTree
    -- go (Descend goal' ancs') state = do
    --   (ConsPDState seen failed env) <- get
    --   let (hd, _) = FN.getFreshName (Env.getFreshNames env)
    --   let goal = Subst.substituteList state goal'
    --   let seen' = Set.insert goal seen
      
    --   let ancs = goal : ancs'
    --   let addAnc x = Descend x ancs
    --   if report goal seen $ variantCheck goal failed
    --   then
    --     return Fail
    --   else
    --     if limit > 0 && hd > limit
    --     then
    --       return $ Prune goal state
    --     else
    --       case if isGround goal then Nothing else findVariant goal seen of
    --         Just v ->
    --           return $ trace "Is variant!" $ traceShow v $ Leaf goal state env v
    --         Nothing | any (`embed` goal) ancs' ->
    --           -- A test for accumulating parameter
    --           -- f (x, y) /\ f (z, y) -> f (m, n) /\ f (z, C (n))
    --           if length goal == 1
    --           then
    --             case findInstance goal seen of
    --               Just v ->
    --                 return $ trace "Is instance!" $ traceShow v $ Leaf goal state env v
    --               Nothing -> do
    --                 let (allFree, generalizer, env') = generalizeAllVarsToFree goal env
    --                 put (ConsPDState seen' failed env')
    --                 ch <- go (addAnc allFree) state
    --                 return $ trace "Is gen!" $ Gen ch goal allFree generalizer state
    --           else do
    --             put (ConsPDState seen' failed env)
    --             let unified = zip (map (:[]) goal) (repeat state)
    --             children <- unfoldSequentially unified addAnc
    --             split children goal state
    --         Nothing ->
    --           if length goal == 1
    --           then do
    --             put (ConsPDState seen' failed env)
    --             unified <- adaptState (oneStep (head goal) state)
    --             -- let (unified, env') = runState (oneStep (head goal) state) env
    --             -- put (seen', failed, env')
    --             ch <- trace "unified: " $ traceShow unified $ trace "ancs" $ traceShow ancs $ unfoldSequentially unified addAnc
    --             or (trace "4" $ ch) (addAnc goal) state
    --           else
    --             case findTupling env state goal ancs of
    --               Just (unfolded, newEnv) -> do
    --                 put (ConsPDState seen' failed newEnv)
    --                 ch <- unfoldSequentially unfolded addAnc
    --                 or (trace "3" $ ch) (addAnc goal) state
    --               Nothing ->
    --                 case findBestByComplexity env state goal of
    --                   -- Either ls or rs is not empty!
    --                   Just zipper ->
    --                     let ls = left zipper in
    --                     let x  = cursor zipper in
    --                     let rs = right zipper in
    --                     case if isGoalStatic env x then Nothing else findVariant [x] seen' of
    --                       Just v -> do
    --                         modifySeen (const seen')
    --                         -- let x' = Conj [Leaf [x] state env v] [x] state in
    --                         let x' = Leaf [x] state env v
    --                         ls' <- unfoldNotNull ls state addAnc
    --                         rs' <- unfoldNotNull rs state addAnc
    --                         split (catMaybes [ls', Just x', rs']) goal state
    --                       Nothing -> do
    --                         -- let (unified, env') = runState (oneStep x state) env
    --                         -- put ([x]:seen', failed, env')
    --                         unified <- adaptState (oneStep x state)
    --                         -- modify (\(seen', f, e) -> ([x]:seen', f, e))
    --                         -- modifySeen ([x]:) -- WHY???
    --                         modifySeen (const seen')
    --                         ch <- unfoldSequentially unified addAnc
    --                         x' <- or (trace "2" $ ch) (addAnc [x]) state
    --                         case computedAnswers x' of
    --                           Just xs ->
    --                             if all (\(gs, _, _) -> null gs || instanceCheck gs seen) xs
    --                             then do
    --                               -- WHAT IF IT RENAMES WITHIN THIS SUBTREE???
    --                               children <- mapM (\(goals, subst, newEnv) -> do
    --                                   let toUnfold = wrap ls rs goals
    --                                   if null toUnfold
    --                                   then
    --                                     return $ leaf subst newEnv
    --                                   else do
    --                                     modifyEnv (const newEnv)
    --                                     go (addAnc toUnfold) subst
    --                                 ) xs
    --                               or (trace "1" $ trace "computed" $ traceShow xs $ reverse children) (addAnc $ wrap ls rs [x]) state
    --                             else do
    --                               ls' <- unfoldNotNull ls state addAnc
    --                               rs' <- unfoldNotNull rs state addAnc
    --                               split (catMaybes [ls', Just x', rs']) goal state
    --                           Nothing -> do
    --                             ls' <- unfoldNotNull ls state addAnc
    --                             rs' <- unfoldNotNull rs state addAnc
    --                             split (catMaybes [ls', Just x', rs']) goal state
    --                   Nothing -> do
    --                     put (ConsPDState seen' failed env)
    --                     children <- unfoldSequentially (zip (map (:[]) goal) (repeat state)) addAnc
    --                     split children goal state
    wrap left right x = left ++ x ++ right

    adaptState :: State Env.Env a -> State ConsPDState a
    adaptState state = do
      (ConsPDState _ _ e) <- get
      let (r, env) = runState state e
      modifyEnv (const env)
      return r

    unfoldNotNull :: [G S] -> Subst.Subst S -> ([G S] -> Descend [G S]) -> State ConsPDState (Maybe ConsPDTree)
    unfoldNotNull xs _ _ | null xs = return Nothing
    unfoldNotNull xs state addAnc = do
      children <- unfoldSequentially (zip (map (:[]) xs) (repeat state)) addAnc
      n <- split children xs state
      return $ Just n

    unfoldSequentially :: [([G S], Subst.Subst S)] -> ([G S] -> Descend [G S]) -> State ConsPDState [ConsPDTree]
    unfoldSequentially unified addAnc = do
      (ConsPDState _ _ env) <- get
      mapM  (\(goals, subst) ->
                if null goals
                then return $ leaf subst env
                else go (addAnc goals) subst
            ) unified

computedAnswers :: ConsPDTree -> Maybe [([G S], Subst.Subst S, Env.Env)]
computedAnswers (Success s e) = Just [([], s, e)]
computedAnswers Fail = Just []
computedAnswers (Leaf g s e _) = Just [(g, s, e)]
computedAnswers (Or ch _ _) =
  let xs = map computedAnswers ch in
  if any isNothing xs
  then Nothing
  else Just $ concatMap fromJust xs
computedAnswers _ = Nothing

-- Finds a conjunct, for which at least one substitution exists in the unfolding.
-- The result is the pair in which first element is list of conjuncts excluding the one which generates substitutions.
-- The second element is the unfolding result for the selected conjunct.
tryFindSubsts :: [G S] -> Env.Env -> Subst.Subst S -> Maybe ([G S], (([Subst.Subst S], [([G S], Subst.Subst S)]), Env.Env), [G S])
tryFindSubsts =
    -- TODO USE PINPOINT
    go []
  where
    go _ [] _ _ = Nothing
    go left (g:gs) env state =
      let (unfolded, env') = doStep g env state in
      let (substs, notSubsts) = partition (null . fst) unfolded in
      if null substs
      then go (g:left) gs env state
      else Just (reverse left, ((map snd substs, notSubsts), env'), gs)

doStep :: G S -> Env.Env -> Subst.Subst S -> ([([G S], Subst.Subst S)], Env.Env)
doStep goal env state =
    fromMaybe
      (runState (oneStep goal state) env)
      (incrDeepOneStep globalLimit 0 goal env state)

isFail :: ConsPDTree -> Bool
isFail Fail = True
isFail _ = False

or :: [ConsPDTree] -> Descend [G S] -> Subst.Subst S -> State ConsPDState ConsPDTree
or ch d@(Descend gs _) state =
  if null ch || all isFail ch
  then do
    modifySeen (Set.delete gs)
    modifyFailed (Set.insert gs)
    -- return $ trace "Is Or Fail!" $ Fail
    return $ Or [Fail] d state 
  else return $  trace "Is Or!" $ Or ch d state


split :: [ConsPDTree] -> [G S] -> Subst.Subst S -> State ConsPDState ConsPDTree
split [x] goal state = return x
split ch goal state = return $ trace "Is split!" $ Split ch goal state

checkConflicts :: [Subst.Subst S] -> Bool
checkConflicts sigmas =
    let conflicting = findConflicting sigmas in
    any (\x -> length x /= 1) conflicting

collectSubsts :: ConsPDTree -> [Subst.Subst S]
collectSubsts (Or ch _ _) =
    mapMaybe go ch
  where
    go Fail            = Nothing
    go (Success s _)   = Just s
    go (Or _ _ s)      = Just s
    go (Conj _ _ s)    = Just s
    go (Gen _ _ _ _ s) = Just s
    go (Leaf _ s _ _)  = Just s
collectSubsts (Leaf _ s _ _) = [s]
collectSubsts x = []

isConflicting :: Subst.Subst S -> Subst.Subst S -> Bool
isConflicting s1 s2 =
    let m1 = s1 in
    let m2 = s2 in
    let intersection = Subst.intersectionWith (\x y -> (E.walk s1 x, E.walk s2 y)) m1 m2 in
    M.size intersection > 0 &&
    any (uncurry conflicting) intersection
  where
    conflicting (C x xs) (C y ys) = x /= y || length xs /= length ys || any (uncurry conflicting) (zip xs ys)
    conflicting _ _ = False

findConflicting :: [Subst.Subst S] -> [[Subst.Subst S]]
findConflicting [] = []
findConflicting [x] = [[x]]
findConflicting (x:xs) =
    go [x] [] xs
  where
    go [] [] [] = []
    go [] conf [] = [reverse conf]
    go [] conf unConf = reverse conf : findConflicting unConf
    go (x:xs) conf unConf =
      let (conf', unConf') = partition (isConflicting x) unConf in
      go (xs ++ conf') (x : conf) unConf'

simplify :: ConsPDTree -> ConsPDTree
simplify tree =
    removeTransient $ go tree
  where
    removeTransient tree =
        replaceChildren (go <$> getChildren tree) tree
      where
        go (Or [Or ch g' s'] g s) = go $ Or ch g s
        go (Or ch g s) = Or (go <$> ch) g s
        go (Split ch g s) = Split (go <$> ch) g s
        go (Conj ch g s) = Conj (go <$> ch) g s
        go x = x

    getChildren :: ConsPDTree -> [ConsPDTree]
    getChildren (Or    ch g s) = ch
    getChildren (Conj  ch g s) = ch
    getChildren (Split ch g s) = ch
    getChildren x = []

    replaceChildren new (Or    ch g s) = Or new g s
    replaceChildren new (Conj  ch g s) = Conj new g s
    replaceChildren new (Split ch g s) = Split new g s
    replaceChildren new x = x

    go (Or    ch g s)   = failOr ch (\x -> Or x g s)
    go (Conj  ch g s)   = failConj ch (\x -> Conj x g s)
    go (Split ch g s)   = failConj ch (\x -> Split x g s)
    go (Gen   ch g g' gen s) = failOr [ch] (\[x] -> Gen x g g' gen s)
    go x                = x
    failOr ch f =
      let simplified = filter (not . isFail) $ map go ch in
      if null simplified
      then Fail
      else f simplified
    failConj ch f =
      let simplified = map go ch in
      if Fail `elem` simplified
      then Fail
      else f simplified

noPrune :: ConsPDTree -> Bool
noPrune (Prune _ _) = False
noPrune (Or ch _ _) = all noPrune ch
noPrune (Conj ch _ _) = all noPrune ch
noPrune (Split ch _ _) = all noPrune ch
noPrune _ = True