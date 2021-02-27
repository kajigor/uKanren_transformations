{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module ConsPD.Unfold where

import           Control.Monad.State
import           Data.Foldable      (foldlM)
import           Data.List          (partition, delete)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (catMaybes, fromJust, fromMaybe,
                                     isNothing, mapMaybe)
import           Descend
import           Embed
import qualified Eval               as E
import qualified FreshNames         as FN
import           Generalization     (Generalizer, generalizeAllVarsToFree)
import           Prelude            hiding (or)
import qualified Subst
import           Syntax
import           Unfold             (findBestByComplexity, oneStep, isGoalStatic)
import           Util.ListZipper
import qualified Environment as Env

data ConsPDTree = Fail
                | Success Subst.Subst Env.Env
                | Or [ConsPDTree] (Descend [G S]) Subst.Subst
                | Conj [ConsPDTree] [G S] Subst.Subst
                | Gen ConsPDTree [G S] [G S] Generalizer Subst.Subst
                | Leaf [G S] Subst.Subst Env.Env [G S] -- last argument is a goal renaming of which current node is
                | Split [ConsPDTree] [G S] Subst.Subst
                | Prune [G S] Subst.Subst
                deriving (Show, Eq)


-- conjConcat :: [([([G S], E.Sigma)], Env.Env)] -> Maybe ([([G S], E.Sigma)], Env.Env)
-- conjConcat x = do
--     let (goals, envs) = unzip x
--     cGoals <- mapM goalsConcat goals
--     cGammas <- gammaConcat envs
--     return (cGoals, cGammas)
--   where
--     gammaConcat xs =
--       foldlM (\(p, i, d) (_, i', d') -> do
--                   i'' <- mergeI i i'
--                   return (p, i'', mergeD d d'))
--              (head xs)
--              (tail xs)
--     mergeD (x:xs) (y:ys) = if x >= y then x : xs else y : ys
--     mergeI (d, f) i@(d', f') =
--       let dd' = d \\ d' in
--       let intersection = d `intersect` d' in
--       if map f intersection /= map f' intersection
--       then Nothing
--       else Just $ foldl (\interp x -> VI.extend interp x (f x)) i dd'

statesConcat :: [Subst.Subst] -> Maybe Subst.Subst
statesConcat = unifySubsts

goalsConcat :: [([a], Subst.Subst)] -> Maybe ([a], Subst.Subst)
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


incrDeepOneStep :: Int -> Int -> G S -> Env.Env -> Subst.Subst -> Maybe ([([G S], Subst.Subst)], Env.Env)
incrDeepOneStep limit depth _ _ _ | limit < depth = Nothing
incrDeepOneStep limit depth goal env state =
    let (unified, env') = oneStep goal env state in
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

leaf :: Subst.Subst -> Env.Env -> ConsPDTree
leaf s _ | Subst.null s = Fail
leaf s e = Success s e

unifySubsts :: [Subst.Subst] -> Maybe Subst.Subst
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
conjToList (g :/\: h) = conjToList g ++ conjToList h
conjToList x          = [x]

globalLimit :: Int
globalLimit = 8

justUnfold :: Int -> Program -> (ConsPDTree, G S, [S])
justUnfold limit (Program defs goal) =
    let env = Env.fromDefs defs in
    let (logicGoal, env', names) = E.preEval env goal in
    (go 0 (Descend (conjToList logicGoal) []) env' Subst.empty, logicGoal, names)
  where
    go n (Descend gs ancs) env subst | n > limit || length gs > 1 =
      case findVariant gs ancs of
        Nothing -> Prune gs subst
        Just v -> Leaf gs subst env v
    go n d@(Descend gs ancs) env subst =
      let [goal] = Subst.substitute subst gs in
      let addDescend g = Descend g ([goal] : ancs) in
      let (unified, env') = oneStep goal env subst in
      let children = map (\(gs, s) -> if null gs
                                      then Success s env'
                                      else go (n+1) (addDescend gs) env' s
                         ) unified in
      Or children d subst

topLevel :: Int -> Program -> (ConsPDTree, G S, [S])
topLevel limit (Program defs goal) =
    let env = Env.fromDefs defs in
    let (logicGoal, env', names) = E.preEval env goal in
    let seen = [] in
    let failed = [] in
    let descend = Descend (conjToList logicGoal) [] in
    let tree = evalState (go descend Subst.empty) (seen, failed, env') in
    (tree, logicGoal, names)
  where
    go :: Descend [G S] -> Subst.Subst  -> State ([[G S]], [[G S]], Env.Env) ConsPDTree
    go (Descend goal' ancs') state = do
      (seen, failed, env :: Env.Env) <- get
      let (hd, _) = FN.getFreshName (Env.getFreshNames env)
      let goal = Subst.substitute state goal'
      let seen' = goal : seen
      let addAnc x = Descend x (goal : ancs')
      if variantCheck goal failed
      then
        return Fail
      else
        if limit > 0 && hd > limit
        then
          return $ Prune goal state
        else
          case if isGround goal then Nothing else findVariant goal seen of
            Just v ->
              return $ Leaf goal state env v
            Nothing | any (`embed` goal) ancs' ->
              -- A test for accumulating parameter
              -- f (x, y) /\ f (z, y) -> f (m, n) /\ f (z, C (n))
              if length goal == 1
              then
                case findInstance goal seen of
                  Just v ->
                    return $ Leaf goal state env v
                  Nothing -> do
                    let (allFree, generalizer, env') = generalizeAllVarsToFree goal env
                    put (seen', failed, env')
                    ch <- go (addAnc allFree) state
                    return $ Gen ch goal allFree generalizer state
              else do
                put (seen', failed, env)
                let unified = zip (map (:[]) goal) (repeat state)
                children <- unfoldSequentially unified addAnc
                split children goal state
            Nothing ->
              if length goal == 1
              then do
                let (unified, env') = oneStep (head goal) env state
                ch <- unfoldSequentially unified addAnc
                or ch (addAnc goal) state
              else
                case findBestByComplexity env state goal of
                  -- Either ls or rs is not empty!
                  Just zipper ->
                    let ls = left zipper in
                    let x  = cursor zipper in
                    let rs = right zipper in
                    case if isGoalStatic env x then Nothing else findVariant [x] seen' of
                      Just v -> do
                        -- let x' = Conj [Leaf [x] state env v] [x] state in
                        let x' = Leaf [x] state env v
                        ls' <- unfoldNotNull ls state addAnc
                        rs' <- unfoldNotNull rs state addAnc
                        split (catMaybes [ls', Just x', rs']) goal state
                      Nothing -> do
                        let (unified, env') = oneStep x env state
                        put ([x]:seen', failed, env')
                        ch <- unfoldSequentially unified addAnc
                        x' <- or ch (addAnc [x]) state
                        case computedAnswers x' of
                          Just xs ->
                            if all (\(gs, _, _) -> null gs || instanceCheck gs seen) xs
                            then do
                              -- WHAT IF IT RENAMES WITHIN THIS SUBTREE???
                              children <- mapM (\(goals, subst, newEnv) -> do
                                  (seen, failed, env) <- get
                                  let toUnfold = wrap ls rs goals
                                  if null toUnfold
                                  then return $ leaf subst newEnv
                                  else do
                                    put (seen, failed, newEnv)
                                    go (addAnc toUnfold) subst
                                ) xs
                              or (reverse children) (addAnc $ wrap ls rs [x]) state
                            else do
                              ls' <- unfoldNotNull ls state addAnc
                              rs' <- unfoldNotNull rs state addAnc
                              split (catMaybes [ls', Just x', rs']) goal state
                          Nothing -> do
                            ls' <- unfoldNotNull ls state addAnc
                            rs' <- unfoldNotNull rs state addAnc
                            split (catMaybes [ls', Just x', rs']) goal state
                  Nothing -> do
                    put (seen', failed, env)
                    children <- unfoldSequentially (zip (map (:[]) goal) (repeat state)) addAnc
                    split children goal state
    wrap left right x = left ++ x ++ right

    nullGoals goals subst env _ | null goals = leaf subst env
    nullGoals _ _ _ v = v

    unfoldNotNull :: [G S] -> Subst.Subst -> ([G S] -> Descend [G S]) -> State ([[G S]], [[G S]], Env.Env) (Maybe ConsPDTree)
    unfoldNotNull xs _ _ | null xs = return Nothing
    unfoldNotNull xs state addAnc = do
      children <- unfoldSequentially (zip (map (:[]) xs) (repeat state)) addAnc
      n <- split children xs state
      return $ Just n

    unfoldSequentially :: [([G S], Subst.Subst)] -> ([G S] -> Descend [G S]) -> State ([[G S]], [[G S]], Env.Env) [ConsPDTree]
    unfoldSequentially unified addAnc = do
      (_, _, env) <- get
      mapM  (\(goals, subst) ->
                if null goals
                then return $ leaf subst env
                else go (addAnc goals) subst
            ) unified

    -- merge :: Env.Env -> Env.Env -> Env.Env
    -- merge (Env.Env _ _ d) env' =
    --   if d > Env.getFreshNames env'
    --   then Env.updateNames env' d
    --   else env'


-- topLevel :: Int -> Program -> (ConsPDTree, G S, [S])
-- topLevel limit (Program defs goal) =
--     let env = Env.fromDefs defs in
--     let (logicGoal, env', names) = E.preEval env goal in
--     let nodes = [] in
--     let failed = [] in
--     let descend = Descend (conjToList logicGoal) [] in
--     (fst4 $ go descend env' nodes Subst.empty failed, logicGoal, names)
--   where
--     go :: Descend [G S] -> Env.Env -> [[G S]] -> Subst.Subst -> [[G S]] -> (ConsPDTree, [[G S]], [[G S]], Env.Env)
--     go (Descend goal' ancs') env seen state failed =
--      let (hd, _) = FN.getFreshName (Env.getFreshNames env) in
--      let goal = Subst.substitute state goal' in
--      let seen' = goal : seen in
--      let addAnc x = Descend x (goal : ancs') in
--     --  if goal `elem` failed
--      if variantCheck goal failed
--      then
--        (Fail, seen, failed, env)
--      else
--       if limit > 0 && hd > limit
--       then
--         (Prune goal state, seen, failed, env)
--       else
--         -- (\(x, y, z) -> (simplify x, y, z)) $
--           case if isGround goal then Nothing else findVariant goal seen of
--             Just v ->
--               -- trace (printf "go\nGoal:\n%s\nSeen:\n%s\n" (show goal) (show' seen)) $
--               (Leaf goal state env v, seen, failed, env)
--             _ ->
--               -- A test for accumulating parameter
--               -- f (x, y) /\ f (z, y) -> f (m, n) /\ f (z, C (n))
--               if any (\x -> {-length x == length goal &&-} x `embed` goal) ancs'
--               then
--                 if length goal == 1
--                 then
--                   case findInstance goal seen of
--                     Just v -> (Leaf goal state env v, seen, failed, env)
--                     Nothing ->
--                       -- (Prune goal state, seen, failed, env)
--                       let (allFree, generalizer, env') = generalizeAllVarsToFree goal env in
--                       let (ch, newSeen, newFailed, newEnv) = go (addAnc allFree) env' seen' state failed in
--                       (Gen ch goal allFree generalizer state, newSeen, newFailed, newEnv)
--                 else
--                   let (children, allSeenGoals, failed', env') = unfoldSequentially failed seen' (zip (map (:[]) goal) (repeat state)) env addAnc in
--                   split children goal state allSeenGoals failed' env'
--               else
--                 if length goal == 1
--                 then
--                   let (unified, env') = oneStep (head goal) env state in
--                   let (ch, allSeenGoals, failed', env'') = unfoldSequentially failed seen' unified env' addAnc in
--                   or ch (addAnc goal) state allSeenGoals failed' env''
--                 else
--                   case findBestByComplexity env state goal of
--                     -- Either ls or rs is not empty!
--                     Just zipper ->
--                       let ls = left zipper in
--                       let x = cursor zipper in
--                       let rs = right zipper in
--                       case if isGoalStatic env x then Nothing else findVariant [x] seen' of
--                         Just v ->
--                           -- let x' = Conj [Leaf [x] state env v] [x] state in
--                           let x' = Leaf [x] state env v in
--                           let (ls', seen'', failed'', env')  = unfoldNotNull failed ls seen'  state env addAnc in
--                           let (rs', seen''', failed''', env'') = unfoldNotNull failed'' rs seen'' state env' addAnc in
--                           split (catMaybes [ls', Just x', rs']) goal state seen''' failed''' env''
--                         Nothing ->
--                           let (unified, env') = oneStep x env state in
--                           let (ch, allSeenGoals', failed_', env'') = unfoldSequentially failed ([x]:seen') unified env' addAnc in
--                           let (x', allSeenGoals, failed', env''') = or ch (addAnc [x]) state allSeenGoals' failed_' env'' in
--                           case computedAnswers x' of
--                             Just xs ->
--                               if all (\(gs, _, _) -> null gs || instanceCheck gs seen) xs
--                               then
--                                 -- WHAT IF IT RENAMES WITHIN THIS SUBTREE???
--                                 let (children, allSeenGoals, failed'', updatedEnv) =
--                                       foldl (\(ys, seenGoals, failedGoals, actualEnv) (goals, subst, newEnv) ->
--                                                 let toUnfold = wrap ls rs goals in
--                                                 let (node, newSeen, newFailed, eNv) =
--                                                       if null toUnfold
--                                                       then (leaf subst newEnv, seenGoals, failedGoals, newEnv)
--                                                       else
--                                                         go (addAnc toUnfold) (merge actualEnv newEnv) seenGoals subst failedGoals in
--                                                 (nullGoals toUnfold subst newEnv node : ys, newSeen, newFailed, eNv)
--                                             )
--                                             ([], seen', failed', env''')
--                                             xs in
--                                 or (reverse children) (addAnc $ wrap ls rs [x]) state allSeenGoals failed'' updatedEnv
--                               else
--                                 let (ls', seen'', failed'', env4)  = unfoldNotNull failed' ls allSeenGoals state env''' addAnc in
--                                 let (rs', seen''', failed''', env5) = unfoldNotNull failed'' rs seen'' state env4 addAnc in
--                                 split (catMaybes [ls', Just x', rs']) goal state seen''' failed''' env5
--                             Nothing ->
--                               let (ls', seen'', failed'', env4)  = unfoldNotNull failed' ls allSeenGoals state env''' addAnc in
--                               let (rs', seen''', failed''', env5) = unfoldNotNull failed'' rs seen'' state env4 addAnc in
--                               split (catMaybes [ls', Just x', rs']) goal state seen''' failed''' env5
--                     Nothing ->
--                       let (children, allSeenGoals, failed'', env') = unfoldSequentially failed seen' (zip (map (:[]) goal) (repeat state)) env addAnc in
--                       split children goal state allSeenGoals failed'' env'
--     wrap left right x = left ++ x ++ right

--     nullGoals goals subst env _ | null goals = leaf subst env
--     nullGoals _ _ _ v = v

--     unfoldNotNull failed xs seen _ env _ | null xs = (Nothing, seen, failed, env)
--     unfoldNotNull failed xs seen state env addAnc =
--       let (children, seen', failed', env') = unfoldSequentially failed seen (zip (map (:[]) xs) (repeat state)) env addAnc in
--       let (n, x, y, z) = split children xs state seen' failed' env' in
--       (Just n, x, y, z)
--         -- (Just $ Split children xs state, seen', failed')

--     goNotNull xs seen _ env _ failed | null xs = (Nothing, seen, failed, env)
--     goNotNull xs seen state env addAnc failed =
--       let (node, seen', failed', env') = go (addAnc xs) env seen state failed in
--       (Just node, seen', failed', env')

--     unfoldSequentially failed seen unified env addAnc =
--       let (ch, allSeenGoals, actualFailed, actualEnv) =
--                       foldl (\(xs, seenGoals, failedGoals, env') (goals, subst) ->
--                                 let (node, newSeen, newFailed, newEnv) =
--                                       if null goals
--                                       then (leaf subst env, seenGoals, failedGoals, env')
--                                       else go (addAnc goals) env' seenGoals subst failedGoals in
--                                 (nullGoals goals subst env node : xs, newSeen, newFailed, newEnv)
--                             )
--                             ([], seen, failed, env)
--                             unified in
--       (reverse ch, allSeenGoals, actualFailed, actualEnv)

--     merge :: Env.Env -> Env.Env -> Env.Env
--     merge (Env.Env _ _ d) env' =
--       if d > Env.getFreshNames env'
--       then Env.updateNames env' d
--       else env'

createLeafNode :: [[G S]] -> ([G S], Subst.Subst, Env.Env) -> ConsPDTree
createLeafNode seen = go
  where
    go ([], state, env) = leaf state env
    go (gs, state, env) =
      case findVariant gs seen of
        Just v -> Leaf  gs state env v
        Nothing -> Prune gs state
-- limitSubsts :: [([G S], E.Sigma, Env.Env)] -> E.Sigma -> [([G S], E.Sigma, Env.Env)]
-- limitSubsts xs state =
--     let varsToLeave = nub $ map fst state ++ concatMap (fv . snd) state in
--     map (\(gs, st, env) -> (gs, go varsToLeave gs st, env)) xs
--   where
--     go vars gs st =
--       let varsToLeave = nub $ vars ++ concatMap fvgs gs in
--       filter (\(v,t) -> any (`elem` varsToLeave) (v : fv t)) st


computedAnswers :: ConsPDTree -> Maybe [([G S], Subst.Subst, Env.Env)]
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
tryFindSubsts :: [G S] -> Env.Env -> Subst.Subst -> Maybe ([G S], (([Subst.Subst], [([G S], Subst.Subst)]), Env.Env), [G S])
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

doStep :: G S -> Env.Env -> Subst.Subst -> ([([G S], Subst.Subst)], Env.Env)
doStep goal env state =
    fromMaybe
      (oneStep goal env state)
      (incrDeepOneStep globalLimit 0 goal env state)

isFail :: ConsPDTree -> Bool
isFail Fail = True
isFail _ = False

or :: [ConsPDTree] -> Descend [G S] -> Subst.Subst -> State ([[G S]], [[G S]], Env.Env) ConsPDTree
or ch d@(Descend gs _) state =
  if null ch || all isFail ch
  then do
    (seen, failed, env) <- get
    put (delete gs seen, gs : failed, env)
    return Fail
  else return $ Or ch d state


split :: [ConsPDTree] -> [G S] -> Subst.Subst -> State ([[G S]], [[G S]], Env.Env) ConsPDTree
split [x] goal state = return x
split ch goal state = return (Split ch goal state)

checkConflicts :: [Subst.Subst] -> Bool
checkConflicts sigmas =
    let conflicting = findConflicting sigmas in
    any (\x -> length x /= 1) conflicting

collectSubsts :: ConsPDTree -> [Subst.Subst]
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

isConflicting :: Subst.Subst -> Subst.Subst -> Bool
isConflicting s1 s2 =
    let m1 = s1 in
    let m2 = s2 in
    let intersection = Subst.intersectionWith (\x y -> (E.walk s1 x, E.walk s2 y)) m1 m2 in
    M.size intersection > 0 &&
    any (uncurry conflicting) intersection
  where
    conflicting (C x xs) (C y ys) = x /= y || length xs /= length ys || any (uncurry conflicting) (zip xs ys)
    conflicting _ _ = False

findConflicting :: [Subst.Subst] -> [[Subst.Subst]]
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
  --   -- trace (printf "simplifying\n%s\n" (show tree)) $
  --   keepRoot tree
  -- where
  --   keepRoot (Or ch g s) =
  --     case failOr ch (\x -> Or x g s) of
  --       Fail -> Or [Fail] g s
  --       x -> x
  --   keepRoot (Conj ch g s) =
  --     case failConj ch (\x -> Conj x g s) of
  --       Fail -> Conj [Fail] g s
  --       x -> x
  --   keepRoot (Split ch g s) =
  --     case failConj ch (\x -> Split ch g s) of
  --       Fail -> Split [Fail] g s
  --       x -> x
  --   keepRoot x = go x

    removeTransient tree =
        replaceChildren (go <$> getChildren tree) tree
      where
        go (Or [Or ch g' s'] g s) = go $ Or ch g s
        go (Or ch g s) = Or (go <$> ch) g s
        go (Split ch g s) = Split (go <$> ch) g s
        go (Conj ch g s) = Conj (go <$> ch) g s
        go x = x

    -- removeTransient tree =
    --     replaceChildren (go <$> getChildren tree) tree
    --   where
    --     go tree =
    --       let children = untilMany tree in
    --       -- trace (printf "\nBefore: %s\nChildren:\n%s\n" (show $ getGs tree) (show' $ getGs <$> children)) $
    --       if length children == 1
    --       then head children
    --       else replaceChildren (go <$> children) tree

    untilMany :: ConsPDTree -> [ConsPDTree]
    untilMany tree =
      let children = getChildren tree in
      case length children of
        0 -> [tree]
        1 -> untilMany $ head children
        n -> children

    getChildren :: ConsPDTree -> [ConsPDTree]
    getChildren (Or    ch g s) = ch
    getChildren (Conj  ch g s) = ch
    getChildren (Split ch g s) = ch
    getChildren x = []


    getGs :: ConsPDTree -> [G S]
    getGs (Or    ch (Descend g _) s) = g
    getGs (Conj  ch g s) = g
    getGs (Split ch g s) = g
    getGs x = []

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