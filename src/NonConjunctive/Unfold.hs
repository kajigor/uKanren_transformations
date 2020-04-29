{-# LANGUAGE ScopedTypeVariables #-}

module NonConjunctive.Unfold where

import qualified CPD.LocalControl   as LC
import           Data.Foldable      (foldlM)
import           Data.List          (find, intersect, partition, (\\), sortBy)
import qualified Data.Map.Strict    as M
import           Data.Maybe         (mapMaybe, fromMaybe, isJust)
import           Debug.Trace        (trace)
import           Embed
import qualified Eval               as E
import           Generalization     (generalizeSplit, Generalizer)
import           Prelude            hiding (or)
import           Syntax
import           Text.Printf        (printf)
import           Unfold             (oneStepUnfold, oneStep, notMaximumBranches, unfoldComplexity, findBestByComplexity)
import           Util.Miscellaneous (fst3, show')

data NCTree = Fail
            | Success E.Sigma
            | Or [NCTree] (LC.Descend [G S]) E.Sigma
            | Conj [NCTree] [G S] E.Sigma
            | Gen NCTree [G S] Generalizer
            | Leaf [G S] E.Sigma
            | Split [NCTree] [G S] E.Sigma
            | Prune [G S] E.Sigma
            deriving (Show, Eq)

conjConcat :: [([([G S], E.Sigma)], E.Gamma)] -> Maybe ([([G S], E.Sigma)], E.Gamma)
conjConcat x = do
    let (goals, envs) = unzip x
    cGoals <- mapM goalsConcat goals
    cGammas <- gammaConcat envs
    return (cGoals, cGammas)
  where
    gammaConcat xs =
      foldlM (\(p, i, d) (_, i', d') -> do
                  i'' <- mergeI i i'
                  return (p, i'', mergeD d d'))
             (head xs)
             (tail xs)
    mergeD (x:xs) (y:ys) = if x >= y then x : xs else y : ys
    mergeI (d, f) i@(d', f') =
      let dd' = d \\ d' in
      let intersection = d `intersect` d' in
      if map f intersection /= map f' intersection
      then Nothing
      else Just $ foldl (\interp x -> E.extend interp x (f x)) i dd'

statesConcat :: [E.Sigma] -> Maybe E.Sigma
statesConcat = unifySubsts

goalsConcat :: [([a], E.Sigma)] -> Maybe ([a], E.Sigma)
goalsConcat x = do
  let (goals, states) = unzip x
  cStates <- statesConcat states
  return (reverse $ concat goals, cStates)

productList :: [[a]] -> [[a]]
productList []       = [[]]
productList (xs:xss) = [ h : t | h <- xs, t <- productList xss]

incrDeepOneStep :: Int -> Int -> G S -> E.Gamma -> E.Sigma -> Maybe ([([G S], E.Sigma)], E.Gamma)
incrDeepOneStep limit depth _ _ _ | limit < depth = Nothing
incrDeepOneStep limit depth goal env state =
    let (unified, gamma) = oneStep goal env state in
    if all (not . null . fst) unified
    then do
      (goals, newEnv) <-
        foldlM
          (\(acc, env) (gs, sigma) -> do
            (x, env') <-
              foldlM
                (\(acc, env) g -> do
                    (x, env') <- incrDeepOneStep limit (depth + 1) g env sigma
                    return (x : acc, env')
                )
                ([], env)
                gs
            return (acc ++ x, env')
          )
          ([], gamma)
          unified
      return (mapMaybe goalsConcat $ productList goals, newEnv)
    else
      return (unified, gamma)

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

leaf :: E.Sigma -> NCTree
leaf [] = Fail
leaf s  = Success s

unifySubsts :: [E.Sigma] -> Maybe E.Sigma
unifySubsts [] = return []
unifySubsts [s] = return s
unifySubsts (x : y : xs) = do
    s <- go x y
    unifySubsts (s : xs)
  where
    go s [] = Just s
    go s ((v, t) : rest) = do
      s' <- E.unify (Just s) (V v) t
      go s' rest

conjToList :: G a -> [G a]
conjToList (g :/\: h) = conjToList g ++ conjToList h
conjToList x          = [x]

conj :: [G a] -> G a
conj (a:as) = foldl (:/\:) a as

globalLimit :: Int
globalLimit = 8

nonConjunctive :: Int -> Program -> (NCTree, G S, [S])
nonConjunctive limit (Program defs goal) =
    let gamma = E.updateDefsInGamma E.env0 defs in
    let (logicGoal, gamma', names) = E.preEval gamma goal in
    let nodes = [] in
    let descend = LC.Descend (conjToList logicGoal) [] in
    (go descend gamma' nodes E.s0, V 1 === V 2, [4, 5, 6, 7])
  where
    go :: LC.Descend [G S] -> E.Gamma -> [[G S]] -> E.Sigma -> NCTree
    go (LC.Descend goal' ancs') env@(x,y,z) seen state =
      let goal = E.substitute state goal' in
      let seen' = goal : seen in
      let addAnc x = LC.Descend x (goal : ancs') in
      let d = addAnc goal' in
      if limit > 0 && head z > limit
      then
        Prune goal state
      else
       simplify $
        if variantCheck goal seen
        then
          Leaf goal state
        else
          case find (`embed` goal) ancs' of
            Just g ->
              let (newGoals, everythingElse, gen1, gen2, names) =
                    generalizeSplit z g goal in

              let env' = (x, y, names) in

              let firstChild =
                    if newGoals `isInst` goal
                    then
                      case findBestByComplexity env' state goal of
                        Just (ls, g, rs) ->
                          let (unified, gamma) = oneStep g env' state in
                          let children =
                                map (\(goals, subst) ->
                                        let goals' = wrap ls rs goals in
                                        nullGoals goals' subst $
                                          go (addAnc goals') gamma seen' subst
                                    )
                                    unified in
                          or children d state
                    else
                      -- trace (printf "\nGeneralization\nGoal: %s\nAnc: %s\nGeneralization: %s\nEverythingElse: %s\n" (show goal) (show g) (show newGoals) (show everythingElse)) $
                      let ch = go (LC.Descend newGoals ancs') env' seen' state in
                      if null gen2
                      then ch
                      else Gen ch newGoals gen2
              in
              if null everythingElse
              then firstChild
              else
                let secondChild = go (LC.Descend everythingElse ancs') env' seen' state in
                Split [firstChild, secondChild] (newGoals ++ everythingElse) state
            Nothing ->
              case if length goal == 1
                   then Just ([], head goal, [])
                   else findBestByComplexity env state goal of
                Just (ls, g, rs) ->
                  let (unified, gamma) = oneStep g env state in
                  let children =
                        map (\(goals, subst) ->
                                let goals' = wrap ls rs goals in
                                nullGoals goals' subst $
                                  go (addAnc goals') gamma seen' subst
                            )
                            unified in
                  or children d state
                Nothing ->
                  -- Prune goal state
                  let ch = map (\g -> go (addAnc [g]) env seen' state) goal in
                  Split ch goal state
    wrap left right x = left ++ x ++ right
    nullGoals goals subst v = if null goals then leaf subst else v
      -- variantCheck curGoal (goal : seen) || isJust (find (`embed` curGoal) (goal : ancs))

-- nonConjunctive :: Program -> (NCTree, G S, [S])
-- nonConjunctive (Program defs goal) =
--     let gamma = E.updateDefsInGamma E.env0 defs in
--     let (logicGoal, gamma', names) = E.preEval gamma goal in
--     let nodes = [] in
--     let descend = LC.Descend (conjToList logicGoal) [] in
--     (go descend gamma' nodes E.s0,  V 1 === V 2, [4, 5, 6, 7])
--   where
--     go :: [G S] -> E.Gamma -> [[G S]] -> E.Sigma -> NCTree
--     go d@(LC.Descend goal' ancs) env@(x,y,z) seen state =
--       let goal = E.substitute state goal' in
--       let seen' = goal : seen in
--       let ancs' = goal : ancs in
--       let d = LC.Descend goal ancs in
--       simplify $
--         if variantCheck goal seen
--         then
--           Leaf goal state
--         else
--           case find (`embed` goal) ancs of
--             Just g ->
--               let (newGoals, everythingElse, gen1, gen2, names) =
--                     generalizeSplit z g goal in

--               let env' = (x, y, names) in

--               let firstChild =
--                     if newGoals `isInst` goal
--                     then
--                       let estimated = map (\g -> (g, unfoldComplexity env' state g)) goal in
--                       let selected@(ls,g',rs) = selectMin estimated in
--                       let (left, g, right) = dropEstimation selected in
--                       trace (printf "\nGoal: %s\nNewGoals: %s\nLs: %s\nG': %s\nRs: %s\n" (show goal) (show newGoals) (show ls) (show g') (show rs)) $
--                       let (unified, gamma) = oneStep g env' state in
--                       let children =
--                             map (\(goals, subst) ->
--                                     let goals' = wrap left right goals in
--                                     if shouldStop goals' goal seen ancs
--                                     then Prune goals' state
--                                     else nullGoals (wrap left right goals) subst $
--                                             go (LC.Descend (wrap left right goals) ancs') gamma seen' subst
--                                 )
--                                 unified in
--                       or children d state
--                     else
--                       -- trace (printf "\nGeneralization\nGoal: %s\nAnc: %s\nGeneralization: %s\nEverythingElse: %s\n" (show goal) (show g) (show newGoals) (show everythingElse)) $
--                       let ch = go (LC.Descend newGoals ancs') env' seen' state in
--                       if null gen2
--                       then ch
--                       else Gen ch newGoals gen2
--               in
--               if null everythingElse
--               then firstChild
--               else
--                 let secondChild = go (LC.Descend everythingElse ancs') env' seen' state in
--                 Split [firstChild, secondChild] (newGoals ++ everythingElse) state
--             Nothing ->
--               let estimated = map (\g -> (g, unfoldComplexity env state g)) goal in
--               let selected = selectMin estimated in
--               let (left, g, right) = dropEstimation selected in
--               let (unified, gamma) = oneStep g env state in
--               let children =
--                     map (\(goals, subst) ->
--                             let goals' = wrap left right goals in
--                             if shouldStop goals' goal seen ancs
--                             then Prune goals' state
--                             else
--                               nullGoals (wrap left right goals) subst $
--                                     go (LC.Descend (wrap left right goals) ancs') gamma seen' subst
--                         )
--                         unified in
--               or children d state
--     wrap left right x = left ++ x ++ right
--     dropEstimation (ls, g, rs) = (map fst ls, fst g, map fst rs)
--     nullGoals goals subst v = if null goals then leaf subst else v
--     shouldStop curGoal goal seen ancs = False
--       -- variantCheck curGoal (goal : seen) || isJust (find (`embed` curGoal) (goal : ancs))


-- Finds a conjunct, for which at least one substitution exists in the unfolding.
-- The result is the pair in which first element is list of conjuncts excluding the one which generates substitutions.
-- The second element is the unfolding result for the selected conjunct.
tryFindSubsts :: [G S] -> E.Gamma -> E.Sigma -> Maybe ([G S], (([E.Sigma], [([G S], E.Sigma)]), E.Gamma), [G S])
tryFindSubsts =
    -- TODO USE PINPOINT
    go []
  where
    go _ [] _ _ = Nothing
    go left (g:gs) env state =
      let (unfolded, gamma) = doStep g env state in
      let (substs, notSubsts) = partition (null . fst) unfolded in
      if null substs
      then go (g:left) gs env state
      else Just (reverse left, ((map snd substs, notSubsts), gamma), gs)

selectMin :: (Eq a, Ord b) => [(a, b)] -> ([(a, b)], (a, b), [(a, b)])
selectMin xs =
    let minimal = head $ sortBy (\(x,n) (y,m) -> n `compare` m) xs in
    let (ls, (h:rs)) = span (/= minimal) xs in
    (ls, h, rs)


-- nonConjunctive :: Program -> (NCTree, G S, [S])
-- nonConjunctive (Program defs goal) =
--     let gamma = E.updateDefsInGamma E.env0 defs in
--     let (logicGoal, gamma', names) = E.preEval gamma goal in
--     let nodes = [] in
--     let descend = LC.Descend logicGoal [] in
--     go descend gamma' nodes E.s0
--   where
--     go d@(LC.Descend goal' ancs) env@(x, y, z) seen state =
--     --  trace (printf "\nGo\nGoal:\n%s\n\nSeen\n%s\n\nAncs:\n%s\n" (show goal') (show' seen) (show' ancs)) $
--     --  if head z > 45
--     --  then (Leaf (V 1 === V 13) [], undefined, undefined )
--     --  else
--       let goal = conj $ E.substitute state (conjToList goal') in
--       let treeResult =
--             if any (\x -> isVariant (conjToList x) (conjToList goal)) seen
--             then
--               Leaf goal state
--             else
--               case find (\x -> conjToList x `embed` conjToList goal) ancs of
--                 Just g ->
--                   let (newGoals, everythingElse, gen1, gen2, names) =
--                         generalizeSplit z (conjToList g) (conjToList goal) in

--                   let env' = (x, y, names) in
--                   let newGoal = conj newGoals in

--                   let secondChild
--                         = if newGoals `isRenaming` conjToList goal
--                           then
--                             Leaf (V 1 === V 2) []
--                           else
--                             let (ch, _, _) = go (LC.Descend newGoal ancs) env' seen state in
--                             Gen ch newGoal in

--                   if null everythingElse
--                   then secondChild
--                   else
--                     let everything :: G S = conj everythingElse in
--                     let firstChild = fst3 $ go (LC.Descend everything ancs) env' seen state in
--                     Split [secondChild, firstChild] [newGoal, everything] state

--                 Nothing ->

--                   -- let incr = incrDeepOneStep 2 0 goal env state in
--                   -- trace "=======================================" $
--                   -- (case incr of
--                   --   Nothing -> trace "Nothing"
--                   --   Just incr' -> trace (printf "\nIncrDeepOneStep\nGoal:\n%s\nResult:\n%s\n\n" (show goal) (show' $ fst incr'))) $
--                   -- trace "=======================================" $

--                   -- let (unified, gamma) = oneStep goal env state in

--                   let (unified, gamma) = doStep goal env state in
--                   or (map (\(g, s') ->
--                         if null g
--                         then
--                           leaf s'
--                         else
--                           let (newEnv, onceUnfolded) =
--                                 foldl (\(env, acc) g' ->
--                                         -- let (x, env') = oneStep g' env s' in
--                                         let (x, env') = doStep g' env s' in


--                                         -- let incr = incrDeepOneStep 2 0 g' env s' in
--                                         -- trace "=======================================" $
--                                         -- (case incr of
--                                         --   Nothing -> trace "Nothing"
--                                         --   Just incr' -> trace (printf "\nIncrDeepOneStep\nGoal:\n%s\nResult:\n%s\n\n" (show g') (show' $ fst incr'))) $
--                                         -- trace "======================================="

--                                         (env', x : acc)
--                                       ) (gamma, []) g in

--                           let sequenced = sequence $ reverse onceUnfolded in

--                           if any checkConflicts $ map (map snd) sequenced
--                           then
--                             let filtered = filter (not . checkConflicts . map snd) sequenced in
--                             let ch =
--                                   map (\seq ->
--                                             let (gs, substs) = unzip seq in
--                                             let conjunction = concat gs in
--                                             let maybeSubst = unifySubsts substs in
--                                             case maybeSubst of
--                                               Nothing -> Fail
--                                               Just newSubst ->
--                                                 if null conjunction
--                                                 then leaf newSubst
--                                                 else
--                                                   let ch =  fst3 $ go (LC.Descend (conj conjunction) (conj g : goal : ancs))
--                                                                       newEnv
--                                                                       (conj g : goal : seen)
--                                                                       newSubst
--                                                   in
--                                                   Conj [ch] (concatMap fst seq) newSubst
--                                       )
--                                       filtered in
--                               or ch (LC.Descend (conj g) (goal : ancs)) s'
--                           else
--                             let ch = map (\h -> fst3 $ go (LC.Descend h (goal : ancs)) gamma (goal : seen) s') g in
--                             Conj ch g s')
--                           unified)
--                     d
--                     state
--       in (simplify treeResult, V 1 === V 2, [4, 5, 6, 7])

doStep :: G S -> E.Gamma -> E.Sigma -> ([([G S], E.Sigma)], E.Gamma)
doStep goal env state =
    fromMaybe
      (oneStep goal env state)
      (incrDeepOneStep globalLimit 0 goal env state)

or :: [NCTree] -> LC.Descend [G S] -> E.Sigma -> NCTree
or ch = Or (if null ch then [Fail] else ch)

checkConflicts :: [E.Sigma] -> Bool
checkConflicts sigmas =
    let conflicting = findConflicting sigmas in
    any (\x -> length x /= 1) conflicting

collectSubsts :: NCTree -> [E.Sigma]
collectSubsts (Or ch _ _) =
    mapMaybe go ch
  where
    go Fail         = Nothing
    go (Success s)  = Just s
    go (Or _ _ s)   = Just s
    go (Conj _ _ s) = Just s
    go (Gen _ _ _)  = Nothing
    go (Leaf _ s)   = Just s
collectSubsts (Leaf _ s) = [s]
collectSubsts x = trace (printf "\nPattern matching Failed on\n%s\n" $ show x) []

isConflicting :: E.Sigma -> E.Sigma -> Bool
isConflicting s1 s2 =
    let m1 = M.fromList s1 in
    let m2 = M.fromList s2 in
    let intersection = M.intersectionWith (\x y -> (E.walk x s1, E.walk y s2)) m1 m2 in
    M.size intersection > 0 &&
    any (uncurry conflicting) intersection
  where
    conflicting (C x xs) (C y ys) = x /= y || length xs /= length ys || any (uncurry conflicting) (zip xs ys)
    conflicting _ _ = False

findConflicting :: [E.Sigma] -> [[E.Sigma]]
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

simplify :: NCTree -> NCTree
simplify =
    go
  where
    go (Or    ch g s) = failOr ch (\x -> Or x g s)
    go (Conj  ch g s) = failConj ch (\x -> Conj x g s)
    go (Split ch g s) = failConj ch (\x -> Split ch g s)
    go (Gen   ch g gen) = failOr [ch] (\[x] -> Gen x g gen)
    go x = x
    failOr ch f =
      let simplified = filter (/= Fail) $ map go ch in
      if null simplified
      then Fail
      else f simplified
    failConj ch f =
      let simplified = map go ch in
      if Fail `elem` simplified
      then Fail
      else f simplified

