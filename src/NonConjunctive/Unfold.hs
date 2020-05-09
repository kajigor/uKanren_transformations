{-# LANGUAGE ScopedTypeVariables #-}

module NonConjunctive.Unfold where

import qualified CPD.LocalControl   as LC
import           Data.Foldable      (foldlM)
import           Data.List          (find, intersect, partition, sortBy, (\\))
import qualified Data.Map.Strict    as M
import           Data.Maybe         (catMaybes, fromJust, fromMaybe, isJust,
                                     isNothing, mapMaybe)
import           Debug.Trace        (trace)
import           Embed
import qualified Eval               as E
import           Generalization     (Generalizer, generalizeSplit)
import           Prelude            hiding (or)
import           Syntax
import           Text.Printf        (printf)
import           Unfold             (findBestByComplexity, notMaximumBranches,
                                     oneStep, oneStepUnfold, unfoldComplexity)
import           Util.Check         (checkConj)
import           Util.Miscellaneous (fst3, show', snd3)

data NCTree = Fail
            | Success E.Sigma E.Gamma
            | Or [NCTree] (LC.Descend [G S]) E.Sigma
            | Conj [NCTree] [G S] E.Sigma
            | Gen NCTree [G S] Generalizer
            | Leaf [G S] E.Sigma E.Gamma [G S] -- last argument is a goal renaming of which current node is
            | Split [NCTree] [G S] E.Sigma
            | Prune [G S] E.Sigma
            -- deriving (Show, Eq)


instance Show NCTree where
  show Fail = "_|_"
  show (Success _ _) = "Success"
  show (Or ch (LC.Descend gs _) _) = printf "Or %s %s" (show gs) (show ch)
  show (Conj ch gs _) = printf "Conj %s %s" (show gs) (show ch)
  show (Leaf gs _ _ _) = printf "Leaf %s" (show gs)
  show (Split ch gs _) = printf "Split %s %s" (show gs) (show ch)
  show (Prune gs _) = printf "Prune %s" (show gs)
  show (Gen _ _ _) = "Gen"


instance Eq NCTree where
  Fail == Fail = True
  Success s _ == Success s' _ = s == s'
  Or ch ds s == Or ch' ds' s' = ch == ch' && ds == ds' && s == s'
  Conj ch gs s == Conj ch' gs' s' = ch == ch' && gs == gs' && s == s'
  Leaf gs s _ r == Leaf gs' s' _ r' = gs == gs' && s == s' && r == r'
  Split ch gs s == Split ch' gs' s' = ch == ch' && gs == gs' && s == s'
  Prune gs s == Prune gs' s' = gs == gs' && s == s'
  _ == _ = False

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

restrictSubsts :: NCTree -> NCTree
restrictSubsts =
    go []
  where
    go subst (Conj ch gs s)  = Conj (map (go s) ch) gs (s \\ subst)
    go subst (Or ch gs s)    = Or (map (go s) ch) gs (s \\ subst)
    go subst (Gen ch gs gen) = Gen (go subst ch) gs gen
    go subst (Leaf gs s e v) = Leaf gs (s \\ subst) e v
    go subst (Split ch gs s) = Split (map (go s) ch) gs (s \\ subst)
    go subst (Prune gs s)    = Prune gs (s \\ subst)
    go subst (Success s e)   = Success (s \\ subst) e
    go _ Fail                = Fail


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

leaf :: E.Sigma -> E.Gamma -> NCTree
leaf [] _ = Fail
leaf s  e = Success s e

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
    let failed = [] in
    let descend = LC.Descend (conjToList logicGoal) [] in
    (fst3 $ go descend gamma' nodes E.s0 failed, logicGoal, names)
  where
    go :: LC.Descend [G S] -> E.Gamma -> [[G S]] -> E.Sigma -> [[G S]] -> (NCTree, [[G S]], [[G S]])
    go (LC.Descend goal' ancs') env@(x,y,z) seen state failed =
     let goal = E.substitute state goal' in
     let seen' = goal : seen in
     let addAnc x = LC.Descend x (goal : ancs') in
     trace (printf "go\nGoal:\n%s\nSeen:\n%s\n" (show goal) (show' seen)) $
     if goal `elem` failed
     then
       (Fail, seen, failed)
     else
      if limit > 0 && head z > limit
      then
        (Prune goal state, seen, failed)
      else
        (\(x, y, z) -> (simplify x, y, z)) $
          case if isGround goal then Nothing else findVariant goal seen of
            Just v ->
              (Leaf goal state env v, seen, failed)
            _ ->
              -- A test for accumulating parameter
              -- f (x, y) /\ f (z, y) -> f (m, n) /\ f (z, C (n))
              if any (\x -> {-length x == length goal &&-} x `embed` goal) ancs'
              then
                if length goal == 1
                then
                  (Prune goal state, seen, failed)
                else
                  let (children, allSeenGoals, failed') = unfoldSequentially failed seen' (zip (map (:[]) goal) (repeat state)) env addAnc in
                  (Split children goal state, allSeenGoals, failed')
              else
                if length goal == 1
                then
                  let (unified, env') = oneStep (head goal) env state in
                  let (ch, allSeenGoals, failed') = unfoldSequentially failed seen' unified env' addAnc in
                  (or ch (addAnc goal) state allSeenGoals failed')
                else
                  case findBestByComplexity env state goal of
                    -- Either ls or rs is not empty!
                    Just (ls, x, rs) ->
                      case findVariant [x] seen' of
                        Just v ->
                          let x' = Conj [Leaf [x] state env v] [x] state in
                          let (ls', seen'', failed'')  = unfoldNotNull failed ls seen'  state env addAnc in
                          let (rs', seen''', failed''') = unfoldNotNull failed'' rs seen'' state env addAnc in
                          (Split (catMaybes [ls', Just x', rs']) goal state, seen''', failed''')
                        Nothing ->
                          let (unified, env') = oneStep x env state in
                          let (ch, allSeenGoals', failed_') = unfoldSequentially failed seen' unified env' addAnc in
                          let (x', allSeenGoals, failed') = or ch (addAnc [x]) state allSeenGoals' failed_' in
                          case computedAnswers x' of
                            Just xs ->
                              if all (\(gs, _, _) -> null gs || instanceCheck gs seen) xs
                              then
                                -- WHAT IF IT RENAMES WITHIN THIS SUBTREE???
                                let (children, allSeenGoals, failed'') =
                                      foldl (\(ys, seenGoals, failedGoals) (goals, subst, newEnv) ->
                                                let toUnfold = wrap ls rs goals in
                                                let (node, newSeen, newFailed) =
                                                      if null toUnfold
                                                      then (leaf subst newEnv, seenGoals, failedGoals)
                                                      else
                                                        go (addAnc toUnfold) newEnv seenGoals subst failedGoals in
                                                (nullGoals toUnfold subst newEnv node : ys, newSeen, newFailed)
                                            )
                                            ([], seen', failed')
                                            xs in
                                (or (reverse children) (addAnc $ wrap ls rs [x]) state allSeenGoals failed'')
                              else
                                let (ls', seen'', failed'')  = unfoldNotNull failed' ls allSeenGoals state env addAnc in
                                let (rs', seen''', failed''') = unfoldNotNull failed'' rs seen'' state env addAnc in
                                (Split (catMaybes [ls', Just x', rs']) goal state, seen''', failed''')
                            Nothing ->
                              let (ls', seen'', failed'')  = unfoldNotNull failed' ls allSeenGoals state env addAnc in
                              let (rs', seen''', failed''') = unfoldNotNull failed'' rs seen'' state env addAnc in
                              (Split (catMaybes [ls', Just x', rs']) goal state, seen''', failed''')
                    Nothing ->
                      let (children, allSeenGoals, failed'') = unfoldSequentially failed seen' (zip (map (:[]) goal) (repeat state)) env addAnc in
                      (Split children goal state, allSeenGoals, failed'')
    wrap left right x = left ++ x ++ right
    nullGoals goals subst env v = if null goals then leaf subst env else v

    unfoldNotNull failed xs seen state env addAnc =
      if null xs
      then (Nothing, seen, failed)
      else
        let (children, seen', failed') = unfoldSequentially failed seen (zip (map (:[]) xs) (repeat state)) env addAnc in
        trace "5" $
        (Just $ Split children xs state, seen', failed')

    goNotNull xs seen state env addAnc failed =
      if null xs
      then (Nothing, seen, failed)
      else
        let (node, seen', failed') = go (addAnc xs) env seen state failed in
        (Just node, seen', failed')

    unfoldSequentially failed seen unified env addAnc =
      let (ch, allSeenGoals, actualFailed) =
                      foldl (\(xs, seenGoals, failedGoals) (goals, subst) ->
                                let (node, newSeen, newFailed) =
                                      if null goals
                                      then (leaf subst env, seenGoals, failedGoals)
                                      else go (addAnc goals) env seenGoals subst failedGoals in
                                (nullGoals goals subst env node : xs, newSeen, newFailed)
                            )
                            ([], seen, failed)
                            unified in
      (reverse ch, allSeenGoals, actualFailed)


computedAnswers :: NCTree -> Maybe ([([G S], E.Sigma, E.Gamma)])
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

doStep :: G S -> E.Gamma -> E.Sigma -> ([([G S], E.Sigma)], E.Gamma)
doStep goal env state =
    fromMaybe
      (oneStep goal env state)
      (incrDeepOneStep globalLimit 0 goal env state)

or :: [NCTree] -> LC.Descend [G S] -> E.Sigma -> [[G S]] -> [[G S]] -> (NCTree, [[G S]], [[G S]])
or ch d@(LC.Descend gs _) state seen failed =
  if null ch
  then (Or [Fail] d state, seen, (gs:failed))
  else (Or ch d state, seen, failed)

checkConflicts :: [E.Sigma] -> Bool
checkConflicts sigmas =
    let conflicting = findConflicting sigmas in
    any (\x -> length x /= 1) conflicting

collectSubsts :: NCTree -> [E.Sigma]
collectSubsts (Or ch _ _) =
    mapMaybe go ch
  where
    go Fail           = Nothing
    go (Success s _)  = Just s
    go (Or _ _ s)     = Just s
    go (Conj _ _ s)   = Just s
    go (Gen _ _ _)    = Nothing
    go (Leaf _ s _ _) = Just s
collectSubsts (Leaf _ s _ _) = [s]
collectSubsts x = []

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
    go (Or    ch g s)   = failOr ch (\x -> Or x g s)
    go (Conj  ch g s)   = failConj ch (\x -> Conj x g s)
    go (Split ch g s)   = failConj ch (\x -> Split ch g s)
    go (Gen   ch g gen) = failOr [ch] (\[x] -> Gen x g gen)
    go x                = x
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

noPrune :: NCTree -> Bool
noPrune (Prune _ _) = False
noPrune (Or ch _ _) = all noPrune ch
noPrune (Conj ch _ _) = all noPrune ch
noPrune (Split ch _ _) = all noPrune ch
noPrune _ = True