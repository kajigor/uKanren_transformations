module Unfold where

import           Control.Applicative
import           Control.Monad.State
import           Data.Function       (on)
import           Data.List           (sortBy, find, partition)
import           Data.Maybe          (mapMaybe, maybeToList, listToMaybe, fromJust, isJust )
import qualified Data.Set            as Set
import           Def
import           Descend 
import           Embed               (variantCheck)
import qualified Environment         as Env
import qualified Eval                as E
import qualified Subst
import           Syntax
import           Text.Printf         (printf)
import           Util.ListZipper
import           Util.Miscellaneous  (pinpoint)
import qualified VarInterpretation   as VI

import Debug.Trace
import Mode.Term (Var(getVar))

oneStepUnfold :: G S -> State Env.Env (G S)
oneStepUnfold g@(Invoke f as) = do
  env <- get
  let (Def n fs body) = Env.getDef env f
  if length fs == length as
  then do
    let i' = foldl (\ interp (f, a) -> VI.extend interp f a) (Env.getInterp env) $ zip fs as
    let ((g', _), env') = runState (E.preEval body) (Env.updateInterp env i')
    put env'
    return g'
  else error $ printf "Unfolding error: different number of factual and actual arguments\nFactual: %s --- %s\nActual: %s --- %s)" f (show as) n (show fs)
oneStepUnfold g = return g

oneStep :: G S -> Subst.Subst Int -> State Env.Env [([G S], Subst.Subst Int)]
oneStep goal state = do
    unfolded <- oneStepUnfold goal
    let normalized = normalize unfolded
    let unified = mapMaybe (unifyStuff state) normalized
    return unified

unfoldConjunction :: [G S] -> Subst.Subst Int -> State Env.Env [([G S], Subst.Subst Int)]
unfoldConjunction (x:xs) state = do
  unified <- oneStep x state
  results <- mapM (\(g, s) -> do
                      goals <- unfoldConjunction xs s
                      return $ (\(x,y) -> (g ++ x, y)) <$> goals
                  ) unified
  return $ concat results
unfoldConjunction [] s = return [([],s)]

normalize :: G S -> [[G S]] -- disjunction of conjunctions of calls and unifications
normalize (Disjunction x y gs) = concatMap normalize (x : y : gs)
normalize (Conjunction x y gs) = (++) <$> normalize x <*> normalize (unsafeConj (y : gs))
normalize g@(Invoke _ _) = [[g]]
normalize g@(_ :=: _) = [[g]]
normalize (Delay g) = normalize g
normalize g = error ("Unexpected goal type in normalization\n" ++ show g)

unifyStuff :: Subst.Subst Int -> [G S] -> Maybe ([G S], Subst.Subst Int)
unifyStuff state gs =
    go gs state []
  where
    go [] state conjs = Just (reverse conjs, state)
    go (g@(Invoke _ _) : gs) state conjs = go gs state (g : conjs)
    go ((t :=: u) : gs) state conjs = do
      s <- E.unify (Just state) t u
      go gs s conjs

maximumBranches :: Def G X -> Int
maximumBranches def@(Def _ args body) =
    let goal = fst $ evalState (E.preEval (fresh args body)) Env.empty in
    length $ evalState (oneStep (succeed goal) Subst.empty) Env.empty
  where
    succeed (Invoke name args) = success
    succeed (t :=: u) = t :=: u
    succeed (Conjunction x y g) = unsafeConj $ succeed <$> (x : y : g)
    succeed (Disjunction x y g) = unsafeDisj $ succeed <$> (x : y : g)
    succeed (Fresh name g) = Fresh name $ succeed g
    succeed (Delay g) = Delay $ succeed g 

    success = C "" [] :=: C "" []

getMaximumBranches :: Env.Env -> G S -> Int
getMaximumBranches env (Invoke name _) =
    let def = Env.getDef env name in
    maximumBranches def


notMaximumBranches :: Env.Env -> Subst.Subst Int -> G S -> Bool
notMaximumBranches env state goal@(Invoke name args) =
    let maxBranches = maximumBranches (Env.getDef env name) in
    let unfolded = evalState (oneStep goal state) env in
    length unfolded < maxBranches
    -- let result = length unfolded < maxBranches in
    -- result
notMaximumBranches _ _ _ = False

-- unfoldComplexity :: Env.Env -> E.Sigma -> G S -> Int
-- unfoldComplexity env@(p, _, _) state goal@(Invoke name args) =
--     let (unfolded, _) = oneStep goal env state in
--     (length $ filter (not . null . fst) unfolded)

-- unfoldComplexity :: Env.Env -> E.Sigma -> G S -> (Int, Int)
-- unfoldComplexity env@(p, _, _) state goal@(Invoke name args) =
--     let (unfolded, _) = oneStep goal env state in
--     (length $ filter (not . null . fst) unfolded, length unfolded)

data Complexity = Complexity { maxBranches :: Int, curBranches :: Int, substs :: Int }
                deriving (Eq, Show)

-- instance Ord Complexity where
--   (Complexity max cur subst) <= (Complexity max' cur' subst') =
--     cur < max || cur - subst >= cur' - subst' || cur <= cur'

findTupling :: Env.Env -> Subst.Subst Int -> [G S] -> [[G S]] -> Maybe (([([G S], Subst.Subst Int)], Env.Env))
findTupling env subst goal ancs =
    let (conjunctions, env') = runState (unfoldConjunction goal subst) env in
    -- let res@(unfolded, env') = runState (mapM (\g -> oneStep g subst) goal) env in
    -- let conjunctions = cross unfolded in
    if any (\(g, s) -> variantCheck (Subst.substituteList s g) ancs) conjunctions
    then Just (conjunctions, env')
    else Nothing
  where
    cross :: [[([G S], Subst.Subst Int)]] -> [([G S], Subst.Subst Int)]
    cross [] = [([],Subst.empty)]
    cross (x:xs) = [ (y ++ z, Subst.union s' s'') | (y, s') <- x, (z, s'') <- cross xs ]

findStatic :: Env.Env -> Zipper (Descend (G S)) -> Maybe (Zipper (Descend (G S))) 
findStatic env = 
  goRightUntil (isGoalStatic env . getCurr)

data ComplexityType 
  = Static 
  | GroundArgs Int Int -- has some ground args. The first argument is the negated number of ground args, the second -- the number of non-ground args
  | Deterministic 
  | Restricting -- less branches than maximally possible 
  | SomeSubsts
  | Complex -- default option for when nothing is really simplified
  deriving (Eq, Ord, Show)

unfoldComplexityType :: Env.Env -> Subst.Subst Int -> G S -> ComplexityType
unfoldComplexityType env state goal =
  let annotated = (goal, unfoldComplexity env state goal) in 
  let res = 
        if goalStatic env annotated then Static else 
        let mGroundArgs = groundArgs annotated in if isJust mGroundArgs then fromJust mGroundArgs else 
        if deterministic annotated then Deterministic else 
        if maxBranch annotated then Restricting else 
        if partialSubst annotated then SomeSubsts else 
        Complex
  in -- trace "UnfoldComplexityType" $ traceShow goal $ traceShow res $ 
     res 

findBestByComplexityDescend :: Env.Env -> Subst.Subst S -> Zipper (Descend (G S)) -> (Descend (G S) -> Bool) -> Maybe (Zipper (Descend (G S)))
findBestByComplexityDescend env state zipper selecter = 
    let zippers = allRights zipper in 
    let annotated = map (\x -> (x, unfoldComplexityType env state $ getCurr $ cursor x)) zippers in 
    let sorted = sortBy (compare `on` snd) annotated in 
    fst <$> find (selecter . cursor . fst) sorted 
    -- case sorted of 
    --   (_, Complex) : _ -> Nothing 
    --   (g, _) : _ -> Just g 
    --   [] -> Nothing 
  
sortByComplexity :: Env.Env -> Subst.Subst S -> [G S] -> [Zipper (G S)]
sortByComplexity env state goals = 
  let estimated = map (\g -> (g, unfoldComplexity env state g)) goals in
  let priorities = [goalStatic env, onlySubsts, deterministic, maxBranch, partialSubst] in 
  throwAwayComplexity $ prioritizeByPred priorities estimated 

goalStatic env (g, _) = isGoalStatic env g 
groundArgs (g, _) = 
    let (ground, notGround) = partition isGround $ args g in 
    if null ground then Nothing else Just $ GroundArgs (-(length ground)) (length notGround)
  where 
    args (Invoke _ ts) = ts 
    isGround t = null (fv t)
    
onlySubsts (_, compl) = curBranches compl == substs compl
deterministic (_, compl) = curBranches compl == 1
maxBranch (_, compl) = maxBranches compl > curBranches compl
partialSubst (_, compl) =  substs compl > 0
throwAwayComplexity z = (fst <$>) <$> z

findBestByComplexity :: Env.Env -> Subst.Subst S -> [G S] -> Maybe (Zipper (G S))
findBestByComplexity env subst gs = 
  listToMaybe $ sortByComplexity env subst gs 

unfoldComplexity :: Env.Env -> Subst.Subst Int -> G S -> Complexity
unfoldComplexity env sigma goal@(Invoke name _) =
  let unfolded = evalState (oneStep goal sigma) env in
  let max = maximumBranches (Env.getDef env name) in
  Complexity max (length unfolded) (length $ filter (null . fst) unfolded)
unfoldComplexity _ _ goal = error $ printf "Called unfoldComplexity on a non-call goal: %s" $ show goal 

static :: Env.Env -> String -> Bool
static env name =
    go (Set.fromList [name]) (getBody name)
  where
    go set (Invoke name _) | Set.member name set = False
    go set (Invoke name _) = go (Set.insert name set) (getBody name)
    go set (Conjunction x y gs) = all (go set) (x : y : gs)
    go set (Disjunction x y gs) = all (go set) (x : y : gs)
    go set (Fresh _ g) = go set g
    go _ _ = True

    getBody name =
      let Def _ _ body = Env.getDef env name in body

isGoalStatic :: Env.Env -> G S -> Bool
isGoalStatic env (Invoke name _) =
  static env name
isGoalStatic _ _ = False


-- is_path x0 = (x0 == [] | 
--   (fresh x1 in (x0 == [x1])) | 
--   (fresh x6, x5, x4, x2, x3, x14, x15, x7, x8, x9, x10, x11, x12, x13, x22, x23, x28 in 
--     ((x0 == (x6 :: (x5 :: x4)) & x2 == Trueo & x3 == Trueo & x5 == x14 & x6 == x15 & x7 == Trueo & x8 == (Pair (Succ (Zero)) (Succ (Succ (Zero))) :: [Pair (Succ (Succ (Zero))) (Zero)]) & x9 == Pair (Zero) (Succ (Zero)) & x10 == Trueo & x11 == Trueo & x12 == Succ (Zero) & x13 == Zero & x14 == Succ (x22) & x15 == Zero & x22 == Zero & x23 == Zero & (x28 == (Succ (Zero) :: x4) & is_path x28)))) | 
--   (fresh x6, x5, x4, x2, x3, x29, x30, x7, x8, x9, x16, x17, x18, x25, x26, x27, x28, x59, x39, x40, x69, x60, x70, x87, x102, x101, x100, x94, x93, x92, x91, x90, x89, x88 in 
--     ((x0 == (x6 :: (x5 :: x4)) & x2 == Trueo & x3 == Trueo & x5 == x29 & x6 == x30 & x7 == Falso & x8 == (Pair (Succ (Zero)) (Succ (Succ (Zero))) :: [Pair (Succ (Succ (Zero))) (Zero)]) & x9 == Pair (Zero) (Succ (Zero)) & x16 == Trueo & x17 == [Pair (Succ (Succ (Zero))) (Zero)] & x18 == Pair (Succ (Zero)) (Succ (Succ (Zero))) & x25 == Trueo & x26 == Trueo & x27 == Succ (Succ (Zero)) & x28 == Succ (Zero) & x29 == Succ (x59) & x30 == Succ (x39) & x39 == Zero & x40 == Zero & x59 == Succ (x69) & x60 == Succ (Zero) & x69 == Zero & x70 == Zero & (x87 == (Succ (Succ (Zero)) :: x4) & is_path x87) & (x102 == Zero & x101 == Zero & x100 == Succ (Zero) & x94 == Zero & x93 == Succ (Zero) & x92 == Succ (Succ (Zero)) & x91 == Zero & x90 == Succ (Zero) & x89 == Falso & x88 == Falso)))) | 
--   (fresh x6, x5, x4, x2, x3, x50, x51, x7, x8, x9, x16, x17, x18, x31, x32, x33, x46, x47, x48, x49, x66, x82, x67, x83, x128, x140, x135, x134, x133, x132, x131, x130, x129, x144, x139, x138, x137 in 
--     ((x0 == (x6 :: (x5 :: x4)) & x2 == Trueo & x3 == Trueo & x5 == x50 & x6 == x51 & x7 == Falso & x8 == (Pair (Succ (Zero)) (Succ (Succ (Zero))) :: [Pair (Succ (Succ (Zero))) (Zero)]) & x9 == Pair (Zero) (Succ (Zero)) & x16 == Falso & x17 == [Pair (Succ (Succ (Zero))) (Zero)] & x18 == Pair (Succ (Zero)) (Succ (Succ (Zero))) & x31 == Trueo & x32 == [] & x33 == Pair (Succ (Succ (Zero))) (Zero) & x46 == Trueo & x47 == Trueo & x48 == Zero & x49 == Succ (Succ (Zero)) & x50 == Zero & x51 == Succ (x66) & x66 == Succ (x82) & x67 == Succ (Zero) & x82 == Zero & x83 == Zero & (x128 == (Zero :: x4) & is_path x128) & (x140 == Zero & x135 == Succ (Zero) & x134 == Succ (Succ (Zero)) & x133 == Zero & x132 == Zero & x131 == Succ (Zero) & x130 == Falso & x129 == Falso) & (x144 == Succ (Zero) & x139 == Zero & x138 == Zero & x137 == Succ (Zero) & x134 == Succ (Succ (Zero)) & x133 == Zero & x132 == Succ (Zero) & x131 == Succ (Succ (Zero)) & x130 == Falso & x129 == Falso)))));

-- _is_path x0 = (fresh x28, x4 in ((x28 == (Succ (Zero) :: x4) & is_path x28)));

-- eq_pair  = (fresh x102, x101, x100, x94, x93, x92, x91, x90, x89, x88 in ((x102 == Zero & x101 == Zero & x100 == Succ (Zero) & x94 == Zero & x93 == Succ (Zero) & x92 == Succ (Succ (Zero)) & x91 == Zero & x90 == Succ (Zero) & x89 == Falso & x88 == Falso)));

-- _eq_pair  = (fresh x140, x135, x134, x133, x132, x131, x130, x129 in ((x140 == Zero & x135 == Succ (Zero) & x134 == Succ (Succ (Zero)) & x133 == Zero & x132 == Zero & x131 == Succ (Zero) & x130 == Falso & x129 == Falso)));

-- __eq_pair  = (fresh x144, x139, x138, x137, x134, x133, x132, x131, x130, x129 in ((x144 == Succ (Zero) & x139 == Zero & x138 == Zero & x137 == Succ (Zero) & x134 == Succ (Succ (Zero)) & x133 == Zero & x132 == Succ (Zero) & x131 == Succ (Succ (Zero)) & x130 == Falso & x129 == Falso)));


-- ? is_path x0