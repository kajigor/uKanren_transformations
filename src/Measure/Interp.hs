{-# LANGUAGE BangPatterns #-}
-- | An instrumented interpreter for the (first-order) miniKanren used in this
--   repository.  It follows standard microKanren semantics: every relation call
--   is wrapped in an inverse-eta delay (the only interleaving point), disjunction
--   is interleaving 'mplus' and conjunction is 'bind'.
--
--   The resulting search stream records every unification and every call, so a
--   consumer can count the work needed to produce the first N answers and can
--   stop the search once a fuel budget is exhausted.  Those counters are a
--   machine-independent cost measure which we use to compare original and
--   specialized programs.
module Measure.Interp
  ( Answer
  , Outcome (..)
  , RunStats (..)
  , runGoal
  , Profile (..)
  , RelProfile (..)
  , runGoalProfiled
  , runProgram
  , reifyCanonical
  ) where

import qualified Data.Map.Strict as M
import           Data.List       (foldl')
import           Def
import           Program
import           Syntax

type Var   = Int
type T     = Term Var
type Subst = M.Map Var T

data St = St !Subst !Int

-- | Search stream with explicit cost ticks.
data CS a
  = Nil
  | Cons a (CS a)
  | Unif Name Bool (CS a)  -- ^ one unification performed in the body of the
                           --   named relation, successful or not (no interleaving)
  | Call Name (CS a)       -- ^ one call of the named relation; interleaving point

mplusC :: CS a -> CS a -> CS a
mplusC Nil y          = y
mplusC (Cons h t) y   = Cons h (mplusC t y)
mplusC (Unif r b s) y = Unif r b (mplusC s y)
mplusC (Call r s) y   = Call r (mplusC y s)

bindC :: CS a -> (a -> CS b) -> CS b
bindC Nil _          = Nil
bindC (Cons h t) f   = mplusC (f h) (bindC t f)
bindC (Unif r b s) f = Unif r b (bindC s f)
bindC (Call r s) f   = Call r (bindC s f)

walk :: Subst -> T -> T
walk s t@(V v) = maybe t (walk s) (M.lookup v s)
walk _ t       = t

occurs :: Subst -> Var -> T -> Bool
occurs s v t = case walk s t of
  V u    -> u == v
  C _ ts -> any (occurs s v) ts

unify :: Subst -> T -> T -> Maybe Subst
unify s a b = go (walk s a) (walk s b)
  where
    go (V x) (V y) | x == y = Just s
    go (V x) t | occurs s x t = Nothing
               | otherwise    = Just (M.insert x t s)
    go t (V y) | occurs s y t = Nothing
               | otherwise    = Just (M.insert y t s)
    go (C f as) (C g bs)
      | f == g && length as == length bs =
          foldl' (\ms (x, y) -> ms >>= \s' -> unify s' x y) (Just s) (zip as bs)
    go _ _ = Nothing

type Defs = M.Map Name (Def G X)

inst :: M.Map X T -> Term X -> T
inst env (V x)    = M.findWithDefault (error $ "Measure.Interp: unbound variable " ++ x) x env
inst env (C f ts) = C f (map (inst env) ts)

-- | @cur@ is the relation whose body is being evaluated (for profiling).
eval :: Defs -> Name -> M.Map X T -> G X -> St -> CS St
eval _ cur env (a :=: b) (St s n) =
  case unify s (inst env a) (inst env b) of
    Nothing -> Unif cur False Nil
    Just s' -> Unif cur True (Cons (St s' n) Nil)
eval defs cur env (Conjunction g h gs) st =
  foldl' (\acc g' -> bindC acc (eval defs cur env g')) (eval defs cur env g st) (h : gs)
eval defs cur env (Disjunction g h gs) st =
  foldr1 mplusC (map (\g' -> eval defs cur env g' st) (g : h : gs))
eval defs cur env (Fresh x g) (St s n) =
  eval defs cur (M.insert x (V n) env) g (St s (n + 1))
eval defs cur env (Delay g) st = eval defs cur env g st
eval defs _ env (Invoke f args) st =
  case M.lookup f defs of
    _ | f == successName && null args -> Cons st Nil
    _ | f == failureName && null args -> Nil
    Just (Def _ params body) ->
      let env' = M.fromList (zip params (map (inst env) args)) in
      Call f (eval defs f env' body st)
    Nothing -> error $ "Measure.Interp: undefined relation " ++ f

-- | A reified answer: one term per query variable, free variables renamed
--   canonically (0, 1, ... in order of appearance).
type Answer = [Term Int]

data Outcome = Finished      -- ^ the search space was exhausted
             | GotEnough     -- ^ the requested number of answers was found
             | OutOfFuel     -- ^ fuel exhausted before either of the above
             deriving (Show, Eq)

data RunStats = RunStats
  { rsAnswers :: [Answer]
  , rsUnifs   :: !Int
  , rsCalls   :: !Int
  , rsOutcome :: Outcome
  } deriving Show

-- | Drive the stream: collect at most @n@ answers, spend at most @fuel@ steps
--   (unifications + calls).
drive :: Int -> Int -> (St -> Answer) -> CS St -> RunStats
drive n fuel rd = go [] 0 0 0
  where
    go acc !k !u !c s
      | k >= n         = RunStats (reverse acc) u c GotEnough
      | u + c >= fuel  = RunStats (reverse acc) u c OutOfFuel
      | otherwise = case s of
          Nil       -> RunStats (reverse acc) u c Finished
          Cons a t  -> let r = rd a in r `seq` go (r : acc) (k + 1) u c t
          Unif _ _ t -> go acc k (u + 1) c t
          Call _ t   -> go acc k u (c + 1) t

-- | Fully apply a substitution.
walkStar :: Subst -> T -> T
walkStar s t = case walk s t of
  V v    -> V v
  C f ts -> C f (map (walkStar s) ts)

-- | Reify a list of terms, renaming free variables in order of appearance, so
--   answers of different programs can be compared syntactically.
reifyCanonical :: Subst -> [T] -> Answer
reifyCanonical s ts =
  let ts' = map (walkStar s) ts
      (_, rs) = foldl' ren (M.empty, []) ts'
  in  forceAll (reverse rs)
  where
    ren (m, acc) t = let (m', t') = rename m t in (m', t' : acc)
    rename m (V v) = case M.lookup v m of
      Just i  -> (m, V i)
      Nothing -> let i = M.size m in (M.insert v i m, V i)
    rename m (C f ts) =
      let (m', ts') = foldl' (\(mm, acc) x -> let (mm', x') = rename mm x in (mm', x' : acc)) (m, []) ts
      in  (m', C f (reverse ts'))

forceAll :: [Term Int] -> [Term Int]
forceAll ts = foldr (\t r -> depth t `seq` r) ts ts
  where depth (V v) = v
        depth (C _ as) = sum (map depth as) + 1

-- | Run a goal whose free variables are @qvars@ (the query variables, they are
--   reified in this order).
runGoal :: [Def G X] -> [X] -> G X -> Int -> Int -> RunStats
runGoal defs qvars goal n fuel =
  let dm   = M.fromList [ (getName d, d) | d <- defs ]
      env  = M.fromList (zip qvars (map V [0 ..]))
      st0  = St M.empty (length qvars)
      rd (St s _) = reifyCanonical s (map V [0 .. length qvars - 1])
  in  drive n fuel rd (eval dm topName env goal st0)

topName :: Name
topName = "<query>"

data RelProfile = RelProfile { rpCalls :: !Int, rpUnifOk :: !Int, rpUnifFail :: !Int }
  deriving Show

-- | A run with a per-relation cost profile, the step count at which every
--   answer appeared, and the first @traceLen@ events of the search.
data Profile = Profile
  { pStats       :: RunStats
  , pRels        :: M.Map Name RelProfile
  , pAnswerSteps :: [Int]
  , pTrace       :: [(String, String)]   -- ^ (event, relation or answer number)
  , pTraceCut    :: Bool
  }

runGoalProfiled :: Int -> [Def G X] -> [X] -> G X -> Int -> Int -> Profile
runGoalProfiled traceLen defs qvars goal n fuel =
  let dm   = M.fromList [ (getName d, d) | d <- defs ]
      env  = M.fromList (zip qvars (map V [0 ..]))
      st0  = St M.empty (length qvars)
      rd (St s _) = reifyCanonical s (map V [0 .. length qvars - 1])
  in  goP rd [] [] M.empty [] 0 0 0 0 (eval dm topName env goal st0)
  where
    upd f r m = M.alter (Just . f . maybe (RelProfile 0 0 0) id) r m
    ev e tr k = if k < traceLen then e : tr else tr
    done acc steps rels tr k u c o =
      Profile (RunStats (reverse acc) u c o) rels (reverse steps) (reverse tr) (k > traceLen)
    goP rd acc steps !rels tr !ne !k !u !c s
      | ne >= n        = done acc steps rels tr k u c GotEnough
      | u + c >= fuel  = done acc steps rels tr k u c OutOfFuel
      | otherwise = case s of
          Nil -> done acc steps rels tr k u c Finished
          Cons a t -> let r = rd a in r `seq`
            goP rd (r : acc) (u + c : steps) rels (ev ("answer", show (ne + 1)) tr k) (ne + 1) (k + 1) u c t
          Unif r b t ->
            let rels' = upd (\p -> if b then p { rpUnifOk = rpUnifOk p + 1 } else p { rpUnifFail = rpUnifFail p + 1 }) r rels
            in  goP rd acc steps rels' (ev (if b then "unif+" else "unif-", r) tr k) ne (k + 1) (u + 1) c t
          Call r t ->
            goP rd acc steps (upd (\p -> p { rpCalls = rpCalls p + 1 }) r rels) (ev ("call", r) tr k) ne (k + 1) u (c + 1) t

-- | Run a program whose goal is @fresh x1 .. xk in g@: the top-level fresh
--   variables are the query variables.
runProgram :: Program G X -> Int -> Int -> RunStats
runProgram (Program defs goal) =
  let (vs, g) = topLevelFreshVars goal in runGoal defs vs g
