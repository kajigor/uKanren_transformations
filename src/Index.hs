-- | Constructor analysis and constructor dispatch for residual programs.
--
--   Specialized interpreters often keep a clause table: a relation whose body
--   is a disjunction of many branches, each testing the same parameter against
--   a constructor (@y == Jo_add a b c & ...@).  Every call then tries every
--   branch, and nearly all of these tests fail.  Two answer-preserving passes
--   remove that work:
--
--   * __Impossible-branch elimination.__  A fixpoint computes, for every
--     parameter of every relation, the set of top-level constructors it can
--     have in a successful derivation: at a call site, an argument @C ts@ has
--     constructor @C@; a variable has the constructor it is unified with on the
--     same conjunctive path (unification is order independent, so a branch
--     binding it to another constructor fails eventually), or the set of the
--     caller's parameter it is, or is unknown.  A branch that tests a parameter
--     against a constructor outside that set cannot contribute an answer and is
--     dropped.
--
--   * __Constructor dispatch__ (switch detection, as in Mercury).  Branches
--     that test the same parameter @p@ against the same constructor @C@, with
--     distinct variables local to the branch as arguments, are grouped:
--
--     > (p == C a1 & A1) | (p == C a2 & A2) | B
--     >   ==>  fresh z (p == C z & (A1[z/a1] | A2[z/a2])) | B
--
--     Conjunction distributes over disjunction, so the answers are the same;
--     a call now tests @p@ once per constructor instead of once per branch.
--     Grouping is applied again inside each group, on the new variables.
--
--   Both passes may change the order in which the interleaving search
--   enumerates answers, never the set of answers.
module Index
  ( indexProgram
  , eliminateImpossible
  , dispatch
  , analyse
  , paths
  , CSet
  , Table
  ) where

import           Data.List       (foldl', nub)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe, mapMaybe)
import qualified Data.Set        as S
import           Def
import           Syntax

type Prog = ([Def G X], G X)

-- | Elimination to a fixpoint, then dispatch.
indexProgram :: Prog -> Prog
indexProgram p = let (ds, g) = elim (8 :: Int) p in (map dispatchDef ds, g)
  where
    elim 0 q = q
    elim k q = let q' = eliminateImpossible q in if sizeP q' == sizeP q then q else elim (k - 1) q'
    sizeP (ds, g) = sum (map (size . getBody) ds) + size g

------------------------------------------------------------------------------
-- Constructor sets

-- | 'Nothing' = any constructor (or unbound at the end): no information.
type CSet = Maybe (S.Set (Name, Int))

joinC :: CSet -> CSet -> CSet
joinC (Just a) (Just b) = Just (S.union a b)
joinC _ _               = Nothing

type Table = M.Map (Name, Int) CSet   -- absent = no call site seen (bottom)

-- | Conjunctive paths of a goal: the atomic conjuncts (unifications, calls)
--   of each disjunct of its disjunctive normal form.
paths :: G X -> [[G X]]
paths g = case g of
  Disjunction a b cs -> concatMap paths (a : b : cs)
  Conjunction a b cs -> map concat (mapM paths (a : b : cs))
  Fresh _ h          -> paths h
  Delay h            -> paths h
  _                  -> [[g]]

-- | Constructor of a variable fixed by a unification on the path.
pathCons :: [G X] -> M.Map X (Name, Int)
pathCons cs = M.fromList $ concatMap one cs
  where
    one (a :=: b) = pairs a b
    one _         = []
    pairs (V x) (C c ts) = [(x, (c, length ts))]
    pairs (C c ts) (V x) = [(x, (c, length ts))]
    pairs (C c as) (C d bs) | c == d && length as == length bs = concat (zipWith pairs as bs)
    pairs _ _ = []

argSet :: Table -> Name -> M.Map X Int -> M.Map X (Name, Int) -> Term X -> CSet
argSet _ _ _ _ (C c ts) = Just (S.singleton (c, length ts))
argSet tbl self params pc (V x) = case M.lookup x pc of
  Just k  -> Just (S.singleton k)
  Nothing -> case M.lookup x params of
    Just i  -> fromMaybe (Just S.empty) (M.lookup (self, i) tbl)
    Nothing -> Nothing

analyse :: Prog -> Table
analyse (ds, goal) = go (M.empty :: Table)
  where
    defs = M.fromList [ (getName d, d) | d <- ds ]
    go tbl = let tbl' = step tbl in if tbl' == tbl then tbl else go tbl'
    step tbl =
      let sites = sitesOf tbl "" M.empty goal
                  ++ concat [ sitesOf tbl f (M.fromList (zip as [0 ..])) body | Def f as body <- ds ]
      in M.fromListWith joinC sites
    sitesOf tbl self params body =
      [ ((f, i), argSet tbl self params pc t)
      | p <- paths body, let pc = pathCons p
      , Invoke f ts <- p, M.member f defs, (i, t) <- zip [0 ..] ts ]

-- | Drop branches whose test on a parameter can never succeed.
eliminateImpossible :: Prog -> Prog
eliminateImpossible prog@(ds, g) = (map elimDef ds, g)
  where
    tbl = analyse prog
    elimDef (Def f as body) =
      let params = M.fromList (zip as [0 ..])
          allowed x = case M.lookup x params of
            Just i  -> fromMaybe (Just S.empty) (M.lookup (f, i) tbl)
            Nothing -> Nothing
      in Def f as (prune allowed S.empty body)
    -- bound: names rebound by an enclosing fresh (they shadow parameters)
    prune allowed bound gl = case gl of
      Disjunction a b cs ->
        let kept = filter (not . impossible allowed bound) (a : b : cs)
        in case map (prune allowed bound) kept of
             []  -> failure
             [x] -> x
             xs  -> unsafeDisj xs
      Conjunction a b cs -> unsafeConj (map (prune allowed bound) (a : b : cs))
      Fresh x h          -> Fresh x (prune allowed (S.insert x bound) h)
      Delay h            -> Delay (prune allowed bound h)
      _                  -> if impossible allowed bound gl then failure else gl
    impossible allowed bound d = any bad (direct d)
      where
        bad (V x :=: C c ts) = test x (c, length ts)
        bad (C c ts :=: V x) = test x (c, length ts)
        bad _                = False
        test x k = not (x `S.member` bound) && maybe False (not . S.member k) (allowed x)
                   && not (x `S.member` freshIn d)

-- | Atomic conjuncts of a goal that do not lie under a disjunction.
direct :: G X -> [G X]
direct g = case g of
  Conjunction a b cs -> concatMap direct (a : b : cs)
  Fresh _ h          -> direct h
  Delay h            -> direct h
  Disjunction {}     -> []
  _                  -> [g]

freshIn :: G X -> S.Set X
freshIn g = case g of
  Fresh x h          -> S.insert x (freshIn h)
  Conjunction a b cs -> S.unions (map freshIn (a : b : cs))
  Delay h            -> freshIn h
  _                  -> S.empty

------------------------------------------------------------------------------
-- Constructor dispatch

dispatchDef :: Def G X -> Def G X
dispatchDef (Def f as body) = Def f as (dispatch as body)

-- | Group the disjuncts of a goal by the constructor they test a candidate
--   variable against (see the module header).
dispatch :: [X] -> G X -> G X
dispatch cands body = case body of
  Disjunction a b cs -> regroup (0 :: Int) (a : b : cs)
  Fresh x h          -> Fresh x (dispatch (filter (/= x) cands) h)
  _                  -> body
  where
    regroup k ds =
      let best = bestVar ds
      in case best of
           Nothing -> unsafeDisj ds
           Just v  -> unsafeDisj (group k v ds)
    -- the candidate that most disjuncts select on, if some constructor is
    -- selected by at least two disjuncts
    bestVar ds =
      let score v = let ks = mapMaybe (fmap fst . selector v) ds
                    in (length ks - length (nub ks), v)
          scored = [ s | v <- cands, s@(n, _) <- [score v], n > 0 ]
      in if null scored then Nothing else Just (snd (maximum scored))
    group k v ds =
      let tagged = [ (selector v d, d) | d <- ds ]
          keys = nub [ c | (Just (c, _), _) <- tagged ]
          multi = S.fromList [ c | c <- keys, length [ () | (Just (c', _), _) <- tagged, c' == c ] > 1 ]
          emit (out, done) (Just (c, _), _)
            | c `S.member` multi && c `S.member` done = (out, done)
            | c `S.member` multi =
                let members = [ (as', d) | (Just (c', as'), d) <- tagged, c' == c ]
                    (cn, n) = c
                    zs = [ "_ix" ++ show k ++ "_" ++ cn ++ "_" ++ show j | j <- [1 .. n] ]
                    inner = [ stripSelector v (M.fromList (zip as' zs)) d | (as', d) <- members ]
                    innerG = dispatch zs (unsafeDisj inner)
                in (out ++ [fresh zs (unsafeConj [V v :=: C cn (map V zs), innerG])], S.insert c done)
          emit (out, done) (_, d) = (out ++ [d], done)
      in fst (foldl' emit ([], S.empty) tagged)

-- | @selector v d@: the constructor @d@ tests @v@ against, with the argument
--   variables, when they are distinct variables bound by a fresh of @d@
--   (so they can be renamed instead of unified).
selector :: X -> G X -> Maybe ((Name, Int), [X])
selector v d =
  let loc = freshIn d
      sels = [ (c, ts) | u <- direct d, Just (c, ts) <- [sel u] ]
      sel (V x :=: C c ts) | x == v = Just (c, ts)
      sel (C c ts :=: V x) | x == v = Just (c, ts)
      sel _ = Nothing
  in case sels of
       [(c, ts)] | Just xs <- mapM unV ts, length (nub xs) == length xs, all (`S.member` loc) xs
                 , v `S.notMember` loc
                 -> Just ((c, length ts), xs)
       _ -> Nothing
  where
    unV (V x) = Just x
    unV _     = Nothing

-- | Remove the selector @v == C xs@ and the fresh binders of @xs@ from a
--   disjunct, renaming @xs@ to the group's variables.
stripSelector :: X -> M.Map X X -> G X -> G X
stripSelector v ren d = rename (go d)
  where
    go g = case g of
      Fresh x h | M.member x ren -> go h
                | otherwise      -> Fresh x (go h)
      Conjunction a b cs ->
        let parts = filter (not . isSel) (a : b : cs)
        in case map go parts of
             []  -> success
             [x] -> x
             xs  -> unsafeConj xs
      Delay h -> Delay (go h)
      _ | isSel g -> success
        | otherwise -> g
    isSel (V x :=: C _ ts) = x == v && all isRen ts
    isSel (C _ ts :=: V x) = x == v && all isRen ts
    isSel _ = False
    isRen (V y) = M.member y ren
    isRen _     = False
    rn x = fromMaybe x (M.lookup x ren)
    rename = fmap rn

------------------------------------------------------------------------------

size :: G X -> Int
size g = case g of
  _ :=: _            -> 1
  Invoke _ _         -> 1
  Conjunction a b cs -> sum (map size (a : b : cs))
  Disjunction a b cs -> sum (map size (a : b : cs))
  Fresh _ h          -> size h
  Delay h            -> size h
