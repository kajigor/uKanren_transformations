-- | Mode-directed conjunct scheduling for (residual) miniKanren programs.
--
--   Given a goal and the set of variables that are ground when it is run, the
--   pass produces a polyvariant program: one copy of a relation per reachable
--   call mode (set of ground argument positions), with the conjunctions of
--   each copy reordered greedily:
--
--   1. unifications first (cheap, never diverge, propagate groundness);
--   2. calls estimated to terminate in their mode (structural recursion on a
--      ground argument), preferring calls with more ground arguments;
--   3. remaining calls in their original order.
--
--   Groundness propagation uses a success-pattern analysis: for every
--   reachable (relation, call mode) pair the set of argument positions that
--   are ground on success (computed as a fixpoint starting from "no answers",
--   i.e. all positions ground).
--
--   Conjunction is commutative w.r.t. the set of answers of a complete
--   (interleaving) search, so the transformation preserves answers; it only
--   changes the cost and termination behaviour of the search.
module Schedule
  ( schedule
  , scheduleWith
  , semidet
  ) where

import           Data.List       (foldl', nub, sortOn)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe)
import qualified Data.Set        as S
import           Def
import           Syntax

type Mode = S.Set Int          -- ground argument positions (0-based)
type Defs = M.Map Name (Def G X)
type Succ = M.Map (Name, Mode) Mode

isSpecial :: Name -> [a] -> Bool
isSpecial f as = null as && (f == successName || f == failureName)

conjs :: G a -> [G a]
conjs (Conjunction a b cs) = concatMap conjs (a : b : cs)
conjs g                    = [g]

disjs :: G a -> [G a]
disjs (Disjunction a b cs) = concatMap disjs (a : b : cs)
disjs g                    = [g]

varsT :: Term X -> [X]
varsT = fv

groundT :: S.Set X -> Term X -> Bool
groundT gr t = all (`S.member` gr) (varsT t)

callMode :: S.Set X -> [Term X] -> Mode
callMode gr ts = S.fromList [ i | (i, t) <- zip [0 ..] ts, groundT gr t ]

------------------------------------------------------------------------------
-- Groundness propagation through a goal, given a success table.

-- | Ground variables after a (non-reordered) goal.
propagate :: Succ -> S.Set X -> G X -> S.Set X
propagate tbl gr g = case g of
  a :=: b -> let gr' = propU gr a b in if gr' == gr then gr else propU gr' a b
  Invoke f ts
    | isSpecial f ts -> gr
    | otherwise ->
        let m = callMode gr ts
            out = M.findWithDefault (S.fromList [0 .. length ts - 1]) (f, m) tbl
        in foldr S.insert gr (concat [ varsT t | (i, t) <- zip [0 ..] ts, i `S.member` out ])
  -- after the whole conjunction succeeded every conjunct holds, so the
  -- result does not depend on the order: iterate to a fixpoint (a single
  -- left-to-right pass missed  qs = h :: t & ground h & ground t  ==> ground qs)
  Conjunction {} ->
    let pass gr0 = foldl' (propagate tbl) gr0 (conjs g)
        fixp gr0 = let gr1 = pass gr0 in if gr1 == gr0 then gr0 else fixp gr1
    in fixp gr
  Disjunction {} -> foldr1 S.intersection (map (propagate tbl gr) (disjs g))
  Fresh _ h -> propagate tbl gr h
  Delay h -> propagate tbl gr h

-- | Groundness through one unification, decomposing constructor terms.
propU :: S.Set X -> Term X -> Term X -> S.Set X
propU gr a b
  | groundT gr a = foldr S.insert gr (varsT b)
  | groundT gr b = foldr S.insert gr (varsT a)
  | otherwise = case (a, b) of
      (C f as, C g bs) | f == g && length as == length bs ->
        foldl' (\acc (x, y) -> propU acc x y) gr (zip as bs)
      _ -> gr

------------------------------------------------------------------------------
-- Structural termination estimate.
--
-- A call r(ts) in mode m is considered terminating when every call to a
-- relation of the same strongly connected component, on every path of r's
-- body, receives as a ground argument a variable that is a strict subterm of a
-- ground input of r (obtained through unifications  x == C(.. v ..)).

-- | Variables that are strict subterms of ground inputs, along a conjunction.
smallerVars :: S.Set X -> [G X] -> S.Set X
smallerVars inputs cs = go S.empty
  where
    roots = inputs
    go acc =
      let acc' = foldl' step acc cs in if acc' == acc then acc else go acc'
    step acc (V x :=: t@(C _ _)) | x `S.member` roots || x `S.member` acc = foldr S.insert acc (varsT t)
    step acc (t@(C _ _) :=: V x) | x `S.member` roots || x `S.member` acc = foldr S.insert acc (varsT t)
    step acc _ = acc

------------------------------------------------------------------------------

data Ctx = Ctx { cDefs :: Defs, cScc :: M.Map Name Int, cTbl :: Succ, cDet :: Bool }

-- | Schedule one goal; returns the reordered goal and the (relation, mode)
--   pairs it calls.
schedG :: Ctx -> Name -> S.Set X -> S.Set X -> G X -> (G X, [(Name, Mode)])
schedG ctx self inputs gr g = case g of
  Disjunction {} ->
    let rs = map (schedG ctx self inputs gr) (disjs g)
    in (unsafeDisj (map fst rs), concatMap snd rs)
  Fresh x h -> let (h', cs) = schedG ctx self inputs gr h in (Fresh x h', cs)
  Delay h -> schedG ctx self inputs gr h
  Conjunction {} -> schedConj (conjs g)
  _ -> schedConj [g]
  where
    schedConj cs0 =
      -- flatten fresh inside conjunctions: binders are kept on top
      let (binders, cs) = flattenFresh cs0
          small = smallerVars inputs cs
          go _ [] = ([], [])
          go grd rest =
            let indexed = zip [0 :: Int ..] rest
                best = snd $ head $ sortOn fst [ (score' grd small rest i c, (i, c)) | (i, c) <- indexed ]
                (bi, bc) = best
                rest' = [ c | (i, c) <- indexed, i /= bi ]
                (bc', calls) = case bc of
                  Invoke f ts | not (isSpecial f ts) && M.member f (cDefs ctx) ->
                    let m = callMode grd ts in (Invoke (modeName f m) ts, [(f, m)])
                  Disjunction {} -> schedG ctx self inputs grd bc
                  _ -> (bc, [])
                grd' = propagate (cTbl ctx) grd bc
                (more, calls') = go grd' rest'
            in (bc' : more, calls ++ calls')
          (ordered, called) = go gr cs
      in (fresh binders (mkConj ordered), called)
    mkConj [x] = x
    mkConj xs = unsafeConj xs
    flattenFresh = foldr (\c (bs, cs) -> case c of
                                Fresh {} -> let (vs, b) = collect c
                                                (bs', cs') = flattenFresh (conjs b)
                                            in (vs ++ bs' ++ bs, cs' ++ cs)
                                _ -> (bs, c : cs)) ([], [])
    collect (Fresh x h) = let (vs, b) = collect h in (x : vs, b)
    collect h = ([], h)
    -- In det mode a generator (nondeterministic call or local disjunction)
    -- is only moved forward when it grounds a variable used by another
    -- remaining conjunct: otherwise  (a | b) & rec  would run rec once per
    -- branch where  rec & (a | b)  shares its answers.
    score' grd small rest i c
      | cDet ctx, (k, _, _) <- sc, k == 2, not (helps grd rest c) = (4, 0, i)
      | otherwise = sc
      where sc = score grd small i c
    helps grd rest c =
      let newG = propagate (cTbl ctx) grd c `S.difference` grd
          others = [ o | o <- rest, not (sameGoal o c) ]
      in any (\o -> any (`S.member` newG) (fvG o)) others
    sameGoal a b = show a == show b
    fvG = fv
    score grd small i c = case c of
      _ :=: _ -> (0 :: Int, 0 :: Int, i)
      Invoke f ts
        | isSpecial f ts -> (0, 0, i)
        | otherwise ->
            let m = callMode grd ts
                nGround = S.size m
                rec = M.lookup f (cScc ctx) == M.lookup self (cScc ctx) && M.member f (cScc ctx)
                term = if rec
                         then any (\(j, t) -> j `S.member` m && case t of V v -> v `S.member` small; _ -> False) (zip [0 ..] ts)
                              && terminates ctx f m
                         else terminates ctx f m
                -- non-terminating calls are delayed; ground-heavy calls first
                det = cDet ctx && semidet ctx f m
                -- terminating semidet calls (tests) as early as possible, most
                -- ground first; terminating generators keep the original order
                -- (the programmer's generate/test interleaving); calls that may
                -- not terminate are delayed
            in if not (cDet ctx) then (if term then 1 else 3, negate nGround, i)
               else if not term then (3, 0, i)
               -- cheap first: non-recursive tests before recursive calls
               else if det then (1, fromEnum rec, i)
               -- generators: the most constrained (most ground inputs) first
               else (2, negate nGround, i)
      Disjunction {} -> (2, 0, i)
      _ -> (2, 0, i)

-- | Is relation f terminating in mode m?  For every conjunctive path of the
--   body there must be an order (unifications first, then greedily any call
--   that is itself safe) in which every call is safe: a call into the same
--   strongly connected component must receive a strict subterm of a ground
--   input as a ground argument (structural recursion), a call into another
--   component must be terminating in its mode (well-founded: the component
--   graph is acyclic).
terminates :: Ctx -> Name -> Mode -> Bool
terminates ctx f m = case M.lookup f (cDefs ctx) of
  Nothing -> True
  Just (Def _ as body) ->
    let inputs = S.fromList [ a | (i, a) <- zip [0 ..] as, i `S.member` m ]
        scc = M.lookup f (cScc ctx)
        okPath cs =
          let small = smallerVars inputs cs
              (us, calls) = (filter isU cs, filter (not . isU) cs)
              gr0 = foldl' (propagate (cTbl ctx)) inputs us
              safe gr c = case c of
                Invoke g ts
                  | isSpecial g ts -> True
                  | M.lookup g (cScc ctx) == scc ->
                      let mg = callMode gr ts in
                      any (\(j, t) -> j `S.member` mg && case t of V v -> v `S.member` small; _ -> False) (zip [0 ..] ts)
                  | otherwise -> terminates ctx g (callMode gr ts)
                _ -> True
              go _ [] = True
              go gr rest = case break (safe gr) rest of
                (_, []) -> False
                (before, c : after) -> go (propagate (cTbl ctx) gr c) (before ++ after)
          in go gr0 calls
    in all okPath (branches body)
  where
    isU (_ :=: _) = True
    isU _ = False

------------------------------------------------------------------------------
-- Determinism (semidet: at most one answer) of a relation in a mode.
--
-- Coinductive (a call pattern under analysis is assumed semidet).  A goal is
-- semidet when its conjuncts are (in their modes, groundness propagated left
-- to right) and the branches of every disjunction are pairwise exclusive: some
-- variable that is ground at that point is unified, at the top level of the
-- two branches, with different constructors.

semidet :: Ctx -> Name -> Mode -> Bool
semidet ctx f0 m0 = rel S.empty f0 m0
  where
    rel vis f m
      | (f, m) `S.member` vis = True
      | otherwise = case M.lookup f (cDefs ctx) of
          Nothing -> True
          Just (Def _ as body) ->
            let gr = S.fromList [ a | (i, a) <- zip [0 ..] as, i `S.member` m ]
            in goal (S.insert (f, m) vis) gr body
    goal vis gr g = case g of
      _ :=: _ -> True
      Invoke f ts | isSpecial f ts -> True
                  | otherwise -> rel vis f (callMode gr ts)
      Conjunction {} ->
        fst $ foldl' (\(ok, gr') c -> (ok && goal vis gr' c, propagate (cTbl ctx) gr' c)) (True, gr) (conjs g)
      Disjunction {} ->
        let bs = disjs g
        in and [ exclusive gr a b | (i, a) <- zip [0 :: Int ..] bs, (j, b) <- zip [0 ..] bs, i < j ]
           && all (goal vis gr) bs
      Fresh _ h -> goal vis gr h
      Delay h -> goal vis gr h
    heads gr b = M.fromListWith (\_ x -> x)
      [ (x, c) | u <- topConjs b, Just (x, c) <- [headOf u], x `S.member` gr ]
    headOf (V x :=: C c _) = Just (x, c)
    headOf (C c _ :=: V x) = Just (x, c)
    headOf _ = Nothing
    exclusive gr a b =
      let ha = heads gr a; hb = heads gr b
      in or [ c /= d | (x, c) <- M.toList ha, Just d <- [M.lookup x hb] ]
    topConjs h = case h of
      Fresh _ k -> topConjs k
      Delay k -> topConjs k
      Conjunction {} -> concatMap topConjs (conjs h)
      _ -> [h]

-- | All conjunctive paths of a goal (disjunctive normal form, fresh erased).
branches :: G X -> [[G X]]
branches g = case g of
  Disjunction {} -> concatMap branches (disjs g)
  Conjunction {} -> map concat (mapM branches (conjs g))
  Fresh _ h -> branches h
  Delay h -> branches h
  _ -> [[g]]

modeName :: Name -> Mode -> Name
modeName f m = f ++ "__" ++ concatMap show (S.toList m)

------------------------------------------------------------------------------

-- | Success-pattern table for all (relation, mode) pairs reachable from the
--   goal, computed as a decreasing fixpoint.
successTable :: Defs -> [(Name, Mode)] -> S.Set X -> G X -> Succ
successTable defs seeds gr0 goal = iter M.empty
  where
    iter tbl =
      let pairs = reachable tbl
          tbl' = M.fromList [ (k, out tbl k) | k <- pairs ]
      in if tbl' == tbl then tbl else iter tbl'
    out tbl (f, m) = case M.lookup f defs of
      Nothing -> S.empty
      Just (Def _ as body) ->
        let gr = S.fromList [ a | (i, a) <- zip [0 ..] as, i `S.member` m ]
            gr' = propagate tbl gr body
        in S.fromList [ i | (i, a) <- zip [0 ..] as, a `S.member` gr' ]
    -- reachable call patterns under the current table (non-reordered order
    -- is an under-approximation of groundness; we explore both the plain and
    -- the greedy order by collecting all patterns that appear)
    reachable tbl = S.toList (explore tbl S.empty (seeds ++ callsIn tbl gr0 goal))
    explore _ seen [] = seen
    explore tbl seen (k@(f, m) : ks)
      | k `S.member` seen = explore tbl seen ks
      | otherwise = case M.lookup f defs of
          Nothing -> explore tbl seen ks
          Just (Def _ as body) ->
            let gr = S.fromList [ a | (i, a) <- zip [0 ..] as, i `S.member` m ]
            in explore tbl (S.insert k seen) (callsIn tbl gr body ++ ks)
    callsIn tbl gr g = case g of
      Invoke f ts | not (isSpecial f ts) -> [(f, callMode gr ts)]
      Conjunction {} ->
        -- greedy: unifications first, then calls in order
        let cs = conjs g
            (us, rest) = (filter isU cs, filter (not . isU) cs)
            gr1 = foldl' (propagate tbl) gr us
            -- all calls see at least the unification groundness; also the
            -- groundness after all other conjuncts (for later positions)
            grAll = foldl' (propagate tbl) gr1 rest
        in concatMap (\c -> callsIn tbl gr1 c ++ callsIn tbl grAll c ++ callsSeq tbl gr1 rest c) rest
      Disjunction {} -> concatMap (callsIn tbl gr) (disjs g)
      Fresh _ h -> callsIn tbl gr h
      Delay h -> callsIn tbl gr h
      _ -> []
    callsSeq tbl gr rest c =
      let before = takeWhile (/= c) rest in callsIn tbl (foldl' (propagate tbl) gr before) c
    isU (_ :=: _) = True
    isU _ = False

-- | Entry point.  @ground@: variables of @goal@ that are ground at run time.
schedule :: S.Set X -> ([Def G X], G X) -> ([Def G X], G X)
schedule = scheduleWith False

-- | @useDet@: use the determinism analysis to separate tests from generators.
scheduleWith :: Bool -> S.Set X -> ([Def G X], G X) -> ([Def G X], G X)
scheduleWith useDet ground (ds, goal) =
  let defs = M.fromList [ (getName d, d) | d <- ds ]
      sccIx = M.fromList [ (n, i) | (i, ns) <- zip [0 ..] (sccs ds), n <- ns ]
      run seeds =
        let tbl = successTable defs seeds ground goal
            ctx = Ctx defs sccIx tbl useDet
            (goal', calls0) = schedG ctx "" S.empty ground goal
            build _ [] = ([], [])
            build seen (k@(f, m) : ks)
              | k `S.member` seen = build seen ks
              | otherwise = case M.lookup f defs of
                  Nothing -> build seen ks
                  Just (Def _ as body) ->
                    let inputs = S.fromList [ a | (i, a) <- zip [0 ..] as, i `S.member` m ]
                        (body', cs) = schedG ctx f inputs inputs body
                        (rest, used) = build (S.insert k seen) (cs ++ ks)
                    in (Def (modeName f m) as body' : rest, k : used)
            (ds', used) = build S.empty calls0
        in (ds', goal', used)
      loop :: Int -> [(Name, Mode)] -> ([Def G X], G X)
      loop k seeds =
        let (ds', g', used) = run seeds
            seeds' = nub (seeds ++ used)
        in if k == 0 || length seeds' == length seeds then (ds', g') else loop (k - 1) seeds'
  in loop 4 []

sccs :: [Def G X] -> [[Name]]
sccs ds =
  let names = map getName ds
      edges = M.fromList [ (getName d, nub (callees (getBody d))) | d <- ds ]
      reach n = go S.empty [n]
        where go seen [] = seen
              go seen (x : xs) | x `S.member` seen = go seen xs
                               | otherwise = go (S.insert x seen) (fromMaybe [] (M.lookup x edges) ++ xs)
      r = M.fromList [ (n, reach' n) | n <- names ]
      reach' n = S.fromList (concatMap (\c -> S.toList (reach c)) (fromMaybe [] (M.lookup n edges)))
      comp n = [ m | m <- names, m == n || (m `S.member` (r M.! n) && n `S.member` (r M.! m)) ]
  in nub (map (sortOn id . comp) names)

callees :: G a -> [Name]
callees (Invoke f _)         = [f]
callees (Conjunction a b cs) = concatMap callees (a : b : cs)
callees (Disjunction a b cs) = concatMap callees (a : b : cs)
callees (Fresh _ g)          = callees g
callees (Delay g)            = callees g
callees _                    = []
