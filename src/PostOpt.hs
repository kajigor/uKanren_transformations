-- | Post-specialization optimizer for residual miniKanren programs.
--
--   The passes are semantics preserving w.r.t. the set of answers (they may
--   change the order in which an interleaving search enumerates answers):
--
--   * 'normalizeG'      : flattening, success/fail simplification;
--   * existential unification elimination  (fresh v. v == t & G  ==>  G[t/v]),
--     constructor decomposition and clash detection;
--   * 'failureAnalysis' : least fixpoint "may succeed" analysis; calls to
--     relations which can never succeed are replaced by failure;
--   * 'inlineNonRec'    : post-unfolding of non-recursive definitions
--     (in the spirit of ECCE's determinate post-unfolding);
--   * 'mergeDuplicates' : coarsest partition refinement over definitions,
--     merging definitions equal up to renaming and equivalence of callees
--     (a bisimulation-based duplicate removal);
--   * 'removeUnreachable'.
module PostOpt
  ( optimize
  , optimizeWith
  , Pass (..)
  , allPasses
  , normalizeG
  , closeExistentials
  ) where

import           Control.Monad.State
import           Control.Monad       (foldM)
import           Data.Graph          (SCC (..), stronglyConnComp)
import           Data.List           (foldl', nub, partition, (\\))
import qualified Data.Map.Strict     as M
import           Data.Maybe          (fromMaybe, mapMaybe)
import qualified Data.Set            as S
import           Def
import           Syntax

data Pass = PNormalize | PFailure | PUnifElim | PInline | PMerge | PUnreachable
  deriving (Show, Eq, Ord, Enum, Bounded)

allPasses :: [Pass]
allPasses = [minBound .. maxBound]

type Prog = ([Def G X], G X)

optimize :: [X] -> Prog -> Prog
optimize = optimizeWith allPasses

-- | Run the selected passes to a fixpoint (at most a few rounds).  @top@ are
--   the free variables of the goal (query variables).
optimizeWith :: [Pass] -> [X] -> Prog -> Prog
optimizeWith ps top p0 = go (6 :: Int) (prep p0)
  where
    on p = p `elem` ps
    prep (ds, g) = (map (\(Def n as b) -> Def n as (closeExistentials as b)) ds, closeExistentials top g)
    go 0 p = p
    go k p =
      let p' = step p in if size p' == size p && p' == p then p else go (k - 1) p'
    size (ds, g) = (length ds, sum (map (gsize . getBody) ds) + gsize g)
    step p = foldl' (\acc f -> f acc) p
      [ when' PNormalize  normalizeP
      , when' PUnifElim   unifElimP
      , when' PNormalize  normalizeP
      , when' PFailure    failureAnalysis
      , when' PNormalize  normalizeP
      , when' PInline     (reclose . inlineNonRec)
      , when' PUnifElim   unifElimP
      , when' PNormalize  normalizeP
      , when' PMerge      mergeDuplicates
      , when' PUnreachable removeUnreachable
      ]
    when' p f = if on p then f else id
    unifElimP (ds, g) = (map (\(Def n as b) -> Def n as (unifElim (S.fromList as) b)) ds, unifElim (S.fromList top) g)
    reclose (ds, g) = (map (\(Def n as b) -> Def n as (closeExistentials as b)) ds, closeExistentials top g)
    normalizeP (ds, g) = (map (\(Def n as b) -> Def n as (normalizeG b)) ds, normalizeG g)

gsize :: G a -> Int
gsize (_ :=: _)            = 1
gsize (Invoke _ _)         = 1
gsize (Conjunction a b cs) = sum (map gsize (a : b : cs))
gsize (Disjunction a b cs) = sum (map gsize (a : b : cs))
gsize (Fresh _ x)          = gsize x
gsize (Delay x)            = gsize x

------------------------------------------------------------------------------
-- Helpers on goals

isFailG :: G a -> Bool
isFailG (Invoke n []) = n == failureName
isFailG _             = False

isSuccG :: G a -> Bool
isSuccG (Invoke n []) = n == successName
isSuccG _             = False

conjs :: G a -> [G a]
conjs (Conjunction a b cs) = concatMap conjs (a : b : cs)
conjs g                    = [g]

disjs :: G a -> [G a]
disjs (Disjunction a b cs) = concatMap disjs (a : b : cs)
disjs g                    = [g]

mkConj :: [G a] -> G a
mkConj gs
  | any isFailG gs = failure
  | otherwise = case filter (not . isSuccG) (concatMap conjs gs) of
      []  -> success
      [g] -> g
      xs  -> unsafeConj xs

mkDisj :: [G a] -> G a
mkDisj gs = case filter (not . isFailG) (concatMap disjs gs) of
  []  -> failure
  [g] -> g
  xs  -> unsafeDisj xs

normalizeG :: G X -> G X
normalizeG (Conjunction a b cs) = mkConj (map normalizeG (a : b : cs))
normalizeG (Disjunction a b cs) = mkDisj (map normalizeG (a : b : cs))
normalizeG (Fresh x g) =
  case normalizeG g of
    g' | isFailG g' -> failure
       | x `notElem` fv g' -> g'
       | otherwise -> Fresh x g'
normalizeG (Delay g) = normalizeG g
normalizeG g@(V x :=: V y) | x == y = success
normalizeG g = g

substT :: M.Map X (Term X) -> Term X -> Term X
substT s t@(V x)  = fromMaybe t (M.lookup x s)
substT s (C f ts) = C f (map (substT s) ts)

-- | Substitution on goals; binders are assumed to be distinct from the
--   variables of the substitution's range (guaranteed by 'closeExistentials').
substG :: M.Map X (Term X) -> G X -> G X
substG s (a :=: b)            = substT s a :=: substT s b
substG s (Invoke f ts)        = Invoke f (map (substT s) ts)
substG s (Conjunction a b cs) = unsafeConj (map (substG s) (a : b : cs))
substG s (Disjunction a b cs) = unsafeDisj (map (substG s) (a : b : cs))
substG s (Fresh x g)          = Fresh x (substG (M.delete x s) g)
substG s (Delay g)            = Delay (substG s g)

------------------------------------------------------------------------------
-- Fresh-variable normal form: binders get unique names and existentials are
-- pushed as deep as possible:  ex x.(A & B) = A & ex x.B  if x not in A, and
-- ex x.(B1 | B2) = ex x.B1 | ex x.B2.

stripFresh :: G X -> State Int (G X)
stripFresh = go M.empty
  where
    go m (Fresh x g) = do
      n <- get
      put (n + 1)
      go (M.insert x ("_l" ++ show n) m) g
    go m (a :=: b)            = return (ren m a :=: ren m b)
    go m (Invoke f ts)        = return (Invoke f (map (ren m) ts))
    go m (Conjunction a b cs) = unsafeConj <$> mapM (go m) (a : b : cs)
    go m (Disjunction a b cs) = unsafeDisj <$> mapM (go m) (a : b : cs)
    go m (Delay g)            = go m g
    ren m = fmap (\x -> fromMaybe x (M.lookup x m))

closeExistentials :: [X] -> G X -> G X
closeExistentials bound g =
  let g' = evalState (stripFresh g) 0 in place (S.fromList bound) (normalizeG g')

place :: S.Set X -> G X -> G X
place bound g = case g of
    Disjunction {} -> mkDisj (map (place bound) (disjs g))
    Conjunction {} ->
      let cs = conjs g
          free = nub (fv g) \\ S.toList bound
          occ v = length (filter (\c -> v `elem` fv c) cs)
          isDisj Disjunction {} = True
          isDisj _ = False
          here = [ v | v <- free, occ v > 1 || any (\c -> not (isDisj c) && v `elem` fv c) cs ]
          bound' = foldr S.insert bound here
      in fresh here (mkConj (map (\c -> if isDisj c then place bound' c else c) cs))
    _ -> fresh (nub (fv g) \\ S.toList bound) g

------------------------------------------------------------------------------
-- Existential unification elimination.
--   fresh vs. (... & v == t & ...)  ==>  fresh vs\{v}. (...)[t/v]   (v not in t)
-- plus constructor decomposition / clash detection on every conjunct.

decompose :: G X -> G X
decompose (a :=: b) = go a b
  where
    go (V x) (V y) | x == y = success
    go (C f as) (C g bs)
      | f == g && length as == length bs = mkConj (zipWith go as bs)
      | otherwise = failure
    go x y = x :=: y
decompose g = g

unifElim :: S.Set X -> G X -> G X
unifElim _ g0 = go g0
  where
    go g = case g of
      Fresh {} ->
        let (vs, body) = collect g
            body' = go body
        in elim vs (conjs body')
      Conjunction {} -> mkConj (map go (conjs g))
      Disjunction {} -> mkDisj (map go (disjs g))
      Delay h -> go h
      _ -> decompose g
    collect (Fresh x h) = let (vs, b) = collect h in (x : vs, b)
    collect h = ([], h)
    elim vs cs =
      let cs' = concatMap (conjs . decompose) cs in
      if any isFailG cs' then failure else
      case pick vs cs' of
        Nothing -> fresh vs (mkConj cs')
        Just (v, t, rest) ->
          let s = M.singleton v t in
          elim (filter (/= v) vs) (map (normalizeG . substG s) rest)
    pick vs cs = firstJust [ (v, t, before ++ after)
                           | (before, c, after) <- splits cs
                           , Just (v, t) <- [binding vs c] ]
    binding vs (V x :=: t) | x `elem` vs && x `notElem` fv t = Just (x, t)
    binding vs (t :=: V x) | x `elem` vs && x `notElem` fv t = Just (x, t)
    binding _ _ = Nothing
    splits xs = [ (take i xs, xs !! i, drop (i + 1) xs) | i <- [0 .. length xs - 1] ]
    firstJust (x : _) = Just x
    firstJust [] = Nothing

------------------------------------------------------------------------------
-- Failure analysis

-- | Local static check: is this conjunction of unifications satisfiable?
--   (variables treated as unknowns; calls ignored)
consistent :: [G X] -> Bool
consistent cs = case foldM u M.empty [ (a, b) | a :=: b <- cs ] of
    Nothing -> False
    Just _  -> True
  where
    u s (a, b) = unifyX s a b
    walkX s t@(V x) = maybe t (walkX s) (M.lookup x s)
    walkX _ t = t
    occ s x t = case walkX s t of
      V y -> x == y
      C _ ts -> any (occ s x) ts
    unifyX s a b = case (walkX s a, walkX s b) of
      (V x, V y) | x == y -> Just s
      (V x, t) -> if occ s x t then Nothing else Just (M.insert x t s)
      (t, V y) -> if occ s y t then Nothing else Just (M.insert y t s)
      (C f as, C g bs) | f == g && length as == length bs ->
        foldM (\s' (p, q) -> unifyX s' p q) s (zip as bs)
      _ -> Nothing

failureAnalysis :: Prog -> Prog
failureAnalysis (ds, g) =
    let ms = lfp S.empty
        prune = normalizeG . pruneG ms
    in (map (\(Def n as b) -> Def n as (prune b)) ds, prune g)
  where
    lfp ms =
      let ms' = S.fromList [ getName d | d <- ds, may ms (getBody d) ] in
      if ms' == ms then ms else lfp ms'
    may ms h = case h of
      Invoke f _ | isSuccG h -> True
                 | isFailG h -> False
                 | otherwise -> f `S.member` ms
      _ :=: _ -> consistent [h]
      Conjunction {} -> let cs = flatten h in consistent cs && all (may ms) [ c | c <- cs, notUnif c ]
      Disjunction {} -> any (may ms) (disjs h)
      Fresh _ b -> may ms b
      Delay b -> may ms b
    -- conjunctions under fresh are flattened for the consistency check
    flatten h = concatMap (\c -> case c of Fresh _ b -> flatten b; _ -> [c]) (conjs h)
    notUnif (_ :=: _) = False
    notUnif _ = True
    pruneG ms h = case h of
      Invoke f _ | isSuccG h || isFailG h -> h
                 | f `S.member` ms -> h
                 | otherwise -> failure
      Conjunction {} ->
        let cs = map (pruneG ms) (conjs h) in
        if consistent (concatMap unifsOf cs) then mkConj cs else failure
      Disjunction {} -> mkDisj (map (pruneG ms) (disjs h))
      Fresh x b -> Fresh x (pruneG ms b)
      Delay b -> pruneG ms b
      _ :=: _ -> if consistent [h] then h else failure
    unifsOf c@(_ :=: _) = [c]
    unifsOf (Fresh _ b) = [ c | c@(_ :=: _) <- conjs b ]
    unifsOf _ = []

------------------------------------------------------------------------------
-- Call graph utilities

callees :: G a -> [Name]
callees (Invoke f _)         = [f]
callees (Conjunction a b cs) = concatMap callees (a : b : cs)
callees (Disjunction a b cs) = concatMap callees (a : b : cs)
callees (Fresh _ g)          = callees g
callees (Delay g)            = callees g
callees _                    = []

removeUnreachable :: Prog -> Prog
removeUnreachable (ds, g) =
  let dm = M.fromList [ (getName d, d) | d <- ds ]
      reach seen [] = seen
      reach seen (f : fs)
        | f `S.member` seen = reach seen fs
        | otherwise = case M.lookup f dm of
            Nothing -> reach seen fs
            Just d  -> reach (S.insert f seen) (callees (getBody d) ++ fs)
      r = reach S.empty (callees g)
  in (filter ((`S.member` r) . getName) ds, g)

------------------------------------------------------------------------------
-- Post-unfolding of non-recursive definitions.

inlineThreshold :: Int
inlineThreshold = 12

inlineNonRec :: Prog -> Prog
inlineNonRec (ds, g) =
    let ds' = map (\(Def n as b) -> Def n as (evalState (inl b) 0)) ds
        g'  = evalState (inl g) 0
    in (ds', g')
  where
    dm = M.fromList [ (getName d, d) | d <- ds ]
    sccs = stronglyConnComp [ (getName d, getName d, nub (callees (getBody d))) | d <- ds ]
    recursive = S.fromList (concat [ ns | CyclicSCC ns <- sccs ])
    useCount = M.fromListWith (+) [ (f, 1 :: Int) | b <- g : map getBody ds, f <- callees b ]
    inlinable f = case M.lookup f dm of
      Just d -> not (f `S.member` recursive)
                && (gsize (getBody d) <= inlineThreshold || M.findWithDefault 0 f useCount <= 1)
      Nothing -> False
    inl :: G X -> State Int (G X)
    inl h = case h of
      Invoke f ts | inlinable f -> do
        let Def _ ps body = dm M.! f
        body' <- renameBinders body
        -- inlined body may itself contain inlinable calls (non-recursive => terminates)
        inl (substG (M.fromList (zip ps ts)) body')
      Conjunction a b cs -> unsafeConj <$> mapM inl (a : b : cs)
      Disjunction a b cs -> unsafeDisj <$> mapM inl (a : b : cs)
      Fresh x b -> Fresh x <$> inl b
      Delay b -> inl b
      _ -> return h
    renameBinders :: G X -> State Int (G X)
    renameBinders = go M.empty
      where
        go m (Fresh x b) = do
          n <- get
          put (n + 1)
          let x' = "_i" ++ show n
          Fresh x' <$> go (M.insert x x' m) b
        go m (a :=: b) = return (r m a :=: r m b)
        go m (Invoke f ts) = return (Invoke f (map (r m) ts))
        go m (Conjunction a b cs) = unsafeConj <$> mapM (go m) (a : b : cs)
        go m (Disjunction a b cs) = unsafeDisj <$> mapM (go m) (a : b : cs)
        go m (Delay b) = go m b
        r m = fmap (\x -> fromMaybe x (M.lookup x m))

------------------------------------------------------------------------------
-- Duplicate definition merging by partition refinement.

-- | Canonical form of a definition body: arguments renamed positionally,
--   binders in order of appearance, callee names replaced by class ids.
canonical :: M.Map Name Int -> Def G X -> String
canonical cls (Def _ as b) =
    show (length as) ++ ":" ++ evalState (go (M.fromList (zip as [ "a" ++ show i | i <- [0 :: Int ..] ])) b) 0
  where
    go m h = case h of
      Fresh x g -> do
        n <- get
        put (n + 1)
        let x' = "f" ++ show n
        s <- go (M.insert x x' m) g
        return ("E" ++ x' ++ "." ++ s)
      a :=: c -> return (t m a ++ "=" ++ t m c)
      Invoke f ts -> return ("@" ++ callName f ++ "(" ++ concatMap ((++ ",") . t m) ts ++ ")")
      Conjunction a c cs -> do ss <- mapM (go m) (a : c : cs); return ("&(" ++ concatMap (++ ";") ss ++ ")")
      Disjunction a c cs -> do ss <- mapM (go m) (a : c : cs); return ("|(" ++ concatMap (++ ";") ss ++ ")")
      Delay g -> go m g
    callName f = maybe ("!" ++ f) (\i -> "#" ++ show i) (M.lookup f cls)
    t m (V x) = fromMaybe ("?" ++ x) (M.lookup x m)
    t m (C f ts) = f ++ "(" ++ concatMap ((++ ",") . t m) ts ++ ")"

mergeDuplicates :: Prog -> Prog
mergeDuplicates (ds, g) =
    let final = refine (M.fromList [ (getName d, 0) | d <- ds ])
        -- choose first definition (in program order) as representative
        repOf = M.fromList [ (c, n) | (c, n) <- firstPerClass final ]
        rn f = case M.lookup f final of
          Just c -> repOf M.! c
          Nothing -> f
        keep = S.fromList (M.elems repOf)
        ds' = [ Def n as (renameCalls rn b) | Def n as b <- ds, n `S.member` keep ]
    in (ds', renameCalls rn g)
  where
    refine cls =
      let keys = [ (getName d, (cls M.! getName d, canonical cls d)) | d <- ds ]
          ids = M.fromList (zip (nub (map snd keys)) [0 :: Int ..])
          cls' = M.fromList [ (n, ids M.! k) | (n, k) <- keys ]
      in if M.size (M.fromList (map swap (M.toList cls'))) == M.size (M.fromList (map swap (M.toList cls)))
         then cls' else refine cls'
    swap (a, b) = (b, a)
    firstPerClass final = foldl' (\acc d -> let c = final M.! getName d in
                                    if c `elem` map fst acc then acc else acc ++ [(c, getName d)]) [] ds

renameCalls :: (Name -> Name) -> G X -> G X
renameCalls rn h = case h of
  Invoke f ts -> Invoke (rn f) ts
  Conjunction a b cs -> unsafeConj (map (renameCalls rn) (a : b : cs))
  Disjunction a b cs -> unsafeDisj (map (renameCalls rn) (a : b : cs))
  Fresh x b -> Fresh x (renameCalls rn b)
  Delay b -> renameCalls rn b
  _ -> h
