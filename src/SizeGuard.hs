-- | Size invariants and size guards ("+guard").
--
--   Backward runs often find all their answers and then search forever: in
--   @rev x acc y@ run with @y@ known, the accumulator grows without bound
--   although no answer can have @|acc| > |y|@.  This pass
--
--   1. infers, for every relation, invariants @|p_i| <= |p_j|@ and
--      @|p_i| <= K@ that hold for every answer, where @|.|@ is the list
--      length (Nil/Cons) or the value of a Peano number (O/S, Zero/Succ); the
--      family of a parameter is the constructor set computed by
--      'Index.analyse'.  The invariants are the greatest set such that every
--      clause establishes each of them, assuming them for its calls: each
--      conjunctive path is checked with a difference-constraint solver
--      (shortest paths over @x - y <= c@).  This is a valid simultaneous
--      inductive invariant of the least model;
--
--   2. puts a guard @size_le A B@ in front of every recursive call @g ts@ for
--      which an invariant @|t_k| <= |t_l|@ (or @<= K@) holds and the bound
--      @t_l@ is ground (a constant term, or a parameter that is ground in the
--      mode of the enclosing copy, see 'Schedule').  The guard recurses on its
--      ground second argument, so it always terminates; every answer satisfies
--      the invariant, so it only cuts branches without answers; when @A@'s
--      spine is unbound, its successes bind @A@ to different lengths, so it
--      does not duplicate answers.
module SizeGuard
  ( guardProgram
  , invariants
  , Inv (..)
  ) where

import           Data.Char       (isDigit)
import           Data.List       (foldl', isPrefixOf, nub, tails)
import qualified Data.Map.Strict as M
import           Data.Maybe      (fromMaybe, isJust)
import qualified Data.Set        as S
import           Def
import qualified Index
import           Syntax

type Prog = ([Def G X], G X)

data Fam = Fam { fLeaf :: Name, fSpine :: Name } deriving (Eq, Ord, Show)

families :: [Fam]
families = [Fam "Nil" "Cons", Fam "O" "S", Fam "Zero" "Succ"]

-- | Bound of an invariant: another parameter or a constant.
data Bound = BParam Int | BConst Int deriving (Eq, Ord, Show)
-- | @Inv f k b@: in every answer of @f@, @|arg k| <= b@.
data Inv = Inv Name Int Bound deriving (Eq, Ord, Show)

famOf :: Index.CSet -> Maybe Fam
famOf (Just cs) | not (S.null cs) =
  case [ f | f <- families, S.isSubsetOf (S.map fst cs) (S.fromList [fLeaf f, fSpine f]) ] of
    (f : _) -> Just f
    []      -> Nothing
famOf _ = Nothing

termFam :: Term X -> Maybe Fam
termFam (C c _) = case [ f | f <- families, c `elem` [fLeaf f, fSpine f] ] of
  (f : _) -> Just f
  []      -> Nothing
termFam _ = Nothing

------------------------------------------------------------------------------
-- Sizes along a conjunctive path

-- | A size: a node (variable representative, or Nothing = the constant 0)
--   plus an offset.
type Size = (Maybe X, Int)

data PathInfo = PathInfo
  { piRep   :: X -> X                      -- union-find of var = var
  , piBind  :: M.Map X [Term X]            -- bindings x = C ...
  , piClash :: Bool                        -- inconsistent constructors: infeasible
  }

pathInfo :: [G X] -> PathInfo
pathInfo cs =
  let eqs = concat [ pairs a b | a :=: b <- cs ]
      pairs (V x) (V y) = [Left (x, y)]
      pairs (V x) t     = [Right (x, t)]
      pairs t (V x)     = [Right (x, t)]
      pairs (C c as) (C d bs)
        | c == d && length as == length bs = concat (zipWith pairs as bs)
        | otherwise = [Left ("#clash", "#clash'")]
      clash0 = any (== Left ("#clash", "#clash'")) eqs
      uf = foldl' (\m e -> case e of
                       Left (x, y) | x /= "#clash" -> union m x y
                       _ -> m) M.empty eqs
      rep x = case M.lookup x uf of
        Just y | y /= x -> rep y
        _ -> x
      binds = M.fromListWith (++) [ (rep x, [t]) | Right (x, t) <- eqs ]
      topClash = any (\ts -> length (nub [ c | C c _ <- ts ]) > 1) (M.elems binds)
  in PathInfo rep binds (clash0 || topClash)
  where
    union m x y = let rx = find m x; ry = find m y in if rx == ry then m else M.insert rx ry m
    find m x = case M.lookup x m of
      Just y | y /= x -> find m y
      _ -> x

-- | Size of a term in a family, resolving variables through the bindings
--   (bounded depth, against cyclic bindings).
sizeOf :: PathInfo -> Fam -> Term X -> Maybe Size
sizeOf pinfo fam = go (40 :: Int)
  where
    go 0 _ = Nothing
    go d t = case t of
      C c [] | c == fLeaf fam -> Just (Nothing, 0)
      C c as | c == fSpine fam, not (null as) -> fmap (fmap (+ 1)) (go (d - 1) (last as))
      C _ _ -> Nothing
      V x -> let r = piRep pinfo x in
             case [ b | b@(C _ _) <- fromMaybe [] (M.lookup r (piBind pinfo)) ] of
               (b : _) -> go (d - 1) b
               []      -> Just (Just r, 0)

-- | Difference constraints @u - v <= c@ as edges, checked by Floyd-Warshall.
type Constraint = (Maybe X, Maybe X, Int)   -- (u, v, c): x_u - x_v <= c

entails :: [Constraint] -> Constraint -> Bool
entails cs (u, v, c) =
  let nodes = nub (Nothing : concat [ [a, b] | (a, b, _) <- cs ] ++ [u, v])
      -- x >= 0 for every variable: 0 - x <= 0
      base = cs ++ [ (Nothing, Just x, 0) | Just x <- nodes ]
      inf = maxBound `div` 4 :: Int
      d0 = M.fromListWith min ([ ((b, a), w) | (a, b, w) <- base ] ++ [ ((n, n), 0) | n <- nodes ])
      get m k = fromMaybe inf (M.lookup k m)
      fw m k = M.fromList [ ((i, j), min (get m (i, j)) (get m (i, k) + get m (k, j))) | i <- nodes, j <- nodes ]
      dist = foldl' fw d0 nodes
      negCycle = any (\n -> get dist (n, n) < 0) nodes
  in negCycle || get dist (v, u) <= c

------------------------------------------------------------------------------
-- Invariant inference

invariants :: Prog -> M.Map Name [Inv]
invariants prog@(ds, _) = M.fromListWith (++) [ (f, [i]) | i@(Inv f _ _) <- S.toList final ]
  where
    tbl = Index.analyse prog
    defs = M.fromList [ (getName d, d) | d <- ds ]
    famP f i = famOf (fromMaybe Nothing (M.lookup (f, i) tbl))
    cands = S.fromList $ concat
      [ [ Inv f i (BParam j) | (j, _) <- zip [0 ..] as, j /= i, famP f j == Just fam ]
        ++ [ Inv f i (BConst k) | k <- nub (constSizes fam body) ]
      | Def f as body <- ds, (i, _) <- zip [0 :: Int ..] as, Just fam <- [famP f i] ]
    constSizes fam body = [ n | p <- Index.paths body, a :=: b <- p, t <- [a, b], isGround t, termFam t == Just fam
                              , Just (Nothing, n) <- [sizeOf (pathInfo []) fam t] ]
    final = go cands
    go s = let s' = S.filter (holds s) s in if s' == s then s else go s'
    holds s inv@(Inv f i b) = case M.lookup f defs of
      Nothing -> False
      Just (Def _ as body) -> all (pathOk s inv as) (Index.paths body)
    pathOk s (Inv f i b) as p =
      let pinfo = pathInfo p
          Just fam = famP f i
          -- the invariants assumed for the calls on the path
          hyps = [ c | Invoke g ts <- p, Inv g' k bd <- S.toList s, g' == g, k < length ts
                     , Just fk <- [famP g k], Just c <- [constraint pinfo fk (ts !! k) bd ts] ]
          lhs = sizeOf pinfo fam (V (as !! i))
          goalC = case (lhs, b) of
            (Just (nu, ku), BParam j) -> do
              (nv, kv) <- sizeOf pinfo fam (V (as !! j))
              return (nu, nv, kv - ku)
            (Just (nu, ku), BConst k) -> Just (nu, Nothing, k - ku)
            _ -> Nothing
      in piClash pinfo || maybe False (entails hyps) goalC
    -- the hypothesis of a call: |t_k| <= bound, as a constraint
    constraint pinfo fam tk bd ts = do
      (nu, ku) <- sizeOf pinfo fam tk
      case bd of
        BParam l | l < length ts -> do
          (nv, kv) <- sizeOf pinfo fam (ts !! l)
          return (nu, nv, kv - ku)
        BConst k -> return (nu, Nothing, k - ku)
        _ -> Nothing

isGround :: Term X -> Bool
isGround (V _)    = False
isGround (C _ ts) = all isGround ts

------------------------------------------------------------------------------
-- Guards

guardName :: Fam -> Name
guardName fam = "size_le_" ++ fSpine fam

guardDef :: Fam -> Def G X
guardDef fam =
  let g = guardName fam
      lf = C (fLeaf fam) []
      sp h t = C (fSpine fam) (if fSpine fam == "Cons" then [V h, V t] else [V t])
  in Def g ["a", "b"] $
       unsafeDisj
         [ unsafeConj [V "b" :=: lf, V "a" :=: lf]
         , fresh ["bh", "bt"] $ unsafeConj
             [ V "b" :=: sp "bh" "bt"
             , unsafeDisj [ V "a" :=: lf
                          , fresh ["ah", "at"] (unsafeConj [V "a" :=: sp "ah" "at", Invoke g [V "at", V "bt"]]) ] ] ]

-- | Ground term of size @k@ in a family (the bound of a constant invariant).
constTerm :: Fam -> Int -> Term X
constTerm fam k = iterate (\t -> C (fSpine fam) (if fSpine fam == "Cons" then [C (fLeaf fam) [], t] else [t])) (C (fLeaf fam) []) !! k

-- | Ground parameter positions of a mode copy, from its name (@f__012@).
modeOf :: Name -> S.Set Int
modeOf n = case [ drop 2 t | t <- tails n, "__" `isPrefixOf` t ] of
  [] -> S.empty
  ss -> let suf = last ss in if not (null suf) && all isDigit suf then S.fromList (map (read . (: [])) suf) else S.empty

guardProgram :: Prog -> Prog
guardProgram prog@(ds, goal) =
  let invs = invariants prog
      tbl = Index.analyse prog
      famP f i = famOf (fromMaybe Nothing (M.lookup (f, i) tbl))
      sccOf = sccIndex ds
      ds' = [ Def f as (insertGuards f as body) | Def f as body <- ds ]
      insertGuards f as body =
        let ground = S.fromList [ a | (i, a) <- zip [0 ..] as, i `S.member` modeOf f ]
        in go ground body
        where
          go ground g = case g of
            Conjunction a b cs -> unsafeConj (concatMap (withGuard ground) (a : b : cs))
            Disjunction a b cs -> unsafeDisj (map (go ground) (a : b : cs))
            Fresh x h          -> Fresh x (go ground h)
            Delay h            -> Delay (go ground h)
            _                  -> unsafeConj' (withGuard ground g)
          unsafeConj' [x] = x
          unsafeConj' xs  = unsafeConj xs
          withGuard ground c = case c of
            Invoke g ts | M.lookup g sccOf == M.lookup f sccOf, isJust (M.lookup g sccOf) ->
              [ Invoke (guardName fam) [tk, bterm]
              | Inv _ k bd <- fromMaybe [] (M.lookup g invs), Just fam <- [famP g k]
              , k < length ts, let tk = ts !! k, not (isGround tk), not (isGroundVar ground tk)
              , Just bterm <- [boundTerm ground fam bd ts] ] ++ [c]
            Conjunction {} -> [go ground c]
            Disjunction {} -> [go ground c]
            Fresh {}       -> [go ground c]
            _              -> [c]
          isGroundVar ground (V x) = x `S.member` ground
          isGroundVar _ _          = False
          boundTerm ground fam bd ts = case bd of
            BConst k -> Just (constTerm fam k)
            BParam l -> let t = ts !! l in if isGround t || isGroundVar ground t then Just t else Nothing
      guardNames = S.fromList [ n | Def _ _ b <- ds', n <- calls b, "size_le_" `isPrefixOf` n ]
      guards = [ guardDef fam | fam <- families, guardName fam `S.member` guardNames ]
  in (ds' ++ guards, goal)

calls :: G X -> [Name]
calls g = case g of
  Invoke f _         -> [f]
  Conjunction a b cs -> concatMap calls (a : b : cs)
  Disjunction a b cs -> concatMap calls (a : b : cs)
  Fresh _ h          -> calls h
  Delay h            -> calls h
  _                  -> []

-- | Strongly connected components of the call graph (index per relation).
sccIndex :: [Def G X] -> M.Map Name Int
sccIndex ds =
  let names = map getName ds
      edges = M.fromList [ (getName d, nub (filter (`elem` names) (calls (getBody d)))) | d <- ds ]
      reach n = go S.empty (fromMaybe [] (M.lookup n edges))
        where go seen [] = seen
              go seen (x : xs) | x `S.member` seen = go seen xs
                               | otherwise = go (S.insert x seen) (fromMaybe [] (M.lookup x edges) ++ xs)
      r = M.fromList [ (n, reach n) | n <- names ]
      comp n = [ m | m <- names, n `S.member` (r M.! m) && m `S.member` (r M.! n) ]
      recursive n = n `S.member` (r M.! n)
      groups = nub [ comp n | n <- names, recursive n ]
  in M.fromList [ (n, i) | (i, g) <- zip [0 ..] groups, n <- g ]
