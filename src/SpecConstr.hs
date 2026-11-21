-- | Constructor specialization of residual programs ("+spc"), the logic
--   programming analogue of GHC's SpecConstr.
--
--   A call @f t1 .. (C us) .. tn@ whose argument at a /dispatch position/ of
--   @f@ (a parameter that the clauses of @f@ test against constructors) is a
--   constructor term is replaced by @f_C t1 .. us .. tn@, where @f_C@ is @f@
--   with that parameter fixed to @C zs@:
--
--   > f_C .. zs .. = body[p := C zs]
--
--   so @f .. (C us) ..@ and @f_C .. us ..@ have the same answers by
--   construction.  'PostOpt' then decomposes or rejects the unifications with
--   @C zs@, removes relations without answers and inlines non-recursive
--   definitions; inlining exposes new constructor arguments, so the two
--   alternate up to a fixpoint.  On the vanilla self-interpreter this unrolls
--   @jo_solve@ over the literal goal lists of the clause bodies and turns
--   @jo_atom (Jo_r ..)@ into a direct call of the clauses of @r@, i.e. it
--   removes the clause-table dispatch that 'Index' could only make cheaper.
--
--   Termination: at most one specialization per (relation, position,
--   constructor), and at most 'maxNew' new relations per program.
module SpecConstr
  ( specConstr
  , specRound
  ) where

import           Data.List       (foldl', mapAccumL, maximumBy)
import qualified Data.Map.Strict as M
import           Data.Ord        (comparing)
import qualified Data.Set        as S
import           Def
import qualified Index
import qualified PostOpt
import           Syntax

type Prog = ([Def G X], G X)

maxNew :: Int
maxNew = 400

-- | Specialize and optimize, alternating to a fixpoint (bounded rounds).
--   @top@: free variables of the goal, as for 'PostOpt.optimize'.
specConstr :: [X] -> Prog -> Prog
specConstr top p0 = go (24 :: Int) 0 M.empty (PostOpt.optimize top p0)
  where
    go 0 _ _ p = p
    go k ctr made p =
      let (ctr', made', p') = specRound ctr made p
          p'' = PostOpt.optimize top (unfoldSingle (S.fromList (M.elems made')) k p')
      in if p'' == p then p'' else go (k - 1) ctr' made' p''

-- | Unfold, once, every call of a specialization whose body is a single
--   clause (no disjunction).  'PostOpt' inlines only non-recursive relations;
--   the specializations of an interpreter's loop (@solve_Cons h t = atom h &
--   solve t@) are mutually recursive with the clause table, but unfolding one
--   call consumes one element of a literal goal list, so repeating it a
--   bounded number of rounds unrolls the loop over the clause bodies.
unfoldSingle :: S.Set Name -> Int -> Prog -> Prog
unfoldSingle names salt (ds, g) =
  let (k1, ds') = mapAccumL (\k (Def n as b) -> let (k', b') = go k b in (k', Def n as b')) 0 ds
      (_, g') = go k1 g
  in (ds', g')
  where
    dm = M.fromList [ (getName d, d) | d <- ds, getName d `S.member` names, single (getBody d) ]
    single b = case b of
      Disjunction {}     -> False
      Conjunction a c cs -> all single (a : c : cs)
      Fresh _ h          -> single h
      Delay h            -> single h
      _                  -> True
    -- every unfolding renames the binders of its copy apart (round and
    -- occurrence number), so no two copies share a local variable even after
    -- PostOpt lifts the binders into one scope
    go :: Int -> G X -> (Int, G X)
    go k h = case h of
      Invoke f ts | Just (Def _ ps body) <- M.lookup f dm, length ps == length ts ->
        let body' = renameFresh (\x -> x ++ "_u" ++ show salt ++ "_" ++ show k) body
        in (k + 1, foldr (\(p, t) acc -> substVar p t acc) body' (zip ps ts))
      Conjunction a b cs -> let (k', xs) = mapAccumL go k (a : b : cs) in (k', unsafeConj xs)
      Disjunction a b cs -> let (k', xs) = mapAccumL go k (a : b : cs) in (k', unsafeDisj xs)
      Fresh x b          -> let (k', b') = go k b in (k', Fresh x b')
      Delay b            -> let (k', b') = go k b in (k', Delay b')
      _                  -> (k, h)

-- | Rename the fresh-bound variables of a goal (they are local to it).
renameFresh :: (X -> X) -> G X -> G X
renameFresh rn g = case g of
  Fresh x h          -> let x' = rn x in Fresh x' (renameFresh rn (substVar x (V x') h))
  Conjunction a b cs -> Conjunction (renameFresh rn a) (renameFresh rn b) (map (renameFresh rn) cs)
  Disjunction a b cs -> Disjunction (renameFresh rn a) (renameFresh rn b) (map (renameFresh rn) cs)
  Delay h            -> Delay (renameFresh rn h)
  _                  -> g

-- | The dispatch position of a relation: the parameter tested against a
--   constructor by the largest number of its clauses (at least one: in
--   @jo_atom y = (y == Jo_eq a a) | jo_clause y@ the second clause passes @y@
--   on to the clause table, and fixing @y@ to @Jo_r ..@ selects @r@'s clauses
--   there).
dispatchPos :: Def G X -> Maybe Int
dispatchPos (Def _ as body) =
  let paths = Index.paths body
      tests i = length [ () | p <- paths, any (isTest (as !! i)) p ]
      isTest x (V y :=: C _ _) = x == y
      isTest x (C _ _ :=: V y) = x == y
      isTest _ _ = False
      scored = [ (tests i, i) | i <- [0 .. length as - 1] ]
  in case scored of
       [] -> Nothing
       _  -> let (n, i) = maximumBy (comparing fst) scored in if n >= 1 then Just i else Nothing

type Key = (Name, Int, Name, Int)   -- relation, position, constructor, arity

-- | One round; @ctr@ numbers the new parameters (never reused: entries of
--   @made@ can be forgotten, so its size is not a fresh-name supply).
specRound :: Int -> M.Map Key Name -> Prog -> (Int, M.Map Key Name, Prog)
specRound ctr0 made1 (ds0, g0) = loop ctr0 made0 ds0 g0 (S.fromList (map getName ds0))
  where
    -- PostOpt may have inlined a specialization everywhere and removed it;
    -- forget it, so a later call creates it again instead of being rewritten
    -- into a call of a relation that no longer exists (which the failure
    -- analysis would then treat as failing)
    present = S.fromList (map getName ds0)
    made0 = M.filter (`S.member` present) made1
    defsOf ds = M.fromList [ (getName d, d) | d <- ds ]
    loop ctr made ds g names =
      let dm = defsOf ds
          pos = M.mapMaybe dispatchPos dm
          -- rewrite every call with a constructor at the dispatch position
          (ctr', made', newDefs, names') = foldl' (collect dm pos) (ctr, made, [], names) (concatMap (callsIn . getBody) ds ++ callsIn g)
          rw = rewrite pos made'
          ds' = [ Def n as (rw b) | Def n as b <- ds ] ++ [ Def n as (rw b) | Def n as b <- newDefs ]
          g' = rw g
      in if null newDefs then (ctr', made', (ds', g')) else loop ctr' made' ds' g' names'
    collect dm pos acc@(ctr, made, new, names) (f, ts) = case M.lookup f pos of
      Just i | i < length ts, C c us <- ts !! i
             , let key = (f, i, c, length us), not (M.member key made), M.size made < maxNew ->
               let Def _ as body = dm M.! f
                   base = f ++ "_" ++ sanitize c ++ show i
                   name = head [ n | k <- [0 :: Int ..], let n = if k == 0 then base else base ++ "_" ++ show k, n `S.notMember` names ]
                   zs = [ "_sc" ++ show ctr ++ "_" ++ show j | j <- [1 .. length us] ]
                   p = as !! i
                   params = take i as ++ drop (i + 1) as ++ zs
                   body' = substVar p (C c (map V zs)) body
               in (ctr + 1, M.insert key name made, new ++ [Def name params body'], S.insert name names)
      _ -> acc
    rewrite pos made gl = case gl of
      Invoke f ts | Just i <- M.lookup f pos, i < length ts, C c us <- ts !! i
                  , Just n <- M.lookup (f, i, c, length us) made ->
                    Invoke n (take i ts ++ drop (i + 1) ts ++ us)
      Conjunction a b cs -> Conjunction (rewrite pos made a) (rewrite pos made b) (map (rewrite pos made) cs)
      Disjunction a b cs -> Disjunction (rewrite pos made a) (rewrite pos made b) (map (rewrite pos made) cs)
      Fresh x h          -> Fresh x (rewrite pos made h)
      Delay h            -> Delay (rewrite pos made h)
      _                  -> gl

sanitize :: Name -> String
sanitize = map (\ch -> if ch `elem` (['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9'] ++ "_") then ch else '_')

callsIn :: G X -> [(Name, [Term X])]
callsIn g = case g of
  Invoke f ts        -> [(f, ts)]
  Conjunction a b cs -> concatMap callsIn (a : b : cs)
  Disjunction a b cs -> concatMap callsIn (a : b : cs)
  Fresh _ h          -> callsIn h
  Delay h            -> callsIn h
  _                  -> []

-- | Substitute a term for a (free) variable; a fresh binder of the same name
--   shadows it.
substVar :: X -> Term X -> G X -> G X
substVar x t = go
  where
    st (V y) | y == x = t
    st (V y)          = V y
    st (C c ts)       = C c (map st ts)
    go g = case g of
      a :=: b            -> st a :=: st b
      Invoke f ts        -> Invoke f (map st ts)
      Conjunction a b cs -> Conjunction (go a) (go b) (map go cs)
      Disjunction a b cs -> Disjunction (go a) (go b) (map go cs)
      Fresh y h | y == x    -> g
                | otherwise -> Fresh y (go h)
      Delay h            -> Delay (go h)

