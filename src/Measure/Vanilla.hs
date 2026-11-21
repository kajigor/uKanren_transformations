-- | Vanilla self-interpreter encoding of a benchmark ("Jones-optimality test").
--
--   Every definition @r x1..xn = body@ of the object program is turned into
--   clauses of one relation @jo_clause@: each disjunct of the body's
--   disjunctive normal form becomes
--
--   > fresh x1..xn, ys in g == Jo_r x1..xn & b == [a1, .., ak]
--
--   where the atoms @ai@ keep the left-to-right order of the original
--   conjunction and are either @Jo_eq t1 t2@ (an object-level unification)
--   or @Jo_f ts@ (a call of @f@).  The object program is then run by the
--   classic vanilla meta-interpreter
--
--   > jo_solve gs = gs == [] | fresh g, t in gs == g :: t & jo_atom g & jo_solve t
--   > jo_atom g   = (fresh a, b in g == Jo_eq a b & a == b)
--   >             | (fresh b in jo_clause g b & jo_solve b)
--
--   Object variables are host variables (non-ground representation).  A query
--   @r ts@ becomes @jo_solve [Jo_r ts]@.
--
--   The first Futamura projection says that specializing @jo_solve@ for a
--   static object program compiles it; a Jones-optimal specializer gives back
--   (a program as fast as) the original.  Comparing
--   steps(S(vanilla(p))) with steps(p) and steps(S(p)) therefore measures how
--   much interpretive overhead a specializer S removes.
module Measure.Vanilla
  ( vanillaProgram
  , vanillaGoal
  , vanillaBench
  ) where

import           Control.Monad.State
import           Def
import           Measure.Runner      (Bench (..))
import           Program
import           Syntax

data Atom = AEq (Term X) (Term X) | ACall Name [Term X]

-- | Disjunctive normal form of a body: a list of clauses, each a list of
--   local variables and a list of atoms in left-to-right order.  Local
--   variables are renamed apart so that lifting them to the clause level
--   cannot capture.
dnf :: G X -> State Int [([X], [Atom])]
dnf (t1 :=: t2)  = return [([], [AEq t1 t2])]
dnf (Invoke f ts) = return [([], [ACall f ts])]
dnf (Delay g)    = dnf g
dnf (Disjunction a b cs) = concat <$> mapM dnf (a : b : cs)
dnf (Conjunction a b cs) = do
  parts <- mapM dnf (a : b : cs)
  return $ foldl (\acc p -> [ (v1 ++ v2, a1 ++ a2) | (v1, a1) <- acc, (v2, a2) <- p ]) [([], [])] parts
dnf (Fresh x g) = do
  k <- get
  put (k + 1)
  let x' = x ++ "_jo" ++ show k
  cls <- dnf (rename x x' g)
  return [ (x' : vs, as) | (vs, as) <- cls ]

rename :: X -> X -> G X -> G X
rename x y = go
  where
    rt (V v) | v == x = V y
    rt (V v)          = V v
    rt (C c ts)       = C c (map rt ts)
    go (t1 :=: t2)          = rt t1 :=: rt t2
    go (Invoke f ts)        = Invoke f (map rt ts)
    go (Delay g)            = Delay (go g)
    go (Conjunction a b cs) = Conjunction (go a) (go b) (map go cs)
    go (Disjunction a b cs) = Disjunction (go a) (go b) (map go cs)
    go g@(Fresh z h) | z == x    = g
                     | otherwise = Fresh z (go h)

quoteCall :: Name -> [Term X] -> Term X
quoteCall f = C ("Jo_" ++ f)

quoteAtom :: Atom -> Term X
quoteAtom (AEq a b)    = C "Jo_eq" [a, b]
quoteAtom (ACall f ts) = quoteCall f ts

list :: [Term X] -> Term X
list = foldr (\h t -> C "Cons" [h, t]) (C "Nil" [])

-- | The self-interpreter together with the object program as its clauses.
vanillaProgram :: [Def G X] -> [Def G X]
vanillaProgram defs = [solve, atom, clause]
  where
    clauses = concat (evalState (mapM defClauses defs) 0)
    defClauses (Def r xs body) = do
      cls <- dnf body
      return [ fresh (xs ++ vs) (V "jo_g" :=: quoteCall r (map V xs) &&& V "jo_b" :=: list (map quoteAtom as))
             | (vs, as) <- cls ]
    clause = Def "jo_clause" ["jo_g", "jo_b"] $ case clauses of
      []  -> C "Jo_none" [] :=: C "Jo_some" []
      _   -> foldr1 (|||) clauses
    solve = Def "jo_solve" ["jo_gs"] $
      (V "jo_gs" :=: C "Nil" []) |||
      fresh ["jo_h", "jo_t"]
        (V "jo_gs" :=: C "Cons" [V "jo_h", V "jo_t"] &&& Invoke "jo_atom" [V "jo_h"] &&& Invoke "jo_solve" [V "jo_t"])
    atom = Def "jo_atom" ["jo_a"] $
      fresh ["jo_l", "jo_r"] (V "jo_a" :=: C "Jo_eq" [V "jo_l", V "jo_r"] &&& V "jo_l" :=: V "jo_r") |||
      fresh ["jo_body"] (Invoke "jo_clause" [V "jo_a", V "jo_body"] &&& Invoke "jo_solve" [V "jo_body"])

-- | Rewrite a query @fresh vs. r ts@ (or any goal whose top-level calls are
--   object calls) into a query of the self-interpreter.
vanillaGoal :: G X -> G X
vanillaGoal g =
  let (vs, body) = topLevelFreshVars g
  in  fresh vs $ case body of
        Invoke f ts -> Invoke "jo_solve" [list [quoteCall f ts]]
        other       -> other

-- | The vanilla-encoded twin of a benchmark, named @<name>\@vanilla@.
vanillaBench :: Bench -> Bench
vanillaBench b =
  let Program defs goal = bProgram b
  in  b { bName    = bName b ++ "@vanilla"
        , bProgram = Program (vanillaProgram defs) (vanillaGoal goal)
        , bRuns    = [ (q, vanillaGoal g) | (q, g) <- bRuns b ]
        }
