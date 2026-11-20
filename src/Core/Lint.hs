-- | Well-formedness checker for (residual) programs.
--
--   Prototype of the "check IR invariants after every pass" proposal in
--   docs/DESIGN.md.  It reports violations of invariants that every
--   transformation in the repository silently assumes:
--
--   * every variable is bound by a parameter, an enclosing 'Fresh', or is a
--     query variable;
--   * every called relation is defined (or is success/fail) and is called
--     with the right number of arguments;
--   * relation names are unique, parameters are pairwise distinct;
--   * binders do not shadow parameters or enclosing binders (several passes
--     substitute under binders without renaming, so shadowing is a latent
--     capture bug even when it is not wrong today).
module Core.Lint
  ( Issue (..)
  , Severity (..)
  , lintProgram
  , lintSummary
  ) where

import           Data.List       (group, nub, sort)
import qualified Data.Map.Strict as M
import qualified Data.Set        as S
import           Def
import           Syntax

data Severity = Error | Warning deriving (Show, Eq, Ord)

data Issue = Issue
  { iSeverity :: Severity
  , iWhere    :: String
  , iWhat     :: String
  } deriving (Show, Eq)

lintProgram :: [X] -> [Def G X] -> G X -> [Issue]
lintProgram qvars defs goal =
       dupDefs
    ++ concatMap lintDef defs
    ++ lintGoal "query" (S.fromList qvars) goal
  where
    arity = M.fromList [ (getName d, length (getArgs d)) | d <- defs ]
    dupDefs = [ Issue Error n "relation defined more than once"
              | (n : _ : _) <- group (sort (map getName defs)) ]

    lintDef (Def n as body) =
         [ Issue Error n "duplicate parameter" | length (nub as) /= length as ]
      ++ lintGoal n (S.fromList as) body

    lintGoal :: String -> S.Set X -> G X -> [Issue]
    lintGoal w = go
      where
        go bound g = case g of
          a :=: b -> unbound bound (fv a ++ fv b)
          Invoke f ts
            | null ts && (f == successName || f == failureName) -> []
            | otherwise ->
                unbound bound (concatMap fv ts) ++
                case M.lookup f arity of
                  Nothing -> [Issue Error w ("call to undefined relation " ++ f)]
                  Just k | k /= length ts ->
                    [Issue Error w ("arity mismatch in call to " ++ f ++ ": expected "
                                    ++ show k ++ ", got " ++ show (length ts))]
                  _ -> []
          Conjunction a b cs -> concatMap (go bound) (a : b : cs)
          Disjunction a b cs -> concatMap (go bound) (a : b : cs)
          Fresh x h ->
            [ Issue Warning w ("binder " ++ x ++ " shadows an outer variable") | x `S.member` bound ]
            ++ go (S.insert x bound) h
          Delay h -> go bound h
        unbound bound vs =
          [ Issue Error w ("unbound variable " ++ v) | v <- nub vs, not (v `S.member` bound) ]

-- | Short summary for a CSV cell: "ok", or counts plus the first error.
lintSummary :: [Issue] -> String
lintSummary [] = "ok"
lintSummary is =
  let es = [ i | i <- is, iSeverity i == Error ]
      ws = [ i | i <- is, iSeverity i == Warning ]
      first = case es ++ ws of
        (i : _) -> " (" ++ iWhere i ++ ": " ++ map (\c -> if c == ',' then ';' else c) (iWhat i) ++ ")"
        [] -> ""
  in show (length es) ++ "E/" ++ show (length ws) ++ "W" ++ first
