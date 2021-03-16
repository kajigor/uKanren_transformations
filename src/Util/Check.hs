module Util.Check where

import qualified Subst
import           Syntax
import           Unfold
import qualified Environment as Env
import Control.Monad.State

-- Checks if the first invocation is less than the second one.
-- r x1 .. xn < r y1 .. yn <=> \exists i : xi -- strict subterm of yi
check :: G S -> G S -> Bool
check (Invoke n xs) (Invoke m ys) | n == m && length xs == length ys =
  any (\(x, y) -> x /= y && x `subTerm` y) (zip xs ys)
check _ _ = False

-- Checks if a conjunction contains a call with an accumulating parameter.
checkConj :: [G S] -> [G S] -> Bool
checkConj xs ys | length xs == length ys = any (uncurry check) (zip xs ys)
checkConj _ _   = False

-- Checks if the first term occurs as a subterm within the second one.
subTerm :: Term S -> Term S -> Bool
subTerm t s@(C _ ys) = t == s || any (subTerm t) ys
subTerm (V x) (V y)  = x == y
subTerm _ _ = False

-- Checks if the current goal should be unfolded more than once.
checkWhenUnfolding :: G S -> Env.Env -> Subst.Subst -> Bool
checkWhenUnfolding g@(Invoke n xs) env subst =
  let gs = evalState (oneStep g subst) env in
  not $ any (\(goals, s) -> any (\g' -> check g (Subst.substitute s g')) goals) gs
