module Util.Check where

import qualified Eval   as E
import           Syntax
import           Unfold

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
checkWhenUnfolding :: G S -> E.Gamma -> E.Sigma -> Bool
checkWhenUnfolding g@(Invoke n xs) gamma@(p,_,_) state =
  let (gs, gamma') = oneStep g gamma state in
  not $ any (\(goals, s) -> any (\g' -> check g (E.substitute s g')) goals) gs
