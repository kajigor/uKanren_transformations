{-# LANGUAGE FlexibleInstances #-}

module InductiveCheck where

import           Data.Maybe
import           Eval
import qualified Subst
import           Syntax
import Program
import           Unfold       (oneStepUnfold, unifyStuff, normalize)
import qualified Environment as Env
import Control.Monad.State

newtype IndWrap a = IndWrap { unwrap :: a }

instance Eq a => Eq (IndWrap (Term a)) where
  t == t' = unwrap t == unwrap t'

instance Ord a => Ord (IndWrap (Term a)) where
  t <= t' = isSubtree (unwrap t) (unwrap t')
    where
      isSubtree t t'          | t == t' = True
      isSubtree (V x) (V y)   = x == y
      isSubtree (C _ _) (V _) = False
      isSubtree x (C _ ts)    = any (isSubtree x) ts

isInductive :: Program G X -> Bool
isInductive (Program defs goal) =
  let env = Env.fromDefs defs in
  let ((logicGoal, _), env') = runState (preEval goal) env in
  let g' = evalState (oneStepUnfold logicGoal) env' in
  let normalized = normalize g' in
  let unified = mapMaybe (unifyStuff Subst.empty) normalized in
  let withCalls = filter (not . null . fst) unified in

  let (Invoke name args) = logicGoal in

  all (\(calls, subst) ->
        length calls == 1 &&
        case head calls of
          Invoke n as | n == name ->
            let terms = map (Subst.substitute subst) args in
            and $ zipWith (\f a -> a <= f) terms as
          _ -> False
      )
      withCalls
