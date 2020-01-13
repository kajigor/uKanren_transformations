{-# LANGUAGE FlexibleInstances #-}

module InductiveCheck where 

import Syntax 
import CPD 
import Purification
import Eval 
import Data.Maybe

newtype IndWrap a = IndWrap { unwrap :: a }
 
instance Eq a => Eq (IndWrap (Term a)) where 
  t == t' = unwrap t == unwrap t'

instance Ord a => Ord (IndWrap (Term a)) where 
  t <= t' = isSubtree (unwrap t) (unwrap t') 
    where
      isSubtree t t' | t == t' = True 
      isSubtree (V x) (V y) = x == y 
      isSubtree (C _ _) (V _) = False 
      isSubtree x (C _ ts) = any (isSubtree x) ts 

isInductive :: G X -> Bool 
isInductive goal = 
  let (goal', defs) = takeOutLets goal in
  let gamma = updateDefsInGamma env0 defs in
  let (logicGoal, gamma', names) = preEval' gamma goal' in
  let (g', _) = oneStepUnfold (logicGoal) gamma' in 
  let normalized = normalize g' in
  let unified = mapMaybe (unifyStuff s0) normalized in
  let withCalls = filter (\(x,_) -> not $ null x) unified in 
  
  let (Invoke name args) = logicGoal in 

  all (\(calls, subst) -> 
        length calls == 1 && 
        case head calls of 
          Invoke n as | n == name -> 
            let terms = map (substitute subst) args in 
            and $ zipWith (\f a -> a <= f) terms as   
          _ -> False
      ) 
      withCalls