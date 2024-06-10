{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BTA.Conditions where

import           Data.Maybe    (mapMaybe)
import qualified BTA.Condition as Cond

data Conditions a 
    = ConditionConj [Conditions a] 
    | ConditionDisj [Conditions a] 
    | Condition (Cond.Condition a)
    deriving (Functor, Show)

disjEmpty = ConditionDisj []
conjEmpty = ConditionDisj [ConditionConj []]

instance Eq a => Eq (Conditions a) where
    (==) conj1@(ConditionConj conds1) conj2@(ConditionConj conds2) = isAttachment conj1 conj2 && isAttachment conj2 conj1
    (==) disj1@(ConditionDisj conds1) disj2@(ConditionDisj conds2) = isAttachment disj1 disj2 && isAttachment disj2 disj1
    (==) (Condition cond1) (Condition cond2) = cond1 == cond2
    (==) _ _ = False

isAttachment :: Eq a => Conditions a -> Conditions a -> Bool
isAttachment (ConditionConj conjs1) (ConditionConj conjs2) = 
    all (\conj -> elem conj conjs1) conjs2
isAttachment disj1@(ConditionDisj disjs1) disj2@(ConditionDisj disjs2) = 
    all (\disj -> elem disj disjs2) disjs1
isAttachment conj (ConditionConj conjs) = 
    elem conj conjs

getOneDisjunct :: Eq a => Conditions a -> Conditions a -> Maybe (Conditions a)
getOneDisjunct disjs@(ConditionDisj disjuncts) disj@(ConditionConj _) | any (\disj1 -> isAttachment disj disj1) disjuncts = Nothing
                                                                      | otherwise = Just disj

getNewDisjunctionOr :: Eq a => Conditions a -> Conditions a -> Conditions a 
getNewDisjunctionOr add@(ConditionDisj disjuncts1) old@(ConditionDisj disjuncts2) =
    let newDisjuncts1 = mapMaybe (getOneDisjunct old) disjuncts1 in
    let newDisjuncts2 = mapMaybe (getOneDisjunct (ConditionDisj newDisjuncts1)) disjuncts2 in 
    ConditionDisj (newDisjuncts1 ++ newDisjuncts2)
