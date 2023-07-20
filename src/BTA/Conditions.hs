{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module BTA.Conditions where
import Data.Maybe (mapMaybe)
import qualified BTA.Condition as Cond

data Conditions a = 
    ConditionConj [Conditions a] | 
    ConditionDisj [Conditions a] | 
    Condition (Cond.Condition a)
    deriving (Functor, Show)

instance (Eq a) => Eq (Conditions a) 
(==) conj1@(ConditionConj conds1) conj2@(ConditionConj conds2) = (isAttachment conj1 conj2) && (isAttachment conj2 conj1)
(==) disj1@(ConditionDisj conds1) disj2@(ConditionDisj conds2) = (isAttachment disj1 disj2) && (isAttachment disj2 disj1)
(==) (Condition cond1) (Condition cond2) = cond1 Prelude.== cond2
(==) _ _ = False

equalConds :: Eq a => Conditions a -> Conditions a -> Bool
equalConds conj1@(ConditionConj conds1) conj2@(ConditionConj conds2) = (isAttachment conj1 conj2) && (isAttachment conj2 conj1)
equalConds disj1@(ConditionDisj conds1) disj2@(ConditionDisj conds2) = (isAttachment disj1 disj2) && (isAttachment disj2 disj1)
equalConds (Condition cond1) (Condition cond2) = cond1 Prelude.== cond2
equalConds _ _ = False

isAttachment :: Eq a => Conditions a -> Conditions a -> Bool
isAttachment (ConditionConj conjs1) (ConditionConj conjs2) = 
    (all (\conj -> (any (equalConds conj) conjs1)) conjs2) 
isAttachment disj1@(ConditionDisj disjs1) disj2@(ConditionDisj disjs2) = 
    (all (\disj -> (any (equalConds disj) disjs2)) disjs1) 
isAttachment conj (ConditionConj conjs) = 
    (any (equalConds conj) conjs)

getOneDisjunct :: Eq a => Conditions a -> Conditions a -> Maybe (Conditions a)
getOneDisjunct disjs@(ConditionDisj disjuncts) disj@(ConditionConj _) | (any (\disj1 -> isAttachment disj disj1) disjuncts) = Nothing
                                                                      | otherwise = Just disj

getNewDisjunctionOr :: Eq a => Conditions a -> Conditions a -> Conditions a 
getNewDisjunctionOr add@(ConditionDisj disjuncts1) old@(ConditionDisj disjuncts2) =
    let newDisjuncts1 = mapMaybe (getOneDisjunct old) disjuncts1 in
    let newDisjuncts2 = mapMaybe (getOneDisjunct (ConditionDisj newDisjuncts1)) disjuncts2 in 
    ConditionDisj (newDisjuncts1 ++ newDisjuncts2)

getOneConjunct :: Eq a => Conditions a -> Conditions a -> Maybe (Conditions a) 
getOneConjunct conjs@(ConditionConj conjuncts) conj | (isAttachment conj conjs) = Nothing
                                                    | otherwise = Just conj

getNewConjunction :: Eq a => Conditions a -> Conditions a -> Conditions a
getNewConjunction add@(ConditionConj conjuncts1) old@(ConditionConj conjuncts2) = 
    let newConjuncts1 = mapMaybe (getOneConjunct old) conjuncts1 in
    let newConjuncts2 = mapMaybe (getOneConjunct (ConditionConj newConjuncts1)) conjuncts2 in 
    ConditionConj (newConjuncts1 ++ newConjuncts2)
    
getNewDisjunctionAnd :: Eq a => Conditions a -> Conditions a -> Conditions a 
getNewDisjunctionAnd add@(ConditionDisj disjuncts1) old@(ConditionDisj disjuncts2) = 
    let newDisjuncts = [(getNewConjunction disj1 disj2) | disj1 <- disjuncts1, disj2 <- disjuncts2] in
    foldl (getNewDisjunctionOr) (ConditionDisj []) (map (\x -> ConditionDisj [x]) newDisjuncts)

