
module Test.BTA.Conditions where

import           Test.Helper
import qualified BTA.Conditions as Conds
import qualified BTA.Inequalities as Ineq
import           BTA.Condition


unit_equalConds = do
    ((Lt "a" "b") == (Lt "a" "b")) @?= True
    ((Lt "c" "d") /= (Lt "a" "b")) @?= True
    ((Eq "a" "b") /= (Lt "a" "b")) @?= True
    ((Eq "a" "b") /= (Eq "c" "b")) @?= True
    (Conds.equalConds (Conds.ConditionDisj [(Conds.Condition (Lt "a" "b")), (Conds.Condition (Eq "c" "d"))]) (Conds.ConditionDisj [(Conds.Condition (Eq "c" "d")), (Conds.Condition (Lt "a" "b"))])) @?= True
    (Conds.equalConds (Conds.ConditionDisj [(Conds.Condition (Eq "c" "d"))]) (Conds.ConditionDisj [(Conds.Condition (Eq "c" "d")), (Conds.Condition (Lt "a" "b"))])) @?= False
    (Conds.equalConds (Conds.ConditionDisj [(Conds.Condition (Lt "c" "d")), (Conds.Condition (Eq "c" "d"))]) (Conds.ConditionDisj [(Conds.Condition (Eq "c" "d")), (Conds.Condition (Lt "a" "b"))])) @?= False
    (Conds.equalConds (Conds.ConditionDisj [] :: Conds.Conditions String) (Conds.ConditionDisj [Conds.ConditionConj [] :: Conds.Conditions String])) @?= False
    (Conds.equalConds (Conds.ConditionDisj [] :: Conds.Conditions String) (Conds.ConditionDisj [Conds.ConditionConj [(Conds.Condition (Eq "a" "a"))]])) @?= False
    (Conds.equalConds (Conds.ConditionDisj [] :: Conds.Conditions String) (Conds.ConditionDisj [] :: Conds.Conditions String)) @?= True


unit_attachment = do
    (Conds.isAttachment (Conds.ConditionDisj [(Conds.Condition (Eq "c" "d"))]) (Conds.ConditionDisj [(Conds.Condition (Eq "c" "d")), (Conds.Condition (Lt "a" "b"))])) @?= True
    (Conds.isAttachment (Conds.ConditionDisj [(Conds.Condition (Eq "c" "d"))]) (Conds.ConditionDisj [(Conds.Condition (Eq "c" "a")), (Conds.Condition (Lt "a" "b"))])) @?= False
    (Conds.isAttachment (Conds.ConditionDisj [(Conds.Condition (Lt "a" "b")), (Conds.Condition (Eq "c" "d"))]) (Conds.ConditionDisj [(Conds.Condition (Eq "c" "d")), (Conds.Condition (Lt "a" "b")), (Conds.Condition (Eq "a" "d"))])) @?= True

    (Conds.isAttachment (Conds.ConditionConj [(Conds.Condition (Eq "c" "d")), (Conds.Condition (Lt "a" "b"))]) (Conds.ConditionConj [(Conds.Condition (Eq "c" "d"))])) @?= True
    (Conds.isAttachment (Conds.ConditionConj [(Conds.Condition (Eq "c" "d"))]) (Conds.ConditionConj [(Conds.Condition (Eq "c" "a")), (Conds.Condition (Lt "a" "b"))])) @?= False
    (Conds.isAttachment (Conds.ConditionConj [(Conds.Condition (Eq "c" "d")), (Conds.Condition (Lt "a" "b")), (Conds.Condition (Eq "a" "d"))]) (Conds.ConditionConj [(Conds.Condition (Lt "a" "b")), (Conds.Condition (Eq "c" "d"))])) @?= True


condCeqD = (Conds.ConditionDisj [(Conds.ConditionConj [Conds.Condition (Eq "c" "d")])]) 
condAeqB = (Conds.ConditionDisj [(Conds.ConditionConj [Conds.Condition (Eq "a" "b")])]) 
condEmpty = (Conds.ConditionDisj [])
condAeqBorCeqD = (Conds.ConditionDisj [(Conds.ConditionConj [Conds.Condition (Eq "c" "d")]), (Conds.ConditionConj [Conds.Condition (Eq "a" "b")])]) 
condAeqBCeqD = (Conds.ConditionDisj [(Conds.ConditionConj [Conds.Condition (Eq "a" "b"), Conds.Condition (Eq "c" "d")])])
condCltD = (Conds.ConditionDisj [(Conds.ConditionConj [Conds.Condition (Lt "c" "d")])])
condCeqDCltD = (Conds.ConditionDisj [(Conds.ConditionConj [Conds.Condition (Eq "c" "d")]), (Conds.ConditionConj [Conds.Condition (Lt "c" "d")])])

unit_newDisjunction = do 
    (Conds.equalConds (Conds.getNewDisjunctionOr condCeqD condCeqD) condCeqD) @?= True

    (Conds.equalConds (Conds.getNewDisjunctionOr condCeqD condAeqB) condAeqB) @?= False
    (Conds.equalConds (Conds.getNewDisjunctionOr condCeqD condAeqB) condAeqBorCeqD) @?= True

    (Conds.equalConds (Conds.getNewDisjunctionOr condCeqD  condAeqBCeqD) condCeqD) @?= True
    (Conds.equalConds (Conds.getNewDisjunctionOr condEmpty condAeqB) condAeqB) @?= True

    (Conds.equalConds (Conds.getNewDisjunctionOr condCeqD condCltD) condCeqDCltD) @?= True
    (Conds.equalConds (Conds.getNewDisjunctionOr condCeqDCltD condCltD) condCeqDCltD) @?= True

unit_newConjunction = do
    (Conds.equalConds (Conds.getNewDisjunctionAnd condCeqD condCeqD) condCeqD) @?= True

    (Conds.equalConds (Conds.getNewDisjunctionAnd condCeqD condAeqB) condAeqB) @?= False
    (Conds.equalConds (Conds.getNewDisjunctionAnd condCeqD condAeqB) condAeqBCeqD) @?= True

    (Conds.equalConds (Conds.getNewDisjunctionAnd condCeqD condAeqBCeqD) condAeqBCeqD) @?= True
    (Conds.equalConds (Conds.getNewDisjunctionAnd condEmpty condAeqB) condEmpty) @?= True

    (Conds.equalConds (Conds.getNewDisjunctionAnd condCeqD condCeqDCltD) condCeqD) @?= True
    (Conds.equalConds (Conds.getNewDisjunctionAnd condCeqDCltD condCltD) condCltD) @?= True

