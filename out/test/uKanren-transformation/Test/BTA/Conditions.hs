
module Test.BTA.Conditions where

import           Test.Helper
import           BTA.Conditions
import           BTA.Condition



ltAB = Lt "a" "b"
ltCD = Lt "c" "d"
eqAB = Eq "a" "b"
eqCD = Eq "c" "d"
eqCA = Eq "c" "a"
eqDA = Eq "a" "d"
condEq = ConditionDisj [ConditionConj [Condition (Eq "x" "x"), Condition (Eq "z" "z"), Condition (Eq "y'" "y'"), Condition (Lt "z" "y'"), Condition (Lt "y'" "x'"), Condition (Eq "y" "y"), Condition (Eq "x'" "x'"), Condition (Lt "x" "x'"), Condition (Lt "z" "x'"), Condition (Lt "y" "x'")]]

condCeqD = ConditionDisj [ConditionConj [Condition eqCD]]
condAeqB = ConditionDisj [ConditionConj [Condition eqAB]] 
condAeqBorCeqD = ConditionDisj [ConditionConj [Condition eqCD], ConditionConj [Condition eqAB]]
condAeqBCeqD = ConditionDisj [ConditionConj [Condition eqAB, Condition eqCD]]
condCltD = ConditionDisj [ConditionConj [Condition ltCD]]
condCeqDCltD = ConditionDisj [ConditionConj [Condition eqCD], ConditionConj [Condition ltCD]]

unit_equalConds = do
    test2True (==) ltAB ltAB
    test2False (==) ltCD ltAB
    test2False (==) eqAB ltAB
    test2True (/=) eqAB eqCD
    ConditionDisj [Condition ltAB, Condition eqCD] @?= ConditionDisj [Condition eqCD, Condition ltAB]
    ConditionDisj [Condition eqCD] /= ConditionDisj [Condition eqCD, Condition ltAB] @?= True
    ConditionDisj [Condition ltCD, Condition eqCD] /= ConditionDisj [Condition eqCD, Condition ltAB] @?= True
    test2True (/=) (disjEmpty :: Conditions String) conjEmpty
    test2False (==) disjEmpty $ ConditionDisj [ConditionConj [Condition (Eq "a" "a")]]
    (disjEmpty :: Conditions String) @?= disjEmpty
    condEq @?= condEq


unit_attachment = do
    test2True isAttachment (ConditionDisj [Condition eqCD]) $ ConditionDisj [Condition eqCD, Condition ltAB]
    test2False isAttachment (ConditionDisj [Condition eqCD]) $ ConditionDisj [Condition eqCA, Condition ltAB]
    test2True isAttachment (ConditionDisj [Condition ltAB, Condition eqCD]) $ ConditionDisj [Condition eqCD, Condition ltAB, Condition eqDA]

    test2True isAttachment (ConditionConj [Condition eqCD, Condition ltAB]) $ ConditionConj [Condition eqCD]
    test2False isAttachment (ConditionConj [Condition eqCD]) $ ConditionConj [Condition eqCA, Condition ltAB]
    test2True isAttachment (ConditionConj [Condition eqCD, Condition ltAB, Condition eqDA]) $ ConditionConj [Condition ltAB, Condition eqCD]

unit_newDisjunction = do 
    test2 getNewDisjunctionOr condCeqD condCeqD condCeqD

    test2True ((/=) . uncurry getNewDisjunctionOr) (condCeqD, condAeqB) condAeqB
    test2 getNewDisjunctionOr condCeqD condAeqB condAeqBorCeqD

    test2 getNewDisjunctionOr condCeqD condAeqBCeqD condCeqD
    test2 getNewDisjunctionOr disjEmpty condAeqB condAeqB

    test2 getNewDisjunctionOr condCeqD condCltD condCeqDCltD
    test2 getNewDisjunctionOr condCeqDCltD condCltD condCeqDCltD

