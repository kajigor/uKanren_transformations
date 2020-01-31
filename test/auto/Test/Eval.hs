module Test.Eval where

import           Test.HUnit (Assertion, (@?=))

import           Eval
import           Syntax

unit_occursCheck = do
  unify (unify (Just []) (V 0) (V 1)) (V 1) (C "constr" [V 0]) @?= Nothing
  unify (unify (unify (Just []) (V 0) (V 1)) (V 1) (C "constr" [V 0])) (V 0) (V 1) @?= Nothing
