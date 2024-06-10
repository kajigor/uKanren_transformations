module Test.Eval where

import           Test.Helper ((@?=))

import           Eval
import qualified Subst
import           Syntax

unit_occursCheck = do
  unify (unify (Just Subst.empty) (V 0) (V 1)) (V 1) (C "constr" [V 0]) @?= Nothing
  unify (unify (unify (Just Subst.empty) (V 0) (V 1)) (V 1) (C "constr" [V 0])) (V 0) (V 1) @?= Nothing
