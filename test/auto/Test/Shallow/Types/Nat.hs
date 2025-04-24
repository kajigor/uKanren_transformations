{-# LANGUAGE TypeFamilies, FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Shallow.Types.Nat(Natural, zro, suc) where
import Shallow.Core.Internal.Logic
import Shallow.Utils.Logic (showLogic)
import Shallow.Utils.Type
import Numeric.Natural

zro :: Logic Natural var
zro = Ground Z

suc :: Logic Natural var -> Logic Natural var
suc = Ground . S

zro' :: Reified Natural var
zro' = Z

suc' :: Reified Natural var -> Reified Natural var
suc' = S . Ground

instance LogicType Natural where

    data Reified Natural var = Z | S (Logic Natural var)

    quote Z = quote0 "Z" Z
    quote (S n) = quote1 "S" S n

    unifyVal _ Z Z = pure ()
    unifyVal unif (S x) (S y) = unif x y
    unifyVal _ _ _ = empty

    derefVal _ Z = pure 0
    derefVal deref (S n) = succ <$> (deref n)

    project 0 = zro'
    project n = suc' $ project (n - 1)
    reify Z = return 0
    reify (S (Ground n)) = succ <$> reify n
    reify _ = Nothing

    
instance Show (Reified Natural var) where
    show Z = "Z"
    show (S n) = "S " ++ showLogic n