{-# LANGUAGE ApplicativeDo #-}
module Test.Shallow.Relations.Addo(addo) where

import Test.Shallow.Types.Nat
import Shallow.Core.Kanren

addo :: (Kanren rel) => L Natural rel -> L Natural rel -> L Natural rel -> Relation rel
addo = relation3 "addo" $ \x y z -> conde 
    [ do
        x <=> zro
        y <=> z
        pure ()
    , fresh2 $ \x' z' -> do
        x <=> suc x'
        call $ addo x' y z'
        z <=> suc z'
        pure ()
    ]