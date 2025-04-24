{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Shallow.Types.Void(Void, absurd') where

import Data.Void
import Shallow.Core.Internal.Logic

absurd' :: Reified Void var -> a
absurd' (RVoid x) = absurd x

instance LogicType Void where

    newtype Reified Void var = RVoid Void

    quote = absurd'
    unifyVal _ _ = absurd'
    derefVal _ = absurd'
    project = absurd
    reify = absurd'