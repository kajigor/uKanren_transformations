{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Test.Shallow.Types.Unit(unit) where

import Shallow.Core.Internal.Logic
import Shallow.Utils.Type

unit :: Logic () var
unit = Ground Unit

instance LogicType () where

    data Reified () var = Unit

    quote Unit = quote0 "Unit" Unit

    unifyVal _ Unit Unit = pure ()

    derefVal _ Unit = pure ()
    project () = Unit
    reify Unit = return ()