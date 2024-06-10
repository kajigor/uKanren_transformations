{-# LANGUAGE PatternSynonyms #-}
module FunConversion.Updated.Utils where

import Mode.Term
import Mode.Inst
import Mode.NormSyntax
import FunConversion.DetMode hiding (simpleFixpoint)

lguard :: e -> Bool -> Either e ()
lguard e True = return ()
lguard e False = Left e

pattern In :: Mode
pattern In = Mode Ground (Just Ground)

pattern Out :: Mode
pattern Out = Mode Free (Just Ground)

pattern InV :: a -> Var (a, Mode)
pattern InV v = Var (v, In)

pattern OutV :: a -> Var (a, Mode)
pattern OutV v = Var (v, Out)

mode :: Var (a, Mode) -> Mode
mode (Var (_, m)) = m

type MV = (Int, Mode)
type MTerm = FlatTerm MV

identifyCall :: String -> [Var MV] -> DefIdentifier
identifyCall name args = identify' name (getVar <$> args)

simpleFixpoint :: (Eq x) => (x -> x) -> x -> x
simpleFixpoint f x = let x' = f x in if x' == x then x' else simpleFixpoint f x'