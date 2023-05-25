module AddoProg where

import           Def
import           Prelude      hiding (succ)
import           Program.Bool
import           Syntax
import           Text.Printf

peanify :: Integer -> Term a
peanify n | n <= 0 = zero
peanify n          = succ (peanify $ n - 1)

zero :: Term a
zero = C "O" []

succ :: Term a -> Term a
succ x = C "S" [x]

addoDef :: Def G X
addoDef =
    ( Def "addo" ["x", "y", "z"]
        (
          x === zero &&& z === y |||
          fresh ["x'", "z'"]
            (x === succ x' &&& z === succ z' &&& call "addo" [x', y, z'] )
        )
    )
  where
    [x, y, z, x', z'] = map V ["x", "y", "z", "x'", "z'"]

addo :: [Def G X]
addo = [addoDef]

groundNatDef :: Def G X
groundNatDef = Def "groundNat" ["x"] (V "x" === zero ||| fresh ["x'"] (V "x" === succ (V "x'") &&& call "groundNat" [V "x'"]))

addoGroundDef :: Def G X
addoGroundDef = 
  ( Def "addoGround" ["x", "y", "z"] (call "addo" [V "x", V "y", V "z"] &&& (call "groundNat" [V "y"])))

addoGround = addo ++ [groundNatDef, addoGroundDef]