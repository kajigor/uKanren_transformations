module Num where

import Prelude hiding (succ)
import Syntax
import Bool
import Text.Printf

peanify :: Integer -> Term a
peanify n | n <= 0 = zero
peanify n          = succ (peanify $ n - 1)

zero :: Term a
zero = C "O" []

succ :: Term a -> Term a
succ x = C "S" [x]

notZero :: G a -> G a
notZero g =
  let x = V "x" in
  let y = V "y" in
  Let (def "notZero" ["x"] (fresh ["y"] (x === succ y))) g

addo :: G a -> G a
addo g =
  let x  = V "x" in
  let y  = V "y" in
  let z  = V "z" in
  let x' = V "x'" in
  let z' = V "z'" in
  Let
    ( def "addo" ["x", "y", "z"]
        (
          x === zero &&& z === y |||
          fresh ["x'", "z'"]
            (x === succ x' &&& z === succ z' &&& call "addo" [x', y, z'])
        )
    ) g

mulo :: G a -> G a
mulo g =
  let x  = V "x" in
  let y  = V "y" in
  let z  = V "z" in
  let x' = V "x'" in
--  let y' = V "y'" in
  let z' = V "z'" in
  Let
    ( def "mulo" ["x", "y", "z"]
      (
        (x === zero &&& z === zero) |||
          fresh ["x'", "y'", "z'"]
            (x === succ x' &&&
             call "addo" [y, z', z] &&&
             call "mulo" [x', y, z'])
      )
    ) $ addo g

leo :: G a -> G a
leo g =
  let x  = V "x"  in
  let y  = V "y"  in
  let b  = V "b"  in
  let x' = V "x'" in
  let y' = V "y'" in
  let zz = V "zz" in
  Let
    ( def "leo" ["x", "y", "b"]
      (
        (x === zero &&& b === trueo) |||
         fresh ["zz"] (x === succ zz &&& y === zero &&& b === falso) |||
         fresh ["x'", "y'"] (x === succ x' &&& y === succ y' &&& call "leo" [x', y', b])
      )
    ) g

gto :: G a -> G a
gto g =
  let x  = V "x"  in
  let y  = V "y"  in
  let b  = V "b"  in
  let x' = V "x'" in
  let y' = V "y'" in
  let zz = V "zz" in
  Let (
    def "gto" ["x", "y", "b"] (
      fresh ["zz"] (x === succ zz &&& y === zero &&& b === trueo) |||
      (x === zero &&& b === falso) |||
      fresh ["x'", "y'"] (x === succ x' &&& y === succ y' &&& call "gto" [x', y', b])
    )
  ) g

geo :: G a -> G a
geo g = Let (def "geo" ["x", "y", "z"] $ call "leo" [V "y", V "x", V "z"]) (leo g)

lto :: G a -> G a
lto g = Let (def "lto" ["x", "y", "z"] $ call "gto" [V "y", V "x", V "z"]) (gto g)

num :: Show a => Term a -> String
num (V n) = printf "._%s" (show n)
num (C "O" []) = "O"
num (C "S" [x]) = printf "S(%s)" (num x)
num _ = "??"

