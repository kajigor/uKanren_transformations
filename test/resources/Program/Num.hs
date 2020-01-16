module Program.Num where

import Prelude hiding (succ)
import Syntax
import Program.Bool
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
  Let 
    ( Def "notZero" ["x"] 
      (
        fresh ["y"] (x === succ y)
      )
    ) g
    where [x, y] = map V ["x", "y"]

addo :: G a -> G a
addo g =
  Let
    ( Def "addo" ["x", "y", "z"]
        (
          x === zero &&& z === y |||
          fresh ["x'", "z'"]
            (x === succ x' &&& z === succ z' &&& call "addo" [x', y, z'])
        )
    ) g
    where [x, y, z, x', z'] = map V ["x", "y", "z", "x'", "z'"]

mulo :: G a -> G a
mulo g =
  Let
    ( Def "mulo" ["x", "y", "z"]
      (
        (x === zero &&& z === zero) |||
          fresh ["x'", "z'"]
            (x === succ x' &&&
             call "addo" [y, z', z] &&&
             call "mulo" [x', y, z'])
      )
    ) $ addo g
    where [x, y, z, x', z'] = map V ["x", "y", "z", "x'", "z'"]

leo :: G a -> G a
leo g =
  Let
    ( Def "leo" ["x", "y", "b"]
      (
        (x === zero &&& b === trueo) |||
         fresh ["zz"] (x === succ zz &&& y === zero &&& b === falso) |||
         fresh ["x'", "y'"] (x === succ x' &&& y === succ y' &&& call "leo" [x', y', b])
      )
    ) g
    where [x, y, b, x', y', zz] = map V ["x", "y", "b", "x'", "y'", "zz"]

gto :: G a -> G a
gto g =
  Let 
    ( Def "gto" ["x", "y", "b"] 
      (
        fresh ["zz"] (x === succ zz &&& y === zero &&& b === trueo) |||
        (x === zero &&& b === falso) |||
        fresh ["x'", "y'"] (x === succ x' &&& y === succ y' &&& call "gto" [x', y', b])
      )
    ) g
    where [x, y, b, x', y', zz] = map V ["x", "y", "b", "x'", "y'", "zz"]

geo :: G a -> G a
geo g = Let (Def "geo" ["x", "y", "z"] $ call "leo" [V "y", V "x", V "z"]) (leo g)

lto :: G a -> G a
lto g = Let (Def "lto" ["x", "y", "z"] $ call "gto" [V "y", V "x", V "z"]) (gto g)

num :: Show a => Term a -> String
num (V n) = printf "._%s" (show n)
num (C "O" []) = "O"
num (C "S" [x]) = printf "S(%s)" (num x)
num _ = "??"

