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

notZeroDef :: Def
notZeroDef = 
    ( Def "notZero" ["x"] 
      (
        fresh ["y"] (x === succ y)
      )
    )
  where 
    [x, y] = map V ["x", "y"]

notZero :: [Def]
notZero = [notZeroDef]

addoDef :: Def
addoDef = 
    ( Def "addo" ["x", "y", "z"]
        (
          x === zero &&& z === y |||
          fresh ["x'", "z'"]
            (x === succ x' &&& z === succ z' &&& call "addo" [x', y, z'])
        )
    )
  where 
    [x, y, z, x', z'] = map V ["x", "y", "z", "x'", "z'"]

addo :: [Def]
addo = [addoDef]

muloDef :: Def
muloDef = 
    ( Def "mulo" ["x", "y", "z"]
      (
        (x === zero &&& z === zero) |||
          fresh ["x'", "z'"]
            (x === succ x' &&&
             call "addo" [y, z', z] &&&
             call "mulo" [x', y, z'])
      )
    )
  where 
    [x, y, z, x', z'] = map V ["x", "y", "z", "x'", "z'"]

mulo :: [Def]
mulo = muloDef : addo 

leoDef :: Def
leoDef = 
    ( Def "leo" ["x", "y", "b"]
      (
        (x === zero &&& b === trueo) |||
         fresh ["zz"] (x === succ zz &&& y === zero &&& b === falso) |||
         fresh ["x'", "y'"] (x === succ x' &&& y === succ y' &&& call "leo" [x', y', b])
      )
    )
  where 
    [x, y, b, x', y', zz] = map V ["x", "y", "b", "x'", "y'", "zz"]

leo :: [Def]
leo = [leoDef]

gtoDef :: Def
gtoDef = 
    ( Def "gto" ["x", "y", "b"] 
      (
        fresh ["zz"] (x === succ zz &&& y === zero &&& b === trueo) |||
        (x === zero &&& b === falso) |||
        fresh ["x'", "y'"] (x === succ x' &&& y === succ y' &&& call "gto" [x', y', b])
      )
    )
  where 
    [x, y, b, x', y', zz] = map V ["x", "y", "b", "x'", "y'", "zz"]

gto :: [Def]
gto = [gtoDef]

geoDef :: Def
geoDef = Def "geo" ["x", "y", "z"] $ call "leo" [V "y", V "x", V "z"]

geo :: [Def]
geo = geoDef : leo 

ltoDef :: Def
ltoDef = Def "lto" ["x", "y", "z"] $ call "gto" [V "y", V "x", V "z"]

lto :: [Def]
lto = ltoDef : gto 

num :: Show a => Term a -> String
num (V n) = printf "._%s" (show n)
num (C "O" []) = "O"
num (C "S" [x]) = printf "S(%s)" (num x)
num _ = "??"

