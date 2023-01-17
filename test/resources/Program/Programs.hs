module Program.Programs where

import Program.List
import Program.Num
import Syntax
import Def
import Prelude hiding (succ)

palindromoDef :: Def G X
palindromoDef =
    ( Def "palindromo" ["x"]
      (
        call "reverso" [x, x]
      )
    )
  where
    x = V "x"

palindromo :: [Def G X]
palindromo = palindromoDef : reverso

someAppendoDef :: Def G X
someAppendoDef =
    ( Def "someAppendo" ["x", "y", "z"]
      (
        fresh ["t"] ( call "appendo" [x, y, z] &&& call "appendo" [y, x, z] )
      )
    )
  where
    [x, y, z] = map V ["x", "y", "z"]

someAppendo :: [Def G X]
someAppendo = someAppendoDef : appendo

doubleAppendoDef :: Def G X
doubleAppendoDef =
    ( Def "doubleAppendo" ["x", "y", "z", "r"]
      (
        fresh ["t"] ( call "appendo" [x, y, t] &&& call "appendo" [t, z, r] )
      )
    )
  where
    [x, y, z, r, t] = map V ["x", "y", "z", "r", "t"]

doubleAppendo :: [Def G X]
doubleAppendo = doubleAppendoDef : appendo

-- evenoDef :: Def G X
-- evenoDef =
--     ( Def "eveno" ["x"]
--       (
--         fresh ["z"] (call "addo" [z, z, x])
--       )
--     ) $ addo g
--   where
--     [x, z] = map V ["x", "z"]

-- eveno :: [Def G X]
-- eveno = evenoDef : addo

doubleoDef :: Def G X
doubleoDef =
    ( Def "doubleo" ["x", "xx"]
      (
        call "appendo" [x, x, xx]
      )
    )
  where
    [x, xx] = map V ["x", "xx"]

doubleo :: [Def G X]
doubleo = doubleoDef : appendo

emptyAppendoDef :: Def G X
emptyAppendoDef =
    ( Def "emptyAppendo" ["x", "y"]
      (
        call "appendo" [nil, x, y]
      )
    )
  where
    [x, y] = map V ["x", "y"]

emptyAppendo :: [Def G X]
emptyAppendo = emptyAppendoDef : appendo

toList [] = nil
toList (c:cs) = peanify c % toList cs

appendo123Def :: Def G X
appendo123Def =
    ( Def "appendo123" ["x", "y"]
      (
        call "appendo" [toList [1..3], x, y]
      )
    )
  where
    [x, y] = map V ["x", "y"]

appendo123 :: [Def G X]
appendo123 = appendo123Def : appendo

appendoXyzDef :: Def G X
appendoXyzDef =
    ( Def "appendoXyz" ["x", "y", "z", "t", "r"]
      (
        call "appendo" [x % (y % (z % nil)), t, r]
      )
    )
  where
    [x, y, z, r, t] = map V ["x", "y", "z", "r", "t"]

appendoXyz :: [Def G X]
appendoXyz = appendoXyzDef : appendo

singletonReversoDef :: Def G X
singletonReversoDef =
    ( Def "singletonReverso" ["x", "y"]
      (
        fresh ["l"] (call "lengtho" [x, peanify 1] &&& call "reverso" [x, y])
      )
    )
  where
    [x, y] = map V ["x", "y"]

singletonReverso :: [Def G X]
singletonReverso = singletonReversoDef : reverso ++ lengtho

is5Def :: Def G X
is5Def = Def "is5" ["x"] (V "x" === peanify 5)

is5 :: [Def G X]
is5 = [is5Def]

isNumDef :: Def G X
isNumDef =
    ( Def "isNum" ["x"]
      (
        (x === zero) ||| (fresh ["y"] (x === succ y))
      )
    )
  where
    [x, y] = map V ["x", "y"]

isNum :: [Def G X]
isNum = [isNumDef]

check5Def :: Def G X
check5Def =
    ( Def "check5" ["x"]
      (
        call "isNum" [x] &&& call "is5" [x]
      )
    )
  where
    x = V "x"

check5 :: [Def G X]
check5 = check5Def : isNum ++ is5

genListsDef :: Def G X
genListsDef =
    ( Def "genLists" ["x"]
      (
        (fresh ["y"] (x === y % nil &&& call "isNum" [y])) |||
        (fresh ["h", "t"] (x === h % t &&& call "isNum" [h] &&& call "genLists" [t]))
      )
    )
  where
    [x, y, h, t] = map V ["x", "y", "h", "t"]

genLists :: [Def G X]
genLists = genListsDef : isNum

has5Def :: Def G X
has5Def =
    ( Def "has5" ["x"]
      ( fresh ["h", "t"]
          ( (x === h % t &&& call "is5" [h]) |||
            (x === h % t &&& call "has5" [t])
          )
      )
    )
  where
    [x, h, t] = map V ["x", "h", "t"]

has5 :: [Def G X]
has5 = has5Def : is5

checkList5Def :: Def G X
checkList5Def =
    ( Def "checkList5" ["x"]
      (
        call "has5" [x] &&& call "genLists" [x]
      )
    )
  where
    x = V "x"

checkList5 :: [Def G X]
checkList5 = checkList5Def : genLists ++ has5

checkList5'Def :: Def G X
checkList5'Def =
    ( Def "checkList5" ["x"]
      (
        call "genLists" [x] &&& call "has5" [x]
      )
    )
  where
    x = V "x"

checkList5' :: [Def G X]
checkList5' = checkList5'Def : genLists ++ has5

memAppDef :: Def G X
memAppDef =
    ( Def "memApp" ["h", "xs", "ys", "rs"]
      (call "membero" [h, xs] &&& call "appendo" [xs, ys, zs])
    )
  where
    [h, xs, ys, zs] = map V ["h", "xs", "ys", "zs"]

memApp :: [Def G X]
memApp = memAppDef : membero ++ appendo

memAppYDef :: Def G X
memAppYDef =
    ( Def "memAppY" ["h", "xs", "ys", "rs"]
      (call "membero" [h, ys] &&& call "appendo" [xs, ys, zs])
    )
  where
    [h, xs, ys, zs] = map V ["h", "xs", "ys", "zs"]

memAppY :: [Def G X]
memAppY = memAppYDef : membero ++ appendo

funDef :: Def G X
funDef =
    ( Def "fun" ["n", "x", "r"]
      (
        (call "eveno" [n] &&& call "f" [n, x, r]) |||
        (call "oddo"  [n] &&& call "g" [n, x, r])
      )
    )
  where
    [n, x, r] = map V ["n", "x", "r"]

fun :: [Def G X]
fun = funDef : eveno ++ oddo ++ f ++ g

fDef :: Def G X
fDef =
    ( Def "f" ["n", "x", "r"]
      (
        (call "eveno" [n] &&& call "fun" [n, x, r]) |||
        (call "oddo"  [n] &&& call "g"   [n, x, r])
      )
    )
  where
    [n, x, r] = map V ["n", "x", "r"]

f :: [Def G X]
f = fDef : g ++ fun

gDef :: Def G X
gDef =
    ( Def "g" ["n", "x", "r"]
      (
        (call "eveno" [n] &&& call "f"   [n, x, r]) |||
        (call "oddo"  [n] &&& call "fun" [n, x, r])
      )
    )
  where
    [n, x, r] = map V ["n", "x", "r"]

g :: [Def G X]
g = gDef : f ++ fun

evenoDef :: Def G X
evenoDef =
    ( Def "eveno" ["n"]
      (
        (n === zero) |||
        (fresh ["k"] (n === succ k &&& call "oddo" [k]))
      )
    )
  where
    [n, k] = map V ["n", "k"]

eveno :: [Def G X]
eveno = [evenoDef, oddoDef]

oddoDef :: Def G X
oddoDef =
    ( Def "oddo" ["n"]
      (
        (n === succ zero) |||
        (fresh ["k"] (n === succ k &&& call "eveno" [k]))
      )
    )
  where
    [n, k] = map V ["n", "k"]

oddo :: [Def G X]
oddo = eveno

repDef :: Def G X
repDef =
    ( Def "rep" ["n", "x"]
      (
        (n === zero &&& x === nil) |||
        (fresh ["n'", "x'"]
          (
            n === succ n' &&& x === zero % x' &&& call "rep" [n', x']
          )
        )
      )
    )
  where
    [n, x, n', x'] = map V ["n", "x", "n'", "x'"]

rep :: [Def G X]
rep = [repDef]