module Program.Programs where

import Program.List
import Program.Num
import Syntax
import Prelude hiding (succ)

palindromoDef :: Def
palindromoDef =   
    ( Def "palindromo" ["x"]
      (
        call "reverso" [x, x]
      )
    )
  where 
    x = V "x"

palindromo :: [Def]
palindromo = palindromoDef : reverso 

someAppendoDef :: Def
someAppendoDef =  
    ( Def "someAppendo" ["x", "y", "z"]
      (
        fresh ["t"] ( call "appendo" [x, y, z] &&& call "appendo" [y, x, z] )
      )
    )
  where 
    [x, y, z] = map V ["x", "y", "z"]

someAppendo :: [Def]
someAppendo = someAppendoDef : appendo 

doubleAppendoDef :: Def
doubleAppendoDef =  
    ( Def "doubleAppendo" ["x", "y", "z", "r"]
      (
        fresh ["t"] ( call "appendo" [x, y, t] &&& call "appendo" [t, z, r] )
      )
    )
  where 
    [x, y, z, r, t] = map V ["x", "y", "z", "r", "t"]

doubleAppendo :: [Def]
doubleAppendo = doubleAppendoDef : appendo

-- evenoDef :: Def
-- evenoDef =  
--     ( Def "eveno" ["x"] 
--       ( 
--         fresh ["z"] (call "addo" [z, z, x])
--       )
--     ) $ addo g
--   where 
--     [x, z] = map V ["x", "z"]

-- eveno :: [Def]
-- eveno = evenoDef : addo

doubleoDef :: Def
doubleoDef =  
    ( Def "doubleo" ["x", "xx"] 
      (
        call "appendo" [x, x, xx]
      )
    )
  where 
    [x, xx] = map V ["x", "xx"]

doubleo :: [Def]
doubleo = doubleoDef : appendo 

emptyAppendoDef :: Def
emptyAppendoDef =  
    ( Def "emptyAppendo" ["x", "y"] 
      (
        call "appendo" [nil, x, y]
      )
    )
  where 
    [x, y] = map V ["x", "y"]

emptyAppendo :: [Def]
emptyAppendo = emptyAppendoDef : appendo 

toList [] = nil
toList (c:cs) = peanify c % toList cs

appendo123Def :: Def
appendo123Def =  
    ( Def "appendo123" ["x", "y"] 
      (
        call "appendo" [toList [1..3], x, y]
      )
    )
  where 
    [x, y] = map V ["x", "y"]

appendo123 :: [Def]
appendo123 = appendo123Def : appendo 

appendoXyzDef :: Def
appendoXyzDef =  
    ( Def "appendoXyz" ["x", "y", "z", "t", "r"] 
      (
        call "appendo" [x % (y % (z % nil)), t, r]
      )
    ) 
  where 
    [x, y, z, r, t] = map V ["x", "y", "z", "r", "t"]

appendoXyz :: [Def]
appendoXyz = appendoXyzDef : appendo 

singletonReversoDef :: Def
singletonReversoDef =  
    ( Def "singletonReverso" ["x", "y"] 
      (
        fresh ["l"] (call "lengtho" [x, peanify 1] &&& call "reverso" [x, y])
      )
    )
  where 
    [x, y] = map V ["x", "y"]

singletonReverso :: [Def]
singletonReverso = singletonReversoDef : reverso ++ lengtho 

is5Def :: Def
is5Def = Def "is5" ["x"] (V "x" === peanify 5)

is5 :: [Def]
is5 = [is5Def]

isNumDef :: Def
isNumDef =  
    ( Def "isNum" ["x"] 
      (
        (x === zero) ||| (fresh ["y"] (x === succ y))
      )
    )
  where 
    [x, y] = map V ["x", "y"]

isNum :: [Def]
isNum = [isNumDef]

check5Def :: Def
check5Def =  
    ( Def "check5" ["x"] 
      (
        call "isNum" [x] &&& call "is5" [x]
      )
    )
  where
    x = V "x"

check5 :: [Def]
check5 = check5Def : isNum ++ is5

genListsDef :: Def
genListsDef =  
    ( Def "genLists" ["x"]
      (
        (fresh ["y"] (x === y % nil &&& call "isNum" [y])) |||
        (fresh ["h", "t"] (x === h % t &&& call "isNum" [h] &&& call "genLists" [t]))
      )
    )
  where 
    [x, y, h, t] = map V ["x", "y", "h", "t"]

genLists :: [Def] 
genLists = genListsDef : isNum 

has5Def :: Def
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

has5 :: [Def]
has5 = has5Def : is5

checkList5Def :: Def
checkList5Def =  
    ( Def "checkList5" ["x"] 
      (
        call "has5" [x] &&& call "genLists" [x]
      )
    )
  where 
    x = V "x"

checkList5 :: [Def]
checkList5 = checkList5Def : genLists ++ has5

checkList5'Def :: Def
checkList5'Def =  
    ( Def "checkList5" ["x"] 
      (
        call "genLists" [x] &&& call "has5" [x]
      )
    )
  where 
    x = V "x"

checkList5' :: [Def]
checkList5' = checkList5'Def : genLists ++ has5

memAppDef :: Def 
memAppDef =   
    ( Def "memApp" ["h", "xs", "ys", "rs"]
      (call "membero" [h, xs] &&& call "appendo" [xs, ys, zs])
    )
  where 
    [h, xs, ys, zs] = map V ["h", "xs", "ys", "zs"]

memApp :: [Def]
memApp = memAppDef : membero ++ appendo 

memAppYDef :: Def 
memAppYDef =   
    ( Def "memAppY" ["h", "xs", "ys", "rs"]
      (call "membero" [h, ys] &&& call "appendo" [xs, ys, zs])
    )
  where 
    [h, xs, ys, zs] = map V ["h", "xs", "ys", "zs"]

memAppY :: [Def]
memAppY = memAppYDef : membero ++ appendo 

funDef :: Def 
funDef =
    ( Def "fun" ["n", "x", "r"] 
      (
        (call "eveno" [n] &&& call "f" [n, x, r]) ||| 
        (call "oddo"  [n] &&& call "g" [n, x, r])
      )
    )
  where 
    [n, x, r] = map V ["n", "x", "r"]
  
fun :: [Def]
fun = funDef : eveno ++ oddo ++ f ++ g

fDef :: Def
fDef = 
    ( Def "f" ["n", "x", "r"]
      (
        (call "eveno" [n] &&& call "fun" [n, x, r]) ||| 
        (call "oddo"  [n] &&& call "g"   [n, x, r])
      )
    )
  where 
    [n, x, r] = map V ["n", "x", "r"]

f :: [Def] 
f = fDef : g ++ fun 

gDef :: Def
gDef = 
    ( Def "g" ["n", "x", "r"]
      (
        (call "eveno" [n] &&& call "f"   [n, x, r]) ||| 
        (call "oddo"  [n] &&& call "fun" [n, x, r])
      )
    )
  where 
    [n, x, r] = map V ["n", "x", "r"]
  
g :: [Def]
g = gDef : f ++ fun 

evenoDef :: Def 
evenoDef =   
    ( Def "eveno" ["n"]
      (
        (n === zero) ||| 
        (fresh ["k"] (n === succ k &&& call "oddo" [k]))
      )
    ) 
  where 
    [n, k] = map V ["n", "k"]

eveno :: [Def]
eveno = [evenoDef, oddoDef]

oddoDef :: Def
oddoDef =   
    ( Def "oddo" ["n"]
      (
        (n === succ zero) ||| 
        (fresh ["k"] (n === succ k &&& call "eveno" [k]))
      )
    )
  where 
    [n, k] = map V ["n", "k"]

oddo :: [Def]
oddo = eveno 