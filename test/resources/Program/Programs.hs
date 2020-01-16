module Program.Programs where

import Program.List
import Program.Num
import Syntax
import Prelude hiding (succ)

palindromo :: G a -> G a
palindromo g = 
  Let 
    ( Def "palindromo" ["x"]
      (
        call "reverso" [x, x]
      )
    ) $ reverso g
    where x = V "x"

someAppendo :: G a -> G a
someAppendo g =
  Let 
    ( Def "someAppendo" ["x", "y", "z"]
      (
        fresh ["t"] ( call "appendo" [x, y, z] &&& call "appendo" [y, x, z] )
      )
    ) $ appendo g
    where [x, y, z] = map V ["x", "y", "z"]



doubleAppendo :: G a -> G a
doubleAppendo g =
  Let 
    ( Def "doubleAppendo" ["x", "y", "z", "r"]
      (
        fresh ["t"] ( call "appendo" [x, y, t] &&& call "appendo" [t, z, r] )
      )
    ) $ appendo g
    where [x, y, z, r, t] = map V ["x", "y", "z", "r", "t"]

-- eveno :: G a -> G a
-- eveno g =
--   Let 
--     ( Def "eveno" ["x"] 
--       ( 
--         fresh ["z"] (call "addo" [z, z, x])
--       )
--     ) $ addo g
--     where [x, z] = map V ["x", "z"]

doubleo :: G a -> G a
doubleo g =
  Let 
    ( Def "doubleo" ["x", "xx"] 
      (
        call "appendo" [x, x, xx]
      )
    ) $ appendo g
    where [x, xx] = map V ["x", "xx"]

emptyAppendo :: G a -> G a
emptyAppendo g =
  Let 
    ( Def "emptyAppendo" ["x", "y"] 
      (
        call "appendo" [nil, x, y]
      )
    ) $ appendo g
    where [x, y] = map V ["x", "y"]

toList [] = nil
toList (c:cs) = peanify c % toList cs

appendo123 :: G a -> G a
appendo123 g =
  Let 
    ( Def "appendo123" ["x", "y"] 
      (
        call "appendo" [toList [1..3], x, y]
      )
    ) $ appendo g
    where [x, y] = map V ["x", "y"]
  
appendoXyz :: G a -> G a
appendoXyz g =
  Let 
    ( Def "appendoXyz" ["x", "y", "z", "t", "r"] 
      (
        call "appendo" [x % (y % (z % nil)), t, r]
      )
    ) $ appendo g
    where [x, y, z, r, t] = map V ["x", "y", "z", "r", "t"]


singletonReverso :: G a -> G a
singletonReverso g =
  Let 
    ( Def "singletonReverso" ["x", "y"] 
      (
        fresh ["l"] (call "lengtho" [x, peanify 1] &&& call "reverso" [x, y])
      )
    ) $ reverso $ lengtho g
    where [x, y] = map V ["x", "y"]

externalVar :: G [Char]
externalVar =
  fresh ["x", "y"{-, "z"-}] (
    x === toList [2..4] &&&
    Let 
      ( Def "appendo" ["a", "b"] 
        (
          a === nil &&& b === x |||
          fresh ["c", "cs", "d", "ds"] (a === c % cs &&& b === c % ds &&& call "appendo" [cs, ds])
        )
      )
    (appendo $ call "appendo" [y, {-z-} toList [0..5]]))
    where [x, y, {-z, -} a, b, c, cs, ds] = map V ["x", "y", {-"z", -} "a", "b", "c", "cs", "ds"]

is5 :: G a -> G a
is5 g = Let ( Def "is5" ["x"] (V "x" === peanify 5)) g

isNum :: G a -> G a
isNum g =
  Let 
    ( Def "isNum" ["x"] 
      (
        (x === zero) ||| (fresh ["y"] (x === succ y))
      )
    ) g
    where [x, y] = map V ["x", "y"]

check5 :: G a -> G a
check5 g =
  Let 
    ( Def "check5" ["x"] 
      (
        call "isNum" [x] &&& call "is5" [x]
      )
    ) $ isNum $ is5 g
    where
      x = V "x"

genLists :: G a -> G a
genLists g =
  Let ( Def "genLists" ["x"]
        (
          (fresh ["y"] (x === y % nil &&& call "isNum" [y])) |||
          (fresh ["h", "t"] (x === h % t &&& call "isNum" [h] &&& call "genLists" [t]))
        )
      ) $ isNum g
    where [x, y, h, t] = map V ["x", "y", "h", "t"]

has5 :: G a -> G a
has5 g =
  Let ( Def "has5" ["x"]
        ( fresh ["h", "t"]
            ((x === h % t &&& call "is5" [h]) |||
             (x === h % t &&& call "has5" [t])
            )
        )
      ) $ is5 g
    where [x, h, t] = map V ["x", "h", "t"]

checkList5 :: G a -> G a
checkList5 g =
  Let 
    ( Def "checkList5" ["x"] 
      (
        call "has5" [x] &&& call "genLists" [x]
      )
    ) $ genLists $ has5 g
    where x = V "x"

checkList5' :: G a -> G a
checkList5' g =
  Let 
    ( Def "checkList5" ["x"] 
      (
        call "genLists" [x] &&& call "has5" [x]
      )
    ) $ genLists $ has5 g
    where x = V "x"


memApp :: G a -> G a 
memApp g = 
  Let 
    ( Def "memApp" ["h", "xs", "ys", "rs"]
      (call "membero" [h, xs] &&& call "appendo" [xs, ys, zs])
    ) $ membero $ appendo g 
    where [h, xs, ys, zs] = map V ["h", "xs", "ys", "zs"]

memAppY :: G a -> G a 
memAppY g = 
  Let 
    ( Def "memAppY" ["h", "xs", "ys", "rs"]
      (call "membero" [h, ys] &&& call "appendo" [xs, ys, zs])
    ) $ membero $ appendo g 
    where [h, xs, ys, zs] = map V ["h", "xs", "ys", "zs"]

fun :: G a -> G a 
fun goal =
  Let 
    ( Def "fun" ["n", "x", "r"] 
      (
        (call "eveno" [n] &&& call "f" [n, x, r]) ||| 
        (call "oddo"  [n] &&& call "g" [n, x, r])
      )
    ) $ eveno $ oddo $ f $ g goal
    where [n, x, r] = map V ["n", "x", "r"]
  
f :: G a -> G a
f goal = 
  Let 
    ( Def "f" ["n", "x", "r"]
      (
        (call "eveno" [n] &&& call "fun" [n, x, r]) ||| 
        (call "oddo"  [n] &&& call "g"   [n, x, r])
      )
    ) $ g $ fun goal
    where [n, x, r] = map V ["n", "x", "r"]

g :: G a -> G a
g goal = 
  Let 
    ( Def "g" ["n", "x", "r"]
      (
        (call "eveno" [n] &&& call "f"   [n, x, r]) ||| 
        (call "oddo"  [n] &&& call "fun" [n, x, r])
      )
    ) $ f $ fun goal
    where [n, x, r] = map V ["n", "x", "r"]
  

eveno :: G a -> G a 
eveno g = 
  Let 
    ( Def "eveno" ["n"]
      (
        (n === zero) ||| 
        (fresh ["k"] (n === succ k &&& call "oddo" [k]))
      )
    ) $ oddo g 
    where [n, k] = map V ["n", "k"]

oddo :: G a -> G a
oddo g = 
  Let 
    ( Def "oddo" ["n"]
      (
        (n === succ zero) ||| 
        (fresh ["k"] (n === succ k &&& call "eveno" [k]))
      )
    ) $ eveno g 
    where [n, k] = map V ["n", "k"]