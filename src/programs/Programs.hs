module Programs where

import List
import Num
import Syntax
import Prelude hiding (succ)

palindromo :: G a -> G a
palindromo g = let x = V "x" in Let (def "palindromo" ["x"] (call "reverso" [x, x])) $ reverso g

doubleAppendo :: G a -> G a
doubleAppendo g =
  let x = V "x" in
  let y = V "y" in
  let z = V "z" in
  let r = V "r" in
  let t = V "t" in
  Let (def "doubleAppendo" ["x", "y", "z", "r"]
        (
          fresh ["t"] ( call "appendo" [x, y, t] &&& call "appendo" [t, z, r] )
        )
      ) $ appendo g

eveno :: G a -> G a
eveno g =
  let x = V "x" in
  let z = V "z" in
  Let (def "eveno" ["x"] (fresh ["z"] (call "addo" [z, z, x]))) $ addo g

doubleo :: G a -> G a
doubleo g =
  let x = V "x" in
  let xx = V "xx" in
  Let (def "doubleo" ["x", "xx"] (call "appendo" [x, x, xx])) $ appendo g

emptyAppendo :: G a -> G a
emptyAppendo g =
  let x = V "x" in
  let y = V "y" in
  Let (def "emptyAppendo" ["x", "y"] (call "appendo" [nil, x, y])) $ appendo g

toList [] = nil
toList (c:cs) = peanify c % toList cs

appendo123 :: G a -> G a
appendo123 g =
  let x = V "x" in
  let y = V "y" in
  Let (def "appendo123" ["x", "y"] (call "appendo" [toList [1..3], x, y])) $ appendo g

appendoXyz :: G a -> G a
appendoXyz g =
  let x = V "x" in
  let y = V "y" in
  let z = V "z" in
  let t = V "t" in
  let r = V "r" in
  Let (def "appendoXyz" ["x", "y", "z", "t", "r"] (call "appendo" [x % (y % (z % nil)), t, r])) $ appendo g


singletonReverso :: G a -> G a
singletonReverso g =
  let x = V "x" in
  let y = V "y" in
  Let (def "singletonReverso" ["x", "y"] (fresh ["l"] (call "lengtho" [x, peanify 1] &&& call "reverso" [x, y]))) $ reverso $ lengtho g

externalVar :: G [Char]
externalVar =
  let x  = V "x"  in
  let y  = V "y"  in
--let z  = V "z"  in
  let a  = V "a"  in
  let b  = V "b"  in
  let c  = V "c"  in
  let cs = V "cs" in
  let ds = V "ds" in
  fresh ["x", "y"{-, "z"-}] (
    x === toList [2..4] &&&
    Let (def "appendo" ["a", "b"] (
      a === nil &&& b === x |||
      fresh ["c", "cs", "d", "ds"] (a === c % cs &&& b === c % ds &&& call "appendo" [cs, ds])))
    (call "appendo" [y, {-z-} toList [0..5]]))

is5 :: G a -> G a
is5 g = Let (def "is5" ["x"] (V "x" === peanify 5)) g

isNum :: G a -> G a
isNum g =
  Let (def "isNum" ["x"] ((x === zero) ||| (fresh ["y"] (x === succ y)))) g
    where
      x = V "x"
      y = V "y"

check5 :: G a -> G a
check5 g =
  Let (def "check5" ["x"] (call "isNum" [x] &&& call "is5" [x])) $ isNum $ is5 g
    where
      x = V "x"

genLists :: G a -> G a
genLists g =
  Let (def "genLists" ["x"]
        (
          (fresh ["y"] (x === y % nil &&& call "isNum" [y])) |||
          (fresh ["h", "t"] (x === h % t &&& call "isNum" [h] &&& call "genLists" [t]))
        )
      ) $ isNum g
    where
      x = V "x"
      y = V "y"
      h = V "h"
      t = V "t"

has5 :: G a -> G a
has5 g =
  Let (def "has5" ["x"]
        ( fresh ["h", "t"]
            ((x === h % t &&& call "is5" [h]) |||
             (x === h % t &&& call "has5" [t])
            )
        )
      ) $ is5 g
    where
      x = V "x"
      h = V "h"
      t = V "t"

checkList5 :: G a -> G a
checkList5 g =
  Let (def "checkList5" ["x"] (call "has5" [x] &&& call "genLists" [x])) $ genLists $ has5 g
    where x = V "x"

checkList5' :: G a -> G a
checkList5' g =
  Let (def "checkList5" ["x"] (call "genLists" [x] &&& call "has5" [x])) $ genLists $ has5 g
    where x = V "x"
