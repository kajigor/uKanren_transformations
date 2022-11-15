module Program.List where

import Syntax
import Program
import Def
import Program.Num
import Program.Bool
import Prelude hiding (succ)
import Text.Printf
import Program.Option

-- Tests
infixr 9 %

nil :: Term a
nil = C "Nil" []

(%) :: Term a -> Term a -> Term a
x % y = C "Cons" [x, y]

lit :: Name -> Term a
lit x = C x []

a :: Term a
a = lit "a"

b :: Term a
b = lit "b"

c :: Term a
c = lit "c"

d :: Term a
d = lit "d"

list :: Show a => Term a -> String
list (V n) = printf "._%s" (show n)
list (C "Cons" [h, t]) = printf "%s %% %s" (list h) (list t)
list (C "Nil"  _     ) = "nil"
list (C s []) = s
list x = show x

listoDef :: Def G X
listoDef =
    ( Def "listo" ["x"]
      (
        ( x === nil ) |||
        (
          fresh ["h", "t"]
            (
              x === h % t &&&
              call "listo" [t]
            )
        )
      )
    )
  where
    [x, h, t] = map V ["x", "h", "t"]

listo :: [Def G X]
listo = [listoDef]

memberoDef :: Def G X
memberoDef =
    ( Def "membero" ["x", "list"]
      (
        fresh ["h", "t"] ( ( list === h % t ) &&&
                            ( ( x === h ) |||
                              ( call "membero" [x, t])
                            )
                          )
      )
    )
  where
    [x, list, h, t] = map V ["x", "list", "h", "t"]

membero :: [Def G X]
membero = [memberoDef]

inBothoDef :: Def G X
inBothoDef =
    ( Def "inBotho" ["x", "ys", "zs"]
      (
        call "membero" [x, ys] &&&
        call "membero" [x, zs]
      )
    )
  where
    [x, ys, zs] = map V ["x", "ys", "zs"]

inBotho :: [Def G X]
inBotho = inBothoDef : membero

niloDef :: Def G X
niloDef =
    ( Def "nilo" ["l"]
      (
        l === nil
      )
    )
  where
    l = V "l"

nilo :: [Def G X]
nilo = [niloDef]

singletonoDef :: Def G X
singletonoDef =
    ( Def "singletono" ["l", "x"]
      (
        l === x % nil
      )
    )
  where
    [l, x] = map V ["l", "x"]

singletono :: [Def G X]
singletono = [singletonoDef]

maxLengthoDef :: Def G X
maxLengthoDef =
    ( Def "maxLengtho" ["x", "m", "l"]
      (
        call "maxo" [x, m] &&&
        call "lengtho" [x, l]
      )
    )
  where
    [x, m, l] = map V ["x", "m", "l"]

maxLengtho :: [Def G X]
maxLengtho = maxLengthoDef : maxo ++ lengtho

maxMinoDef :: Def G X
maxMinoDef =
    ( Def "maxMino" ["x", "m", "l"]
      (
        call "maxo" [x, m] &&&
        call "mino" [x, l]
      )
    )
  where
    [x, m, l] = map V ["x", "m", "l"]

maxMino :: [Def G X]
maxMino = maxMinoDef : maxo ++ mino

copyDef :: Def G X
copyDef =
    ( Def "copy" ["l", "c"]
        (
          (l === nil &&& c === nil) |||
          (
            fresh ["h", "t", "t'"]
            (
              l === h % t &&&
              c === h % t' &&&
              call "copy" [t, t']
            )
          )
        )
      )
  where
    [l, c, h, t, t'] = map V ["l", "c", "h", "t", "t'"]

copy :: [Def G X]
copy = [copyDef]

copy2Def :: Def G X
copy2Def =
    ( Def "copy2" ["l", "c"]
        (
          (l === nil &&& c === nil) |||
          (
            fresh ["h"]
            (
              l === h % nil &&&
              c === h % nil)
          ) |||
          (
            fresh ["h1", "h2", "t", "t'"]
            (
              l === h1 % (h2 % t) &&&
              c === h1 % t' &&&
              call "copy2" [t, t']
            )
          )
        )
      )
  where
    [l, c, h, h1, h2, t, t'] = map V ["l", "c", "h", "h1", "h2", "t", "t'"]

copycopyDef :: Def G X
copycopyDef =
    ( Def "copycopy" ["l", "l1", "l2"]
      (
        call "copy" [l, l1] &&&
        call "copy2" [l, l2]
      )
    )
  where
    [l, l1, l2] = map V ["l", "l1", "l2"]

copycopy :: [Def G X]
copycopy = [copycopyDef, copyDef, copy2Def]

lengthoDef :: Def G X
lengthoDef =
    ( Def "lengtho" ["x", "l"]
      (
        (x === nil &&& l === zero) |||
        fresh ["h", "t", "z"]
          (
            x === h % t &&&
            l === succ z &&&
            call "lengtho" [t, z]
          )
      )
    )
  where
    [x, l, h, t, z] = map V ["x", "l", "h", "t", "z"]

lengtho :: [Def G X]
lengtho = [lengthoDef]

lengtho'Def :: Def G X
lengtho'Def =
    ( Def "lengtho'" ["x", "l"]
      (
        (x === nil &&& l === zero) |||
        fresh ["h", "t", "z"]
          (
            x === h % t &&&
            call "lengtho'" [t, z] &&&
            l === succ z
          )
      )
    )
  where
    [x, l, h, t, z] = map V ["x", "l", "h", "t", "z"]

lengtho' :: [Def G X]
lengtho' = [lengtho'Def]

maxoDef :: Def G X
maxoDef =
    ( Def "maxo" ["x", "m"] (call "maxo1" [x, zero, m]))
  where
    [x, m] = map V ["x", "m"]

maxo1Def :: Def G X
maxo1Def =
    ( Def "maxo1" ["x", "n", "m" ]
      (
        (x === nil &&& m === n) |||
        fresh ["h", "t", "z"]
          (
            x === h % t &&&
            call "leo" [h, n, trueo]  &&&
            call "maxo1" [t, n, m]
          ) |||
        fresh ["h", "t", "z"]
          (
            x === h % t &&&
            call "gto" [h, n, trueo] &&&
            call "maxo1" [t, h, m]
          )
      )
    )
  where
    [x, m, n, h, t, z] = map V ["x", "m", "n", "h", "t", "z"]

maxo1 :: [Def G X]
maxo1 = maxo1Def : leo ++ gto

maxo :: [Def G X]
maxo = maxoDef : maxo1

minoDef :: Def G X
minoDef =
    ( Def "mino" ["x", "m"]
      (
        ( x === nil &&& m === zero) |||
        fresh ["h", "t"]
        (
          x === h % t &&&
          call "mino1" [t, h, m]
        )
      )
    )
  where
    [x, m, h, t] = map V ["x", "m", "h", "t"]

mino1Def :: Def G X
mino1Def =
    ( Def "mino1" ["x", "n", "m" ]
      (
        (x === nil &&& m === n) |||
        fresh ["h", "t", "z"]
          (
            x === h % t &&&
            call "leo" [h, n, trueo]  &&&
            call "mino1" [t, h, m]
          ) |||
        fresh ["h", "t", "z"]
          (
            x === h % t &&&
            call "gto" [h, n, trueo] &&&
            call "mino1" [t, n, m]
          )
      )
    )
  where
    [x, m, n, h, t, z] = map V ["x", "m", "n", "h", "t", "z"]

mino1 :: [Def G X]
mino1 = mino1Def : leo ++ gto

mino :: [Def G X]
mino = minoDef : mino1

appLengthoDef :: Def G X
appLengthoDef =
    ( Def "appLengtho" []
      (
        fresh ["xs", "ys", "zs", "m", "n", "s"]
          (
            call "appendo" [xs, ys, zs] &&&
            call "lengtho" [xs, m] &&&
            call "lengtho" [ys, n] &&&
            call "lengtho" [zs, s] &&&
            call "addo" [m, n, s]
          )
      )
    )
  where
    [xs, ys, zs, m, n, s] = map V ["xs", "ys", "zs", "m", "n", "s"]

appLengtho :: [Def G X]
appLengtho = appLengthoDef : addo ++ lengtho ++ appendo

appendoDef :: Def G X
appendoDef =
    ( Def "appendo" ["x", "y", "xy"]
      (
        (x === nil &&& xy === y) |||
        fresh ["h", "t", "ty"]
          (
            x === h % t &&&
            xy === h % ty &&&
            call "appendo" [t, y, ty]
          )
      )
    )
  where
    [x, y, xy, h, t, ty] = map V ["x", "y", "xy", "h", "t", "ty"]

appendo :: [Def G X]
appendo = [appendoDef]

appendo'Def :: Def G X
appendo'Def =
    ( Def "appendo'" ["x", "y", "xy"]
      (
        (x === nil ||| xy === y) |||
        fresh ["h", "t", "ty"]
          (
            x === h % t |||
            xy === h % ty |||
            call "appendo'" [t, y, ty]
          )
      )
    )
  where
    [x, y, xy, h, t, ty] = map V ["x", "y", "xy", "h", "t", "ty"]

appendo' :: [Def G X]
appendo' = [appendo'Def]

reversoDef :: Def G X
reversoDef =
    ( Def "reverso" ["x", "y"]
      (
        (x === nil &&& y === nil) |||
        fresh ["h", "t", "rt"]
          (
            x === h % t &&&
            call "reverso" [t, rt] &&&
            call "appendo" [rt, h % nil, y]
          )
      )
    )
  where
    [x, y, h, t, rt] = map V ["x", "y", "h", "t", "rt"]

reverso :: [Def G X]
reverso = reversoDef : appendo

doubleReversoDef :: Def G X
doubleReversoDef =
    ( Def "doubleReverso" ["xs"]
      (
        fresh ["sx"]
        (
          call "reverso" [xs, sx] &&& call "reverso" [sx, xs]
        )
      )
    )
  where
    [xs, sx] = map V ["xs", "sx"]

doubleReverso :: [Def G X]
doubleReverso = doubleReversoDef : reverso

revAccoDef :: Def G X
revAccoDef =
    ( Def "revacco" ["xs", "acc", "sx"]
      (
        xs === nil &&& sx === acc |||
        (
          fresh ["h", "t"]
            (
              (xs === h % t) &&&
              call "revacco" [t, h % acc, sx]
            )
        )
      )
    )
  where
    [xs, acc, sx, h, t] = map V ["xs", "acc", "sx", "h", "t"]

revAcco :: [Def G X]
revAcco = [revAccoDef]

assocoDef :: Def G X
assocoDef =
    ( Def "assoco" ["x", "xs", "v"]
      (
        fresh ["a", "b", "tl"]
          (
            xs === (C "pair" [a, b]) % tl &&&
            (
              a === x &&& b === v |||
              call "assoco" [x, tl, v] -- &&& a =/= x
            )
          )
      )
    )
  where [x, xs, v, a, b, tl] = map V ["x", "xs", "v", "a", "b", "tl"]

assoco :: [Def G X]
assoco = [assocoDef]

nthOptDef :: Def G X
nthOptDef =
    ( Def "nthOpt" ["xs", "n", "r"]
      (
        fresh ["h", "t", "x"]
        (
          xs === nil &&& r === none |||
          xs === h % t &&&
            (n === zero &&& r === some h |||
             n === succ x &&& call "nthOpt" [t, n, r]
            )
        )
      )
    )
  where
    [xs, n, r, h, t, x] = map V ["xs", "n", "r", "h", "t", "x"]

nthOpt :: [Def G X]
nthOpt = [nthOptDef]

  -- let rec assoco x xs v =
  --   Fresh.three (fun a b tl ->
  --     (xs === (LPair.pair a b) % tl) &&&
  --     conde [
  --       (a === x) &&& (b === v);
  --       (a =/= x) &&& (assoco x tl v)
  --     ]
  --   )