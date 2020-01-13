module Program.List where

import Syntax
import Program.Num
import Program.Bool
import Prelude hiding (succ)
import Text.Printf

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

listo :: G a -> G a
listo g =
  Let 
    ( def "listo" ["x"] 
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
    ) g
    where [x, h, t] = map V ["x", "h", "t"]

membero :: G a -> G a
membero g =
  Let ( def "membero" ["x", "list"]
        (
          fresh ["h", "t"] ( ( list === h % t ) &&&
                             ( ( x === h ) |||
                               ( call "membero" [x, t])
                             )
                           )
        )
      ) g
    where [x, list, h, t] = map V ["x", "list", "h", "t"]

inBotho :: G a -> G a
inBotho g =
  Let 
    ( def "inBotho" ["x", "ys", "zs"] 
      ( 
        call "membero" [x, ys] &&& 
        call "membero" [x, zs]
      )
    ) $ membero g
    where [x, ys, zs] = map V ["x", "ys", "zs"]

nilo :: G a -> G a
nilo g =
  Let 
    ( def "nilo" ["l"] 
      ( 
        l === nil
      )
    ) g
    where l = V "l"

singletono :: G a -> G a
singletono g =
  Let 
    ( def "singletono" ["l", "x"] 
      ( 
        l === x % nil
      )
    ) g
    where [l, x] = map V ["l", "x"]

maxLengtho :: G a -> G a
maxLengtho g =
  Let 
    ( def "maxLengtho" ["x", "m", "l"] 
      (
        call "maxo" [x, m] &&& 
        call "lengtho" [x, l]
      )
    ) $ maxo $ lengtho g
  where [x, m, l] = map V ["x", "m", "l"]

copy :: G a -> G a
copy g =
  Let ( def "copy" ["l", "c"]
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
      ) g
    where [l, c, h, t, t'] = map V ["l", "c", "h", "t", "t'"]

copy2 :: G a -> G a
copy2 g =
  Let ( def "copy2" ["l", "c"]
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
      ) g
    where [l, c, h, h1, h2, t, t'] = map V ["l", "c", "h", "h1", "h2", "t", "t'"]

copycopy :: G a -> G a
copycopy g =
  Let ( def "copycopy" ["l", "l1", "l2"]
        (
          call "copy" [l, l1] &&& 
          call "copy2" [l, l2]
        )
      ) $ copy $ copy2 g
    where [l, l1, l2] = map V ["l", "l1", "l2"]

lengtho :: G a -> G a
lengtho g =
  Let ( def "lengtho" ["x", "l"]
        (
          (x === nil &&& l === zero) |||
          fresh ["h", "t", "z"]
            (
              x === h % t &&& 
              l === succ z &&& 
              call "lengtho" [t, z]
            )
        )
      ) g
    where [x, l, h, t, z] = map V ["x", "l", "h", "t", "z"]

maxo :: G a -> G a
maxo g =
  Let ( def "maxo" ["x", "m"] (call "maxo1" [x, zero, m])) $ maxo1 g
  where
    maxo1 g =
      Let ( def "maxo1" ["x", "n", "m" ]
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
          ) $ leo $ gto g
    [x, m, n, h, t, z] = map V ["x", "m", "n", "h", "t", "z"]

appLengtho :: G a -> G a 
appLengtho g = 
  Let 
    ( def "appLengtho" [] 
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
    ) $ addo $ lengtho $ appendo g  
    where [xs, ys, zs, m, n, s] = map V ["xs", "ys", "zs", "m", "n", "s"]

appendo :: G a -> G a
appendo g =
  Let
    ( def "appendo" ["x", "y", "xy"]
      (
        (x === nil &&& xy === y) |||
        fresh ["h", "t", "ty"]
          (
            x === h % t &&& 
            xy === h % ty &&& 
            call "appendo" [t, y, ty]
          )
      )
    ) g
    where [x, y, xy, h, t, ty] = map V ["x", "y", "xy", "h", "t", "ty"]

appendo' :: G a -> G a
appendo' g =
  Let
    ( def "appendo'" ["x", "y", "xy"]
      (
        (x === nil ||| xy === y) |||
        fresh ["h", "t", "ty"]
          (
            x === h % t ||| 
            xy === h % ty ||| 
            call "appendo'" [t, y, ty]
          )
      )
    ) g
    where [x, y, xy, h, t, ty] = map V ["x", "y", "xy", "h", "t", "ty"]

reverso :: G a -> G a
reverso g =
  Let
    ( def "reverso" ["x", "y"]
      (
        (x === nil &&& y === nil) |||
        fresh ["h", "t", "rt"]
          (
            x === h % t &&&
            call "reverso" [t, rt] &&& 
            call "appendo" [rt, h % nil, y]
          )
      )
    ) $ appendo g 
    where [x, y, h, t, rt] = map V ["x", "y", "h", "t", "rt"]

doubleReverso :: G a -> G a 
doubleReverso g = 
  Let 
    ( def "doubleReverso" ["xs"]
      (
        fresh ["sx"] 
        (
          call "reverso" [xs, sx] &&& call "reverso" [sx, xs]
        )
      )
    ) $ reverso g 
    where [xs, sx] = map V ["xs", "sx"]

revAcco :: G a -> G a
revAcco g =
  Let
    ( def "revacco" ["xs", "acc", "sx"]
      (
        xs === nil &&& sx === acc |||
        (
          fresh ["h", "t"]
            (xs === h % t) &&&
            call "revacco" [t, h % acc, sx]
        )
      )
    ) g
    where [xs, acc, sx, h, t] = map V ["xs", "acc", "sx", "h", "t"]


assoco g = 
  Let 
    ( def "assoco" ["x", "xs", "v"] 
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
    ) g
    where [x, xs, v, a, b, tl] = map V ["x", "xs", "v", "a", "b", "tl"]

  -- let rec assoco x xs v =
  --   Fresh.three (fun a b tl ->
  --     (xs === (LPair.pair a b) % tl) &&&
  --     conde [
  --       (a === x) &&& (b === v);
  --       (a =/= x) &&& (assoco x tl v)
  --     ]
  --   )