module List where

import Syntax
import Num
import Bool
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
  let x = V "x" in
  let h = V "h" in
  let t = V "t" in
  Let (def "listo" ["x"] (( x === nil ) ||| (fresh ["h", "t"] (x === h % t &&& call "listo" [t])))
      ) g

membero :: G a -> G a
membero g =
  let x = V "x" in
  let list = V "list" in
  let h = V "h" in
  let t = V "t" in
  Let ( def "membero" ["x", "list"]
        (
          fresh ["h", "t"] ( ( list === h % t ) &&&
                             ( ( x === h ) |||
                               ( call "membero" [x, t])
                             )
                           )
        )
      ) g

inBotho :: G a -> G a
inBotho g =
  let x = V "x" in
  let ys = V "ys" in
  let zs = V "zs" in
  Let ( def "inBotho" ["x", "ys", "zs"] ( call "membero" [x, ys] &&& call "membero" [x, zs]) ) $ membero g

nilo :: G a -> G a
nilo g =
  let l = V "l" in
  Let ( def "nilo" ["l"] ( l === nil ) ) g

singletono :: G a -> G a
singletono g =
  let l = V "l" in
  let x = V "x" in
  Let ( def "singletono" ["l", "x"] ( l === x % nil ) ) g

maxLengtho :: G a -> G a
maxLengtho g =
  Let (def "maxLengtho" ["x", "m", "l"] (call "maxo" [x, m] &&& call "lengtho" [x, l])) $ maxo $ lengtho g
  where
    x = V "x"
    m = V "m"
    l = V "l"

copy :: G a -> G a
copy g =
  Let (def "copy" ["l", "c"]
        (
          (l === nil &&& c === nil) |||
          (fresh ["h", "t", "t'"]
            (
              l === h % t &&& c === h % t' &&& call "copy" [t, t']
            )
          )
        )
      ) g
    where
      l = V "l"
      c = V "c"
      h = V "h"
      t = V "t"
      t' = V "t'"

copy2 :: G a -> G a
copy2 g =
  Let (def "copy2" ["l", "c"]
        (
          (l === nil &&& c === nil) |||
          (fresh ["h"] (l === h % nil &&& c === h % nil)) |||
          (fresh ["h1", "h2", "t", "t'"]
            (
              l === h1 % (h2 % t) &&& c === h1 % t' &&& call "copy2" [t, t']
            )
          )
        )
      ) g
    where
      l = V "l"
      c = V "c"
      h = V "h"
      h1 = V "h1"
      h2 = V "h2"
      t = V "t"
      t' = V "t'"

copycopy :: G a -> G a
copycopy g =
  Let (def "copycopy" ["l", "l1", "l2"]
        (call "copy" [l, l1] &&& call "copy2" [l, l2])
      ) $ copy $ copy2 g
    where
      l = V "l"
      l1 = V "l1"
      l2 = V "l2"



lengtho :: G a -> G a
lengtho g =
  let x = V "x" in
  let l = V "l" in
  let h = V "h" in
  let t = V "t" in
  let z = V "z" in
  Let (def "lengtho" ["x", "l"]
        (
          (x === nil &&& l === zero) |||
           fresh ["h", "t", "z"]
             (x === h % t &&& l === succ z &&& call "lengtho" [t, z])
        )
      ) g

maxo :: G a -> G a
maxo g =
  Let (def "maxo" ["x", "m"] (call "maxo1" [x, zero, m])) $ maxo1 g
  where
    maxo1 g =
      Let (def "maxo1" ["x", "n", "m" ]
            (
              (x === nil &&& m === n) |||
              fresh ["h", "t", "z"] (x === h % t &&& call "leo" [h, n, trueo]  &&& call "maxo1" [t, n, m]) |||
              fresh ["h", "t", "z"] (x === h % t &&& call "gto" [h, n, trueo]  &&& call "maxo1" [t, h, m])
            )
          ) $ leo $ gto g
    x = V "x"
    m = V "m"
    n = V "n"
    h = V "h"
    t = V "t"
    z = V "z"

appLengtho :: G a -> G a 
appLengtho g = 
  Let (
    def "appLengtho" [] (
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
  let x  = V "x"  in
  let y  = V "y"  in
  let xy = V "xy" in
  let h  = V "h"  in
  let t  = V "t"  in
  let ty = V "ty" in
  Let
    (def "appendo" ["x", "y", "xy"]
         ((x === nil &&& xy === y) |||
           fresh ["h", "t", "ty"]
             (x === h % t &&& xy === h % ty &&& call "appendo" [t, y, ty])
         )
    ) g

appendo' :: G a -> G a
appendo' g =
  let x  = V "x"  in
  let y  = V "y"  in
  let xy = V "xy" in
  let h  = V "h"  in
  let t  = V "t"  in
  let ty = V "ty" in
  Let
    (def "appendo'" ["x", "y", "xy"]
           ((x === nil ||| xy === y) |||
             fresh ["h", "t", "ty"]
               (x === h % t ||| xy === h % ty ||| call "appendo'" [t, y, ty])
           )
    ) g

reverso :: G a -> G a
reverso g =
  let x  = V "x"  in
  let y  = V "y"  in
  let h  = V "h"  in
  let t  = V "t"  in
  let rt = V "rt" in
  Let
    (def "reverso" ["x", "y"]
           ((x === nil &&& y === nil) |||
             fresh ["h", "t", "rt"]
               (x === h % t &&&
                 call "reverso" [t, rt] &&& call "appendo" [rt, h % nil, y])
           )
    ) $ appendo g

doubleReverso :: G a -> G a 
doubleReverso g = 
  let xs = V "xs" in 
  let sx = V "sx" in 
  Let 
    (def "doubleReverso" ["xs"]
      (fresh ["sx"] (call "reverso" [xs, sx] &&& call "reverso" [sx, xs]))
    ) $ reverso g 

revAcco :: G a -> G a
revAcco g =
  let xs = V "xs"
      acc = V "acc"
      sx = V "sx"
      h = V "h"
      t = V "t"
  in
  Let
    (def "revacco" ["xs", "acc", "sx"]
       (
         (xs === nil &&& sx === acc) |||
         (fresh ["h", "t"]
           (xs === h % t) &&&
           call "revacco" [t, h % acc, sx]
         )
       )
    ) g


assoco g = 
  Let (def "assoco" ["x", "xs", "v"] (
    fresh ["a", "b", "tl"] (
      xs === (C "pair" [a, b]) % tl &&& 
      (a === x &&& b === v ||| 
       call "assoco" [x, tl, v] -- &&& a =/= x
      )
  ))) g
    where [x, xs, v, a, b, tl] = map V ["x", "xs", "v", "a", "b", "tl"]

  -- let rec assoco x xs v =
  --   Fresh.three (fun a b tl ->
  --     (xs === (LPair.pair a b) % tl) &&&
  --     conde [
  --       (a === x) &&& (b === v);
  --       (a =/= x) &&& (assoco x tl v)
  --     ]
  --   )