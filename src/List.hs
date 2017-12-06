module List where

import Syntax
import Stream
import Prelude hiding (succ)

-- Tests
infixr 9 %

nil   = C "Nil"  []
x % y = C "Cons" [x, y]
i x   = C x      []

a = i "a"
b = i "b"
c = i "c"
d = i "d"

list (V n) = "._" ++ show n
list (C "Cons" [h, t]) = list h ++ " % " ++ list t
list (C "Nil"  _     ) = "nil"
list (C s []) = s

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
          (fresh ["h", "t", "ty"] 
             (x  === h % t  &&&
              xy === h % ty &&&
              call "appendo" [t, y, ty]
             )
          )
         )
    ) g

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
            (fresh ["h", "t", "ty"] 
               (x  === h % t  |||
                xy === h % ty |||
                call "appendo'" [t, y, ty]
               )
            )
           )
    ) g

reverso g =
  let x  = V "x"  in
  let y  = V "y"  in
  let h  = V "h"  in
  let t  = V "t"  in
  let rt = V "rt" in
  Let 
    (def "reverso" ["x", "y"]
           ((x === nil &&& y === nil) |||
            (fresh ["h", "t", "rt"]
               (x === h % t &&&
                call "reverso" [t, rt] &&&
                call "appendo" [rt, h % nil, y]
               )
            )
           )
    ) $ appendo g

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
