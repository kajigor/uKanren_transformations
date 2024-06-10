module Program.Map5 where

import           Def
import           Prelude      hiding (succ)
import           Program.Bool
import           Program.List (nil, (%))
import           Program.Num
import           Syntax

map5Def :: Def G X
map5Def =
    ( Def "map5" ["f", "i", "a", "xs"]
      (
        call "geo" [V "i", peanify 5, trueo] |||
        (
          call "lto" [V "i", peanify 5, trueo] &&&
          call "apply" [V "f", V "a", V "i", V "h"] &&&
          call "map5" [V "f", succ (V "i"), V "a", V "t"]
        )
      )
    )

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
