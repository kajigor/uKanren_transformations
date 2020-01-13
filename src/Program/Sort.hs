module Program.Sort where

import Prelude hiding (succ, min, max)
import Syntax
import Program.Bool
import Program.Num
import Program.List hiding (a, b)

minmaxo :: G a -> G a
minmaxo g =
  Let 
    ( def "minmaxo" ["a", "b", "min", "max"] 
      (
        (min === a &&& max === b &&& call "leo" [a, b, trueo]) |||
        (max === a &&& min === b &&& call "gto" [a, b, trueo])
      )
    ) $ leo $ gto g
    where [a, b, min, max] = map V ["a", "b", "min", "max"]

smallesto :: G a -> G a
smallesto g =
  Let 
    ( def "smallesto" ["l", "s", "l'"] 
      (
        l === s % nil &&& l' === nil |||
        fresh ["h", "t", "s'", "t'", "max"] 
        (
          l' === max % t' &&&
          l === h % t &&&
          call "minmaxo" [h, s', s, max] &&&
          call "smallesto" [t, s', t']
        )
      )
    ) $ minmaxo g
    where [l, s, l', h, t, s', t', max] = map V ["l", "s", "l'", "h", "t", "s'", "t'", "max"]

sorto :: G a -> G a
sorto g =
  Let 
    ( def "sorto" ["x", "y"] 
      (
        x === nil &&& y === nil |||
        fresh ["s", "xs", "xs'"]
          ( 
            y === s % xs' &&& 
            call "sorto" [xs, xs'] &&& 
            call "smallesto" [x, s, xs] 
          )
      )
    ) $ smallesto g
    where [x, y, s, xs, xs'] = map V ["x", "y", "s", "xs", "xs'"]