module Program.Sort where

import Prelude hiding (succ, min, max)
import Syntax
import Program.Bool
import Program.Num
import Program.List hiding (a, b)

minmaxo :: [Def]
minmaxo = minmaxoDef : leo ++ gto

minmaxoDef :: Def
minmaxoDef =
    ( Def "minmaxo" ["a", "b", "min", "max"] 
      (
        (min === a &&& max === b &&& call "leo" [a, b, trueo]) |||
        (max === a &&& min === b &&& call "gto" [a, b, trueo])
      )
    ) 
  where 
    [a, b, min, max] = map V ["a", "b", "min", "max"]

smallesto :: [Def]
smallesto = smallestoDef : minmaxo 

smallestoDef :: Def
smallestoDef =
    ( Def "smallesto" ["l", "s", "l'"] 
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
    )
  where 
    [l, s, l', h, t, s', t', max] = map V ["l", "s", "l'", "h", "t", "s'", "t'", "max"]

sorto :: [Def]
sorto = sortoDef : smallesto 

sortoDef :: Def
sortoDef =
    ( Def "sorto" ["x", "y"] 
      (
        x === nil &&& y === nil |||
        fresh ["s", "xs", "xs'"]
          ( 
            y === s % xs' &&& 
            call "sorto" [xs, xs'] &&& 
            call "smallesto" [x, s, xs] 
          )
      )
    )
  where 
    [x, y, s, xs, xs'] = map V ["x", "y", "s", "xs", "xs'"]