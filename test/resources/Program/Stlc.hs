module Program.Stlc where

import Prelude hiding (abs)
import Syntax
import Program
import Def

var :: Term a -> Term a
var x   = C "V" [x]

abs :: Term a -> Term a -> Term a
abs v b = C "Abs" [v, b]

app :: Term a -> Term a -> Term a
app p q = C "App" [p, q]

substo :: [Def G X]
substo = [substoDef]

substoDef :: Def G X
substoDef =
    ( Def "substo" ["l", "x", "a", "l'"]
      (
        fresh ["y"]
        (
          l === var y &&&
          y === x &&&
          l' === a
        ) |||
        fresh ["m", "n", "m'", "n'"]
        (
          l === app m n &&&
          l' === app m' n' &&&
          call "substo" [m, x, a, m'] &&&
          call "substo" [n, x, a, n']
        ) |||
        fresh ["v", "b"]
        (
          l === abs v b &&&
          (
            x === v &&& l' === l |||
            fresh ["b'"]
            (
              l' === abs v b' &&&
              call "substo" [b, x, a, b']
            )
          )
        )
      )
    )
  where
    [l, x, a, l', y, m, n, m', n', v, b, b'] =
      map V ["l", "x", "a", "l'", "y", "m", "n", "m'", "n'", "v", "b", "b'"]

evalo :: [Def G X]
evalo = evaloDef : substo

evaloDef :: Def G X
evaloDef =
    ( Def "evalo" ["m", "n"]
      (
        fresh ["x"]
        (
          m === var x &&& n === m
        ) |||
        fresh ["x", "l"]
        (
          m === abs x l &&&
          n === m
        ) |||
        fresh ["f", "a", "f'", "a'"]
        (
          m === app f a &&&
          call "evalo" [f, f'] &&&
          call "evalo" [a, a'] &&&
          (
            fresh ["x", "l", "l'"]
            (
              f' === abs x l &&&
              call "substo" [l, x, a', l'] &&&
              call "evalo" [l', n]
            ) |||
            fresh ["p", "q"]
            (
              f' === app p q &&&
              n === app f' a'
            ) |||
            fresh ["x"]
            (
              f' === var x &&&
              n === app f' a'
            )
          )
        )
      )
    )
  where
    [m, n, x, l, l', f, a, f', a', p, q] =
            map V ["m", "n", "x", "l", "l'", "f", "a", "f'", "a'", "p", "q"]
