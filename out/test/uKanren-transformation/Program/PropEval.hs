module Program.PropEval where

import           Def
import           Prelude      hiding (succ)
import           Program
import           Program.Bool
import           Program.List
import           Program.Num
import           Syntax

plainFirstQuery = Program plainFirst $ fresh ["fm", "st"] (call "evalo" [V "st", V "fm", trueo])
plainLastQuery  = Program plainLast  $ fresh ["fm", "st"] (call "evalo" [V "st", V "fm", trueo])
nandoFirstQuery = Program nandoFirst $ fresh ["fm", "st"] (call "evalo" [V "st", V "fm", trueo])
nandoLastQuery  = Program nandoLast  $ fresh ["fm", "st"] (call "evalo" [V "st", V "fm", trueo])

plainFirst = firstEvaloDef : plainAndo ++ plainOro ++ plainNoto ++ elemo
plainLast  =  lastEvaloDef : plainAndo ++ plainOro ++ plainNoto ++ elemo
nandoFirst = firstEvaloDef : ando ++ oro ++ noto ++ elemo
nandoLast  =  lastEvaloDef : ando ++ oro ++ noto ++ elemo


plainAndo = [plainAndoDef]
plainAndoDef =
    ( Def "ando" ["x", "y", "v"]
      (
        x === trueo &&& y === trueo &&& v === trueo |||
        x === falso &&& y === trueo &&& v === falso |||
        x === trueo &&& y === falso &&& v === falso |||
        x === falso &&& y === falso &&& v === falso
      )
    )
  where
    [x, y, v] = map V ["x", "y", "v"]


plainOro = [plainOroDef]
plainOroDef =
    ( Def "oro" ["x", "y", "v"]
      (
        x === trueo &&& y === trueo &&& v === trueo |||
        x === falso &&& y === trueo &&& v === trueo |||
        x === trueo &&& y === falso &&& v === trueo |||
        x === falso &&& y === falso &&& v === falso
      )
    )
  where
    [x, y, v] = map V ["x", "y", "v"]

plainNoto = [plainNotoDef]
plainNotoDef =
    ( Def "noto" ["x", "y"]
      (
        x === trueo &&& y === falso |||
        x === falso &&& y === trueo
      )
    )
  where
    [x, y] = map V ["x", "y"]


firstEvaloDef :: Def G X
firstEvaloDef =
    ( Def "evalo" ["st", "fm", "u"]
      (
        fresh ["x", "y", "v", "w", "z"]
        (
          (
            fm === C "conj" [x, y] &&&
            call "ando" [v, w, u] &&&
            call "evalo" [st, x, v] &&&
            call "evalo" [st, y, w]
          ) |||
          (
            fm === C "disj" [x, y] &&&
            call "oro" [v, w, u] &&&
            call "evalo" [st, x, v] &&&
            call "evalo" [st, y, w]
          ) |||
          (
            fm === C "neg" [x] &&&
            call "noto" [v, u] &&&
            call "evalo" [st, x, v]
          ) |||
          (
            fm === C "var" [z] &&&
            call "elemo" [z, st, u]
          )
        )
      )
    )
    where
      [st, fm, u, x, y, v, w, z] = map V ["st", "fm", "u", "x", "y", "v", "w", "z"]

lastEvaloDef :: Def G X
lastEvaloDef =
    ( Def "evalo" ["st", "fm", "u"]
      (
        fresh ["x", "y", "v", "w", "z"]
        (
          (
            fm === C "conj" [x, y] &&&
            call "evalo" [st, x, v] &&&
            call "evalo" [st, y, w] &&&
            call "ando" [v, w, u]
          ) |||
          (
            fm === C "disj" [x, y] &&&
            call "evalo" [st, x, v] &&&
            call "evalo" [st, y, w] &&&
            call "oro" [v, w, u]
          ) |||
          (
            fm === C "neg" [x] &&&
            call "evalo" [st, x, v] &&&
            call "noto" [v, u]
          ) |||
          (
            fm === C "var" [z] &&&
            call "elemo" [z, st, u]
          )
        )
      )
    )
    where
      [st, fm, u, x, y, v, w, z] = map V ["st", "fm", "u", "x", "y", "v", "w", "z"]

elemo :: [Def G X]
elemo = [elemoDef]

elemoDef :: Def G X
elemoDef =
    ( Def "elemo" ["n", "s", "v"]
      (
        fresh ["h", "t", "n'"]
        (
          n === zero &&& s === h % t &&& v === h |||
          n === succ n' &&& s === h % t &&& call "elemo" [n', t, v]
        )
      )
    )
    where [n, s, v, h, t, n'] = map V ["n", "s", "v", "h", "t", "n'"]
