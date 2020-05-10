module Program.Prop where

import Syntax
import Program.Bool
import Program.List
import Program.Num
import Prelude hiding (succ)

fm  = C "conj" [C "var" [C "x" []], C "neg" [C "var" [C "x" []]]] -- always fails
fm1 = C "conj" [C "var" [C "x" []], C "neg" [C "var" [C "y" []]]]

query  = Program evalo $ fresh ["st"] (call "evalo" [V "st", fm, trueo])
query1 = Program evalo $ fresh ["st"] (call "evalo" [V "st", fm, falso])
query2 = Program evalo $ fresh ["st"] (call "evalo" [V "st", fm1, trueo])

fm'  = C "conj" [C "var" [zero], C "neg" [C "var" [zero]]] -- always fails
fm1' = C "conj" [C "var" [zero], C "neg" [C "var" [succ zero]]]

query''1 = Program evalo'' $ fresh ["st"] (call "evalo" [V "st", fm', trueo])
query''2 = Program evalo'' $ fresh ["st"] (call "evalo" [V "st", fm', falso])
query''3 = Program evalo'' $ fresh ["st"] (call "evalo" [V "st", fm1', trueo])

query3 = Program evalo $ fresh ["fm", "st"]     (call "evalo" [V "st", V "fm", trueo])
query4 = Program evalo $ fresh ["x", "y", "st"] (call "evalo" [V "st",  C "conj" [V "x", C "neg" [V "y"]], trueo])

query'  = Program evalo'  $ fresh ["st", "fm"] (call "evalo" [V "st", V "fm", trueo])
query'' = Program evalo'' $ fresh ["st", "fm"] (call "evalo" [V "st", V "fm", trueo])

fm2 = C "disj" [C "var" [zero], C "var" [succ zero]]

query1''' = Program evalo''' $ fresh ["fm", "st"]        (call "evalo" [V "st", V "fm", trueo])
query2''' = Program evalo''' $ fresh ["fm", "st", "res"] (call "evalo" [V "st", V "fm", V "res"])

plainQuery  = Program plainEvalo $ fresh ["fm", "st"]        (call "evalo" [V "st", V "fm", trueo])
plainQuery' = Program plainEvalo $ fresh ["fm", "st", "res"] (call "evalo" [V "st", V "fm", V "res"])

plainQueryConj = Program evaloConj (fresh ["st", "fm1", "fm2"] (call "evaloConj" $ map V ["st", "fm1", "fm2"]))

evaloConjDef :: Def
evaloConjDef =
    ( Def "evaloConj" ["st", "fm1", "fm2"]
      (
        call "evalo" [V "st", V "fm1", trueo] &&& call "evalo" [V "st", V "fm2", trueo]
      )
    )

evaloConj :: [Def]
evaloConj = evaloConjDef : plainEvalo

evalo' :: [Def]
evalo' = evalo'Def : elemo

-- no ando/oro
evalo'Def :: Def
evalo'Def =
    ( Def "evalo" ["st", "fm", "u"]
      (
        fresh ["x", "y", "z"]
        (
          (
            fm === C "var" [z] &&&
            call "elemo" [z, st, u]
          ) |||
          (
            fm === C "conj" [x, y] &&&
            (
              ( u === trueo &&&
                call "evalo" [st, x, trueo] &&&
                call "evalo" [st, y, trueo]
              ) |||
              (
                u === falso &&&
                (
                  call "evalo" [st, x, falso] |||
                  call "evalo" [st, y, falso]
                )
              )
            )
          ) |||
          (
            fm === C "disj" [x, y] &&&
            (
              ( u === trueo &&&
                (
                  call "evalo" [st, x, trueo] |||
                  call "evalo" [st, y, trueo]
                )
              ) |||
              (
                u === falso &&&
                call "evalo" [st, x, falso] &&&
                call "evalo" [st, y, falso]
              )
            )
          )
        )
      )
    )
    where [st, fm, u, x, y, z] = map V ["st", "fm", "u", "x", "y", "z"]

elemo :: [Def]
elemo = [elemoDef]

elemoDef :: Def
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

evalo'' :: [Def]
evalo'' = evalo''Def : ando ++ oro ++ noto ++ elemo

-- ando/oro last
-- uses elemo
evalo''Def :: Def
evalo''Def =
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
      [st, fm, u, x, y, z, v, w] = map V ["st", "fm", "u", "x", "y", "z", "v", "w"]

evalo''' :: [Def]
evalo''' = evalo'''Def : ando ++ oro ++ noto ++ elemo

-- ando/oro first
-- uses elemo
evalo'''Def :: Def
evalo'''Def =
    ( Def "evalo" ["st", "fm", "u"]
      (
        fresh ["x", "y", "v", "w"]
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
            call "evalo" [st, x, v] &&&
            call "noto" [v, u]
          ) |||
          (
            fm === C "var" [x] &&&
            call "elemo" [x, st, u]
          )
        )
      )
    )
    where
      [st, fm, u, x, y, v, w] = map V ["st", "fm", "u", "x", "y", "v", "w"]


-- table implementation of ando/oro
plainEvalo :: [Def]
plainEvalo =
    plainEvaloDef : ando ++ oro ++ noto ++ elemo
  where
    ando = [andoDef]
    andoDef =
        ( Def "ando" ["x", "y", "v"]
          (
            x === trueo &&& y === trueo &&& v === trueo |||
            x === falso &&& y === trueo &&& v === falso |||
            x === trueo &&& y === falso &&& v === falso |||
            x === falso &&& y === falso &&& v === falso
          )
        )
    oro = [oroDef]
    oroDef =
        ( Def "oro" ["x", "y", "v"]
          (
            x === trueo &&& y === trueo &&& v === trueo |||
            x === falso &&& y === trueo &&& v === trueo |||
            x === trueo &&& y === falso &&& v === trueo |||
            x === falso &&& y === falso &&& v === falso
          )
        )
    noto = [notoDef]
    notoDef =
      ( Def "noto" ["x", "y"]
        (
          x === trueo &&& y === falso |||
          x === falso &&& y === trueo
        )
      )
    [x, y, v] = map V ["x", "y", "v"]

plainEvaloDef :: Def
plainEvaloDef =
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

evalo :: [Def]
evalo = evaloDef : ando ++ oro ++ noto ++ assoco

-- ando/oro last
-- uses assoco
evaloDef :: Def
evaloDef =
    ( Def "evalo" ["st", "fm", "u"]
      (
        fresh ["x", "y", "v", "w", "var"]
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
            fm === C "var" [var] &&&
            call "assoco" [var, st, u]
          )
        )
      )
    )
    where
      [st, fm, u, x, y, v, w, var] = map V ["st", "fm", "u", "x", "y", "v", "w", "var"]


-- let rec evalo st fm u = ocanren (
--       fresh x, y, z, v, w in
--           (fm == conj x y & evalo st x v & evalo st y w & Std.Bool.ando v w u) |
--           (fm == disj x y & evalo st x v & evalo st y w & Std.Bool.oro  v w u) |
--           (fm == neg  x   & evalo st x v & Std.Bool.noto v u) |
--           (fm == var  z   & Std.List.assoco z st u)
--         )


