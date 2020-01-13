module Program.Prop where 

import Syntax
import Program.Bool 
import Program.List 
import Program.Num
import Prelude hiding (succ)

fm  = C "conj" [C "var" [C "x" []], C "neg" [C "var" [C "x" []]]] -- always fails
fm1 = C "conj" [C "var" [C "x" []], C "neg" [C "var" [C "y" []]]]


query  = evalo $ fresh ["st"] (call "evalo" [V "st", fm, trueo])
query1 = evalo $ fresh ["st"] (call "evalo" [V "st", fm, falso])
query2 = evalo $ fresh ["st"] (call "evalo" [V "st", fm1, trueo])
query3 = evalo $ fresh ["fm", "st"] (call "evalo" [V "st", V "fm", trueo])
query4 = evalo $ fresh ["x", "y", "st"] (call "evalo" [V "st",  C "conj" [V "x", C "neg" [V "y"]], trueo])

query' = evalo' $ fresh ["st", "fm"] (call "evalo" [V "st", V "fm", trueo])

query'' = evalo'' $ fresh ["st", "fm"] (call "evalo" [V "st", V "fm", trueo])

fm2 = C "disj" [C "var" [zero], C "var" [succ zero]]

query1'' = evalo'' $ fresh ["fm", "st"] (call "evalo" [V "st", V "fm", trueo])
query1''' = evalo''' $ fresh ["fm", "st"] (call "evalo" [V "st", V "fm", trueo])
query2''' = evalo''' $ fresh ["fm", "st", "res"] (call "evalo" [V "st", V "fm", V "res"])

plainQuery = plainEvalo $ fresh ["fm", "st"] (call "evalo" [V "st", V "fm", trueo])
plainQuery' = plainEvalo $ fresh ["fm", "st", "res"] (call "evalo" [V "st", V "fm", V "res"])

plainQueryConj = evaloConj $ fresh ["st", "fm1", "fm2"] (call "evaloConj" $ map V ["st", "fm1", "fm2"])

evaloConj g = 
  Let 
    ( def "evaloConj" ["st", "fm1", "fm2"] 
      ( 
        call "evalo" [V "st", V "fm1", trueo] &&& call "evalo" [V "st", V "fm2", trueo]
      )
    ) $ plainEvalo g

evalo' g = 
  Let 
    ( def "evalo" ["st", "fm", "u"] 
      (
        fresh ["x", "y"]
        (   
          (
            fm === C "var" [x] &&& 
            call "elemo" [x, st, u]
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
    ) $ elemo g 
    where [st, fm, u, x, y] = map V ["st", "fm", "u", "x", "y"]
  
elemo g = 
  Let 
    ( def "elemo" ["n", "s", "v"] 
      (
        fresh ["h", "t", "n'"]
        (
          n === zero &&& s === h % t &&& v === h ||| 
          n === succ n' &&& s === h % t &&& call "elemo" [n', t, v]
        )
      )
    ) g 
    where [n, s, v, h, t, n'] = map V ["n", "s", "v", "h", "t", "n'"]   

evalo'' g = 
  Let
    ( def "evalo" ["st", "fm", "u"] 
      (
        fresh ["x", "y", "v", "w"] 
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
          -- (
          --   fm === C "neg" [x] &&& 
          --   call "evalo" [st, x, v] &&& 
          --   call "noto" [v, u] 
          -- ) ||| 
          (
            fm === C "var" [x] &&& 
            call "elemo" [x, st, u]
          )
        )
      )
    ) $ ando $ oro $ noto $ elemo g
    where 
      [st, fm, u, x, y, v, w] = map V ["st", "fm", "u", "x", "y", "v", "w"]

evalo''' g = 
  Let
    ( def "evalo" ["st", "fm", "u"] 
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
          -- (
          --   fm === C "neg" [x] &&& 
          --   call "evalo" [st, x, v] &&& 
          --   call "noto" [v, u] 
          -- ) ||| 
          (
            fm === C "var" [x] &&& 
            call "elemo" [x, st, u]
          )
        )
      )
    ) $ ando $ oro $ noto $ elemo g
    where 
      [st, fm, u, x, y, v, w] = map V ["st", "fm", "u", "x", "y", "v", "w"]
      
plainEvalo g = 
  Let
    ( def "evalo" ["st", "fm", "u"] 
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
            fm === C "var" [x] &&& 
            call "elemo" [x, st, u]
          )
        )
      )
    ) $ ando $ oro $ elemo g
    where 
      [st, fm, u, x, y, v, w] = map V ["st", "fm", "u", "x", "y", "v", "w"]
      ando g = 
        Let 
          ( def "ando" ["x", "y", "v"] 
            (
              x === trueo &&& y === trueo &&& v === trueo ||| 
              x === falso &&& y === trueo &&& v === falso ||| 
              x === trueo &&& y === falso &&& v === falso ||| 
              x === falso &&& y === falso &&& v === falso 
            )
          ) g
      oro g = 
        Let 
          ( def "oro" ["x", "y", "v"] 
            (
              x === trueo &&& y === trueo &&& v === trueo ||| 
              x === falso &&& y === trueo &&& v === trueo ||| 
              x === trueo &&& y === falso &&& v === trueo ||| 
              x === falso &&& y === falso &&& v === falso 
            )
          ) g 

evalo g = 
  Let
    ( def "evalo" ["st", "fm", "u"] 
      (
        fresh ["x", "y", "v", "w"] 
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
          -- (
          --   fm === C "neg" [x] &&& 
          --   call "evalo" [st, x, v] &&& 
          --   call "noto" [v, u] 
          -- ) ||| 
          (
            fm === C "var" [x] &&& 
            call "assoco" [x, st, u]
          )
        )
      )
    ) $ ando $ oro $ noto $ assoco $ g
    where 
      [st, fm, u, x, y, v, w] = map V ["st", "fm", "u", "x", "y", "v", "w"]


-- let rec evalo st fm u = ocanren (
--       fresh x, y, z, v, w in 
--           (fm == conj x y & evalo st x v & evalo st y w & Std.Bool.ando v w u) | 
--           (fm == disj x y & evalo st x v & evalo st y w & Std.Bool.oro  v w u) |
--           (fm == neg  x   & evalo st x v & Std.Bool.noto v u) |
--           (fm == var  z   & Std.List.assoco z st u)
--         )


