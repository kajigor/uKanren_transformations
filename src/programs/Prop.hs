module Prop where 

import Syntax
import Bool 
import List 

fm = C "conj" [C "var" [C "x" []], C "neg" [C "var" [C "x" []]]] -- always fails
fm1 = C "conj" [C "var" [C "x" []], C "neg" [C "var" [C "y" []]]]


query = evalo $ fresh ["st"] (call "evalo" [V "st", fm, trueo])
query1 = evalo $ fresh ["st"] (call "evalo" [V "st", fm, falso])
query2 = evalo $ fresh ["st"] (call "evalo" [V "st", fm1, trueo])
query3 = evalo $ fresh ["fm", "st"] (call "evalo" [V "st", V "fm", trueo])
query4 = evalo $ fresh ["x", "y", "st"] (call "evalo" [V "st",  C "conj" [V "x", C "neg" [V "y"]], trueo])


evalo g = 
  Let (def "evalo" ["st", "fm", "u"] (
    fresh ["x", "y", "z", "v", "w"] (
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
        fm === C "var" [x] &&& 
        call "assoco" [z, st, u]
      )
  ))) $ ando $ oro $ noto $ assoco $ g
    where 
      [st, fm, u, x, y, z, v, w] = map V ["st", "fm", "u", "x", "y", "z", "v", "w"]


-- let rec evalo st fm u = ocanren (
--       fresh x, y, z, v, w in 
--           (fm == conj x y & evalo st x v & evalo st y w & Std.Bool.ando v w u) | 
--           (fm == disj x y & evalo st x v & evalo st y w & Std.Bool.oro  v w u) |
--           (fm == neg  x   & evalo st x v & Std.Bool.noto v u) |
--           (fm == var  z   & Std.List.assoco z st u)
--         )


