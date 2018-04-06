module Stlc where

import Prelude hiding (abs)
import Syntax
import Bool
import Debug.Trace

var x   = C "V" [x]
abs v b = C "Abs" [v, b]
app p q = C "App" [p, q]

substo g = 
  let l  = V "l"  in 
  let x  = V "x"  in
  let a  = V "a"  in 
  let l' = V "l'" in 
  let y  = V "y"  in 
  let m  = V "m"  in 
  let n  = V "n"  in
  let m' = V "m'" in 
  let n' = V "n'" in
  let v  = V "v"  in 
  let b  = V "b"  in
  let b' = V "b'" in
  Let (def "substo" ["l", "x", "a", "l'"] 
        (
          (fresh ["y"] ((l === var y) &&& (y === x) &&& (l' === a))) |||
          (fresh ["m", "n", "m'", "n'"] 
            ( 
              (l === app m n) &&& 
              (l' === app m' n') &&&
              (call "substo" [m, x, a, m']) &&& 
              (call "substo" [n, x, a, n'])  
            )
          ) |||
          (fresh ["v", "b"] 
            (l === abs v b) &&& 
            (
              ((x === v) &&& (l' === l)) ||| 
              (fresh ["b'"] ((l' === abs v b') &&& (call "substo" [b, x, a, b'])))
            ) 
          )
        )
      ) g

evalo g = 
  let m  = V "m"  in
  let n  = V "n"  in 
  let x  = V "x"  in 
  let l  = V "l"  in
  let l' = V "l'" in
  let f  = V "f"  in
  let a  = V "a"  in 
  let f' = V "f'" in
  let a' = V "a'" in
  let p  = V "p"  in 
  let q  = V "q"  in 
  Let ( def "evalo" ["m", "n"] 
        (
          ( fresh ["x"] ((m === var x) &&& (n === m))) ||| 
          ( fresh ["x", "l"] ((m === abs x l) &&& (n === m))) |||
          ( fresh ["f", "a", "f'", "a'"]
            (
              (m === app f a) &&&
              (call "evalo" [f, f']) &&&
              (call "evalo" [a, a']) &&&
              (
                (fresh ["x", "l", "l'"] ((f' === abs x l) &&& (call "substo" [l, x, a', l']) &&& (call "evalo" [l', n]))) ||| 
                (fresh ["p", "q"] ((f' === app p q) &&& (n === app f' a'))) ||| 
                (fresh ["x"] ((f' === var x) &&& (n === app f' a')))
              ) 
            ) 
          )
        )
      ) $ substo g
