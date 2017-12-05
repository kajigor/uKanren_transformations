module Num where

import Prelude hiding (succ)
import Syntax
import Debug.Trace

zero   = C "O" []
succ x = C "S" [x]

addo g =
  let x  = V "x" in 
  let y  = V "y" in 
  let z  = V "z" in 
  let x' = V "x'" in 
  let z' = V "z'" in 
  Let
    ( def "addo" ["x", "y", "z"] 
        (
          (x === zero &&& z === y) |||
          (
            Zzz (fresh ["x'", "z'"] 
            (
              x === succ x' &&&
              z === succ z' &&&
              call "addo" [x', y, z']
            ))
          ) 
        )
    ) g

num (V n) = "._" ++ show n
num (C "O" []) = "O"
num (C "S" [x]) = "S(" ++ num x ++ ")"
num _ = "??"

