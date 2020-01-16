module Program.Sample1 where

import Syntax

query = fAndS $  fresh ["x"] (call "fAndS" [x])

x = V "x"

fAndS :: G a -> G a
fAndS g =
  Let (Def "fAndS" ["x"]
        (call "f" [x] &&& call "s" [x])
      ) $ f $ s g



f :: G a -> G a
f g =
  Let (Def "f" ["x"]
          (x === C "5" [] ||| call "f" [x])
      ) g

s :: G a -> G a
s g =
  Let (Def "s" ["x"]
          (x === C "6" [] ||| call "s" [x])
      ) g
