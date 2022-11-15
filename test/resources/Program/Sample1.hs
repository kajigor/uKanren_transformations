module Program.Sample1 where

import Syntax
import Program
import Def

query = Program fAndS $ fresh ["x"] (call "fAndS" [x])

x = V "x"

fAndS :: [Def G X]
fAndS = fAndSDef : f ++ s

fAndSDef :: Def G X
fAndSDef =
    (Def "fAndS" ["x"]
      (call "f" [x] &&& call "s" [x])
    )

f :: [Def G X]
f = [fDef]

fDef :: Def G X
fDef =
    (Def "f" ["x"]
        (x === C "5" [] ||| call "f" [x])
    )

s :: [Def G X]
s = [sDef]

sDef :: Def G X
sDef =
    (Def "s" ["x"]
        (x === C "6" [] ||| call "s" [x])
    )
