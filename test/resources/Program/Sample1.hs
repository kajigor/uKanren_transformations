module Program.Sample1 where

import Syntax

query = Program fAndS $ fresh ["x"] (call "fAndS" [x])

x = V "x"

fAndS :: [Def]
fAndS = fAndSDef : f ++ s

fAndSDef :: Def
fAndSDef =
    (Def "fAndS" ["x"]
      (call "f" [x] &&& call "s" [x])
    )

f :: [Def]
f = [fDef]

fDef :: Def
fDef =
    (Def "f" ["x"]
        (x === C "5" [] ||| call "f" [x])
    )

s :: [Def]
s = [sDef]

sDef :: Def
sDef =
    (Def "s" ["x"]
        (x === C "6" [] ||| call "s" [x])
    )
