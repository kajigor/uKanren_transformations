module ResidualizationTest where

import Residualization
import Syntax
import Text.Printf
import Program.List
import Driving
import DrivingTest

scoping :: (G X, [String])
scoping =
  let f g =
        let x = V "x"
            y = V "y"
            z = V "z"
            t = V "t"
        in Let (def "f" ["x", "y", "t"]
                 (fresh ["z"] ( z === x % y &&& t === z % nil))
               ) g
  in residualize (drive (f ( fresh ["x","y","t"] $ call "f" [V "x", V "y", V "t"])))

redtest :: G X
redtest   = let (r, _) = residualize tc in r

redtest' :: G X
redtest'  = let (r, _) = residualize tc'
                ((x, y, _), _, _) = tc'
            in r
redtest'' :: G X
redtest'' = let (r, _) = residualize tc'' in r
