module ResidualizeTest where

import Residualize
import Syntax
import Debug.Trace
import Text.Printf
import List
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
redtest   = let (r, _) = residualize tc in trace (printf "\n\n%s\n\n" $ show r) r

redtest' :: G X
redtest'  = let (r, _) = residualize tc'
                ((x, y, _), _, _) = tc'
            in trace (printf "\n\n%s\n\n%s\n\n%s" (show r) (show x) (show y)) r
redtest'' :: G X
redtest'' = let (r, _) = residualize tc'' in trace (printf "\n\n%s\n\n" $ show r) r
