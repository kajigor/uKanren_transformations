module DrivingTest where

import Driving
import Program.List
import Syntax
import Util.Miscellaneous

tc = drive (appendo
              (fresh ["q", "r", "s", "t", "p"]
                 (call "appendo" [V "q", V "r", V "s"] &&&
                  call "appendo" [V "s", V "t", V "p"])
              )
           )

tc'  = drive (reverso $ fresh ["q", "r"] (call "reverso" [V "q", V "r"]))
tc'' = drive (revAcco $ fresh ["q", "s"] (call "revacco" [V "q", nil, V "s"]))


tree   = snd3 tc
tree'  = snd3 tc'
tree'' = snd3 tc''
