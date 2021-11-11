module Program (stdlibPrograms) where

import Program.Bool
import Program.List
import Program.Num
import Syntax (success, Program (..))


stdlibPrograms =
  map (\defs -> Program defs success)
      [ nando, noto, oro, ando
      , listo, membero, inBotho, nilo, singletono, lengtho, maxo, mino, appendo, reverso, revAcco, assoco, nthOpt
      , notZero, addo, mulo, leo, gto, geo, lto
      ]
