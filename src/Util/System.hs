module Util.System where 

import Text.Printf (printf)
import System.Process (system)
import System.FilePath ((</>))
import qualified GHC.IO.Exception

graphsToPdf :: FilePath -> IO GHC.IO.Exception.ExitCode
graphsToPdf dir =
  system (printf "dot -O -Tpdf %s" dir </> "*.dot")
