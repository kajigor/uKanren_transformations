module AllTransformationsApp where 

import Program 
import Util.File (prologExt)
import qualified Transformer.PD as PD
import qualified Transformer.CPD as CPD
import System.FilePath (takeBaseName, (</>))
import CPD.LocalControl (Heuristic (..))
import qualified Transformer.ConsPD as ConsPD 

runWithParser mKparser inputFile outputDir = do 
    res <- mKparser inputFile 
    case res of 
      Left err -> 
        putStrLn err 
      Right program@(Program defs goal) -> do 
        pd program 
        cpd program Branching
        cpd program Deterministic
        conspd program
        offline program 
  where 
    pd = PD.transform inputFile outputDir
    cpd program = CPD.transform' outputDir inputFile program Nothing 
    conspd = ConsPD.runConsPD' outputDir Nothing inputFile 
    offline program = putStrLn "Offline Partial Deduction transformation not implemented\n" 


