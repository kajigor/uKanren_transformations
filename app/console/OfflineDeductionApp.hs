module OfflineDeductionApp where


import           BTA.AnnotatedProgram
import           BTA.InvokeAnnotation
import qualified Transformer.OfflinePD
import qualified Syntax
import qualified CPD.LocalControl as LC

runWithParser :: (FilePath -> IO (Either String (AnnotatedProgram (AnnG Syntax.Term) String))) -> FilePath -> FilePath -> LC.Heuristic -> IO ()
runWithParser parser inputFile outDir heu = do
    program <- parser inputFile
    case program of 
        Left err -> 
            putStrLn err 
        Right program -> do
            res <- Transformer.OfflinePD.transform' outDir inputFile program Nothing heu 
            return ()
