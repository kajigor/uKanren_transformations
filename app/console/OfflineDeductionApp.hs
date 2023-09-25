module OfflineDeductionApp where


import           BTA.AnnotatedProgram
import           BTA.InvokeAnnotation
import qualified Transformer.OfflinePD
import qualified Syntax

runWithParser :: (FilePath -> IO (Either String (AnnotatedProgram (AnnG Syntax.Term) String))) -> FilePath -> FilePath -> IO ()
runWithParser parser inputFile outDir = do
    program <- parser inputFile
    case program of 
        Left err -> 
            putStrLn err 
        Right program -> do
            Transformer.OfflinePD.transform' outDir inputFile program Nothing 
