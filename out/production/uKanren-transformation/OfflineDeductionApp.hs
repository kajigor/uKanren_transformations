module OfflineDeductionApp where


import           Debug.Trace
import           System.FilePath                   ((</>))
import           Util.File                         (createDirRemoveExisting)
import           System.FilePath                   (takeBaseName)
import           BTA.AnnotatedDef
import           BTA.AnnotatedProgram
import           BTA.InvokeAnnotation
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import qualified Syntax

runWithParser :: (FilePath -> IO (Either String (AnnotatedProgram (AnnG Syntax.Term) String))) -> FilePath -> FilePath -> IO ()
runWithParser parser inputFile outDir = do
    program <- parser inputFile
    case program of 
        Left err -> 
            putStrLn err 
        Right program -> do
            print program
