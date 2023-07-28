module TerminationCheckApp where
import qualified Parser.AnnotatedParser as AnnotatedParser
import AnnotatedProgram
import InvokeAnnotation
import AnnotatedDef
import qualified Syntax
import BTA.SizeConversion (convert)
import Debug.Trace
import BTA.AnnotationsSetting (setAnnotations)


runWithParser :: (FilePath -> IO (Either String (AnnotatedProgram Syntax.G String))) -> FilePath -> FilePath -> IO ()
runWithParser parser inputFile outputFile = do
    program <- parser inputFile
    case program of 
        Left err -> 
            putStrLn err 
        Right originalPr -> do
            let annotatedProgram = setAnnotations $ annotateInvokesPr originalPr
            writeFile outputFile $ show annotatedProgram