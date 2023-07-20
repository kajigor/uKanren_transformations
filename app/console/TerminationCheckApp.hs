module TerminationCheckApp where
import qualified Parser.AnnotatedParser as AnnotatedParser
import AnnotatedProgram
import InvokeAnnotation
import AnnotatedDef
import qualified Syntax
import BTA.SizeConversion (convert)
import Debug.Trace
import BTA.NormalizeAnnotated


runWithParser :: (FilePath -> IO (Either String (AnnotatedProgram Syntax.G String))) -> FilePath -> FilePath -> IO ()
runWithParser parser inputFile outputFile = do
    program <- parser inputFile
    case program of 
        Left err -> 
            putStrLn err 
        Right originalPr -> 
            let normProgram = makeNormal $ convert $ annotateInvokesPr originalPr in 
            putStrLn $ show $ normProgram
    -- case program of
    --     Left err -> 
    --         putStrLn err
    --     Right p -> 
    --         putStrLn $ show $ convert $ annotateInvokesPr (trace (show p) p)