module AllTransformationsApp where 

import Program 
import Util.File (listDirectoriesRecursive, prologExt)
import qualified Transformer.PD as PD
import qualified Transformer.CPD as CPD
import qualified Transformer.ConsPD as ConsPD
import qualified Transformer.ConsPDGlobal as ConsPDGlobal
import qualified Transformer.OfflinePD as OfflinePD
import System.FilePath (takeBaseName, takeFileName, (</>), (<.>))
import CPD.LocalControl (Heuristic (..))
import qualified ConsPD.LocalControl as ConsLC
import Debug.Trace
import System.IO
import System.Timeout
import Text.Printf 

runAll mkParser annParser inputDir heu = do
    dirs <- listDirectoriesRecursive "out" inputDir 
    if null dirs 
    then run inputDir 
    else mapM_ run dirs 
  where 
    run dir = do 
      putStrLn $ printf "Working on %s..." dir
      let lastDir = takeFileName dir  
      let inputFile = dir </> lastDir <.> "mk" 
      let annInputFile = dir </> lastDir <.> "ann" <.> "mk" 
      let outputDir = dir </> "out" 
      runWithParser mkParser annParser inputFile annInputFile outputDir heu
      putStr " Done."
runWithParser mkParser annParser inputFile annInputFile outputDir heu = do 
    res <- mkParser inputFile 
    case res of 
      Left err -> 
        putStrLn err 
      Right program@(Program defs goal) -> do 
        -- pd program
        -- cpd program Branching
        -- cpd program Deterministic
        conspdGlobal program ConsLC.Branching
        -- conspdGlobal program ConsLC.Deterministic
        -- conspd program
        -- ann <- annParser annInputFile 
        -- case ann of 
        --   Left err -> putStrLn err 
        --   Right program -> offline program 
  where 
    pd program = do 
      informed "Partial deduction" $ PD.transform inputFile outputDir program 
    cpd program heu = do 
      informed (printf "Conjunctive partial deduction, %s" $ show heu) $ CPD.transform' outputDir inputFile program Nothing heu 
    conspdGlobal program heu = do
      informed (printf "Conjunctive partial deduction, Global, %s" $ show heu) $ ConsPDGlobal.transform' outputDir inputFile program Nothing heu
    conspd program = do
      informed "Conservative partial deduction" $ ConsPD.runConsPD' outputDir Nothing inputFile program 
    offline program = 
      informed "Offline partial deduction" $ OfflinePD.transform' outputDir annInputFile program Nothing heu
    informed :: String -> IO a -> IO () 
    informed msg f = do 
      putStr $ printf "  %s... " msg
      hFlush stdout 
      let time = 10000000
      let timed = timeout time
      result <- timed f 
      case result of 
        Just _ -> putStrLn "Done"
        Nothing -> putStrLn $ printf "Timeout, %ds" (time `div` 1000000)



