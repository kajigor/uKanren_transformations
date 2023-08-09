module AnnotationsSettingApp where


import           Debug.Trace
import           System.FilePath                   ((</>))
import           Util.File                         (createDirRemoveExisting)
import           System.FilePath                   (takeBaseName)
import           BTA.AnnotatedDef
import           BTA.AnnotatedProgram
import           BTA.AnnotationsSetting            (setAnnotations)
import           BTA.DotAbstract                   (printTree)
import           BTA.Graph
import           BTA.Inequalities                  (go)
import           BTA.InvokeAnnotation
import           BTA.NormalizeAnnotated            (makeNormal)
import           BTA.TerminationCheck              (goGraphMap, getPairsDef)
import           BTA.SizeConversion                (convert, AbstractTerm)
import qualified Data.Map                          as Map
import qualified Data.Set                          as Set
import qualified Syntax

runWithParser :: (FilePath -> IO (Either String (AnnotatedProgram Syntax.G String))) -> FilePath -> FilePath -> IO ()
runWithParser parser inputFile outDir = do
    program <- parser inputFile
    case program of 
        Left err -> 
            putStrLn err 
        Right originalPr -> do
            let annotatedProgram = setAnnotations $ annotateInvokesPr originalPr
            let dir = outDir </> (takeBaseName inputFile)
            showGraphs annotatedProgram $ dir </> "conds"
            writeFile (dir </> "ans.mk") $ show annotatedProgram


showGraphs :: AnnotatedProgram (AnnG Syntax.Term) String -> FilePath -> IO ()
showGraphs annotatedProgram dir = do 
    let defs = getDefs $ makeNormal $ convert annotatedProgram 
    let mapDefs = Map.fromList $ zip (map getName defs) defs
    let mapConditions = go defs mapDefs Map.empty 
    let (defsGraphs, defsVars) = unzip $ map (getPairsDef mapConditions mapDefs) defs
    let defsVarsRes = Map.unions $ traceShow defsGraphs defsVars 
    let defsGraphsRes = goGraphMap (Map.unions defsGraphs) defsVarsRes $ map getName defs
    let graphsPairs = zip [0..] $ map (normalView mapDefs defsVarsRes) $ concatMap (\((a, b), graphs) -> zip (repeat (a, b)) $ Set.toList graphs) $ Map.assocs defsGraphsRes
    createDirRemoveExisting dir
    mapM_ (\(i, ((from, to), graph)) -> printTree (dir </> from </> to) (show i ++ ".dot") graph) graphsPairs


normalView :: Map.Map String (AnnotatedDef (AnnG AbstractTerm) String) -> Map.Map (String, String) ([String], [String]) -> ((String, String), Graph String) -> ((String, String), Graph String) 
normalView mapDefs defsVars ((a, b), graph) = 
    let (inVars, outVars) = defsVars Map.! (a, b) in 
    let inVarsReal = map ("from_" ++ ) $ getArgs $ mapDefs Map.! a in 
    let outVarsReal = map ("to_" ++ ) $ getArgs $ mapDefs Map.! b in 
    let mapVars = Map.fromList $ zip (inVars ++ outVars) (inVarsReal ++ outVarsReal) in 
    ((a, b), graphfmap (mapVars Map.!) graph)