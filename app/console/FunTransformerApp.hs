{-# LANGUAGE TupleSections #-}

module FunTransformerApp where

import qualified Program                   as Pr
import           Syntax
import           Data.List
import qualified Data.List.NonEmpty        as Ne
import qualified Data.Set                  as Set
import qualified BTA.AnnotatedProgram      as AnnPr
import           BTA.AnnotationType
import           BTA.AnnotatedDef
import qualified BTA.InvokeAnnotation      as INV
import qualified BTA.NormalizeAnnotated    as NA
import qualified Def
import           System.Directory          (copyFile)
import           System.FilePath           (takeBaseName, (<.>), (</>))
import           Util.File                 (createDirRemoveExisting)
import           Util.String
import           CPD.LocalControl          (Heuristic)
import qualified Transformer.CPD           as CPD    
import qualified Transformer.OfflinePD     as Offline
import           Language.Haskell.TH       (pprint)
import           BTA.AnnotationsSetting    (setAnnotations)
import           Printer.PrettyMkPrinter   (prettyMk)
import qualified BTA.BTA                   as BTA
import qualified Subst
import FunConversion.Trans (transMultiMode)
import FunConversion.Syntax (embedProgSafe, types)
import FunConversion.OCamlPretty (prettyString)
import Transformer.ConsPD (haskellPreamble)
import Debug.Trace


import Mode.Toplevel
import qualified Mode.Term as MT
import qualified Mode.NormSyntax as N
import qualified Mode.Syntax as MS 
import qualified Mode.Pretty as P
import qualified FunConversion.DetMode as D
import Mode.Inst

data Deduction =
  Offline | 
  Online 
  deriving (Show, Read, Eq)

transformGoal :: MS.Goal (S, Mode) -> INV.AnnG Term S
transformGoal (MS.Disj f s l) =  INV.Disjunction (transformGoal f) (transformGoal s) (map transformGoal l)
transformGoal (MS.Conj f s l) =  INV.Conjunction (transformGoal f) (transformGoal s) (map transformGoal l)
transformGoal (MS.Unif var t) = 
  (MT.varToPlainTerm $ fmap fst var) INV.:=: (MT.toPlainTerm $ fmap fst t) 
transformGoal (MS.Call name args) = 
  INV.Invoke (transformName name (map MT.getVar args)) (map (MT.varToPlainTerm . fmap fst) args) INV.Memo
  
transformName name args = name ++ map (getShow . before. snd) args

getShow Ground = 's'
getShow Free = 'd'

-- convV (MT.Var x) = (V $ fst x)

-- convT :: MT.FlatTerm S -> Term S
-- convT (MT.FTVar (MT.Var x)) = V x 
-- convT (MT.FTCon f l) = C f (map (\(MT.Var x) -> V x) l)

normalize :: [S] -> INV.AnnG Term S  -> INV.AnnG Term S
normalize args (INV.Disjunction f s l) = 
  INV.Disjunction (normalize' args f) (normalize' args s) (map (normalize' args) l)
normalize args g = normalize' args g

normalize' :: [S] -> INV.AnnG Term S  -> INV.AnnG Term S
normalize' args g =
  let vars = fv g in
  NA.fresh (vars \\ args) g

transformDef :: Def.Def MS.Goal (S, Mode) -> AnnotatedDef (INV.AnnG Term) S 
transformDef (Def.Def name argsAnn body) = 
  let (args, anns) = unzip $ map (\(arg, mode) -> (arg, getMode $ before mode)) argsAnn in 
  -- let vars = map fst $ Set.toList $ MS.allVars body in 
  AnnotatedDef (transformName name argsAnn) args (normalize args $ transformGoal body) anns --NA.fresh (vars \\ args) $ 
  where 
    getMode Ground = Static
    getMode Free = Dynamic

transformAnn :: Pr.Program MS.Goal (S, Mode) -> AnnPr.AnnotatedProgram (INV.AnnG Term) X
transformAnn pr@(Pr.Program defs goal) = 
  fmap (\s -> "v" ++ (show s)) $ AnnPr.AnnotatedProgram (map transformDef defs) (transformGoal goal)


transformWith :: INV.AnnG Term String -> INV.AnnG Term String ->  INV.AnnG Term String 
transformWith (INV.Conjunction g1 g2 gl) (INV.Conjunction h1 h2 hl) = 
  INV.Conjunction (transformWith g1 h1) (transformWith g2 h2) (map (uncurry transformWith) $ zip gl hl)
transformWith (INV.Disjunction g1 g2 gl) (INV.Disjunction h1 h2 hl) = 
  INV.Disjunction (transformWith g1 h1) (transformWith g2 h2) (map (uncurry transformWith) $ zip gl hl)
transformWith (INV.Fresh x body) g =
  INV.Fresh x $ transformWith body g
transformWith (INV.Delay body) (INV.Delay body1) =
  INV.Delay $ transformWith body body1
transformWith (INV.Invoke name terms ann) (INV.Invoke name1 terms1 ann1) =
  INV.Invoke name1 terms ann
transformWith (a INV.:=: b) _ = 
  a INV.:=: b
transformWith a b = a
  
deductor :: FilePath -> Deduction -> FilePath -> String -> Pr.Program G String -> Heuristic -> String -> [Int] -> G String -> IO (Either String (Pr.Program G String))
deductor outFile deduction outDir baseName program heuristic relName inputs pgoal | deduction == Offline = do
  case topLevelWithDefaultCall program relName inputs of
    Right program -> do
      let resMode = transformAnn (N.back program)
      putStrLn $ show resMode
      putStrLn $ show inputs
      let resModeAnalysis = AnnPr.AnnotatedProgram (AnnPr.getDefs resMode) $ transformWith (INV.annotateInvokes INV.Memo pgoal) (AnnPr.getGoal resMode)
      putStrLn "-------"
      putStrLn $ show resModeAnalysis
      writeFile (outFile <.> "mode") $ show resModeAnalysis
      let improvedModes = BTA.improveModes resModeAnalysis
      writeFile (outFile <.> "imp.mode") $ show improvedModes
      putStrLn "-------"
      putStrLn $ show improvedModes
      let annotatedProgram = setAnnotations resModeAnalysis -- improvedModes
      print annotatedProgram
      writeFile (outFile <.> "ann") $ show annotatedProgram
      res <- Offline.transform' (outDir </> "dec") baseName annotatedProgram Nothing heuristic
      writeFile (outFile <.> "deduced") $ prettyMk res    
      return $ Right res
    Left err -> do
      return $ Left $ "Mode analysis failed:\n" ++ show err
deductor outFile deduction outDir baseName program heuristic _ _ _ | deduction == Online = do
  res <- CPD.transform' outDir baseName program Nothing heuristic
  writeFile (outFile <.> "deduced") $ prettyMk res   
  return $ Right res


runWithParser :: (FilePath -> IO (Either String (Pr.Program G String))) -> FilePath -> FilePath -> Heuristic -> Deduction -> String -> [Int] -> IO ()
runWithParser parser inputFile outDir heuristic deduction relName inputs = do
  let baseName = takeBaseName inputFile
  let uBaseName = toUpper baseName
  createDirRemoveExisting $ outDir </> baseName
  let outFile = outDir </> baseName </> baseName
  let uOutFile = outDir </> baseName </> uBaseName
  let haskellFile = uOutFile <.> "hs"
  let ocamlFile = uOutFile <.> "ml"
  
  print $ "Chosen heuristic: " ++ show heuristic
  print $ "Chosen deduction: " ++ show deduction
  
  parsed <- parser inputFile

  case parsed of
    Left err ->
      putStrLn err
    Right program@(Pr.Program pdefs pgoal) -> do 
      deduced <- deductor outFile deduction outDir baseName program heuristic relName inputs pgoal 
        -- case deduction of 
        --   Offline -> do
        --     case topLevelWithDefaultCall program relName inputs of
        --       Right program -> do
        --         putStrLn $ (show . transformAnn) (N.back program)
        --         let annotatedProgram = AnnPr.AnnotatedProgram (AnnPr.getDefs $ transformAnn (N.back program)) (INV.annotateInvokes INV.Memo pgoal)
        --         print annotatedProgram
        --         res <- Offline.transform' outDir baseName annotatedProgram Nothing heuristic
        --         writeFile (outFile <.> "deduced") $ prettyMk res    
        --         writeFile (outFile <.> "ann") $ show annotatedProgram
        --         return $ Just res
        --       Left err -> do
        --         putStrLn $ "Mode analysis failed:\n" ++ show err
        --         return Nothing
        --   Online -> do 
        --     res <- CPD.transform' outDir baseName program Nothing heuristic
        --     writeFile (outFile <.> "deduced") $ prettyMk res   
        --     return $ Just res 
      
      case deduced of 
        Right deduced@(Pr.Program defs goal) -> do
          relName <- getName goal 
          
          print deduced
            
          let def = find (\def -> Def.getName def == relName) defs
          case def of
            Just def -> do
              let inputs = subsequences [0 .. length (Def.getArgs def) - 1]
              let translatedProgram = transMultiMode defs (map (relName,) inputs)
              outputProgram relName ocamlFile haskellFile uBaseName translatedProgram
            Nothing -> putStrLn $ relName ++ " undefined"
                      
          copyFile inputFile (outFile <.> "mk")
        Left error -> 
          putStrLn $ show error
    where
      outputProgram relName ocamlFile haskellFile uBaseName program = do
        case program of
          Left err -> putStrLn $ "Error in translating: " ++ err
          Right hs -> do
            let ocamlPr = prettyString hs
            writeFile ocamlFile ocamlPr
  
            let pr = embedProgSafe relName hs
            case pr of
              Left err -> putStrLn $ "Template Haskell error: " ++ err
              Right hs ->
                writeFile haskellFile (haskellPreamble uBaseName ++ pprint hs)
      getName (Invoke name _) = do 
        return name
      getName (Fresh x g) = do 
        getName g 
      
      
      
convertToSimplePr :: AnnPr.AnnotatedProgram G X -> Pr.Program G X 
convertToSimplePr pr@(AnnPr.AnnotatedProgram defs goal) = 
  Pr.Program (map transnformOneDef defs) goal
            
transnformOneDef :: AnnotatedDef G X -> Def.Def G X 
transnformOneDef (AnnotatedDef name args body _) = Def.Def name args body
         