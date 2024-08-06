module Transformer.OfflinePD where

import           Control.Monad.State
import           Syntax (Term, S, X)
import           BTA.AnnotatedProgram
import           BTA.AnnotatedDef
import           BTA.InvokeAnnotation
import qualified BTA.AnnotatedDefs as AnnDefs
import qualified OfflinePD.LocalControl as LC
import qualified OfflinePD.GlobalControl as GC
import qualified OfflinePD.AnnEnvironment as Env
import qualified OfflinePD.AnnotatedEval as E
import qualified Subst
import           Printer.AnnSldTree
import           Printer.Dot
import qualified GHC.IO.Exception
import           System.FilePath        ((<.>), (</>))
import           System.Process         (system)
import           Text.Printf
import           Util.File              (createDirRemoveExisting, shortenFileName)
import           Debug.Trace
import           Residualization
import           CPD.Residualization
import           Purification (purification)
import           Program
import           Syntax
import           Def
import           Descend
import           Printer.GlobalTree
import           Printer.PrettyMkPrinter
import qualified Data.Map as Map
import qualified CPD.GlobalControl as GCcpd
import qualified CPD.LocalControl as LCcpd
import qualified Environment as Envcpd
import qualified Definitions as Defs
import qualified Transformer.MkToProlog
import qualified OCanrenize as OC
import           Data.List           (find, partition)
import           Util.Miscellaneous
import           Util.System (graphsToPdf)


data TransformResult = Result { original   :: [AnnotatedDef (AnnG Term) X]
                              , globalTree :: GC.GlobalTree
                              , localTrees :: [([AnnG Term S], LC.SldTree)]
                              , beforePur  :: Program G X
                              , purified   :: (G X, [X], [Def G X])
                              }

transnformDef :: AnnotatedDef (AnnG Term) t -> Def G t
transnformDef (AnnotatedDef name args body anns) = (Def name args (convertToSimple  body))

transformEnv :: Env.Env -> Envcpd.Env
transformEnv (Env.Env defs i fn) = Envcpd.Env (Defs.Definitions $ Map.map transnformDef $ AnnDefs.getDefinitions defs) i fn

transformSld :: LC.SldTree -> LCcpd.SldTree
transformSld LC.Fail = LCcpd.Fail
transformSld (LC.Success subst) = LCcpd.Success subst
transformSld (LC.Or trees g subst) = LCcpd.Or (map transformSld trees) (fmap convertToSimple g) subst
transformSld (LC.Conj sldTree descs subst) = LCcpd.Conj (transformSld sldTree) (map transformDesc1 descs) subst
transformSld (LC.Leaf descs subst env) = LCcpd.Leaf (map transformDesc1 descs) subst (transformEnv env)

transformDesc1 :: Descend (AnnG Term S) -> Descend (G S)
transformDesc1 (Descend curr ancs) = Descend (convertToSimple curr) (map convertToSimple ancs)

transformDesc :: Descend [AnnG Term S] -> Descend [G S]
transformDesc (Descend curr ancs) = Descend (map convertToSimple curr) (map (map convertToSimple) ancs)

transformGlobal :: GC.GlobalTree -> GCcpd.GlobalTree
transformGlobal (GC.Leaf desc generalizer subst) = GCcpd.Leaf (transformDesc desc) generalizer subst
transformGlobal (GC.Node desc generalizer sld trees) = GCcpd.Node (transformDesc desc) generalizer (transformSld sld) (map transformGlobal trees)
transformGlobal (GC.Prune desc subst) = GCcpd.Prune (transformDesc desc) subst


runTransformation :: AnnotatedProgram (AnnG Term) X -> LCcpd.Heuristic -> TransformResult
runTransformation goal@(AnnotatedProgram original _) heu =
  let (globalTree, logicGoal, names) = GC.topLevel goal heu in
  let localTrees = GC.getNodes globalTree in
  let beforePur = residualizationTopLevel $ transformGlobal globalTree in
  let purified = purification (beforePur, vident <$> reverse names) in
  Result original globalTree localTrees beforePur purified

renderLocalTree :: FilePath -> [AnnG Term S] -> LC.SldTree -> IO ()
renderLocalTree localDir goal =
    printTree (localDir </> shortenFileName (show goal) <.> "dot")

transform' :: FilePath -> FilePath -> AnnotatedProgram (AnnG Term) X -> Maybe String -> LCcpd.Heuristic -> IO (Program G X)
transform' outDir filename program@(AnnotatedProgram defs goal) env heu = do
    let path = outDir </> filename
    let localDir = path </> "local"
    let cpdFile = path </> "cpd"
    mapM_ createDirRemoveExisting [path, localDir]

    let result = runTransformation program heu
--
--    let env = Env.fromDefs defs
--    let ((logicGoal, names), env') = runState (E.preEval goal) env
--    let logicGoals = LC.conjToList logicGoal
--    printf $ show logicGoals
--    let nodes = [logicGoals]
--    let sldTree = LC.sldResolution logicGoals env' Subst.empty []
----    printf $ show nodes
--    printTree (path </> "local.dot") sldTree
--    let (substs, bodies) = partition (null . snd3) $ LC.resultants sldTree
----
--    printf $ show bodies

--    Transformer.MkToProlog.transform (path </> "original.pl") definitions
--
--    let (globalTree, logicGoal, names) = GC.topLevel program
--    let localTrees = GC.getNodes globalTree
--    printTree (path </> "global.dot") (transformGlobal globalTree)
--    mapM_ (uncurry (renderLocalTree localDir)) localTrees


    printTree (path </> "global.dot") (transformGlobal $ globalTree result)
    mapM_ (uncurry (renderLocalTree localDir)) (localTrees result)
    writeFile (cpdFile <.> "before.pur") (prettyMk $ beforePur result)
    let pur@(goal,xs,defs) = purified result
    Transformer.MkToProlog.transform (cpdFile <.> "pl") defs
    let purified = Program defs goal
    writeFile (cpdFile <.> "pur") (prettyMk purified)
    let ocamlCodeFileName = cpdFile <.> "ml"
    OC.topLevel ocamlCodeFileName "topLevel" env pur

    mapM_ graphsToPdf [path, localDir]
    return purified

transform :: FilePath -> AnnotatedProgram (AnnG Term) X -> Maybe String -> LCcpd.Heuristic -> IO ()
transform filePath pr str heu = do  
  res <- transform' "test/out/cpd" filePath pr str heu
  return ()