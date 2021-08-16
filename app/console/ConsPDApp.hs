module ConsPDApp where

import qualified Program.Bottles
import qualified Program.Bridge
import qualified Program.Desert
import qualified Program.L
import           Program.List           (appendo, maxLengtho, nil, revAcco,
                                         reverso)
import qualified Program.LLangType
import           Program.Path
import           Program.Programs       (doubleAppendo, rep)
import qualified Program.Prop
import qualified Program.PropEval
import qualified Program.Sort
import           Program.Stlc           (evalo)
import qualified Program.Typing
import qualified Program.Unify
import           Syntax
import           System.FilePath        (takeBaseName)
import qualified Transformer.ConsPD
import qualified Transformer.JustUnfold

-- runWithParser :: FilePath -> FilePath -> IO ()
runWithParser parser outDir inputFile = do
  program <- readFile inputFile
  case parser program of
    Left err ->
      putStrLn err
    Right program ->
      Transformer.ConsPD.runConsPD' outDir (takeBaseName inputFile) program

dA = Program doubleAppendo $ fresh ["x", "y", "z", "r"] (call "doubleAppendo" [V "x", V "y", V "z", V "r"])
revAcco' = Program revAcco $ fresh ["x", "y"] (call "revacco" [V "x", nil, V "y"])
rev = Program reverso $ fresh ["x", "y"] (call "reverso" [V "x", V "y"])
maxLen = Program maxLengtho $ fresh ["xs", "m", "l"] (call "maxLengtho" [V "xs", V "m", V "l"])
lambda = Program evalo $ fresh ["m", "n"] (call "evalo" [V "m", V "n"])

runJu = Transformer.JustUnfold.transform

runConsPD = Transformer.ConsPD.runConsPD

runRep = do
    runJu 100 "rep" rep
  where
    rep = Program Program.Programs.rep $ fresh ["n", "x"] (call "rep" [V "n", V "x"])

runProp = do
    -- runConsPD (-1) "prop"  prop
    -- runConsPD (-1) "prop1" prop1
    -- runConsPD (-1) "prop2" prop2
    -- runConsPD (-1) "prop3" prop3
    -- runConsPD (-1) "prop4" prop4
    -- runConsPD (-1) "prop_" prop'
    runConsPD (-1) "prop__" prop''
    -- runConsPD (-1) "prop1___" prop1'''
    -- runConsPD (-1) "prop2___" prop2'''
    runConsPD (-1) "propPlain" propPlain
    -- runConsPD (-1) "propPlain_" propPlain'
    -- runConsPD (-1) "prop__1" prop''1
    -- runConsPD (-1) "prop__2" prop''2
    -- runConsPD (-1) "prop__3" prop''3

  where
    -- won't terminate: accumulator in assoco
    prop       = Program.Prop.query
    -- won't terminate: accumulator in assoco
    prop1      = Program.Prop.query1
    -- won't terminate: accumulator in assoco
    prop2      = Program.Prop.query2
    prop3      = Program.Prop.query3
    prop4      = Program.Prop.query4
    prop'      = Program.Prop.query'
    prop''     = Program.Prop.query''
    prop1'''   = Program.Prop.query1'''
    prop2'''   = Program.Prop.query2'''
    propPlain  = Program.Prop.plainQuery
    propPlain' = Program.Prop.plainQuery'
    prop''1    = Program.Prop.query''1
    prop''2    = Program.Prop.query''2
    prop''3    = Program.Prop.query''3

runBottles = do
    runConsPD (-1) "bottles" Program.Bottles.query
    -- runConsPD(-1) "bottlesQuery" Program.Bottles.query'
    -- runConsPD(-1) "fancyEq" Program.Bottles.queryEq

runBridge = do
    runConsPD (-1) "bridge" Program.Bridge.query
    -- runConsPD (-1) "bridge2" Program.Bridge2.query

runDesert = do
    -- runConsPD (-1) "desert"    Program.Desert.query
    -- runConsPD (-1) "desert_"   Program.Desert.query'
    -- runConsPD (-1) "desert__"  Program.Desert.query''
    -- runConsPD (-1) "desert___" Program.Desert.query'''
    runConsPD (-1) "desert1" Program.Desert.query1

runUnify = do
    runConsPD (-1) "unify" Program.Unify.query

runPath = do
    runConsPD (-1) "path" Program.Path.query1
    runConsPD (-1) "pathlen" Program.Path.querylength

runAppendo = do
    mapM (\d -> runJu d ("app" ++ show d) app) [1..15]
    return ()
  where
    app = Program Program.List.appendo $ fresh ["x", "y", "z"] (call "appendo" [V "x", V "y", V "z"])

runSort = do
    runConsPD (-1) "sort" Program.Sort.query

runTyping = do
    runConsPD (-1) "type" Program.Typing.query

runL = do
    runConsPD (-1) "llang" Program.L.query
    runConsPD (-1) "llang1" Program.L.query1
    -- Transformer.CPD.transform "llangBranch" Program.L.query1 Nothing Branching
    -- putStrLn "llangBranch done"
    -- Transformer.CPD.transform "llangDeterm" Program.L.query1 Nothing Deterministic
    -- putStrLn "llangDeterm done"


runL' = do
    runConsPD (-1) "llangPeter" Program.LLangType.query1
    -- Transformer.CPD.transform "llangPeterBranch" Program.LLangType.query1 Nothing Branching
    -- putStrLn "llangPeterBranch done"
    -- Transformer.CPD.transform "llangPeterDeterm" Program.LLangType.query1 Nothing Deterministic
    -- putStrLn "llangPeterDeterm done"

runDoubleApp = do
    runConsPD (-1) "da" dA

runMaxlen = do
    runConsPD (-1) "maxlen" maxLen

runPropEval = do
    runConsPD (-1) "propFirstPlain" Program.PropEval.plainFirstQuery
    runConsPD (-1) "propFirstNando" Program.PropEval.nandoFirstQuery
    runConsPD (-1) "propLastPlain"  Program.PropEval.plainLastQuery
    runConsPD (-1) "propLastNando"  Program.PropEval.nandoLastQuery

run :: IO ()
run = do
  runPath
  runUnify
  runL'
  runPropEval
  runDoubleApp
  runMaxlen
  runL

main :: IO ()
main = do
    runPropEval
    -- mapM_ (Transformer.PrologToMk.transform "/home/ev/prj/geoff/cpd/examples/")
    --   [ "len.pl"
    --   ]

    -- Transformer.PrologToMk.transform "/home/ev/prj/mk-transformers-bench/experiments/propEval" "perfectLastPlain.pl"
    -- Transformer.PrologToMk.transform "/home/ev/prj/mk-transformers-bench/experiments/propEval" "etalonLastPlain.pl"
    -- Transformer.PrologToMk.transform "/home/ev/prj/mk-transformers-bench/experiments/propEval" "originalLastPlainLimited.pl"
    -- Transformer.PrologToMk.transform "/home/ev/prj/mk-transformers-bench/experiments/propEval" "etalonLastPlainLimited.pl"


    -- mapM_ (Transformer.PrologToMk.transform "/home/ev/prj/kajigor/cpd/examples")
        --   [ "geoff_doubleApp.pl"
        --   , "geoff_originalFirstNando.pl"
        --   , "geoff_originalLastNando.pl"
        --   , "geoff_path.pl"
        --   , "geoff_maxLengtho.pl"
        --   , "geoff_originalFirstPlain.pl"
        --   , "geoff_originalLastPlain.pl"
        --   , "geoff_unify.pl"
        --   ]


    -- Transformer.CPD.transform "branchPlainFirst" Program.PropEval.plainFirstQuery Nothing Branching
    -- Transformer.CPD.transform "cpdPlainFirst" Program.PropEval.plainFirstQuery Nothing Deterministic
    -- Transformer.CPD.transform "branchNandoLast" Program.PropEval.nandoLastQuery Nothing Branching
    -- Transformer.CPD.transform "cpdNandoLast" Program.PropEval.nandoLastQuery Nothing Deterministic

    -- Transformer.CPD.transform "branchUnify" Program.Unify.query Nothing Branching
    -- Transformer.CPD.transform "cpdUnify" Program.Unify.query Nothing Deterministic

    -- Transformer.CPD.transform "cpdPath" Program.Path.query1 Nothing Deterministic
    -- traceM "\n==============================\nstarted transforming path:  branching\n\n"
    -- Transformer.CPD.transform "branchPath" Program.Path.query1 Nothing Branching

    -- runPath
    -- runUnify

    -- -- runL'

    -- -- -- runPropEval
    -- -- -- runDoubleApp
    -- -- -- runMaxlen
    -- -- -- runL

    -- -- Transformer.PrologToMk.transform  "test/out/nc/da/ecce.pl"
    -- -- Transformer.PrologToMk.transform  "test/out/nc/maxlen/ecce.pl"
    -- -- Transformer.PrologToMk.transform  "test/out/nc/llang1/ecce.pl"
    -- -- Transformer.PrologToMk.transform  "test/out/nc/llangPeter/ecce.pl"
    -- -- Transformer.PrologToMk.transform  "test/out/nc/path/ecce.pl"
    -- -- Transformer.PrologToMk.transform  "test/out/nc/unify/ecce.pl"
    -- -- Transformer.PrologToMk.transform  "test/out/nc/propFirstPlain/ecce.pl"
    -- -- Transformer.PrologToMk.transform  "test/out/nc/propLastPlain/ecce.pl"
    -- -- Transformer.PrologToMk.transform  "test/out/nc/propFirstNando/ecce.pl"
    -- -- Transformer.PrologToMk.transform  "test/out/nc/propLastNando/ecce.pl"
