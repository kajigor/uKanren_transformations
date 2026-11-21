{-# LANGUAGE ScopedTypeVariables #-}
-- | Exporter for the specialization visualizer (@viz/@).
--
--   viz-export [--out DIR] [--answers N] [--fuel F] [--only a,b] [--spec a,b,..] [--vanilla] BENCHDIR..
--
--   For every benchmark it writes @DIR/<name>.json@ (format: @viz/SCHEMA.md@):
--   the source, the global and local trees of each base specializer, the
--   residual programs, and profiled runs of the original and every residual
--   program on the benchmark's run-time queries.  @DIR/index.json@ lists the
--   benchmarks.
module Main where

import           Control.Exception    (SomeException, evaluate, try)
import           Control.Monad        (forM, forM_)
import           Control.Monad.State
import           Data.Char            (ord)
import           Data.IORef
import           Data.List            (intercalate, isPrefixOf, sort)
import           Data.List.Split      (splitOn)
import qualified Data.Map.Strict      as M
import           Data.Time.Clock      (addUTCTime, diffUTCTime, getCurrentTime)
import           Data.Time.Format     (defaultTimeLocale, formatTime)
import           Def                  (Def (..))
import           Descend              (getCurr)
import           Measure.Interp
import           Measure.Runner
import           Measure.Vanilla      (vanillaBench)
import           Program
import           Syntax
import           System.Directory     (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, listDirectory)
import qualified System.Environment
import           System.FilePath      (takeDirectory, takeFileName, (<.>), (</>))
import           System.IO
import           System.Timeout       (timeout)
import           Text.Printf          (printf)

import qualified CPD.GlobalControl    as CGC
import qualified CPD.LocalControl     as CLC
import qualified ConsPD.GlobalControl as SGC
import qualified ConsPD.LocalControl  as SLC

------------------------------------------------------------------------------
-- Minimal JSON

data J = JS String | JN Double | JI Int | JB Bool | JNull | JA [J] | JO [(String, J)]

render :: J -> String
render j = go j ""
  where
    go (JS s) = str s
    go (JN d) = showString (printf "%.3f" d)
    go (JI i) = shows i
    go (JB b) = showString (if b then "true" else "false")
    go JNull  = showString "null"
    go (JA xs) = showChar '[' . commas (map go xs) . showChar ']'
    go (JO kvs) = showChar '{' . commas [ str k . showChar ':' . go v | (k, v) <- kvs ] . showChar '}'
    commas [] = id
    commas (x : xs) = x . foldr (\y r -> showChar ',' . y . r) id xs
    str s = showChar '"' . foldr (\c r -> esc c . r) id s . showChar '"'
    esc '"'  = showString "\\\""
    esc '\\' = showString "\\\\"
    esc '\n' = showString "\\n"
    esc '\t' = showString "\\t"
    esc c | ord c < 0x20 = showString (printf "\\u%04x" (ord c))
          | otherwise    = showChar c

------------------------------------------------------------------------------
-- Pretty strings

clean :: String -> String
clean = replace "&rarr;" "\8614" . replace "&lt;" "<" . replace "&gt;" ">" . replace "&amp;" "&"
  where
    replace old new s = intercalate new (splitOn old s)

cut :: Int -> String -> String
cut n s = let s' = clean s in if length s' > n then take n s' ++ "\8230" else s'

goals :: Show g => [g] -> String
goals = cut 400 . intercalate " & " . map show

------------------------------------------------------------------------------
-- Trees (node budget per tree)

maxNodes :: Int
maxNodes = 1500

type Budget = State Int

tick :: Budget Bool
tick = do
  k <- get
  if k >= maxNodes then return False else put (k + 1) >> return True

truncatedNode :: J
truncatedNode = JO [("kind", JS "truncated"), ("goal", JS ""), ("subst", JS ""), ("children", JA [])]

sNode :: String -> String -> String -> [J] -> J
sNode k g s ch = JO [("kind", JS k), ("goal", JS g), ("subst", JS s), ("children", JA ch)]

cpdLocal :: CLC.SldTree -> Budget J
cpdLocal t = do
  ok <- tick
  if not ok then return truncatedNode else case t of
    CLC.Fail -> return $ sNode "fail" "" "" []
    CLC.Success s -> return $ sNode "success" "" (cut 400 (show s)) []
    CLC.Or ch g s -> sNode "or" (maybe "" (cut 400 . show) g) (cut 400 (show s)) <$> mapM cpdLocal ch
    CLC.Conj ch ds s -> sNode "conj" (goals (map getCurr ds)) (cut 400 (show s)) . (: []) <$> cpdLocal ch
    CLC.Leaf ds s _ -> return $ sNode "leaf" (goals (map getCurr ds)) (cut 400 (show s)) []

consLocal :: SLC.SldTree -> Budget J
consLocal t = do
  ok <- tick
  if not ok then return truncatedNode else case t of
    SLC.Fail -> return $ sNode "fail" "" "" []
    SLC.Success s -> return $ sNode "success" "" (cut 400 (show s)) []
    SLC.Or ch g s -> sNode "or" (maybe "" (cut 400 . show) g) (cut 400 (show s)) <$> mapM consLocal ch
    SLC.Conj ch ds s -> sNode "conj" (goals (map getCurr ds)) (cut 400 (show s)) . (: []) <$> consLocal ch
    SLC.Leaf ds s _ -> return $ sNode "leaf" (goals (map getCurr ds)) (cut 400 (show s)) []

-- The global trees are built lazily by the specializers; we traverse them with
-- the same budget, so a diverging specialization is cut rather than hanging
-- (it is additionally guarded by a timeout).
data GTree = GTree String String String String (Maybe (Budget J)) [GTree]

cpdGT :: CGC.GlobalTree -> GTree
cpdGT t = case t of
  CGC.Leaf d gen s    -> GTree "leaf" (goals (getCurr d)) (cut 400 (show gen)) (cut 400 (show s)) Nothing []
  CGC.Node d gen l ch -> GTree "node" (goals (getCurr d)) (cut 400 (show gen)) "" (Just (cpdLocal l)) (map cpdGT ch)
  CGC.Prune d s       -> GTree "prune" (goals (getCurr d)) "" (cut 400 (show s)) Nothing []

consGT :: SGC.GlobalTree -> GTree
consGT t = case t of
  SGC.Fail                -> GTree "fail" "" "" "" Nothing []
  SGC.Success s           -> GTree "success" "" "" (cut 400 (show s)) Nothing []
  SGC.Leaf d gen s        -> GTree "leaf" (goals (getCurr d)) (cut 400 (show gen)) (cut 400 (show s)) Nothing []
  SGC.Node d gen s l ch   -> GTree "node" (goals (getCurr d)) (cut 400 (show gen)) (cut 400 (show s)) (Just (consLocal l)) (map consGT ch)
  SGC.Split gs ch s       -> GTree "split" (goals gs) "" (cut 400 (show s)) Nothing (map consGT ch)
  SGC.Transient d c s ml  -> GTree "transient" (goals (getCurr d)) "" (cut 400 (show s)) (consLocal <$> ml) [consGT c]
  SGC.Prune d s           -> GTree "prune" (goals (getCurr d)) "" (cut 400 (show s)) Nothing []

-- | Render a global tree: a budget of 'maxNodes' global nodes, each local
--   tree with its own budget, and a deadline of @us@ microseconds.  The trees
--   are lazy, so every node, every local tree and every step along a list of
--   children is evaluated under the remaining time; what is not evaluated when
--   the deadline passes becomes a @truncated@ node with @reason: "timeout"@.
--   For a specialization that timed out this shows the tree as far as the
--   specializer got.  Returns the tree, whether the node budget cut it and
--   whether the deadline did.
renderGT :: Int -> GTree -> IO (J, Bool, Bool)
renderGT us t0 = do
  start <- getCurrentTime
  let deadline = addUTCTime (fromIntegral us / 1000000) start
  ctr <- newIORef 0
  cutRef <- newIORef False
  timeRef <- newIORef False
  let within :: IO a -> IO (Maybe a)
      within act = do
        now <- getCurrentTime
        let left = floor (realToFrac (diffUTCTime deadline now) * 1000000 :: Double) :: Int
        if left <= 0 then return Nothing else timeout left act
      timedOut = writeIORef timeRef True >> return timeoutNode
      go t = do
        n <- readIORef ctr
        if n >= maxNodes
          then writeIORef cutRef True >> return truncatedNode
          else do
            m <- within $ do
              gt@(GTree k g gen s _ _) <- evaluate t
              _ <- evaluate (length k + length g + length gen + length s)
              return gt
            case m of
              Nothing -> timedOut
              Just (GTree k g gen s loc ch) -> do
                i <- readIORef ctr
                modifyIORef ctr (+ 1)
                locJ <- case loc of
                  Nothing -> return JNull
                  Just b -> do
                    r <- within (let j = evalState b 0 in evaluate (length (render j)) >> return j)
                    maybe timedOut return r
                chJ <- goList ch
                return $ JO [ ("id", JI i), ("kind", JS k), ("goal", JS g), ("gen", JS gen), ("subst", JS s)
                            , ("local", locJ), ("children", JA chJ) ]
      goList xs = do
        m <- within (evaluate (case xs of { [] -> Nothing; (y : ys) -> Just (y, ys) }))
        case m of
          Nothing -> (: []) <$> timedOut
          Just Nothing -> return []
          Just (Just (y, ys)) -> (:) <$> go y <*> goList ys
  j <- go t0
  c <- readIORef cutRef
  tc <- readIORef timeRef
  return (j, c, tc)

timeoutNode :: J
timeoutNode = JO [("kind", JS "truncated"), ("reason", JS "timeout"), ("goal", JS ""), ("subst", JS ""), ("children", JA [])]

globalTreeOf :: String -> Program G X -> Maybe GTree
globalTreeOf base p = case base of
  "cpd-det"       -> Just $ cpdGT (fst3 (CGC.topLevel p CLC.Deterministic))
  "cpd-branch"    -> Just $ cpdGT (fst3 (CGC.topLevel p CLC.Branching))
  "cpd-la"        -> Just $ cpdGT (fst3 (CGC.topLevel p (CLC.DeterministicLA 2)))
  "cpd-lap"       -> Just $ cpdGT (fst3 (CGC.topLevel p (CLC.DeterministicLAP 2)))
  "conspd-det"    -> Just $ consGT (fst3 (SGC.topLevel p SLC.Deterministic))
  "conspd-branch" -> Just $ consGT (fst3 (SGC.topLevel p SLC.Branching))
  "conspd-la"     -> Just $ consGT (fst3 (SGC.topLevel p (SLC.DeterministicLA 2)))
  _               -> Nothing
  where fst3 (a, _, _) = a

------------------------------------------------------------------------------
-- Runs

traceLen :: Int
traceLen = 400

profileJ :: String -> String -> Profile -> String -> J
profileJ sp q pr chk =
  let st = pStats pr
  in  JO [ ("spec", JS sp), ("query", JS q)
         , ("steps", JI (rsUnifs st + rsCalls st)), ("unifs", JI (rsUnifs st)), ("calls", JI (rsCalls st))
         , ("outcome", JS (show (rsOutcome st))), ("check", JS chk)
         , ("answers", JA [ JS (cut 300 (showAnswer a)) | a <- rsAnswers st ])
         , ("answerSteps", JA (map JI (pAnswerSteps pr)))
         , ("profile", JA [ JO [ ("rel", JS r), ("calls", JI (rpCalls p)), ("unifOk", JI (rpUnifOk p))
                               , ("unifFail", JI (rpUnifFail p)) ]
                          | (r, p) <- M.toList (pRels pr) ])
         , ("trace", JA [ JA [JS e, JS x] | (e, x) <- pTrace pr ])
         , ("traceTruncated", JB (pTraceCut pr)) ]
  where showAnswer a = case a of
          [t] -> show t
          ts  -> "(" ++ intercalate ", " (map show ts) ++ ")"

errRun :: String -> String -> String -> J
errRun sp q why = JO [ ("spec", JS sp), ("query", JS q), ("steps", JI 0), ("unifs", JI 0), ("calls", JI 0)
                     , ("outcome", JS why), ("check", JS "-"), ("answers", JA []), ("answerSteps", JA [])
                     , ("profile", JA []), ("trace", JA []), ("traceTruncated", JB False) ]

timed :: IO a -> IO (a, Double)
timed act = do
  t0 <- getCurrentTime
  x <- act
  t1 <- getCurrentTime
  return (x, realToFrac (diffUTCTime t1 t0) * 1000)

------------------------------------------------------------------------------

data Opts = Opts { oOut :: FilePath, oN :: Int, oFuel :: Int, oOnly :: [String]
                 , oSpecs :: [String], oVanilla :: Bool, oDirs :: [FilePath] }

defaultSpecs :: [String]
defaultSpecs = [ "original", "cpd-det", "cpd-la", "cpd-branch", "conspd-det", "conspd-la"
               , "cpd-det+opt", "cpd-det+opt+sched", "conspd-det+opt", "conspd-det+opt+sched"
               , "cpd-la+opt+sched", "conspd-la+opt+sched", "cpd-lap", "cpd-lap+opt+sched" ]

parse :: [String] -> Opts -> Opts
parse ("--out" : d : r)     o = parse r o { oOut = d }
parse ("--answers" : n : r) o = parse r o { oN = read n }
parse ("--fuel" : n : r)    o = parse r o { oFuel = read n }
parse ("--only" : n : r)    o = parse r o { oOnly = splitOn "," n }
parse ("--spec" : s : r)    o = parse r o { oSpecs = splitOn "," s }
parse ("--vanilla" : r)     o = parse r o { oVanilla = True }
parse (d : r)               o = parse r o { oDirs = oDirs o ++ [d] }
parse []                    o = o

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  o <- flip parse (Opts ("viz" </> "data") 10 300000 [] defaultSpecs False []) <$> System.Environment.getArgs
  createDirectoryIfMissing True (oOut o)
  benches <- concat <$> mapM findBenches (oDirs o)
  let selected = [ b | b@(n, _, _) <- benches, null (oOnly o) || n `elem` oOnly o ]
      allSps = allVariants
      sps = [ s | n <- oSpecs o, s <- allSps, spName s == n ]
  entries <- fmap concat $ forM selected $ \(name, group, mk) -> do
    hPutStrLn stderr ("== " ++ name)
    eb <- loadBench mk name (oN o) (oFuel o)
    case eb of
      Left err -> hPutStrLn stderr ("  load failed: " ++ err) >> return []
      Right b0 -> do
        src <- readFile mk
        let runsFile = take (length mk - 3) mk ++ ".runs"
        hasRuns <- doesFileExist runsFile
        runsSrc <- if hasRuns then readFile runsFile else return ""
        forM (b0 : [ vanillaBench b0 | oVanilla o ]) $ \b -> do
          let src' = if bName b == bName b0 then src else src ++ "\n-- (run through the vanilla self-interpreter, see src/Measure/Vanilla.hs)\n"
          j <- exportBench sps group src' runsSrc b
          let file = bName b ++ ".json"
          writeFile (oOut o </> file) (render j)
          return $ JO [ ("name", JS (bName b)), ("group", JS group), ("file", JS file)
                      , ("queries", JI (length (bRuns b))), ("specializers", JA (map (JS . spName) sps)) ]
  now <- getCurrentTime
  writeFile (oOut o </> "index.json") $ render $
    JO [ ("generated", JS (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now))
       , ("fuel", JI (oFuel o)), ("answers", JI (oN o)), ("benches", JA entries) ]

exportBench :: [Specializer] -> String -> String -> String -> Bench -> IO J
exportBench sps group src runsSrc b = do
  let prog@(Program odefs ogoal) = bProgram b
      queries = bRuns b
  -- reference runs of the original program
  refs <- forM queries $ \(qn, q) -> do
    let (ws, body) = topLevelFreshVars q
    pr <- evaluate (forceP (runGoalProfiled traceLen odefs ws body (bAnswers b) (bFuel b)))
    return (qn, ws, body, pr)
  cache <- newIORef M.empty
  specRows <- forM sps $ \sp -> do
    hPutStrLn stderr ("   " ++ spName sp)
    c <- readIORef cache
    base <- case M.lookup (spBase sp) c of
      Just r -> return r
      Nothing -> do
        r <- timeout specTimeoutUs $ timed $ do
          e <- try (evaluate (forceRes (spRun sp prog)))
          return $ case e of
            Left (ex :: SomeException) -> Left (takeWhile (/= '\n') (show ex))
            Right x -> x
        let r' = case r of
              Nothing -> (Left "specialization timed out", 0)
              Just x  -> x
        modifyIORef cache (M.insert (spBase sp) r')
        return r'
    res <- case base of
      (Right r, ms) -> do
        e <- try (evaluate (forceRes (Right (spPost sp r))))
        return $ case e of
          Left (ex :: SomeException) -> (Left ("post: " ++ takeWhile (/= '\n') (show ex)), ms)
          Right x -> (x, ms)
      other -> return other
    -- trees for base specializers; when the specialization failed or timed
    -- out, the tree is rendered with the same time budget, i.e. as far as the
    -- specializer got
    tree <- if spName sp == spBase sp
      then case globalTreeOf (spBase sp) prog of
        Nothing -> return Nothing
        Just gt -> do
          let budget = either (const specTimeoutUs) (const (30 * 1000000)) (fst res)
          (j, wasCut, timedOut) <- renderGT budget gt
          _ <- evaluate (length (render j))
          return (Just (j, wasCut, timedOut))
      else return Nothing
    let treeFields = [ ("globalTree", maybe JNull (\(j, _, _) -> j) tree)
                     , ("treeTruncated", JB (maybe False (\(_, c, t) -> c || t) tree))
                     , ("treeTimedOut", JB (maybe False (\(_, _, t) -> t) tree)) ]
    runs <- case fst res of
      Left err -> return [ errRun (spName sp) qn ("spec-fail: " ++ take 80 err) | (qn, _, _, _) <- refs ]
      Right r -> forM refs $ \(qn, ws, origBody, ref) ->
        case runAgainst r (fresh ws origBody) of
          Left err -> return (errRun (spName sp) qn err)
          Right (qvs, goal0) -> do
            let (rdefs, goal) = case spPerQuery sp of
                  Nothing -> (resDefs r, goal0)
                  Just f  -> f (resDefs r) qvs goal0
                r' = r { resDefs = rdefs }
            e <- try (evaluate (forceP (runGoalProfiled traceLen rdefs qvs goal (bAnswers b) (bFuel b))))
            case e of
              Left (ex :: SomeException) -> return (errRun (spName sp) qn ("eval-crash: " ++ take 60 (show ex)))
              Right pr -> do
                let chk = if spName sp == "original" then "ok"
                          else crossCheck b r' qvs goal ws origBody (pStats ref) (pStats pr)
                _ <- evaluate (length chk)
                return (profileJ (spName sp) qn pr chk)
    let specJ = case res of
          (Left err, ms) ->
            JO [ ("name", JS (spName sp)), ("base", JS (spBase sp)), ("specMs", JN ms), ("error", JS err)
               , ("residual", JS ""), ("residualGoal", JS ""), ("defs", JI 0), ("size", JI 0) ]
               `withFields` treeFields
          (Right r, ms) ->
            let (nd, sz) = programSize (resDefs r) (resGoal r)
            in  JO [ ("name", JS (spName sp)), ("base", JS (spBase sp)), ("specMs", JN ms), ("error", JNull)
                   , ("residual", JS (clean (unlines (map show (resDefs r)))))
                   , ("residualGoal", JS (clean (show (resGoal r))))
                   , ("defs", JI nd), ("size", JI sz) ]
                   `withFields` treeFields
    return (specJ, runs)
  let specQuery = case specCall ogoal of
        Just (_, f, ts) -> clean (show (Invoke f ts :: G X))
        Nothing -> clean (show ogoal)
  return $ JO
    [ ("name", JS (bName b)), ("group", JS group), ("source", JS src), ("runsSource", JS runsSrc)
    , ("specQuery", JS specQuery), ("fuel", JI (bFuel b)), ("answers", JI (bAnswers b))
    , ("queries", JA [ JO [("id", JS qn), ("goal", JS (clean (show (snd (topLevelFreshVars q)))))] | (qn, q) <- queries ])
    , ("specializers", JA (map fst specRows))
    , ("runs", JA (concatMap snd specRows)) ]

forceP :: Profile -> Profile
forceP p = length (rsAnswers (pStats p)) `seq` M.size (pRels p) `seq` length (pTrace p) `seq` p

findBenches :: FilePath -> IO [(String, String, FilePath)]
findBenches dir = do
  entries <- sort <$> listDirectory dir
  concat <$> forM entries (\e -> do
    let sub = dir </> e
    isDir <- doesDirectoryExist sub
    let mk = sub </> e <.> "mk"
    hasMk <- doesFileExist mk
    if isDir && hasMk then return [(e, takeFileName (takeDirectory sub), mk)]
    else if isDir then findBenches sub else return [])

withFields :: J -> [(String, J)] -> J
withFields (JO kvs) more = JO (kvs ++ more)
withFields j _ = j
