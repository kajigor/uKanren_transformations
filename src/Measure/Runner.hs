{-# LANGUAGE ScopedTypeVariables #-}
-- | Measurement harness: runs a set of specializers on a benchmark, evaluates
--   original and residual programs on a list of run-time queries with the
--   instrumented interpreter, and cross-checks the answers.
module Measure.Runner
  ( Specializer (..)
  , Residual (..)
  , Bench (..)
  , Row (..)
  , specializers
  , baseNames
  , mkSpecializer
  , passTokens
  , expandPasses
  , withPostOpt
  , withSchedule
  , measureBench
  , programSize
  , renderMarkdown
  , renderCsv
  , loadBench
  ) where

import           Control.Exception    (SomeException, evaluate, try)
import           Data.IORef
import           Data.List            (intercalate, nub)
import qualified Data.Map.Strict      as M
import           Data.Maybe           (fromMaybe)
import           Data.Time.Clock      (diffUTCTime, getCurrentTime)
import           Def
import           Eval                 (postEval)
import           Measure.Interp
import qualified Parser.Parser        as Parser
import qualified Parser.SimpleParser  as SP
import           Program
import           Syntax
import           System.Directory     (doesFileExist)
import           System.Timeout       (timeout)
import           Text.Megaparsec      (errorBundlePretty, runParser, many, eof)
import           Text.Printf          (printf)
import qualified Parser.Lexer         as Lexer

import qualified CPD.LocalControl         as CPDLC
import qualified ConsPD.LocalControl      as ConsLC
import qualified Transformer.CPD          as CPD
import qualified Transformer.ConsPDGlobal as ConsPDGlobal
import qualified Transformer.PD           as PD
import qualified PostOpt
import qualified Core.Lint                as Lint
import qualified Schedule
import qualified Data.Set                 as S

-- | Result of a specializer: residual definitions plus a residual goal whose
--   free variables are @resVars@; @resArgs@ are the terms of the original
--   specialization call expressed over @resVars@ (so a run-time query
--   @p(s1..sn)@ is evaluated as @(t1..tn) == (s1..sn) & resGoal@).
data Residual = Residual
  { resDefs :: [Def G X]
  , resGoal :: G X
  , resVars :: [X]
  , resArgs :: [Term X]
  }

data Specializer = Specializer
  { spName     :: String
  , spBase     :: String   -- ^ name of the base specializer (results are shared)
  , spRun      :: Program G X -> Either String Residual
  , spPost     :: Residual -> Residual
  , spPerQuery :: Maybe ([Def G X] -> [X] -> G X -> ([Def G X], G X))
    -- ^ optional query-directed post-processing (defs, query vars, run goal)
  }

data Bench = Bench
  { bName    :: String
  , bProgram :: Program G X      -- ^ definitions + specialization goal
  , bRuns    :: [(String, G X)]  -- ^ run-time queries (closed by fresh)
  , bAnswers :: Int              -- ^ how many answers to ask for
  , bFuel    :: Int
  , bTimeout :: Int              -- ^ specialization timeout, seconds
  }

data Row = Row
  { rBench   :: String
  , rQuery   :: String
  , rSpec    :: String
  , rSpecMs  :: Double
  , rDefs    :: Int
  , rSize    :: Int
  , rUnifs   :: Int
  , rCalls   :: Int
  , rEvalMs  :: Double
  , rNAns    :: Int
  , rOutcome :: String
  , rCheck   :: String
  , rProg    :: String   -- ^ pretty-printed residual program (for --dump)
  , rLint    :: String   -- ^ Core.Lint summary of the residual program
  }

-- | Split the specialization goal @fresh vs. p(ts)@.
specCall :: G X -> Maybe ([X], Name, [Term X])
specCall g = case topLevelFreshVars g of
  (vs, Invoke p ts) -> Just (vs, p, ts)
  _                 -> Nothing

-- | Wrap the triple produced by the purification step.
fromPurified :: Program G X -> [X] -> (G X, [X], [Def G X]) -> Either String Residual
fromPurified (Program _ goal) names (g, _xs, defs) =
  case specCall goal of
    Nothing -> Left "specialization goal is not a single call"
    Just (vs, _, ts) ->
      let ren = M.fromList (zip vs names)
          rn x = fromMaybe x (M.lookup x ren)
      in  Right $ Residual defs g names (map (fmap rn) ts)

topNames :: Program G X -> [X]
topNames (Program _ goal) = [ 'x' : show i | (i, _) <- zip [0 :: Int ..] (fst $ topLevelFreshVars goal) ]

specializers :: [Specializer]
specializers =
  [ sp "original" $ \p@(Program defs goal) ->
      case specCall goal of
        Nothing -> Left "goal is not a single call"
        Just (vs, f, ts) -> Right $ Residual defs (Invoke f ts) vs ts
  , sp "pd" $ \p ->
      case PD.purified (PD.runTransformation p) of
        Nothing  -> Left "pd failed"
        Just pur -> fromPurified p (topNames p) pur
  , sp "cpd-det" $ \p ->
      fromPurified p (topNames p) (CPD.purified (CPD.runTransformation p CPDLC.Deterministic))
  , sp "cpd-branch" $ \p ->
      fromPurified p (topNames p) (CPD.purified (CPD.runTransformation p CPDLC.Branching))
  , sp "conspd-det" $ \p ->
      ConsPDGlobal.purified (ConsPDGlobal.runTransformation p ConsLC.Deterministic) >>= fromPurified p (topNames p)
  , sp "conspd-branch" $ \p ->
      ConsPDGlobal.purified (ConsPDGlobal.runTransformation p ConsLC.Branching) >>= fromPurified p (topNames p)
  , sp "pd/raw" $ \p ->
      case PD.beforePurification (PD.runTransformation p) of
        Nothing  -> Left "pd failed"
        Just pur -> fromPurified p (topNames p) pur
    -- raw residualization output (before purification): only meant for
    -- linting pass boundaries, see docs/DESIGN.md
  , sp "cpd-det/raw" $ \p ->
      let Program ds g = CPD.beforePur (CPD.runTransformation p CPDLC.Deterministic)
      in fromPurified p (topNames p) (g, [], ds)
  , sp "cpd-branch/raw" $ \p ->
      let Program ds g = CPD.beforePur (CPD.runTransformation p CPDLC.Branching)
      in fromPurified p (topNames p) (g, [], ds)
  , sp "conspd-det/raw" $ \p ->
      ConsPDGlobal.beforePur (ConsPDGlobal.runTransformation p ConsLC.Deterministic) >>= \(Program ds g) ->
      fromPurified p (topNames p) (g, [], ds)
  ]
  where sp n f = Specializer n n f id Nothing

------------------------------------------------------------------------------
-- Modular pipeline.  A specializer name is  BASE(+TOKEN)*  where BASE is one
-- of 'baseNames' and every TOKEN switches on one post-specialization
-- component (or a preset of several).  Components are applied in a fixed
-- order (the order of 'passTokens'), independently of the order in the name,
-- so that e.g. cpd-det+sched+ue and cpd-det+ue+sched are the same pipeline.

-- | Atomic components, in pipeline order, with a description.
passTokens :: [(String, String)]
passTokens =
  [ ("ue",    "existential unification elimination + constructor decomposition")
  , ("fail",  "may-succeed failure analysis")
  , ("inl",   "post-unfolding of non-recursive definitions")
  , ("merge", "duplicate-definition merging (bisimulation)")
  , ("dead",  "unreachable-definition removal")
  , ("sched", "per-query mode-directed conjunct scheduling")
  ]

-- | Presets expand to several atomic components.
presets :: [(String, [String])]
presets =
  [ ("opt", ["ue", "fail", "inl", "merge", "dead"]) ]

expandPasses :: [String] -> Either String [String]
expandPasses ts = do
  xs <- concat <$> mapM one ts
  return [ t | (t, _) <- passTokens, t `elem` xs ]
  where
    one t | Just ps <- lookup t presets = Right ps
          | t `elem` map fst passTokens = Right [t]
          | otherwise = Left ("unknown pipeline component: " ++ t)

baseNames :: [String]
baseNames = map spName specializers

-- | Build a specializer from its name, e.g. @cpd-det+ue+fail+sched@.
mkSpecializer :: String -> Either String Specializer
mkSpecializer name = do
  let (b : ts) = splitPlus name
  base <- maybe (Left ("unknown base specializer: " ++ b)) Right
                (lookup b [ (spName s, s) | s <- specializers ])
  ps <- expandPasses ts
  let postPasses = [ p | (t, p) <- postOptPasses, t `elem` ps ]
      post r | null postPasses = r
             | otherwise =
                 let (ds, g) = PostOpt.optimizeWith (PostOpt.PNormalize : postPasses) (resVars r) (resDefs r, resGoal r)
                 in r { resDefs = ds, resGoal = g }
      perQ | "sched" `elem` ps = Just $ \ds _qvs g -> Schedule.schedule S.empty (ds, g)
           | otherwise = Nothing
  return base { spName = name, spPost = post, spPerQuery = perQ }
  where
    postOptPasses = [ ("ue", PostOpt.PUnifElim), ("fail", PostOpt.PFailure), ("inl", PostOpt.PInline)
                    , ("merge", PostOpt.PMerge), ("dead", PostOpt.PUnreachable) ]
    splitPlus str = case break (== '+') str of
      (a, [])      -> [a]
      (a, _ : rest) -> a : splitPlus rest

-- | Backwards-compatible helpers.
withPostOpt :: Specializer -> Specializer
withPostOpt sp = either error id (mkSpecializer (spName sp ++ "+opt"))

withSchedule :: Specializer -> Specializer
withSchedule sp = either error id (mkSpecializer (spName sp ++ "+sched"))

-- | Number of definitions and number of goal nodes (unifications + calls).
programSize :: [Def G X] -> G X -> (Int, Int)
programSize defs g = (length defs, sum (map (goalSize . getBody) defs) + goalSize g)
  where
    goalSize (_ :=: _)            = 1
    goalSize (Invoke _ _)         = 1
    goalSize (Conjunction a b cs) = sum (map goalSize (a : b : cs))
    goalSize (Disjunction a b cs) = sum (map goalSize (a : b : cs))
    goalSize (Fresh _ x)          = goalSize x
    goalSize (Delay x)            = goalSize x

-- | Build the goal that evaluates run-time query @q@ against residual @r@.
runAgainst :: Residual -> G X -> Either String ([X], G X)
runAgainst r q = case specCall q of
  Nothing -> Left "run query is not a single call"
  Just (ws, _, ss) ->
    let ws' = map ("r_" ++) ws
        ren = M.fromList (zip ws ws')
        ss' = map (fmap (\x -> fromMaybe x (M.lookup x ren))) ss
        bind = C "tuple" (resArgs r) :=: C "tuple" ss'
        extra = nub (resVars r ++ concatMap fv (resArgs r)) `without` ws'
        body = fresh extra (bind &&& resGoal r)
    in  Right (ws', body)
  where without xs ys = filter (`notElem` ys) xs

answerGoal :: [X] -> Answer -> G X
answerGoal qvs ans =
  let avs = nub (concatMap fv ans)
      nm i = "a_" ++ show i
      ts = map (fmap nm) ans
  in  fresh (map nm avs) (C "tuple" (map V qvs) :=: C "tuple" ts)

-- | Does @defs@/@goal@ admit (an instance of) answer @a@?
admits :: Int -> [Def G X] -> [X] -> G X -> Answer -> Bool
admits fuel defs qvs goal a =
  let st = runGoal defs qvs (answerGoal qvs a &&& goal) 1 fuel
  in  not (null (rsAnswers st))

timed :: IO a -> IO (a, Double)
timed act = do
  t0 <- getCurrentTime
  x <- act
  t1 <- getCurrentTime
  return (x, realToFrac (diffUTCTime t1 t0) * 1000)

measureBench :: [Specializer] -> Bench -> IO [Row]
measureBench sps b = do
  let Program odefs _ = bProgram b
  -- reference: the original program on each query
  refs <- mapM (\(qn, q) -> do
                  let (ws, body) = topLevelFreshVars q
                  st <- evaluate (runGoal odefs ws body (bAnswers b) (bFuel b))
                  return (qn, ws, body, st)) (bRuns b)
  cache <- newIORef M.empty
  concat <$> mapM (one cache refs) sps
  where
    base sp = do
      res <- timeout (bTimeout b * 1000000) $ timed $ do
        e <- try (evaluate (forceRes (spRun sp (bProgram b))))
        return $ case e of
          Left (ex :: SomeException) -> Left (takeWhile (/= '\n') (show ex))
          Right x -> x
      return res
    post sp res = case res of
      Just (Right r, ms) -> do
        (r', ms') <- timed $ do
          e <- try (evaluate (forceRes (Right (spPost sp r))))
          return $ case e of
            Left (ex :: SomeException) -> Left ("post: " ++ takeWhile (/= '\n') (show ex))
            Right x -> x
        return (Just (r', ms + ms'))
      _ -> return res
    one cache refs sp = do
      c <- readIORef cache
      res0 <- case M.lookup (spBase sp) c of
        Just r -> return r
        Nothing -> do
          r <- base sp
          modifyIORef cache (M.insert (spBase sp) r)
          return r
      res <- post sp res0
      case res of
        Nothing -> return [failedRow sp "spec-timeout"]
        Just (Left err, _) -> return [failedRow sp ("spec-fail: " ++ take 60 err)]
        Just (Right r, specMs) -> mapM (runOne sp r specMs) refs
    failedRow sp why = Row (bName b) "*" (spName sp) 0 0 0 0 0 0 0 why "-" "" "-"
    runOne sp r specMs (qn, ws, origBody, ref) =
      case runAgainst r (fresh ws origBody) of
        Left err -> return $ Row (bName b) qn (spName sp) specMs 0 0 0 0 0 0 err "-" "" "-"
        Right (qvs, goal0) -> do
          let (rdefs, goal) = case spPerQuery sp of
                Nothing -> (resDefs r, goal0)
                Just f  -> f (resDefs r) qvs goal0
              r' = r { resDefs = rdefs }
              (nd, sz) = programSize rdefs goal
              progTxt = unlines (map show rdefs) ++ "?? " ++ show goal
          let lintTxt = Lint.lintSummary (Lint.lintProgram qvs rdefs goal)
          (_, qMs) <- timed (evaluate (length progTxt))
          er <- try $ timed (evaluate (forceStats (runGoal rdefs qvs goal (bAnswers b) (bFuel b))))
          case er of
            Left (ex :: SomeException) ->
              return $ Row (bName b) qn (spName sp) (specMs + qMs) nd sz 0 0 0 0 ("eval-crash: " ++ take 50 (show ex)) "-" progTxt lintTxt
            Right (st, evalMs) -> do
              let chk = check r' qvs goal ws origBody ref st
              return $ Row (bName b) qn (spName sp) (specMs + qMs) nd sz (rsUnifs st) (rsCalls st) evalMs
                           (length (rsAnswers st)) (show (rsOutcome st)) chk
                           (progTxt ++ "\n-- answers: " ++ show (rsAnswers st) ++ "\n-- reference: " ++ show (rsAnswers ref))
                           lintTxt
    -- cross validation
    check r qvs goal ws origBody ref st
      | rsOutcome ref == Finished && rsOutcome st == Finished =
          if sameSet (rsAnswers ref) (rsAnswers st) then "ok" else "MISMATCH"
      | otherwise =
          let Program odefs _ = bProgram b
              fuel = bFuel b
              sound = all (admits fuel odefs ws origBody) (take 20 (rsAnswers st))
              -- answer variables are named r_<w> in the residual run
              complete = all (admits fuel (resDefs r) qvs goal) (take 20 (rsAnswers ref))
          in  case (sound, complete) of
                (True, True)  -> "ok~"
                (False, _)    -> "unverified-sound"
                (_, False)    -> "unverified-complete"
    sameSet xs ys = all (`elem` ys) xs && all (`elem` xs) ys

forceStats :: RunStats -> RunStats
forceStats st = length (rsAnswers st) `seq` rsUnifs st `seq` st

forceRes :: Either String Residual -> Either String Residual
forceRes e@(Left s) = length s `seq` e
forceRes e@(Right r) = length (show (resDefs r)) `seq` length (show (resGoal r)) `seq` e

------------------------------------------------------------------------------
-- Loading benchmarks: <dir>/<name>.mk holds definitions and the
-- specialization query; <dir>/<name>.runs holds run-time queries, one per
-- line, in the form  @? goal@  (lines starting with -- are comments).

loadBench :: FilePath -> String -> Int -> Int -> Int -> IO (Either String Bench)
loadBench mkFile name n fuel tmo = do
  parsed <- Parser.importsParser Parser.Simple mkFile
  case parsed of
    Left err -> return $ Left (show err)
    Right prog -> do
      let runsFile = take (length mkFile - 3) mkFile ++ ".runs"
      ex <- doesFileExist runsFile
      runs <- if ex
        then do
          txt <- readFile runsFile
          let ls = [ l | l <- lines txt, not (null (dropWhile (== ' ') l)), take 2 (dropWhile (== ' ') l) /= "--" ]
          return $ mapM parseRun (zip [1 :: Int ..] ls)
        else return $ Right [("spec", getGoal prog)]
      return $ (\rs -> Bench name prog rs n fuel tmo) <$> runs
  where
    parseRun (i, l) =
      case runParser (Lexer.sc *> SP.parseQuery <* eof) "runs" l of
        Left e  -> Left (errorBundlePretty e)
        Right g -> Right (printf "q%d" i, postEval [] g)

------------------------------------------------------------------------------

renderMarkdown :: [Row] -> String
renderMarkdown rows = unlines $
  [ "| bench | query | specializer | spec ms | defs | size | unifs | calls | eval ms | #ans | outcome | check | lint |"
  , "|---|---|---|---:|---:|---:|---:|---:|---:|---:|---|---|---|" ] ++
  [ printf "| %s | %s | %s | %.1f | %d | %d | %d | %d | %.2f | %d | %s | %s | %s |"
      (rBench r) (rQuery r) (rSpec r) (rSpecMs r) (rDefs r) (rSize r) (rUnifs r) (rCalls r) (rEvalMs r) (rNAns r) (rOutcome r) (rCheck r) (rLint r)
  | r <- rows ]

renderCsv :: [Row] -> String
renderCsv rows = unlines $
  "bench,query,specializer,spec_ms,defs,size,unifs,calls,eval_ms,answers,outcome,check,lint" :
  [ intercalate "," [rBench r, rQuery r, rSpec r, printf "%.2f" (rSpecMs r), show (rDefs r), show (rSize r), show (rUnifs r), show (rCalls r), printf "%.3f" (rEvalMs r), show (rNAns r), rOutcome r, rCheck r, rLint r]
  | r <- rows ]
