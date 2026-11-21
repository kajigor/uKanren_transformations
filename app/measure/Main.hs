-- | Measurement driver.
--
--   measure [OPTIONS] DIR...
--
--   Every benchmark lives in its own sub-directory of DIR:
--   <name>/<name>.mk (definitions + specialization query) and optionally
--   <name>/<name>.runs (run-time queries, one '? goal' per line).
--
--   Options:
--     --answers N       answers to ask for (default 10)
--     --fuel F          step budget per run (default 2000000)
--     --timeout S       specialization timeout in seconds (default 60)
--     --jobs J          benchmarks measured in parallel (default 1)
--     --only A,B        only these benchmarks
--     --bases A,B       base specializers (default: all non-raw ones)
--     --variants V      base | std | v2 | full | ablation   (default std)
--     --spec S1,S2      explicit specializer names, e.g. cpd-det+ue+fail+sched
--                       (overrides --bases/--variants)
--     --csv FILE / --md FILE / --dump
--     --list            print bases, pipeline components and presets
--
--   A specializer name is BASE(+COMPONENT)*; see 'passTokens'.
module Main where

import           Control.Concurrent
import           Control.Exception  (finally)
import           Control.Monad      (forM, replicateM_, when)
import           Data.List          (isSuffixOf, sort)
import           Data.List.Split    (splitOn)
import           Data.Maybe         (fromMaybe)
import           Measure.Runner
import           System.Directory   (doesDirectoryExist, doesFileExist, listDirectory)
import           System.Environment (getArgs)
import           System.Exit        (exitFailure)
import           System.FilePath    ((</>), (<.>))
import           System.IO

data Opts = Opts
  { oN :: Int, oFuel :: Int, oTimeout :: Int, oJobs :: Int, oOnly :: [String]
  , oCsv :: Maybe FilePath, oMd :: Maybe FilePath, oSpecs :: Maybe [String]
  , oBases :: Maybe [String], oVariants :: String, oDirs :: [FilePath], oDump :: Bool, oList :: Bool }

defaults :: Opts
defaults = Opts 10 2000000 60 1 [] Nothing Nothing Nothing Nothing "std" [] False False

parse :: [String] -> Opts -> Opts
parse ("--answers" : n : r)  o = parse r o { oN = read n }
parse ("--fuel" : n : r)     o = parse r o { oFuel = read n }
parse ("--timeout" : n : r)  o = parse r o { oTimeout = read n }
parse ("--jobs" : n : r)     o = parse r o { oJobs = read n }
parse ("--only" : n : r)     o = parse r o { oOnly = splitOn "," n }
parse ("--csv" : f : r)      o = parse r o { oCsv = Just f }
parse ("--md" : f : r)       o = parse r o { oMd = Just f }
parse ("--dump" : r)         o = parse r o { oDump = True }
parse ("--list" : r)         o = parse r o { oList = True }
parse ("--spec" : s : r)     o = parse r o { oSpecs = Just (splitOn "," s) }
parse ("--bases" : s : r)    o = parse r o { oBases = Just (splitOn "," s) }
parse ("--variants" : s : r) o = parse r o { oVariants = s }
parse (d : r)                o = parse r o { oDirs = oDirs o ++ [d] }
parse []                     o = o

-- | Every component, in pipeline order.
fullPipe :: [String]
fullPipe = map fst passTokens

-- | Variant presets: lists of component tokens added to every base.
--   @ablation@ is the full pipeline with one component removed at a time.
variantSets :: String -> Either String [[String]]
variantSets "base"     = Right [[]]
variantSets "std"      = Right [[], ["opt"], ["sched"], ["opt", "sched"]]
variantSets "full"     = Right [[], fullPipe]
variantSets "v2"       = Right [[], ["opt"], ["sched"], ["opt", "sched"], ["opt", "sched", "det"], fullPipe]
variantSets "ablation" = Right ([[], fullPipe] ++ [ filter (/= c) fullPipe | c <- fullPipe ])
variantSets v          = Left ("unknown --variants " ++ v)

specNames :: Opts -> Either String [String]
specNames o = case oSpecs o of
  Just ns -> Right ns
  Nothing -> do
    vs <- variantSets (oVariants o)
    let bs = fromMaybe (filter (not . ("/raw" `isSuffixOf`)) baseNames) (oBases o)
    return [ b ++ concatMap ('+' :) v | b <- bs, v <- vs ]

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  o <- flip parse defaults <$> getArgs
  when (oList o) $ do
    putStrLn ("bases: " ++ unwords baseNames)
    putStrLn "components (pipeline order):"
    mapM_ (\(t, d) -> putStrLn ("  " ++ t ++ "\t" ++ d)) passTokens
    putStrLn "presets: opt = ue+fail+inl+merge+dead"
    putStrLn "variants: base | std | v2 | full | ablation"
  sps <- case specNames o >>= mapM mkSpecializer of
    Left err -> do hPutStrLn stderr err; exitFailure
    Right s  -> return s
  benches <- concat <$> mapM findBenches (oDirs o)
  let selected = [ b | b@(n, _) <- benches, null (oOnly o) || n `elem` oOnly o ]
  -- a small worker pool; results are collected in benchmark order
  outLock <- newMVar ()
  slots <- mapM (const newEmptyMVar) selected
  queue <- newMVar (zip selected slots)
  done <- newEmptyMVar
  let nj = max 1 (oJobs o)
      worker = loop `finally` putMVar done ()
      loop = do
        next <- modifyMVar queue (\q -> return (drop 1 q, take 1 q))
        case next of
          [((name, mk), slot)] -> do
            rs <- runBench o sps name mk
            withMVar outLock $ \_ -> do
              hPutStrLn stderr ("== " ++ name)
              putStr (renderMarkdown rs)
              when (oDump o) $
                mapM_ (\r -> putStrLn ("--- " ++ rSpec r ++ " " ++ rQuery r ++ "\n" ++ rProg r)) rs
            putMVar slot rs
            loop
          _ -> return ()
  replicateM_ nj (forkIO worker)
  replicateM_ nj (takeMVar done)
  rows <- concat <$> mapM readMVar slots
  maybe (return ()) (\f -> writeFile f (renderCsv rows)) (oCsv o)
  maybe (return ()) (\f -> writeFile f (renderMarkdown rows)) (oMd o)

runBench :: Opts -> [Specializer] -> String -> FilePath -> IO [Row]
runBench o sps name mk = do
  eb <- loadBench mk name (oN o) (oFuel o) (oTimeout o)
  case eb of
    Left err -> do hPutStrLn stderr ("  " ++ name ++ ": load failed: " ++ err); return []
    Right b  -> measureBench sps b

findBenches :: FilePath -> IO [(String, FilePath)]
findBenches dir = do
  entries <- sort <$> listDirectory dir
  concat <$> forM entries (\e -> do
    let sub = dir </> e
    isDir <- doesDirectoryExist sub
    let mk = sub </> e <.> "mk"
    hasMk <- doesFileExist mk
    if isDir && hasMk then return [(e, mk)]
    else if isDir then findBenches sub else return [])
