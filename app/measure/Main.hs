-- | Measurement driver.
--
--   measure [--answers N] [--fuel F] [--only NAME] [--csv FILE] [--md FILE] [--spec a,b,..] [--vanilla] DIR
--
--   --vanilla also measures each benchmark encoded for the vanilla
--   self-interpreter ("<name>@vanilla", see "Measure.Vanilla").
--
--   Every benchmark lives in its own sub-directory of DIR:
--   <name>/<name>.mk (definitions + specialization query) and optionally
--   <name>/<name>.runs (run-time queries, one '? goal' per line).
module Main where

import           Control.Monad      (forM, when)
import           Data.List          (isPrefixOf, sort)
import           Data.List.Split    (splitOn)
import           Measure.Runner
import           Measure.Vanilla    (vanillaBench)
import           System.Directory   (doesDirectoryExist, doesFileExist, listDirectory)
import           System.Environment (getArgs)
import           System.FilePath    ((</>), (<.>))
import           System.IO

data Opts = Opts { oN :: Int, oFuel :: Int, oOnly :: [String], oCsv :: Maybe FilePath
                 , oMd :: Maybe FilePath, oSpecs :: Maybe [String], oDirs :: [FilePath], oDump :: Bool, oVanilla :: Bool }

parse :: [String] -> Opts -> Opts
parse ("--answers" : n : r) o = parse r o { oN = read n }
parse ("--fuel" : n : r)    o = parse r o { oFuel = read n }
parse ("--only" : n : r)    o = parse r o { oOnly = splitOn "," n }
parse ("--csv" : f : r)     o = parse r o { oCsv = Just f }
parse ("--md" : f : r)      o = parse r o { oMd = Just f }
parse ("--dump" : r)        o = parse r o { oDump = True }
parse ("--vanilla" : r)     o = parse r o { oVanilla = True }
parse ("--spec" : s : r)    o = parse r o { oSpecs = Just (splitOn "," s) }
parse (d : r)               o = parse r o { oDirs = oDirs o ++ [d] }
parse []                    o = o

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  o <- flip parse (Opts 10 2000000 [] Nothing Nothing Nothing [] False False) <$> getArgs
  benches <- concat <$> mapM findBenches (oDirs o)
  let selected = [ b | b@(n, _) <- benches, null (oOnly o) || n `elem` oOnly o ]
      allSps = allVariants
      sps = maybe allSps (\ns -> filter ((`elem` ns) . spName) allSps) (oSpecs o)
  rows <- concat <$> forM selected (\(name, mk) -> do
    hPutStrLn stderr ("== " ++ name)
    eb <- loadBench mk name (oN o) (oFuel o)
    case eb of
      Left err -> do hPutStrLn stderr ("  load failed: " ++ err); return []
      Right b0 -> fmap concat $ forM (b0 : [ vanillaBench b0 | oVanilla o ]) $ \b -> do
        rs <- measureBench sps b
        putStr (renderMarkdown rs)
        if oDump o then mapM_ (\r -> putStrLn ("--- " ++ rSpec r ++ " " ++ rQuery r ++ "\n" ++ rProg r)) rs else return ()
        return rs)
  maybe (return ()) (\f -> writeFile f (renderCsv rows)) (oCsv o)
  maybe (return ()) (\f -> writeFile f (renderMarkdown rows)) (oMd o)

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
