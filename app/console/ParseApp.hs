module ParseApp where

import Syntax
import Program
import Text.Printf (printf)
import Util.File (prologExt)
import Parser.SimplePretty as PP
import qualified Transformer.MkToProlog

run :: (String -> IO (Either String (Program G X))) -> String -> IO ()
run parser fileName = do
  res <- parser fileName
  case res of
    Left err ->
      putStrLn err
    Right prog@(Program defs goal) -> do
      let prologFile = prologExt fileName
      Transformer.MkToProlog.transform prologFile defs
      prolog <- readFile prologFile
      putStrLn $ printf "\nProlog'ed:\n\n%s\n" prolog
      putStrLn $ printf "\nGoal:\n\n%s\n" (show goal)
      putStrLn $ printf "\nNew Syntax:\n\n%s\n" (PP.prettyString prog)