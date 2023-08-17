module DepApp where

import qualified Syntax as S
import qualified Hypergraph.Hypergraph as H
import qualified Hypergraph.Converter as H
import Program

runWithParser :: (t -> IO (Either String (Program S.G String))) -> t -> IO ()
runWithParser parser fileName = do
  res <- parser fileName
  case res of
    Left err ->
      putStrLn err
    Right (Program defs goal) -> do
      let program = H.defsToProgram defs
      let depmap = H.alphaInterp program
      putStrLn $ H.showDependences depmap