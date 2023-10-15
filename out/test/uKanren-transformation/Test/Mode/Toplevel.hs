{-# LANGUAGE TupleSections #-}
module Test.Mode.Toplevel where

import           Data.List       (subsequences)
import qualified Mode.NormSyntax as N
import           Mode.Pretty
import           Mode.Toplevel
import           Program.Num

unit_manyDefs = do
    let args = subsequences [0,1,2]
    run addo "addo" args
    run mulo "mulo" [[0,2], [0, 1]]

  where
    run defs name args = do
      putStrLn "\n=================================="
      case topLevelManyModes defs (map (name,) args) of
        Right r -> mapM_ prettyPrint r
        Left err -> putStrLn err
      putStrLn "==================================\n"

    prettyPrint def = do
      putStrLn "\n----------------------------------"
      putStrLn $ prettyString (N.backDef def)
      putStrLn "----------------------------------\n"
