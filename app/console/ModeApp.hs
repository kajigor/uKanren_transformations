module ModeApp where

import Syntax
import Program
import Mode.Toplevel
import qualified Mode.NormSyntax as N
import qualified Mode.Pretty as P
import qualified FunConversion.DetMode as D

runWithParser :: (t -> IO (Either String (Program G String))) -> t -> String -> [Int] -> IO ()
runWithParser parser inputFile relName inputs = do
  program <- parser inputFile
  case program of
    Left err ->
      putStrLn err
    Right p ->
      case topLevelWithDefaultCall p relName inputs of
        Right program -> do
          putStrLn $ P.prettyString (N.back program)
          putStrLn "------------"
          let det = D.detcheck program
          putStrLn $ P.prettyString (N.back det)
          print $ D.checkDefs (getDefs det)
        Left err ->
          putStrLn $ "Mode analysis failed:\n" ++ show err
