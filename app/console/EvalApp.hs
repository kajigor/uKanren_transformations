module EvalApp where

import           Eval
import           Prelude hiding (succ)
import           Program
import           Stream
import qualified Subst
import           Syntax

reify :: Subst.Subst -> Ts -> Term S
reify s x@(V v) =
  case Subst.lookup v s of
    Nothing -> x
    Just t  -> reify s t
reify s (C n ts) = C n $ map (reify s) ts

toplevel :: Int -> (Term S -> String) -> Program G X -> [String]
toplevel n printer program =
  let stream = run program in
  map (\s -> printer $ reify s (V 0)) $ takeS n stream

addVar :: Program G String -> Program G String
addVar p@(Program defs goal) =
  let (vars, g) = topLevelFreshVars goal in
  if length vars <= 1
  then p
  else
    let newVar = concat vars in
    Program defs (fresh (newVar:vars) (V newVar === C "tuple" (map V vars)) &&& g)

runWithParser :: (t -> IO (Either String (Program G String))) -> t -> Int -> IO ()
runWithParser parser inputFile num = do
  res <- parser inputFile
  case res of
    Left err ->
      putStrLn err
    Right p ->
      mapM_ putStrLn (toplevel num show (addVar p))


