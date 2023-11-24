module Test.FunConversion.Det where

import Program.Num
import Program (Program(Program))
import Test.HUnit (assertBool, assertFailure, assertEqual)
import qualified FunConversion.DetMode as Det
import qualified Mode.NormSyntax as N
import qualified Mode.Toplevel as Mode
import Mode.Inst
import Def
import qualified Syntax as S
import FunConversion.Updated.Utils

computeDets :: [Def S.G S.X] -> [(String, [Int])] -> IO Det.DetMap
computeDets defs modes = case Mode.topLevelManyModes defs modes of
    Right v -> return $ Det.checkDefs $ Det.detcheck' v
    Left e -> assertFailure e

assertDet :: Det.DetMap -> Det.DefIdentifier -> Bool -> IO ()
assertDet dets def@(Det.DId (name, modes)) expected = assertEqual ("Determinism check: " ++ name ++ " # " ++ show modes) expected (Det.isDet def dets)

groundsToModes' :: Def a b -> [Int] -> [Mode]
groundsToModes' def grounds = let n = length (getArgs def) in [if i `elem` grounds then In else Out | i <- [0..(n-1)]]

groundsToModes :: [Def a b] -> String -> [Int] -> IO [Mode]
groundsToModes defs name grounds = do
    def <- findDef defs
    return $ groundsToModes' def grounds
    where 
        findDef [] = assertFailure $ "Def " ++ name ++ " not found."
        findDef (d:ds) | getName d == name = return d
                       | otherwise = findDef ds

checkDets :: [Def S.G S.X] -> [(String, [Int], Bool)] -> IO ()
checkDets defs modes = do
    dets <- computeDets defs ((\(n, m, _) -> (n, m)) <$> modes)
    mapM_ (\(n, g, d) -> groundsToModes defs n g >>= \m -> assertDet dets (Det.DId (n, m)) d) modes

unit_AddoDeterminism :: IO ()
unit_AddoDeterminism = checkDets addo 
    [ ("addo", [       ], False) 
    , ("addo", [0      ], False)
    , ("addo", [   1   ], False)
    , ("addo", [      2], False)
    , ("addo", [0, 1   ], True)
    -- , ("addo", [   1, 2], True) -- not detected by current algorithm
    , ("addo", [0,    2], True)
    , ("addo", [0, 1, 2], True) 
    ]

unit_MuloDeterminism :: IO ()
unit_MuloDeterminism = checkDets mulo 
    [ ("mulo", [       ], False) 
    , ("mulo", [0      ], False)
    , ("mulo", [   1   ], False)
    , ("mulo", [      2], False)
    , ("mulo", [0, 1   ], True)
    , ("mulo", [   1, 2], False)
    , ("mulo", [0,    2], False)
    , ("mulo", [0, 1, 2], True) 
    ]