{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty.Bench
import Prelude hiding (succ)
import PropProg (evalo)
import Data.List (subsequences)
import Program (Program(Program))
import qualified Syntax as K

import Eval (run)

import qualified FunConversion.Trans as TR
import qualified FunConversion.Syntax as S

import Control.Monad (msum, guard)
import Stream

import qualified Parser.Parser as Parser


$(return $ S.embedProg "evalo" $ TR.transMultiMode evalo [("evalo", x) | x <- subsequences [0..2]])

fmToRel Zero = K.C"Zero" []
fmToRel (Var t) = K.C"Var" [fmToRel t]
fmToRel Trueo = K.C"Trueo" []
fmToRel Falso = K.C"Falso" []
fmToRel (Succ t) = K.C"Succ" [fmToRel t]
fmToRel (Neg t) = K.C"Neg" [fmToRel t]
fmToRel (Disj a b) = K.C"Disj" [fmToRel a, fmToRel b]
fmToRel (Cons a b) = K.C"Cons" [fmToRel a, fmToRel b]
fmToRel (Conj a b) = K.C"Conj" [fmToRel a, fmToRel b]

lstToRel :: [Bool] -> K.Term a
lstToRel = fmToRel . lstToFun

lstToFun :: [Bool] -> Term
lstToFun [] = Zero
lstToFun (x:xs) = Cons (boolToFun x) (lstToFun xs)

boolToFun :: Bool -> Term
boolToFun True = Trueo
boolToFun False = Falso

red = flip seq ()

eval :: (a -> Stream b) -> Int -> a -> ()
eval f n = red . takeS n . f

eval2 :: (a -> b -> Stream c) -> Int -> (a, b) -> ()
eval2 f n = eval (\(x,y) -> f x y) n

eval3 :: (a -> b -> c -> Stream d) -> Int -> (a, b, c) -> ()
eval3 f n = eval (\(x,y,z) -> f x y z) n

eval4 :: (a -> b -> c -> d -> Stream e) -> Int -> (a, b, c, d) -> ()
eval4 f n = eval (\(x,y,z, w) -> f x y z w) n

benchSet state r = bgroup ("ioi") [
  bench "ooo-rel" $ nf (eval run 1000) (Program evalo (K.fresh ["st", "fm", "u"] $ K.Invoke "evalo" [K.V "st", K.V "fm", K.V "u"])),
  bench "ooo-fun" $ nf (eval4 evaloOOO 1000) (undefined, undefined, undefined, undefined)
  -- bench "ioi-rel" $ nf (eval run) (Program evalo (K.fresh ["b"] $ K.Invoke "evalo" [lstToRel state, K.V "b", fmToRel (boolToFun r)])),
  -- bench "ioi-fun" $ nf (eval2 evaloIOI) (lstToFun state, boolToFun r)
  -- bench "oii-rel" $ nf (eval run) (Program mulo (K.fresh ["a"] $ K.Invoke "mulo" [K.V "a", natToTerm b, natToTerm c])),
  -- bench "oii-fun" $ nf (eval2 muloOII) (natToFTerm b, natToFTerm c)
  ]

main :: IO ()
main = defaultMain
  [ 
    -- benchSet 1 1
    -- , benchSet 10 0
    -- , benchSet 0 10
    -- , benchSet 10 10
    benchSet [True, False, True] True
    -- , benchSet 1000 1000
  ]