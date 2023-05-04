{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty.Bench
import Prelude hiding (succ)
import Program.Num
import Program (Program(Program))
import qualified Syntax as K

import Eval (run)

import qualified FunConversion.Trans as TR
import qualified FunConversion.Syntax as S

import Control.Monad (msum, guard)
import Stream

$(return $ S.embedProg "mulo" $ TR.transMultiMode mulo [("mulo", [0, 1]), ("mulo", [1, 2])])

natToTerm :: Int -> K.Term a
natToTerm 0 = zero
natToTerm n = succ $ natToTerm (n - 1)

natToFTerm :: Int -> Term
natToFTerm 0 = O
natToFTerm n = S $ natToFTerm (n - 1)

red = flip seq ()

eval :: (a -> b) -> a -> ()
eval f = red . f

eval2 :: (a -> b -> Stream b) -> (a, b) -> ()
eval2 f = eval $ \(x,y) -> f x y

benchSet a b = let c = a * b in bgroup (show a ++ " * " ++ show b ++ " = " ++ show c) [
  bench "iio-rel" $ nf (eval run) (Program mulo (K.fresh ["c"] $ K.Invoke "mulo" [natToTerm a, natToTerm b, K.V "c"])),
  bench "iio-fun" $ nf (eval2 muloIIO) (natToFTerm a, natToFTerm b),
  bench "oii-rel" $ nf (eval run) (Program mulo (K.fresh ["a"] $ K.Invoke "mulo" [K.V "a", natToTerm b, natToTerm c])),
  bench "oii-fun" $ nf (eval2 muloOII) (natToFTerm b, natToFTerm c)
  ]

main :: IO ()
main = defaultMain
  [ 
    benchSet 1 1
    , benchSet 10 0
    , benchSet 0 10
    , benchSet 10 10
    -- , benchSet 100 100
    -- , benchSet 1000 1000
  ]