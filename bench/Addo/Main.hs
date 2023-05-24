{-# LANGUAGE TemplateHaskell #-}
import Test.Tasty.Bench
import Prelude hiding (succ)
import AddoProg
import Program (Program(Program))
import Data.List (subsequences)
import Data.Maybe (maybeToList)
import Data.Map.Strict (toList)
import qualified Syntax as K

import qualified Control.DeepSeq as DS

import Eval (run)

import qualified FunConversion.Trans as TR
import qualified FunConversion.Syntax as S

import Control.Monad (msum, guard)
import Stream
import Subst (Subst, getSubst, showSubst')


instance DS.NFData Subst where
  rnf = DS.rnf . toList . getSubst

instance (DS.NFData a) => DS.NFData (K.Term a) where
  rnf (K.V x) = DS.rnf x
  rnf (K.C n xs) = DS.rnf n `seq` DS.rnf xs

$(return $ S.embedProg "addo" $ TR.transMultiMode addo [("addo", x) | x <- subsequences [0..2]])

instance DS.NFData Term where
  rnf O = ()
  rnf (S n) = DS.rnf n

natToFTerm :: Integer -> Term
natToFTerm 0 = O
natToFTerm n = S $ natToFTerm (n - 1)

natGen = return O <|> (S <$> natGen)

eval :: Int -> (a -> Stream b) -> a -> [b]
eval n f = takeS n . f

evalMaybe :: (a -> Maybe b) -> a -> [b]
evalMaybe f = maybeToList . f

eval2 :: Int -> (a -> b -> Stream c) -> (a, b) -> [c]
eval2 n f = eval n $ \(x,y) -> f x y

evalMaybe2 :: (a -> b -> Maybe c) -> (a, b) -> [c]
evalMaybe2 f = evalMaybe $ \(x,y) -> f x y

eval3 :: Int -> (a -> b -> c -> Stream d) -> (a, b, c) -> [d]
eval3 n f = eval n $ \(x,y,z) -> f x y z

evalMaybe3 :: (a -> b -> c -> Maybe d) -> (a, b, c) -> [d]
evalMaybe3 f = evalMaybe $ \(x,y,z) -> f x y z

benchFullOut n = bgroup ("out;" ++ show n) [
  bench "ooo-rel" $ nf (eval n run) (Program addo (K.fresh ["a", "b", "c"] $ K.Invoke "addo" [K.V "a", K.V "b", K.V "c"])),
  bench "ooo-fun" $ nf (eval n addoOOO) natGen
  ]

benchPairs n x = bgroup ("pairs;" ++ show n ++ ";" ++ show x) [
  bench "ioo-rel" $ nf (eval n run) (Program addo (K.fresh ["b", "c"] $ K.Invoke "addo" [peanify x, K.V "b", K.V "c"])),
  bench "ioo-rel-ground" $ nf (eval n run) (Program addoGround (K.fresh ["b", "c"] $ K.Invoke "addoGround" [peanify x, K.V "b", K.V "c"])),
  bench "ioo-fun" $ nf (eval2 n addoIOO) (natToFTerm x, natGen),
  bench "oio-rel" $ nf (eval n run) (Program addo (K.fresh ["a", "c"] $ K.Invoke "addo" [K.V "a", peanify x, K.V "c"])),
  bench "oio-fun" $ nf (eval n addoOIO) (natToFTerm x),
  bench "ooi-rel" $ nf (eval n run) (Program addo (K.fresh ["a", "b"] $ K.Invoke "addo" [K.V "a", K.V "b", peanify x])),
  bench "ooi-fun" $ nf (eval n addoOOI) (natToFTerm x)
  ]

benchFull a b = let c = a + b in bgroup ("full;" ++ show a ++ ";" ++ show b ++ ";" ++ show c) [
  bench "iio-rel" $ nf (eval 1 run) (Program addo (K.fresh ["c"] $ K.Invoke "addo" [peanify a, peanify b, K.V "c"])),
  bench "iio-fun" $ nf (eval2 1 addoIIO) (natToFTerm a, natToFTerm b),
  bench "iio-det" $ nf (evalMaybe2 addoIIO) (natToFTerm a, natToFTerm b),
  bench "ioi-rel" $ nf (eval 1 run) (Program addo (K.fresh ["b"] $ K.Invoke "addo" [peanify a, K.V "b", peanify c])),
  bench "ioi-fun" $ nf (eval2 1 addoIOI) (natToFTerm a, natToFTerm c),
  bench "ioi-det" $ nf (evalMaybe2 addoIOI) (natToFTerm a, natToFTerm c),
  bench "oii-rel" $ nf (eval 1 run) (Program addo (K.fresh ["a"] $ K.Invoke "addo" [K.V "a", peanify b, peanify c])),
  bench "oii-fun" $ nf (eval2 1 addoOII) (natToFTerm b, natToFTerm c),
  bench "oii-det" $ nf (evalMaybe2 addoOII) (natToFTerm b, natToFTerm b),
  bench "iii-rel" $ nf (eval 1 run) (Program addo (K.Invoke "addo" [peanify a, peanify b, peanify c])),
  bench "iii-fun" $ nf (eval3 1 addoIII) (natToFTerm a, natToFTerm b, natToFTerm c),
  bench "iii-det" $ nf (evalMaybe3 addoIII) (natToFTerm a, natToFTerm b, natToFTerm c)
  ]

main :: IO ()
main = defaultMain $ concat [
  -- [benchFullOut n | n <- [10,20..100]],
  -- [benchPairs n x | x <- [0,5..20], n <- [10,30..100]]
  [benchFull a b | a <- [0,5..100], b <- [0,20]]
  ]