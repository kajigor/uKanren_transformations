-- {-# LANGUAGE TemplateHaskell #-}
import Test.Tasty.Bench
import Prelude hiding (succ)
import Program.Num
import Program (Program(Program))
import Data.List (subsequences)
import Data.Maybe (maybeToList)
import Data.Map.Strict (toList)
import qualified Syntax as K

import qualified Control.DeepSeq as DS

import Eval (run)

import qualified FunConversion.Trans as TR
import qualified FunConversion.Syntax as S

import Control.Monad (msum, guard, MonadPlus)
import Stream
import qualified FunConversion.SemiStream as Semi
import qualified FunConversion.DetStream as Det
import Subst (Subst, getSubst, showSubst')

-- $(return $ S.embedProg "mulo" $ TR.transMultiMode mulo [("mulo", [0, 1]), ("mulo", [1, 2])])

-- peanify :: Integer -> Term a
-- peanify n | n <= 0 = zero
-- peanify n          = succ (peanify $ n - 1)

-- zero :: Term a
-- zero = C "O" []

-- succ :: Term a -> Term a
-- succ x = C "S" [x]

instance DS.NFData Subst where
  rnf = DS.rnf . toList . getSubst

instance (DS.NFData a) => DS.NFData (K.Term a) where
  rnf (K.V x) = DS.rnf x
  rnf (K.C n xs) = DS.rnf n `seq` DS.rnf xs

data Term
    = O
    | S Term
    deriving (Show, Eq)

muloIIO x0 x1 = msum [
    do {
      guard (x0 == O);
      let {x2 = O};
      return x2
    },
    do {
      x3 <- case x0 of {S y3 -> return y3; _ -> mzero};
      x4 <- muloIIO x3 x1;
      x2 <- addoIIO x1 x4;
      return x2
    }
  ]
addoIIO x0 x1 = msum [
    do {
      guard (x0 == O);
      let {x2 = x1};
      return x2
    },
    do {
      x3 <- case x0 of {S y3 -> return y3; _ -> mzero};
      x4 <- addoIIO x3 x1;
      let {x2 = S x4};
      return x2
    }
  ]

add :: Term -> Term -> Term
add O y = y
add (S x') y = S (add x' y)

mul :: Term -> Term -> Term
mul O _ = O
mul (S x') y = add (mul x' y) y

instance DS.NFData Term where
  rnf O = ()
  rnf (S n) = DS.rnf n

natToFTerm :: Integer -> Term
natToFTerm 0 = O
natToFTerm n = S $ natToFTerm (n - 1)

--natGen :: (MonadPlus m) => m Term
--natGen = return O <|> (S <$> natGen)

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x,y,z) -> f x y z


benchFull a b = let c = a * b in bgroup ("full;" ++ show a ++ ";" ++ show b ++ ";" ++ show c) [
  bench "iio-rel" $ nf (eval (takeS 1) run) (Program addo (K.fresh ["c"] $ K.Invoke "addo" [peanify a, peanify b, K.V "c"])),
  bench "iio-stream" $ nf (eval2 (takeS 1) muloIIO) (natToFTerm a, natToFTerm b),
  bench "iio-maybe" $ nf (eval2 maybeToList muloIIO) (natToFTerm a, natToFTerm b),
  bench "iio-list" $ nf (eval2 id muloIIO) (natToFTerm a, natToFTerm b)
  -- bench "iio-semi" $ nf (eval2 (Semi.takeS 1) addoIIOSemi) (natToFTerm a, natToFTerm b),
  -- bench "iio-det" $ nf (eval2 (Det.takeS 1) addoIIODet) (natToFTerm a, natToFTerm b)
  ]

main :: IO ()
main = defaultMain
  [ 
    -- benchSet 1 1
    -- , benchSet 10 0
    -- , benchSet 0 10
    -- , benchSet 10 10
    benchFull 20 20
    -- , benchSet 1000 1000
  ]