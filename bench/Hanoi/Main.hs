{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
import qualified Hanoi_offline
import qualified Hanoi_online

eval0 :: MonadPlus m => (m r -> [r]) -> m r -> a -> [r]
eval0 listify f _ = listify f

eval :: MonadPlus m => (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval21 :: MonadPlus m => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 ->
  x14 -> x15 -> x16 -> x17 -> x18 -> x19 -> x20 -> x21 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13,
                                                                    x14, x15, x16, x17, x18, x19, x20, x21) -> [r]
eval21 listify f = eval listify $ \(x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14, x15, x16, x17, x18, x19, x20, x21) -> f x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16 x17 x18 x19 x20 x21

listGen :: (MonadPlus m) => m Term
listGen = do
  x <- natGen
  return Nil <|> (Cons x <$> listGen)

lst = [Pair One Thr,
       Pair One Two,
       Pair Thr Two,
       Pair One Thr,
       Pair Two One,
       Pair Two Thr,
       Pair One Thr]

makeTerm x | x == [] = Nil
           | otherwise = Cons (head x) $ makeTerm $ tail x

main = defaultMain
  [
    bgroup "Hanoi3"
     [
        bench "offlineI"    $ nf (eval (takeS 1) Hanoi_offline.checkI) (makeTerm lst)
      , bench "onlineI"     $ nf (eval (takeS 1) Hanoi_online.checkI) (makeTerm lst)
      , bench "offlineO"    $ nf (eval0 (takeS 10) Hanoi_offline.checkO) ()
      , bench "onlineO"     $ nf (eval21 (takeS 10) Hanoi_online.checkO) (numGen, numGen, numGen, numGen, numGen, numGen, numGen,
                                                                      numGen, numGen, numGen, numGen, numGen, numGen, numGen,
                                                                      numGen, numGen, numGen, numGen, numGen, numGen, numGen)
     ]
--    ,bgroup "SortGen"
--    [
--        bench "offlineGen"  $ nf (eval26 (takeS 1) double_appendoIIIOnline) $ natGen23 a b c
--      , bench "onlineGen"   $ nf (eval7 (takeS 1) sortoOOnline) genSort -- failing
--    ]
  ]

--main =
--  print $ (takeS 1) $ Hanoi_online.checkO numGen numGen numGen numGen numGen numGen numGen
--                                          numGen numGen numGen numGen numGen numGen numGen
--                                          numGen numGen numGen numGen numGen numGen numGen
--  print $ (takeS 10) $ Hanoi_offline.checkO
--  print (makeTerm lst)
--  print $ (takeS 1) $ Hanoi_offline.checkI $ makeTerm lst


