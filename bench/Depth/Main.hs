{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

data Term =
    Cons Term Term |
    O |
    S Term |
    Nil
   deriving (Show, Eq, Generic, DS.NFData)

depthIOff x0 = msum [do {x1 <- case x0 of
                            {S y1 -> return y1; _ -> mzero};
                      x2 <- case x1 of
                            {S y2 -> return y2; _ -> mzero};
                      x3 <- case x2 of
                            {S y3 -> return y3; _ -> mzero};
                      x4 <- case x3 of
                            {S y4 -> return y4; _ -> mzero};
                      x5 <- case x4 of
                            {S y5 -> return y5; _ -> mzero};
                      x6 <- case x5 of
                            {S y6 -> return y6; _ -> mzero};
                      x7 <- case x6 of
                            {S y7 -> return y7; _ -> mzero};
                      guard (x7 == O);
                      return ()},
                  do {x8 <- case x0 of
                            {S y8 -> return y8; _ -> mzero};
                      x9 <- case x8 of
                            {S y9 -> return y9; _ -> mzero};
                      x10 <- case x9 of
                             {S y10 -> return y10; _ -> mzero};
                      x11 <- case x10 of
                             {S y11 -> return y11; _ -> mzero};
                      x12 <- case x11 of
                             {S y12 -> return y12; _ -> mzero};
                      x13 <- case x12 of
                             {S y13 -> return y13; _ -> mzero};
                      x14 <- case x13 of
                             {S y14 -> return y14; _ -> mzero};
                      x15 <- case x14 of
                             {S y15 -> return y15; _ -> mzero};
                      x16 <- case x15 of
                             {S y16 -> return y16; _ -> mzero};
                      guard (x16 == O);
                      return ()},
                  do {x17 <- case x0 of
                             {S y17 -> return y17; _ -> mzero};
                      x18 <- case x17 of
                             {S y18 -> return y18; _ -> mzero};
                      x19 <- case x18 of
                             {S y19 -> return y19; _ -> mzero};
                      x20 <- case x19 of
                             {S y20 -> return y20; _ -> mzero};
                      x21 <- case x20 of
                             {S y21 -> return y21; _ -> mzero};
                      x22 <- case x21 of
                             {S y22 -> return y22; _ -> mzero};
                      x23 <- case x22 of
                             {S y23 -> return y23; _ -> mzero};
                      x24 <- case x23 of
                             {S y24 -> return y24; _ -> mzero};
                      x25 <- case x24 of
                             {S y25 -> return y25; _ -> mzero};
                      x26 <- case x25 of
                             {S y26 -> return y26; _ -> mzero};
                      x27 <- case x26 of
                             {S y27 -> return y27; _ -> mzero};
                      x28 <- case x27 of
                             {S y28 -> return y28; _ -> mzero};
                      guard (x28 == O);
                      return ()},
                  do {x29 <- case x0 of
                             {S y29 -> return y29; _ -> mzero};
                      x30 <- case x29 of
                             {S y30 -> return y30; _ -> mzero};
                      x31 <- case x30 of
                             {S y31 -> return y31; _ -> mzero};
                      x32 <- case x31 of
                             {S y32 -> return y32; _ -> mzero};
                      x33 <- case x32 of
                             {S y33 -> return y33; _ -> mzero};
                      x34 <- case x33 of
                             {S y34 -> return y34; _ -> mzero};
                      x35 <- case x34 of
                             {S y35 -> return y35; _ -> mzero};
                      x36 <- case x35 of
                             {S y36 -> return y36; _ -> mzero};
                      x37 <- case x36 of
                             {S y37 -> return y37; _ -> mzero};
                      x38 <- case x37 of
                             {S y38 -> return y38; _ -> mzero};
                      x39 <- case x38 of
                             {S y39 -> return y39; _ -> mzero};
                      x40 <- case x39 of
                             {S y40 -> return y40; _ -> mzero};
                      guard (x40 == O);
                      return ()}]

depthOOff :: MonadPlus m => m Term
depthOOff = msum [do {let {x7 = O};
                   let {x6 = S x7};
                   let {x5 = S x6};
                   let {x4 = S x5};
                   let {x3 = S x4};
                   let {x2 = S x3};
                   let {x1 = S x2};
                   let {x0 = S x1};
                   return x0},
               do {let {x16 = O};
                   let {x15 = S x16};
                   let {x14 = S x15};
                   let {x13 = S x14};
                   let {x12 = S x13};
                   let {x11 = S x12};
                   let {x10 = S x11};
                   let {x9 = S x10};
                   let {x8 = S x9};
                   let {x0 = S x8};
                   return x0},
               do {let {x28 = O};
                   let {x27 = S x28};
                   let {x26 = S x27};
                   let {x25 = S x26};
                   let {x24 = S x25};
                   let {x23 = S x24};
                   let {x22 = S x23};
                   let {x21 = S x22};
                   let {x20 = S x21};
                   let {x19 = S x20};
                   let {x18 = S x19};
                   let {x17 = S x18};
                   let {x0 = S x17};
                   return x0},
               do {let {x40 = O};
                   let {x39 = S x40};
                   let {x38 = S x39};
                   let {x37 = S x38};
                   let {x36 = S x37};
                   let {x35 = S x36};
                   let {x34 = S x35};
                   let {x33 = S x34};
                   let {x32 = S x33};
                   let {x31 = S x32};
                   let {x30 = S x31};
                   let {x29 = S x30};
                   let {x0 = S x29};
                   return x0}]

depthIOnl x0 gen_prog_clauseDepth3O_x16 = msum [do {depth0I x0;
                                                 x1 <- depth1O;
                                                 return ()},
                                             do {depth2I x0;
                                                 x2 <- prog_clauseDepthO gen_prog_clauseDepth3O_x16;
                                                 return ()}]
depthOOnl gen_depth0O_x2 gen_depth2O_x8 gen_prog_clauseDepth3O_x16 = msum [do {x0 <- depth0O gen_depth0O_x2;
                                                                            x1 <- depth1O;
                                                                            return x0},
                                                                        do {x0 <- depth2O gen_depth2O_x8;
                                                                            x2 <- prog_clauseDepthO gen_prog_clauseDepth3O_x16;
                                                                            return x0}]
depth0I x0 = msum [do {x2 <- case x0 of
                             {S y2 -> return y2; _ -> mzero};
                       x3 <- case x2 of
                             {S y3 -> return y3; _ -> mzero};
                       x4 <- case x3 of
                             {S y4 -> return y4; _ -> mzero};
                       x5 <- case x4 of
                             {S y5 -> return y5; _ -> mzero};
                       x6 <- case x5 of
                             {S y6 -> return y6; _ -> mzero};
                       x7 <- case x6 of
                             {S y7 -> return y7; _ -> mzero};
                       x1 <- case x7 of
                             {S y1 -> return y1; _ -> mzero};
                       return ()}]
depth0O gen_depth0O_x2 = msum [do {(x0,
                                    x2) <- do {x2 <- gen_depth0O_x2;
                                               let {x0 = S x2};
                                               return (x0, x2)};
                                   x3 <- case x2 of
                                         {S y3 -> return y3; _ -> mzero};
                                   x4 <- case x3 of
                                         {S y4 -> return y4; _ -> mzero};
                                   x5 <- case x4 of
                                         {S y5 -> return y5; _ -> mzero};
                                   x6 <- case x5 of
                                         {S y6 -> return y6; _ -> mzero};
                                   x7 <- case x6 of
                                         {S y7 -> return y7; _ -> mzero};
                                   x1 <- case x7 of
                                         {S y1 -> return y1; _ -> mzero};
                                   return x0}]
depth1O = msum [do {let {x0 = O}; return x0}]
depth2I x0 = msum [do {x8 <- case x0 of
                             {S y8 -> return y8; _ -> mzero};
                       x9 <- case x8 of
                             {S y9 -> return y9; _ -> mzero};
                       x10 <- case x9 of
                              {S y10 -> return y10; _ -> mzero};
                       x11 <- case x10 of
                              {S y11 -> return y11; _ -> mzero};
                       x12 <- case x11 of
                              {S y12 -> return y12; _ -> mzero};
                       x13 <- case x12 of
                              {S y13 -> return y13; _ -> mzero};
                       x14 <- case x13 of
                              {S y14 -> return y14; _ -> mzero};
                       x15 <- case x14 of
                              {S y15 -> return y15; _ -> mzero};
                       x2 <- case x15 of
                             {S y2 -> return y2; _ -> mzero};
                       return ()}]
depth2O gen_depth2O_x8 = msum [do {(x0,
                                    x8) <- do {x8 <- gen_depth2O_x8;
                                               let {x0 = S x8};
                                               return (x0, x8)};
                                   x9 <- case x8 of
                                         {S y9 -> return y9; _ -> mzero};
                                   x10 <- case x9 of
                                          {S y10 -> return y10; _ -> mzero};
                                   x11 <- case x10 of
                                          {S y11 -> return y11; _ -> mzero};
                                   x12 <- case x11 of
                                          {S y12 -> return y12; _ -> mzero};
                                   x13 <- case x12 of
                                          {S y13 -> return y13; _ -> mzero};
                                   x14 <- case x13 of
                                          {S y14 -> return y14; _ -> mzero};
                                   x15 <- case x14 of
                                          {S y15 -> return y15; _ -> mzero};
                                   x2 <- case x15 of
                                         {S y2 -> return y2; _ -> mzero};
                                   return x0}]
prog_clauseDepthO gen_prog_clauseDepth3O_x16 = msum [do {x0 <- depth1O;
                                                         return x0},
                                                     do {x0 <- prog_clauseDepth3O gen_prog_clauseDepth3O_x16;
                                                         x1 <- prog_clauseDepth1O;
                                                         return x0}]
prog_clauseDepth1O = msum [do {x0 <- depth1O; return x0},
                           do {x1 <- prog_clauseDepth2O; let {x0 = S x1}; return x0}]
prog_clauseDepth2O = msum [do {x0 <- depth1O; return x0}]
prog_clauseDepth3O gen_prog_clauseDepth3O_x16 = msum [do {(x0,
                                                           x16) <- do {x16 <- gen_prog_clauseDepth3O_x16;
                                                                       let {x0 = S x16};
                                                                       return (x0, x16)};
                                                          x17 <- case x16 of
                                                                 {S y17 -> return y17; _ -> mzero};
                                                          x1 <- case x17 of
                                                                {S y1 -> return y1; _ -> mzero};
                                                          return x0}]

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval3 :: (m r -> [r]) -> (a -> b -> c -> m r) -> (a, b, c) -> [r]
eval3 listify f = eval listify $ \(x,y,z) -> f x y z

eval6N :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> m r) -> (a, x1, x2, x3, x4, x5, x6) -> [r]
eval6N listify f = eval listify  $ \(b, y1, y2, y3, y4, y5, y6) -> f b y1 y2 y3 y4 y5 y6

eval7 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> m r) -> (x1, x2, x3, x4, x5, x6, x7) -> [r]
eval7 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7) -> f y1 y2 y3 y4 y5 y6 y7

natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

natRight = S (S (S (S (S (S (S O))))))
natWrong = S (S (S (S (S (S (S (S O)))))))


depthOOff1 :: MonadPlus m => p -> m Term
depthOOff1 x = do
  depthOOff

main = defaultMain
  [
    bgroup "DepthRun"
     [
        bench "offline1"    $ nf (eval (takeS 1) depthIOff) natRight
      , bench "online1"     $ nf (eval2 (takeS 1) depthIOnl) (natRight, natGen)
      , bench "offline2"    $ nf (eval (takeS 1) depthIOff) natWrong
      , bench "online2"     $ nf (eval2 (takeS 1) depthIOnl) (natWrong, natGen)
     ],
    bgroup "DepthGen"
    [
        bench "offlineGen"  $ nf (eval (takeS 1) depthOOff1) () 
      , bench "onlineGen"   $ nf (eval3 (takeS 1) depthOOnl) (natGen, natGen, natGen) 
    ]
  ]