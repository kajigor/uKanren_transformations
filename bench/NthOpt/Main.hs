{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)


data Term
    = Cons Term Term
    | Nil
    | None
    | Some Term
    | Zero
    | Succ Term
    deriving (Show, Eq, Generic, DS.NFData)

nthOptIIOffline x0 x1 = msum [do {guard (x1 == None);
                           guard (x0 == Nil);
                           return ()},
                       do {guard (x1 == None);
                           (x2, x3) <- case x0 of
                                       {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                           guard (x3 == Nil);
                           return ()},
                       do {guard (x1 == None);
                           (x2, x4) <- case x0 of
                                       {Cons y2 y4 -> return (y2, y4); _ -> mzero};
                           (x3, x5) <- case x4 of
                                       {Cons y3 y5 -> return (y3, y5); _ -> mzero};
                           guard (x5 == Nil);
                           return ()},
                       do {guard (x1 == None);
                           (x2, x6) <- case x0 of
                                       {Cons y2 y6 -> return (y2, y6); _ -> mzero};
                           (x3, x7) <- case x6 of
                                       {Cons y3 y7 -> return (y3, y7); _ -> mzero};
                           (x4, x8) <- case x7 of
                                       {Cons y4 y8 -> return (y4, y8); _ -> mzero};
                           guard (x8 == Nil);
                           return ()},
                       do {x5 <- case x1 of
                                 {Some y5 -> return y5; _ -> mzero};
                           (x2, x9) <- case x0 of
                                       {Cons y2 y9 -> return (y2, y9); _ -> mzero};
                           (x3, x10) <- case x9 of
                                        {Cons y3 y10 -> return (y3, y10); _ -> mzero};
                           (x4, x11) <- case x10 of
                                        {Cons y4 y11 -> return (y4, y11); _ -> mzero};
                           x6 <- case x11 of
                                 {Cons y5 y6 -> do {guard (x5 == y5); return y6}; _ -> mzero};
                           return ()}]
nthOptIOOffline x0 = msum [do {let {x1 = None};
                        guard (x0 == Nil);
                        return x1},
                    do {let {x1 = None};
                        (x2, x3) <- case x0 of
                                    {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                        guard (x3 == Nil);
                        return x1},
                    do {let {x1 = None};
                        (x2, x4) <- case x0 of
                                    {Cons y2 y4 -> return (y2, y4); _ -> mzero};
                        (x3, x5) <- case x4 of
                                    {Cons y3 y5 -> return (y3, y5); _ -> mzero};
                        guard (x5 == Nil);
                        return x1},
                    do {let {x1 = None};
                        (x2, x6) <- case x0 of
                                    {Cons y2 y6 -> return (y2, y6); _ -> mzero};
                        (x3, x7) <- case x6 of
                                    {Cons y3 y7 -> return (y3, y7); _ -> mzero};
                        (x4, x8) <- case x7 of
                                    {Cons y4 y8 -> return (y4, y8); _ -> mzero};
                        guard (x8 == Nil);
                        return x1},
                    do {(x2, x9) <- case x0 of
                                    {Cons y2 y9 -> return (y2, y9); _ -> mzero};
                        (x3, x10) <- case x9 of
                                     {Cons y3 y10 -> return (y3, y10); _ -> mzero};
                        (x4, x11) <- case x10 of
                                     {Cons y4 y11 -> return (y4, y11); _ -> mzero};
                        (x5, x6) <- case x11 of
                                    {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                        let {x1 = Some x5};
                        return x1}]
nthOptOIOffline x1 gen_nthOptOI_x2 gen_nthOptOI_x3 gen_nthOptOI_x4 gen_nthOptOI_x6 = msum [do {guard (x1 == None);
                                                                                        let {x0 = Nil};
                                                                                        return x0},
                                                                                    do {guard (x1 == None);
                                                                                        let {x3 = Nil};
                                                                                        (x0,
                                                                                         x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                    let {x0 = Cons x2 x3};
                                                                                                    return (x0,
                                                                                                            x2)};
                                                                                        return x0},
                                                                                    do {guard (x1 == None);
                                                                                        let {x5 = Nil};
                                                                                        (x4,
                                                                                         x3) <- do {x3 <- gen_nthOptOI_x3;
                                                                                                    let {x4 = Cons x3 x5};
                                                                                                    return (x4,
                                                                                                            x3)};
                                                                                        (x0,
                                                                                         x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                    let {x0 = Cons x2 x4};
                                                                                                    return (x0,
                                                                                                            x2)};
                                                                                        return x0},
                                                                                    do {guard (x1 == None);
                                                                                        let {x8 = Nil};
                                                                                        (x7,
                                                                                         x4) <- do {x4 <- gen_nthOptOI_x4;
                                                                                                    let {x7 = Cons x4 x8};
                                                                                                    return (x7,
                                                                                                            x4)};
                                                                                        (x6,
                                                                                         x3) <- do {x3 <- gen_nthOptOI_x3;
                                                                                                    let {x6 = Cons x3 x7};
                                                                                                    return (x6,
                                                                                                            x3)};
                                                                                        (x0,
                                                                                         x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                    let {x0 = Cons x2 x6};
                                                                                                    return (x0,
                                                                                                            x2)};
                                                                                        return x0},
                                                                                    do {x5 <- case x1 of
                                                                                              {Some y5 -> return y5;
                                                                                               _ -> mzero};
                                                                                        (x11,
                                                                                         x6) <- do {x6 <- gen_nthOptOI_x6;
                                                                                                    let {x11 = Cons x5 x6};
                                                                                                    return (x11,
                                                                                                            x6)};
                                                                                        (x10,
                                                                                         x4) <- do {x4 <- gen_nthOptOI_x4;
                                                                                                    let {x10 = Cons x4 x11};
                                                                                                    return (x10,
                                                                                                            x4)};
                                                                                        (x9,
                                                                                         x3) <- do {x3 <- gen_nthOptOI_x3;
                                                                                                    let {x9 = Cons x3 x10};
                                                                                                    return (x9,
                                                                                                            x3)};
                                                                                        (x0,
                                                                                         x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                    let {x0 = Cons x2 x9};
                                                                                                    return (x0,
                                                                                                            x2)};
                                                                                        return x0}]
nthOptOOOffline gen_nthOptOO_x2 gen_nthOptOO_x3 gen_nthOptOO_x4 gen_nthOptOO_x5 gen_nthOptOO_x9 = msum [do {let {x1 = None};
                                                                                                     let {x0 = Nil};
                                                                                                     return (x0,
                                                                                                             x1)},
                                                                                                 do {let {x1 = None};
                                                                                                     let {x3 = Nil};
                                                                                                     (x0,
                                                                                                      x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                                 let {x0 = Cons x2 x3};
                                                                                                                 return (x0,
                                                                                                                         x2)};
                                                                                                     return (x0,
                                                                                                             x1)},
                                                                                                 do {let {x1 = None};
                                                                                                     let {x5 = Nil};
                                                                                                     (x4,
                                                                                                      x3) <- do {x3 <- gen_nthOptOO_x3;
                                                                                                                 let {x4 = Cons x3 x5};
                                                                                                                 return (x4,
                                                                                                                         x3)};
                                                                                                     (x0,
                                                                                                      x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                                 let {x0 = Cons x2 x4};
                                                                                                                 return (x0,
                                                                                                                         x2)};
                                                                                                     return (x0,
                                                                                                             x1)},
                                                                                                 do {let {x1 = None};
                                                                                                     let {x8 = Nil};
                                                                                                     (x7,
                                                                                                      x4) <- do {x4 <- gen_nthOptOO_x4;
                                                                                                                 let {x7 = Cons x4 x8};
                                                                                                                 return (x7,
                                                                                                                         x4)};
                                                                                                     (x6,
                                                                                                      x3) <- do {x3 <- gen_nthOptOO_x3;
                                                                                                                 let {x6 = Cons x3 x7};
                                                                                                                 return (x6,
                                                                                                                         x3)};
                                                                                                     (x0,
                                                                                                      x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                                 let {x0 = Cons x2 x6};
                                                                                                                 return (x0,
                                                                                                                         x2)};
                                                                                                     return (x0,
                                                                                                             x1)},
                                                                                                 do {(x1,
                                                                                                      x5) <- do {x5 <- gen_nthOptOO_x5;
                                                                                                                 let {x1 = Some x5};
                                                                                                                 return (x1,
                                                                                                                         x5)};
                                                                                                     (x0,
                                                                                                      x2,
                                                                                                      x9) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                                 x9 <- gen_nthOptOO_x9;
                                                                                                                 let {x0 = Cons x2 x9};
                                                                                                                 return (x0,
                                                                                                                         x2,
                                                                                                                         x9)};
                                                                                                     (x3,
                                                                                                      x10) <- case x9 of
                                                                                                              {Cons y3
                                                                                                                    y10 -> return (y3,
                                                                                                                                   y10);
                                                                                                               _ -> mzero};
                                                                                                     (x4,
                                                                                                      x11) <- case x10 of
                                                                                                              {Cons y4
                                                                                                                    y11 -> return (y4,
                                                                                                                                   y11);
                                                                                                               _ -> mzero};
                                                                                                     x6 <- case x11 of
                                                                                                           {Cons y5
                                                                                                                 y6 -> do {guard (x5 == y5);
                                                                                                                           return y6};
                                                                                                            _ -> mzero};
                                                                                                     return (x0,
                                                                                                             x1)}]


nthOptIIOnline x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {nthOpt0II x0 x1;
                                                                                 nthOpt4II x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                                 return ()}]
nthOptIOOnline x0 gen_nthOpt0IO_x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {x1 <- nthOpt0IO x0 gen_nthOpt0IO_x1;
                                                                                               nthOpt4II x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                                               return x1}]
nthOptOIOnline x1 gen_nthOpt0OI_x0 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {x0 <- nthOpt0OI x1 gen_nthOpt0OI_x0;
                                                                                               nthOpt4II x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                                               return x0}]
nthOptOOOnline gen_nthOpt0OO_x0 gen_nthOpt0OO_x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {(x0,
                                                                                                              x1) <- nthOpt0OO gen_nthOpt0OO_x0 gen_nthOpt0OO_x1;
                                                                                                             nthOpt4II x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                                                             return (x0,
                                                                                                                     x1)}]
nthOpt0II x0 x1 = msum [do {guard (x1 == None); return ()},
                        do {guard (x0 == Nil); return ()}]
nthOpt0IO x0 gen_nthOpt0IO_x1 = msum [do {let {x1 = None};
                                          return x1},
                                      do {guard (x0 == Nil); x1 <- gen_nthOpt0IO_x1; return x1}]
nthOpt0OI x1 gen_nthOpt0OI_x0 = msum [do {guard (x1 == None);
                                          x0 <- gen_nthOpt0OI_x0;
                                          return x0},
                                      do {let {x0 = Nil}; return x0}]
nthOpt0OO gen_nthOpt0OO_x0 gen_nthOpt0OO_x1 = msum [do {let {x1 = None};
                                                        x0 <- gen_nthOpt0OO_x0;
                                                        return (x0, x1)},
                                                    do {let {x0 = Nil};
                                                        x1 <- gen_nthOpt0OO_x1;
                                                        return (x0, x1)}]
nthOpt4II x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {(x2,
                                                                                   x3) <- case x0 of
                                                                                          {Cons y2
                                                                                                y3 -> return (y2,
                                                                                                              y3);
                                                                                           _ -> mzero};
                                                                                  return ()},
                                                                              do {x3 <- nthOpt1IO x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                                  return ()}]
nthOpt1IO x0 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {x1 <- nthOpt14IO x0 gen_nthOpt14IO_x1;
                                                                               nthOpt15II x0 x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                               return x1}]
nthOpt14IO x0 gen_nthOpt14IO_x1 = msum [do {let {x1 = Nil};
                                            return x1},
                                        do {guard (x0 == None); x1 <- gen_nthOpt14IO_x1; return x1}]
nthOpt15II x0 x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {(x2,
                                                                  x3) <- case x1 of
                                                                         {Cons y2 y3 -> return (y2,
                                                                                                y3);
                                                                          _ -> mzero};
                                                                 return ()},
                                                             do {x3 <- nthOpt2IO x0 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                 return ()}]
nthOpt2IO x0 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {x1 <- nthOpt25IO x0 gen_nthOpt25IO_x1;
                                                             nthOpt26II x0 x1 gen_nthOpt36IO_x1;
                                                             return x1}]
nthOpt25IO x0 gen_nthOpt25IO_x1 = msum [do {let {x1 = Nil};
                                            return x1},
                                        do {guard (x0 == None); x1 <- gen_nthOpt25IO_x1; return x1}]
nthOpt26II x0 x1 gen_nthOpt36IO_x1 = msum [do {(x2,
                                                x3) <- case x1 of
                                                       {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                               return ()},
                                           do {x3 <- nthOpt3IO x0 gen_nthOpt36IO_x1; return ()}]
nthOpt3IO x0 gen_nthOpt36IO_x1 = msum [do {x1 <- nthOpt36IO x0 gen_nthOpt36IO_x1;
                                           nthOpt37II x0 x1;
                                           return x1}]
nthOpt36IO x0 gen_nthOpt36IO_x1 = msum [do {let {x1 = Nil};
                                            return x1},
                                        do {guard (x0 == None); x1 <- gen_nthOpt36IO_x1; return x1}]
nthOpt37II x0 x1 = msum [do {(x2, x3) <- case x1 of
                                         {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                             return ()},
                         do {x2 <- case x0 of
                                   {Some y2 -> return y2; _ -> mzero};
                             return ()}]


eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval5 :: (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> m r) -> (x1, x2, x3, x4, x5) -> [r]
eval5 listify f = eval listify $ \(x1, x2, x3, x4, x5) -> f x1 x2 x3 x4 x5

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

rightList1 = Cons Zero (Cons (Succ Zero) Nil)
rightAns1 = None
rightAns2 = Succ Zero


main :: IO ()
main = defaultMain
  [
    bgroup "nthOpt"
     [ bench "offlineII"    $ nf (eval2 (takeS 1) nthOptIIOffline) (rightList1, rightAns1)
     , bench "onlineII"     $ nf (eval5 (takeS 1) nthOptIIOnline) (rightList1, rightAns1, natGen, natGen, natGen)
     , bench "offlineIO"    $ nf (eval (takeS 1) nthOptIOOffline) rightList1
     , bench "onlineIO"     $ nf (eval5 (takeS 1) nthOptIOOnline) (rightList1, natGen, natGen, natGen, natGen)
     -- nthOptOIOffline, nthOptOIOnline
     , bench "offlineOI1"    $ nf (eval5 (takeS 1) nthOptOIOffline) (rightAns1, natGen, natGen, natGen, natGen)
--     , bench "onlineOI1"     $ nf (eval5 (takeS 1) nthOptOIOnline) (rightAns1, natGen, natGen, natGen, natGen) -- doesn't fit in time
     , bench "offlineOI2"    $ nf (eval5 (takeS 1) nthOptOIOffline) (rightAns2, natGen, natGen, natGen, natGen)
     , bench "onlineOI2"     $ nf (eval5 (takeS 1) nthOptOIOnline) (rightAns2, natGen, natGen, natGen, natGen)
     -- nthOptOOOffline, nthOptOOOnline 5
     , bench "offlineOO"     $ nf (eval5 (takeS 1) nthOptOOOffline) (natGen, natGen, natGen, natGen, natGen)
  --   , bench "onlineOO"      $ nf (eval5 (takeS 1) nthOptOOOnline) (natGen, natGen, natGen, natGen, natGen) -- doesn't fit in time
     ]
  ]
