{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Simple where 

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
    
nthOptIII x0 x1 x2 = msum [do {guard (x0 == Nil);
                               guard (x2 == None);
                               return ()},
                           do {(x3, x4) <- case x0 of
                                           {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                               nthOpt0IIII x1 x2 x3 x4;
                               return ()}]
nthOptIIO x0 x1 = msum [do {guard (x0 == Nil);
                            let {x2 = None};
                            return x2},
                        do {(x3, x4) <- case x0 of
                                        {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                            x2 <- nthOpt0IOII x1 x3 x4;
                            return x2}]
nthOptIOI x0 x2 gen_nthOptIOI_x1 = msum [do {guard (x0 == Nil);
                                             guard (x2 == None);
                                             x1 <- gen_nthOptIOI_x1;
                                             return x1},
                                         do {(x3, x4) <- case x0 of
                                                         {Cons y3 y4 -> return (y3, y4);
                                                          _ -> mzero};
                                             x1 <- nthOpt0OIII x2 x3 x4 gen_nthOptIOI_x1;
                                             return x1}]
nthOptIOO x0 gen_nthOptIOO_x1 = msum [do {guard (x0 == Nil);
                                          let {x2 = None};
                                          x1 <- gen_nthOptIOO_x1;
                                          return (x1, x2)},
                                      do {(x3, x4) <- case x0 of
                                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                          (x1, x2) <- nthOpt0OOII x3 x4 gen_nthOptIOO_x1;
                                          return (x1, x2)}]
nthOptOII x1 x2 gen_nthOpt0IIOO_x3 gen_nthOpt0IIOO_x4 = msum [do {let {x0 = Nil};
                                                                  guard (x2 == None);
                                                                  return x0},
                                                              do {(x3,
                                                                   x4) <- nthOpt0IIOO x1 x2 gen_nthOpt0IIOO_x3 gen_nthOpt0IIOO_x4;
                                                                  let {x0 = Cons x3 x4};
                                                                  return x0}]
nthOptOIO x1 gen_nthOpt0IOOO_x3 gen_nthOpt0IOOO_x4 = msum [do {let {x0 = Nil};
                                                               let {x2 = None};
                                                               return (x0, x2)},
                                                           do {(x2,
                                                                x3,
                                                                x4) <- nthOpt0IOOO x1 gen_nthOpt0IOOO_x3 gen_nthOpt0IOOO_x4;
                                                               let {x0 = Cons x3 x4};
                                                               return (x0, x2)}]
nthOptOOI x2 gen_nthOpt0OIOO_x3 gen_nthOpt0OIOO_x4 gen_nthOptOOI_x1 = msum [do {let {x0 = Nil};
                                                                                guard (x2 == None);
                                                                                x1 <- gen_nthOptOOI_x1;
                                                                                return (x0, x1)},
                                                                            do {(x1,
                                                                                 x3,
                                                                                 x4) <- nthOpt0OIOO x2 gen_nthOpt0OIOO_x3 gen_nthOpt0OIOO_x4 gen_nthOptOOI_x1;
                                                                                let {x0 = Cons x3 x4};
                                                                                return (x0, x1)}]
nthOptOOO gen_nthOpt0OOOO_x3 gen_nthOpt0OOOO_x4 gen_nthOptOOO_x1 = msum [do {let {x0 = Nil};
                                                                             let {x2 = None};
                                                                             x1 <- gen_nthOptOOO_x1;
                                                                             return (x0, x1, x2)},
                                                                         do {(x1,
                                                                              x2,
                                                                              x3,
                                                                              x4) <- nthOpt0OOOO gen_nthOpt0OOOO_x3 gen_nthOpt0OOOO_x4 gen_nthOptOOO_x1;
                                                                             let {x0 = Cons x3 x4};
                                                                             return (x0, x1, x2)}]
nthOpt0IIII x1 x2 x3 x4 = msum [do {guard (x1 == Zero);
                                    guard (x2 == Some x3);
                                    return ()},
                                do {x5 <- case x1 of
                                          {Succ y5 -> return y5; _ -> mzero};
                                    nthOptIII x4 x5 x2;
                                    return ()}]
nthOpt0IIOO x1 x2 gen_nthOpt0IIOO_x3 gen_nthOpt0IIOO_x4 = msum [do {guard (x1 == Zero);
                                                                    x3 <- case x2 of
                                                                          {Some y3 -> return y3;
                                                                           _ -> mzero};
                                                                    x4 <- gen_nthOpt0IIOO_x4;
                                                                    return (x3, x4)},
                                                                do {x5 <- case x1 of
                                                                          {Succ y5 -> return y5;
                                                                           _ -> mzero};
                                                                    x4 <- nthOptOII x5 x2 gen_nthOpt0IIOO_x3 gen_nthOpt0IIOO_x4;
                                                                    x3 <- gen_nthOpt0IIOO_x3;
                                                                    return (x3, x4)}]
nthOpt0IOII x1 x3 x4 = msum [do {guard (x1 == Zero);
                                 let {x2 = Some x3};
                                 return x2},
                             do {x5 <- case x1 of
                                       {Succ y5 -> return y5; _ -> mzero};
                                 x2 <- nthOptIIO x4 x5;
                                 return x2}]
nthOpt0IOOO x1 gen_nthOpt0IOOO_x3 gen_nthOpt0IOOO_x4 = msum [do {guard (x1 == Zero);
                                                                 (x2,
                                                                  x3) <- do {x3 <- gen_nthOpt0IOOO_x3;
                                                                             let {x2 = Some x3};
                                                                             return (x2, x3)};
                                                                 x4 <- gen_nthOpt0IOOO_x4;
                                                                 return (x2, x3, x4)},
                                                             do {x5 <- case x1 of
                                                                       {Succ y5 -> return y5;
                                                                        _ -> mzero};
                                                                 (x4,
                                                                  x2) <- nthOptOIO x5 gen_nthOpt0IOOO_x3 gen_nthOpt0IOOO_x4;
                                                                 x3 <- gen_nthOpt0IOOO_x3;
                                                                 return (x2, x3, x4)}]
nthOpt0OIII x2 x3 x4 gen_nthOptIOI_x1 = msum [do {let {x1 = Zero};
                                                  guard (x2 == Some x3);
                                                  return x1},
                                              do {x5 <- nthOptIOI x4 x2 gen_nthOptIOI_x1;
                                                  let {x1 = Succ x5};
                                                  return x1}]
nthOpt0OIOO x2 gen_nthOpt0OIOO_x3 gen_nthOpt0OIOO_x4 gen_nthOptOOI_x1 = msum [do {let {x1 = Zero};
                                                                                  x3 <- case x2 of
                                                                                        {Some y3 -> return y3;
                                                                                         _ -> mzero};
                                                                                  x4 <- gen_nthOpt0OIOO_x4;
                                                                                  return (x1,
                                                                                          x3,
                                                                                          x4)},
                                                                              do {(x4,
                                                                                   x5) <- nthOptOOI x2 gen_nthOpt0OIOO_x3 gen_nthOpt0OIOO_x4 gen_nthOptOOI_x1;
                                                                                  let {x1 = Succ x5};
                                                                                  x3 <- gen_nthOpt0OIOO_x3;
                                                                                  return (x1,
                                                                                          x3,
                                                                                          x4)}]
nthOpt0OOII x3 x4 gen_nthOptIOO_x1 = msum [do {let {x1 = Zero};
                                               let {x2 = Some x3};
                                               return (x1, x2)},
                                           do {(x5, x2) <- nthOptIOO x4 gen_nthOptIOO_x1;
                                               let {x1 = Succ x5};
                                               return (x1, x2)}]
nthOpt0OOOO gen_nthOpt0OOOO_x3 gen_nthOpt0OOOO_x4 gen_nthOptOOO_x1 = msum [do {let {x1 = Zero};
                                                                               (x2,
                                                                                x3) <- do {x3 <- gen_nthOpt0OOOO_x3;
                                                                                           let {x2 = Some x3};
                                                                                           return (x2,
                                                                                                   x3)};
                                                                               x4 <- gen_nthOpt0OOOO_x4;
                                                                               return (x1,
                                                                                       x2,
                                                                                       x3,
                                                                                       x4)},
                                                                           do {(x4,
                                                                                x5,
                                                                                x2) <- nthOptOOO gen_nthOpt0OOOO_x3 gen_nthOpt0OOOO_x4 gen_nthOptOOO_x1;
                                                                               let {x1 = Succ x5};
                                                                               x3 <- gen_nthOpt0OOOO_x3;
                                                                               return (x1,
                                                                                       x2,
                                                                                       x3,
                                                                                       x4)}]