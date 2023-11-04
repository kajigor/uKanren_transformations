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
    | Succ Term
    | Zero
    deriving (Show, Eq, Generic, DS.NFData)
    
    
sortoII x0 x1 gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0 = msum [do {guard (x0 == Nil);
                                                                                guard (x1 == Nil);
                                                                                return ()},
                                                                            do {(x2,
                                                                                 x3) <- case x0 of
                                                                                        {Cons y2
                                                                                              y3 -> return (y2,
                                                                                                            y3);
                                                                                         _ -> mzero};
                                                                                (x4,
                                                                                 x6) <- splitoIIOO x2 x3;
                                                                                x7 <- sortoIO x4 gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0;
                                                                                (x5,
                                                                                 x8) <- sortoOO gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0;
                                                                                sorto0IIIII x1 x2 x5 x7 x8;
                                                                                return ()}]
sortoIO x0 gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0 = msum [do {guard (x0 == Nil);
                                                                             let {x1 = Nil};
                                                                             return x1},
                                                                         do {(x2, x3) <- case x0 of
                                                                                         {Cons y2
                                                                                               y3 -> return (y2,
                                                                                                             y3);
                                                                                          _ -> mzero};
                                                                             (x4,
                                                                              x6) <- splitoIIOO x2 x3;
                                                                             x7 <- sortoIO x4 gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0;
                                                                             (x5,
                                                                              x8) <- sortoOO gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0;
                                                                             x1 <- sorto0OIIII x2 x5 x7 x8;
                                                                             return x1}]
sortoOI x1 gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0 = msum [do {let {x0 = Nil};
                                                                             guard (x1 == Nil);
                                                                             return x0},
                                                                         do {(x2,
                                                                              x3,
                                                                              x4,
                                                                              x6) <- splitoOOOO gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0;
                                                                             let {x0 = Cons x2 x3};
                                                                             x7 <- sortoIO x4 gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0;
                                                                             (x5,
                                                                              x8) <- sortoOO gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0;
                                                                             sorto0IIIII x1 x2 x5 x7 x8;
                                                                             return x0}]
sortoOO gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0 = msum [do {let {x0 = Nil};
                                                                          let {x1 = Nil};
                                                                          return (x0, x1)},
                                                                      do {(x2,
                                                                           x3,
                                                                           x4,
                                                                           x6) <- splitoOOOO gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0;
                                                                          let {x0 = Cons x2 x3};
                                                                          x7 <- sortoIO x4 gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0;
                                                                          (x5,
                                                                           x8) <- sortoOO gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0;
                                                                          x1 <- sorto0OIIII x2 x5 x7 x8;
                                                                          return (x0, x1)}]
sorto0IIIII x1 x2 x5 x7 x8 = msum [do {appendoIII x7 x5 x1;
                                       guard (x5 == Cons x2 x8);
                                       return ()}]
appendoIII x0 x1 x2 = msum [do {guard (x0 == Nil);
                                guard (x1 == x2);
                                return ()},
                            do {(x3, x4) <- case x0 of
                                            {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                x5 <- case x2 of
                                      {Cons y3 y5 -> do {guard (x3 == y3); return y5}; _ -> mzero};
                                appendoIII x4 x1 x5;
                                return ()}]
sorto0OIIII x2 x5 x7 x8 = msum [do {x1 <- appendoIIO x7 x5;
                                    guard (x5 == Cons x2 x8);
                                    return x1}]
appendoIIO x0 x1 = msum [do {guard (x0 == Nil);
                             let {x2 = x1};
                             return x2},
                         do {(x3, x4) <- case x0 of
                                         {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                             x5 <- appendoIIO x4 x1;
                             let {x2 = Cons x3 x5};
                             return x2}]
splitoIIOO x0 x1 = msum [do {guard (x1 == Nil);
                             let {x2 = Nil};
                             let {x3 = Nil};
                             return (x2, x3)},
                         do {(x4, x5) <- case x1 of
                                         {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                             (x2, x3) <- splito0IOOII x0 x4 x5;
                             return (x2, x3)}]
splitoOOOO gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 gen_splitoOOOO_x0 = msum [do {let {x1 = Nil};
                                                                             let {x2 = Nil};
                                                                             let {x3 = Nil};
                                                                             x0 <- gen_splitoOOOO_x0;
                                                                             return (x0,
                                                                                     x1,
                                                                                     x2,
                                                                                     x3)},
                                                                         do {(x0,
                                                                              x2,
                                                                              x3,
                                                                              x4,
                                                                              x5) <- splito0OOOOO gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1;
                                                                             let {x1 = Cons x4 x5};
                                                                             return (x0,
                                                                                     x1,
                                                                                     x2,
                                                                                     x3)}]
splito0IOOII x0 x4 x5 = msum [do {leII x4 x0;
                                  (x6, x3) <- splitoIIOO x0 x5;
                                  let {x2 = Cons x4 x6};
                                  return (x2, x3)},
                              do {gtII x4 x0;
                                  (x2, x6) <- splitoIIOO x0 x5;
                                  let {x3 = Cons x4 x6};
                                  return (x2, x3)}]
gtII x0 x1 = msum [do {x2 <- case x0 of
                             {Succ y2 -> return y2; _ -> mzero};
                       guard (x1 == Zero);
                       return ()},
                   do {x2 <- case x0 of
                             {Succ y2 -> return y2; _ -> mzero};
                       x3 <- case x1 of
                             {Succ y3 -> return y3; _ -> mzero};
                       gtII x2 x3;
                       return ()}]
leII x0 x1 = msum [do {guard (x0 == Zero); return ()},
                   do {x2 <- case x0 of
                             {Succ y2 -> return y2; _ -> mzero};
                       x3 <- case x1 of
                             {Succ y3 -> return y3; _ -> mzero};
                       leII x2 x3;
                       return ()}]
splito0OOOOO gen_gtOI_x2 gen_gtOO_x2 gen_leOO_x1 = msum [do {(x4,
                                                              x0) <- leOO gen_leOO_x1;
                                                             (x5,
                                                              x6,
                                                              x3) <- splitoIOOO x0 gen_gtOI_x2;
                                                             let {x2 = Cons x4 x6};
                                                             return (x0, x2, x3, x4, x5)},
                                                         do {(x4, x0) <- gtOO gen_gtOO_x2;
                                                             (x5,
                                                              x2,
                                                              x6) <- splitoIOOO x0 gen_gtOI_x2;
                                                             let {x3 = Cons x4 x6};
                                                             return (x0, x2, x3, x4, x5)}]
gtOO gen_gtOO_x2 = msum [do {let {x1 = Zero};
                             (x0, x2) <- do {x2 <- gen_gtOO_x2;
                                             let {x0 = Succ x2};
                                             return (x0, x2)};
                             return (x0, x1)},
                         do {(x2, x3) <- gtOO gen_gtOO_x2;
                             let {x0 = Succ x2};
                             let {x1 = Succ x3};
                             return (x0, x1)}]
leOO gen_leOO_x1 = msum [do {let {x0 = Zero};
                             x1 <- gen_leOO_x1;
                             return (x0, x1)},
                         do {(x2, x3) <- leOO gen_leOO_x1;
                             let {x0 = Succ x2};
                             let {x1 = Succ x3};
                             return (x0, x1)}]
splitoIOOO x0 gen_gtOI_x2 = msum [do {let {x1 = Nil};
                                      let {x2 = Nil};
                                      let {x3 = Nil};
                                      return (x1, x2, x3)},
                                  do {(x2, x3, x4, x5) <- splito0IOOOO x0 gen_gtOI_x2;
                                      let {x1 = Cons x4 x5};
                                      return (x1, x2, x3)}]
splito0IOOOO x0 gen_gtOI_x2 = msum [do {x4 <- leOI x0;
                                        (x5, x6, x3) <- splitoIOOO x0 gen_gtOI_x2;
                                        let {x2 = Cons x4 x6};
                                        return (x2, x3, x4, x5)},
                                    do {x4 <- gtOI x0 gen_gtOI_x2;
                                        (x5, x2, x6) <- splitoIOOO x0 gen_gtOI_x2;
                                        let {x3 = Cons x4 x6};
                                        return (x2, x3, x4, x5)}]
gtOI x1 gen_gtOI_x2 = msum [do {guard (x1 == Zero);
                                (x0, x2) <- do {x2 <- gen_gtOI_x2;
                                                let {x0 = Succ x2};
                                                return (x0, x2)};
                                return x0},
                            do {x3 <- case x1 of
                                      {Succ y3 -> return y3; _ -> mzero};
                                x2 <- gtOI x3 gen_gtOI_x2;
                                let {x0 = Succ x2};
                                return x0}]
leOI x1 = msum [do {let {x0 = Zero}; return x0},
                do {x3 <- case x1 of
                          {Succ y3 -> return y3; _ -> mzero};
                    x2 <- leOI x3;
                    let {x0 = Succ x2};
                    return x0}]