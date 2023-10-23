{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Simple where

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

data Term = 
    Cons Term Term | 
    Zero | 
    Succ Term | 
    Nil 
   deriving (Show, Eq, Generic, DS.NFData)
    
applastoIIISimple x0 x1 x2 gen_lastoIO_x2 = msum [do {applasto0II x0 x1;
                                                x3 <- lastoIO x2 gen_lastoIO_x2;
                                                return ()}]
applastoIIO x0 x1 gen_lastoOO_x0 gen_lastoOO_x2 = msum [do {applasto0II x0 x1;
                                                            (x2,
                                                             x3) <- lastoOO gen_lastoOO_x0 gen_lastoOO_x2;
                                                            return x2}]
applastoIOI x0 x2 gen_appendoIOO_x1 gen_lastoIO_x2 = msum [do {x1 <- applasto0IO x0 gen_appendoIOO_x1;
                                                               x3 <- lastoIO x2 gen_lastoIO_x2;
                                                               return x1}]
applastoIOO x0 gen_appendoIOO_x1 gen_lastoOO_x0 gen_lastoOO_x2 = msum [do {x1 <- applasto0IO x0 gen_appendoIOO_x1;
                                                                           (x2,
                                                                            x3) <- lastoOO gen_lastoOO_x0 gen_lastoOO_x2;
                                                                           return (x1, x2)}]
applastoOII x1 x2 gen_appendoOIO_x3 gen_lastoIO_x2 = msum [do {x0 <- applasto0OI x1 gen_appendoOIO_x3;
                                                               x3 <- lastoIO x2 gen_lastoIO_x2;
                                                               return x0}]
applastoOIO x1 gen_appendoOIO_x3 gen_lastoOO_x0 gen_lastoOO_x2 = msum [do {x0 <- applasto0OI x1 gen_appendoOIO_x3;
                                                                           (x2,
                                                                            x3) <- lastoOO gen_lastoOO_x0 gen_lastoOO_x2;
                                                                           return (x0, x2)}]
applastoOOI x2 gen_appendoOOO_x1 gen_appendoOOO_x3 gen_lastoIO_x2 = msum [do {x3 <- lastoIO x2 gen_lastoIO_x2;
                                                                              (x0,
                                                                               x1) <- applasto0OO gen_appendoOOO_x1 gen_appendoOOO_x3;
                                                                              return (x0, x1)}]
applastoOOO gen_appendoOOO_x1 gen_appendoOOO_x3 gen_lastoOO_x0 gen_lastoOO_x2 = msum [do {(x0,
                                                                                           x1) <- applasto0OO gen_appendoOOO_x1 gen_appendoOOO_x3;
                                                                                          (x2,
                                                                                           x3) <- lastoOO gen_lastoOO_x0 gen_lastoOO_x2;
                                                                                          return (x0,
                                                                                                  x1,
                                                                                                  x2)}]
applasto0II x0 x1 = msum [do {let {x5 = Nil};
                              let {x4 = Cons x1 x5};
                              x3 <- appendoIIO x0 x4;
                              return ()}]
appendoIIO x0 x1 = msum [do {guard (x0 == Nil);
                             let {x2 = x1};
                             return x2},
                         do {(x3, x4) <- case x0 of
                                         {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                             x5 <- appendoIIO x4 x1;
                             let {x2 = Cons x3 x5};
                             return x2}]
applasto0IO x0 gen_appendoIOO_x1 = msum [do {let {x5 = Nil};
                                             (x4, x3) <- appendoIOO x0 gen_appendoIOO_x1;
                                             x1 <- case x4 of
                                                   {Cons y1 y5 -> do {guard (x5 == y5); return y1};
                                                    _ -> mzero};
                                             return x1}]
appendoIOO x0 gen_appendoIOO_x1 = msum [do {guard (x0 == Nil);
                                            (x2, x1) <- do {x1 <- gen_appendoIOO_x1;
                                                            return (x1, x1)};
                                            return (x1, x2)},
                                        do {(x3, x4) <- case x0 of
                                                        {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                            (x1, x5) <- appendoIOO x4 gen_appendoIOO_x1;
                                            let {x2 = Cons x3 x5};
                                            return (x1, x2)}]
applasto0OI x1 gen_appendoOIO_x3 = msum [do {let {x5 = Nil};
                                             let {x4 = Cons x1 x5};
                                             (x0, x3) <- appendoOIO x4 gen_appendoOIO_x3;
                                             return x0}]
appendoOIO x1 gen_appendoOIO_x3 = msum [do {let {x0 = Nil};
                                            let {x2 = x1};
                                            return (x0, x2)},
                                        do {(x4, x5) <- appendoOIO x1 gen_appendoOIO_x3;
                                            (x0, x3) <- do {x3 <- gen_appendoOIO_x3;
                                                            let {x0 = Cons x3 x4};
                                                            return (x0, x3)};
                                            let {x2 = Cons x3 x5};
                                            return (x0, x2)}]
applasto0OO gen_appendoOOO_x1 gen_appendoOOO_x3 = msum [do {let {x5 = Nil};
                                                            (x0,
                                                             x4,
                                                             x3) <- appendoOOO gen_appendoOOO_x1 gen_appendoOOO_x3;
                                                            x1 <- case x4 of
                                                                  {Cons y1
                                                                        y5 -> do {guard (x5 == y5);
                                                                                  return y1};
                                                                   _ -> mzero};
                                                            return (x0, x1)}]
appendoOOO gen_appendoOOO_x1 gen_appendoOOO_x3 = msum [do {let {x0 = Nil};
                                                           (x2, x1) <- do {x1 <- gen_appendoOOO_x1;
                                                                           return (x1, x1)};
                                                           return (x0, x1, x2)},
                                                       do {(x4,
                                                            x1,
                                                            x5) <- appendoOOO gen_appendoOOO_x1 gen_appendoOOO_x3;
                                                           (x0, x3) <- do {x3 <- gen_appendoOOO_x3;
                                                                           let {x0 = Cons x3 x4};
                                                                           return (x0, x3)};
                                                           let {x2 = Cons x3 x5};
                                                           return (x0, x1, x2)}]
lastoIO x0 gen_lastoIO_x2 = msum [do {let {x6 = Nil};
                                      let {x1 = Cons x0 x6};
                                      return x1},
                                  do {x3 <- lastoIO x0 gen_lastoIO_x2;
                                      (x1, x2) <- do {x2 <- gen_lastoIO_x2;
                                                      let {x1 = Cons x2 x3};
                                                      return (x1, x2)};
                                      return x1}]
lastoOO gen_lastoOO_x0 gen_lastoOO_x2 = msum [do {let {x6 = Nil};
                                                  (x1, x0) <- do {x0 <- gen_lastoOO_x0;
                                                                  let {x1 = Cons x0 x6};
                                                                  return (x1, x0)};
                                                  return (x0, x1)},
                                              do {(x0, x3) <- lastoOO gen_lastoOO_x0 gen_lastoOO_x2;
                                                  (x1, x2) <- do {x2 <- gen_lastoOO_x2;
                                                                  let {x1 = Cons x2 x3};
                                                                  return (x1, x2)};
                                                  return (x0, x1)}]