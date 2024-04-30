module List3_for_cpd where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    deriving (Show, Eq)
helpOOOI x3 = msum [do {x2 <- reversoOI x3;
                        (x0, x1) <- appendoOOI x3;
                        return (x0, x1, x2)}]
appendoOOI x2 = msum [do {let {x0 = Nil};
                          let {x1 = x2};
                          return (x0, x1)},
                      do {(x3, x5) <- case x2 of
                                      {Cons y3 y5 -> return (y3, y5); _ -> mzero};
                          let {x6 = x3};
                          (x4, x1) <- appendoOOI x5;
                          let {x7 = x4};
                          let {x0 = Cons x6 x7};
                          return (x0, x1)}]
reversoOI x1 = msum [do {let {x0 = Nil};
                         guard (x1 == Nil);
                         return x0},
                     do {let {x9 = Nil};
                         (x4, x8) <- appendoOOI x1;
                         x2 <- case x8 of
                               {Cons y2 y9 -> do {guard (x9 == y9); return y2}; _ -> mzero};
                         let {x10 = x2};
                         x3 <- reversoOI x4;
                         let {x0 = Cons x10 x3};
                         return x0}]
help x3 = helpOOOI x3