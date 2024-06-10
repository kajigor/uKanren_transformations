module Transpose_clean where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    deriving (Show, Eq)
transposeOI x1 = msum [do {guard (x1 == Nil);
                           x0 <- nullrowsO;
                           return x0},
                       do {(x2, x3) <- case x1 of
                                       {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                           x4 <- transposeOI x3;
                           x0 <- makerowOII x2 x4;
                           return x0}]
makerowOII x1 x2 = msum [do {let {x0 = Nil};
                             guard (x1 == Nil);
                             guard (x2 == Nil);
                             return x0},
                         do {(x9, x7) <- case x1 of
                                         {Cons y9 y7 -> return (y9, y7); _ -> mzero};
                             (x6, x8) <- case x2 of
                                         {Cons y6 y8 -> return (y6, y8); _ -> mzero};
                             let {x10 = x9};
                             let {x11 = x6};
                             let {x3 = Cons x10 x11};
                             x4 <- makerowOII x7 x8;
                             let {x0 = Cons x3 x4};
                             return x0}]
nullrowsO = msum [do {let {x0 = Nil}; return x0},
                  do {let {x12 = Nil};
                      let {x13 = x12};
                      x1 <- nullrowsO;
                      let {x14 = x1};
                      let {x0 = Cons x13 x14};
                      return x0}]
transpose x1 = transposeOI x1