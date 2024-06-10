module Deforestation1_clean where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
rrIO x0 = msum [do {x2 <- rIO x0; x1 <- rIO x2; return x1}]
rIO x0 = msum [do {let {x1 = Nil}; guard (x0 == Nil); return x1},
               do {(x8, x9) <- case x0 of
                               {Cons y8 y9 -> return (y8, y9); _ -> mzero};
                   let {x2 = x8};
                   let {x7 = x2};
                   let {x6 = x9};
                   x3 <- case x6 of
                         {Cons y7 y3 -> do {guard (x7 == y7); return y3}; _ -> mzero};
                   let {x10 = x2};
                   x4 <- rIO x3;
                   let {x1 = Cons x10 x4};
                   return x1},
               do {(x2, x12) <- case x0 of
                                {Cons y2 y12 -> return (y2, y12); _ -> mzero};
                   let {x11 = x12};
                   (x3, x4) <- case x11 of
                               {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                   neqII x2 x3;
                   let {x13 = Cons x3 x4};
                   let {x14 = x2};
                   x5 <- rIO x13;
                   let {x15 = x5};
                   let {x1 = Cons x14 x15};
                   return x1}]
neqII x0 x1 = msum [do {guard (x0 == O);
                        x2 <- case x1 of
                              {S y2 -> return y2; _ -> mzero};
                        return ()},
                    do {x2 <- case x0 of
                              {S y2 -> return y2; _ -> mzero};
                        guard (x1 == O);
                        return ()},
                    do {x2 <- case x0 of
                              {S y2 -> return y2; _ -> mzero};
                        x3 <- case x1 of
                              {S y3 -> return y3; _ -> mzero};
                        neqII x2 x3;
                        return ()}]
rr x0 = rrIO x0