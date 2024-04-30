module Deforestation2_clean where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
rrOI x1 gen_neqIO_x2 = msum [do {x2 <- rOI x1 gen_neqIO_x2;
                                 x0 <- rOI x2 gen_neqIO_x2;
                                 return x0}]
rOI x1 gen_neqIO_x2 = msum [do {let {x0 = Nil};
                                guard (x1 == Nil);
                                return x0},
                            do {(x10, x4) <- case x1 of
                                             {Cons y10 y4 -> return (y10, y4); _ -> mzero};
                                let {x2 = x10};
                                let {x7 = x2};
                                let {x8 = x2};
                                x3 <- rOI x4 gen_neqIO_x2;
                                let {x6 = Cons x7 x3};
                                let {x9 = x6};
                                let {x0 = Cons x8 x9};
                                return x0},
                            do {(x13, x5) <- case x1 of
                                             {Cons y13 y5 -> return (y13, y5); _ -> mzero};
                                let {x2 = x13};
                                x3 <- neqIO x2 gen_neqIO_x2;
                                x14 <- rOI x5 gen_neqIO_x2;
                                x4 <- case x14 of
                                      {Cons y3 y4 -> do {guard (x3 == y3); return y4}; _ -> mzero};
                                let {x11 = Cons x3 x4};
                                let {x12 = x11};
                                let {x0 = Cons x2 x12};
                                return x0}]
neqIO x0 gen_neqIO_x2 = msum [do {guard (x0 == O);
                                  (x1, x2) <- do {x2 <- gen_neqIO_x2;
                                                  let {x1 = S x2};
                                                  return (x1, x2)};
                                  return x1},
                              do {let {x1 = O};
                                  x2 <- case x0 of
                                        {S y2 -> return y2; _ -> mzero};
                                  return x1},
                              do {x2 <- case x0 of
                                        {S y2 -> return y2; _ -> mzero};
                                  x3 <- neqIO x2 gen_neqIO_x2;
                                  let {x1 = S x3};
                                  return x1}]
rr x1 gen_neqIO_x2 = rrOI x1 gen_neqIO_x2