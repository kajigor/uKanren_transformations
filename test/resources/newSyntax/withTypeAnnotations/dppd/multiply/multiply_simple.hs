module Multiply_clean where

import Stream
import Control.Monad

data Term
    = O
    | S Term
    deriving (Show, Eq)
multiplyOOI x2 gen_multiplyOOI_x0 = msum [do {let {x1 = O};
                                              guard (x2 == O);
                                              x0 <- gen_multiplyOOI_x0;
                                              return (x0, x1)},
                                          do {(x0, x4) <- addOOI x2;
                                              x3 <- multiplyIOI x0 x4;
                                              let {x1 = S x3};
                                              return (x0, x1)}]
addOOI x2 = msum [do {let {x0 = O};
                      let {x1 = x2};
                      return (x0, x1)},
                  do {x4 <- case x2 of
                            {S y4 -> return y4; _ -> mzero};
                      (x3, x1) <- addOOI x4;
                      let {x0 = S x3};
                      return (x0, x1)}]
multiplyIOI x0 x2 = msum [do {let {x1 = O};
                              guard (x2 == O);
                              return x1},
                          do {x4 <- addIOI x0 x2;
                              x3 <- multiplyIOI x0 x4;
                              let {x1 = S x3};
                              return x1}]
addIOI x0 x2 = msum [do {guard (x0 == O);
                         let {x1 = x2};
                         return x1},
                     do {x3 <- case x0 of
                               {S y3 -> return y3; _ -> mzero};
                         x4 <- case x2 of
                               {S y4 -> return y4; _ -> mzero};
                         x1 <- addIOI x3 x4;
                         return x1}]
multiply x2 gen_multiplyOOI_x0 = multiplyOOI x2 gen_multiplyOOI_x0