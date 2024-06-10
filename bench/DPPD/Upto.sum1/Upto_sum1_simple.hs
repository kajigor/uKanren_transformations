module Upto_sum1_simple where

import Stream
import Control.Monad
import Term

sumsquaresuptoIO x0 = Immature $ msum [do {x2 <- uptoIO x0;
                                x3 <- squaresIO x2;
                                x1 <- sumIO x3;
                                return x1}]
squaresIO x0 = Immature $ msum [do {let {x1 = Nil};
                         guard (x0 == Nil);
                         return x1},
                     do {(x2, x3) <- case x0 of
                                     {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                         x4 <- squareIO x2;
                         x5 <- squaresIO x3;
                         let {x1 = Cons x4 x5};
                         return x1}]
squareIO x0 = Immature $ msum [do {let {x8 = x0};
                        x1 <- multiplyIIO x0 x8;
                        return x1}]
multiplyIIO x0 x1 = Immature $ msum [do {let {x2 = O};
                              guard (x1 == O);
                              return x2},
                          do {x3 <- case x1 of
                                    {S y3 -> return y3; _ -> mzero};
                              x4 <- multiplyIIO x0 x3;
                              x2 <- addIIO x0 x4;
                              return x2}]
addIIO x0 x1 = Immature $ msum [do {guard (x0 == O);
                         let {x2 = x1};
                         return x2},
                     do {x3 <- case x0 of
                               {S y3 -> return y3; _ -> mzero};
                         x4 <- addIIO x3 x1;
                         let {x2 = S x4};
                         return x2}]
sumIO x0 = Immature $ msum [do {let {x9 = O}; x1 <- sum1IIO x0 x9; return x1}]
sum1IIO x0 x1 = Immature $ msum [do {guard (x0 == Nil);
                          let {x2 = x1};
                          return x2},
                      do {(x3, x4) <- case x0 of
                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                          x5 <- addIIO x3 x1;
                          x2 <- sum1IIO x4 x5;
                          return x2}]
uptoIO x0 = Immature $ msum [do {let {x1 = Nil}; guard (x0 == O); return x1},
                  do {x3 <- case x0 of
                            {S y3 -> return y3; _ -> mzero};
                      let {x6 = x0};
                      x2 <- uptoIO x3;
                      let {x7 = x2};
                      let {x1 = Cons x6 x7};
                      return x1}]
sumsquaresupto x0 = sumsquaresuptoIO x0