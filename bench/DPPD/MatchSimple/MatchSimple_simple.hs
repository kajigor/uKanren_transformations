module MatchSimple_simple where

import Stream
import Control.Monad
import Term

matchOI x1 gen_match10OIOIOI_x6 gen_match1OIOI_x2 gen_neqOI_x2 = Immature $ msum [do {let {x11 = x1};
                                                                           (x0,
                                                                            x10) <- match1OIOI x1 x11 gen_match10OIOIOI_x6 gen_match1OIOI_x2 gen_neqOI_x2;
                                                                           guard (x10 == x0);
                                                                           return x0}]
match1OIOI x1 x3 gen_match10OIOIOI_x6 gen_match1OIOI_x2 gen_neqOI_x2 = Immature $ msum [do {let {x0 = Nil};
                                                                                 x2 <- gen_match1OIOI_x2;
                                                                                 return (x0, x2)},
                                                                             do {(x5,
                                                                                  x7) <- case x1 of
                                                                                         {Cons y5
                                                                                               y7 -> return (y5,
                                                                                                             y7);
                                                                                          _ -> mzero};
                                                                                 (x2,
                                                                                  x4,
                                                                                  x6) <- match10OIOIOI x3 x5 x7 gen_match10OIOIOI_x6 gen_match1OIOI_x2 gen_neqOI_x2;
                                                                                 let {x0 = Cons x4 x6};
                                                                                 return (x0, x2)}]
match10OIOIOI x3 x5 x7 gen_match10OIOIOI_x6 gen_match1OIOI_x2 gen_neqOI_x2 = Immature $ msum [do {let {x4 = x5};
                                                                                       (x6,
                                                                                        x2) <- match1OIOI x7 x3 gen_match10OIOIOI_x6 gen_match1OIOI_x2 gen_neqOI_x2;
                                                                                       return (x2,
                                                                                               x4,
                                                                                               x6)},
                                                                                   do {(x8,
                                                                                        x9) <- case x3 of
                                                                                               {Cons y8
                                                                                                     y9 -> return (y8,
                                                                                                                   y9);
                                                                                                _ -> mzero};
                                                                                       let {x13 = x9};
                                                                                       x4 <- neqOI x5 gen_neqOI_x2;
                                                                                       (x2,
                                                                                        x12) <- match1OIOI x9 x13 gen_match10OIOIOI_x6 gen_match1OIOI_x2 gen_neqOI_x2;
                                                                                       guard (x12 == x2);
                                                                                       x6 <- gen_match10OIOIOI_x6;
                                                                                       return (x2,
                                                                                               x4,
                                                                                               x6)}]
neqOI x1 gen_neqOI_x2 = Immature $ msum [do {guard (x1 == O);
                                  (x0, x2) <- do {x2 <- gen_neqOI_x2;
                                                  let {x0 = S x2};
                                                  return (x0, x2)};
                                  return x0},
                              do {let {x0 = O};
                                  x2 <- case x1 of
                                        {S y2 -> return y2; _ -> mzero};
                                  return x0},
                              do {x3 <- case x1 of
                                        {S y3 -> return y3; _ -> mzero};
                                  x2 <- neqOI x3 gen_neqOI_x2;
                                  let {x0 = S x2};
                                  return x0}]
match x1 gen_match10OIOIOI_x6 gen_match1OIOI_x2 gen_neqOI_x2 = matchOI x1 gen_match10OIOIOI_x6 gen_match1OIOI_x2 gen_neqOI_x2