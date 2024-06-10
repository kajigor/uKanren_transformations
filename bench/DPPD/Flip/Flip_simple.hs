module Flip_simple where

import Stream
import Control.Monad
import Term

flipflipOI x1 = Immature $ msum [do {x2 <- flipOI x1;
                          x0 <- flipOI x2;
                          return x0}]
flipOI x1 = Immature $ msum [do {x2 <- case x1 of
                            {Leaf y2 -> return y2; _ -> mzero};
                      let {x7 = x2};
                      let {x0 = Leaf x7};
                      return x0},
                  do {(x5, x4, x6) <- case x1 of
                                      {Tree y5 y4 y6 -> return (y5, y4, y6); _ -> mzero};
                      let {x9 = x4};
                      x2 <- flipOI x6;
                      let {x8 = x2};
                      x3 <- flipOI x5;
                      let {x10 = x3};
                      let {x0 = Tree x8 x9 x10};
                      return x0}]
flipflip x1 = flipflipOI x1