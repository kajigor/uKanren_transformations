module Match_simple where

import Stream
import Control.Monad
import Term

matchoOI x1 = Immature $ msum [do {(x4, x2) <- appendoOOI x1;
                        (x3, x0) <- appendoOOI x4;
                        return x0}]
appendoOOI x2 = Immature $ msum [do {let {x0 = Nil};
                          let {x1 = x2};
                          return (x0, x1)},
                      do {(x6, x7) <- case x2 of
                                      {Cons y6 y7 -> return (y6, y7); _ -> mzero};
                          let {x3 = x6};
                          let {x5 = x7};
                          (x4, x1) <- appendoOOI x5;
                          let {x0 = Cons x3 x4};
                          return (x0, x1)}]
matcho x1 = matchoOI x1