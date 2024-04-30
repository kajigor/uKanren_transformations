module Deforestation1_offline where

import Stream
import Control.Monad
import Term

rrsdI x0 = Immature $ msum [do {let {x2 = O};
                     let {x1 = S x2};
                     let {x3 = Nil};
                     (x4, x5) <- case x0 of
                                 {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                     guard (x4 == x1);
                     guard (x5 == x3);
                     return ()}]
rrsdO = Immature $ msum [do {let {x2 = O};
                  let {x1 = S x2};
                  let {x3 = Nil};
                  let {x4 = x1};
                  let {x5 = x3};
                  let {x0 = Cons x4 x5};
                  return x0}]