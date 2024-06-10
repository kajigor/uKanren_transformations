module Flip_online where

import Stream
import Control.Monad
import Term

flipflipI x0 = Immature $ msum [do {let {x3 = D};
                         (x1, x4, x2) <- case x0 of
                                         {Tree y1 y4 y2 -> return (y1, y4, y2); _ -> mzero};
                         guard (x4 == x3);
                         flipI x1;
                         _flipI x2;
                         return ()}]
_flipI x0 = Immature $ msum [do {let {x22 = F};
                      let {x21 = Leaf x22};
                      let {x23 = R};
                      let {x25 = C};
                      let {x24 = Leaf x25};
                      (x26, x27, x28) <- case x0 of
                                         {Tree y26 y27 y28 -> return (y26, y27, y28); _ -> mzero};
                      guard (x26 == x21);
                      guard (x27 == x23);
                      guard (x28 == x24);
                      return ()}]
flipI x0 = Immature $ msum [do {let {x8 = A};
                     let {x7 = Leaf x8};
                     let {x9 = E};
                     let {x11 = B};
                     let {x10 = Leaf x11};
                     let {x6 = Tree x7 x9 x10};
                     let {x12 = D};
                     let {x14 = C};
                     let {x13 = Leaf x14};
                     let {x5 = Tree x6 x12 x13};
                     let {x15 = E};
                     let {x17 = B};
                     let {x16 = Leaf x17};
                     (x18, x19, x20) <- case x0 of
                                        {Tree y18 y19 y20 -> return (y18, y19, y20); _ -> mzero};
                     guard (x18 == x5);
                     guard (x19 == x15);
                     guard (x20 == x16);
                     return ()}]
flipflipO = Immature $ msum [do {let {x3 = D};
                      let {x4 = x3};
                      x1 <- flipO;
                      x2 <- _flipO;
                      let {x0 = Tree x1 x4 x2};
                      return x0}]
_flipO = Immature $ msum [do {let {x22 = F};
                   let {x21 = Leaf x22};
                   let {x23 = R};
                   let {x25 = C};
                   let {x24 = Leaf x25};
                   let {x26 = x21};
                   let {x27 = x23};
                   let {x28 = x24};
                   let {x0 = Tree x26 x27 x28};
                   return x0}]
flipO = Immature $ msum [do {let {x8 = A};
                  let {x7 = Leaf x8};
                  let {x9 = E};
                  let {x11 = B};
                  let {x10 = Leaf x11};
                  let {x6 = Tree x7 x9 x10};
                  let {x12 = D};
                  let {x14 = C};
                  let {x13 = Leaf x14};
                  let {x5 = Tree x6 x12 x13};
                  let {x15 = E};
                  let {x17 = B};
                  let {x16 = Leaf x17};
                  let {x18 = x5};
                  let {x19 = x15};
                  let {x20 = x16};
                  let {x0 = Tree x18 x19 x20};
                  return x0}]