module Flip_offline where

import Stream
import Control.Monad
import Term

flipflipdsI x0 = Immature $ msum [do {let {x5 = A};
                           let {x4 = Leaf x5};
                           let {x6 = E};
                           let {x8 = B};
                           let {x7 = Leaf x8};
                           let {x3 = Tree x4 x6 x7};
                           let {x9 = D};
                           let {x11 = C};
                           let {x10 = Leaf x11};
                           let {x2 = Tree x3 x9 x10};
                           let {x12 = E};
                           let {x14 = B};
                           let {x13 = Leaf x14};
                           let {x1 = Tree x2 x12 x13};
                           let {x15 = D};
                           let {x18 = F};
                           let {x17 = Leaf x18};
                           let {x19 = R};
                           let {x21 = C};
                           let {x20 = Leaf x21};
                           let {x16 = Tree x17 x19 x20};
                           (x22, x23, x24) <- case x0 of
                                              {Tree y22 y23 y24 -> return (y22, y23, y24);
                                               _ -> mzero};
                           guard (x22 == x1);
                           guard (x23 == x15);
                           guard (x24 == x16);
                           return ()}]
flipflipdsO = Immature $ msum [do {let {x5 = A};
                        let {x4 = Leaf x5};
                        let {x6 = E};
                        let {x8 = B};
                        let {x7 = Leaf x8};
                        let {x3 = Tree x4 x6 x7};
                        let {x9 = D};
                        let {x11 = C};
                        let {x10 = Leaf x11};
                        let {x2 = Tree x3 x9 x10};
                        let {x12 = E};
                        let {x14 = B};
                        let {x13 = Leaf x14};
                        let {x1 = Tree x2 x12 x13};
                        let {x15 = D};
                        let {x18 = F};
                        let {x17 = Leaf x18};
                        let {x19 = R};
                        let {x21 = C};
                        let {x20 = Leaf x21};
                        let {x16 = Tree x17 x19 x20};
                        let {x22 = x1};
                        let {x23 = x15};
                        let {x24 = x16};
                        let {x0 = Tree x22 x23 x24};
                        return x0}]