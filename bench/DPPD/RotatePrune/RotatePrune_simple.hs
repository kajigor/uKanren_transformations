module RotatePrune_simple where

import Stream
import Control.Monad
import Term

pruneOI x1 gen_pruneOI_x2 gen_pruneOI_x4 = Immature $ msum [do {x2 <- case x1 of
                                                           {Leaf y2 -> return y2; _ -> mzero};
                                                     let {x16 = x2};
                                                     let {x0 = Leaf x16};
                                                     return x0},
                                                 do {let {x3 = O};
                                                     let {x5 = O};
                                                     guard (x1 == Leaf x5);
                                                     let {x17 = x3};
                                                     (x0, x2, x4) <- do {x2 <- gen_pruneOI_x2;
                                                                         x4 <- gen_pruneOI_x4;
                                                                         let {x0 = Tree x2 x17 x4};
                                                                         return (x0, x2, x4)};
                                                     return x0},
                                                 do {(x5, x7, x6) <- case x1 of
                                                                     {Tree y5 y7 y6 -> return (y5,
                                                                                               y7,
                                                                                               y6);
                                                                      _ -> mzero};
                                                     x8 <- case x7 of
                                                           {S y8 -> return y8; _ -> mzero};
                                                     let {x18 = x8};
                                                     let {x3 = S x18};
                                                     let {x20 = x3};
                                                     x2 <- pruneOI x5 gen_pruneOI_x2 gen_pruneOI_x4;
                                                     let {x19 = x2};
                                                     x4 <- pruneOI x6 gen_pruneOI_x2 gen_pruneOI_x4;
                                                     let {x21 = x4};
                                                     let {x0 = Tree x19 x20 x21};
                                                     return x0}]
prune x1 gen_pruneOI_x2 gen_pruneOI_x4 = pruneOI x1 gen_pruneOI_x2 gen_pruneOI_x4