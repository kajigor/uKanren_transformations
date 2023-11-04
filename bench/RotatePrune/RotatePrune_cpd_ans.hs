module RotatePrune_cpd_ans where

import Stream
import Control.Monad
import Term

pruneI :: MonadPlus m => Term -> m Term -> m ()
pruneI x0 gen_pruneI_x15 = msum [do {let {x4 = O};
                                     let {x3 = Leaf x4};
                                     let {x6 = O};
                                     let {x5 = S x6};
                                     let {x9 = O};
                                     let {x8 = S x9};
                                     let {x7 = Leaf x8};
                                     let {x2 = Tree x3 x5 x7};
                                     let {x11 = O};
                                     let {x10 = S x11};
                                     (x12, x13, x1) <- case x0 of
                                                       {Tree y12 y13 y1 -> return (y12, y13, y1);
                                                        _ -> mzero};
                                     guard (x12 == x2);
                                     guard (x13 == x10);
                                     _pruneI x1;
                                     return ()},
                                 do {let {x16 = O};
                                     let {x18 = O};
                                     let {x17 = S x18};
                                     let {x21 = O};
                                     let {x20 = S x21};
                                     let {x19 = Leaf x20};
                                     let {x23 = O};
                                     let {x22 = S x23};
                                     (x24, x25, x1) <- case x0 of
                                                       {Tree y24 y25 y1 -> return (y24, y25, y1);
                                                        _ -> mzero};
                                     guard (x25 == x22);
                                     _pruneI x1;
                                     (x14, x15) <- do {x15 <- gen_pruneI_x15;
                                                       let {x14 = Tree x15 x17 x19};
                                                       return (x14, x15)};
                                     guard (x24 == x14);
                                     (x2, x3) <- case x15 of
                                                 {Tree y2 y16 y3 -> do {guard (x16 == y16);
                                                                        return (y2, y3)};
                                                  _ -> mzero};
                                     return ()}]
_pruneI :: MonadPlus m => Term -> m ()
_pruneI x0 = msum [do {let {x26 = O};
                       x27 <- case x0 of
                              {Leaf y27 -> return y27; _ -> mzero};
                       guard (x27 == x26);
                       return ()},
                   do {let {x28 = O};
                       (x1, x29, x2) <- case x0 of
                                        {Tree y1 y29 y2 -> return (y1, y29, y2); _ -> mzero};
                       guard (x29 == x28);
                       return ()}]
pruneO :: MonadPlus m => m Term -> m Term -> m Term -> m Term
pruneO gen__pruneO_x1 gen__pruneO_x2 gen_pruneO_x14 = msum [do {let {x4 = O};
                                                                let {x3 = Leaf x4};
                                                                let {x6 = O};
                                                                let {x5 = S x6};
                                                                let {x9 = O};
                                                                let {x8 = S x9};
                                                                let {x7 = Leaf x8};
                                                                let {x2 = Tree x3 x5 x7};
                                                                let {x11 = O};
                                                                let {x10 = S x11};
                                                                let {x12 = x2};
                                                                let {x13 = x10};
                                                                x1 <- _pruneO gen__pruneO_x1 gen__pruneO_x2;
                                                                let {x0 = Tree x12 x13 x1};
                                                                return x0},
                                                            do {let {x16 = O};
                                                                let {x18 = O};
                                                                let {x17 = S x18};
                                                                let {x21 = O};
                                                                let {x20 = S x21};
                                                                let {x19 = Leaf x20};
                                                                let {x23 = O};
                                                                let {x22 = S x23};
                                                                let {x25 = x22};
                                                                x1 <- _pruneO gen__pruneO_x1 gen__pruneO_x2;
                                                                (x24,
                                                                 x14) <- do {x14 <- gen_pruneO_x14;
                                                                             return (x14, x14)};
                                                                let {x0 = Tree x24 x25 x1};
                                                                x15 <- case x14 of
                                                                       {Tree y15
                                                                             y17
                                                                             y19 -> do {guard (x17 == y17);
                                                                                        guard (x19 == y19);
                                                                                        return y15};
                                                                        _ -> mzero};
                                                                (x2, x3) <- case x15 of
                                                                            {Tree y2
                                                                                  y16
                                                                                  y3 -> do {guard (x16 == y16);
                                                                                            return (y2,
                                                                                                    y3)};
                                                                             _ -> mzero};
                                                                return x0}]
_pruneO :: MonadPlus m => m Term -> m Term -> m Term
_pruneO gen__pruneO_x1 gen__pruneO_x2 = msum [do {let {x26 = O};
                                                  let {x27 = x26};
                                                  let {x0 = Leaf x27};
                                                  return x0},
                                              do {let {x28 = O};
                                                  let {x29 = x28};
                                                  (x0, x1, x2) <- do {x1 <- gen__pruneO_x1;
                                                                      x2 <- gen__pruneO_x2;
                                                                      let {x0 = Tree x1 x29 x2};
                                                                      return (x0, x1, x2)};
                                                  return x0}]