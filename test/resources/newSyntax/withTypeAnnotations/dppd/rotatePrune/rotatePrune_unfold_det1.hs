module RotatePrune where

import Stream
import Control.Monad

data Term
    = Leaf Term
    | O
    | S Term
    | Tree Term Term Term
    deriving (Show, Eq)
pruneI x0 = msum [do {let {x7 = O};
                      let {x6 = Leaf x7};
                      let {x9 = O};
                      let {x8 = S x9};
                      let {x12 = O};
                      let {x11 = S x12};
                      let {x10 = Leaf x11};
                      let {x5 = Tree x6 x8 x10};
                      let {x14 = O};
                      let {x13 = S x14};
                      let {x16 = O};
                      let {x15 = Leaf x16};
                      (x17, x18, x19) <- case x0 of
                                         {Tree y17 y18 y19 -> return (y17, y18, y19); _ -> mzero};
                      guard (x17 == x5);
                      guard (x18 == x13);
                      guard (x19 == x15);
                      return ()},
                  do {let {x22 = O};
                      let {x21 = Leaf x22};
                      let {x24 = O};
                      let {x23 = S x24};
                      let {x27 = O};
                      let {x26 = S x27};
                      let {x25 = Leaf x26};
                      let {x20 = Tree x21 x23 x25};
                      let {x29 = O};
                      let {x28 = S x29};
                      let {x31 = O};
                      (x32, x33, x34) <- case x0 of
                                         {Tree y32 y33 y34 -> return (y32, y33, y34); _ -> mzero};
                      guard (x32 == x20);
                      guard (x33 == x28);
                      let {x30 = x34};
                      (x1, x2) <- case x30 of
                                  {Tree y1 y31 y2 -> do {guard (x31 == y31); return (y1, y2)};
                                   _ -> mzero};
                      return ()},
                  do {let {x37 = O};
                      let {x39 = O};
                      let {x38 = S x39};
                      let {x42 = O};
                      let {x41 = S x42};
                      let {x40 = Leaf x41};
                      let {x44 = O};
                      let {x43 = S x44};
                      let {x46 = O};
                      let {x45 = Leaf x46};
                      (x47, x48, x49) <- case x0 of
                                         {Tree y47 y48 y49 -> return (y47, y48, y49); _ -> mzero};
                      guard (x48 == x43);
                      guard (x49 == x45);
                      let {x35 = x47};
                      x36 <- case x35 of
                             {Tree y36 y38 y40 -> do {guard (x38 == y38);
                                                      guard (x40 == y40);
                                                      return y36};
                              _ -> mzero};
                      (x3, x4) <- case x36 of
                                  {Tree y3 y37 y4 -> do {guard (x37 == y37); return (y3, y4)};
                                   _ -> mzero};
                      return ()},
                  do {let {x52 = O};
                      let {x54 = O};
                      let {x53 = S x54};
                      let {x57 = O};
                      let {x56 = S x57};
                      let {x55 = Leaf x56};
                      let {x59 = O};
                      let {x58 = S x59};
                      let {x61 = O};
                      (x62, x63, x64) <- case x0 of
                                         {Tree y62 y63 y64 -> return (y62, y63, y64); _ -> mzero};
                      guard (x63 == x58);
                      let {x50 = x62};
                      x51 <- case x50 of
                             {Tree y51 y53 y55 -> do {guard (x53 == y53);
                                                      guard (x55 == y55);
                                                      return y51};
                              _ -> mzero};
                      (x3, x4) <- case x51 of
                                  {Tree y3 y52 y4 -> do {guard (x52 == y52); return (y3, y4)};
                                   _ -> mzero};
                      let {x60 = x64};
                      (x1, x2) <- case x60 of
                                  {Tree y1 y61 y2 -> do {guard (x61 == y61); return (y1, y2)};
                                   _ -> mzero};
                      return ()}]
pruneO gen_pruneO_x30 gen_pruneO_x35 gen_pruneO_x50 gen_pruneO_x60 = msum [do {let {x7 = O};
                                                                               let {x6 = Leaf x7};
                                                                               let {x9 = O};
                                                                               let {x8 = S x9};
                                                                               let {x12 = O};
                                                                               let {x11 = S x12};
                                                                               let {x10 = Leaf x11};
                                                                               let {x5 = Tree x6 x8 x10};
                                                                               let {x14 = O};
                                                                               let {x13 = S x14};
                                                                               let {x16 = O};
                                                                               let {x15 = Leaf x16};
                                                                               let {x17 = x5};
                                                                               let {x18 = x13};
                                                                               let {x19 = x15};
                                                                               let {x0 = Tree x17 x18 x19};
                                                                               return x0},
                                                                           do {let {x22 = O};
                                                                               let {x21 = Leaf x22};
                                                                               let {x24 = O};
                                                                               let {x23 = S x24};
                                                                               let {x27 = O};
                                                                               let {x26 = S x27};
                                                                               let {x25 = Leaf x26};
                                                                               let {x20 = Tree x21 x23 x25};
                                                                               let {x29 = O};
                                                                               let {x28 = S x29};
                                                                               let {x31 = O};
                                                                               let {x32 = x20};
                                                                               let {x33 = x28};
                                                                               (x34,
                                                                                x30) <- do {x30 <- gen_pruneO_x30;
                                                                                            return (x30,
                                                                                                    x30)};
                                                                               let {x0 = Tree x32 x33 x34};
                                                                               (x1,
                                                                                x2) <- case x30 of
                                                                                       {Tree y1
                                                                                             y31
                                                                                             y2 -> do {guard (x31 == y31);
                                                                                                       return (y1,
                                                                                                               y2)};
                                                                                        _ -> mzero};
                                                                               return x0},
                                                                           do {let {x37 = O};
                                                                               let {x39 = O};
                                                                               let {x38 = S x39};
                                                                               let {x42 = O};
                                                                               let {x41 = S x42};
                                                                               let {x40 = Leaf x41};
                                                                               let {x44 = O};
                                                                               let {x43 = S x44};
                                                                               let {x46 = O};
                                                                               let {x45 = Leaf x46};
                                                                               let {x48 = x43};
                                                                               let {x49 = x45};
                                                                               (x47,
                                                                                x35) <- do {x35 <- gen_pruneO_x35;
                                                                                            return (x35,
                                                                                                    x35)};
                                                                               let {x0 = Tree x47 x48 x49};
                                                                               x36 <- case x35 of
                                                                                      {Tree y36
                                                                                            y38
                                                                                            y40 -> do {guard (x38 == y38);
                                                                                                       guard (x40 == y40);
                                                                                                       return y36};
                                                                                       _ -> mzero};
                                                                               (x3,
                                                                                x4) <- case x36 of
                                                                                       {Tree y3
                                                                                             y37
                                                                                             y4 -> do {guard (x37 == y37);
                                                                                                       return (y3,
                                                                                                               y4)};
                                                                                        _ -> mzero};
                                                                               return x0},
                                                                           do {let {x52 = O};
                                                                               let {x54 = O};
                                                                               let {x53 = S x54};
                                                                               let {x57 = O};
                                                                               let {x56 = S x57};
                                                                               let {x55 = Leaf x56};
                                                                               let {x59 = O};
                                                                               let {x58 = S x59};
                                                                               let {x61 = O};
                                                                               let {x63 = x58};
                                                                               (x62,
                                                                                x50) <- do {x50 <- gen_pruneO_x50;
                                                                                            return (x50,
                                                                                                    x50)};
                                                                               x51 <- case x50 of
                                                                                      {Tree y51
                                                                                            y53
                                                                                            y55 -> do {guard (x53 == y53);
                                                                                                       guard (x55 == y55);
                                                                                                       return y51};
                                                                                       _ -> mzero};
                                                                               (x3,
                                                                                x4) <- case x51 of
                                                                                       {Tree y3
                                                                                             y52
                                                                                             y4 -> do {guard (x52 == y52);
                                                                                                       return (y3,
                                                                                                               y4)};
                                                                                        _ -> mzero};
                                                                               (x64,
                                                                                x60) <- do {x60 <- gen_pruneO_x60;
                                                                                            return (x60,
                                                                                                    x60)};
                                                                               let {x0 = Tree x62 x63 x64};
                                                                               (x1,
                                                                                x2) <- case x60 of
                                                                                       {Tree y1
                                                                                             y61
                                                                                             y2 -> do {guard (x61 == y61);
                                                                                                       return (y1,
                                                                                                               y2)};
                                                                                        _ -> mzero};
                                                                               return x0}]