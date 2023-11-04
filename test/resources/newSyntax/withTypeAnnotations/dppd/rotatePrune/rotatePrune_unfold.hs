module RotatePrune_unfold where

import Stream
import Control.Monad

data Term
    = Leaf Term
    | O
    | S Term
    | Tree Term Term Term
    deriving (Show, Eq)
pruneI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m ()
pruneI x0 gen_pruneI_x1 gen_pruneI_x2 gen_pruneI_x33 gen_pruneI_x48 = msum [do {let {x4 = O};
                                                                                let {x3 = Leaf x4};
                                                                                let {x6 = O};
                                                                                let {x5 = S x6};
                                                                                let {x9 = O};
                                                                                let {x8 = S x9};
                                                                                let {x7 = Leaf x8};
                                                                                let {x2 = Tree x3 x5 x7};
                                                                                let {x11 = O};
                                                                                let {x10 = S x11};
                                                                                let {x13 = O};
                                                                                let {x12 = Leaf x13};
                                                                                (x14,
                                                                                 x15,
                                                                                 x16) <- case x0 of
                                                                                         {Tree y14
                                                                                               y15
                                                                                               y16 -> return (y14,
                                                                                                              y15,
                                                                                                              y16);
                                                                                          _ -> mzero};
                                                                                guard (x14 == x2);
                                                                                guard (x15 == x10);
                                                                                guard (x16 == x12);
                                                                                return ()},
                                                                            do {let {x19 = O};
                                                                                let {x18 = Leaf x19};
                                                                                let {x21 = O};
                                                                                let {x20 = S x21};
                                                                                let {x24 = O};
                                                                                let {x23 = S x24};
                                                                                let {x22 = Leaf x23};
                                                                                let {x17 = Tree x18 x20 x22};
                                                                                let {x26 = O};
                                                                                let {x25 = S x26};
                                                                                let {x28 = O};
                                                                                (x29,
                                                                                 x30,
                                                                                 x31) <- case x0 of
                                                                                         {Tree y29
                                                                                               y30
                                                                                               y31 -> return (y29,
                                                                                                              y30,
                                                                                                              y31);
                                                                                          _ -> mzero};
                                                                                guard (x29 == x17);
                                                                                guard (x30 == x25);
                                                                                (x27,
                                                                                 x1,
                                                                                 x2) <- do {x1 <- gen_pruneI_x1;
                                                                                            x2 <- gen_pruneI_x2;
                                                                                            let {x27 = Tree x1 x28 x2};
                                                                                            return (x27,
                                                                                                    x1,
                                                                                                    x2)};
                                                                                guard (x31 == x27);
                                                                                return ()},
                                                                            do {let {x34 = O};
                                                                                let {x36 = O};
                                                                                let {x35 = S x36};
                                                                                let {x39 = O};
                                                                                let {x38 = S x39};
                                                                                let {x37 = Leaf x38};
                                                                                let {x41 = O};
                                                                                let {x40 = S x41};
                                                                                let {x43 = O};
                                                                                let {x42 = Leaf x43};
                                                                                (x44,
                                                                                 x45,
                                                                                 x46) <- case x0 of
                                                                                         {Tree y44
                                                                                               y45
                                                                                               y46 -> return (y44,
                                                                                                              y45,
                                                                                                              y46);
                                                                                          _ -> mzero};
                                                                                guard (x45 == x40);
                                                                                guard (x46 == x42);
                                                                                (x32,
                                                                                 x33) <- do {x33 <- gen_pruneI_x33;
                                                                                             let {x32 = Tree x33 x35 x37};
                                                                                             return (x32,
                                                                                                     x33)};
                                                                                guard (x44 == x32);
                                                                                (x3,
                                                                                 x4) <- case x33 of
                                                                                        {Tree y3
                                                                                              y34
                                                                                              y4 -> do {guard (x34 == y34);
                                                                                                        return (y3,
                                                                                                                y4)};
                                                                                         _ -> mzero};
                                                                                return ()},
                                                                            do {let {x49 = O};
                                                                                let {x51 = O};
                                                                                let {x50 = S x51};
                                                                                let {x54 = O};
                                                                                let {x53 = S x54};
                                                                                let {x52 = Leaf x53};
                                                                                let {x56 = O};
                                                                                let {x55 = S x56};
                                                                                let {x58 = O};
                                                                                (x59,
                                                                                 x60,
                                                                                 x61) <- case x0 of
                                                                                         {Tree y59
                                                                                               y60
                                                                                               y61 -> return (y59,
                                                                                                              y60,
                                                                                                              y61);
                                                                                          _ -> mzero};
                                                                                guard (x60 == x55);
                                                                                (x47,
                                                                                 x48) <- do {x48 <- gen_pruneI_x48;
                                                                                             let {x47 = Tree x48 x50 x52};
                                                                                             return (x47,
                                                                                                     x48)};
                                                                                guard (x59 == x47);
                                                                                (x3,
                                                                                 x4) <- case x48 of
                                                                                        {Tree y3
                                                                                              y49
                                                                                              y4 -> do {guard (x49 == y49);
                                                                                                        return (y3,
                                                                                                                y4)};
                                                                                         _ -> mzero};
                                                                                (x57,
                                                                                 x1,
                                                                                 x2) <- do {x1 <- gen_pruneI_x1;
                                                                                            x2 <- gen_pruneI_x2;
                                                                                            let {x57 = Tree x1 x58 x2};
                                                                                            return (x57,
                                                                                                    x1,
                                                                                                    x2)};
                                                                                guard (x61 == x57);
                                                                                return ()}]
pruneO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term
pruneO gen_pruneO_x27 gen_pruneO_x32 gen_pruneO_x47 gen_pruneO_x57 = msum [do {let {x4 = O};
                                                                               let {x3 = Leaf x4};
                                                                               let {x6 = O};
                                                                               let {x5 = S x6};
                                                                               let {x9 = O};
                                                                               let {x8 = S x9};
                                                                               let {x7 = Leaf x8};
                                                                               let {x2 = Tree x3 x5 x7};
                                                                               let {x11 = O};
                                                                               let {x10 = S x11};
                                                                               let {x13 = O};
                                                                               let {x12 = Leaf x13};
                                                                               let {x14 = x2};
                                                                               let {x15 = x10};
                                                                               let {x16 = x12};
                                                                               let {x0 = Tree x14 x15 x16};
                                                                               return x0},
                                                                           do {let {x19 = O};
                                                                               let {x18 = Leaf x19};
                                                                               let {x21 = O};
                                                                               let {x20 = S x21};
                                                                               let {x24 = O};
                                                                               let {x23 = S x24};
                                                                               let {x22 = Leaf x23};
                                                                               let {x17 = Tree x18 x20 x22};
                                                                               let {x26 = O};
                                                                               let {x25 = S x26};
                                                                               let {x28 = O};
                                                                               let {x29 = x17};
                                                                               let {x30 = x25};
                                                                               (x31,
                                                                                x27) <- do {x27 <- gen_pruneO_x27;
                                                                                            return (x27,
                                                                                                    x27)};
                                                                               let {x0 = Tree x29 x30 x31};
                                                                               (x1,
                                                                                x2) <- case x27 of
                                                                                       {Tree y1
                                                                                             y28
                                                                                             y2 -> do {guard (x28 == y28);
                                                                                                       return (y1,
                                                                                                               y2)};
                                                                                        _ -> mzero};
                                                                               return x0},
                                                                           do {let {x34 = O};
                                                                               let {x36 = O};
                                                                               let {x35 = S x36};
                                                                               let {x39 = O};
                                                                               let {x38 = S x39};
                                                                               let {x37 = Leaf x38};
                                                                               let {x41 = O};
                                                                               let {x40 = S x41};
                                                                               let {x43 = O};
                                                                               let {x42 = Leaf x43};
                                                                               let {x45 = x40};
                                                                               let {x46 = x42};
                                                                               (x44,
                                                                                x32) <- do {x32 <- gen_pruneO_x32;
                                                                                            return (x32,
                                                                                                    x32)};
                                                                               let {x0 = Tree x44 x45 x46};
                                                                               x33 <- case x32 of
                                                                                      {Tree y33
                                                                                            y35
                                                                                            y37 -> do {guard (x35 == y35);
                                                                                                       guard (x37 == y37);
                                                                                                       return y33};
                                                                                       _ -> mzero};
                                                                               (x3,
                                                                                x4) <- case x33 of
                                                                                       {Tree y3
                                                                                             y34
                                                                                             y4 -> do {guard (x34 == y34);
                                                                                                       return (y3,
                                                                                                               y4)};
                                                                                        _ -> mzero};
                                                                               return x0},
                                                                           do {let {x49 = O};
                                                                               let {x51 = O};
                                                                               let {x50 = S x51};
                                                                               let {x54 = O};
                                                                               let {x53 = S x54};
                                                                               let {x52 = Leaf x53};
                                                                               let {x56 = O};
                                                                               let {x55 = S x56};
                                                                               let {x58 = O};
                                                                               let {x60 = x55};
                                                                               (x59,
                                                                                x47) <- do {x47 <- gen_pruneO_x47;
                                                                                            return (x47,
                                                                                                    x47)};
                                                                               x48 <- case x47 of
                                                                                      {Tree y48
                                                                                            y50
                                                                                            y52 -> do {guard (x50 == y50);
                                                                                                       guard (x52 == y52);
                                                                                                       return y48};
                                                                                       _ -> mzero};
                                                                               (x3,
                                                                                x4) <- case x48 of
                                                                                       {Tree y3
                                                                                             y49
                                                                                             y4 -> do {guard (x49 == y49);
                                                                                                       return (y3,
                                                                                                               y4)};
                                                                                        _ -> mzero};
                                                                               (x61,
                                                                                x57) <- do {x57 <- gen_pruneO_x57;
                                                                                            return (x57,
                                                                                                    x57)};
                                                                               let {x0 = Tree x59 x60 x61};
                                                                               (x1,
                                                                                x2) <- case x57 of
                                                                                       {Tree y1
                                                                                             y58
                                                                                             y2 -> do {guard (x58 == y58);
                                                                                                       return (y1,
                                                                                                               y2)};
                                                                                        _ -> mzero};
                                                                               return x0}]