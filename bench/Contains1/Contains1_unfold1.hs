module Contains1_unfold1 where

import Stream
import Control.Monad
import Term

containsoI :: MonadPlus m => Term -> m ()
containsoI x0 = msum [do {let {x8 = O};
                          let {x7 = S x8};
                          let {x10 = O};
                          (x12, x13) <- case x0 of
                                        {Cons y12 y13 -> return (y12, y13); _ -> mzero};
                          guard (x12 == x7);
                          let {x9 = x13};
                          x11 <- case x9 of
                                 {Cons y10 y11 -> do {guard (x10 == y10); return y11}; _ -> mzero};
                          (x2, x1) <- case x11 of
                                      {Cons y2 y1 -> return (y2, y1); _ -> mzero};
                          _conoI x1;
                          return ()},
                      do {let {x15 = O};
                          let {x14 = S x15};
                          let {x17 = O};
                          let {x20 = O};
                          let {x19 = S x20};
                          (x21, x22) <- case x0 of
                                        {Cons y21 y22 -> return (y21, y22); _ -> mzero};
                          guard (x21 == x14);
                          let {x16 = x22};
                          x18 <- case x16 of
                                 {Cons y17 y18 -> do {guard (x17 == y17); return y18}; _ -> mzero};
                          x1 <- case x18 of
                                {Cons y19 y1 -> do {guard (x19 == y19); return y1}; _ -> mzero};
                          __conoI x1;
                          return ()},
                      do {let {x24 = O};
                          let {x23 = S x24};
                          (x26, x27) <- case x0 of
                                        {Cons y26 y27 -> return (y26, y27); _ -> mzero};
                          guard (x26 == x23);
                          let {x25 = x27};
                          (x3, x4) <- case x25 of
                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                          _conoI x4;
                          return ()},
                      do {let {x29 = O};
                          let {x28 = S x29};
                          let {x32 = O};
                          let {x31 = S x32};
                          (x33, x34) <- case x0 of
                                        {Cons y33 y34 -> return (y33, y34); _ -> mzero};
                          guard (x33 == x28);
                          let {x30 = x34};
                          x4 <- case x30 of
                                {Cons y31 y4 -> do {guard (x31 == y31); return y4}; _ -> mzero};
                          __conoI x4;
                          return ()},
                      do {(x5, x6) <- case x0 of
                                      {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                          _conoI x6;
                          return ()}]
__conoI :: MonadPlus m => Term -> m ()
__conoI x0 = msum [do {let {x38 = O};
                       (x40, x41) <- case x0 of
                                     {Cons y40 y41 -> return (y40, y41); _ -> mzero};
                       guard (x40 == x38);
                       let {x39 = x41};
                       (x2, x1) <- case x39 of
                                   {Cons y2 y1 -> return (y2, y1); _ -> mzero};
                       _conoI x1;
                       return ()},
                   do {let {x42 = O};
                       let {x45 = O};
                       let {x44 = S x45};
                       (x46, x47) <- case x0 of
                                     {Cons y46 y47 -> return (y46, y47); _ -> mzero};
                       guard (x46 == x42);
                       let {x43 = x47};
                       x1 <- case x43 of
                             {Cons y44 y1 -> do {guard (x44 == y44); return y1}; _ -> mzero};
                       __conoI x1;
                       return ()},
                   do {(x3, x4) <- case x0 of
                                   {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                       _conoI x4;
                       return ()},
                   do {let {x49 = O};
                       let {x48 = S x49};
                       (x50, x4) <- case x0 of
                                    {Cons y50 y4 -> return (y50, y4); _ -> mzero};
                       guard (x50 == x48);
                       __conoI x4;
                       return ()}]
_conoI :: MonadPlus m => Term -> m ()
_conoI x0 = msum [do {let {x36 = O};
                      let {x35 = S x36};
                      (x37, x1) <- case x0 of
                                   {Cons y37 y1 -> return (y37, y1); _ -> mzero};
                      guard (x37 == x35);
                      __conoI x1;
                      return ()},
                  do {(x2, x1) <- case x0 of
                                  {Cons y2 y1 -> return (y2, y1); _ -> mzero};
                      _conoI x1;
                      return ()}]
containsoO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term
containsoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2 gen_containsoO_x25 gen_containsoO_x5 gen_containsoO_x9 = msum [do {let {x8 = O};
                                                                                                                           let {x7 = S x8};
                                                                                                                           let {x10 = O};
                                                                                                                           let {x12 = x7};
                                                                                                                           x1 <- _conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                                                                           (x13,
                                                                                                                            x9) <- do {x9 <- gen_containsoO_x9;
                                                                                                                                       return (x9,
                                                                                                                                               x9)};
                                                                                                                           let {x0 = Cons x12 x13};
                                                                                                                           x11 <- case x9 of
                                                                                                                                  {Cons y10
                                                                                                                                        y11 -> do {guard (x10 == y10);
                                                                                                                                                   return y11};
                                                                                                                                   _ -> mzero};
                                                                                                                           x2 <- case x11 of
                                                                                                                                 {Cons y2
                                                                                                                                       y1 -> do {guard (x1 == y1);
                                                                                                                                                 return y2};
                                                                                                                                  _ -> mzero};
                                                                                                                           return x0},
                                                                                                                       do {let {x15 = O};
                                                                                                                           let {x14 = S x15};
                                                                                                                           let {x17 = O};
                                                                                                                           let {x20 = O};
                                                                                                                           let {x19 = S x20};
                                                                                                                           let {x21 = x14};
                                                                                                                           x1 <- __conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                                                                           let {x18 = Cons x19 x1};
                                                                                                                           let {x16 = Cons x17 x18};
                                                                                                                           let {x22 = x16};
                                                                                                                           let {x0 = Cons x21 x22};
                                                                                                                           return x0},
                                                                                                                       do {let {x24 = O};
                                                                                                                           let {x23 = S x24};
                                                                                                                           let {x26 = x23};
                                                                                                                           x4 <- _conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                                                                           (x27,
                                                                                                                            x25) <- do {x25 <- gen_containsoO_x25;
                                                                                                                                        return (x25,
                                                                                                                                                x25)};
                                                                                                                           let {x0 = Cons x26 x27};
                                                                                                                           x3 <- case x25 of
                                                                                                                                 {Cons y3
                                                                                                                                       y4 -> do {guard (x4 == y4);
                                                                                                                                                 return y3};
                                                                                                                                  _ -> mzero};
                                                                                                                           return x0},
                                                                                                                       do {let {x29 = O};
                                                                                                                           let {x28 = S x29};
                                                                                                                           let {x32 = O};
                                                                                                                           let {x31 = S x32};
                                                                                                                           let {x33 = x28};
                                                                                                                           x4 <- __conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                                                                           let {x30 = Cons x31 x4};
                                                                                                                           let {x34 = x30};
                                                                                                                           let {x0 = Cons x33 x34};
                                                                                                                           return x0},
                                                                                                                       do {x6 <- _conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                                                                           (x0,
                                                                                                                            x5) <- do {x5 <- gen_containsoO_x5;
                                                                                                                                       let {x0 = Cons x5 x6};
                                                                                                                                       return (x0,
                                                                                                                                               x5)};
                                                                                                                           return x0}]
__conoO :: MonadPlus m => m Term -> m Term -> m Term -> m Term
__conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2 = msum [do {let {x38 = O};
                                                                 let {x40 = x38};
                                                                 x1 <- _conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                 (x41,
                                                                  x39) <- do {x39 <- gen___conoO_x39;
                                                                              return (x39, x39)};
                                                                 let {x0 = Cons x40 x41};
                                                                 x2 <- case x39 of
                                                                       {Cons y2
                                                                             y1 -> do {guard (x1 == y1);
                                                                                       return y2};
                                                                        _ -> mzero};
                                                                 return x0},
                                                             do {let {x42 = O};
                                                                 let {x45 = O};
                                                                 let {x44 = S x45};
                                                                 let {x46 = x42};
                                                                 x1 <- __conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                 let {x43 = Cons x44 x1};
                                                                 let {x47 = x43};
                                                                 let {x0 = Cons x46 x47};
                                                                 return x0},
                                                             do {x4 <- _conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                 (x0,
                                                                  x3) <- do {x3 <- gen___conoO_x3;
                                                                             let {x0 = Cons x3 x4};
                                                                             return (x0, x3)};
                                                                 return x0},
                                                             do {let {x49 = O};
                                                                 let {x48 = S x49};
                                                                 let {x50 = x48};
                                                                 x4 <- __conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                 let {x0 = Cons x50 x4};
                                                                 return x0}]
_conoO :: MonadPlus m => m Term -> m Term -> m Term -> m Term
_conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2 = msum [do {let {x36 = O};
                                                                let {x35 = S x36};
                                                                let {x37 = x35};
                                                                x1 <- __conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                let {x0 = Cons x37 x1};
                                                                return x0},
                                                            do {x1 <- _conoO gen___conoO_x3 gen___conoO_x39 gen__conoO_x2;
                                                                (x0, x2) <- do {x2 <- gen__conoO_x2;
                                                                                let {x0 = Cons x2 x1};
                                                                                return (x0, x2)};
                                                                return x0}]