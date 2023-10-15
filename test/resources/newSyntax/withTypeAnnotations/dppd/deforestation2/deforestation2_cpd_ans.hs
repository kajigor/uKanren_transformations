module Deforestation2_cpd_ans where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
rrI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rRI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                      return ()},
                                                                                                                                                  do {neqRRI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                      return ()}]
neqRRI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR4I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                           return ()}]
rRI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR14I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                      return ()}]
rR14I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR15I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                        return ()}]
rR15I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR16I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                        return ()}]
rR16I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                        return ()}]
rI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {r0I x0;
                                                                                                                                                     x1 <- rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                     return ()},
                                                                                                                                                 do {r1I x0;
                                                                                                                                                     (x2,
                                                                                                                                                      x3) <- neqR1OO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                     return ()}]
neqR1OO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {let {x0 = O};
                                                                                                                                                       x58 <- rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                       (x59,
                                                                                                                                                        x1) <- case x58 of
                                                                                                                                                               {Cons y59
                                                                                                                                                                     y1 -> return (y59,
                                                                                                                                                                                   y1);
                                                                                                                                                                _ -> mzero};
                                                                                                                                                       guard (x59 == O);
                                                                                                                                                       return (x0,
                                                                                                                                                               x1)},
                                                                                                                                                   do {x61 <- rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                       (x62,
                                                                                                                                                        x1) <- case x61 of
                                                                                                                                                               {Cons y62
                                                                                                                                                                     y1 -> return (y62,
                                                                                                                                                                                   y1);
                                                                                                                                                                _ -> mzero};
                                                                                                                                                       x63 <- case x62 of
                                                                                                                                                              {S y63 -> return y63;
                                                                                                                                                               _ -> mzero};
                                                                                                                                                       x2 <- case x63 of
                                                                                                                                                             {S y2 -> return y2;
                                                                                                                                                              _ -> mzero};
                                                                                                                                                       let {x60 = S x2};
                                                                                                                                                       let {x0 = S x60};
                                                                                                                                                       return (x0,
                                                                                                                                                               x1)}]
r0I x0 = msum [do {(x3, x5) <- case x0 of
                               {Cons y3 y5 -> return (y3, y5); _ -> mzero};
                   x4 <- case x3 of
                         {S y4 -> return y4; _ -> mzero};
                   guard (x4 == O);
                   (x6, x1) <- case x5 of
                               {Cons y6 y1 -> return (y6, y1); _ -> mzero};
                   x7 <- case x6 of
                         {S y7 -> return y7; _ -> mzero};
                   guard (x7 == O);
                   return ()}]
r1I x0 = msum [do {(x8, x10) <- case x0 of
                                {Cons y8 y10 -> return (y8, y10); _ -> mzero};
                   x9 <- case x8 of
                         {S y9 -> return y9; _ -> mzero};
                   guard (x9 == O);
                   (x2, x3) <- case x10 of
                               {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                   return ()}]
rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR131O gen_rR131O_x13;
                                                                                                                                                     x1 <- rR12O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                     return x0},
                                                                                                                                                 do {x0 <- rR132O gen_rR132O_x18;
                                                                                                                                                     (x2,
                                                                                                                                                      x3) <- neqROO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                     return x0}]
neqROO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {let {x0 = O};
                                                                                                                        x52 <- rR12O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                        (x53,
                                                                                                                         x1) <- case x52 of
                                                                                                                                {Cons y53
                                                                                                                                      y1 -> return (y53,
                                                                                                                                                    y1);
                                                                                                                                 _ -> mzero};
                                                                                                                        guard (x53 == O);
                                                                                                                        return (x0,
                                                                                                                                x1)},
                                                                                                                    do {x55 <- rR12O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                        (x56,
                                                                                                                         x1) <- case x55 of
                                                                                                                                {Cons y56
                                                                                                                                      y1 -> return (y56,
                                                                                                                                                    y1);
                                                                                                                                 _ -> mzero};
                                                                                                                        x57 <- case x56 of
                                                                                                                               {S y57 -> return y57;
                                                                                                                                _ -> mzero};
                                                                                                                        x2 <- case x57 of
                                                                                                                              {S y2 -> return y2;
                                                                                                                               _ -> mzero};
                                                                                                                        let {x54 = S x2};
                                                                                                                        let {x0 = S x54};
                                                                                                                        return (x0,
                                                                                                                                x1)}]
rR12O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR122O gen_rR122O_x20;
                                                                                                                       x1 <- rR11O gen_rR102O_x28 gen_rR112O_x23 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                       return x0}]
rR11O gen_rR102O_x28 gen_rR112O_x23 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR112O gen_rR112O_x23;
                                                                                                        x1 <- rR10O gen_rR102O_x28 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                        return x0}]
rR10O gen_rR102O_x28 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR102O gen_rR102O_x28;
                                                                                         x1 <- rR9O gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                         return x0}]
rR102O gen_rR102O_x28 = msum [do {let {x27 = O};
                                  let {x26 = S x27};
                                  let {x25 = S x26};
                                  let {x31 = O};
                                  let {x30 = S x31};
                                  let {x29 = S x30};
                                  (x0, x28) <- do {x28 <- gen_rR102O_x28;
                                                   let {x0 = Cons x25 x28};
                                                   return (x0, x28)};
                                  x1 <- case x28 of
                                        {Cons y29 y1 -> do {guard (x29 == y29); return y1};
                                         _ -> mzero};
                                  return x0}]
rR112O gen_rR112O_x23 = msum [do {let {x22 = O};
                                  let {x24 = O};
                                  (x0, x23) <- do {x23 <- gen_rR112O_x23;
                                                   let {x0 = Cons x22 x23};
                                                   return (x0, x23)};
                                  x1 <- case x23 of
                                        {Cons y24 y1 -> do {guard (x24 == y24); return y1};
                                         _ -> mzero};
                                  return x0}]
rR122O gen_rR122O_x20 = msum [do {let {x19 = O};
                                  let {x21 = O};
                                  (x0, x20) <- do {x20 <- gen_rR122O_x20;
                                                   let {x0 = Cons x19 x20};
                                                   return (x0, x20)};
                                  x1 <- case x20 of
                                        {Cons y21 y1 -> do {guard (x21 == y21); return y1};
                                         _ -> mzero};
                                  return x0}]
rR131O gen_rR131O_x13 = msum [do {let {x12 = O};
                                  let {x11 = S x12};
                                  let {x15 = O};
                                  let {x14 = S x15};
                                  (x0, x13) <- do {x13 <- gen_rR131O_x13;
                                                   let {x0 = Cons x11 x13};
                                                   return (x0, x13)};
                                  x1 <- case x13 of
                                        {Cons y14 y1 -> do {guard (x14 == y14); return y1};
                                         _ -> mzero};
                                  return x0}]
rR132O gen_rR132O_x18 = msum [do {let {x17 = O};
                                  let {x16 = S x17};
                                  (x0, x18) <- do {x18 <- gen_rR132O_x18;
                                                   let {x0 = Cons x16 x18};
                                                   return (x0, x18)};
                                  (x2, x3) <- case x18 of
                                              {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                  return x0}]
rR4I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR3I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                         return ()}]
rR3I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR2I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                         return ()}]
rR2I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR1I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                         return ()}]
rR1I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR13I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                         return ()}]
rR13I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR131I x0;
                                                                                                                          x1 <- rR12O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                          return ()},
                                                                                                                      do {rR132I x0;
                                                                                                                          (x2,
                                                                                                                           x3) <- neqROO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                          return ()}]
rR131I x0 = msum [do {(x11, x13) <- case x0 of
                                    {Cons y11 y13 -> return (y11, y13); _ -> mzero};
                      x12 <- case x11 of
                             {S y12 -> return y12; _ -> mzero};
                      guard (x12 == O);
                      (x14, x1) <- case x13 of
                                   {Cons y14 y1 -> return (y14, y1); _ -> mzero};
                      x15 <- case x14 of
                             {S y15 -> return y15; _ -> mzero};
                      guard (x15 == O);
                      return ()}]
rR132I x0 = msum [do {(x16, x18) <- case x0 of
                                    {Cons y16 y18 -> return (y16, y18); _ -> mzero};
                      x17 <- case x16 of
                             {S y17 -> return y17; _ -> mzero};
                      guard (x17 == O);
                      (x2, x3) <- case x18 of
                                  {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                      return ()}]
rR9O gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR92O gen_rR92O_x35;
                                                                         x1 <- rR8O gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40;
                                                                         return x0}]
rR8O gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 = msum [do {x0 <- rR82O gen_rR82O_x40;
                                                           x1 <- rR7O gen_rR62O_x46 gen_rR72O_x43;
                                                           return x0}]
rR7O gen_rR62O_x46 gen_rR72O_x43 = msum [do {x0 <- rR72O gen_rR72O_x43;
                                             x1 <- rR6O gen_rR62O_x46;
                                             return x0}]
rR6O gen_rR62O_x46 = msum [do {x0 <- rR62O gen_rR62O_x46;
                               x1 <- rR5O;
                               return x0}]
rR5O = msum [do {let {x48 = O};
                 let {x50 = O};
                 let {x51 = Nil};
                 let {x49 = Cons x50 x51};
                 let {x0 = Cons x48 x49};
                 return x0}]
rR62O gen_rR62O_x46 = msum [do {let {x45 = O};
                                let {x47 = O};
                                (x0, x46) <- do {x46 <- gen_rR62O_x46;
                                                 let {x0 = Cons x45 x46};
                                                 return (x0, x46)};
                                x1 <- case x46 of
                                      {Cons y47 y1 -> do {guard (x47 == y47); return y1};
                                       _ -> mzero};
                                return x0}]
rR72O gen_rR72O_x43 = msum [do {let {x42 = O};
                                let {x44 = O};
                                (x0, x43) <- do {x43 <- gen_rR72O_x43;
                                                 let {x0 = Cons x42 x43};
                                                 return (x0, x43)};
                                x1 <- case x43 of
                                      {Cons y44 y1 -> do {guard (x44 == y44); return y1};
                                       _ -> mzero};
                                return x0}]
rR82O gen_rR82O_x40 = msum [do {let {x39 = O};
                                let {x41 = O};
                                (x0, x40) <- do {x40 <- gen_rR82O_x40;
                                                 let {x0 = Cons x39 x40};
                                                 return (x0, x40)};
                                x1 <- case x40 of
                                      {Cons y41 y1 -> do {guard (x41 == y41); return y1};
                                       _ -> mzero};
                                return x0}]
rR92O gen_rR92O_x35 = msum [do {let {x34 = O};
                                let {x33 = S x34};
                                let {x32 = S x33};
                                let {x38 = O};
                                let {x37 = S x38};
                                let {x36 = S x37};
                                (x0, x35) <- do {x35 <- gen_rR92O_x35;
                                                 let {x0 = Cons x32 x35};
                                                 return (x0, x35)};
                                x1 <- case x35 of
                                      {Cons y36 y1 -> do {guard (x36 == y36); return y1};
                                       _ -> mzero};
                                return x0}]
rrO gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rRO gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                          return x0},
                                                                                                                                                                      do {x0 <- neqRRO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                          return x0}]
neqRRO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR4O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                      return x0}]
rRO gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR14O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                          return x0}]
rR14O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR15O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                            return x0}]
rR15O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR16O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                            return x0}]
rR16O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rO gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                            return x0}]
rO gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- r0O gen_r0O_x5;
                                                                                                                                                                         x1 <- rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                         return x0},
                                                                                                                                                                     do {x0 <- r1O gen_r1O_x10;
                                                                                                                                                                         (x2,
                                                                                                                                                                          x3) <- neqR1OO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                         return x0}]
r0O gen_r0O_x5 = msum [do {let {x4 = O};
                           let {x3 = S x4};
                           let {x7 = O};
                           let {x6 = S x7};
                           (x0, x5) <- do {x5 <- gen_r0O_x5;
                                           let {x0 = Cons x3 x5};
                                           return (x0, x5)};
                           x1 <- case x5 of
                                 {Cons y6 y1 -> do {guard (x6 == y6); return y1}; _ -> mzero};
                           return x0}]
r1O gen_r1O_x10 = msum [do {let {x9 = O};
                            let {x8 = S x9};
                            (x0, x10) <- do {x10 <- gen_r1O_x10;
                                             let {x0 = Cons x8 x10};
                                             return (x0, x10)};
                            (x2, x3) <- case x10 of
                                        {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                            return x0}]
rR4O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR3O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                    return x0}]
rR3O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR2O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                    return x0}]
rR2O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR1O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                    return x0}]
rR1O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                    return x0}]