module Deforestation2_conspd_ans where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
rrI x0 gen_neqIO_x2 gen_r0O_x31 gen_r10IIO_x2 = msum [do {x1 <- rO gen_r0O_x31;
                                                          r1II x0 x1 gen_neqIO_x2 gen_r10IIO_x2;
                                                          return ()}]
rO gen_r0O_x31 = msum [do {let {x4 = O};
                           let {x3 = S x4};
                           let {x7 = O};
                           let {x6 = S x7};
                           let {x9 = O};
                           let {x11 = O};
                           let {x15 = O};
                           let {x14 = S x15};
                           let {x13 = S x14};
                           let {x19 = O};
                           let {x18 = S x19};
                           let {x17 = S x18};
                           let {x21 = O};
                           let {x23 = O};
                           let {x25 = O};
                           let {x27 = O};
                           let {x28 = Nil};
                           let {x26 = Cons x27 x28};
                           let {x24 = Cons x25 x26};
                           let {x22 = Cons x23 x24};
                           let {x20 = Cons x21 x22};
                           let {x16 = Cons x17 x20};
                           let {x12 = Cons x13 x16};
                           let {x10 = Cons x11 x12};
                           let {x8 = Cons x9 x10};
                           let {x5 = Cons x6 x8};
                           let {x0 = Cons x3 x5};
                           return x0},
                       do {x0 <- r0O gen_r0O_x31; x1 <- r2O; return x0}]
r0O gen_r0O_x31 = msum [do {let {x30 = O};
                            let {x29 = S x30};
                            let {x32 = O};
                            let {x34 = O};
                            (x0, x31) <- do {x31 <- gen_r0O_x31;
                                             let {x0 = Cons x29 x31};
                                             return (x0, x31)};
                            x33 <- case x31 of
                                   {Cons y32 y33 -> do {guard (x32 == y32); return y33};
                                    _ -> mzero};
                            x1 <- case x33 of
                                  {Cons y34 y1 -> do {guard (x34 == y34); return y1}; _ -> mzero};
                            return x0}]
r1II x0 x1 gen_neqIO_x2 gen_r10IIO_x2 = msum [do {guard (x1 == Nil);
                                                  guard (x0 == Nil);
                                                  return ()},
                                              do {(x2, x3) <- case x1 of
                                                              {Cons y2 y3 -> return (y2, y3);
                                                               _ -> mzero};
                                                  x35 <- case x0 of
                                                         {Cons y2 y35 -> do {guard (x2 == y2);
                                                                             return y35};
                                                          _ -> mzero};
                                                  (x36, x4) <- case x35 of
                                                               {Cons y36 y4 -> return (y36, y4);
                                                                _ -> mzero};
                                                  guard (x36 == x2);
                                                  r1II x4 x3 gen_neqIO_x2 gen_r10IIO_x2;
                                                  return ()},
                                              do {(x5, x6) <- case x1 of
                                                              {Cons y5 y6 -> return (y5, y6);
                                                               _ -> mzero};
                                                  x37 <- case x0 of
                                                         {Cons y5 y37 -> do {guard (x5 == y5);
                                                                             return y37};
                                                          _ -> mzero};
                                                  (x7, x8) <- case x37 of
                                                              {Cons y7 y8 -> return (y7, y8);
                                                               _ -> mzero};
                                                  neqII x5 x7;
                                                  r10III x6 x7 x8 gen_neqIO_x2 gen_r10IIO_x2;
                                                  return ()}]
neqII x0 x1 = msum [do {x2 <- case x1 of
                              {S y2 -> return y2; _ -> mzero};
                        guard (x0 == O);
                        return ()},
                    do {guard (x1 == O);
                        x3 <- case x0 of
                              {S y3 -> return y3; _ -> mzero};
                        return ()},
                    do {x4 <- case x1 of
                              {S y4 -> return y4; _ -> mzero};
                        x5 <- case x0 of
                              {S y5 -> return y5; _ -> mzero};
                        neqII x5 x4;
                        return ()}]
r10III x0 x1 x2 gen_neqIO_x2 gen_r10IIO_x2 = msum [do {x38 <- r1OI x6 gen_neqIO_x2 gen_r10IIO_x2;
                                                       guard (x38 == Cons x7 x8);
                                                       return ()}]
r1OI x1 gen_neqIO_x2 gen_r10IIO_x2 = msum [do {guard (x1 == Nil);
                                               let {x0 = Nil};
                                               return x0},
                                           do {(x2, x3) <- case x1 of
                                                           {Cons y2 y3 -> return (y2, y3);
                                                            _ -> mzero};
                                               let {x36 = x2};
                                               x4 <- r1OI x3 gen_neqIO_x2 gen_r10IIO_x2;
                                               let {x35 = Cons x36 x4};
                                               let {x0 = Cons x2 x35};
                                               return x0},
                                           do {(x5, x6) <- case x1 of
                                                           {Cons y5 y6 -> return (y5, y6);
                                                            _ -> mzero};
                                               x7 <- neqIO x5 gen_neqIO_x2;
                                               x8 <- r10IIO x6 x7 gen_neqIO_x2 gen_r10IIO_x2;
                                               let {x37 = Cons x7 x8};
                                               let {x0 = Cons x5 x37};
                                               return x0}]
neqIO x0 gen_neqIO_x2 = msum [do {guard (x0 == O);
                                  (x1, x2) <- do {x2 <- gen_neqIO_x2;
                                                  let {x1 = S x2};
                                                  return (x1, x2)};
                                  return x1},
                              do {let {x1 = O};
                                  x3 <- case x0 of
                                        {S y3 -> return y3; _ -> mzero};
                                  return x1},
                              do {x5 <- case x0 of
                                        {S y5 -> return y5; _ -> mzero};
                                  x4 <- neqIO x5 gen_neqIO_x2;
                                  let {x1 = S x4};
                                  return x1}]
r10IIO x0 x1 gen_neqIO_x2 gen_r10IIO_x2 = msum [do {x38 <- r1OI x6 gen_neqIO_x2 gen_r10IIO_x2;
                                                    x8 <- case x38 of
                                                          {Cons y7 y8 -> do {guard (x7 == y7);
                                                                             return y8};
                                                           _ -> mzero};
                                                    x2 <- gen_r10IIO_x2;
                                                    return x2}]
r2O = msum [do {let {x41 = O};
                let {x40 = S x41};
                let {x39 = S x40};
                let {x45 = O};
                let {x44 = S x45};
                let {x43 = S x44};
                let {x47 = O};
                let {x49 = O};
                let {x51 = O};
                let {x53 = O};
                let {x54 = Nil};
                let {x52 = Cons x53 x54};
                let {x50 = Cons x51 x52};
                let {x48 = Cons x49 x50};
                let {x46 = Cons x47 x48};
                let {x42 = Cons x43 x46};
                let {x0 = Cons x39 x42};
                return x0}]
rrO gen_neqIO_x2 gen_r0O_x31 gen_r10IIO_x2 = msum [do {x1 <- rO gen_r0O_x31;
                                                       x0 <- r1OI x1 gen_neqIO_x2 gen_r10IIO_x2;
                                                       return x0}]