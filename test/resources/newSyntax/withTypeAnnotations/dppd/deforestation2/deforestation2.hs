module Deforestation2_for_cpd where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
rrII x0 x1 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r1IIIO_x3 gen_r2IOI_x1 = msum [do {x2 <- rOI x1 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1;
                                                                                     rII x0 x2 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r1IIIO_x3 gen_r2IOI_x1;
                                                                                     return ()}]
rII x0 x1 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r1IIIO_x3 gen_r2IOI_x1 = msum [do {guard (x0 == Nil);
                                                                                    guard (x1 == Nil);
                                                                                    return ()},
                                                                                do {r0I x0;
                                                                                    (x2,
                                                                                     x4) <- case x1 of
                                                                                            {Cons y2
                                                                                                  y4 -> return (y2,
                                                                                                                y4);
                                                                                             _ -> mzero};
                                                                                    x3 <- rOI x4 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1;
                                                                                    return ()},
                                                                                do {(x2,
                                                                                     x5) <- case x1 of
                                                                                            {Cons y2
                                                                                                  y5 -> return (y2,
                                                                                                                y5);
                                                                                             _ -> mzero};
                                                                                    x3 <- neqIO x2 gen_neqIO_x2;
                                                                                    x4 <- r1IIIO x0 x2 x3 gen_r1IIIO_x3;
                                                                                    r2III x3 x4 x5 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1;
                                                                                    return ()}]
neqIO x0 gen_neqIO_x2 = msum [do {guard (x0 == O);
                                  (x1, x2) <- do {x2 <- gen_neqIO_x2;
                                                  let {x1 = S x2};
                                                  return (x1, x2)};
                                  return x1},
                              do {x2 <- case x0 of
                                        {S y2 -> return y2; _ -> mzero};
                                  let {x1 = O};
                                  return x1},
                              do {x2 <- case x0 of
                                        {S y2 -> return y2; _ -> mzero};
                                  x3 <- neqIO x2 gen_neqIO_x2;
                                  let {x1 = S x3};
                                  return x1}]
rOI x1 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1 = msum [do {let {x0 = Nil};
                                                                   guard (x1 == Nil);
                                                                   return x0},
                                                               do {x0 <- r0O gen_r0O_x2 gen_r0O_x3;
                                                                   (x2, x4) <- case x1 of
                                                                               {Cons y2
                                                                                     y4 -> return (y2,
                                                                                                   y4);
                                                                                _ -> mzero};
                                                                   x3 <- rOI x4 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1;
                                                                   return x0},
                                                               do {(x2, x5) <- case x1 of
                                                                               {Cons y2
                                                                                     y5 -> return (y2,
                                                                                                   y5);
                                                                                _ -> mzero};
                                                                   x3 <- neqIO x2 gen_neqIO_x2;
                                                                   x4 <- r2IOI x3 x5 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1;
                                                                   x0 <- r1OIII x2 x3 x4;
                                                                   return x0}]
r0I x0 = msum [do {(x2, x3) <- case x0 of
                               {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                   x4 <- case x3 of
                         {Cons y4 y3 -> do {guard (x3 == y3); return y4}; _ -> mzero};
                   guard (x4 == x2);
                   return ()}]
r0O gen_r0O_x2 gen_r0O_x3 = msum [do {(x0,
                                       x2,
                                       x3) <- do {x2 <- gen_r0O_x2;
                                                  x3 <- gen_r0O_x3;
                                                  let {x0 = Cons x2 x3};
                                                  return (x0, x2, x3)};
                                      x4 <- case x3 of
                                            {Cons y4 y3 -> do {guard (x3 == y3); return y4};
                                             _ -> mzero};
                                      guard (x4 == x2);
                                      return x0}]
r1IIIO x0 x1 x2 gen_r1IIIO_x3 = msum [do {x5 <- case x0 of
                                                {Cons y2 y5 -> do {guard (x2 == y2); return y5};
                                                 _ -> mzero};
                                          x4 <- case x5 of
                                                {Cons y3 y4 -> do {guard (x3 == y3); return y4};
                                                 _ -> mzero};
                                          x3 <- gen_r1IIIO_x3;
                                          return x3}]
r1OIII x1 x2 x3 = msum [do {let {x5 = Cons x3 x4};
                            let {x0 = Cons x2 x5};
                            return x0}]
r2III x0 x1 x2 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1 = msum [do {x6 <- rOI x5 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1;
                                                                           guard (x6 == Cons x3 x4);
                                                                           return ()}]
r2IOI x0 x2 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1 = msum [do {x6 <- rOI x5 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1;
                                                                        x4 <- case x6 of
                                                                              {Cons y3
                                                                                    y4 -> do {guard (x3 == y3);
                                                                                              return y4};
                                                                               _ -> mzero};
                                                                        x1 <- gen_r2IOI_x1;
                                                                        return x1}]
rrIO x0 gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2 = msum [do {x2 <- rIO x0 gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2;
                                                                                                                                     x1 <- rIO x2 gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2;
                                                                                                                                     return x1}]
rIO x0 gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2 = msum [do {guard (x0 == Nil);
                                                                                                                                    let {x1 = Nil};
                                                                                                                                    return x1},
                                                                                                                                do {r0I x0;
                                                                                                                                    (x3,
                                                                                                                                     x4) <- rOO gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2;
                                                                                                                                    (x1,
                                                                                                                                     x2) <- do {x2 <- gen_rIO_x2;
                                                                                                                                                let {x1 = Cons x2 x4};
                                                                                                                                                return (x1,
                                                                                                                                                        x2)};
                                                                                                                                    return x1},
                                                                                                                                do {(x2,
                                                                                                                                     x3,
                                                                                                                                     x4) <- r1IOOO x0 gen_r1IOOO_x1;
                                                                                                                                    neqII x2 x3;
                                                                                                                                    x5 <- r2IIO x3 x4 gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2;
                                                                                                                                    let {x1 = Cons x2 x5};
                                                                                                                                    return x1}]
neqII x0 x1 = msum [do {guard (x0 == O);
                        x2 <- case x1 of
                              {S y2 -> return y2; _ -> mzero};
                        return ()},
                    do {x2 <- case x0 of
                              {S y2 -> return y2; _ -> mzero};
                        guard (x1 == O);
                        return ()},
                    do {x2 <- case x0 of
                              {S y2 -> return y2; _ -> mzero};
                        x3 <- case x1 of
                              {S y3 -> return y3; _ -> mzero};
                        neqII x2 x3;
                        return ()}]
rOO gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2 = msum [do {let {x0 = Nil};
                                                                                                                                 let {x1 = Nil};
                                                                                                                                 return (x0,
                                                                                                                                         x1)},
                                                                                                                             do {x0 <- r0O gen_r0O_x2 gen_r0O_x3;
                                                                                                                                 (x3,
                                                                                                                                  x4) <- rOO gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2;
                                                                                                                                 (x1,
                                                                                                                                  x2) <- do {x2 <- gen_rOO_x2;
                                                                                                                                             let {x1 = Cons x2 x4};
                                                                                                                                             return (x1,
                                                                                                                                                     x2)};
                                                                                                                                 return (x0,
                                                                                                                                         x1)},
                                                                                                                             do {(x0,
                                                                                                                                  x2,
                                                                                                                                  x3,
                                                                                                                                  x4) <- r1OOOO gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5;
                                                                                                                                 neqII x2 x3;
                                                                                                                                 x5 <- r2IIO x3 x4 gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2;
                                                                                                                                 let {x1 = Cons x2 x5};
                                                                                                                                 return (x0,
                                                                                                                                         x1)}]
r1IOOO x0 gen_r1IOOO_x1 = msum [do {(x2, x5) <- case x0 of
                                                {Cons y2 y5 -> return (y2, y5); _ -> mzero};
                                    (x3, x4) <- case x5 of
                                                {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                    x1 <- gen_r1IOOO_x1;
                                    return (x1, x2, x3)}]
r1OOOO gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 = msum [do {(x0,
                                                              x2,
                                                              x5) <- do {x2 <- gen_r1OOOO_x2;
                                                                         x5 <- gen_r1OOOO_x5;
                                                                         let {x0 = Cons x2 x5};
                                                                         return (x0, x2, x5)};
                                                             (x3, x4) <- case x5 of
                                                                         {Cons y3 y4 -> return (y3,
                                                                                                y4);
                                                                          _ -> mzero};
                                                             x1 <- gen_r1OOOO_x1;
                                                             return (x0, x1, x2, x3)}]
r2IIO x0 x1 gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2 = msum [do {let {x6 = Cons x3 x4};
                                                                                                                                         x5 <- rIO x6 gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2;
                                                                                                                                         x2 <- gen_r2IIO_x2;
                                                                                                                                         return x2}]
rrOI x1 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1 = msum [do {x2 <- rOI x1 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1;
                                                                    x0 <- rOI x2 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1;
                                                                    return x0}]
rrOO gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_r2IOI_x1 gen_rIO_x2 gen_rOO_x2 = msum [do {(x2,
                                                                                                                                                             x1) <- rOO gen_r0O_x2 gen_r0O_x3 gen_r1IOOO_x1 gen_r1OOOO_x1 gen_r1OOOO_x2 gen_r1OOOO_x5 gen_r2IIO_x2 gen_rIO_x2 gen_rOO_x2;
                                                                                                                                                            x0 <- rOI x2 gen_neqIO_x2 gen_r0O_x2 gen_r0O_x3 gen_r2IOI_x1;
                                                                                                                                                            return (x0,
                                                                                                                                                                    x1)}]