module Remove2_simple where

import Stream
import Control.Monad
import Term

rrOI x1 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2 = Immature $ msum [do {x2 <- fOI x1 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2;
                                                                x0 <- fOI x2 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2;
                                                                return x0}]
fOI x1 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2 = Immature $ msum [do {let {x0 = Nil};
                                                               guard (x1 == Nil);
                                                               return x0},
                                                           do {(x2,
                                                                x3) <- hOOI x1 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2;
                                                               let {x0 = Cons x2 x3};
                                                               return x0}]
hOOI x2 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2 = Immature $ msum [do {let {x1 = Nil};
                                                                let {x7 = Nil};
                                                                (x8, x9) <- case x2 of
                                                                            {Cons y8
                                                                                  y9 -> return (y8,
                                                                                                y9);
                                                                             _ -> mzero};
                                                                guard (x9 == x7);
                                                                let {x0 = x8};
                                                                return (x0, x1)},
                                                            do {(x0,
                                                                 x3,
                                                                 x1,
                                                                 x4) <- gOOOOI x2 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2;
                                                                guard (x1 == Cons x3 x4);
                                                                return (x0, x1)}]
gOOOOI x4 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2 = Immature $ msum [do {(x5,
                                                                   x6) <- case x4 of
                                                                          {Cons y5 y6 -> return (y5,
                                                                                                 y6);
                                                                           _ -> mzero};
                                                                  let {x0 = x5};
                                                                  (x1,
                                                                   x2,
                                                                   x3) <- g0IOOOI x0 x6 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2;
                                                                  return (x0, x1, x2, x3)}]
g0IOOOI x0 x6 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2 = Immature $ msum [do {let {x1 = x0};
                                                                      x3 <- fOI x6 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2;
                                                                      x2 <- gen_g0IOOOI_x2;
                                                                      return (x1, x2, x3)},
                                                                  do {x1 <- neqIO x0 gen_neq0IO_x2;
                                                                      x2 <- fOI x6 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2;
                                                                      x3 <- gen_g0IOOOI_x3;
                                                                      return (x1, x2, x3)}]
neqIO x0 gen_neq0IO_x2 = Immature $ msum [do {x1 <- neq0IO x0 gen_neq0IO_x2;
                                   return x1},
                               do {x2 <- case x0 of
                                         {Succ y2 -> return y2; _ -> mzero};
                                   x3 <- neqIO x2 gen_neq0IO_x2;
                                   let {x1 = Succ x3};
                                   return x1}]
neq0IO x0 gen_neq0IO_x2 = Immature $ msum [do {guard (x0 == Zero);
                                    (x1, x2) <- do {x2 <- gen_neq0IO_x2;
                                                    let {x1 = Succ x2};
                                                    return (x1, x2)};
                                    return x1},
                                do {let {x1 = Zero};
                                    x2 <- case x0 of
                                          {Succ y2 -> return y2; _ -> mzero};
                                    return x1}]
rr x1 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2 = rrOI x1 gen_g0IOOOI_x2 gen_g0IOOOI_x3 gen_neq0IO_x2