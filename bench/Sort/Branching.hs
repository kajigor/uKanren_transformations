module Branching where

import Stream
import Control.Monad
import Simple (Term (..))

sortoI x0 gen_gtIO_x2 = msum [do {let {x11 = Zero};
                                  (x12, x1) <- case x0 of
                                               {Cons y12 y1 -> return (y12, y1); _ -> mzero};
                                  guard (x12 == x11);
                                  (x2, x3) <- sortoAppendoAppendoOOI x1 gen_gtIO_x2;
                                  x4 <- __sortoOI x3 gen_gtIO_x2;
                                  x5 <- __sortoOI x2 gen_gtIO_x2;
                                  return ()}]
__sortoOI x1 gen_gtIO_x2 = msum [do {let {x0 = Nil};
                                     guard (x1 == Nil);
                                     return x0},
                                 do {(x2, x3, x4, x5, x6) <- splitoAppendoIOOOOO x1 gen_gtIO_x2;
                                     __sortoII x4 x5 gen_gtIO_x2;
                                     let {x0 = Cons x2 x3};
                                     x7 <- __sortoOI x6 gen_gtIO_x2;
                                     return x0}]
__sortoII x0 x1 gen_gtIO_x2 = msum [do {guard (x1 == Nil);
                                        guard (x0 == Nil);
                                        return ()},
                                    do {(x2, x3) <- case x0 of
                                                    {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                        (x4, x5, x6) <- splitoAppendoIIIOOO x1 x2 x3;
                                        __sortoII x4 x5 gen_gtIO_x2;
                                        x7 <- __sortoOI x6 gen_gtIO_x2;
                                        return ()}]
sortoO gen_gtIO_x2 gen_sortoO_x1 = msum [do {let {x11 = Zero};
                                             let {x12 = x11};
                                             (x0, x1) <- do {x1 <- gen_sortoO_x1;
                                                             let {x0 = Cons x12 x1};
                                                             return (x0, x1)};
                                             (x2, x3) <- sortoAppendoAppendoOOI x1 gen_gtIO_x2;
                                             x4 <- __sortoOI x3 gen_gtIO_x2;
                                             x5 <- __sortoOI x2 gen_gtIO_x2;
                                             return x0}]
sortoAppendoAppendoOOI x2 gen_gtIO_x2 = msum [do {let {x13 = Zero};
                                                  let {x16 = Zero};
                                                  let {x15 = Succ x16};
                                                  (x14, x3) <- case x2 of
                                                               {Cons y14 y3 -> return (y14, y3);
                                                                _ -> mzero};
                                                  guard (x14 == x13);
                                                  (x1, x0) <- _appendoIIOO x3 x15;
                                                  return (x0, x1)},
                                              do {(x0, x1, x4) <- appendoAppendoOOOI x2;
                                                  (x5,
                                                   x6,
                                                   x7,
                                                   x8,
                                                   x9) <- splitoAppendoIOOOOO x4 gen_gtIO_x2;
                                                  __sortoII x7 x8 gen_gtIO_x2;
                                                  x10 <- __sortoOI x9 gen_gtIO_x2;
                                                  return (x0, x1)}]
_appendoIIOO x0 x1 = msum [do {let {x2 = Nil};
                               (x24, x25) <- case x0 of
                                             {Cons y24 y25 -> return (y24, y25); _ -> mzero};
                               guard (x24 == x1);
                               let {x3 = x25};
                               return (x2, x3)},
                           do {(x26, x6) <- case x0 of
                                            {Cons y26 y6 -> return (y26, y6); _ -> mzero};
                               let {x4 = x26};
                               (x5, x3) <- _appendoIIOO x6 x1;
                               let {x2 = Cons x4 x5};
                               return (x2, x3)}]
appendoAppendoOOOI x3 = msum [do {let {x2 = Nil};
                                  let {x18 = Zero};
                                  let {x17 = Succ x18};
                                  let {x20 = Zero};
                                  (x19, x0) <- _appendoIIOO x3 x17;
                                  x1 <- case x19 of
                                        {Cons y20 y1 -> do {guard (x20 == y20); return y1};
                                         _ -> mzero};
                                  return (x0, x1, x2)},
                              do {(x4, x5) <- case x3 of
                                              {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                  let {x21 = x4};
                                  (x0, x1, x6) <- appendoAppendoOOOI x5;
                                  let {x2 = Cons x21 x6};
                                  return (x0, x1, x2)}]
splitoAppendoIIIOOO x0 x1 x2 = msum [do {let {x3 = Nil};
                                         guard (x2 == Nil);
                                         (x4, x5) <- _appendoIIOO x0 x1;
                                         return (x3, x4, x5)},
                                     do {(x22, x23) <- case x2 of
                                                       {Cons y22 y23 -> return (y22, y23);
                                                        _ -> mzero};
                                         let {x6 = x22};
                                         leII x1 x6;
                                         let {x8 = x23};
                                         (x7, x4, x5) <- splitoAppendoIIIOOO x0 x1 x8;
                                         let {x3 = Cons x6 x7};
                                         return (x3, x4, x5)},
                                     do {(x6, x8) <- case x2 of
                                                     {Cons y6 y8 -> return (y6, y8); _ -> mzero};
                                         gtII x1 x6;
                                         (x3, x4, x5) <- splitoAppendoIIIOOO x0 x1 x8;
                                         return (x3, x4, x5)}]
gtII x0 x1 = msum [do {x2 <- case x1 of
                             {Succ y2 -> return y2; _ -> mzero};
                       guard (x0 == Zero);
                       return ()},
                   do {x3 <- case x1 of
                             {Succ y3 -> return y3; _ -> mzero};
                       x4 <- case x0 of
                             {Succ y4 -> return y4; _ -> mzero};
                       gtII x4 x3;
                       return ()}]
leII x0 x1 = msum [do {guard (x1 == Zero); return ()},
                   do {x2 <- case x1 of
                             {Succ y2 -> return y2; _ -> mzero};
                       x3 <- case x0 of
                             {Succ y3 -> return y3; _ -> mzero};
                       leII x3 x2;
                       return ()}]
splitoAppendoIOOOOO x0 gen_gtIO_x2 = msum [do {let {x3 = Nil};
                                               let {x2 = Nil};
                                               (x1, x4, x5) <- _appendoIOOO x0;
                                               return (x1, x2, x3, x4, x5)},
                                           do {(x1,
                                                x8,
                                                x7,
                                                x4,
                                                x5) <- splitoAppendoIOOOOO x0 gen_gtIO_x2;
                                               let {x23 = x8};
                                               x6 <- leIO x1;
                                               let {x3 = Cons x6 x7};
                                               let {x22 = x6};
                                               let {x2 = Cons x22 x23};
                                               return (x1, x2, x3, x4, x5)},
                                           do {(x1,
                                                x8,
                                                x3,
                                                x4,
                                                x5) <- splitoAppendoIOOOOO x0 gen_gtIO_x2;
                                               x6 <- gtIO x1 gen_gtIO_x2;
                                               let {x2 = Cons x6 x8};
                                               return (x1, x2, x3, x4, x5)}]
_appendoIOOO x0 = msum [do {let {x2 = Nil};
                            (x24, x25) <- case x0 of
                                          {Cons y24 y25 -> return (y24, y25); _ -> mzero};
                            let {x1 = x24};
                            let {x3 = x25};
                            return (x1, x2, x3)},
                        do {(x26, x6) <- case x0 of
                                         {Cons y26 y6 -> return (y26, y6); _ -> mzero};
                            let {x4 = x26};
                            (x1, x5, x3) <- _appendoIOOO x6;
                            let {x2 = Cons x4 x5};
                            return (x1, x2, x3)}]
gtIO x0 gen_gtIO_x2 = msum [do {guard (x0 == Zero);
                                (x1, x2) <- do {x2 <- gen_gtIO_x2;
                                                let {x1 = Succ x2};
                                                return (x1, x2)};
                                return x1},
                            do {x4 <- case x0 of
                                      {Succ y4 -> return y4; _ -> mzero};
                                x3 <- gtIO x4 gen_gtIO_x2;
                                let {x1 = Succ x3};
                                return x1}]
leIO x0 = msum [do {let {x1 = Zero}; return x1},
                do {x3 <- case x0 of
                          {Succ y3 -> return y3; _ -> mzero};
                    x2 <- leIO x3;
                    let {x1 = Succ x2};
                    return x1}]