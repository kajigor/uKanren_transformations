module RelSort_simple where

import Stream
import Control.Monad
import Term

sortoII x0 x1 = Immature $ msum [do {guard (x0 == Nil);
                          guard (x1 == Nil);
                          return ()},
                      do {(x2, x4) <- case x1 of
                                      {Cons y2 y4 -> return (y2, y4); _ -> mzero};
                          x3 <- sortoOI x4;
                          smallestoIII x0 x2 x3;
                          return ()}]
smallestoIII x0 x1 x2 = Immature $ msum [do {let {x10 = Nil};
                                  (x11, x12) <- case x0 of
                                                {Cons y11 y12 -> return (y11, y12); _ -> mzero};
                                  guard (x11 == x1);
                                  guard (x12 == x10);
                                  guard (x2 == Nil);
                                  return ()},
                              do {(x7, x6) <- case x2 of
                                              {Cons y7 y6 -> return (y7, y6); _ -> mzero};
                                  (x13, x14) <- case x0 of
                                                {Cons y13 y14 -> return (y13, y14); _ -> mzero};
                                  let {x3 = x13};
                                  let {x4 = x14};
                                  x5 <- minmaxoIOII x3 x1 x7;
                                  smallestoIII x4 x5 x6;
                                  return ()}]
minmaxoIOII x0 x2 x3 = Immature $ msum [do {guard (x2 == x0);
                                 let {x8 = Trueo};
                                 let {x1 = x3};
                                 leoIII x0 x1 x8;
                                 return x1},
                             do {guard (x3 == x0);
                                 let {x9 = Trueo};
                                 let {x1 = x2};
                                 gtoIII x0 x1 x9;
                                 return x1}]
gtoIII x0 x1 x2 = Immature $ msum [do {guard (x0 == Zero);
                            guard (x2 == Falso);
                            return ()},
                        do {x3 <- case x0 of
                                  {Succ y3 -> return y3; _ -> mzero};
                            guard (x1 == Zero);
                            guard (x2 == Trueo);
                            return ()},
                        do {x3 <- case x0 of
                                  {Succ y3 -> return y3; _ -> mzero};
                            x4 <- case x1 of
                                  {Succ y4 -> return y4; _ -> mzero};
                            gtoIII x3 x4 x2;
                            return ()}]
leoIII x0 x1 x2 = Immature $ msum [do {guard (x0 == Zero);
                            guard (x2 == Trueo);
                            return ()},
                        do {x3 <- case x0 of
                                  {Succ y3 -> return y3; _ -> mzero};
                            guard (x1 == Zero);
                            guard (x2 == Falso);
                            return ()},
                        do {x3 <- case x0 of
                                  {Succ y3 -> return y3; _ -> mzero};
                            x4 <- case x1 of
                                  {Succ y4 -> return y4; _ -> mzero};
                            leoIII x3 x4 x2;
                            return ()}]
sortoIO x0 = Immature $ msum [do {let {x1 = Nil};
                       guard (x0 == Nil);
                       return x1},
                   do {(x2, x3) <- smallestoIOO x0;
                       x4 <- sortoIO x3;
                       let {x1 = Cons x2 x4};
                       return x1}]
smallestoIOO x0 = Immature $ msum [do {let {x10 = Nil};
                            let {x2 = Nil};
                            (x11, x12) <- case x0 of
                                          {Cons y11 y12 -> return (y11, y12); _ -> mzero};
                            guard (x12 == x10);
                            let {x1 = x11};
                            return (x1, x2)},
                        do {(x13, x14) <- case x0 of
                                          {Cons y13 y14 -> return (y13, y14); _ -> mzero};
                            let {x3 = x13};
                            let {x4 = x14};
                            (x5, x6) <- smallestoIOO x4;
                            (x1, x7) <- minmaxoIIOO x3 x5;
                            let {x2 = Cons x7 x6};
                            return (x1, x2)}]
minmaxoIIOO x0 x1 = Immature $ msum [do {let {x8 = Trueo};
                              leoIII x0 x1 x8;
                              let {x2 = x0};
                              let {x3 = x1};
                              return (x2, x3)},
                          do {let {x9 = Trueo};
                              gtoIII x0 x1 x9;
                              let {x3 = x0};
                              let {x2 = x1};
                              return (x2, x3)}]
sortoOI x1 = Immature $ msum [do {let {x0 = Nil};
                       guard (x1 == Nil);
                       return x0},
                   do {(x2, x4) <- case x1 of
                                   {Cons y2 y4 -> return (y2, y4); _ -> mzero};
                       x3 <- sortoOI x4;
                       x0 <- smallestoOII x2 x3;
                       return x0}]
smallestoOII x1 x2 = Immature $ msum [do {let {x10 = Nil};
                               guard (x2 == Nil);
                               let {x11 = x1};
                               let {x12 = x10};
                               let {x0 = Cons x11 x12};
                               return x0},
                           do {(x7, x6) <- case x2 of
                                           {Cons y7 y6 -> return (y7, y6); _ -> mzero};
                               (x3, x5) <- minmaxoOOII x1 x7;
                               let {x13 = x3};
                               x4 <- smallestoOII x5 x6;
                               let {x14 = x4};
                               let {x0 = Cons x13 x14};
                               return x0}]
minmaxoOOII x2 x3 = Immature $ msum [do {let {x8 = Trueo};
                              let {x0 = x2};
                              let {x1 = x3};
                              leoIII x0 x1 x8;
                              return (x0, x1)},
                          do {let {x9 = Trueo};
                              let {x0 = x3};
                              let {x1 = x2};
                              gtoIII x0 x1 x9;
                              return (x0, x1)}]
sortoOO gen_leoOII_x3 gen_smallestoOOI_x1 = Immature $ msum [do {let {x0 = Nil};
                                                      let {x1 = Nil};
                                                      return (x0, x1)},
                                                  do {(x3,
                                                       x4) <- sortoOO gen_leoOII_x3 gen_smallestoOOI_x1;
                                                      (x0,
                                                       x2) <- smallestoOOI x3 gen_leoOII_x3 gen_smallestoOOI_x1;
                                                      let {x1 = Cons x2 x4};
                                                      return (x0, x1)}]
smallestoOOI x2 gen_leoOII_x3 gen_smallestoOOI_x1 = Immature $ msum [do {let {x10 = Nil};
                                                              guard (x2 == Nil);
                                                              let {x12 = x10};
                                                              (x11,
                                                               x1) <- do {x1 <- gen_smallestoOOI_x1;
                                                                          return (x1, x1)};
                                                              let {x0 = Cons x11 x12};
                                                              return (x0, x1)},
                                                          do {(x7, x6) <- case x2 of
                                                                          {Cons y7 y6 -> return (y7,
                                                                                                 y6);
                                                                           _ -> mzero};
                                                              (x4,
                                                               x5) <- smallestoOOI x6 gen_leoOII_x3 gen_smallestoOOI_x1;
                                                              let {x14 = x4};
                                                              (x3,
                                                               x1) <- minmaxoOIOI x5 x7 gen_leoOII_x3;
                                                              let {x13 = x3};
                                                              let {x0 = Cons x13 x14};
                                                              return (x0, x1)}]
minmaxoOIOI x1 x3 gen_leoOII_x3 = Immature $ msum [do {guard (x1 == x3);
                                            let {x8 = Trueo};
                                            x0 <- leoOII x1 x8 gen_leoOII_x3;
                                            let {x2 = x0};
                                            return (x0, x2)},
                                        do {let {x9 = Trueo};
                                            let {x0 = x3};
                                            gtoIII x0 x1 x9;
                                            let {x2 = x1};
                                            return (x0, x2)}]
leoOII x1 x2 gen_leoOII_x3 = Immature $ msum [do {let {x0 = Zero};
                                       guard (x2 == Trueo);
                                       return x0},
                                   do {guard (x1 == Zero);
                                       guard (x2 == Falso);
                                       (x0, x3) <- do {x3 <- gen_leoOII_x3;
                                                       let {x0 = Succ x3};
                                                       return (x0, x3)};
                                       return x0},
                                   do {x4 <- case x1 of
                                             {Succ y4 -> return y4; _ -> mzero};
                                       x3 <- leoOII x4 x2 gen_leoOII_x3;
                                       let {x0 = Succ x3};
                                       return x0}]