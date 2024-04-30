module Contains1_simple where

import Stream
import Control.Monad
import Term

containsoIO x0 gen_appendo1IOO_x1 gen_conoOII_x0 = Immature $ msum [do {let {x10 = Nil};
                                                             x1 <- conoOII x10 x0 gen_appendo1IOO_x1 gen_conoOII_x0;
                                                             return x1}]
conoOII x1 x2 gen_appendo1IOO_x1 gen_conoOII_x0 = Immature $ msum [do {guard (x2 == Nil);
                                                            x0 <- gen_conoOII_x0;
                                                            return x0},
                                                        do {(x5,
                                                             x3,
                                                             x4) <- newoOIIOO x1 x2 gen_appendo1IOO_x1;
                                                            x6 <- conoOII x3 x4 gen_appendo1IOO_x1 gen_conoOII_x0;
                                                            let {x0 = Cons x5 x6};
                                                            return x0}]
newoOIIOO x1 x2 gen_appendo1IOO_x1 = Immature $ msum [do {let {x14 = Nil};
                                               (x11, x12) <- case x2 of
                                                             {Cons y11 y12 -> return (y11, y12);
                                                              _ -> mzero};
                                               let {x0 = x11};
                                               let {x13 = Cons x0 x14};
                                               let {x4 = x12};
                                               x3 <- appendo1IIO x1 x13;
                                               return (x0, x3, x4)},
                                           do {let {x16 = Nil};
                                               (x6, x5) <- case x2 of
                                                           {Cons y6 y5 -> return (y6, y5);
                                                            _ -> mzero};
                                               let {x17 = Cons x6 x5};
                                               (x15, x7) <- appendo1IOO x1 gen_appendo1IOO_x1;
                                               x0 <- case x15 of
                                                     {Cons y0 y16 -> do {guard (x16 == y16);
                                                                         return y0};
                                                      _ -> mzero};
                                               (x3, x8) <- appendo2OOI x1;
                                               x9 <- appendo2OII x3 x7;
                                               x4 <- appendo1IIO x8 x17;
                                               return (x0, x3, x4)}]
appendo1IIO x0 x1 = Immature $ msum [do {guard (x0 == Nil);
                              let {x2 = x1};
                              return x2},
                          do {(x3, x4) <- case x0 of
                                          {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                              let {x18 = x3};
                              x5 <- appendo1IIO x4 x1;
                              let {x2 = Cons x18 x5};
                              return x2}]
appendo1IOO x0 gen_appendo1IOO_x1 = Immature $ msum [do {guard (x0 == Nil);
                                              (x2, x1) <- do {x1 <- gen_appendo1IOO_x1;
                                                              return (x1, x1)};
                                              return (x1, x2)},
                                          do {(x3, x4) <- case x0 of
                                                          {Cons y3 y4 -> return (y3, y4);
                                                           _ -> mzero};
                                              let {x18 = x3};
                                              (x1, x5) <- appendo1IOO x4 gen_appendo1IOO_x1;
                                              let {x2 = Cons x18 x5};
                                              return (x1, x2)}]
appendo2OII x1 x2 = Immature $ msum [do {guard (x2 == x1);
                              let {x0 = Nil};
                              return x0},
                          do {(x19, x5) <- case x2 of
                                           {Cons y19 y5 -> return (y19, y5); _ -> mzero};
                              let {x3 = x19};
                              x4 <- appendo2OII x1 x5;
                              let {x0 = Cons x3 x4};
                              return x0}]
appendo2OOI x2 = Immature $ msum [do {let {x0 = Nil};
                           let {x1 = x2};
                           return (x0, x1)},
                       do {(x19, x5) <- case x2 of
                                        {Cons y19 y5 -> return (y19, y5); _ -> mzero};
                           let {x3 = x19};
                           (x4, x1) <- appendo2OOI x5;
                           let {x0 = Cons x3 x4};
                           return (x0, x1)}]
containso x0 gen_appendo1IOO_x1 gen_conoOII_x0 = containsoIO x0 gen_appendo1IOO_x1 gen_conoOII_x0