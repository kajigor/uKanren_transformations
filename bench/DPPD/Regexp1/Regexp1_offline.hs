module Regexp1_offline where

import Stream
import Control.Monad
import Term

generatesdsIIIII x0 x1 x2 x3 x4 = Immature $ msum [do {guard (x4 == Nil);
                                            return ()},
                                        do {(x7, x8) <- case x4 of
                                                        {Cons y7 y8 -> return (y7, y8); _ -> mzero};
                                            guard (x7 == x0);
                                            let {x6 = x8};
                                            x5 <- case x6 of
                                                  {Cons y2 y5 -> do {guard (x2 == y2); return y5};
                                                   _ -> mzero};
                                            generatesdsIIIII x0 x1 x2 x3 x5;
                                            return ()},
                                        do {(x10, x11) <- case x4 of
                                                          {Cons y10 y11 -> return (y10, y11);
                                                           _ -> mzero};
                                            guard (x10 == x1);
                                            let {x9 = x11};
                                            x5 <- case x9 of
                                                  {Cons y2 y5 -> do {guard (x2 == y2); return y5};
                                                   _ -> mzero};
                                            generatesdsIIIII x0 x1 x2 x3 x5;
                                            return ()},
                                        do {(x13, x14) <- case x4 of
                                                          {Cons y13 y14 -> return (y13, y14);
                                                           _ -> mzero};
                                            guard (x13 == x0);
                                            let {x12 = x14};
                                            x5 <- case x12 of
                                                  {Cons y3 y5 -> do {guard (x3 == y3); return y5};
                                                   _ -> mzero};
                                            generatesdsIIIII x0 x1 x2 x3 x5;
                                            return ()},
                                        do {(x16, x17) <- case x4 of
                                                          {Cons y16 y17 -> return (y16, y17);
                                                           _ -> mzero};
                                            guard (x16 == x1);
                                            let {x15 = x17};
                                            x5 <- case x15 of
                                                  {Cons y3 y5 -> do {guard (x3 == y3); return y5};
                                                   _ -> mzero};
                                            generatesdsIIIII x0 x1 x2 x3 x5;
                                            return ()}]
generatesdsIIIIO x0 x1 x2 x3 = Immature $ msum [do {let {x4 = Nil};
                                         return x4},
                                     do {let {x7 = x0};
                                         x5 <- generatesdsIIIIO x0 x1 x2 x3;
                                         let {x6 = Cons x2 x5};
                                         let {x8 = x6};
                                         let {x4 = Cons x7 x8};
                                         return x4},
                                     do {let {x10 = x1};
                                         x5 <- generatesdsIIIIO x0 x1 x2 x3;
                                         let {x9 = Cons x2 x5};
                                         let {x11 = x9};
                                         let {x4 = Cons x10 x11};
                                         return x4},
                                     do {let {x13 = x0};
                                         x5 <- generatesdsIIIIO x0 x1 x2 x3;
                                         let {x12 = Cons x3 x5};
                                         let {x14 = x12};
                                         let {x4 = Cons x13 x14};
                                         return x4},
                                     do {let {x16 = x1};
                                         x5 <- generatesdsIIIIO x0 x1 x2 x3;
                                         let {x15 = Cons x3 x5};
                                         let {x17 = x15};
                                         let {x4 = Cons x16 x17};
                                         return x4}]
generatesdsIIIOI x0 x1 x2 x4 gen_generatesdsIIIOI_x3 = Immature $ msum [do {guard (x4 == Nil);
                                                                 x3 <- gen_generatesdsIIIOI_x3;
                                                                 return x3},
                                                             do {(x7, x8) <- case x4 of
                                                                             {Cons y7
                                                                                   y8 -> return (y7,
                                                                                                 y8);
                                                                              _ -> mzero};
                                                                 guard (x7 == x0);
                                                                 let {x6 = x8};
                                                                 x5 <- case x6 of
                                                                       {Cons y2
                                                                             y5 -> do {guard (x2 == y2);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 x3 <- generatesdsIIIOI x0 x1 x2 x5 gen_generatesdsIIIOI_x3;
                                                                 return x3},
                                                             do {(x10, x11) <- case x4 of
                                                                               {Cons y10
                                                                                     y11 -> return (y10,
                                                                                                    y11);
                                                                                _ -> mzero};
                                                                 guard (x10 == x1);
                                                                 let {x9 = x11};
                                                                 x5 <- case x9 of
                                                                       {Cons y2
                                                                             y5 -> do {guard (x2 == y2);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 x3 <- generatesdsIIIOI x0 x1 x2 x5 gen_generatesdsIIIOI_x3;
                                                                 return x3},
                                                             do {(x13, x14) <- case x4 of
                                                                               {Cons y13
                                                                                     y14 -> return (y13,
                                                                                                    y14);
                                                                                _ -> mzero};
                                                                 guard (x13 == x0);
                                                                 let {x12 = x14};
                                                                 (x3, x5) <- case x12 of
                                                                             {Cons y3
                                                                                   y5 -> return (y3,
                                                                                                 y5);
                                                                              _ -> mzero};
                                                                 generatesdsIIIII x0 x1 x2 x3 x5;
                                                                 return x3},
                                                             do {(x16, x17) <- case x4 of
                                                                               {Cons y16
                                                                                     y17 -> return (y16,
                                                                                                    y17);
                                                                                _ -> mzero};
                                                                 guard (x16 == x1);
                                                                 let {x15 = x17};
                                                                 (x3, x5) <- case x15 of
                                                                             {Cons y3
                                                                                   y5 -> return (y3,
                                                                                                 y5);
                                                                              _ -> mzero};
                                                                 generatesdsIIIII x0 x1 x2 x3 x5;
                                                                 return x3}]
generatesdsIIIOO x0 x1 x2 gen_generatesdsIIIOO_x3 = Immature $ msum [do {let {x4 = Nil};
                                                              x3 <- gen_generatesdsIIIOO_x3;
                                                              return (x3, x4)},
                                                          do {let {x7 = x0};
                                                              (x3,
                                                               x5) <- generatesdsIIIOO x0 x1 x2 gen_generatesdsIIIOO_x3;
                                                              let {x6 = Cons x2 x5};
                                                              let {x8 = x6};
                                                              let {x4 = Cons x7 x8};
                                                              return (x3, x4)},
                                                          do {let {x10 = x1};
                                                              (x3,
                                                               x5) <- generatesdsIIIOO x0 x1 x2 gen_generatesdsIIIOO_x3;
                                                              let {x9 = Cons x2 x5};
                                                              let {x11 = x9};
                                                              let {x4 = Cons x10 x11};
                                                              return (x3, x4)},
                                                          do {let {x13 = x0};
                                                              (x3,
                                                               x5) <- generatesdsIIIOO x0 x1 x2 gen_generatesdsIIIOO_x3;
                                                              let {x12 = Cons x3 x5};
                                                              let {x14 = x12};
                                                              let {x4 = Cons x13 x14};
                                                              return (x3, x4)},
                                                          do {let {x16 = x1};
                                                              (x3,
                                                               x5) <- generatesdsIIIOO x0 x1 x2 gen_generatesdsIIIOO_x3;
                                                              let {x15 = Cons x3 x5};
                                                              let {x17 = x15};
                                                              let {x4 = Cons x16 x17};
                                                              return (x3, x4)}]
generatesdsIIOII x0 x1 x3 x4 gen_generatesdsIIOII_x2 = Immature $ msum [do {guard (x4 == Nil);
                                                                 x2 <- gen_generatesdsIIOII_x2;
                                                                 return x2},
                                                             do {(x7, x8) <- case x4 of
                                                                             {Cons y7
                                                                                   y8 -> return (y7,
                                                                                                 y8);
                                                                              _ -> mzero};
                                                                 guard (x7 == x0);
                                                                 let {x6 = x8};
                                                                 (x2, x5) <- case x6 of
                                                                             {Cons y2
                                                                                   y5 -> return (y2,
                                                                                                 y5);
                                                                              _ -> mzero};
                                                                 generatesdsIIIII x0 x1 x2 x3 x5;
                                                                 return x2},
                                                             do {(x10, x11) <- case x4 of
                                                                               {Cons y10
                                                                                     y11 -> return (y10,
                                                                                                    y11);
                                                                                _ -> mzero};
                                                                 guard (x10 == x1);
                                                                 let {x9 = x11};
                                                                 (x2, x5) <- case x9 of
                                                                             {Cons y2
                                                                                   y5 -> return (y2,
                                                                                                 y5);
                                                                              _ -> mzero};
                                                                 generatesdsIIIII x0 x1 x2 x3 x5;
                                                                 return x2},
                                                             do {(x13, x14) <- case x4 of
                                                                               {Cons y13
                                                                                     y14 -> return (y13,
                                                                                                    y14);
                                                                                _ -> mzero};
                                                                 guard (x13 == x0);
                                                                 let {x12 = x14};
                                                                 x5 <- case x12 of
                                                                       {Cons y3
                                                                             y5 -> do {guard (x3 == y3);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 x2 <- generatesdsIIOII x0 x1 x3 x5 gen_generatesdsIIOII_x2;
                                                                 return x2},
                                                             do {(x16, x17) <- case x4 of
                                                                               {Cons y16
                                                                                     y17 -> return (y16,
                                                                                                    y17);
                                                                                _ -> mzero};
                                                                 guard (x16 == x1);
                                                                 let {x15 = x17};
                                                                 x5 <- case x15 of
                                                                       {Cons y3
                                                                             y5 -> do {guard (x3 == y3);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 x2 <- generatesdsIIOII x0 x1 x3 x5 gen_generatesdsIIOII_x2;
                                                                 return x2}]
generatesdsIIOIO x0 x1 x3 gen_generatesdsIIOIO_x2 = Immature $ msum [do {let {x4 = Nil};
                                                              x2 <- gen_generatesdsIIOIO_x2;
                                                              return (x2, x4)},
                                                          do {let {x7 = x0};
                                                              (x2,
                                                               x5) <- generatesdsIIOIO x0 x1 x3 gen_generatesdsIIOIO_x2;
                                                              let {x6 = Cons x2 x5};
                                                              let {x8 = x6};
                                                              let {x4 = Cons x7 x8};
                                                              return (x2, x4)},
                                                          do {let {x10 = x1};
                                                              (x2,
                                                               x5) <- generatesdsIIOIO x0 x1 x3 gen_generatesdsIIOIO_x2;
                                                              let {x9 = Cons x2 x5};
                                                              let {x11 = x9};
                                                              let {x4 = Cons x10 x11};
                                                              return (x2, x4)},
                                                          do {let {x13 = x0};
                                                              (x2,
                                                               x5) <- generatesdsIIOIO x0 x1 x3 gen_generatesdsIIOIO_x2;
                                                              let {x12 = Cons x3 x5};
                                                              let {x14 = x12};
                                                              let {x4 = Cons x13 x14};
                                                              return (x2, x4)},
                                                          do {let {x16 = x1};
                                                              (x2,
                                                               x5) <- generatesdsIIOIO x0 x1 x3 gen_generatesdsIIOIO_x2;
                                                              let {x15 = Cons x3 x5};
                                                              let {x17 = x15};
                                                              let {x4 = Cons x16 x17};
                                                              return (x2, x4)}]
generatesdsIIOOI x0 x1 x4 gen_generatesdsIIIOI_x3 gen_generatesdsIIOII_x2 gen_generatesdsIIOOI_x2 gen_generatesdsIIOOI_x3 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                      x2 <- gen_generatesdsIIOOI_x2;
                                                                                                                                      x3 <- gen_generatesdsIIOOI_x3;
                                                                                                                                      return (x2,
                                                                                                                                              x3)},
                                                                                                                                  do {(x7,
                                                                                                                                       x8) <- case x4 of
                                                                                                                                              {Cons y7
                                                                                                                                                    y8 -> return (y7,
                                                                                                                                                                  y8);
                                                                                                                                               _ -> mzero};
                                                                                                                                      guard (x7 == x0);
                                                                                                                                      let {x6 = x8};
                                                                                                                                      (x2,
                                                                                                                                       x5) <- case x6 of
                                                                                                                                              {Cons y2
                                                                                                                                                    y5 -> return (y2,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      x3 <- generatesdsIIIOI x0 x1 x2 x5 gen_generatesdsIIIOI_x3;
                                                                                                                                      return (x2,
                                                                                                                                              x3)},
                                                                                                                                  do {(x10,
                                                                                                                                       x11) <- case x4 of
                                                                                                                                               {Cons y10
                                                                                                                                                     y11 -> return (y10,
                                                                                                                                                                    y11);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x10 == x1);
                                                                                                                                      let {x9 = x11};
                                                                                                                                      (x2,
                                                                                                                                       x5) <- case x9 of
                                                                                                                                              {Cons y2
                                                                                                                                                    y5 -> return (y2,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      x3 <- generatesdsIIIOI x0 x1 x2 x5 gen_generatesdsIIIOI_x3;
                                                                                                                                      return (x2,
                                                                                                                                              x3)},
                                                                                                                                  do {(x13,
                                                                                                                                       x14) <- case x4 of
                                                                                                                                               {Cons y13
                                                                                                                                                     y14 -> return (y13,
                                                                                                                                                                    y14);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x13 == x0);
                                                                                                                                      let {x12 = x14};
                                                                                                                                      (x3,
                                                                                                                                       x5) <- case x12 of
                                                                                                                                              {Cons y3
                                                                                                                                                    y5 -> return (y3,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      x2 <- generatesdsIIOII x0 x1 x3 x5 gen_generatesdsIIOII_x2;
                                                                                                                                      return (x2,
                                                                                                                                              x3)},
                                                                                                                                  do {(x16,
                                                                                                                                       x17) <- case x4 of
                                                                                                                                               {Cons y16
                                                                                                                                                     y17 -> return (y16,
                                                                                                                                                                    y17);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x16 == x1);
                                                                                                                                      let {x15 = x17};
                                                                                                                                      (x3,
                                                                                                                                       x5) <- case x15 of
                                                                                                                                              {Cons y3
                                                                                                                                                    y5 -> return (y3,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      x2 <- generatesdsIIOII x0 x1 x3 x5 gen_generatesdsIIOII_x2;
                                                                                                                                      return (x2,
                                                                                                                                              x3)}]
generatesdsIIOOO x0 x1 gen_generatesdsIIOOO_x2 gen_generatesdsIIOOO_x3 = Immature $ msum [do {let {x4 = Nil};
                                                                                   x2 <- gen_generatesdsIIOOO_x2;
                                                                                   x3 <- gen_generatesdsIIOOO_x3;
                                                                                   return (x2,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {let {x7 = x0};
                                                                                   (x2,
                                                                                    x3,
                                                                                    x5) <- generatesdsIIOOO x0 x1 gen_generatesdsIIOOO_x2 gen_generatesdsIIOOO_x3;
                                                                                   let {x6 = Cons x2 x5};
                                                                                   let {x8 = x6};
                                                                                   let {x4 = Cons x7 x8};
                                                                                   return (x2,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {let {x10 = x1};
                                                                                   (x2,
                                                                                    x3,
                                                                                    x5) <- generatesdsIIOOO x0 x1 gen_generatesdsIIOOO_x2 gen_generatesdsIIOOO_x3;
                                                                                   let {x9 = Cons x2 x5};
                                                                                   let {x11 = x9};
                                                                                   let {x4 = Cons x10 x11};
                                                                                   return (x2,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {let {x13 = x0};
                                                                                   (x2,
                                                                                    x3,
                                                                                    x5) <- generatesdsIIOOO x0 x1 gen_generatesdsIIOOO_x2 gen_generatesdsIIOOO_x3;
                                                                                   let {x12 = Cons x3 x5};
                                                                                   let {x14 = x12};
                                                                                   let {x4 = Cons x13 x14};
                                                                                   return (x2,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {let {x16 = x1};
                                                                                   (x2,
                                                                                    x3,
                                                                                    x5) <- generatesdsIIOOO x0 x1 gen_generatesdsIIOOO_x2 gen_generatesdsIIOOO_x3;
                                                                                   let {x15 = Cons x3 x5};
                                                                                   let {x17 = x15};
                                                                                   let {x4 = Cons x16 x17};
                                                                                   return (x2,
                                                                                           x3,
                                                                                           x4)}]
generatesdsIOIII x0 x2 x3 x4 gen_generatesdsIOIII_x1 = Immature $ msum [do {guard (x4 == Nil);
                                                                 x1 <- gen_generatesdsIOIII_x1;
                                                                 return x1},
                                                             do {(x7, x8) <- case x4 of
                                                                             {Cons y7
                                                                                   y8 -> return (y7,
                                                                                                 y8);
                                                                              _ -> mzero};
                                                                 guard (x7 == x0);
                                                                 let {x6 = x8};
                                                                 x5 <- case x6 of
                                                                       {Cons y2
                                                                             y5 -> do {guard (x2 == y2);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 x1 <- generatesdsIOIII x0 x2 x3 x5 gen_generatesdsIOIII_x1;
                                                                 return x1},
                                                             do {(x10, x11) <- case x4 of
                                                                               {Cons y10
                                                                                     y11 -> return (y10,
                                                                                                    y11);
                                                                                _ -> mzero};
                                                                 let {x1 = x10};
                                                                 let {x9 = x11};
                                                                 x5 <- case x9 of
                                                                       {Cons y2
                                                                             y5 -> do {guard (x2 == y2);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 generatesdsIIIII x0 x1 x2 x3 x5;
                                                                 return x1},
                                                             do {(x13, x14) <- case x4 of
                                                                               {Cons y13
                                                                                     y14 -> return (y13,
                                                                                                    y14);
                                                                                _ -> mzero};
                                                                 guard (x13 == x0);
                                                                 let {x12 = x14};
                                                                 x5 <- case x12 of
                                                                       {Cons y3
                                                                             y5 -> do {guard (x3 == y3);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 x1 <- generatesdsIOIII x0 x2 x3 x5 gen_generatesdsIOIII_x1;
                                                                 return x1},
                                                             do {(x16, x17) <- case x4 of
                                                                               {Cons y16
                                                                                     y17 -> return (y16,
                                                                                                    y17);
                                                                                _ -> mzero};
                                                                 let {x1 = x16};
                                                                 let {x15 = x17};
                                                                 x5 <- case x15 of
                                                                       {Cons y3
                                                                             y5 -> do {guard (x3 == y3);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 generatesdsIIIII x0 x1 x2 x3 x5;
                                                                 return x1}]
generatesdsIOIIO x0 x2 x3 gen_generatesdsIOIIO_x1 = Immature $ msum [do {let {x4 = Nil};
                                                              x1 <- gen_generatesdsIOIIO_x1;
                                                              return (x1, x4)},
                                                          do {let {x7 = x0};
                                                              (x1,
                                                               x5) <- generatesdsIOIIO x0 x2 x3 gen_generatesdsIOIIO_x1;
                                                              let {x6 = Cons x2 x5};
                                                              let {x8 = x6};
                                                              let {x4 = Cons x7 x8};
                                                              return (x1, x4)},
                                                          do {(x1,
                                                               x5) <- generatesdsIOIIO x0 x2 x3 gen_generatesdsIOIIO_x1;
                                                              let {x9 = Cons x2 x5};
                                                              let {x10 = x1};
                                                              let {x11 = x9};
                                                              let {x4 = Cons x10 x11};
                                                              return (x1, x4)},
                                                          do {let {x13 = x0};
                                                              (x1,
                                                               x5) <- generatesdsIOIIO x0 x2 x3 gen_generatesdsIOIIO_x1;
                                                              let {x12 = Cons x3 x5};
                                                              let {x14 = x12};
                                                              let {x4 = Cons x13 x14};
                                                              return (x1, x4)},
                                                          do {(x1,
                                                               x5) <- generatesdsIOIIO x0 x2 x3 gen_generatesdsIOIIO_x1;
                                                              let {x15 = Cons x3 x5};
                                                              let {x16 = x1};
                                                              let {x17 = x15};
                                                              let {x4 = Cons x16 x17};
                                                              return (x1, x4)}]
generatesdsIOIOI x0 x2 x4 gen_generatesdsIIIOI_x3 gen_generatesdsIOIII_x1 gen_generatesdsIOIOI_x1 gen_generatesdsIOIOI_x3 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                      x1 <- gen_generatesdsIOIOI_x1;
                                                                                                                                      x3 <- gen_generatesdsIOIOI_x3;
                                                                                                                                      return (x1,
                                                                                                                                              x3)},
                                                                                                                                  do {(x7,
                                                                                                                                       x8) <- case x4 of
                                                                                                                                              {Cons y7
                                                                                                                                                    y8 -> return (y7,
                                                                                                                                                                  y8);
                                                                                                                                               _ -> mzero};
                                                                                                                                      guard (x7 == x0);
                                                                                                                                      let {x6 = x8};
                                                                                                                                      x5 <- case x6 of
                                                                                                                                            {Cons y2
                                                                                                                                                  y5 -> do {guard (x2 == y2);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      (x1,
                                                                                                                                       x3) <- generatesdsIOIOI x0 x2 x5 gen_generatesdsIIIOI_x3 gen_generatesdsIOIII_x1 gen_generatesdsIOIOI_x1 gen_generatesdsIOIOI_x3;
                                                                                                                                      return (x1,
                                                                                                                                              x3)},
                                                                                                                                  do {(x10,
                                                                                                                                       x11) <- case x4 of
                                                                                                                                               {Cons y10
                                                                                                                                                     y11 -> return (y10,
                                                                                                                                                                    y11);
                                                                                                                                                _ -> mzero};
                                                                                                                                      let {x1 = x10};
                                                                                                                                      let {x9 = x11};
                                                                                                                                      x5 <- case x9 of
                                                                                                                                            {Cons y2
                                                                                                                                                  y5 -> do {guard (x2 == y2);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      x3 <- generatesdsIIIOI x0 x1 x2 x5 gen_generatesdsIIIOI_x3;
                                                                                                                                      return (x1,
                                                                                                                                              x3)},
                                                                                                                                  do {(x13,
                                                                                                                                       x14) <- case x4 of
                                                                                                                                               {Cons y13
                                                                                                                                                     y14 -> return (y13,
                                                                                                                                                                    y14);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x13 == x0);
                                                                                                                                      let {x12 = x14};
                                                                                                                                      (x3,
                                                                                                                                       x5) <- case x12 of
                                                                                                                                              {Cons y3
                                                                                                                                                    y5 -> return (y3,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      x1 <- generatesdsIOIII x0 x2 x3 x5 gen_generatesdsIOIII_x1;
                                                                                                                                      return (x1,
                                                                                                                                              x3)},
                                                                                                                                  do {(x16,
                                                                                                                                       x17) <- case x4 of
                                                                                                                                               {Cons y16
                                                                                                                                                     y17 -> return (y16,
                                                                                                                                                                    y17);
                                                                                                                                                _ -> mzero};
                                                                                                                                      let {x1 = x16};
                                                                                                                                      let {x15 = x17};
                                                                                                                                      (x3,
                                                                                                                                       x5) <- case x15 of
                                                                                                                                              {Cons y3
                                                                                                                                                    y5 -> return (y3,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      generatesdsIIIII x0 x1 x2 x3 x5;
                                                                                                                                      return (x1,
                                                                                                                                              x3)}]
generatesdsIOIOO x0 x2 gen_generatesdsIOIOO_x1 gen_generatesdsIOIOO_x3 = Immature $ msum [do {let {x4 = Nil};
                                                                                   x1 <- gen_generatesdsIOIOO_x1;
                                                                                   x3 <- gen_generatesdsIOIOO_x3;
                                                                                   return (x1,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {let {x7 = x0};
                                                                                   (x1,
                                                                                    x3,
                                                                                    x5) <- generatesdsIOIOO x0 x2 gen_generatesdsIOIOO_x1 gen_generatesdsIOIOO_x3;
                                                                                   let {x6 = Cons x2 x5};
                                                                                   let {x8 = x6};
                                                                                   let {x4 = Cons x7 x8};
                                                                                   return (x1,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {(x1,
                                                                                    x3,
                                                                                    x5) <- generatesdsIOIOO x0 x2 gen_generatesdsIOIOO_x1 gen_generatesdsIOIOO_x3;
                                                                                   let {x9 = Cons x2 x5};
                                                                                   let {x10 = x1};
                                                                                   let {x11 = x9};
                                                                                   let {x4 = Cons x10 x11};
                                                                                   return (x1,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {let {x13 = x0};
                                                                                   (x1,
                                                                                    x3,
                                                                                    x5) <- generatesdsIOIOO x0 x2 gen_generatesdsIOIOO_x1 gen_generatesdsIOIOO_x3;
                                                                                   let {x12 = Cons x3 x5};
                                                                                   let {x14 = x12};
                                                                                   let {x4 = Cons x13 x14};
                                                                                   return (x1,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {(x1,
                                                                                    x3,
                                                                                    x5) <- generatesdsIOIOO x0 x2 gen_generatesdsIOIOO_x1 gen_generatesdsIOIOO_x3;
                                                                                   let {x15 = Cons x3 x5};
                                                                                   let {x16 = x1};
                                                                                   let {x17 = x15};
                                                                                   let {x4 = Cons x16 x17};
                                                                                   return (x1,
                                                                                           x3,
                                                                                           x4)}]
generatesdsIOOII x0 x3 x4 gen_generatesdsIIOII_x2 gen_generatesdsIOIII_x1 gen_generatesdsIOOII_x1 gen_generatesdsIOOII_x2 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                      x1 <- gen_generatesdsIOOII_x1;
                                                                                                                                      x2 <- gen_generatesdsIOOII_x2;
                                                                                                                                      return (x1,
                                                                                                                                              x2)},
                                                                                                                                  do {(x7,
                                                                                                                                       x8) <- case x4 of
                                                                                                                                              {Cons y7
                                                                                                                                                    y8 -> return (y7,
                                                                                                                                                                  y8);
                                                                                                                                               _ -> mzero};
                                                                                                                                      guard (x7 == x0);
                                                                                                                                      let {x6 = x8};
                                                                                                                                      (x2,
                                                                                                                                       x5) <- case x6 of
                                                                                                                                              {Cons y2
                                                                                                                                                    y5 -> return (y2,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      x1 <- generatesdsIOIII x0 x2 x3 x5 gen_generatesdsIOIII_x1;
                                                                                                                                      return (x1,
                                                                                                                                              x2)},
                                                                                                                                  do {(x10,
                                                                                                                                       x11) <- case x4 of
                                                                                                                                               {Cons y10
                                                                                                                                                     y11 -> return (y10,
                                                                                                                                                                    y11);
                                                                                                                                                _ -> mzero};
                                                                                                                                      let {x1 = x10};
                                                                                                                                      let {x9 = x11};
                                                                                                                                      (x2,
                                                                                                                                       x5) <- case x9 of
                                                                                                                                              {Cons y2
                                                                                                                                                    y5 -> return (y2,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      generatesdsIIIII x0 x1 x2 x3 x5;
                                                                                                                                      return (x1,
                                                                                                                                              x2)},
                                                                                                                                  do {(x13,
                                                                                                                                       x14) <- case x4 of
                                                                                                                                               {Cons y13
                                                                                                                                                     y14 -> return (y13,
                                                                                                                                                                    y14);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x13 == x0);
                                                                                                                                      let {x12 = x14};
                                                                                                                                      x5 <- case x12 of
                                                                                                                                            {Cons y3
                                                                                                                                                  y5 -> do {guard (x3 == y3);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      (x1,
                                                                                                                                       x2) <- generatesdsIOOII x0 x3 x5 gen_generatesdsIIOII_x2 gen_generatesdsIOIII_x1 gen_generatesdsIOOII_x1 gen_generatesdsIOOII_x2;
                                                                                                                                      return (x1,
                                                                                                                                              x2)},
                                                                                                                                  do {(x16,
                                                                                                                                       x17) <- case x4 of
                                                                                                                                               {Cons y16
                                                                                                                                                     y17 -> return (y16,
                                                                                                                                                                    y17);
                                                                                                                                                _ -> mzero};
                                                                                                                                      let {x1 = x16};
                                                                                                                                      let {x15 = x17};
                                                                                                                                      x5 <- case x15 of
                                                                                                                                            {Cons y3
                                                                                                                                                  y5 -> do {guard (x3 == y3);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      x2 <- generatesdsIIOII x0 x1 x3 x5 gen_generatesdsIIOII_x2;
                                                                                                                                      return (x1,
                                                                                                                                              x2)}]
generatesdsIOOIO x0 x3 gen_generatesdsIOOIO_x1 gen_generatesdsIOOIO_x2 = Immature $ msum [do {let {x4 = Nil};
                                                                                   x1 <- gen_generatesdsIOOIO_x1;
                                                                                   x2 <- gen_generatesdsIOOIO_x2;
                                                                                   return (x1,
                                                                                           x2,
                                                                                           x4)},
                                                                               do {let {x7 = x0};
                                                                                   (x1,
                                                                                    x2,
                                                                                    x5) <- generatesdsIOOIO x0 x3 gen_generatesdsIOOIO_x1 gen_generatesdsIOOIO_x2;
                                                                                   let {x6 = Cons x2 x5};
                                                                                   let {x8 = x6};
                                                                                   let {x4 = Cons x7 x8};
                                                                                   return (x1,
                                                                                           x2,
                                                                                           x4)},
                                                                               do {(x1,
                                                                                    x2,
                                                                                    x5) <- generatesdsIOOIO x0 x3 gen_generatesdsIOOIO_x1 gen_generatesdsIOOIO_x2;
                                                                                   let {x9 = Cons x2 x5};
                                                                                   let {x10 = x1};
                                                                                   let {x11 = x9};
                                                                                   let {x4 = Cons x10 x11};
                                                                                   return (x1,
                                                                                           x2,
                                                                                           x4)},
                                                                               do {let {x13 = x0};
                                                                                   (x1,
                                                                                    x2,
                                                                                    x5) <- generatesdsIOOIO x0 x3 gen_generatesdsIOOIO_x1 gen_generatesdsIOOIO_x2;
                                                                                   let {x12 = Cons x3 x5};
                                                                                   let {x14 = x12};
                                                                                   let {x4 = Cons x13 x14};
                                                                                   return (x1,
                                                                                           x2,
                                                                                           x4)},
                                                                               do {(x1,
                                                                                    x2,
                                                                                    x5) <- generatesdsIOOIO x0 x3 gen_generatesdsIOOIO_x1 gen_generatesdsIOOIO_x2;
                                                                                   let {x15 = Cons x3 x5};
                                                                                   let {x16 = x1};
                                                                                   let {x17 = x15};
                                                                                   let {x4 = Cons x16 x17};
                                                                                   return (x1,
                                                                                           x2,
                                                                                           x4)}]
generatesdsIOOOI x0 x4 gen_generatesdsIIIOI_x3 gen_generatesdsIIOII_x2 gen_generatesdsIOIII_x1 gen_generatesdsIOIOI_x1 gen_generatesdsIOIOI_x3 gen_generatesdsIOOII_x1 gen_generatesdsIOOII_x2 gen_generatesdsIOOOI_x1 gen_generatesdsIOOOI_x2 gen_generatesdsIOOOI_x3 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                                                                                                                                                                   x1 <- gen_generatesdsIOOOI_x1;
                                                                                                                                                                                                                                                                                   x2 <- gen_generatesdsIOOOI_x2;
                                                                                                                                                                                                                                                                                   x3 <- gen_generatesdsIOOOI_x3;
                                                                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                                                                           x2,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x7,
                                                                                                                                                                                                                                                                                    x8) <- case x4 of
                                                                                                                                                                                                                                                                                           {Cons y7
                                                                                                                                                                                                                                                                                                 y8 -> return (y7,
                                                                                                                                                                                                                                                                                                               y8);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   guard (x7 == x0);
                                                                                                                                                                                                                                                                                   let {x6 = x8};
                                                                                                                                                                                                                                                                                   (x2,
                                                                                                                                                                                                                                                                                    x5) <- case x6 of
                                                                                                                                                                                                                                                                                           {Cons y2
                                                                                                                                                                                                                                                                                                 y5 -> return (y2,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   (x1,
                                                                                                                                                                                                                                                                                    x3) <- generatesdsIOIOI x0 x2 x5 gen_generatesdsIIIOI_x3 gen_generatesdsIOIII_x1 gen_generatesdsIOIOI_x1 gen_generatesdsIOIOI_x3;
                                                                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                                                                           x2,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x10,
                                                                                                                                                                                                                                                                                    x11) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y10
                                                                                                                                                                                                                                                                                                  y11 -> return (y10,
                                                                                                                                                                                                                                                                                                                 y11);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x1 = x10};
                                                                                                                                                                                                                                                                                   let {x9 = x11};
                                                                                                                                                                                                                                                                                   (x2,
                                                                                                                                                                                                                                                                                    x5) <- case x9 of
                                                                                                                                                                                                                                                                                           {Cons y2
                                                                                                                                                                                                                                                                                                 y5 -> return (y2,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   x3 <- generatesdsIIIOI x0 x1 x2 x5 gen_generatesdsIIIOI_x3;
                                                                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                                                                           x2,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x13,
                                                                                                                                                                                                                                                                                    x14) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y13
                                                                                                                                                                                                                                                                                                  y14 -> return (y13,
                                                                                                                                                                                                                                                                                                                 y14);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   guard (x13 == x0);
                                                                                                                                                                                                                                                                                   let {x12 = x14};
                                                                                                                                                                                                                                                                                   (x3,
                                                                                                                                                                                                                                                                                    x5) <- case x12 of
                                                                                                                                                                                                                                                                                           {Cons y3
                                                                                                                                                                                                                                                                                                 y5 -> return (y3,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   (x1,
                                                                                                                                                                                                                                                                                    x2) <- generatesdsIOOII x0 x3 x5 gen_generatesdsIIOII_x2 gen_generatesdsIOIII_x1 gen_generatesdsIOOII_x1 gen_generatesdsIOOII_x2;
                                                                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                                                                           x2,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x16,
                                                                                                                                                                                                                                                                                    x17) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y16
                                                                                                                                                                                                                                                                                                  y17 -> return (y16,
                                                                                                                                                                                                                                                                                                                 y17);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x1 = x16};
                                                                                                                                                                                                                                                                                   let {x15 = x17};
                                                                                                                                                                                                                                                                                   (x3,
                                                                                                                                                                                                                                                                                    x5) <- case x15 of
                                                                                                                                                                                                                                                                                           {Cons y3
                                                                                                                                                                                                                                                                                                 y5 -> return (y3,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   x2 <- generatesdsIIOII x0 x1 x3 x5 gen_generatesdsIIOII_x2;
                                                                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                                                                           x2,
                                                                                                                                                                                                                                                                                           x3)}]
generatesdsIOOOO x0 gen_generatesdsIOOOO_x1 gen_generatesdsIOOOO_x2 gen_generatesdsIOOOO_x3 = Immature $ msum [do {let {x4 = Nil};
                                                                                                        x1 <- gen_generatesdsIOOOO_x1;
                                                                                                        x2 <- gen_generatesdsIOOOO_x2;
                                                                                                        x3 <- gen_generatesdsIOOOO_x3;
                                                                                                        return (x1,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {let {x7 = x0};
                                                                                                        (x1,
                                                                                                         x2,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsIOOOO x0 gen_generatesdsIOOOO_x1 gen_generatesdsIOOOO_x2 gen_generatesdsIOOOO_x3;
                                                                                                        let {x6 = Cons x2 x5};
                                                                                                        let {x8 = x6};
                                                                                                        let {x4 = Cons x7 x8};
                                                                                                        return (x1,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {(x1,
                                                                                                         x2,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsIOOOO x0 gen_generatesdsIOOOO_x1 gen_generatesdsIOOOO_x2 gen_generatesdsIOOOO_x3;
                                                                                                        let {x9 = Cons x2 x5};
                                                                                                        let {x10 = x1};
                                                                                                        let {x11 = x9};
                                                                                                        let {x4 = Cons x10 x11};
                                                                                                        return (x1,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {let {x13 = x0};
                                                                                                        (x1,
                                                                                                         x2,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsIOOOO x0 gen_generatesdsIOOOO_x1 gen_generatesdsIOOOO_x2 gen_generatesdsIOOOO_x3;
                                                                                                        let {x12 = Cons x3 x5};
                                                                                                        let {x14 = x12};
                                                                                                        let {x4 = Cons x13 x14};
                                                                                                        return (x1,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {(x1,
                                                                                                         x2,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsIOOOO x0 gen_generatesdsIOOOO_x1 gen_generatesdsIOOOO_x2 gen_generatesdsIOOOO_x3;
                                                                                                        let {x15 = Cons x3 x5};
                                                                                                        let {x16 = x1};
                                                                                                        let {x17 = x15};
                                                                                                        let {x4 = Cons x16 x17};
                                                                                                        return (x1,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4)}]
generatesdsOIIII x1 x2 x3 x4 gen_generatesdsOIIII_x0 = Immature $ msum [do {guard (x4 == Nil);
                                                                 x0 <- gen_generatesdsOIIII_x0;
                                                                 return x0},
                                                             do {(x7, x8) <- case x4 of
                                                                             {Cons y7
                                                                                   y8 -> return (y7,
                                                                                                 y8);
                                                                              _ -> mzero};
                                                                 let {x0 = x7};
                                                                 let {x6 = x8};
                                                                 x5 <- case x6 of
                                                                       {Cons y2
                                                                             y5 -> do {guard (x2 == y2);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 generatesdsIIIII x0 x1 x2 x3 x5;
                                                                 return x0},
                                                             do {(x10, x11) <- case x4 of
                                                                               {Cons y10
                                                                                     y11 -> return (y10,
                                                                                                    y11);
                                                                                _ -> mzero};
                                                                 guard (x10 == x1);
                                                                 let {x9 = x11};
                                                                 x5 <- case x9 of
                                                                       {Cons y2
                                                                             y5 -> do {guard (x2 == y2);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 x0 <- generatesdsOIIII x1 x2 x3 x5 gen_generatesdsOIIII_x0;
                                                                 return x0},
                                                             do {(x13, x14) <- case x4 of
                                                                               {Cons y13
                                                                                     y14 -> return (y13,
                                                                                                    y14);
                                                                                _ -> mzero};
                                                                 let {x0 = x13};
                                                                 let {x12 = x14};
                                                                 x5 <- case x12 of
                                                                       {Cons y3
                                                                             y5 -> do {guard (x3 == y3);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 generatesdsIIIII x0 x1 x2 x3 x5;
                                                                 return x0},
                                                             do {(x16, x17) <- case x4 of
                                                                               {Cons y16
                                                                                     y17 -> return (y16,
                                                                                                    y17);
                                                                                _ -> mzero};
                                                                 guard (x16 == x1);
                                                                 let {x15 = x17};
                                                                 x5 <- case x15 of
                                                                       {Cons y3
                                                                             y5 -> do {guard (x3 == y3);
                                                                                       return y5};
                                                                        _ -> mzero};
                                                                 x0 <- generatesdsOIIII x1 x2 x3 x5 gen_generatesdsOIIII_x0;
                                                                 return x0}]
generatesdsOIIIO x1 x2 x3 gen_generatesdsOIIIO_x0 = Immature $ msum [do {let {x4 = Nil};
                                                              x0 <- gen_generatesdsOIIIO_x0;
                                                              return (x0, x4)},
                                                          do {(x0,
                                                               x5) <- generatesdsOIIIO x1 x2 x3 gen_generatesdsOIIIO_x0;
                                                              let {x6 = Cons x2 x5};
                                                              let {x7 = x0};
                                                              let {x8 = x6};
                                                              let {x4 = Cons x7 x8};
                                                              return (x0, x4)},
                                                          do {let {x10 = x1};
                                                              (x0,
                                                               x5) <- generatesdsOIIIO x1 x2 x3 gen_generatesdsOIIIO_x0;
                                                              let {x9 = Cons x2 x5};
                                                              let {x11 = x9};
                                                              let {x4 = Cons x10 x11};
                                                              return (x0, x4)},
                                                          do {(x0,
                                                               x5) <- generatesdsOIIIO x1 x2 x3 gen_generatesdsOIIIO_x0;
                                                              let {x12 = Cons x3 x5};
                                                              let {x13 = x0};
                                                              let {x14 = x12};
                                                              let {x4 = Cons x13 x14};
                                                              return (x0, x4)},
                                                          do {let {x16 = x1};
                                                              (x0,
                                                               x5) <- generatesdsOIIIO x1 x2 x3 gen_generatesdsOIIIO_x0;
                                                              let {x15 = Cons x3 x5};
                                                              let {x17 = x15};
                                                              let {x4 = Cons x16 x17};
                                                              return (x0, x4)}]
generatesdsOIIOI x1 x2 x4 gen_generatesdsIIIOI_x3 gen_generatesdsOIIII_x0 gen_generatesdsOIIOI_x0 gen_generatesdsOIIOI_x3 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                      x0 <- gen_generatesdsOIIOI_x0;
                                                                                                                                      x3 <- gen_generatesdsOIIOI_x3;
                                                                                                                                      return (x0,
                                                                                                                                              x3)},
                                                                                                                                  do {(x7,
                                                                                                                                       x8) <- case x4 of
                                                                                                                                              {Cons y7
                                                                                                                                                    y8 -> return (y7,
                                                                                                                                                                  y8);
                                                                                                                                               _ -> mzero};
                                                                                                                                      let {x0 = x7};
                                                                                                                                      let {x6 = x8};
                                                                                                                                      x5 <- case x6 of
                                                                                                                                            {Cons y2
                                                                                                                                                  y5 -> do {guard (x2 == y2);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      x3 <- generatesdsIIIOI x0 x1 x2 x5 gen_generatesdsIIIOI_x3;
                                                                                                                                      return (x0,
                                                                                                                                              x3)},
                                                                                                                                  do {(x10,
                                                                                                                                       x11) <- case x4 of
                                                                                                                                               {Cons y10
                                                                                                                                                     y11 -> return (y10,
                                                                                                                                                                    y11);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x10 == x1);
                                                                                                                                      let {x9 = x11};
                                                                                                                                      x5 <- case x9 of
                                                                                                                                            {Cons y2
                                                                                                                                                  y5 -> do {guard (x2 == y2);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      (x0,
                                                                                                                                       x3) <- generatesdsOIIOI x1 x2 x5 gen_generatesdsIIIOI_x3 gen_generatesdsOIIII_x0 gen_generatesdsOIIOI_x0 gen_generatesdsOIIOI_x3;
                                                                                                                                      return (x0,
                                                                                                                                              x3)},
                                                                                                                                  do {(x13,
                                                                                                                                       x14) <- case x4 of
                                                                                                                                               {Cons y13
                                                                                                                                                     y14 -> return (y13,
                                                                                                                                                                    y14);
                                                                                                                                                _ -> mzero};
                                                                                                                                      let {x0 = x13};
                                                                                                                                      let {x12 = x14};
                                                                                                                                      (x3,
                                                                                                                                       x5) <- case x12 of
                                                                                                                                              {Cons y3
                                                                                                                                                    y5 -> return (y3,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      generatesdsIIIII x0 x1 x2 x3 x5;
                                                                                                                                      return (x0,
                                                                                                                                              x3)},
                                                                                                                                  do {(x16,
                                                                                                                                       x17) <- case x4 of
                                                                                                                                               {Cons y16
                                                                                                                                                     y17 -> return (y16,
                                                                                                                                                                    y17);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x16 == x1);
                                                                                                                                      let {x15 = x17};
                                                                                                                                      (x3,
                                                                                                                                       x5) <- case x15 of
                                                                                                                                              {Cons y3
                                                                                                                                                    y5 -> return (y3,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      x0 <- generatesdsOIIII x1 x2 x3 x5 gen_generatesdsOIIII_x0;
                                                                                                                                      return (x0,
                                                                                                                                              x3)}]
generatesdsOIIOO x1 x2 gen_generatesdsOIIOO_x0 gen_generatesdsOIIOO_x3 = Immature $ msum [do {let {x4 = Nil};
                                                                                   x0 <- gen_generatesdsOIIOO_x0;
                                                                                   x3 <- gen_generatesdsOIIOO_x3;
                                                                                   return (x0,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {(x0,
                                                                                    x3,
                                                                                    x5) <- generatesdsOIIOO x1 x2 gen_generatesdsOIIOO_x0 gen_generatesdsOIIOO_x3;
                                                                                   let {x6 = Cons x2 x5};
                                                                                   let {x7 = x0};
                                                                                   let {x8 = x6};
                                                                                   let {x4 = Cons x7 x8};
                                                                                   return (x0,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {let {x10 = x1};
                                                                                   (x0,
                                                                                    x3,
                                                                                    x5) <- generatesdsOIIOO x1 x2 gen_generatesdsOIIOO_x0 gen_generatesdsOIIOO_x3;
                                                                                   let {x9 = Cons x2 x5};
                                                                                   let {x11 = x9};
                                                                                   let {x4 = Cons x10 x11};
                                                                                   return (x0,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {(x0,
                                                                                    x3,
                                                                                    x5) <- generatesdsOIIOO x1 x2 gen_generatesdsOIIOO_x0 gen_generatesdsOIIOO_x3;
                                                                                   let {x12 = Cons x3 x5};
                                                                                   let {x13 = x0};
                                                                                   let {x14 = x12};
                                                                                   let {x4 = Cons x13 x14};
                                                                                   return (x0,
                                                                                           x3,
                                                                                           x4)},
                                                                               do {let {x16 = x1};
                                                                                   (x0,
                                                                                    x3,
                                                                                    x5) <- generatesdsOIIOO x1 x2 gen_generatesdsOIIOO_x0 gen_generatesdsOIIOO_x3;
                                                                                   let {x15 = Cons x3 x5};
                                                                                   let {x17 = x15};
                                                                                   let {x4 = Cons x16 x17};
                                                                                   return (x0,
                                                                                           x3,
                                                                                           x4)}]
generatesdsOIOII x1 x3 x4 gen_generatesdsIIOII_x2 gen_generatesdsOIIII_x0 gen_generatesdsOIOII_x0 gen_generatesdsOIOII_x2 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                      x0 <- gen_generatesdsOIOII_x0;
                                                                                                                                      x2 <- gen_generatesdsOIOII_x2;
                                                                                                                                      return (x0,
                                                                                                                                              x2)},
                                                                                                                                  do {(x7,
                                                                                                                                       x8) <- case x4 of
                                                                                                                                              {Cons y7
                                                                                                                                                    y8 -> return (y7,
                                                                                                                                                                  y8);
                                                                                                                                               _ -> mzero};
                                                                                                                                      let {x0 = x7};
                                                                                                                                      let {x6 = x8};
                                                                                                                                      (x2,
                                                                                                                                       x5) <- case x6 of
                                                                                                                                              {Cons y2
                                                                                                                                                    y5 -> return (y2,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      generatesdsIIIII x0 x1 x2 x3 x5;
                                                                                                                                      return (x0,
                                                                                                                                              x2)},
                                                                                                                                  do {(x10,
                                                                                                                                       x11) <- case x4 of
                                                                                                                                               {Cons y10
                                                                                                                                                     y11 -> return (y10,
                                                                                                                                                                    y11);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x10 == x1);
                                                                                                                                      let {x9 = x11};
                                                                                                                                      (x2,
                                                                                                                                       x5) <- case x9 of
                                                                                                                                              {Cons y2
                                                                                                                                                    y5 -> return (y2,
                                                                                                                                                                  y5);
                                                                                                                                               _ -> mzero};
                                                                                                                                      x0 <- generatesdsOIIII x1 x2 x3 x5 gen_generatesdsOIIII_x0;
                                                                                                                                      return (x0,
                                                                                                                                              x2)},
                                                                                                                                  do {(x13,
                                                                                                                                       x14) <- case x4 of
                                                                                                                                               {Cons y13
                                                                                                                                                     y14 -> return (y13,
                                                                                                                                                                    y14);
                                                                                                                                                _ -> mzero};
                                                                                                                                      let {x0 = x13};
                                                                                                                                      let {x12 = x14};
                                                                                                                                      x5 <- case x12 of
                                                                                                                                            {Cons y3
                                                                                                                                                  y5 -> do {guard (x3 == y3);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      x2 <- generatesdsIIOII x0 x1 x3 x5 gen_generatesdsIIOII_x2;
                                                                                                                                      return (x0,
                                                                                                                                              x2)},
                                                                                                                                  do {(x16,
                                                                                                                                       x17) <- case x4 of
                                                                                                                                               {Cons y16
                                                                                                                                                     y17 -> return (y16,
                                                                                                                                                                    y17);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x16 == x1);
                                                                                                                                      let {x15 = x17};
                                                                                                                                      x5 <- case x15 of
                                                                                                                                            {Cons y3
                                                                                                                                                  y5 -> do {guard (x3 == y3);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      (x0,
                                                                                                                                       x2) <- generatesdsOIOII x1 x3 x5 gen_generatesdsIIOII_x2 gen_generatesdsOIIII_x0 gen_generatesdsOIOII_x0 gen_generatesdsOIOII_x2;
                                                                                                                                      return (x0,
                                                                                                                                              x2)}]
generatesdsOIOIO x1 x3 gen_generatesdsOIOIO_x0 gen_generatesdsOIOIO_x2 = Immature $ msum [do {let {x4 = Nil};
                                                                                   x0 <- gen_generatesdsOIOIO_x0;
                                                                                   x2 <- gen_generatesdsOIOIO_x2;
                                                                                   return (x0,
                                                                                           x2,
                                                                                           x4)},
                                                                               do {(x0,
                                                                                    x2,
                                                                                    x5) <- generatesdsOIOIO x1 x3 gen_generatesdsOIOIO_x0 gen_generatesdsOIOIO_x2;
                                                                                   let {x6 = Cons x2 x5};
                                                                                   let {x7 = x0};
                                                                                   let {x8 = x6};
                                                                                   let {x4 = Cons x7 x8};
                                                                                   return (x0,
                                                                                           x2,
                                                                                           x4)},
                                                                               do {let {x10 = x1};
                                                                                   (x0,
                                                                                    x2,
                                                                                    x5) <- generatesdsOIOIO x1 x3 gen_generatesdsOIOIO_x0 gen_generatesdsOIOIO_x2;
                                                                                   let {x9 = Cons x2 x5};
                                                                                   let {x11 = x9};
                                                                                   let {x4 = Cons x10 x11};
                                                                                   return (x0,
                                                                                           x2,
                                                                                           x4)},
                                                                               do {(x0,
                                                                                    x2,
                                                                                    x5) <- generatesdsOIOIO x1 x3 gen_generatesdsOIOIO_x0 gen_generatesdsOIOIO_x2;
                                                                                   let {x12 = Cons x3 x5};
                                                                                   let {x13 = x0};
                                                                                   let {x14 = x12};
                                                                                   let {x4 = Cons x13 x14};
                                                                                   return (x0,
                                                                                           x2,
                                                                                           x4)},
                                                                               do {let {x16 = x1};
                                                                                   (x0,
                                                                                    x2,
                                                                                    x5) <- generatesdsOIOIO x1 x3 gen_generatesdsOIOIO_x0 gen_generatesdsOIOIO_x2;
                                                                                   let {x15 = Cons x3 x5};
                                                                                   let {x17 = x15};
                                                                                   let {x4 = Cons x16 x17};
                                                                                   return (x0,
                                                                                           x2,
                                                                                           x4)}]
generatesdsOIOOI x1 x4 gen_generatesdsIIIOI_x3 gen_generatesdsIIOII_x2 gen_generatesdsOIIII_x0 gen_generatesdsOIIOI_x0 gen_generatesdsOIIOI_x3 gen_generatesdsOIOII_x0 gen_generatesdsOIOII_x2 gen_generatesdsOIOOI_x0 gen_generatesdsOIOOI_x2 gen_generatesdsOIOOI_x3 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                                                                                                                                                                   x0 <- gen_generatesdsOIOOI_x0;
                                                                                                                                                                                                                                                                                   x2 <- gen_generatesdsOIOOI_x2;
                                                                                                                                                                                                                                                                                   x3 <- gen_generatesdsOIOOI_x3;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x2,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x7,
                                                                                                                                                                                                                                                                                    x8) <- case x4 of
                                                                                                                                                                                                                                                                                           {Cons y7
                                                                                                                                                                                                                                                                                                 y8 -> return (y7,
                                                                                                                                                                                                                                                                                                               y8);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x0 = x7};
                                                                                                                                                                                                                                                                                   let {x6 = x8};
                                                                                                                                                                                                                                                                                   (x2,
                                                                                                                                                                                                                                                                                    x5) <- case x6 of
                                                                                                                                                                                                                                                                                           {Cons y2
                                                                                                                                                                                                                                                                                                 y5 -> return (y2,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   x3 <- generatesdsIIIOI x0 x1 x2 x5 gen_generatesdsIIIOI_x3;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x2,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x10,
                                                                                                                                                                                                                                                                                    x11) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y10
                                                                                                                                                                                                                                                                                                  y11 -> return (y10,
                                                                                                                                                                                                                                                                                                                 y11);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   guard (x10 == x1);
                                                                                                                                                                                                                                                                                   let {x9 = x11};
                                                                                                                                                                                                                                                                                   (x2,
                                                                                                                                                                                                                                                                                    x5) <- case x9 of
                                                                                                                                                                                                                                                                                           {Cons y2
                                                                                                                                                                                                                                                                                                 y5 -> return (y2,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   (x0,
                                                                                                                                                                                                                                                                                    x3) <- generatesdsOIIOI x1 x2 x5 gen_generatesdsIIIOI_x3 gen_generatesdsOIIII_x0 gen_generatesdsOIIOI_x0 gen_generatesdsOIIOI_x3;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x2,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x13,
                                                                                                                                                                                                                                                                                    x14) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y13
                                                                                                                                                                                                                                                                                                  y14 -> return (y13,
                                                                                                                                                                                                                                                                                                                 y14);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x0 = x13};
                                                                                                                                                                                                                                                                                   let {x12 = x14};
                                                                                                                                                                                                                                                                                   (x3,
                                                                                                                                                                                                                                                                                    x5) <- case x12 of
                                                                                                                                                                                                                                                                                           {Cons y3
                                                                                                                                                                                                                                                                                                 y5 -> return (y3,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   x2 <- generatesdsIIOII x0 x1 x3 x5 gen_generatesdsIIOII_x2;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x2,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x16,
                                                                                                                                                                                                                                                                                    x17) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y16
                                                                                                                                                                                                                                                                                                  y17 -> return (y16,
                                                                                                                                                                                                                                                                                                                 y17);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   guard (x16 == x1);
                                                                                                                                                                                                                                                                                   let {x15 = x17};
                                                                                                                                                                                                                                                                                   (x3,
                                                                                                                                                                                                                                                                                    x5) <- case x15 of
                                                                                                                                                                                                                                                                                           {Cons y3
                                                                                                                                                                                                                                                                                                 y5 -> return (y3,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   (x0,
                                                                                                                                                                                                                                                                                    x2) <- generatesdsOIOII x1 x3 x5 gen_generatesdsIIOII_x2 gen_generatesdsOIIII_x0 gen_generatesdsOIOII_x0 gen_generatesdsOIOII_x2;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x2,
                                                                                                                                                                                                                                                                                           x3)}]
generatesdsOIOOO x1 gen_generatesdsOIOOO_x0 gen_generatesdsOIOOO_x2 gen_generatesdsOIOOO_x3 = Immature $ msum [do {let {x4 = Nil};
                                                                                                        x0 <- gen_generatesdsOIOOO_x0;
                                                                                                        x2 <- gen_generatesdsOIOOO_x2;
                                                                                                        x3 <- gen_generatesdsOIOOO_x3;
                                                                                                        return (x0,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {(x0,
                                                                                                         x2,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsOIOOO x1 gen_generatesdsOIOOO_x0 gen_generatesdsOIOOO_x2 gen_generatesdsOIOOO_x3;
                                                                                                        let {x6 = Cons x2 x5};
                                                                                                        let {x7 = x0};
                                                                                                        let {x8 = x6};
                                                                                                        let {x4 = Cons x7 x8};
                                                                                                        return (x0,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {let {x10 = x1};
                                                                                                        (x0,
                                                                                                         x2,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsOIOOO x1 gen_generatesdsOIOOO_x0 gen_generatesdsOIOOO_x2 gen_generatesdsOIOOO_x3;
                                                                                                        let {x9 = Cons x2 x5};
                                                                                                        let {x11 = x9};
                                                                                                        let {x4 = Cons x10 x11};
                                                                                                        return (x0,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {(x0,
                                                                                                         x2,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsOIOOO x1 gen_generatesdsOIOOO_x0 gen_generatesdsOIOOO_x2 gen_generatesdsOIOOO_x3;
                                                                                                        let {x12 = Cons x3 x5};
                                                                                                        let {x13 = x0};
                                                                                                        let {x14 = x12};
                                                                                                        let {x4 = Cons x13 x14};
                                                                                                        return (x0,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {let {x16 = x1};
                                                                                                        (x0,
                                                                                                         x2,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsOIOOO x1 gen_generatesdsOIOOO_x0 gen_generatesdsOIOOO_x2 gen_generatesdsOIOOO_x3;
                                                                                                        let {x15 = Cons x3 x5};
                                                                                                        let {x17 = x15};
                                                                                                        let {x4 = Cons x16 x17};
                                                                                                        return (x0,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4)}]
generatesdsOOIII x2 x3 x4 gen_generatesdsIOIII_x1 gen_generatesdsOIIII_x0 gen_generatesdsOOIII_x0 gen_generatesdsOOIII_x1 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                      x0 <- gen_generatesdsOOIII_x0;
                                                                                                                                      x1 <- gen_generatesdsOOIII_x1;
                                                                                                                                      return (x0,
                                                                                                                                              x1)},
                                                                                                                                  do {(x7,
                                                                                                                                       x8) <- case x4 of
                                                                                                                                              {Cons y7
                                                                                                                                                    y8 -> return (y7,
                                                                                                                                                                  y8);
                                                                                                                                               _ -> mzero};
                                                                                                                                      let {x0 = x7};
                                                                                                                                      let {x6 = x8};
                                                                                                                                      x5 <- case x6 of
                                                                                                                                            {Cons y2
                                                                                                                                                  y5 -> do {guard (x2 == y2);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      x1 <- generatesdsIOIII x0 x2 x3 x5 gen_generatesdsIOIII_x1;
                                                                                                                                      return (x0,
                                                                                                                                              x1)},
                                                                                                                                  do {(x10,
                                                                                                                                       x11) <- case x4 of
                                                                                                                                               {Cons y10
                                                                                                                                                     y11 -> return (y10,
                                                                                                                                                                    y11);
                                                                                                                                                _ -> mzero};
                                                                                                                                      let {x1 = x10};
                                                                                                                                      let {x9 = x11};
                                                                                                                                      x5 <- case x9 of
                                                                                                                                            {Cons y2
                                                                                                                                                  y5 -> do {guard (x2 == y2);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      x0 <- generatesdsOIIII x1 x2 x3 x5 gen_generatesdsOIIII_x0;
                                                                                                                                      return (x0,
                                                                                                                                              x1)},
                                                                                                                                  do {(x13,
                                                                                                                                       x14) <- case x4 of
                                                                                                                                               {Cons y13
                                                                                                                                                     y14 -> return (y13,
                                                                                                                                                                    y14);
                                                                                                                                                _ -> mzero};
                                                                                                                                      let {x0 = x13};
                                                                                                                                      let {x12 = x14};
                                                                                                                                      x5 <- case x12 of
                                                                                                                                            {Cons y3
                                                                                                                                                  y5 -> do {guard (x3 == y3);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      x1 <- generatesdsIOIII x0 x2 x3 x5 gen_generatesdsIOIII_x1;
                                                                                                                                      return (x0,
                                                                                                                                              x1)},
                                                                                                                                  do {(x16,
                                                                                                                                       x17) <- case x4 of
                                                                                                                                               {Cons y16
                                                                                                                                                     y17 -> return (y16,
                                                                                                                                                                    y17);
                                                                                                                                                _ -> mzero};
                                                                                                                                      let {x1 = x16};
                                                                                                                                      let {x15 = x17};
                                                                                                                                      x5 <- case x15 of
                                                                                                                                            {Cons y3
                                                                                                                                                  y5 -> do {guard (x3 == y3);
                                                                                                                                                            return y5};
                                                                                                                                             _ -> mzero};
                                                                                                                                      x0 <- generatesdsOIIII x1 x2 x3 x5 gen_generatesdsOIIII_x0;
                                                                                                                                      return (x0,
                                                                                                                                              x1)}]
generatesdsOOIIO x2 x3 gen_generatesdsOOIIO_x0 gen_generatesdsOOIIO_x1 = Immature $ msum [do {let {x4 = Nil};
                                                                                   x0 <- gen_generatesdsOOIIO_x0;
                                                                                   x1 <- gen_generatesdsOOIIO_x1;
                                                                                   return (x0,
                                                                                           x1,
                                                                                           x4)},
                                                                               do {(x0,
                                                                                    x1,
                                                                                    x5) <- generatesdsOOIIO x2 x3 gen_generatesdsOOIIO_x0 gen_generatesdsOOIIO_x1;
                                                                                   let {x6 = Cons x2 x5};
                                                                                   let {x7 = x0};
                                                                                   let {x8 = x6};
                                                                                   let {x4 = Cons x7 x8};
                                                                                   return (x0,
                                                                                           x1,
                                                                                           x4)},
                                                                               do {(x0,
                                                                                    x1,
                                                                                    x5) <- generatesdsOOIIO x2 x3 gen_generatesdsOOIIO_x0 gen_generatesdsOOIIO_x1;
                                                                                   let {x9 = Cons x2 x5};
                                                                                   let {x10 = x1};
                                                                                   let {x11 = x9};
                                                                                   let {x4 = Cons x10 x11};
                                                                                   return (x0,
                                                                                           x1,
                                                                                           x4)},
                                                                               do {(x0,
                                                                                    x1,
                                                                                    x5) <- generatesdsOOIIO x2 x3 gen_generatesdsOOIIO_x0 gen_generatesdsOOIIO_x1;
                                                                                   let {x12 = Cons x3 x5};
                                                                                   let {x13 = x0};
                                                                                   let {x14 = x12};
                                                                                   let {x4 = Cons x13 x14};
                                                                                   return (x0,
                                                                                           x1,
                                                                                           x4)},
                                                                               do {(x0,
                                                                                    x1,
                                                                                    x5) <- generatesdsOOIIO x2 x3 gen_generatesdsOOIIO_x0 gen_generatesdsOOIIO_x1;
                                                                                   let {x15 = Cons x3 x5};
                                                                                   let {x16 = x1};
                                                                                   let {x17 = x15};
                                                                                   let {x4 = Cons x16 x17};
                                                                                   return (x0,
                                                                                           x1,
                                                                                           x4)}]
generatesdsOOIOI x2 x4 gen_generatesdsIIIOI_x3 gen_generatesdsIOIII_x1 gen_generatesdsIOIOI_x1 gen_generatesdsIOIOI_x3 gen_generatesdsOIIII_x0 gen_generatesdsOIIOI_x0 gen_generatesdsOIIOI_x3 gen_generatesdsOOIOI_x0 gen_generatesdsOOIOI_x1 gen_generatesdsOOIOI_x3 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                                                                                                                                                                   x0 <- gen_generatesdsOOIOI_x0;
                                                                                                                                                                                                                                                                                   x1 <- gen_generatesdsOOIOI_x1;
                                                                                                                                                                                                                                                                                   x3 <- gen_generatesdsOOIOI_x3;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x1,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x7,
                                                                                                                                                                                                                                                                                    x8) <- case x4 of
                                                                                                                                                                                                                                                                                           {Cons y7
                                                                                                                                                                                                                                                                                                 y8 -> return (y7,
                                                                                                                                                                                                                                                                                                               y8);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x0 = x7};
                                                                                                                                                                                                                                                                                   let {x6 = x8};
                                                                                                                                                                                                                                                                                   x5 <- case x6 of
                                                                                                                                                                                                                                                                                         {Cons y2
                                                                                                                                                                                                                                                                                               y5 -> do {guard (x2 == y2);
                                                                                                                                                                                                                                                                                                         return y5};
                                                                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                                                                   (x1,
                                                                                                                                                                                                                                                                                    x3) <- generatesdsIOIOI x0 x2 x5 gen_generatesdsIIIOI_x3 gen_generatesdsIOIII_x1 gen_generatesdsIOIOI_x1 gen_generatesdsIOIOI_x3;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x1,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x10,
                                                                                                                                                                                                                                                                                    x11) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y10
                                                                                                                                                                                                                                                                                                  y11 -> return (y10,
                                                                                                                                                                                                                                                                                                                 y11);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x1 = x10};
                                                                                                                                                                                                                                                                                   let {x9 = x11};
                                                                                                                                                                                                                                                                                   x5 <- case x9 of
                                                                                                                                                                                                                                                                                         {Cons y2
                                                                                                                                                                                                                                                                                               y5 -> do {guard (x2 == y2);
                                                                                                                                                                                                                                                                                                         return y5};
                                                                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                                                                   (x0,
                                                                                                                                                                                                                                                                                    x3) <- generatesdsOIIOI x1 x2 x5 gen_generatesdsIIIOI_x3 gen_generatesdsOIIII_x0 gen_generatesdsOIIOI_x0 gen_generatesdsOIIOI_x3;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x1,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x13,
                                                                                                                                                                                                                                                                                    x14) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y13
                                                                                                                                                                                                                                                                                                  y14 -> return (y13,
                                                                                                                                                                                                                                                                                                                 y14);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x0 = x13};
                                                                                                                                                                                                                                                                                   let {x12 = x14};
                                                                                                                                                                                                                                                                                   (x3,
                                                                                                                                                                                                                                                                                    x5) <- case x12 of
                                                                                                                                                                                                                                                                                           {Cons y3
                                                                                                                                                                                                                                                                                                 y5 -> return (y3,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   x1 <- generatesdsIOIII x0 x2 x3 x5 gen_generatesdsIOIII_x1;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x1,
                                                                                                                                                                                                                                                                                           x3)},
                                                                                                                                                                                                                                                                               do {(x16,
                                                                                                                                                                                                                                                                                    x17) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y16
                                                                                                                                                                                                                                                                                                  y17 -> return (y16,
                                                                                                                                                                                                                                                                                                                 y17);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x1 = x16};
                                                                                                                                                                                                                                                                                   let {x15 = x17};
                                                                                                                                                                                                                                                                                   (x3,
                                                                                                                                                                                                                                                                                    x5) <- case x15 of
                                                                                                                                                                                                                                                                                           {Cons y3
                                                                                                                                                                                                                                                                                                 y5 -> return (y3,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   x0 <- generatesdsOIIII x1 x2 x3 x5 gen_generatesdsOIIII_x0;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x1,
                                                                                                                                                                                                                                                                                           x3)}]
generatesdsOOIOO x2 gen_generatesdsOOIOO_x0 gen_generatesdsOOIOO_x1 gen_generatesdsOOIOO_x3 = Immature $ msum [do {let {x4 = Nil};
                                                                                                        x0 <- gen_generatesdsOOIOO_x0;
                                                                                                        x1 <- gen_generatesdsOOIOO_x1;
                                                                                                        x3 <- gen_generatesdsOOIOO_x3;
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {(x0,
                                                                                                         x1,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsOOIOO x2 gen_generatesdsOOIOO_x0 gen_generatesdsOOIOO_x1 gen_generatesdsOOIOO_x3;
                                                                                                        let {x6 = Cons x2 x5};
                                                                                                        let {x7 = x0};
                                                                                                        let {x8 = x6};
                                                                                                        let {x4 = Cons x7 x8};
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {(x0,
                                                                                                         x1,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsOOIOO x2 gen_generatesdsOOIOO_x0 gen_generatesdsOOIOO_x1 gen_generatesdsOOIOO_x3;
                                                                                                        let {x9 = Cons x2 x5};
                                                                                                        let {x10 = x1};
                                                                                                        let {x11 = x9};
                                                                                                        let {x4 = Cons x10 x11};
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {(x0,
                                                                                                         x1,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsOOIOO x2 gen_generatesdsOOIOO_x0 gen_generatesdsOOIOO_x1 gen_generatesdsOOIOO_x3;
                                                                                                        let {x12 = Cons x3 x5};
                                                                                                        let {x13 = x0};
                                                                                                        let {x14 = x12};
                                                                                                        let {x4 = Cons x13 x14};
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x3,
                                                                                                                x4)},
                                                                                                    do {(x0,
                                                                                                         x1,
                                                                                                         x3,
                                                                                                         x5) <- generatesdsOOIOO x2 gen_generatesdsOOIOO_x0 gen_generatesdsOOIOO_x1 gen_generatesdsOOIOO_x3;
                                                                                                        let {x15 = Cons x3 x5};
                                                                                                        let {x16 = x1};
                                                                                                        let {x17 = x15};
                                                                                                        let {x4 = Cons x16 x17};
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x3,
                                                                                                                x4)}]
generatesdsOOOII x3 x4 gen_generatesdsIIOII_x2 gen_generatesdsIOIII_x1 gen_generatesdsIOOII_x1 gen_generatesdsIOOII_x2 gen_generatesdsOIIII_x0 gen_generatesdsOIOII_x0 gen_generatesdsOIOII_x2 gen_generatesdsOOOII_x0 gen_generatesdsOOOII_x1 gen_generatesdsOOOII_x2 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                                                                                                                                                                   x0 <- gen_generatesdsOOOII_x0;
                                                                                                                                                                                                                                                                                   x1 <- gen_generatesdsOOOII_x1;
                                                                                                                                                                                                                                                                                   x2 <- gen_generatesdsOOOII_x2;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x1,
                                                                                                                                                                                                                                                                                           x2)},
                                                                                                                                                                                                                                                                               do {(x7,
                                                                                                                                                                                                                                                                                    x8) <- case x4 of
                                                                                                                                                                                                                                                                                           {Cons y7
                                                                                                                                                                                                                                                                                                 y8 -> return (y7,
                                                                                                                                                                                                                                                                                                               y8);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x0 = x7};
                                                                                                                                                                                                                                                                                   let {x6 = x8};
                                                                                                                                                                                                                                                                                   (x2,
                                                                                                                                                                                                                                                                                    x5) <- case x6 of
                                                                                                                                                                                                                                                                                           {Cons y2
                                                                                                                                                                                                                                                                                                 y5 -> return (y2,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   x1 <- generatesdsIOIII x0 x2 x3 x5 gen_generatesdsIOIII_x1;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x1,
                                                                                                                                                                                                                                                                                           x2)},
                                                                                                                                                                                                                                                                               do {(x10,
                                                                                                                                                                                                                                                                                    x11) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y10
                                                                                                                                                                                                                                                                                                  y11 -> return (y10,
                                                                                                                                                                                                                                                                                                                 y11);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x1 = x10};
                                                                                                                                                                                                                                                                                   let {x9 = x11};
                                                                                                                                                                                                                                                                                   (x2,
                                                                                                                                                                                                                                                                                    x5) <- case x9 of
                                                                                                                                                                                                                                                                                           {Cons y2
                                                                                                                                                                                                                                                                                                 y5 -> return (y2,
                                                                                                                                                                                                                                                                                                               y5);
                                                                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                                                                   x0 <- generatesdsOIIII x1 x2 x3 x5 gen_generatesdsOIIII_x0;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x1,
                                                                                                                                                                                                                                                                                           x2)},
                                                                                                                                                                                                                                                                               do {(x13,
                                                                                                                                                                                                                                                                                    x14) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y13
                                                                                                                                                                                                                                                                                                  y14 -> return (y13,
                                                                                                                                                                                                                                                                                                                 y14);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x0 = x13};
                                                                                                                                                                                                                                                                                   let {x12 = x14};
                                                                                                                                                                                                                                                                                   x5 <- case x12 of
                                                                                                                                                                                                                                                                                         {Cons y3
                                                                                                                                                                                                                                                                                               y5 -> do {guard (x3 == y3);
                                                                                                                                                                                                                                                                                                         return y5};
                                                                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                                                                   (x1,
                                                                                                                                                                                                                                                                                    x2) <- generatesdsIOOII x0 x3 x5 gen_generatesdsIIOII_x2 gen_generatesdsIOIII_x1 gen_generatesdsIOOII_x1 gen_generatesdsIOOII_x2;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x1,
                                                                                                                                                                                                                                                                                           x2)},
                                                                                                                                                                                                                                                                               do {(x16,
                                                                                                                                                                                                                                                                                    x17) <- case x4 of
                                                                                                                                                                                                                                                                                            {Cons y16
                                                                                                                                                                                                                                                                                                  y17 -> return (y16,
                                                                                                                                                                                                                                                                                                                 y17);
                                                                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                                                                   let {x1 = x16};
                                                                                                                                                                                                                                                                                   let {x15 = x17};
                                                                                                                                                                                                                                                                                   x5 <- case x15 of
                                                                                                                                                                                                                                                                                         {Cons y3
                                                                                                                                                                                                                                                                                               y5 -> do {guard (x3 == y3);
                                                                                                                                                                                                                                                                                                         return y5};
                                                                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                                                                   (x0,
                                                                                                                                                                                                                                                                                    x2) <- generatesdsOIOII x1 x3 x5 gen_generatesdsIIOII_x2 gen_generatesdsOIIII_x0 gen_generatesdsOIOII_x0 gen_generatesdsOIOII_x2;
                                                                                                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                                                                                                           x1,
                                                                                                                                                                                                                                                                                           x2)}]
generatesdsOOOIO x3 gen_generatesdsOOOIO_x0 gen_generatesdsOOOIO_x1 gen_generatesdsOOOIO_x2 = Immature $ msum [do {let {x4 = Nil};
                                                                                                        x0 <- gen_generatesdsOOOIO_x0;
                                                                                                        x1 <- gen_generatesdsOOOIO_x1;
                                                                                                        x2 <- gen_generatesdsOOOIO_x2;
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x2,
                                                                                                                x4)},
                                                                                                    do {(x0,
                                                                                                         x1,
                                                                                                         x2,
                                                                                                         x5) <- generatesdsOOOIO x3 gen_generatesdsOOOIO_x0 gen_generatesdsOOOIO_x1 gen_generatesdsOOOIO_x2;
                                                                                                        let {x6 = Cons x2 x5};
                                                                                                        let {x7 = x0};
                                                                                                        let {x8 = x6};
                                                                                                        let {x4 = Cons x7 x8};
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x2,
                                                                                                                x4)},
                                                                                                    do {(x0,
                                                                                                         x1,
                                                                                                         x2,
                                                                                                         x5) <- generatesdsOOOIO x3 gen_generatesdsOOOIO_x0 gen_generatesdsOOOIO_x1 gen_generatesdsOOOIO_x2;
                                                                                                        let {x9 = Cons x2 x5};
                                                                                                        let {x10 = x1};
                                                                                                        let {x11 = x9};
                                                                                                        let {x4 = Cons x10 x11};
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x2,
                                                                                                                x4)},
                                                                                                    do {(x0,
                                                                                                         x1,
                                                                                                         x2,
                                                                                                         x5) <- generatesdsOOOIO x3 gen_generatesdsOOOIO_x0 gen_generatesdsOOOIO_x1 gen_generatesdsOOOIO_x2;
                                                                                                        let {x12 = Cons x3 x5};
                                                                                                        let {x13 = x0};
                                                                                                        let {x14 = x12};
                                                                                                        let {x4 = Cons x13 x14};
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x2,
                                                                                                                x4)},
                                                                                                    do {(x0,
                                                                                                         x1,
                                                                                                         x2,
                                                                                                         x5) <- generatesdsOOOIO x3 gen_generatesdsOOOIO_x0 gen_generatesdsOOOIO_x1 gen_generatesdsOOOIO_x2;
                                                                                                        let {x15 = Cons x3 x5};
                                                                                                        let {x16 = x1};
                                                                                                        let {x17 = x15};
                                                                                                        let {x4 = Cons x16 x17};
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x2,
                                                                                                                x4)}]
generatesdsOOOOI x4 gen_generatesdsIIIOI_x3 gen_generatesdsIIOII_x2 gen_generatesdsIOIII_x1 gen_generatesdsIOIOI_x1 gen_generatesdsIOIOI_x3 gen_generatesdsIOOII_x1 gen_generatesdsIOOII_x2 gen_generatesdsOIIII_x0 gen_generatesdsOIIOI_x0 gen_generatesdsOIIOI_x3 gen_generatesdsOIOII_x0 gen_generatesdsOIOII_x2 gen_generatesdsOOOOI_x0 gen_generatesdsOOOOI_x1 gen_generatesdsOOOOI_x2 gen_generatesdsOOOOI_x3 = Immature $ msum [do {guard (x4 == Nil);
                                                                                                                                                                                                                                                                                                                                                                                                                                x0 <- gen_generatesdsOOOOI_x0;
                                                                                                                                                                                                                                                                                                                                                                                                                                x1 <- gen_generatesdsOOOOI_x1;
                                                                                                                                                                                                                                                                                                                                                                                                                                x2 <- gen_generatesdsOOOOI_x2;
                                                                                                                                                                                                                                                                                                                                                                                                                                x3 <- gen_generatesdsOOOOI_x3;
                                                                                                                                                                                                                                                                                                                                                                                                                                return (x0,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x1,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x2,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x3)},
                                                                                                                                                                                                                                                                                                                                                                                                                            do {(x7,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x8) <- case x4 of
                                                                                                                                                                                                                                                                                                                                                                                                                                        {Cons y7
                                                                                                                                                                                                                                                                                                                                                                                                                                              y8 -> return (y7,
                                                                                                                                                                                                                                                                                                                                                                                                                                                            y8);
                                                                                                                                                                                                                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                                                                                                                                                                                                                let {x0 = x7};
                                                                                                                                                                                                                                                                                                                                                                                                                                let {x6 = x8};
                                                                                                                                                                                                                                                                                                                                                                                                                                (x2,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x5) <- case x6 of
                                                                                                                                                                                                                                                                                                                                                                                                                                        {Cons y2
                                                                                                                                                                                                                                                                                                                                                                                                                                              y5 -> return (y2,
                                                                                                                                                                                                                                                                                                                                                                                                                                                            y5);
                                                                                                                                                                                                                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                                                                                                                                                                                                                (x1,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x3) <- generatesdsIOIOI x0 x2 x5 gen_generatesdsIIIOI_x3 gen_generatesdsIOIII_x1 gen_generatesdsIOIOI_x1 gen_generatesdsIOIOI_x3;
                                                                                                                                                                                                                                                                                                                                                                                                                                return (x0,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x1,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x2,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x3)},
                                                                                                                                                                                                                                                                                                                                                                                                                            do {(x10,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x11) <- case x4 of
                                                                                                                                                                                                                                                                                                                                                                                                                                         {Cons y10
                                                                                                                                                                                                                                                                                                                                                                                                                                               y11 -> return (y10,
                                                                                                                                                                                                                                                                                                                                                                                                                                                              y11);
                                                                                                                                                                                                                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                                                                                                                                                                                                                let {x1 = x10};
                                                                                                                                                                                                                                                                                                                                                                                                                                let {x9 = x11};
                                                                                                                                                                                                                                                                                                                                                                                                                                (x2,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x5) <- case x9 of
                                                                                                                                                                                                                                                                                                                                                                                                                                        {Cons y2
                                                                                                                                                                                                                                                                                                                                                                                                                                              y5 -> return (y2,
                                                                                                                                                                                                                                                                                                                                                                                                                                                            y5);
                                                                                                                                                                                                                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                                                                                                                                                                                                                (x0,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x3) <- generatesdsOIIOI x1 x2 x5 gen_generatesdsIIIOI_x3 gen_generatesdsOIIII_x0 gen_generatesdsOIIOI_x0 gen_generatesdsOIIOI_x3;
                                                                                                                                                                                                                                                                                                                                                                                                                                return (x0,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x1,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x2,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x3)},
                                                                                                                                                                                                                                                                                                                                                                                                                            do {(x13,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x14) <- case x4 of
                                                                                                                                                                                                                                                                                                                                                                                                                                         {Cons y13
                                                                                                                                                                                                                                                                                                                                                                                                                                               y14 -> return (y13,
                                                                                                                                                                                                                                                                                                                                                                                                                                                              y14);
                                                                                                                                                                                                                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                                                                                                                                                                                                                let {x0 = x13};
                                                                                                                                                                                                                                                                                                                                                                                                                                let {x12 = x14};
                                                                                                                                                                                                                                                                                                                                                                                                                                (x3,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x5) <- case x12 of
                                                                                                                                                                                                                                                                                                                                                                                                                                        {Cons y3
                                                                                                                                                                                                                                                                                                                                                                                                                                              y5 -> return (y3,
                                                                                                                                                                                                                                                                                                                                                                                                                                                            y5);
                                                                                                                                                                                                                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                                                                                                                                                                                                                (x1,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x2) <- generatesdsIOOII x0 x3 x5 gen_generatesdsIIOII_x2 gen_generatesdsIOIII_x1 gen_generatesdsIOOII_x1 gen_generatesdsIOOII_x2;
                                                                                                                                                                                                                                                                                                                                                                                                                                return (x0,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x1,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x2,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x3)},
                                                                                                                                                                                                                                                                                                                                                                                                                            do {(x16,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x17) <- case x4 of
                                                                                                                                                                                                                                                                                                                                                                                                                                         {Cons y16
                                                                                                                                                                                                                                                                                                                                                                                                                                               y17 -> return (y16,
                                                                                                                                                                                                                                                                                                                                                                                                                                                              y17);
                                                                                                                                                                                                                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                                                                                                                                                                                                                let {x1 = x16};
                                                                                                                                                                                                                                                                                                                                                                                                                                let {x15 = x17};
                                                                                                                                                                                                                                                                                                                                                                                                                                (x3,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x5) <- case x15 of
                                                                                                                                                                                                                                                                                                                                                                                                                                        {Cons y3
                                                                                                                                                                                                                                                                                                                                                                                                                                              y5 -> return (y3,
                                                                                                                                                                                                                                                                                                                                                                                                                                                            y5);
                                                                                                                                                                                                                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                                                                                                                                                                                                                (x0,
                                                                                                                                                                                                                                                                                                                                                                                                                                 x2) <- generatesdsOIOII x1 x3 x5 gen_generatesdsIIOII_x2 gen_generatesdsOIIII_x0 gen_generatesdsOIOII_x0 gen_generatesdsOIOII_x2;
                                                                                                                                                                                                                                                                                                                                                                                                                                return (x0,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x1,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x2,
                                                                                                                                                                                                                                                                                                                                                                                                                                        x3)}]
generatesdsOOOOO gen_generatesdsOOOOO_x0 gen_generatesdsOOOOO_x1 gen_generatesdsOOOOO_x2 gen_generatesdsOOOOO_x3 = Immature $ msum [do {let {x4 = Nil};
                                                                                                                             x0 <- gen_generatesdsOOOOO_x0;
                                                                                                                             x1 <- gen_generatesdsOOOOO_x1;
                                                                                                                             x2 <- gen_generatesdsOOOOO_x2;
                                                                                                                             x3 <- gen_generatesdsOOOOO_x3;
                                                                                                                             return (x0,
                                                                                                                                     x1,
                                                                                                                                     x2,
                                                                                                                                     x3,
                                                                                                                                     x4)},
                                                                                                                         do {(x0,
                                                                                                                              x1,
                                                                                                                              x2,
                                                                                                                              x3,
                                                                                                                              x5) <- generatesdsOOOOO gen_generatesdsOOOOO_x0 gen_generatesdsOOOOO_x1 gen_generatesdsOOOOO_x2 gen_generatesdsOOOOO_x3;
                                                                                                                             let {x6 = Cons x2 x5};
                                                                                                                             let {x7 = x0};
                                                                                                                             let {x8 = x6};
                                                                                                                             let {x4 = Cons x7 x8};
                                                                                                                             return (x0,
                                                                                                                                     x1,
                                                                                                                                     x2,
                                                                                                                                     x3,
                                                                                                                                     x4)},
                                                                                                                         do {(x0,
                                                                                                                              x1,
                                                                                                                              x2,
                                                                                                                              x3,
                                                                                                                              x5) <- generatesdsOOOOO gen_generatesdsOOOOO_x0 gen_generatesdsOOOOO_x1 gen_generatesdsOOOOO_x2 gen_generatesdsOOOOO_x3;
                                                                                                                             let {x9 = Cons x2 x5};
                                                                                                                             let {x10 = x1};
                                                                                                                             let {x11 = x9};
                                                                                                                             let {x4 = Cons x10 x11};
                                                                                                                             return (x0,
                                                                                                                                     x1,
                                                                                                                                     x2,
                                                                                                                                     x3,
                                                                                                                                     x4)},
                                                                                                                         do {(x0,
                                                                                                                              x1,
                                                                                                                              x2,
                                                                                                                              x3,
                                                                                                                              x5) <- generatesdsOOOOO gen_generatesdsOOOOO_x0 gen_generatesdsOOOOO_x1 gen_generatesdsOOOOO_x2 gen_generatesdsOOOOO_x3;
                                                                                                                             let {x12 = Cons x3 x5};
                                                                                                                             let {x13 = x0};
                                                                                                                             let {x14 = x12};
                                                                                                                             let {x4 = Cons x13 x14};
                                                                                                                             return (x0,
                                                                                                                                     x1,
                                                                                                                                     x2,
                                                                                                                                     x3,
                                                                                                                                     x4)},
                                                                                                                         do {(x0,
                                                                                                                              x1,
                                                                                                                              x2,
                                                                                                                              x3,
                                                                                                                              x5) <- generatesdsOOOOO gen_generatesdsOOOOO_x0 gen_generatesdsOOOOO_x1 gen_generatesdsOOOOO_x2 gen_generatesdsOOOOO_x3;
                                                                                                                             let {x15 = Cons x3 x5};
                                                                                                                             let {x16 = x1};
                                                                                                                             let {x17 = x15};
                                                                                                                             let {x4 = Cons x16 x17};
                                                                                                                             return (x0,
                                                                                                                                     x1,
                                                                                                                                     x2,
                                                                                                                                     x3,
                                                                                                                                     x4)}]