module Hanoi_simple where

import Stream
import Control.Monad
import Term

checkIII x0 x1 x2 = Immature $ msum [do {let {x29 = One};
                              let {x30 = Two};
                              guard (x1 == Nil);
                              x5 <- Immature $ getIIO x29 x0;
                              x3 <- Immature $ isNilIO x5;
                              x6 <- Immature $ getIIO x30 x0;
                              x4 <- Immature $ isNilIO x6;
                              check2III x2 x3 x4;
                              return ()},
                          do {(x31, x32) <- Immature $ case x1 of
                                            {Cons y31 y32 -> return (y31, y32); _ -> mzero};
                              let {x3 = x31};
                              let {x4 = x32};
                              x5 <- Immature $ one_stepIIO x3 x0;
                              checkIII x5 x4 x2;
                              return ()}]
checkIIO x0 x1 = Immature $ msum [do {let {x29 = One};
                           let {x30 = Two};
                           guard (x1 == Nil);
                           x5 <- Immature $ getIIO x29 x0;
                           x3 <- Immature $ isNilIO x5;
                           x6 <- Immature $ getIIO x30 x0;
                           x4 <- Immature $ isNilIO x6;
                           x2 <- Immature $ check2OII x3 x4;
                           return x2},
                       do {(x31, x32) <- Immature $ case x1 of
                                         {Cons y31 y32 -> return (y31, y32); _ -> mzero};
                           let {x3 = x31};
                           let {x4 = x32};
                           x5 <- Immature $ one_stepIIO x3 x0;
                           x2 <- Immature $ checkIIO x5 x4;
                           return x2}]
checkIOI x0 x2 = Immature $ msum [do {let {x1 = Nil};
                           let {x29 = One};
                           let {x30 = Two};
                           x5 <- Immature $ getIIO x29 x0;
                           x3 <- Immature $ isNilIO x5;
                           x6 <- Immature $ getIIO x30 x0;
                           x4 <- Immature $ isNilIO x6;
                           check2III x2 x3 x4;
                           return x1},
                       do {(x3, x5) <- Immature $ one_stepOIO x0;
                           let {x31 = x3};
                           x4 <- Immature $ checkIOI x5 x2;
                           let {x32 = x4};
                           let {x1 = Cons x31 x32};
                           return x1}]
checkIOO x0 = Immature $ msum [do {let {x1 = Nil};
                        let {x29 = One};
                        let {x30 = Two};
                        x5 <- Immature $ getIIO x29 x0;
                        x3 <- Immature $ isNilIO x5;
                        x6 <- Immature $ getIIO x30 x0;
                        x4 <- Immature $ isNilIO x6;
                        x2 <- Immature $ check2OII x3 x4;
                        return (x1, x2)},
                    do {(x3, x5) <- Immature $ one_stepOIO x0;
                        let {x31 = x3};
                        (x4, x2) <- Immature $ checkIOO x5;
                        let {x32 = x4};
                        let {x1 = Cons x31 x32};
                        return (x1, x2)}]
checkOII x1 x2 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5 = Immature $ msum [do {let {x29 = One};
                                                                                               let {x30 = Two};
                                                                                               guard (x1 == Nil);
                                                                                               (x0,
                                                                                                x5) <- Immature $ getIOO x29 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                               x3 <- Immature $ isNilIO x5;
                                                                                               x6 <- Immature $ getIIO x30 x0;
                                                                                               x4 <- Immature $ isNilIO x6;
                                                                                               check2III x2 x3 x4;
                                                                                               return x0},
                                                                                           do {(x31,
                                                                                                x32) <- Immature $ case x1 of
                                                                                                        {Cons y31
                                                                                                              y32 -> return (y31,
                                                                                                                             y32);
                                                                                                         _ -> mzero};
                                                                                               let {x3 = x31};
                                                                                               let {x4 = x32};
                                                                                               x5 <- Immature $ checkOII x4 x2 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                               x0 <- Immature $ one_stepIOI x3 x5 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                               return x0}]
checkOIO x1 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5 = Immature $ msum [do {let {x29 = One};
                                                                                            let {x30 = Two};
                                                                                            guard (x1 == Nil);
                                                                                            (x0,
                                                                                             x5) <- Immature $ getIOO x29 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                            x3 <- Immature $ isNilIO x5;
                                                                                            x6 <- Immature $ getIIO x30 x0;
                                                                                            x4 <- Immature $ isNilIO x6;
                                                                                            x2 <- Immature $ check2OII x3 x4;
                                                                                            return (x0,
                                                                                                    x2)},
                                                                                        do {(x31,
                                                                                             x32) <- Immature $ case x1 of
                                                                                                     {Cons y31
                                                                                                           y32 -> return (y31,
                                                                                                                          y32);
                                                                                                      _ -> mzero};
                                                                                            let {x3 = x31};
                                                                                            let {x4 = x32};
                                                                                            (x5,
                                                                                             x2) <- Immature $ checkOIO x4 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                            x0 <- Immature $ one_stepIOI x3 x5 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                            return (x0,
                                                                                                    x2)}]
checkOOI x2 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5 = Immature $ msum [do {let {x1 = Nil};
                                                                                            let {x29 = One};
                                                                                            let {x30 = Two};
                                                                                            (x0,
                                                                                             x5) <- Immature $ getIOO x29 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                            x3 <- Immature $ isNilIO x5;
                                                                                            x6 <- Immature $ getIIO x30 x0;
                                                                                            x4 <- Immature $ isNilIO x6;
                                                                                            check2III x2 x3 x4;
                                                                                            return (x0,
                                                                                                    x1)},
                                                                                        do {(x5,
                                                                                             x4) <- Immature $ checkOOI x2 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                            let {x32 = x4};
                                                                                            (x3,
                                                                                             x0) <- Immature $ one_stepOOI x5 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                            let {x31 = x3};
                                                                                            let {x1 = Cons x31 x32};
                                                                                            return (x0,
                                                                                                    x1)}]
checkOOO gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5 = Immature $ msum [do {let {x1 = Nil};
                                                                                         let {x29 = One};
                                                                                         let {x30 = Two};
                                                                                         (x0,
                                                                                          x5) <- Immature $ getIOO x29 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                         x3 <- Immature $ isNilIO x5;
                                                                                         x6 <- Immature $ getIIO x30 x0;
                                                                                         x4 <- Immature $ isNilIO x6;
                                                                                         x2 <- Immature $ check2OII x3 x4;
                                                                                         return (x0,
                                                                                                 x1,
                                                                                                 x2)},
                                                                                     do {(x5,
                                                                                          x4,
                                                                                          x2) <- Immature $ checkOOO gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                         let {x32 = x4};
                                                                                         (x3,
                                                                                          x0) <- Immature $ one_stepOOI x5 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                         let {x31 = x3};
                                                                                         let {x1 = Cons x31 x32};
                                                                                         return (x0,
                                                                                                 x1,
                                                                                                 x2)}]
check2III x2 x3 x4 = Immature $ msum [do {guard (x3 == Falso);
                               guard (x2 == Falso);
                               return ()},
                           do {guard (x2 == x4); guard (x3 == Trueo); return ()}]
check2OII x3 x4 = Immature $ msum [do {let {x2 = Falso};
                            guard (x3 == Falso);
                            return x2},
                        do {guard (x3 == Trueo); let {x2 = x4}; return x2}]
getIIO x0 x1 = Immature $ msum [do {(x3, x4, x5) <- Immature $ case x1 of
                                         {Triple y3 y4 y5 -> return (y3, y4, y5); _ -> mzero};
                         x2 <- Immature $ get2IOIII x0 x3 x4 x5;
                         return x2}]
getIOO x0 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5 = Immature $ msum [do {(x2,
                                                                                           x3,
                                                                                           x4,
                                                                                           x5) <- Immature $ get2IOOOO x0 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                          let {x1 = Triple x3 x4 x5};
                                                                                          return (x1,
                                                                                                  x2)}]
get2IOIII x0 x3 x4 x5 = Immature $ msum [do {guard (x0 == One);
                                  let {x2 = x3};
                                  return x2},
                              do {guard (x0 == Two); let {x2 = x4}; return x2},
                              do {guard (x0 == Thr); let {x2 = x5}; return x2}]
get2IOOOO x0 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5 = Immature $ msum [do {guard (x0 == One);
                                                                                             (x3,
                                                                                              x2) <- Immature $ do {x2 <- Immature $ gen_get2IOOOO_x2;
                                                                                                         return (x2,
                                                                                                                 x2)};
                                                                                             x4 <- Immature $ gen_get2IOOOO_x4;
                                                                                             x5 <- Immature $ gen_get2IOOOO_x5;
                                                                                             return (x2,
                                                                                                     x3,
                                                                                                     x4,
                                                                                                     x5)},
                                                                                         do {guard (x0 == Two);
                                                                                             (x4,
                                                                                              x2) <- Immature $ do {x2 <- Immature $ gen_get2IOOOO_x2;
                                                                                                         return (x2,
                                                                                                                 x2)};
                                                                                             x3 <- Immature $ gen_get2IOOOO_x3;
                                                                                             x5 <- Immature $ gen_get2IOOOO_x5;
                                                                                             return (x2,
                                                                                                     x3,
                                                                                                     x4,
                                                                                                     x5)},
                                                                                         do {guard (x0 == Thr);
                                                                                             (x5,
                                                                                              x2) <- Immature $ do {x2 <- Immature $ gen_get2IOOOO_x2;
                                                                                                         return (x2,
                                                                                                                 x2)};
                                                                                             x3 <- Immature $ gen_get2IOOOO_x3;
                                                                                             x4 <- Immature $ gen_get2IOOOO_x4;
                                                                                             return (x2,
                                                                                                     x3,
                                                                                                     x4,
                                                                                                     x5)}]
isNilIO x0 = Immature $ msum [do {let {x1 = Trueo};
                       guard (x0 == Nil);
                       return x1},
                   do {let {x1 = Falso};
                       (x2, x3) <- Immature $ case x0 of
                                   {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                       return x1}]
one_stepIIO x0 x1 = Immature $ msum [do {let {x5 = Trueo};
                              (x23, x24) <- Immature $ case x0 of
                                            {Pair y23 y24 -> return (y23, y24); _ -> mzero};
                              let {x3 = x23};
                              let {x4 = x24};
                              notEqStickIII x3 x4 x5;
                              x6 <- Immature $ getIIO x3 x1;
                              (x7, x8) <- Immature $ case x6 of
                                          {Cons y7 y8 -> return (y7, y8); _ -> mzero};
                              x9 <- Immature $ getIIO x4 x1;
                              x2 <- Immature $ one_step2IOIIIII x1 x3 x4 x7 x8 x9;
                              return x2}]
notEqStickIII x0 x1 x2 = Immature $ msum [do {notEqStick0II x1 x2;
                                   guard (x0 == One);
                                   return ()},
                               do {notEqStick1II x1 x2; guard (x0 == Two); return ()},
                               do {notEqStick2II x1 x2; guard (x0 == Thr); return ()}]
notEqStick0II x1 x2 = Immature $ msum [do {guard (x1 == One);
                                guard (x2 == Falso);
                                return ()},
                            do {guard (x1 == Two); guard (x2 == Trueo); return ()},
                            do {guard (x1 == Thr); guard (x2 == Trueo); return ()}]
notEqStick1II x1 x2 = Immature $ msum [do {guard (x1 == One);
                                guard (x2 == Trueo);
                                return ()},
                            do {guard (x1 == Two); guard (x2 == Falso); return ()},
                            do {guard (x1 == Thr); guard (x2 == Trueo); return ()}]
notEqStick2II x1 x2 = Immature $ msum [do {guard (x1 == One);
                                guard (x2 == Trueo);
                                return ()},
                            do {guard (x1 == Two); guard (x2 == Trueo); return ()},
                            do {guard (x1 == Thr); guard (x2 == Falso); return ()}]
one_stepIOI x0 x2 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5 = Immature $ msum [do {let {x5 = Trueo};
                                                                                                  (x23,
                                                                                                   x24) <- Immature $ case x0 of
                                                                                                           {Pair y23
                                                                                                                 y24 -> return (y23,
                                                                                                                                y24);
                                                                                                            _ -> mzero};
                                                                                                  let {x3 = x23};
                                                                                                  let {x4 = x24};
                                                                                                  notEqStickIII x3 x4 x5;
                                                                                                  (x1,
                                                                                                   x6) <- Immature $ getIOO x3 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                                  (x7,
                                                                                                   x8) <- Immature $ case x6 of
                                                                                                          {Cons y7
                                                                                                                y8 -> return (y7,
                                                                                                                              y8);
                                                                                                           _ -> mzero};
                                                                                                  x9 <- Immature $ getIIO x4 x1;
                                                                                                  one_step2IIIIIII x1 x2 x3 x4 x7 x8 x9;
                                                                                                  return x1}]
one_stepOIO x1 = Immature $ msum [do {let {x5 = Trueo};
                           (x3, x4) <- Immature $ notEqStickOOI x5;
                           let {x23 = x3};
                           let {x24 = x4};
                           let {x0 = Pair x23 x24};
                           x6 <- Immature $ getIIO x3 x1;
                           (x7, x8) <- Immature $ case x6 of
                                       {Cons y7 y8 -> return (y7, y8); _ -> mzero};
                           x9 <- Immature $ getIIO x4 x1;
                           x2 <- Immature $ one_step2IOIIIII x1 x3 x4 x7 x8 x9;
                           return (x0, x2)}]
notEqStickOOI x2 = Immature $ msum [do {let {x0 = One};
                             x1 <- Immature $ notEqStick0OI x2;
                             return (x0, x1)},
                         do {let {x0 = Two}; x1 <- Immature $ notEqStick1OI x2; return (x0, x1)},
                         do {let {x0 = Thr}; x1 <- Immature $ notEqStick2OI x2; return (x0, x1)}]
notEqStick0OI x2 = Immature $ msum [do {let {x1 = One};
                             guard (x2 == Falso);
                             return x1},
                         do {let {x1 = Two}; guard (x2 == Trueo); return x1},
                         do {let {x1 = Thr}; guard (x2 == Trueo); return x1}]
notEqStick1OI x2 = Immature $ msum [do {let {x1 = One};
                             guard (x2 == Trueo);
                             return x1},
                         do {let {x1 = Two}; guard (x2 == Falso); return x1},
                         do {let {x1 = Thr}; guard (x2 == Trueo); return x1}]
notEqStick2OI x2 = Immature $ msum [do {let {x1 = One};
                             guard (x2 == Trueo);
                             return x1},
                         do {let {x1 = Two}; guard (x2 == Trueo); return x1},
                         do {let {x1 = Thr}; guard (x2 == Falso); return x1}]
one_stepOOI x2 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5 = Immature $ msum [do {let {x5 = Trueo};
                                                                                               (x3,
                                                                                                x4) <- Immature $ notEqStickOOI x5;
                                                                                               let {x23 = x3};
                                                                                               let {x24 = x4};
                                                                                               let {x0 = Pair x23 x24};
                                                                                               (x1,
                                                                                                x6) <- Immature $ getIOO x3 gen_get2IOOOO_x2 gen_get2IOOOO_x3 gen_get2IOOOO_x4 gen_get2IOOOO_x5;
                                                                                               (x7,
                                                                                                x8) <- Immature $ case x6 of
                                                                                                       {Cons y7
                                                                                                             y8 -> return (y7,
                                                                                                                           y8);
                                                                                                        _ -> mzero};
                                                                                               x9 <- Immature $ getIIO x4 x1;
                                                                                               one_step2IIIIIII x1 x2 x3 x4 x7 x8 x9;
                                                                                               return (x0,
                                                                                                       x1)}]
one_step2IIIIIII x1 x2 x3 x4 x7 x8 x9 = Immature $ msum [do {let {x26 = Nil};
                                                  let {x25 = Cons x7 x26};
                                                  guard (x9 == Nil);
                                                  x10 <- Immature $ setIIIO x3 x8 x1;
                                                  setIIII x4 x25 x10 x2;
                                                  return ()},
                                              do {let {x12 = Trueo};
                                                  (x10, x11) <- Immature $ case x9 of
                                                                {Cons y10 y11 -> return (y10, y11);
                                                                 _ -> mzero};
                                                  lessIII x7 x10 x12;
                                                  let {x28 = Cons x10 x11};
                                                  let {x27 = Cons x7 x28};
                                                  x13 <- Immature $ setIIIO x3 x8 x1;
                                                  setIIII x4 x27 x13 x2;
                                                  return ()}]
lessIII x0 x1 x2 = Immature $ msum [do {x3 <- Immature $ case x1 of
                                   {S y3 -> return y3; _ -> mzero};
                             less2III x0 x2 x3;
                             return ()}]
less2III x0 x2 x3 = Immature $ msum [do {guard (x0 == O);
                              guard (x2 == Trueo);
                              return ()},
                          do {x4 <- Immature $ case x0 of
                                    {S y4 -> return y4; _ -> mzero};
                              lessIII x4 x3 x2;
                              return ()}]
one_step2IOIIIII x1 x3 x4 x7 x8 x9 = Immature $ msum [do {let {x26 = Nil};
                                               let {x25 = Cons x7 x26};
                                               guard (x9 == Nil);
                                               x10 <- Immature $ setIIIO x3 x8 x1;
                                               x2 <- Immature $ setIIIO x4 x25 x10;
                                               return x2},
                                           do {let {x12 = Trueo};
                                               (x10, x11) <- Immature $ case x9 of
                                                             {Cons y10 y11 -> return (y10, y11);
                                                              _ -> mzero};
                                               lessIII x7 x10 x12;
                                               let {x28 = Cons x10 x11};
                                               let {x27 = Cons x7 x28};
                                               x13 <- Immature $ setIIIO x3 x8 x1;
                                               x2 <- Immature $ setIIIO x4 x27 x13;
                                               return x2}]
setIIII x0 x1 x2 x3 = Immature $ msum [do {(x4, x5, x6) <- Immature $ case x2 of
                                                {Triple y4 y5 y6 -> return (y4, y5, y6);
                                                 _ -> mzero};
                                set2IIIIII x0 x1 x3 x4 x5 x6;
                                return ()}]
setIIIO x0 x1 x2 = Immature $ msum [do {(x4, x5, x6) <- Immature $ case x2 of
                                             {Triple y4 y5 y6 -> return (y4, y5, y6); _ -> mzero};
                             x3 <- Immature $ set2IIOIII x0 x1 x4 x5 x6;
                             return x3}]
set2IIIIII x0 x1 x3 x4 x5 x6 = Immature $ msum [do {guard (x0 == One);
                                         (x14, x15, x16) <- Immature $ case x3 of
                                                            {Triple y14 y15 y16 -> return (y14,
                                                                                           y15,
                                                                                           y16);
                                                             _ -> mzero};
                                         guard (x14 == x1);
                                         guard (x15 == x5);
                                         guard (x16 == x6);
                                         return ()},
                                     do {guard (x0 == Two);
                                         (x17, x18, x19) <- Immature $ case x3 of
                                                            {Triple y17 y18 y19 -> return (y17,
                                                                                           y18,
                                                                                           y19);
                                                             _ -> mzero};
                                         guard (x17 == x4);
                                         guard (x18 == x1);
                                         guard (x19 == x6);
                                         return ()},
                                     do {guard (x0 == Thr);
                                         (x20, x21, x22) <- Immature $ case x3 of
                                                            {Triple y20 y21 y22 -> return (y20,
                                                                                           y21,
                                                                                           y22);
                                                             _ -> mzero};
                                         guard (x20 == x4);
                                         guard (x21 == x5);
                                         guard (x22 == x1);
                                         return ()}]
set2IIOIII x0 x1 x4 x5 x6 = Immature $ msum [do {guard (x0 == One);
                                      let {x14 = x1};
                                      let {x15 = x5};
                                      let {x16 = x6};
                                      let {x3 = Triple x14 x15 x16};
                                      return x3},
                                  do {guard (x0 == Two);
                                      let {x17 = x4};
                                      let {x18 = x1};
                                      let {x19 = x6};
                                      let {x3 = Triple x17 x18 x19};
                                      return x3},
                                  do {guard (x0 == Thr);
                                      let {x20 = x4};
                                      let {x21 = x5};
                                      let {x22 = x1};
                                      let {x3 = Triple x20 x21 x22};
                                      return x3}]