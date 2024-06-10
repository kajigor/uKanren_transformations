module Match_online where

import Stream
import Control.Monad
import Term

matchoI x0 = Immature $ msum [do {appendoI x0; return ()},
                   do {appendoAppendoI x0; return ()}]
appendoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                    do {let {x4 = O};
                        let {x3 = S x4};
                        let {x2 = S x3};
                        (x5, x1) <- case x0 of
                                    {Cons y5 y1 -> return (y5, y1); _ -> mzero};
                        guard (x5 == x2);
                        _appendoI x1;
                        return ()}]
_appendoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                     do {let {x7 = O};
                         let {x6 = S x7};
                         (x8, x1) <- case x0 of
                                     {Cons y8 y1 -> return (y8, y1); _ -> mzero};
                         guard (x8 == x6);
                         __appendoI x1;
                         return ()}]
__appendoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                      do {let {x10 = O};
                          let {x9 = S x10};
                          (x11, x1) <- case x0 of
                                       {Cons y11 y1 -> return (y11, y1); _ -> mzero};
                          guard (x11 == x9);
                          ___appendoI x1;
                          return ()}]
___appendoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                       do {let {x12 = O};
                           (x13, x1) <- case x0 of
                                        {Cons y13 y1 -> return (y13, y1); _ -> mzero};
                           guard (x13 == x12);
                           ____appendoI x1;
                           return ()}]
____appendoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                        do {let {x14 = O};
                            (x15, x1) <- case x0 of
                                         {Cons y15 y1 -> return (y15, y1); _ -> mzero};
                            guard (x15 == x14);
                            _____appendoI x1;
                            return ()}]
_____appendoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                         do {let {x18 = O};
                             let {x17 = S x18};
                             let {x16 = S x17};
                             (x19, x1) <- case x0 of
                                          {Cons y19 y1 -> return (y19, y1); _ -> mzero};
                             guard (x19 == x16);
                             ______appendoI x1;
                             return ()}]
______appendoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                          do {let {x20 = O};
                              (x21, x1) <- case x0 of
                                           {Cons y21 y1 -> return (y21, y1); _ -> mzero};
                              guard (x21 == x20);
                              _______appendoI x1;
                              return ()}]
_______appendoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                           do {let {x23 = O};
                               let {x22 = S x23};
                               let {x24 = Nil};
                               (x25, x26) <- case x0 of
                                             {Cons y25 y26 -> return (y25, y26); _ -> mzero};
                               guard (x25 == x22);
                               guard (x26 == x24);
                               return ()}]
appendoAppendoI x0 = Immature $ msum [do {_appendoI x0; return ()},
                           do {_appendoAppendoI x0; return ()}]
_appendoAppendoI x0 = Immature $ msum [do {__appendoI x0; return ()},
                            do {__appendoAppendoI x0; return ()}]
__appendoAppendoI x0 = Immature $ msum [do {___appendoI x0; return ()},
                             do {___appendoAppendoI x0; return ()}]
___appendoAppendoI x0 = Immature $ msum [do {____appendoI x0; return ()},
                              do {____appendoAppendoI x0; return ()}]
____appendoAppendoI x0 = Immature $ msum [do {_____appendoI x0; return ()},
                               do {_____appendoAppendoI x0; return ()}]
_____appendoAppendoI x0 = Immature $ msum [do {______appendoI x0; return ()},
                                do {______appendoAppendoI x0; return ()}]
______appendoAppendoI x0 = Immature $ msum [do {_______appendoI x0;
                                     return ()},
                                 do {________appendoI x0; return ()}]
________appendoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()}]
matchoO = Immature $ msum [do {x0 <- appendoO; return x0},
                do {x0 <- appendoAppendoO; return x0}]
appendoO = Immature $ msum [do {let {x0 = Nil}; return x0},
                 do {let {x4 = O};
                     let {x3 = S x4};
                     let {x2 = S x3};
                     let {x5 = x2};
                     x1 <- _appendoO;
                     let {x0 = Cons x5 x1};
                     return x0}]
_appendoO = Immature $ msum [do {let {x0 = Nil}; return x0},
                  do {let {x7 = O};
                      let {x6 = S x7};
                      let {x8 = x6};
                      x1 <- __appendoO;
                      let {x0 = Cons x8 x1};
                      return x0}]
__appendoO = Immature $ msum [do {let {x0 = Nil}; return x0},
                   do {let {x10 = O};
                       let {x9 = S x10};
                       let {x11 = x9};
                       x1 <- ___appendoO;
                       let {x0 = Cons x11 x1};
                       return x0}]
___appendoO = Immature $ msum [do {let {x0 = Nil}; return x0},
                    do {let {x12 = O};
                        let {x13 = x12};
                        x1 <- ____appendoO;
                        let {x0 = Cons x13 x1};
                        return x0}]
____appendoO = Immature $ msum [do {let {x0 = Nil}; return x0},
                     do {let {x14 = O};
                         let {x15 = x14};
                         x1 <- _____appendoO;
                         let {x0 = Cons x15 x1};
                         return x0}]
_____appendoO = Immature $ msum [do {let {x0 = Nil}; return x0},
                      do {let {x18 = O};
                          let {x17 = S x18};
                          let {x16 = S x17};
                          let {x19 = x16};
                          x1 <- ______appendoO;
                          let {x0 = Cons x19 x1};
                          return x0}]
______appendoO = Immature $ msum [do {let {x0 = Nil}; return x0},
                       do {let {x20 = O};
                           let {x21 = x20};
                           x1 <- _______appendoO;
                           let {x0 = Cons x21 x1};
                           return x0}]
_______appendoO = Immature $ msum [do {let {x0 = Nil}; return x0},
                        do {let {x23 = O};
                            let {x22 = S x23};
                            let {x24 = Nil};
                            let {x25 = x22};
                            let {x26 = x24};
                            let {x0 = Cons x25 x26};
                            return x0}]
appendoAppendoO = Immature $ msum [do {x0 <- _appendoO; return x0},
                        do {x0 <- _appendoAppendoO; return x0}]
_appendoAppendoO = Immature $ msum [do {x0 <- __appendoO; return x0},
                         do {x0 <- __appendoAppendoO; return x0}]
__appendoAppendoO = Immature $ msum [do {x0 <- ___appendoO; return x0},
                          do {x0 <- ___appendoAppendoO; return x0}]
___appendoAppendoO = Immature $ msum [do {x0 <- ____appendoO; return x0},
                           do {x0 <- ____appendoAppendoO; return x0}]
____appendoAppendoO = Immature $ msum [do {x0 <- _____appendoO; return x0},
                            do {x0 <- _____appendoAppendoO; return x0}]
_____appendoAppendoO = Immature $ msum [do {x0 <- ______appendoO; return x0},
                             do {x0 <- ______appendoAppendoO; return x0}]
______appendoAppendoO = Immature $ msum [do {x0 <- _______appendoO;
                                  return x0},
                              do {x0 <- ________appendoO; return x0}]
________appendoO = Immature $ msum [do {let {x0 = Nil}; return x0}]