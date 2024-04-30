module Sort_cpd_ans where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | Succ Term
    | Zero
    deriving (Show, Eq)
sortoI x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 = msum [do {(x1,
                                                                                                                                       x2) <- splitoSplitoSortoAppendoAppendoIOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                                                      x3 <- sorto1OI x2 gen_gtIO_x2;
                                                                                                                                      x4 <- sorto1OI x1 gen_gtIO_x2;
                                                                                                                                      return ()}]
sortoO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3 = msum [do {(x0,
                                                                                                                                                                        x1,
                                                                                                                                                                        x2) <- splitoSplitoSortoAppendoAppendoOOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3;
                                                                                                                                                                       x3 <- sorto1OI x2 gen_gtIO_x2;
                                                                                                                                                                       x4 <- sorto1OI x1 gen_gtIO_x2;
                                                                                                                                                                       return x0}]
sorto1OI x1 gen_gtIO_x2 = msum [do {guard (x1 == Nil);
                                    let {x0 = Nil};
                                    return x0},
                                do {(x2, x3, x4, x5, x6) <- splitoAppendoIOOOOO x1 gen_gtIO_x2;
                                    let {x0 = Cons x2 x3};
                                    sorto1II x4 x5 gen_gtIO_x2;
                                    x7 <- sorto1OI x6 gen_gtIO_x2;
                                    return x0}]
sorto1II x0 x1 gen_gtIO_x2 = msum [do {guard (x1 == Nil);
                                       guard (x0 == Nil);
                                       return ()},
                                   do {(x2, x3) <- case x0 of
                                                   {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                       (x4, x5, x6) <- splitoAppendoIIIOOO x1 x2 x3;
                                       sorto1II x4 x5 gen_gtIO_x2;
                                       x7 <- sorto1OI x6 gen_gtIO_x2;
                                       return ()}]
splitoAppendoIIIOOO x0 x1 x2 = msum [do {let {x3 = Nil};
                                         guard (x2 == Nil);
                                         (x4, x5) <- appendo1IIOO x0 x1;
                                         return (x3, x4, x5)},
                                     do {(x6, x8) <- case x2 of
                                                     {Cons y6 y8 -> return (y6, y8); _ -> mzero};
                                         leII x1 x6;
                                         (x7, x4, x5) <- splitoAppendoIIIOOO x0 x1 x8;
                                         let {x3 = Cons x6 x7};
                                         return (x3, x4, x5)},
                                     do {(x6, x8) <- case x2 of
                                                     {Cons y6 y8 -> return (y6, y8); _ -> mzero};
                                         gtII x1 x6;
                                         (x3, x4, x5) <- splitoAppendoIIIOOO x0 x1 x8;
                                         return (x3, x4, x5)}]
appendo1IIOO x0 x1 = msum [do {let {x2 = Nil};
                               x3 <- case x0 of
                                     {Cons y1 y3 -> do {guard (x1 == y1); return y3}; _ -> mzero};
                               return (x2, x3)},
                           do {(x4, x6) <- case x0 of
                                           {Cons y4 y6 -> return (y4, y6); _ -> mzero};
                               (x5, x3) <- appendo1IIOO x6 x1;
                               let {x2 = Cons x4 x5};
                               return (x2, x3)}]
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
                                               (x1, x4, x5) <- appendo1IOOO x0;
                                               return (x1, x2, x3, x4, x5)},
                                           do {(x1,
                                                x8,
                                                x7,
                                                x4,
                                                x5) <- splitoAppendoIOOOOO x0 gen_gtIO_x2;
                                               x6 <- leIO x1;
                                               let {x3 = Cons x6 x7};
                                               let {x2 = Cons x6 x8};
                                               return (x1, x2, x3, x4, x5)},
                                           do {(x1,
                                                x8,
                                                x3,
                                                x4,
                                                x5) <- splitoAppendoIOOOOO x0 gen_gtIO_x2;
                                               x6 <- gtIO x1 gen_gtIO_x2;
                                               let {x2 = Cons x6 x8};
                                               return (x1, x2, x3, x4, x5)}]
appendo1IOOO x0 = msum [do {let {x2 = Nil};
                            (x1, x3) <- case x0 of
                                        {Cons y1 y3 -> return (y1, y3); _ -> mzero};
                            return (x1, x2, x3)},
                        do {(x4, x6) <- case x0 of
                                        {Cons y4 y6 -> return (y4, y6); _ -> mzero};
                            (x1, x5, x3) <- appendo1IOOO x6;
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
splitoSplitoSortoAppendoAppendoIOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 = msum [do {(x1,
                                                                                                                                                                   x2) <- splitoSplitoSortoAppendoAppendo1IOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                                                                                  return (x1,
                                                                                                                                                                          x2)}]
splitoSplitoSortoAppendoAppendoOOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3 = msum [do {(x0,
                                                                                                                                                                                                    x1,
                                                                                                                                                                                                    x2) <- splitoSplitoSortoAppendoAppendo1OOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3;
                                                                                                                                                                                                   return (x0,
                                                                                                                                                                                                           x1,
                                                                                                                                                                                                           x2)}]
splitoSplitoSortoAppendoAppendo1IOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 = msum [do {(x1,
                                                                                                                                                                    x2) <- splitoSplitoSortoAppendoAppendo2IOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                                                                                   return (x1,
                                                                                                                                                                           x2)}]
splitoSplitoSortoAppendoAppendo1OOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3 = msum [do {(x0,
                                                                                                                                                                                                     x1,
                                                                                                                                                                                                     x2) <- splitoSplitoSortoAppendoAppendo2OOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3;
                                                                                                                                                                                                    return (x0,
                                                                                                                                                                                                            x1,
                                                                                                                                                                                                            x2)}]
splitoSplitoSortoAppendoAppendo2IOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 = msum [do {(x1,
                                                                                                                                                                    x2) <- splitoSortoAppendoAppendoIOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                                                                                   return (x1,
                                                                                                                                                                           x2)}]
splitoSortoAppendoAppendoIOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 = msum [do {(x1,
                                                                                                                                                             x2) <- splitoSortoAppendoAppendo1IOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                                                                            return (x1,
                                                                                                                                                                    x2)}]
splitoSortoAppendoAppendo1IOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 = msum [do {splitoSortoAppendoAppendo10I x0;
                                                                                                                                                             (x1,
                                                                                                                                                              x2,
                                                                                                                                                              x3) <- sortoAppendoAppendoOOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                                                                             return (x1,
                                                                                                                                                                     x2)}]
sortoAppendoAppendoOOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 = msum [do {let {x8 = Zero};
                                                                                                                                                   let {x10 = Zero};
                                                                                                                                                   let {x9 = Succ x10};
                                                                                                                                                   (x3,
                                                                                                                                                    x1,
                                                                                                                                                    x0) <- appendo1OIOO x9 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4;
                                                                                                                                                   let {x2 = Cons x8 x3};
                                                                                                                                                   return (x0,
                                                                                                                                                           x1,
                                                                                                                                                           x2)},
                                                                                                                                               do {(x4,
                                                                                                                                                    x5,
                                                                                                                                                    x6,
                                                                                                                                                    x7,
                                                                                                                                                    x8,
                                                                                                                                                    x9) <- splitoAppendoOOOOOO gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                                                                   sorto1II x7 x8 gen_gtIO_x2;
                                                                                                                                                   x10 <- sorto1OI x9 gen_gtIO_x2;
                                                                                                                                                   (x0,
                                                                                                                                                    x1,
                                                                                                                                                    x2) <- appendoAppendoOOIO x4 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4;
                                                                                                                                                   return (x0,
                                                                                                                                                           x1,
                                                                                                                                                           x2)}]
appendo1OIOO x1 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 = msum [do {let {x2 = Nil};
                                                                    (x0,
                                                                     x3) <- do {x3 <- gen_appendo1OIOO_x3;
                                                                                let {x0 = Cons x1 x3};
                                                                                return (x0, x3)};
                                                                    return (x0, x2, x3)},
                                                                do {(x6,
                                                                     x5,
                                                                     x3) <- appendo1OIOO x1 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4;
                                                                    (x2,
                                                                     x4) <- do {x4 <- gen_appendo1OIOO_x4;
                                                                                let {x2 = Cons x4 x5};
                                                                                return (x2, x4)};
                                                                    let {x0 = Cons x4 x6};
                                                                    return (x0, x2, x3)}]
appendoAppendoOOIO x2 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 = msum [do {guard (x2 == Nil);
                                                                          let {x12 = Zero};
                                                                          let {x11 = Succ x12};
                                                                          let {x14 = Zero};
                                                                          (x3,
                                                                           x13,
                                                                           x0) <- appendo1OIOO x11 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4;
                                                                          x1 <- case x13 of
                                                                                {Cons y14
                                                                                      y1 -> do {guard (x14 == y14);
                                                                                                return y1};
                                                                                 _ -> mzero};
                                                                          return (x0, x1, x3)},
                                                                      do {(x4, x6) <- case x2 of
                                                                                      {Cons y4
                                                                                            y6 -> return (y4,
                                                                                                          y6);
                                                                                       _ -> mzero};
                                                                          (x0,
                                                                           x1,
                                                                           x5) <- appendoAppendoOOIO x6 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4;
                                                                          let {x3 = Cons x4 x5};
                                                                          return (x0, x1, x3)}]
splitoAppendoOOOOOO gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 = msum [do {let {x3 = Nil};
                                                                                                        let {x2 = Nil};
                                                                                                        (x0,
                                                                                                         x1,
                                                                                                         x4,
                                                                                                         x5) <- appendo1OOOO gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4;
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4,
                                                                                                                x5)},
                                                                                                    do {(x0,
                                                                                                         x1,
                                                                                                         x8,
                                                                                                         x7,
                                                                                                         x4,
                                                                                                         x5) <- splitoAppendoOOOOOO gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                        x6 <- leIO x1;
                                                                                                        let {x3 = Cons x6 x7};
                                                                                                        let {x2 = Cons x6 x8};
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4,
                                                                                                                x5)},
                                                                                                    do {(x0,
                                                                                                         x1,
                                                                                                         x8,
                                                                                                         x3,
                                                                                                         x4,
                                                                                                         x5) <- splitoAppendoOOOOOO gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                        x6 <- gtIO x1 gen_gtIO_x2;
                                                                                                        let {x2 = Cons x6 x8};
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x2,
                                                                                                                x3,
                                                                                                                x4,
                                                                                                                x5)}]
appendo1OOOO gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 = msum [do {let {x2 = Nil};
                                                                                     (x0,
                                                                                      x1,
                                                                                      x3) <- do {x1 <- gen_appendo1OOOO_x1;
                                                                                                 x3 <- gen_appendo1OOOO_x3;
                                                                                                 let {x0 = Cons x1 x3};
                                                                                                 return (x0,
                                                                                                         x1,
                                                                                                         x3)};
                                                                                     return (x0,
                                                                                             x1,
                                                                                             x2,
                                                                                             x3)},
                                                                                 do {(x6,
                                                                                      x1,
                                                                                      x5,
                                                                                      x3) <- appendo1OOOO gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4;
                                                                                     (x2,
                                                                                      x4) <- do {x4 <- gen_appendo1OOOO_x4;
                                                                                                 let {x2 = Cons x4 x5};
                                                                                                 return (x2,
                                                                                                         x4)};
                                                                                     let {x0 = Cons x4 x6};
                                                                                     return (x0,
                                                                                             x1,
                                                                                             x2,
                                                                                             x3)}]
splitoSortoAppendoAppendo10I x0 = msum [do {(x7, x3) <- case x0 of
                                                        {Cons y7 y3 -> return (y7, y3); _ -> mzero};
                                            guard (x7 == Zero);
                                            return ()}]
splitoSplitoSortoAppendoAppendo2OOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3 = msum [do {(x0,
                                                                                                                                                                                                     x1,
                                                                                                                                                                                                     x2) <- splitoSortoAppendoAppendoOOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3;
                                                                                                                                                                                                    return (x0,
                                                                                                                                                                                                            x1,
                                                                                                                                                                                                            x2)}]
splitoSortoAppendoAppendoOOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3 = msum [do {(x0,
                                                                                                                                                                                              x1,
                                                                                                                                                                                              x2) <- splitoSortoAppendoAppendo1OOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3;
                                                                                                                                                                                             return (x0,
                                                                                                                                                                                                     x1,
                                                                                                                                                                                                     x2)}]
splitoSortoAppendoAppendo1OOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3 = msum [do {x0 <- splitoSortoAppendoAppendo10O gen_splitoSortoAppendoAppendo10O_x3;
                                                                                                                                                                                              (x1,
                                                                                                                                                                                               x2,
                                                                                                                                                                                               x3) <- sortoAppendoAppendoOOO gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                                                                                                              return (x0,
                                                                                                                                                                                                      x1,
                                                                                                                                                                                                      x2)}]
splitoSortoAppendoAppendo10O gen_splitoSortoAppendoAppendo10O_x3 = msum [do {let {x7 = Zero};
                                                                             (x0,
                                                                              x3) <- do {x3 <- gen_splitoSortoAppendoAppendo10O_x3;
                                                                                         let {x0 = Cons x7 x3};
                                                                                         return (x0,
                                                                                                 x3)};
                                                                             return x0}]