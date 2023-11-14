import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Simple (Term (..), sortoII, sortoIO)
import qualified Branching 
import qualified Det_unfold

sortoIOffline x0 = msum [do {(x1, x2) <- case x0 of
                                  {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                      guard (x1 == Zero);
                      (x3, x4) <- case x2 of
                                  {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                      guard (x3 == Zero);
                      (x5, x7) <- case x4 of
                                  {Cons y5 y7 -> return (y5, y7); _ -> mzero};
                      x6 <- case x5 of
                            {Succ y6 -> return y6; _ -> mzero};
                      guard (x6 == Zero);
                      (x8, x10) <- case x7 of
                                   {Cons y8 y10 -> return (y8, y10); _ -> mzero};
                      x9 <- case x8 of
                            {Succ y9 -> return y9; _ -> mzero};
                      guard (x9 == Zero);
                      (x11, x14) <- case x10 of
                                    {Cons y11 y14 -> return (y11, y14); _ -> mzero};
                      x12 <- case x11 of
                             {Succ y12 -> return y12; _ -> mzero};
                      x13 <- case x12 of
                             {Succ y13 -> return y13; _ -> mzero};
                      guard (x13 == Zero);
                      guard (x14 == Nil);
                      return ()}]

sortoOOffline :: MonadPlus m => m Term
sortoOOffline = msum [do {let {x1 = Zero};
                   let {x3 = Zero};
                   let {x6 = Zero};
                   let {x5 = Succ x6};
                   let {x9 = Zero};
                   let {x8 = Succ x9};
                   let {x13 = Zero};
                   let {x12 = Succ x13};
                   let {x11 = Succ x12};
                   let {x14 = Nil};
                   let {x10 = Cons x11 x14};
                   let {x7 = Cons x8 x10};
                   let {x4 = Cons x5 x7};
                   let {x2 = Cons x3 x4};
                   let {x0 = Cons x1 x2};
                   return x0}]

sortoIOnline x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 = msum [do {(x1,
                                                                                                                                       x2) <- splitoSplitoSortoAppendoAppendoIOO x0 gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2;
                                                                                                                                      x3 <- sorto1OI x2 gen_gtIO_x2;
                                                                                                                                      x4 <- sorto1OI x1 gen_gtIO_x2;
                                                                                                                                      return ()}]

sortoOOnline gen_appendo1OIOO_x3 gen_appendo1OIOO_x4 gen_appendo1OOOO_x1 gen_appendo1OOOO_x3 gen_appendo1OOOO_x4 gen_gtIO_x2 gen_splitoSortoAppendoAppendo10O_x3 = msum [do {(x0,
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

eval :: (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval2 :: (m r -> [r]) -> (a -> b -> m r) -> (a, b) -> [r]
eval2 listify f = eval listify $ \(x,y) -> f x y

eval5 :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4-> m r) -> (a, x1, x2, x3, x4) -> [r]
eval5 listify f = eval listify  $ \(b, y1, y2, y3, y4) -> f b y1 y2 y3 y4

eval6 :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> m r) -> (a, x1, x2, x3, x4, x5) -> [r]
eval6 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5) -> f b y1 y2 y3 y4 y5

eval7 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> m r) -> (x1, x2, x3, x4, x5, x6, x7) -> [r]
eval7 listify f = eval listify  $ \(y1, y2, y3, y4, y5, y6, y7) -> f y1 y2 y3 y4 y5 y6 y7

natGen :: (MonadPlus m) => m Term
natGen = return Zero <|> (Succ <$> natGen)

--[Zero, Zero, Succ Zero, Succ Zero, Succ (Succ Zero)]
rightSort :: Term
rightSort = Cons Zero (Cons Zero (Cons (Succ Zero) (Cons (Succ Zero) (Cons (Succ (Succ Zero)) Nil))))

rightGen :: (Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term)
rightGen = (rightSort, natGen, natGen, natGen, natGen, natGen, natGen)

genSort :: (Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term)
genSort = (natGen, natGen, natGen, natGen, natGen, natGen, natGen)

sortoOOffline1 :: MonadPlus m => p -> m Term
sortoOOffline1 x = do
  sortoOOffline
  
  
sortoODetUnf :: MonadPlus m => p -> m Term
sortoODetUnf x = do
  Det_unfold.sortoO
  
-- [Succ Zero, Zero, Succ (Succ Zero), Succ Zero, Zero] 
input = Cons (Succ Zero) (Cons Zero (Cons (Succ (Succ Zero)) (Cons (Succ Zero) (Cons Zero Nil))))

main = defaultMain
  [
    bgroup "SortRun"
     [
        bench "offline1"    $ nf (eval (takeS 1) sortoIOffline) rightSort
      , bench "branching1"  $ nf (eval2 (takeS 1) Branching.sortoI) (rightSort, natGen)
      , bench "detUnfold1" $ nf (eval (takeS 1) Det_unfold.sortoI) rightSort
--      , bench "online1"     $ nf (eval7 (takeS 1) sortoIOnline) $ traceShow (eval6N (takeS 1) sortoIOnline rightGen) rightGen -- failing
--        , bench "simple1"   $ nf (eval6 (takeS 1) sortoII) (input, rightSort, natGen, natGen, natGen, natGen) -- failing
     ],
    bgroup "SortGen"
    [
        bench "offlineGen"  $ nf (eval (takeS 1) sortoOOffline1) ()
      , bench "detUnfoldGen" $ nf (eval (takeS 1) sortoODetUnf) ()
--      , bench "branchingGen"  $ nf (eval2 (takeS 1) Branching.sortoO) (natGen, natGen)
   --   , bench "onlineGen"   $ nf (eval7 (takeS 1) sortoOOnline) genSort -- failing
--       , bench "simpleGen"  $ nf (eval5 (takeS 1) sortoIO) (input, natGen, natGen, natGen, natGen)
    ]
  ]
  
  
