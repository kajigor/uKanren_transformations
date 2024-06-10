{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module Simple where 

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)

data Term
    = Succ Term
    | Zero
    | Nil 
    | Cons Term Term 
    | Trueo 
    | Falso
    deriving (Show, Eq, Generic, DS.NFData)
    
maxMinoIII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m ()
maxMinoIII x0 x1 x2 gen_gtoIOI_x1 gen_leoIOI_x1 = msum [do {maxoII x0 x1 gen_gtoIOI_x1 gen_leoIOI_x1;
                                                            minoII x0 x2;
                                                            return ()}]
maxMinoIIO x0 x1 gen_gtoIOI_x1 gen_leoIOI_x1 = msum [do {maxoII x0 x1 gen_gtoIOI_x1 gen_leoIOI_x1;
                                                         x2 <- minoIO x0;
                                                         return x2}]
maxMinoIOI x0 x2 = msum [do {x1 <- maxoIO x0;
                             minoII x0 x2;
                             return x1}]
maxMinoIOO x0 = msum [do {x1 <- maxoIO x0;
                          x2 <- minoIO x0;
                          return (x1, x2)}]
maxMinoOII x1 x2 gen_gtoOII_x3 gen_leoOII_x3 = msum [do {x0 <- maxoOI x1 gen_gtoOII_x3 gen_leoOII_x3;
                                                         minoII x0 x2;
                                                         return x0}]
maxMinoOIO x1 gen_gtoOII_x3 gen_leoOII_x3 = msum [do {x0 <- maxoOI x1 gen_gtoOII_x3 gen_leoOII_x3;
                                                      x2 <- minoIO x0;
                                                      return (x0, x2)}]
maxMinoOOI x2 gen_gtoOII_x3 gen_gtoOOI_x1 gen_gtoOOI_x3 gen_leoOII_x3 gen_leoOOI_x1 gen_leoOOI_x3 = msum [do {x0 <- minoOI x2 gen_gtoOII_x3 gen_gtoOOI_x1 gen_gtoOOI_x3 gen_leoOII_x3 gen_leoOOI_x1 gen_leoOOI_x3;
                                                                                                              x1 <- maxoIO x0;
                                                                                                              return (x0,
                                                                                                                      x1)}]
maxMinoOOO gen_gtoOII_x3 gen_leoOII_x3 = msum [do {(x0,
                                                    x1) <- maxoOO gen_gtoOII_x3 gen_leoOII_x3;
                                                   x2 <- minoIO x0;
                                                   return (x0, x1, x2)}]
maxoII x0 x1 gen_gtoIOI_x1 gen_leoIOI_x1 = msum [do {x4 <- maxo1IOI x0 x1 gen_gtoIOI_x1 gen_leoIOI_x1;
                                                     guard (x4 == Zero);
                                                     return ()}]
maxoIO x0 = msum [do {let {x4 = Zero};
                      x1 <- maxo1IIO x0 x4;
                      return x1}]
maxoOI x1 gen_gtoOII_x3 gen_leoOII_x3 = msum [do {let {x4 = Zero};
                                                  x0 <- maxo1OII x4 x1 gen_gtoOII_x3 gen_leoOII_x3;
                                                  return x0}]
maxoOO gen_gtoOII_x3 gen_leoOII_x3 = msum [do {let {x4 = Zero};
                                               (x0, x1) <- maxo1OIO x4 gen_gtoOII_x3 gen_leoOII_x3;
                                               return (x0, x1)}]
maxo1IIO x0 x1 = msum [do {guard (x0 == Nil);
                           let {x2 = x1};
                           return x2},
                       do {(x3, x4) <- case x0 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           x5 <- leoIIO x3 x1;
                           guard (x5 == Trueo);
                           x2 <- maxo1IIO x4 x1;
                           return x2},
                       do {(x3, x4) <- case x0 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           x6 <- gtoIIO x3 x1;
                           guard (x6 == Trueo);
                           x2 <- maxo1IIO x4 x3;
                           return x2}]
gtoIIO x0 x1 = msum [do {guard (x0 == Zero);
                         let {x2 = Falso};
                         return x2},
                     do {x3 <- case x0 of
                               {Succ y3 -> return y3; _ -> mzero};
                         guard (x1 == Zero);
                         let {x2 = Trueo};
                         return x2},
                     do {x3 <- case x0 of
                               {Succ y3 -> return y3; _ -> mzero};
                         x4 <- case x1 of
                               {Succ y4 -> return y4; _ -> mzero};
                         x2 <- gtoIIO x3 x4;
                         return x2}]
leoIIO x0 x1 = msum [do {guard (x0 == Zero);
                         let {x2 = Trueo};
                         return x2},
                     do {x3 <- case x0 of
                               {Succ y3 -> return y3; _ -> mzero};
                         guard (x1 == Zero);
                         let {x2 = Falso};
                         return x2},
                     do {x3 <- case x0 of
                               {Succ y3 -> return y3; _ -> mzero};
                         x4 <- case x1 of
                               {Succ y4 -> return y4; _ -> mzero};
                         x2 <- leoIIO x3 x4;
                         return x2}]
maxo1IOI x0 x2 gen_gtoIOI_x1 gen_leoIOI_x1 = msum [do {guard (x0 == Nil);
                                                       let {x1 = x2};
                                                       return x1},
                                                   do {(x3, x4) <- case x0 of
                                                                   {Cons y3 y4 -> return (y3, y4);
                                                                    _ -> mzero};
                                                       let {x5 = Trueo};
                                                       x1 <- leoIOI x3 x5 gen_leoIOI_x1;
                                                       maxo1III x4 x1 x2;
                                                       return x1},
                                                   do {(x3, x4) <- case x0 of
                                                                   {Cons y3 y4 -> return (y3, y4);
                                                                    _ -> mzero};
                                                       let {x6 = Trueo};
                                                       x1 <- gtoIOI x3 x6 gen_gtoIOI_x1;
                                                       maxo1III x4 x3 x2;
                                                       return x1}]
gtoIOI x0 x2 gen_gtoIOI_x1 = msum [do {guard (x0 == Zero);
                                       guard (x2 == Falso);
                                       x1 <- gen_gtoIOI_x1;
                                       return x1},
                                   do {x3 <- case x0 of
                                             {Succ y3 -> return y3; _ -> mzero};
                                       let {x1 = Zero};
                                       guard (x2 == Trueo);
                                       return x1},
                                   do {x3 <- case x0 of
                                             {Succ y3 -> return y3; _ -> mzero};
                                       x4 <- gtoIOI x3 x2 gen_gtoIOI_x1;
                                       let {x1 = Succ x4};
                                       return x1}]
leoIOI x0 x2 gen_leoIOI_x1 = msum [do {guard (x0 == Zero);
                                       guard (x2 == Trueo);
                                       x1 <- gen_leoIOI_x1;
                                       return x1},
                                   do {x3 <- case x0 of
                                             {Succ y3 -> return y3; _ -> mzero};
                                       let {x1 = Zero};
                                       guard (x2 == Falso);
                                       return x1},
                                   do {x3 <- case x0 of
                                             {Succ y3 -> return y3; _ -> mzero};
                                       x4 <- leoIOI x3 x2 gen_leoIOI_x1;
                                       let {x1 = Succ x4};
                                       return x1}]
maxo1III x0 x1 x2 = msum [do {guard (x0 == Nil);
                              guard (x2 == x1);
                              return ()},
                          do {(x3, x4) <- case x0 of
                                          {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                              x5 <- leoIIO x3 x1;
                              guard (x5 == Trueo);
                              maxo1III x4 x1 x2;
                              return ()},
                          do {(x3, x4) <- case x0 of
                                          {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                              x6 <- gtoIIO x3 x1;
                              guard (x6 == Trueo);
                              maxo1III x4 x3 x2;
                              return ()}]
maxo1OII x1 x2 gen_gtoOII_x3 gen_leoOII_x3 = msum [do {let {x0 = Nil};
                                                       guard (x2 == x1);
                                                       return x0},
                                                   do {let {x5 = Trueo};
                                                       x3 <- leoOII x1 x5 gen_leoOII_x3;
                                                       x4 <- maxo1OII x1 x2 gen_gtoOII_x3 gen_leoOII_x3;
                                                       let {x0 = Cons x3 x4};
                                                       return x0},
                                                   do {let {x6 = Trueo};
                                                       x3 <- gtoOII x1 x6 gen_gtoOII_x3;
                                                       x4 <- maxo1OII x3 x2 gen_gtoOII_x3 gen_leoOII_x3;
                                                       let {x0 = Cons x3 x4};
                                                       return x0}]
gtoOII x1 x2 gen_gtoOII_x3 = msum [do {let {x0 = Zero};
                                       guard (x2 == Falso);
                                       return x0},
                                   do {guard (x1 == Zero);
                                       guard (x2 == Trueo);
                                       (x0, x3) <- do {x3 <- gen_gtoOII_x3;
                                                       let {x0 = Succ x3};
                                                       return (x0, x3)};
                                       return x0},
                                   do {x4 <- case x1 of
                                             {Succ y4 -> return y4; _ -> mzero};
                                       x3 <- gtoOII x4 x2 gen_gtoOII_x3;
                                       let {x0 = Succ x3};
                                       return x0}]
leoOII x1 x2 gen_leoOII_x3 = msum [do {let {x0 = Zero};
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
maxo1OIO x1 gen_gtoOII_x3 gen_leoOII_x3 = msum [do {let {x0 = Nil};
                                                    let {x2 = x1};
                                                    return (x0, x2)},
                                                do {let {x5 = Trueo};
                                                    x3 <- leoOII x1 x5 gen_leoOII_x3;
                                                    (x4,
                                                     x2) <- maxo1OIO x1 gen_gtoOII_x3 gen_leoOII_x3;
                                                    let {x0 = Cons x3 x4};
                                                    return (x0, x2)},
                                                do {let {x6 = Trueo};
                                                    x3 <- gtoOII x1 x6 gen_gtoOII_x3;
                                                    (x4,
                                                     x2) <- maxo1OIO x3 gen_gtoOII_x3 gen_leoOII_x3;
                                                    let {x0 = Cons x3 x4};
                                                    return (x0, x2)}]
minoII x0 x1 = msum [do {guard (x0 == Nil);
                         guard (x1 == Zero);
                         return ()},
                     do {(x2, x3) <- case x0 of
                                     {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                         mino1III x3 x2 x1;
                         return ()}]
minoIO x0 = msum [do {guard (x0 == Nil);
                      let {x1 = Zero};
                      return x1},
                  do {(x2, x3) <- case x0 of
                                  {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                      x1 <- mino1IIO x3 x2;
                      return x1}]
minoOI x1 gen_gtoOII_x3 gen_gtoOOI_x1 gen_gtoOOI_x3 gen_leoOII_x3 gen_leoOOI_x1 gen_leoOOI_x3 = msum [do {let {x0 = Nil};
                                                                                                          guard (x1 == Zero);
                                                                                                          return x0},
                                                                                                      do {(x3,
                                                                                                           x2) <- mino1OOI x1 gen_gtoOII_x3 gen_gtoOOI_x1 gen_gtoOOI_x3 gen_leoOII_x3 gen_leoOOI_x1 gen_leoOOI_x3;
                                                                                                          let {x0 = Cons x2 x3};
                                                                                                          return x0}]
mino1III x0 x1 x2 = msum [do {guard (x0 == Nil);
                              guard (x2 == x1);
                              return ()},
                          do {(x3, x4) <- case x0 of
                                          {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                              x7 <- leoIIO x3 x1;
                              guard (x7 == Trueo);
                              mino1III x4 x3 x2;
                              return ()},
                          do {(x3, x4) <- case x0 of
                                          {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                              x8 <- gtoIIO x3 x1;
                              guard (x8 == Trueo);
                              mino1III x4 x1 x2;
                              return ()}]
mino1IIO x0 x1 = msum [do {guard (x0 == Nil);
                           let {x2 = x1};
                           return x2},
                       do {(x3, x4) <- case x0 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           x7 <- leoIIO x3 x1;
                           guard (x7 == Trueo);
                           x2 <- mino1IIO x4 x3;
                           return x2},
                       do {(x3, x4) <- case x0 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           x8 <- gtoIIO x3 x1;
                           guard (x8 == Trueo);
                           x2 <- mino1IIO x4 x1;
                           return x2}]
mino1OOI x2 gen_gtoOII_x3 gen_gtoOOI_x1 gen_gtoOOI_x3 gen_leoOII_x3 gen_leoOOI_x1 gen_leoOOI_x3 = msum [do {let {x0 = Nil};
                                                                                                            let {x1 = x2};
                                                                                                            return (x0,
                                                                                                                    x1)},
                                                                                                        do {let {x7 = Trueo};
                                                                                                            (x3,
                                                                                                             x1) <- leoOOI x7 gen_leoOOI_x1 gen_leoOOI_x3;
                                                                                                            x4 <- mino1OII x3 x2 gen_gtoOII_x3 gen_leoOII_x3;
                                                                                                            let {x0 = Cons x3 x4};
                                                                                                            return (x0,
                                                                                                                    x1)},
                                                                                                        do {let {x8 = Trueo};
                                                                                                            (x3,
                                                                                                             x1) <- gtoOOI x8 gen_gtoOOI_x1 gen_gtoOOI_x3;
                                                                                                            x4 <- mino1OII x1 x2 gen_gtoOII_x3 gen_leoOII_x3;
                                                                                                            let {x0 = Cons x3 x4};
                                                                                                            return (x0,
                                                                                                                    x1)}]
gtoOOI x2 gen_gtoOOI_x1 gen_gtoOOI_x3 = msum [do {let {x0 = Zero};
                                                  guard (x2 == Falso);
                                                  x1 <- gen_gtoOOI_x1;
                                                  return (x0, x1)},
                                              do {let {x1 = Zero};
                                                  guard (x2 == Trueo);
                                                  (x0, x3) <- do {x3 <- gen_gtoOOI_x3;
                                                                  let {x0 = Succ x3};
                                                                  return (x0, x3)};
                                                  return (x0, x1)},
                                              do {(x3, x4) <- gtoOOI x2 gen_gtoOOI_x1 gen_gtoOOI_x3;
                                                  let {x0 = Succ x3};
                                                  let {x1 = Succ x4};
                                                  return (x0, x1)}]
leoOOI x2 gen_leoOOI_x1 gen_leoOOI_x3 = msum [do {let {x0 = Zero};
                                                  guard (x2 == Trueo);
                                                  x1 <- gen_leoOOI_x1;
                                                  return (x0, x1)},
                                              do {let {x1 = Zero};
                                                  guard (x2 == Falso);
                                                  (x0, x3) <- do {x3 <- gen_leoOOI_x3;
                                                                  let {x0 = Succ x3};
                                                                  return (x0, x3)};
                                                  return (x0, x1)},
                                              do {(x3, x4) <- leoOOI x2 gen_leoOOI_x1 gen_leoOOI_x3;
                                                  let {x0 = Succ x3};
                                                  let {x1 = Succ x4};
                                                  return (x0, x1)}]
mino1OII x1 x2 gen_gtoOII_x3 gen_leoOII_x3 = msum [do {let {x0 = Nil};
                                                       guard (x2 == x1);
                                                       return x0},
                                                   do {let {x7 = Trueo};
                                                       x3 <- leoOII x1 x7 gen_leoOII_x3;
                                                       x4 <- mino1OII x3 x2 gen_gtoOII_x3 gen_leoOII_x3;
                                                       let {x0 = Cons x3 x4};
                                                       return x0},
                                                   do {let {x8 = Trueo};
                                                       x3 <- gtoOII x1 x8 gen_gtoOII_x3;
                                                       x4 <- mino1OII x1 x2 gen_gtoOII_x3 gen_leoOII_x3;
                                                       let {x0 = Cons x3 x4};
                                                       return x0}]