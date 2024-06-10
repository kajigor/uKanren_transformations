{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Simple where 
  
import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)
import Term
    
    
sortoII x0 x1 = msum [do {guard (x0 == Nil);
                          guard (x1 == Nil);
                          return ()},
                      do {(x2, x3) <- case x0 of
                                      {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                          (x4, x5) <- splitoIIOO x2 x3;
                          x6 <- sortoIO x4;
                          x7 <- sortoIO x5;
                          let {x11 = Cons x2 x7};
                          appendoIII x6 x11 x1;
                          return ()}]
appendoIII x0 x1 x2 = msum [do {guard (x1 == x2);
                                guard (x0 == Nil);
                                return ()},
                            do {(x3, x4) <- case x0 of
                                            {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                (x8, x5) <- case x2 of
                                            {Cons y8 y5 -> return (y8, y5); _ -> mzero};
                                guard (x8 == x3);
                                appendoIII x4 x1 x5;
                                return ()}]
sortoIO x0 = msum [do {let {x1 = Nil};
                       guard (x0 == Nil);
                       return x1},
                   do {(x2, x3) <- case x0 of
                                   {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                       (x4, x5) <- splitoIIOO x2 x3;
                       x6 <- sortoIO x4;
                       x7 <- sortoIO x5;
                       let {x11 = Cons x2 x7};
                       x1 <- appendoIIO x6 x11;
                       return x1}]
appendoIIO x0 x1 = msum [do {guard (x0 == Nil);
                             let {x2 = x1};
                             return x2},
                         do {(x3, x4) <- case x0 of
                                         {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                             let {x8 = x3};
                             x5 <- appendoIIO x4 x1;
                             let {x2 = Cons x8 x5};
                             return x2}]
sortoOI x1 = msum [do {let {x0 = Nil};
                       guard (x1 == Nil);
                       return x0},
                   do {(x6, x11) <- appendoOOI x1;
                       (x2, x7) <- case x11 of
                                   {Cons y2 y7 -> return (y2, y7); _ -> mzero};
                       x4 <- sortoOI x6;
                       x5 <- sortoOI x7;
                       x3 <- splitoIOII x2 x4 x5;
                       let {x0 = Cons x2 x3};
                       return x0}]
appendoOOI x2 = msum [do {let {x0 = Nil};
                          let {x1 = x2};
                          return (x0, x1)},
                      do {(x8, x5) <- case x2 of
                                      {Cons y8 y5 -> return (y8, y5); _ -> mzero};
                          let {x3 = x8};
                          (x4, x1) <- appendoOOI x5;
                          let {x0 = Cons x3 x4};
                          return (x0, x1)}]
sortoOO gen_leIO_x1 gen_splitoOOII_x0 = msum [do {let {x0 = Nil};
                                                  let {x1 = Nil};
                                                  return (x0, x1)},
                                              do {(x4, x6) <- sortoOO gen_leIO_x1 gen_splitoOOII_x0;
                                                  (x5, x7) <- sortoOO gen_leIO_x1 gen_splitoOOII_x0;
                                                  (x2,
                                                   x3) <- splitoOOII x4 x5 gen_leIO_x1 gen_splitoOOII_x0;
                                                  let {x0 = Cons x2 x3};
                                                  let {x11 = Cons x2 x7};
                                                  x1 <- appendoIIO x6 x11;
                                                  return (x0, x1)}]
splitoIIOO x0 x1 = msum [do {let {x2 = Nil};
                             let {x3 = Nil};
                             guard (x1 == Nil);
                             return (x2, x3)},
                         do {(x4, x5) <- case x1 of
                                         {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                             (x2, x3) <- splito0IOOII x0 x4 x5;
                             return (x2, x3)}]
splitoIOII x0 x2 x3 = msum [do {let {x1 = Nil};
                                guard (x2 == Nil);
                                guard (x3 == Nil);
                                return x1},
                            do {(x4, x5) <- splito0IIIOO x0 x2 x3;
                                let {x1 = Cons x4 x5};
                                return x1}]
splitoOOII x2 x3 gen_leIO_x1 gen_splitoOOII_x0 = msum [do {let {x1 = Nil};
                                                           guard (x2 == Nil);
                                                           guard (x3 == Nil);
                                                           x0 <- gen_splitoOOII_x0;
                                                           return (x0, x1)},
                                                       do {(x0,
                                                            x4,
                                                            x5) <- splito0OIIOO x2 x3 gen_leIO_x1;
                                                           let {x1 = Cons x4 x5};
                                                           return (x0, x1)}]
splito0IIIOO x0 x2 x3 = msum [do {(x9, x6) <- case x2 of
                                              {Cons y9 y6 -> return (y9, y6); _ -> mzero};
                                  let {x4 = x9};
                                  leII x4 x0;
                                  x5 <- splitoIOII x0 x6 x3;
                                  return (x4, x5)},
                              do {(x10, x6) <- case x3 of
                                               {Cons y10 y6 -> return (y10, y6); _ -> mzero};
                                  let {x4 = x10};
                                  gtII x4 x0;
                                  x5 <- splitoIOII x0 x2 x6;
                                  return (x4, x5)}]
gtII x0 x1 = msum [do {x2 <- case x0 of
                             {Succ y2 -> return y2; _ -> mzero};
                       guard (x1 == Zero);
                       return ()},
                   do {x2 <- case x0 of
                             {Succ y2 -> return y2; _ -> mzero};
                       x3 <- case x1 of
                             {Succ y3 -> return y3; _ -> mzero};
                       gtII x2 x3;
                       return ()}]
leII x0 x1 = msum [do {guard (x0 == Zero); return ()},
                   do {x2 <- case x0 of
                             {Succ y2 -> return y2; _ -> mzero};
                       x3 <- case x1 of
                             {Succ y3 -> return y3; _ -> mzero};
                       leII x2 x3;
                       return ()}]
splito0IOOII x0 x4 x5 = msum [do {leII x4 x0;
                                  let {x9 = x4};
                                  (x6, x3) <- splitoIIOO x0 x5;
                                  let {x2 = Cons x9 x6};
                                  return (x2, x3)},
                              do {gtII x4 x0;
                                  let {x10 = x4};
                                  (x2, x6) <- splitoIIOO x0 x5;
                                  let {x3 = Cons x10 x6};
                                  return (x2, x3)}]
splito0OIIOO x2 x3 gen_leIO_x1 = msum [do {(x9, x6) <- case x2 of
                                                       {Cons y9 y6 -> return (y9, y6); _ -> mzero};
                                           let {x4 = x9};
                                           x0 <- leIO x4 gen_leIO_x1;
                                           x5 <- splitoIOII x0 x6 x3;
                                           return (x0, x4, x5)},
                                       do {(x10, x6) <- case x3 of
                                                        {Cons y10 y6 -> return (y10, y6);
                                                         _ -> mzero};
                                           let {x4 = x10};
                                           x0 <- gtIO x4;
                                           x5 <- splitoIOII x0 x2 x6;
                                           return (x0, x4, x5)}]
gtIO x0 = msum [do {let {x1 = Zero};
                    x2 <- case x0 of
                          {Succ y2 -> return y2; _ -> mzero};
                    return x1},
                do {x2 <- case x0 of
                          {Succ y2 -> return y2; _ -> mzero};
                    x3 <- gtIO x2;
                    let {x1 = Succ x3};
                    return x1}]
leIO x0 gen_leIO_x1 = msum [do {guard (x0 == Zero);
                                x1 <- gen_leIO_x1;
                                return x1},
                            do {x2 <- case x0 of
                                      {Succ y2 -> return y2; _ -> mzero};
                                x3 <- leIO x2 gen_leIO_x1;
                                let {x1 = Succ x3};
                                return x1}]