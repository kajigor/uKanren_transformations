module Hanoi_cpd_ans where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | One
    | Pair Term Term
    | Thr
    | Two
    deriving (Show, Eq)
checkI x0 = msum [do {(x1, x2) <- case x0 of
                                  {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                      one_stepCheckII x1 x2;
                      return ()}]
checkO gen_one_stepCheck0O_x2 gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2 = msum [do {(x1,
                                                                                           x2) <- one_stepCheckOO gen_one_stepCheck0O_x2 gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2;
                                                                                          let {x0 = Cons x1 x2};
                                                                                          return x0}]
one_stepCheckII x0 x1 = msum [do {one_stepCheck0I x0;
                                  x2 <- notEqStickGetSetCheckIO x1;
                                  return ()}]
notEqStickGetSetCheckIO x0 = msum [do {let {x1 = Two};
                                       check1I x0;
                                       return x1},
                                   do {let {x1 = Thr}; check3I x0; return x1}]
check1I x0 = msum [do {(x1, x2) <- case x0 of
                                   {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                       one_stepCheck1II x1 x2;
                       return ()}]
check3I x0 = msum [do {(x1, x2) <- case x0 of
                                   {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                       one_stepCheck3II x1 x2;
                       return ()}]
one_stepCheckOO gen_one_stepCheck0O_x2 gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2 = msum [do {x0 <- one_stepCheck0O gen_one_stepCheck0O_x2;
                                                                                                   (x1,
                                                                                                    x2) <- notEqStickGetSetCheckOO gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2;
                                                                                                   return (x0,
                                                                                                           x1)}]
notEqStickGetSetCheckOO gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2 = msum [do {let {x1 = Two};
                                                                                    x0 <- check1O gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2;
                                                                                    return (x0,
                                                                                            x1)},
                                                                                do {let {x1 = Thr};
                                                                                    x0 <- check3O gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2;
                                                                                    return (x0,
                                                                                            x1)}]
check1O gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2 = msum [do {(x1,
                                                                     x2) <- one_stepCheck1OO gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2;
                                                                    let {x0 = Cons x1 x2};
                                                                    return x0}]
check3O gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2 = msum [do {(x1,
                                                                     x2) <- one_stepCheck3OO gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2;
                                                                    let {x0 = Cons x1 x2};
                                                                    return x0}]
one_stepCheck0I x0 = msum [do {(x3, x2) <- case x0 of
                                           {Pair y3 y2 -> return (y3, y2); _ -> mzero};
                               guard (x3 == One);
                               return ()}]
one_stepCheck0O gen_one_stepCheck0O_x2 = msum [do {let {x3 = One};
                                                   (x0, x2) <- do {x2 <- gen_one_stepCheck0O_x2;
                                                                   let {x0 = Pair x3 x2};
                                                                   return (x0, x2)};
                                                   return x0}]
one_stepCheck1II x0 x1 = msum [do {one_stepCheck10I x0;
                                   x2 <- notEqStickGetSetCheck1IO x1;
                                   return ()}]
notEqStickGetSetCheck1IO x0 = msum [do {let {x1 = Two};
                                        check3I x0;
                                        return x1}]
one_stepCheck1OO gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2 = msum [do {x0 <- one_stepCheck10O gen_one_stepCheck10O_x2;
                                                                             (x1,
                                                                              x2) <- notEqStickGetSetCheck1OO gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2;
                                                                             return (x0, x1)}]
notEqStickGetSetCheck1OO gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2 = msum [do {let {x1 = Two};
                                                                                     x0 <- check3O gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2;
                                                                                     return (x0,
                                                                                             x1)}]
one_stepCheck10I x0 = msum [do {(x2, x4) <- case x0 of
                                            {Pair y2 y4 -> return (y2, y4); _ -> mzero};
                                guard (x4 == Thr);
                                return ()}]
one_stepCheck10O gen_one_stepCheck10O_x2 = msum [do {let {x4 = Thr};
                                                     (x0, x2) <- do {x2 <- gen_one_stepCheck10O_x2;
                                                                     let {x0 = Pair x2 x4};
                                                                     return (x0, x2)};
                                                     return x0}]
one_stepCheck3II x0 x1 = msum [do {one_stepCheck30I x0;
                                   x2 <- notEqStickGetSetCheck2IO x1;
                                   return ()}]
notEqStickGetSetCheck2IO x0 = msum [do {let {x1 = Thr};
                                        check1I x0;
                                        return x1}]
one_stepCheck3OO gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2 = msum [do {x0 <- one_stepCheck30O gen_one_stepCheck30O_x2;
                                                                             (x1,
                                                                              x2) <- notEqStickGetSetCheck2OO gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2;
                                                                             return (x0, x1)}]
notEqStickGetSetCheck2OO gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2 = msum [do {let {x1 = Thr};
                                                                                     x0 <- check1O gen_one_stepCheck10O_x2 gen_one_stepCheck30O_x2;
                                                                                     return (x0,
                                                                                             x1)}]
one_stepCheck30I x0 = msum [do {(x2, x5) <- case x0 of
                                            {Pair y2 y5 -> return (y2, y5); _ -> mzero};
                                guard (x5 == Two);
                                return ()}]
one_stepCheck30O gen_one_stepCheck30O_x2 = msum [do {let {x5 = Two};
                                                     (x0, x2) <- do {x2 <- gen_one_stepCheck30O_x2;
                                                                     let {x0 = Pair x2 x5};
                                                                     return (x0, x2)};
                                                     return x0}]