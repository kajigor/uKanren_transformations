module NthOpt_cpd_ans where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | None
    | Some Term
    deriving (Show, Eq)
nthOptII x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {nthOpt0II x0 x1;
                                                                                 nthOpt4II x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                                 return ()}]
nthOptIO x0 gen_nthOpt0IO_x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {x1 <- nthOpt0IO x0 gen_nthOpt0IO_x1;
                                                                                               nthOpt4II x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                                               return x1}]
nthOptOI x1 gen_nthOpt0OI_x0 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {x0 <- nthOpt0OI x1 gen_nthOpt0OI_x0;
                                                                                               nthOpt4II x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                                               return x0}]
nthOptOO gen_nthOpt0OO_x0 gen_nthOpt0OO_x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {(x0,
                                                                                                              x1) <- nthOpt0OO gen_nthOpt0OO_x0 gen_nthOpt0OO_x1;
                                                                                                             nthOpt4II x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                                                             return (x0,
                                                                                                                     x1)}]
nthOpt0II x0 x1 = msum [do {guard (x1 == None); return ()},
                        do {guard (x0 == Nil); return ()}]
nthOpt0IO x0 gen_nthOpt0IO_x1 = msum [do {let {x1 = None};
                                          return x1},
                                      do {guard (x0 == Nil); x1 <- gen_nthOpt0IO_x1; return x1}]
nthOpt0OI x1 gen_nthOpt0OI_x0 = msum [do {guard (x1 == None);
                                          x0 <- gen_nthOpt0OI_x0;
                                          return x0},
                                      do {let {x0 = Nil}; return x0}]
nthOpt0OO gen_nthOpt0OO_x0 gen_nthOpt0OO_x1 = msum [do {let {x1 = None};
                                                        x0 <- gen_nthOpt0OO_x0;
                                                        return (x0, x1)},
                                                    do {let {x0 = Nil};
                                                        x1 <- gen_nthOpt0OO_x1;
                                                        return (x0, x1)}]
nthOpt4II x0 x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {(x2,
                                                                                   x3) <- case x0 of
                                                                                          {Cons y2
                                                                                                y3 -> return (y2,
                                                                                                              y3);
                                                                                           _ -> mzero};
                                                                                  return ()},
                                                                              do {x3 <- nthOpt1IO x1 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                                  return ()}]
nthOpt1IO x0 gen_nthOpt14IO_x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {x1 <- nthOpt14IO x0 gen_nthOpt14IO_x1;
                                                                               nthOpt15II x0 x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                               return x1}]
nthOpt14IO x0 gen_nthOpt14IO_x1 = msum [do {let {x1 = Nil};
                                            return x1},
                                        do {guard (x0 == None); x1 <- gen_nthOpt14IO_x1; return x1}]
nthOpt15II x0 x1 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {(x2,
                                                                  x3) <- case x1 of
                                                                         {Cons y2 y3 -> return (y2,
                                                                                                y3);
                                                                          _ -> mzero};
                                                                 return ()},
                                                             do {x3 <- nthOpt2IO x0 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1;
                                                                 return ()}]
nthOpt2IO x0 gen_nthOpt25IO_x1 gen_nthOpt36IO_x1 = msum [do {x1 <- nthOpt25IO x0 gen_nthOpt25IO_x1;
                                                             nthOpt26II x0 x1 gen_nthOpt36IO_x1;
                                                             return x1}]
nthOpt25IO x0 gen_nthOpt25IO_x1 = msum [do {let {x1 = Nil};
                                            return x1},
                                        do {guard (x0 == None); x1 <- gen_nthOpt25IO_x1; return x1}]
nthOpt26II x0 x1 gen_nthOpt36IO_x1 = msum [do {(x2,
                                                x3) <- case x1 of
                                                       {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                               return ()},
                                           do {x3 <- nthOpt3IO x0 gen_nthOpt36IO_x1; return ()}]
nthOpt3IO x0 gen_nthOpt36IO_x1 = msum [do {x1 <- nthOpt36IO x0 gen_nthOpt36IO_x1;
                                           nthOpt37II x0 x1;
                                           return x1}]
nthOpt36IO x0 gen_nthOpt36IO_x1 = msum [do {let {x1 = Nil};
                                            return x1},
                                        do {guard (x0 == None); x1 <- gen_nthOpt36IO_x1; return x1}]
nthOpt37II x0 x1 = msum [do {(x2, x3) <- case x1 of
                                         {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                             return ()},
                         do {x2 <- case x0 of
                                   {Some y2 -> return y2; _ -> mzero};
                             return ()}]