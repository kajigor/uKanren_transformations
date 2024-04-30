module Model_elim_cpd_ans where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    deriving (Show, Eq)
solveIII x0 x1 x2 = msum [do {contrapositiveProveallIII x0 x1 x2;
                              return ()}]
contrapositiveProveallIII x0 x1 x2 = msum [do {input_clauseProveallIII x0 x1 x2;
                                               return ()}]
input_clauseProveallIII x0 x1 x2 = msum [do {guard (x1 == x2);
                                             guard (x0 == Nil);
                                             return ()},
                                         do {(x3, x4) <- case x2 of
                                                         {Cons y3 y4 -> return (y3, y4);
                                                          _ -> mzero};
                                             (x6, x5) <- case x0 of
                                                         {Cons y6 y5 -> return (y6, y5);
                                                          _ -> mzero};
                                             guard (x6 == x3);
                                             proveIII x5 x1 x4;
                                             return ()}]
proveIII x0 x1 x2 = msum [do {contrapositiveProveallIII x0 x1 x2;
                              return ()}]
solveIIO x0 x1 = msum [do {x2 <- contrapositiveProveallIIO x0 x1;
                           return x2}]
contrapositiveProveallIIO x0 x1 = msum [do {x2 <- input_clauseProveallIIO x0 x1;
                                            return x2}]
input_clauseProveallIIO x0 x1 = msum [do {guard (x0 == Nil);
                                          let {x2 = x1};
                                          return x2},
                                      do {(x6, x5) <- case x0 of
                                                      {Cons y6 y5 -> return (y6, y5); _ -> mzero};
                                          let {x3 = x6};
                                          x4 <- proveIIO x5 x1;
                                          let {x2 = Cons x3 x4};
                                          return x2}]
proveIIO x0 x1 = msum [do {x2 <- contrapositiveProveallIIO x0 x1;
                           return x2}]
solveIOI x0 x2 = msum [do {x1 <- contrapositiveProveallIOI x0 x2;
                           return x1}]
contrapositiveProveallIOI x0 x2 = msum [do {x1 <- input_clauseProveallIOI x0 x2;
                                            return x1}]
input_clauseProveallIOI x0 x2 = msum [do {guard (x0 == Nil);
                                          let {x1 = x2};
                                          return x1},
                                      do {(x3, x4) <- case x2 of
                                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                          (x6, x5) <- case x0 of
                                                      {Cons y6 y5 -> return (y6, y5); _ -> mzero};
                                          guard (x6 == x3);
                                          x1 <- proveIOI x5 x4;
                                          return x1}]
proveIOI x0 x2 = msum [do {x1 <- contrapositiveProveallIOI x0 x2;
                           return x1}]
solveIOO x0 gen_input_clauseProveallIOO_x2 = msum [do {(x1,
                                                        x2) <- contrapositiveProveallIOO x0 gen_input_clauseProveallIOO_x2;
                                                       return (x1, x2)}]
contrapositiveProveallIOO x0 gen_input_clauseProveallIOO_x2 = msum [do {(x1,
                                                                         x2) <- input_clauseProveallIOO x0 gen_input_clauseProveallIOO_x2;
                                                                        return (x1, x2)}]
input_clauseProveallIOO x0 gen_input_clauseProveallIOO_x2 = msum [do {guard (x0 == Nil);
                                                                      (x1,
                                                                       x2) <- do {x2 <- gen_input_clauseProveallIOO_x2;
                                                                                  return (x2, x2)};
                                                                      return (x1, x2)},
                                                                  do {(x6, x5) <- case x0 of
                                                                                  {Cons y6
                                                                                        y5 -> return (y6,
                                                                                                      y5);
                                                                                   _ -> mzero};
                                                                      let {x3 = x6};
                                                                      (x1,
                                                                       x4) <- proveIOO x5 gen_input_clauseProveallIOO_x2;
                                                                      let {x2 = Cons x3 x4};
                                                                      return (x1, x2)}]
proveIOO x0 gen_input_clauseProveallIOO_x2 = msum [do {(x1,
                                                        x2) <- contrapositiveProveallIOO x0 gen_input_clauseProveallIOO_x2;
                                                       return (x1, x2)}]
solveOII x1 x2 = msum [do {x0 <- contrapositiveProveallOII x1 x2;
                           return x0}]
contrapositiveProveallOII x1 x2 = msum [do {x0 <- input_clauseProveallOII x1 x2;
                                            return x0}]
input_clauseProveallOII x1 x2 = msum [do {guard (x1 == x2);
                                          let {x0 = Nil};
                                          return x0},
                                      do {(x3, x4) <- case x2 of
                                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                          let {x6 = x3};
                                          x5 <- proveOII x1 x4;
                                          let {x0 = Cons x6 x5};
                                          return x0}]
proveOII x1 x2 = msum [do {x0 <- contrapositiveProveallOII x1 x2;
                           return x0}]
solveOIO x1 gen_input_clauseProveallOIO_x3 = msum [do {(x0,
                                                        x2) <- contrapositiveProveallOIO x1 gen_input_clauseProveallOIO_x3;
                                                       return (x0, x2)}]
contrapositiveProveallOIO x1 gen_input_clauseProveallOIO_x3 = msum [do {(x0,
                                                                         x2) <- input_clauseProveallOIO x1 gen_input_clauseProveallOIO_x3;
                                                                        return (x0, x2)}]
input_clauseProveallOIO x1 gen_input_clauseProveallOIO_x3 = msum [do {let {x0 = Nil};
                                                                      let {x2 = x1};
                                                                      return (x0, x2)},
                                                                  do {(x5,
                                                                       x4) <- proveOIO x1 gen_input_clauseProveallOIO_x3;
                                                                      (x6,
                                                                       x3) <- do {x3 <- gen_input_clauseProveallOIO_x3;
                                                                                  return (x3, x3)};
                                                                      let {x2 = Cons x3 x4};
                                                                      let {x0 = Cons x6 x5};
                                                                      return (x0, x2)}]
proveOIO x1 gen_input_clauseProveallOIO_x3 = msum [do {(x0,
                                                        x2) <- contrapositiveProveallOIO x1 gen_input_clauseProveallOIO_x3;
                                                       return (x0, x2)}]
solveOOI x2 = msum [do {(x0, x1) <- contrapositiveProveallOOI x2;
                        return (x0, x1)}]
contrapositiveProveallOOI x2 = msum [do {(x0,
                                          x1) <- input_clauseProveallOOI x2;
                                         return (x0, x1)}]
input_clauseProveallOOI x2 = msum [do {let {x0 = Nil};
                                       let {x1 = x2};
                                       return (x0, x1)},
                                   do {(x3, x4) <- case x2 of
                                                   {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                       let {x6 = x3};
                                       (x5, x1) <- proveOOI x4;
                                       let {x0 = Cons x6 x5};
                                       return (x0, x1)}]
proveOOI x2 = msum [do {(x0, x1) <- contrapositiveProveallOOI x2;
                        return (x0, x1)}]
solveOOO gen_input_clauseProveallOOO_x2 gen_input_clauseProveallOOO_x3 gen_input_clauseProveallOOO_x4 = msum [do {(x0,
                                                                                                                   x1,
                                                                                                                   x2) <- contrapositiveProveallOOO gen_input_clauseProveallOOO_x2 gen_input_clauseProveallOOO_x3 gen_input_clauseProveallOOO_x4;
                                                                                                                  return (x0,
                                                                                                                          x1,
                                                                                                                          x2)}]
contrapositiveProveallOOO gen_input_clauseProveallOOO_x2 gen_input_clauseProveallOOO_x3 gen_input_clauseProveallOOO_x4 = msum [do {(x0,
                                                                                                                                    x1,
                                                                                                                                    x2) <- input_clauseProveallOOO gen_input_clauseProveallOOO_x2 gen_input_clauseProveallOOO_x3 gen_input_clauseProveallOOO_x4;
                                                                                                                                   return (x0,
                                                                                                                                           x1,
                                                                                                                                           x2)}]
input_clauseProveallOOO gen_input_clauseProveallOOO_x2 gen_input_clauseProveallOOO_x3 gen_input_clauseProveallOOO_x4 = msum [do {let {x0 = Nil};
                                                                                                                                 (x1,
                                                                                                                                  x2) <- do {x2 <- gen_input_clauseProveallOOO_x2;
                                                                                                                                             return (x2,
                                                                                                                                                     x2)};
                                                                                                                                 return (x0,
                                                                                                                                         x1,
                                                                                                                                         x2)},
                                                                                                                             do {(x6,
                                                                                                                                  x3) <- do {x3 <- gen_input_clauseProveallOOO_x3;
                                                                                                                                             return (x3,
                                                                                                                                                     x3)};
                                                                                                                                 (x2,
                                                                                                                                  x4) <- do {x4 <- gen_input_clauseProveallOOO_x4;
                                                                                                                                             let {x2 = Cons x3 x4};
                                                                                                                                             return (x2,
                                                                                                                                                     x4)};
                                                                                                                                 (x5,
                                                                                                                                  x1) <- proveOOI x4;
                                                                                                                                 let {x0 = Cons x6 x5};
                                                                                                                                 return (x0,
                                                                                                                                         x1,
                                                                                                                                         x2)}]