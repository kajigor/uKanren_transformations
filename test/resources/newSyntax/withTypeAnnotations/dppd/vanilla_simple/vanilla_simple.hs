module Vanilla_simple_for_fun where

import Stream
import Control.Monad

data Term
    = App Term Term Term
    | Cons Term Term
    | DoubleApp Term Term Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
helpOOO gen_my_clauseIO_x6 gen_my_clauseOO_x19 gen_my_clauseOO_x2 gen_my_clauseOO_x20 gen_my_clauseOO_x24 gen_my_clauseOO_x6 gen_my_clauseOO_x7 gen_my_clauseOO_x8 = msum [do {let {x86 = O};
                                                                                                                                                                               let {x85 = S x86};
                                                                                                                                                                               let {x84 = S x85};
                                                                                                                                                                               let {x89 = O};
                                                                                                                                                                               let {x88 = S x89};
                                                                                                                                                                               let {x93 = O};
                                                                                                                                                                               let {x92 = S x93};
                                                                                                                                                                               let {x91 = S x92};
                                                                                                                                                                               let {x96 = O};
                                                                                                                                                                               let {x95 = S x96};
                                                                                                                                                                               let {x97 = Nil};
                                                                                                                                                                               let {x94 = Cons x95 x97};
                                                                                                                                                                               let {x90 = Cons x91 x94};
                                                                                                                                                                               let {x87 = Cons x88 x90};
                                                                                                                                                                               let {x83 = Cons x84 x87};
                                                                                                                                                                               let {x98 = Nil};
                                                                                                                                                                               x81 <- solveO gen_my_clauseIO_x6 gen_my_clauseOO_x19 gen_my_clauseOO_x2 gen_my_clauseOO_x20 gen_my_clauseOO_x24 gen_my_clauseOO_x6 gen_my_clauseOO_x7 gen_my_clauseOO_x8;
                                                                                                                                                                               x82 <- case x81 of
                                                                                                                                                                                      {Cons y82
                                                                                                                                                                                            y98 -> do {guard (x98 == y98);
                                                                                                                                                                                                       return y82};
                                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                               (x0,
                                                                                                                                                                                x1,
                                                                                                                                                                                x2) <- case x82 of
                                                                                                                                                                                       {DoubleApp y0
                                                                                                                                                                                                  y1
                                                                                                                                                                                                  y2
                                                                                                                                                                                                  y83 -> do {guard (x83 == y83);
                                                                                                                                                                                                             return (y0,
                                                                                                                                                                                                                     y1,
                                                                                                                                                                                                                     y2)};
                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                               return (x0,
                                                                                                                                                                                       x1,
                                                                                                                                                                                       x2)}]
solveO gen_my_clauseIO_x6 gen_my_clauseOO_x19 gen_my_clauseOO_x2 gen_my_clauseOO_x20 gen_my_clauseOO_x24 gen_my_clauseOO_x6 gen_my_clauseOO_x7 gen_my_clauseOO_x8 = msum [do {let {x0 = Nil};
                                                                                                                                                                              return x0},
                                                                                                                                                                          do {x1 <- solve_atomO gen_my_clauseIO_x6 gen_my_clauseOO_x19 gen_my_clauseOO_x2 gen_my_clauseOO_x20 gen_my_clauseOO_x24 gen_my_clauseOO_x6 gen_my_clauseOO_x7 gen_my_clauseOO_x8;
                                                                                                                                                                              x2 <- solveO gen_my_clauseIO_x6 gen_my_clauseOO_x19 gen_my_clauseOO_x2 gen_my_clauseOO_x20 gen_my_clauseOO_x24 gen_my_clauseOO_x6 gen_my_clauseOO_x7 gen_my_clauseOO_x8;
                                                                                                                                                                              let {x0 = Cons x1 x2};
                                                                                                                                                                              return x0}]
solve_atomO gen_my_clauseIO_x6 gen_my_clauseOO_x19 gen_my_clauseOO_x2 gen_my_clauseOO_x20 gen_my_clauseOO_x24 gen_my_clauseOO_x6 gen_my_clauseOO_x7 gen_my_clauseOO_x8 = msum [do {(x0,
                                                                                                                                                                                    x1) <- my_clauseOO gen_my_clauseOO_x19 gen_my_clauseOO_x2 gen_my_clauseOO_x20 gen_my_clauseOO_x24 gen_my_clauseOO_x6 gen_my_clauseOO_x7 gen_my_clauseOO_x8;
                                                                                                                                                                                   solveI x1 gen_my_clauseIO_x6;
                                                                                                                                                                                   return x0}]
my_clauseOO gen_my_clauseOO_x19 gen_my_clauseOO_x2 gen_my_clauseOO_x20 gen_my_clauseOO_x24 gen_my_clauseOO_x6 gen_my_clauseOO_x7 gen_my_clauseOO_x8 = msum [do {let {x11 = Nil};
                                                                                                                                                                (x10,
                                                                                                                                                                 x6) <- do {x6 <- gen_my_clauseOO_x6;
                                                                                                                                                                            return (x6,
                                                                                                                                                                                    x6)};
                                                                                                                                                                (x12,
                                                                                                                                                                 x7) <- do {x7 <- gen_my_clauseOO_x7;
                                                                                                                                                                            return (x7,
                                                                                                                                                                                    x7)};
                                                                                                                                                                (x2,
                                                                                                                                                                 x3) <- case x7 of
                                                                                                                                                                        {App y2
                                                                                                                                                                             y3
                                                                                                                                                                             y6 -> do {guard (x6 == y6);
                                                                                                                                                                                       return (y2,
                                                                                                                                                                                               y3)};
                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                (x13,
                                                                                                                                                                 x8) <- do {x8 <- gen_my_clauseOO_x8;
                                                                                                                                                                            return (x8,
                                                                                                                                                                                    x8)};
                                                                                                                                                                let {x1 = Cons x12 x13};
                                                                                                                                                                x9 <- case x8 of
                                                                                                                                                                      {Cons y9
                                                                                                                                                                            y11 -> do {guard (x11 == y11);
                                                                                                                                                                                       return y9};
                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                (x4,
                                                                                                                                                                 x5) <- case x9 of
                                                                                                                                                                        {App y10
                                                                                                                                                                             y4
                                                                                                                                                                             y5 -> do {guard (x10 == y10);
                                                                                                                                                                                       return (y4,
                                                                                                                                                                                               y5)};
                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                let {x0 = DoubleApp x2 x3 x4 x5};
                                                                                                                                                                return (x0,
                                                                                                                                                                        x1)},
                                                                                                                                                            do {let {x14 = Nil};
                                                                                                                                                                let {x1 = Nil};
                                                                                                                                                                let {x16 = x14};
                                                                                                                                                                (x15,
                                                                                                                                                                 x2) <- do {x2 <- gen_my_clauseOO_x2;
                                                                                                                                                                            return (x2,
                                                                                                                                                                                    x2)};
                                                                                                                                                                let {x17 = x2};
                                                                                                                                                                let {x18 = x15};
                                                                                                                                                                let {x0 = App x16 x17 x18};
                                                                                                                                                                return (x0,
                                                                                                                                                                        x1)},
                                                                                                                                                            do {let {x25 = Nil};
                                                                                                                                                                let {x27 = x25};
                                                                                                                                                                (x21,
                                                                                                                                                                 x2) <- do {x2 <- gen_my_clauseOO_x2;
                                                                                                                                                                            return (x2,
                                                                                                                                                                                    x2)};
                                                                                                                                                                (x22,
                                                                                                                                                                 x19) <- do {x19 <- gen_my_clauseOO_x19;
                                                                                                                                                                             return (x19,
                                                                                                                                                                                     x19)};
                                                                                                                                                                x3 <- case x19 of
                                                                                                                                                                      {Cons y2
                                                                                                                                                                            y3 -> do {guard (x2 == y2);
                                                                                                                                                                                      return y3};
                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                (x23,
                                                                                                                                                                 x20) <- do {x20 <- gen_my_clauseOO_x20;
                                                                                                                                                                             return (x20,
                                                                                                                                                                                     x20)};
                                                                                                                                                                x5 <- case x20 of
                                                                                                                                                                      {Cons y21
                                                                                                                                                                            y5 -> do {guard (x21 == y21);
                                                                                                                                                                                      return y5};
                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                (x26,
                                                                                                                                                                 x24) <- do {x24 <- gen_my_clauseOO_x24;
                                                                                                                                                                             return (x24,
                                                                                                                                                                                     x24)};
                                                                                                                                                                let {x1 = Cons x26 x27};
                                                                                                                                                                x4 <- case x24 of
                                                                                                                                                                      {App y3
                                                                                                                                                                           y4
                                                                                                                                                                           y5 -> do {guard (x3 == y3);
                                                                                                                                                                                     guard (x5 == y5);
                                                                                                                                                                                     return y4};
                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                let {x0 = App x22 x4 x23};
                                                                                                                                                                return (x0,
                                                                                                                                                                        x1)}]
solveI x0 gen_my_clauseIO_x6 = msum [do {guard (x0 == Nil);
                                         return ()},
                                     do {(x1, x2) <- case x0 of
                                                     {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                                         solve_atomI x1 gen_my_clauseIO_x6;
                                         solveI x2 gen_my_clauseIO_x6;
                                         return ()}]
solve_atomI x0 gen_my_clauseIO_x6 = msum [do {x1 <- my_clauseIO x0 gen_my_clauseIO_x6;
                                              solveI x1 gen_my_clauseIO_x6;
                                              return ()}]
my_clauseIO x0 gen_my_clauseIO_x6 = msum [do {let {x11 = Nil};
                                              (x2, x3, x4, x5) <- case x0 of
                                                                  {DoubleApp y2
                                                                             y3
                                                                             y4
                                                                             y5 -> return (y2,
                                                                                           y3,
                                                                                           y4,
                                                                                           y5);
                                                                   _ -> mzero};
                                              (x10, x6) <- do {x6 <- gen_my_clauseIO_x6;
                                                               return (x6, x6)};
                                              let {x7 = App x2 x3 x6};
                                              let {x9 = App x10 x4 x5};
                                              let {x8 = Cons x9 x11};
                                              let {x12 = x7};
                                              let {x13 = x8};
                                              let {x1 = Cons x12 x13};
                                              return x1},
                                          do {let {x14 = Nil};
                                              let {x1 = Nil};
                                              (x16, x17, x18) <- case x0 of
                                                                 {App y16 y17 y18 -> return (y16,
                                                                                             y17,
                                                                                             y18);
                                                                  _ -> mzero};
                                              guard (x16 == x14);
                                              let {x2 = x17};
                                              let {x15 = x2};
                                              guard (x18 == x15);
                                              return x1},
                                          do {let {x25 = Nil};
                                              (x22, x4, x23) <- case x0 of
                                                                {App y22 y4 y23 -> return (y22,
                                                                                           y4,
                                                                                           y23);
                                                                 _ -> mzero};
                                              let {x19 = x22};
                                              (x2, x3) <- case x19 of
                                                          {Cons y2 y3 -> return (y2, y3);
                                                           _ -> mzero};
                                              let {x21 = x2};
                                              let {x20 = x23};
                                              x5 <- case x20 of
                                                    {Cons y21 y5 -> do {guard (x21 == y21);
                                                                        return y5};
                                                     _ -> mzero};
                                              let {x24 = App x3 x4 x5};
                                              let {x26 = x24};
                                              let {x27 = x25};
                                              let {x1 = Cons x26 x27};
                                              return x1}]
help gen_my_clauseIO_x6 gen_my_clauseOO_x19 gen_my_clauseOO_x2 gen_my_clauseOO_x20 gen_my_clauseOO_x24 gen_my_clauseOO_x6 gen_my_clauseOO_x7 gen_my_clauseOO_x8 = helpOOO gen_my_clauseIO_x6 gen_my_clauseOO_x19 gen_my_clauseOO_x2 gen_my_clauseOO_x20 gen_my_clauseOO_x24 gen_my_clauseOO_x6 gen_my_clauseOO_x7 gen_my_clauseOO_x8