module Depth_simple where

import Stream
import Control.Monad
import Term

depthIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x7 = Immature $ msum [do {let {x1 = O};
                                                                 guard (x0 == Term.True);
                                                                 return x1},
                                                             do {(x2, x3) <- case x0 of
                                                                             {Cons y2
                                                                                   y3 -> return (y2,
                                                                                                 y3);
                                                                              _ -> mzero};
                                                                 x4 <- depthIO x2 gen_prog_clauseIO_x4 gen_prog_clauseIO_x7;
                                                                 x5 <- depthIO x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x7;
                                                                 x1 <- maxIIO x4 x5;
                                                                 return x1},
                                                             do {x2 <- prog_clauseIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x7;
                                                                 x3 <- depthIO x2 gen_prog_clauseIO_x4 gen_prog_clauseIO_x7;
                                                                 let {x6 = x3};
                                                                 let {x1 = S x6};
                                                                 return x1}]
maxIIO x0 x1 = Immature $ msum [do {guard (x1 == O);
                         let {x2 = x0};
                         return x2},
                     do {guard (x0 == O); let {x2 = x1}; return x2},
                     do {x3 <- case x0 of
                               {S y3 -> return y3; _ -> mzero};
                         x4 <- case x1 of
                               {S y4 -> return y4; _ -> mzero};
                         x5 <- maxIIO x3 x4;
                         let {x2 = S x5};
                         return x2}]
prog_clauseIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x7 = Immature $ msum [do {(x2,
                                                                        x3) <- case x0 of
                                                                               {Member y2
                                                                                       y3 -> return (y2,
                                                                                                     y3);
                                                                                _ -> mzero};
                                                                       let {x9 = x3};
                                                                       (x8,
                                                                        x7) <- do {x7 <- gen_prog_clauseIO_x7;
                                                                                   return (x7, x7)};
                                                                       x5 <- case x7 of
                                                                             {Cons y2
                                                                                   y5 -> do {guard (x2 == y2);
                                                                                             return y5};
                                                                              _ -> mzero};
                                                                       (x1,
                                                                        x4) <- do {x4 <- gen_prog_clauseIO_x4;
                                                                                   let {x1 = Append x4 x8 x9};
                                                                                   return (x1, x4)};
                                                                       return x1},
                                                                   do {let {x10 = Nil};
                                                                       let {x1 = Term.True};
                                                                       (x12, x13, x14) <- case x0 of
                                                                                          {Append y12
                                                                                                  y13
                                                                                                  y14 -> return (y12,
                                                                                                                 y13,
                                                                                                                 y14);
                                                                                           _ -> mzero};
                                                                       guard (x12 == x10);
                                                                       let {x2 = x13};
                                                                       let {x11 = x2};
                                                                       guard (x14 == x11);
                                                                       return x1},
                                                                   do {(x18, x4, x19) <- case x0 of
                                                                                         {Append y18
                                                                                                 y4
                                                                                                 y19 -> return (y18,
                                                                                                                y4,
                                                                                                                y19);
                                                                                          _ -> mzero};
                                                                       let {x15 = x18};
                                                                       (x2, x3) <- case x15 of
                                                                                   {Cons y2
                                                                                         y3 -> return (y2,
                                                                                                       y3);
                                                                                    _ -> mzero};
                                                                       let {x17 = x2};
                                                                       let {x16 = x19};
                                                                       x5 <- case x16 of
                                                                             {Cons y17
                                                                                   y5 -> do {guard (x17 == y17);
                                                                                             return y5};
                                                                              _ -> mzero};
                                                                       let {x20 = x3};
                                                                       let {x21 = x4};
                                                                       let {x22 = x5};
                                                                       let {x1 = Append x20 x21 x22};
                                                                       return x1}]
depth x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x7 = depthIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x7