module Vanilla where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
solveIII x0 x1 x2 = msum [do {my_clauseSolveMy_clauseSolveIII x0 x1 x2;
                              return ()},
                          do {solveIII x0 x1 x2; return ()}]
my_clauseSolveMy_clauseSolveIII x0 x1 x2 = msum [do {let {x8 = O};
                                                     let {x7 = S x8};
                                                     let {x11 = O};
                                                     let {x10 = S x11};
                                                     let {x15 = O};
                                                     let {x14 = S x15};
                                                     let {x13 = S x14};
                                                     let {x18 = O};
                                                     let {x17 = S x18};
                                                     let {x19 = Nil};
                                                     let {x16 = Cons x17 x19};
                                                     let {x12 = Cons x13 x16};
                                                     let {x9 = Cons x10 x12};
                                                     let {x6 = Cons x7 x9};
                                                     my_clauseSolveIII x1 x2 x6;
                                                     guard (x0 == Nil);
                                                     return ()},
                                                 do {let {x23 = O};
                                                     let {x22 = S x23};
                                                     let {x26 = O};
                                                     let {x25 = S x26};
                                                     let {x30 = O};
                                                     let {x29 = S x30};
                                                     let {x28 = S x29};
                                                     let {x33 = O};
                                                     let {x32 = S x33};
                                                     let {x34 = Nil};
                                                     let {x31 = Cons x32 x34};
                                                     let {x27 = Cons x28 x31};
                                                     let {x24 = Cons x25 x27};
                                                     let {x21 = Cons x22 x24};
                                                     (x3, x4) <- case x0 of
                                                                 {Cons y3 y4 -> return (y3, y4);
                                                                  _ -> mzero};
                                                     x5 <- my_clauseSolveIIO x4 x1;
                                                     let {x20 = Cons x3 x5};
                                                     my_clauseSolveIII x20 x2 x21;
                                                     return ()},
                                                 do {my_clauseSolveMy_clauseSolveIII x0 x1 x2;
                                                     return ()}]
my_clauseSolveIII x0 x1 x2 = msum [do {guard (x1 == x2);
                                       guard (x0 == Nil);
                                       return ()},
                                   do {(x3, x4) <- case x2 of
                                                   {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                       (x35, x5) <- case x0 of
                                                    {Cons y35 y5 -> return (y35, y5); _ -> mzero};
                                       guard (x35 == x3);
                                       my_clauseSolveIII x5 x1 x4;
                                       return ()},
                                   do {my_clauseSolveIII x0 x1 x2; return ()}]
my_clauseSolveIIO x0 x1 = msum [do {guard (x0 == Nil);
                                    let {x2 = x1};
                                    return x2},
                                do {(x35, x5) <- case x0 of
                                                 {Cons y35 y5 -> return (y35, y5); _ -> mzero};
                                    let {x3 = x35};
                                    x4 <- my_clauseSolveIIO x5 x1;
                                    let {x2 = Cons x3 x4};
                                    return x2},
                                do {x2 <- my_clauseSolveIIO x0 x1; return x2}]
solveIIO x0 x1 = msum [do {x2 <- my_clauseSolveMy_clauseSolveIIO x0 x1;
                           return x2},
                       do {x2 <- solveIIO x0 x1; return x2}]
my_clauseSolveMy_clauseSolveIIO x0 x1 = msum [do {let {x8 = O};
                                                  let {x7 = S x8};
                                                  let {x11 = O};
                                                  let {x10 = S x11};
                                                  let {x15 = O};
                                                  let {x14 = S x15};
                                                  let {x13 = S x14};
                                                  let {x18 = O};
                                                  let {x17 = S x18};
                                                  let {x19 = Nil};
                                                  let {x16 = Cons x17 x19};
                                                  let {x12 = Cons x13 x16};
                                                  let {x9 = Cons x10 x12};
                                                  let {x6 = Cons x7 x9};
                                                  guard (x0 == Nil);
                                                  x2 <- my_clauseSolveIOI x1 x6;
                                                  return x2},
                                              do {let {x23 = O};
                                                  let {x22 = S x23};
                                                  let {x26 = O};
                                                  let {x25 = S x26};
                                                  let {x30 = O};
                                                  let {x29 = S x30};
                                                  let {x28 = S x29};
                                                  let {x33 = O};
                                                  let {x32 = S x33};
                                                  let {x34 = Nil};
                                                  let {x31 = Cons x32 x34};
                                                  let {x27 = Cons x28 x31};
                                                  let {x24 = Cons x25 x27};
                                                  let {x21 = Cons x22 x24};
                                                  (x3, x4) <- case x0 of
                                                              {Cons y3 y4 -> return (y3, y4);
                                                               _ -> mzero};
                                                  x5 <- my_clauseSolveIIO x4 x1;
                                                  let {x20 = Cons x3 x5};
                                                  x2 <- my_clauseSolveIOI x20 x21;
                                                  return x2},
                                              do {x2 <- my_clauseSolveMy_clauseSolveIIO x0 x1;
                                                  return x2}]
my_clauseSolveIOI x0 x2 = msum [do {guard (x0 == Nil);
                                    let {x1 = x2};
                                    return x1},
                                do {(x3, x4) <- case x2 of
                                                {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                    (x35, x5) <- case x0 of
                                                 {Cons y35 y5 -> return (y35, y5); _ -> mzero};
                                    guard (x35 == x3);
                                    x1 <- my_clauseSolveIOI x5 x4;
                                    return x1},
                                do {x1 <- my_clauseSolveIOI x0 x2; return x1}]
solveIOI x0 x2 = msum [do {x1 <- my_clauseSolveMy_clauseSolveIOI x0 x2;
                           return x1},
                       do {x1 <- solveIOI x0 x2; return x1}]
my_clauseSolveMy_clauseSolveIOI x0 x2 = msum [do {let {x8 = O};
                                                  let {x7 = S x8};
                                                  let {x11 = O};
                                                  let {x10 = S x11};
                                                  let {x15 = O};
                                                  let {x14 = S x15};
                                                  let {x13 = S x14};
                                                  let {x18 = O};
                                                  let {x17 = S x18};
                                                  let {x19 = Nil};
                                                  let {x16 = Cons x17 x19};
                                                  let {x12 = Cons x13 x16};
                                                  let {x9 = Cons x10 x12};
                                                  let {x6 = Cons x7 x9};
                                                  guard (x0 == Nil);
                                                  x1 <- my_clauseSolveOII x2 x6;
                                                  return x1},
                                              do {let {x23 = O};
                                                  let {x22 = S x23};
                                                  let {x26 = O};
                                                  let {x25 = S x26};
                                                  let {x30 = O};
                                                  let {x29 = S x30};
                                                  let {x28 = S x29};
                                                  let {x33 = O};
                                                  let {x32 = S x33};
                                                  let {x34 = Nil};
                                                  let {x31 = Cons x32 x34};
                                                  let {x27 = Cons x28 x31};
                                                  let {x24 = Cons x25 x27};
                                                  let {x21 = Cons x22 x24};
                                                  (x3, x4) <- case x0 of
                                                              {Cons y3 y4 -> return (y3, y4);
                                                               _ -> mzero};
                                                  x20 <- my_clauseSolveOII x2 x21;
                                                  x5 <- case x20 of
                                                        {Cons y3 y5 -> do {guard (x3 == y3);
                                                                           return y5};
                                                         _ -> mzero};
                                                  x1 <- my_clauseSolveIOI x4 x5;
                                                  return x1},
                                              do {x1 <- my_clauseSolveMy_clauseSolveIOI x0 x2;
                                                  return x1}]
my_clauseSolveOII x1 x2 = msum [do {guard (x1 == x2);
                                    let {x0 = Nil};
                                    return x0},
                                do {(x3, x4) <- case x2 of
                                                {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                    let {x35 = x3};
                                    x5 <- my_clauseSolveOII x1 x4;
                                    let {x0 = Cons x35 x5};
                                    return x0},
                                do {x0 <- my_clauseSolveOII x1 x2; return x0}]
solveIOO x0 gen_my_clauseSolveIOO_x2 = msum [do {(x1,
                                                  x2) <- my_clauseSolveMy_clauseSolveIOO x0 gen_my_clauseSolveIOO_x2;
                                                 return (x1, x2)},
                                             do {(x1, x2) <- solveIOO x0 gen_my_clauseSolveIOO_x2;
                                                 return (x1, x2)}]
my_clauseSolveMy_clauseSolveIOO x0 gen_my_clauseSolveIOO_x2 = msum [do {let {x8 = O};
                                                                        let {x7 = S x8};
                                                                        let {x11 = O};
                                                                        let {x10 = S x11};
                                                                        let {x15 = O};
                                                                        let {x14 = S x15};
                                                                        let {x13 = S x14};
                                                                        let {x18 = O};
                                                                        let {x17 = S x18};
                                                                        let {x19 = Nil};
                                                                        let {x16 = Cons x17 x19};
                                                                        let {x12 = Cons x13 x16};
                                                                        let {x9 = Cons x10 x12};
                                                                        let {x6 = Cons x7 x9};
                                                                        guard (x0 == Nil);
                                                                        (x1,
                                                                         x2) <- my_clauseSolveOOI x6;
                                                                        return (x1, x2)},
                                                                    do {let {x23 = O};
                                                                        let {x22 = S x23};
                                                                        let {x26 = O};
                                                                        let {x25 = S x26};
                                                                        let {x30 = O};
                                                                        let {x29 = S x30};
                                                                        let {x28 = S x29};
                                                                        let {x33 = O};
                                                                        let {x32 = S x33};
                                                                        let {x34 = Nil};
                                                                        let {x31 = Cons x32 x34};
                                                                        let {x27 = Cons x28 x31};
                                                                        let {x24 = Cons x25 x27};
                                                                        let {x21 = Cons x22 x24};
                                                                        (x3, x4) <- case x0 of
                                                                                    {Cons y3
                                                                                          y4 -> return (y3,
                                                                                                        y4);
                                                                                     _ -> mzero};
                                                                        (x1,
                                                                         x5) <- my_clauseSolveIOO x4 gen_my_clauseSolveIOO_x2;
                                                                        let {x20 = Cons x3 x5};
                                                                        x2 <- my_clauseSolveIOI x20 x21;
                                                                        return (x1, x2)},
                                                                    do {(x1,
                                                                         x2) <- my_clauseSolveMy_clauseSolveIOO x0 gen_my_clauseSolveIOO_x2;
                                                                        return (x1, x2)}]
my_clauseSolveIOO x0 gen_my_clauseSolveIOO_x2 = msum [do {guard (x0 == Nil);
                                                          (x1,
                                                           x2) <- do {x2 <- gen_my_clauseSolveIOO_x2;
                                                                      return (x2, x2)};
                                                          return (x1, x2)},
                                                      do {(x35, x5) <- case x0 of
                                                                       {Cons y35 y5 -> return (y35,
                                                                                               y5);
                                                                        _ -> mzero};
                                                          let {x3 = x35};
                                                          (x1,
                                                           x4) <- my_clauseSolveIOO x5 gen_my_clauseSolveIOO_x2;
                                                          let {x2 = Cons x3 x4};
                                                          return (x1, x2)},
                                                      do {(x1,
                                                           x2) <- my_clauseSolveIOO x0 gen_my_clauseSolveIOO_x2;
                                                          return (x1, x2)}]
my_clauseSolveOOI x2 = msum [do {let {x0 = Nil};
                                 let {x1 = x2};
                                 return (x0, x1)},
                             do {(x3, x4) <- case x2 of
                                             {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                 let {x35 = x3};
                                 (x5, x1) <- my_clauseSolveOOI x4;
                                 let {x0 = Cons x35 x5};
                                 return (x0, x1)},
                             do {(x0, x1) <- my_clauseSolveOOI x2; return (x0, x1)}]
solveOII x1 x2 = msum [do {x0 <- my_clauseSolveMy_clauseSolveOII x1 x2;
                           return x0},
                       do {x0 <- solveOII x1 x2; return x0}]
my_clauseSolveMy_clauseSolveOII x1 x2 = msum [do {let {x0 = Nil};
                                                  let {x8 = O};
                                                  let {x7 = S x8};
                                                  let {x11 = O};
                                                  let {x10 = S x11};
                                                  let {x15 = O};
                                                  let {x14 = S x15};
                                                  let {x13 = S x14};
                                                  let {x18 = O};
                                                  let {x17 = S x18};
                                                  let {x19 = Nil};
                                                  let {x16 = Cons x17 x19};
                                                  let {x12 = Cons x13 x16};
                                                  let {x9 = Cons x10 x12};
                                                  let {x6 = Cons x7 x9};
                                                  my_clauseSolveIII x1 x2 x6;
                                                  return x0},
                                              do {let {x23 = O};
                                                  let {x22 = S x23};
                                                  let {x26 = O};
                                                  let {x25 = S x26};
                                                  let {x30 = O};
                                                  let {x29 = S x30};
                                                  let {x28 = S x29};
                                                  let {x33 = O};
                                                  let {x32 = S x33};
                                                  let {x34 = Nil};
                                                  let {x31 = Cons x32 x34};
                                                  let {x27 = Cons x28 x31};
                                                  let {x24 = Cons x25 x27};
                                                  let {x21 = Cons x22 x24};
                                                  x20 <- my_clauseSolveOII x2 x21;
                                                  (x3, x5) <- case x20 of
                                                              {Cons y3 y5 -> return (y3, y5);
                                                               _ -> mzero};
                                                  x4 <- my_clauseSolveOII x1 x5;
                                                  let {x0 = Cons x3 x4};
                                                  return x0},
                                              do {x0 <- my_clauseSolveMy_clauseSolveOII x1 x2;
                                                  return x0}]
solveOIO x1 gen_my_clauseSolveOIO_x3 = msum [do {(x0,
                                                  x2) <- my_clauseSolveMy_clauseSolveOIO x1 gen_my_clauseSolveOIO_x3;
                                                 return (x0, x2)},
                                             do {(x0, x2) <- solveOIO x1 gen_my_clauseSolveOIO_x3;
                                                 return (x0, x2)}]
my_clauseSolveMy_clauseSolveOIO x1 gen_my_clauseSolveOIO_x3 = msum [do {let {x0 = Nil};
                                                                        let {x8 = O};
                                                                        let {x7 = S x8};
                                                                        let {x11 = O};
                                                                        let {x10 = S x11};
                                                                        let {x15 = O};
                                                                        let {x14 = S x15};
                                                                        let {x13 = S x14};
                                                                        let {x18 = O};
                                                                        let {x17 = S x18};
                                                                        let {x19 = Nil};
                                                                        let {x16 = Cons x17 x19};
                                                                        let {x12 = Cons x13 x16};
                                                                        let {x9 = Cons x10 x12};
                                                                        let {x6 = Cons x7 x9};
                                                                        x2 <- my_clauseSolveIOI x1 x6;
                                                                        return (x0, x2)},
                                                                    do {let {x23 = O};
                                                                        let {x22 = S x23};
                                                                        let {x26 = O};
                                                                        let {x25 = S x26};
                                                                        let {x30 = O};
                                                                        let {x29 = S x30};
                                                                        let {x28 = S x29};
                                                                        let {x33 = O};
                                                                        let {x32 = S x33};
                                                                        let {x34 = Nil};
                                                                        let {x31 = Cons x32 x34};
                                                                        let {x27 = Cons x28 x31};
                                                                        let {x24 = Cons x25 x27};
                                                                        let {x21 = Cons x22 x24};
                                                                        (x4,
                                                                         x5) <- my_clauseSolveOIO x1 gen_my_clauseSolveOIO_x3;
                                                                        (x20,
                                                                         x2) <- my_clauseSolveOOI x21;
                                                                        x3 <- case x20 of
                                                                              {Cons y3
                                                                                    y5 -> do {guard (x5 == y5);
                                                                                              return y3};
                                                                               _ -> mzero};
                                                                        let {x0 = Cons x3 x4};
                                                                        return (x0, x2)},
                                                                    do {(x0,
                                                                         x2) <- my_clauseSolveMy_clauseSolveOIO x1 gen_my_clauseSolveOIO_x3;
                                                                        return (x0, x2)}]
my_clauseSolveOIO x1 gen_my_clauseSolveOIO_x3 = msum [do {let {x0 = Nil};
                                                          let {x2 = x1};
                                                          return (x0, x2)},
                                                      do {(x5,
                                                           x4) <- my_clauseSolveOIO x1 gen_my_clauseSolveOIO_x3;
                                                          (x35,
                                                           x3) <- do {x3 <- gen_my_clauseSolveOIO_x3;
                                                                      return (x3, x3)};
                                                          let {x2 = Cons x3 x4};
                                                          let {x0 = Cons x35 x5};
                                                          return (x0, x2)},
                                                      do {(x0,
                                                           x2) <- my_clauseSolveOIO x1 gen_my_clauseSolveOIO_x3;
                                                          return (x0, x2)}]
solveOOI x2 = msum [do {(x0,
                         x1) <- my_clauseSolveMy_clauseSolveOOI x2;
                        return (x0, x1)},
                    do {(x0, x1) <- solveOOI x2; return (x0, x1)}]
my_clauseSolveMy_clauseSolveOOI x2 = msum [do {let {x0 = Nil};
                                               let {x8 = O};
                                               let {x7 = S x8};
                                               let {x11 = O};
                                               let {x10 = S x11};
                                               let {x15 = O};
                                               let {x14 = S x15};
                                               let {x13 = S x14};
                                               let {x18 = O};
                                               let {x17 = S x18};
                                               let {x19 = Nil};
                                               let {x16 = Cons x17 x19};
                                               let {x12 = Cons x13 x16};
                                               let {x9 = Cons x10 x12};
                                               let {x6 = Cons x7 x9};
                                               x1 <- my_clauseSolveOII x2 x6;
                                               return (x0, x1)},
                                           do {let {x23 = O};
                                               let {x22 = S x23};
                                               let {x26 = O};
                                               let {x25 = S x26};
                                               let {x30 = O};
                                               let {x29 = S x30};
                                               let {x28 = S x29};
                                               let {x33 = O};
                                               let {x32 = S x33};
                                               let {x34 = Nil};
                                               let {x31 = Cons x32 x34};
                                               let {x27 = Cons x28 x31};
                                               let {x24 = Cons x25 x27};
                                               let {x21 = Cons x22 x24};
                                               x20 <- my_clauseSolveOII x2 x21;
                                               (x3, x5) <- case x20 of
                                                           {Cons y3 y5 -> return (y3, y5);
                                                            _ -> mzero};
                                               (x4, x1) <- my_clauseSolveOOI x5;
                                               let {x0 = Cons x3 x4};
                                               return (x0, x1)},
                                           do {(x0, x1) <- my_clauseSolveMy_clauseSolveOOI x2;
                                               return (x0, x1)}]
solveOOO = msum [do {(x0,
                      x1,
                      x2) <- my_clauseSolveMy_clauseSolveOOO;
                     return (x0, x1, x2)},
                 do {(x0, x1, x2) <- solveOOO; return (x0, x1, x2)}]
my_clauseSolveMy_clauseSolveOOO = msum [do {let {x0 = Nil};
                                            let {x8 = O};
                                            let {x7 = S x8};
                                            let {x11 = O};
                                            let {x10 = S x11};
                                            let {x15 = O};
                                            let {x14 = S x15};
                                            let {x13 = S x14};
                                            let {x18 = O};
                                            let {x17 = S x18};
                                            let {x19 = Nil};
                                            let {x16 = Cons x17 x19};
                                            let {x12 = Cons x13 x16};
                                            let {x9 = Cons x10 x12};
                                            let {x6 = Cons x7 x9};
                                            (x1, x2) <- my_clauseSolveOOI x6;
                                            return (x0, x1, x2)},
                                        do {let {x23 = O};
                                            let {x22 = S x23};
                                            let {x26 = O};
                                            let {x25 = S x26};
                                            let {x30 = O};
                                            let {x29 = S x30};
                                            let {x28 = S x29};
                                            let {x33 = O};
                                            let {x32 = S x33};
                                            let {x34 = Nil};
                                            let {x31 = Cons x32 x34};
                                            let {x27 = Cons x28 x31};
                                            let {x24 = Cons x25 x27};
                                            let {x21 = Cons x22 x24};
                                            (x20, x2) <- my_clauseSolveOOI x21;
                                            (x3, x5) <- case x20 of
                                                        {Cons y3 y5 -> return (y3, y5); _ -> mzero};
                                            (x4, x1) <- my_clauseSolveOOI x5;
                                            let {x0 = Cons x3 x4};
                                            return (x0, x1, x2)},
                                        do {(x0, x1, x2) <- my_clauseSolveMy_clauseSolveOOO;
                                            return (x0, x1, x2)}]