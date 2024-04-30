module Vanilla_clean where

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
my_clauseSolveMy_clauseSolveIII x0 x1 x2 = msum [do {let {x9 = O};
                                                     let {x8 = S x9};
                                                     let {x7 = S x8};
                                                     let {x12 = O};
                                                     let {x11 = S x12};
                                                     let {x16 = O};
                                                     let {x15 = S x16};
                                                     let {x14 = S x15};
                                                     let {x19 = O};
                                                     let {x18 = S x19};
                                                     let {x20 = Nil};
                                                     let {x17 = Cons x18 x20};
                                                     let {x13 = Cons x14 x17};
                                                     let {x10 = Cons x11 x13};
                                                     let {x6 = Cons x7 x10};
                                                     my_clauseSolveIII x1 x2 x6;
                                                     guard (x0 == Nil);
                                                     return ()},
                                                 do {let {x25 = O};
                                                     let {x24 = S x25};
                                                     let {x23 = S x24};
                                                     let {x28 = O};
                                                     let {x27 = S x28};
                                                     let {x32 = O};
                                                     let {x31 = S x32};
                                                     let {x30 = S x31};
                                                     let {x35 = O};
                                                     let {x34 = S x35};
                                                     let {x36 = Nil};
                                                     let {x33 = Cons x34 x36};
                                                     let {x29 = Cons x30 x33};
                                                     let {x26 = Cons x27 x29};
                                                     let {x22 = Cons x23 x26};
                                                     (x3, x4) <- case x0 of
                                                                 {Cons y3 y4 -> return (y3, y4);
                                                                  _ -> mzero};
                                                     x5 <- my_clauseSolveIIO x4 x1;
                                                     let {x21 = Cons x3 x5};
                                                     my_clauseSolveIII x21 x2 x22;
                                                     return ()},
                                                 do {my_clauseSolveMy_clauseSolveIII x0 x1 x2;
                                                     return ()}]
my_clauseSolveIII x0 x1 x2 = msum [do {guard (x1 == x2);
                                       guard (x0 == Nil);
                                       return ()},
                                   do {(x3, x4) <- case x2 of
                                                   {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                       (x37, x5) <- case x0 of
                                                    {Cons y37 y5 -> return (y37, y5); _ -> mzero};
                                       guard (x37 == x3);
                                       my_clauseSolveIII x5 x1 x4;
                                       return ()},
                                   do {my_clauseSolveIII x0 x1 x2; return ()}]
my_clauseSolveIIO x0 x1 = msum [do {guard (x0 == Nil);
                                    let {x2 = x1};
                                    return x2},
                                do {(x37, x5) <- case x0 of
                                                 {Cons y37 y5 -> return (y37, y5); _ -> mzero};
                                    let {x3 = x37};
                                    x4 <- my_clauseSolveIIO x5 x1;
                                    let {x2 = Cons x3 x4};
                                    return x2},
                                do {x2 <- my_clauseSolveIIO x0 x1; return x2}]
solveIIO x0 x1 = msum [do {x2 <- my_clauseSolveMy_clauseSolveIIO x0 x1;
                           return x2},
                       do {x2 <- solveIIO x0 x1; return x2}]
my_clauseSolveMy_clauseSolveIIO x0 x1 = msum [do {let {x9 = O};
                                                  let {x8 = S x9};
                                                  let {x7 = S x8};
                                                  let {x12 = O};
                                                  let {x11 = S x12};
                                                  let {x16 = O};
                                                  let {x15 = S x16};
                                                  let {x14 = S x15};
                                                  let {x19 = O};
                                                  let {x18 = S x19};
                                                  let {x20 = Nil};
                                                  let {x17 = Cons x18 x20};
                                                  let {x13 = Cons x14 x17};
                                                  let {x10 = Cons x11 x13};
                                                  let {x6 = Cons x7 x10};
                                                  guard (x0 == Nil);
                                                  x2 <- my_clauseSolveIOI x1 x6;
                                                  return x2},
                                              do {let {x25 = O};
                                                  let {x24 = S x25};
                                                  let {x23 = S x24};
                                                  let {x28 = O};
                                                  let {x27 = S x28};
                                                  let {x32 = O};
                                                  let {x31 = S x32};
                                                  let {x30 = S x31};
                                                  let {x35 = O};
                                                  let {x34 = S x35};
                                                  let {x36 = Nil};
                                                  let {x33 = Cons x34 x36};
                                                  let {x29 = Cons x30 x33};
                                                  let {x26 = Cons x27 x29};
                                                  let {x22 = Cons x23 x26};
                                                  (x3, x4) <- case x0 of
                                                              {Cons y3 y4 -> return (y3, y4);
                                                               _ -> mzero};
                                                  x5 <- my_clauseSolveIIO x4 x1;
                                                  let {x21 = Cons x3 x5};
                                                  x2 <- my_clauseSolveIOI x21 x22;
                                                  return x2},
                                              do {x2 <- my_clauseSolveMy_clauseSolveIIO x0 x1;
                                                  return x2}]
my_clauseSolveIOI x0 x2 = msum [do {guard (x0 == Nil);
                                    let {x1 = x2};
                                    return x1},
                                do {(x3, x4) <- case x2 of
                                                {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                    (x37, x5) <- case x0 of
                                                 {Cons y37 y5 -> return (y37, y5); _ -> mzero};
                                    guard (x37 == x3);
                                    x1 <- my_clauseSolveIOI x5 x4;
                                    return x1},
                                do {x1 <- my_clauseSolveIOI x0 x2; return x1}]
solveIOI x0 x2 = msum [do {x1 <- my_clauseSolveMy_clauseSolveIOI x0 x2;
                           return x1},
                       do {x1 <- solveIOI x0 x2; return x1}]
my_clauseSolveMy_clauseSolveIOI x0 x2 = msum [do {let {x9 = O};
                                                  let {x8 = S x9};
                                                  let {x7 = S x8};
                                                  let {x12 = O};
                                                  let {x11 = S x12};
                                                  let {x16 = O};
                                                  let {x15 = S x16};
                                                  let {x14 = S x15};
                                                  let {x19 = O};
                                                  let {x18 = S x19};
                                                  let {x20 = Nil};
                                                  let {x17 = Cons x18 x20};
                                                  let {x13 = Cons x14 x17};
                                                  let {x10 = Cons x11 x13};
                                                  let {x6 = Cons x7 x10};
                                                  guard (x0 == Nil);
                                                  x1 <- my_clauseSolveOII x2 x6;
                                                  return x1},
                                              do {let {x25 = O};
                                                  let {x24 = S x25};
                                                  let {x23 = S x24};
                                                  let {x28 = O};
                                                  let {x27 = S x28};
                                                  let {x32 = O};
                                                  let {x31 = S x32};
                                                  let {x30 = S x31};
                                                  let {x35 = O};
                                                  let {x34 = S x35};
                                                  let {x36 = Nil};
                                                  let {x33 = Cons x34 x36};
                                                  let {x29 = Cons x30 x33};
                                                  let {x26 = Cons x27 x29};
                                                  let {x22 = Cons x23 x26};
                                                  (x3, x4) <- case x0 of
                                                              {Cons y3 y4 -> return (y3, y4);
                                                               _ -> mzero};
                                                  x21 <- my_clauseSolveOII x2 x22;
                                                  x5 <- case x21 of
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
                                    let {x37 = x3};
                                    x5 <- my_clauseSolveOII x1 x4;
                                    let {x0 = Cons x37 x5};
                                    return x0},
                                do {x0 <- my_clauseSolveOII x1 x2; return x0}]
solveIOO x0 gen_my_clauseSolveIOO_x2 = msum [do {(x1,
                                                  x2) <- my_clauseSolveMy_clauseSolveIOO x0 gen_my_clauseSolveIOO_x2;
                                                 return (x1, x2)},
                                             do {(x1, x2) <- solveIOO x0 gen_my_clauseSolveIOO_x2;
                                                 return (x1, x2)}]
my_clauseSolveMy_clauseSolveIOO x0 gen_my_clauseSolveIOO_x2 = msum [do {let {x9 = O};
                                                                        let {x8 = S x9};
                                                                        let {x7 = S x8};
                                                                        let {x12 = O};
                                                                        let {x11 = S x12};
                                                                        let {x16 = O};
                                                                        let {x15 = S x16};
                                                                        let {x14 = S x15};
                                                                        let {x19 = O};
                                                                        let {x18 = S x19};
                                                                        let {x20 = Nil};
                                                                        let {x17 = Cons x18 x20};
                                                                        let {x13 = Cons x14 x17};
                                                                        let {x10 = Cons x11 x13};
                                                                        let {x6 = Cons x7 x10};
                                                                        guard (x0 == Nil);
                                                                        (x1,
                                                                         x2) <- my_clauseSolveOOI x6;
                                                                        return (x1, x2)},
                                                                    do {let {x25 = O};
                                                                        let {x24 = S x25};
                                                                        let {x23 = S x24};
                                                                        let {x28 = O};
                                                                        let {x27 = S x28};
                                                                        let {x32 = O};
                                                                        let {x31 = S x32};
                                                                        let {x30 = S x31};
                                                                        let {x35 = O};
                                                                        let {x34 = S x35};
                                                                        let {x36 = Nil};
                                                                        let {x33 = Cons x34 x36};
                                                                        let {x29 = Cons x30 x33};
                                                                        let {x26 = Cons x27 x29};
                                                                        let {x22 = Cons x23 x26};
                                                                        (x3, x4) <- case x0 of
                                                                                    {Cons y3
                                                                                          y4 -> return (y3,
                                                                                                        y4);
                                                                                     _ -> mzero};
                                                                        (x1,
                                                                         x5) <- my_clauseSolveIOO x4 gen_my_clauseSolveIOO_x2;
                                                                        let {x21 = Cons x3 x5};
                                                                        x2 <- my_clauseSolveIOI x21 x22;
                                                                        return (x1, x2)},
                                                                    do {(x1,
                                                                         x2) <- my_clauseSolveMy_clauseSolveIOO x0 gen_my_clauseSolveIOO_x2;
                                                                        return (x1, x2)}]
my_clauseSolveIOO x0 gen_my_clauseSolveIOO_x2 = msum [do {guard (x0 == Nil);
                                                          (x1,
                                                           x2) <- do {x2 <- gen_my_clauseSolveIOO_x2;
                                                                      return (x2, x2)};
                                                          return (x1, x2)},
                                                      do {(x37, x5) <- case x0 of
                                                                       {Cons y37 y5 -> return (y37,
                                                                                               y5);
                                                                        _ -> mzero};
                                                          let {x3 = x37};
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
                                 let {x37 = x3};
                                 (x5, x1) <- my_clauseSolveOOI x4;
                                 let {x0 = Cons x37 x5};
                                 return (x0, x1)},
                             do {(x0, x1) <- my_clauseSolveOOI x2; return (x0, x1)}]
solveOII x1 x2 = msum [do {x0 <- my_clauseSolveMy_clauseSolveOII x1 x2;
                           return x0},
                       do {x0 <- solveOII x1 x2; return x0}]
my_clauseSolveMy_clauseSolveOII x1 x2 = msum [do {let {x0 = Nil};
                                                  let {x9 = O};
                                                  let {x8 = S x9};
                                                  let {x7 = S x8};
                                                  let {x12 = O};
                                                  let {x11 = S x12};
                                                  let {x16 = O};
                                                  let {x15 = S x16};
                                                  let {x14 = S x15};
                                                  let {x19 = O};
                                                  let {x18 = S x19};
                                                  let {x20 = Nil};
                                                  let {x17 = Cons x18 x20};
                                                  let {x13 = Cons x14 x17};
                                                  let {x10 = Cons x11 x13};
                                                  let {x6 = Cons x7 x10};
                                                  my_clauseSolveIII x1 x2 x6;
                                                  return x0},
                                              do {let {x25 = O};
                                                  let {x24 = S x25};
                                                  let {x23 = S x24};
                                                  let {x28 = O};
                                                  let {x27 = S x28};
                                                  let {x32 = O};
                                                  let {x31 = S x32};
                                                  let {x30 = S x31};
                                                  let {x35 = O};
                                                  let {x34 = S x35};
                                                  let {x36 = Nil};
                                                  let {x33 = Cons x34 x36};
                                                  let {x29 = Cons x30 x33};
                                                  let {x26 = Cons x27 x29};
                                                  let {x22 = Cons x23 x26};
                                                  x21 <- my_clauseSolveOII x2 x22;
                                                  (x3, x5) <- case x21 of
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
                                                                        let {x9 = O};
                                                                        let {x8 = S x9};
                                                                        let {x7 = S x8};
                                                                        let {x12 = O};
                                                                        let {x11 = S x12};
                                                                        let {x16 = O};
                                                                        let {x15 = S x16};
                                                                        let {x14 = S x15};
                                                                        let {x19 = O};
                                                                        let {x18 = S x19};
                                                                        let {x20 = Nil};
                                                                        let {x17 = Cons x18 x20};
                                                                        let {x13 = Cons x14 x17};
                                                                        let {x10 = Cons x11 x13};
                                                                        let {x6 = Cons x7 x10};
                                                                        x2 <- my_clauseSolveIOI x1 x6;
                                                                        return (x0, x2)},
                                                                    do {let {x25 = O};
                                                                        let {x24 = S x25};
                                                                        let {x23 = S x24};
                                                                        let {x28 = O};
                                                                        let {x27 = S x28};
                                                                        let {x32 = O};
                                                                        let {x31 = S x32};
                                                                        let {x30 = S x31};
                                                                        let {x35 = O};
                                                                        let {x34 = S x35};
                                                                        let {x36 = Nil};
                                                                        let {x33 = Cons x34 x36};
                                                                        let {x29 = Cons x30 x33};
                                                                        let {x26 = Cons x27 x29};
                                                                        let {x22 = Cons x23 x26};
                                                                        (x4,
                                                                         x5) <- my_clauseSolveOIO x1 gen_my_clauseSolveOIO_x3;
                                                                        (x21,
                                                                         x2) <- my_clauseSolveOOI x22;
                                                                        x3 <- case x21 of
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
                                                          (x37,
                                                           x3) <- do {x3 <- gen_my_clauseSolveOIO_x3;
                                                                      return (x3, x3)};
                                                          let {x2 = Cons x3 x4};
                                                          let {x0 = Cons x37 x5};
                                                          return (x0, x2)},
                                                      do {(x0,
                                                           x2) <- my_clauseSolveOIO x1 gen_my_clauseSolveOIO_x3;
                                                          return (x0, x2)}]
solveOOI x2 = msum [do {(x0,
                         x1) <- my_clauseSolveMy_clauseSolveOOI x2;
                        return (x0, x1)},
                    do {(x0, x1) <- solveOOI x2; return (x0, x1)}]
my_clauseSolveMy_clauseSolveOOI x2 = msum [do {let {x0 = Nil};
                                               let {x9 = O};
                                               let {x8 = S x9};
                                               let {x7 = S x8};
                                               let {x12 = O};
                                               let {x11 = S x12};
                                               let {x16 = O};
                                               let {x15 = S x16};
                                               let {x14 = S x15};
                                               let {x19 = O};
                                               let {x18 = S x19};
                                               let {x20 = Nil};
                                               let {x17 = Cons x18 x20};
                                               let {x13 = Cons x14 x17};
                                               let {x10 = Cons x11 x13};
                                               let {x6 = Cons x7 x10};
                                               x1 <- my_clauseSolveOII x2 x6;
                                               return (x0, x1)},
                                           do {let {x25 = O};
                                               let {x24 = S x25};
                                               let {x23 = S x24};
                                               let {x28 = O};
                                               let {x27 = S x28};
                                               let {x32 = O};
                                               let {x31 = S x32};
                                               let {x30 = S x31};
                                               let {x35 = O};
                                               let {x34 = S x35};
                                               let {x36 = Nil};
                                               let {x33 = Cons x34 x36};
                                               let {x29 = Cons x30 x33};
                                               let {x26 = Cons x27 x29};
                                               let {x22 = Cons x23 x26};
                                               x21 <- my_clauseSolveOII x2 x22;
                                               (x3, x5) <- case x21 of
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
                                            let {x9 = O};
                                            let {x8 = S x9};
                                            let {x7 = S x8};
                                            let {x12 = O};
                                            let {x11 = S x12};
                                            let {x16 = O};
                                            let {x15 = S x16};
                                            let {x14 = S x15};
                                            let {x19 = O};
                                            let {x18 = S x19};
                                            let {x20 = Nil};
                                            let {x17 = Cons x18 x20};
                                            let {x13 = Cons x14 x17};
                                            let {x10 = Cons x11 x13};
                                            let {x6 = Cons x7 x10};
                                            (x1, x2) <- my_clauseSolveOOI x6;
                                            return (x0, x1, x2)},
                                        do {let {x25 = O};
                                            let {x24 = S x25};
                                            let {x23 = S x24};
                                            let {x28 = O};
                                            let {x27 = S x28};
                                            let {x32 = O};
                                            let {x31 = S x32};
                                            let {x30 = S x31};
                                            let {x35 = O};
                                            let {x34 = S x35};
                                            let {x36 = Nil};
                                            let {x33 = Cons x34 x36};
                                            let {x29 = Cons x30 x33};
                                            let {x26 = Cons x27 x29};
                                            let {x22 = Cons x23 x26};
                                            (x21, x2) <- my_clauseSolveOOI x22;
                                            (x3, x5) <- case x21 of
                                                        {Cons y3 y5 -> return (y3, y5); _ -> mzero};
                                            (x4, x1) <- my_clauseSolveOOI x5;
                                            let {x0 = Cons x3 x4};
                                            return (x0, x1, x2)},
                                        do {(x0, x1, x2) <- my_clauseSolveMy_clauseSolveOOO;
                                            return (x0, x1, x2)}]