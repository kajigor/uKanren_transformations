module Vanilla_simple_clean where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
solveIII x0 x1 x2 = msum [do {my_clauseSolveII x2 x1;
                              guard (x0 == Nil);
                              return ()},
                          do {let {x6 = O};
                              let {x5 = S x6};
                              let {x4 = S x5};
                              (x7, x3) <- case x0 of
                                          {Cons y7 y3 -> return (y7, y3); _ -> mzero};
                              guard (x7 == x4);
                              solve_atomMy_clauseSolveIII x3 x1 x2;
                              return ()}]
my_clauseSolveII x0 x1 = msum [do {let {x10 = O};
                                   let {x9 = S x10};
                                   let {x8 = S x9};
                                   let {x13 = O};
                                   let {x12 = S x13};
                                   let {x17 = O};
                                   let {x16 = S x17};
                                   let {x15 = S x16};
                                   let {x20 = O};
                                   let {x19 = S x20};
                                   let {x21 = Nil};
                                   let {x18 = Cons x19 x21};
                                   let {x14 = Cons x15 x18};
                                   let {x11 = Cons x12 x14};
                                   guard (x1 == Nil);
                                   (x22, x23) <- case x0 of
                                                 {Cons y22 y23 -> return (y22, y23); _ -> mzero};
                                   guard (x22 == x8);
                                   guard (x23 == x11);
                                   return ()},
                               do {let {x26 = O};
                                   let {x25 = S x26};
                                   let {x24 = S x25};
                                   (x27, x2) <- case x1 of
                                                {Cons y27 y2 -> return (y27, y2); _ -> mzero};
                                   guard (x27 == x24);
                                   _my_clauseSolveII x2 x0;
                                   return ()}]
_my_clauseSolveII x0 x1 = msum [do {let {x29 = O};
                                    let {x28 = S x29};
                                    let {x33 = O};
                                    let {x32 = S x33};
                                    let {x31 = S x32};
                                    let {x36 = O};
                                    let {x35 = S x36};
                                    let {x37 = Nil};
                                    let {x34 = Cons x35 x37};
                                    let {x30 = Cons x31 x34};
                                    (x38, x39) <- case x1 of
                                                  {Cons y38 y39 -> return (y38, y39); _ -> mzero};
                                    guard (x38 == x28);
                                    guard (x39 == x30);
                                    guard (x0 == Nil);
                                    return ()},
                                do {let {x41 = O};
                                    let {x40 = S x41};
                                    (x42, x2) <- case x0 of
                                                 {Cons y42 y2 -> return (y42, y2); _ -> mzero};
                                    guard (x42 == x40);
                                    __my_clauseSolveII x2 x1;
                                    return ()}]
__my_clauseSolveII x0 x1 = msum [do {let {x45 = O};
                                     let {x44 = S x45};
                                     let {x43 = S x44};
                                     let {x48 = O};
                                     let {x47 = S x48};
                                     let {x49 = Nil};
                                     let {x46 = Cons x47 x49};
                                     (x50, x51) <- case x1 of
                                                   {Cons y50 y51 -> return (y50, y51); _ -> mzero};
                                     guard (x50 == x43);
                                     guard (x51 == x46);
                                     guard (x0 == Nil);
                                     return ()},
                                 do {let {x54 = O};
                                     let {x53 = S x54};
                                     let {x52 = S x53};
                                     (x55, x2) <- case x0 of
                                                  {Cons y55 y2 -> return (y55, y2); _ -> mzero};
                                     guard (x55 == x52);
                                     ___my_clauseSolveII x2 x1;
                                     return ()}]
___my_clauseSolveII x0 x1 = msum [do {let {x57 = O};
                                      let {x56 = S x57};
                                      let {x58 = Nil};
                                      (x59, x60) <- case x1 of
                                                    {Cons y59 y60 -> return (y59, y60); _ -> mzero};
                                      guard (x59 == x56);
                                      guard (x60 == x58);
                                      guard (x0 == Nil);
                                      return ()},
                                  do {let {x62 = O};
                                      let {x61 = S x62};
                                      let {x63 = Nil};
                                      guard (x1 == Nil);
                                      (x64, x65) <- case x0 of
                                                    {Cons y64 y65 -> return (y64, y65); _ -> mzero};
                                      guard (x64 == x61);
                                      guard (x65 == x63);
                                      return ()}]
solveIIO x0 x1 = msum [do {guard (x0 == Nil);
                           x2 <- my_clauseSolveOI x1;
                           return x2},
                       do {let {x6 = O};
                           let {x5 = S x6};
                           let {x4 = S x5};
                           (x7, x3) <- case x0 of
                                       {Cons y7 y3 -> return (y7, y3); _ -> mzero};
                           guard (x7 == x4);
                           x2 <- solve_atomMy_clauseSolveIIO x3 x1;
                           return x2}]
my_clauseSolveOI x1 = msum [do {let {x10 = O};
                                let {x9 = S x10};
                                let {x8 = S x9};
                                let {x13 = O};
                                let {x12 = S x13};
                                let {x17 = O};
                                let {x16 = S x17};
                                let {x15 = S x16};
                                let {x20 = O};
                                let {x19 = S x20};
                                let {x21 = Nil};
                                let {x18 = Cons x19 x21};
                                let {x14 = Cons x15 x18};
                                let {x11 = Cons x12 x14};
                                guard (x1 == Nil);
                                let {x22 = x8};
                                let {x23 = x11};
                                let {x0 = Cons x22 x23};
                                return x0},
                            do {let {x26 = O};
                                let {x25 = S x26};
                                let {x24 = S x25};
                                (x27, x2) <- case x1 of
                                             {Cons y27 y2 -> return (y27, y2); _ -> mzero};
                                guard (x27 == x24);
                                x0 <- _my_clauseSolveIO x2;
                                return x0}]
_my_clauseSolveIO x0 = msum [do {let {x29 = O};
                                 let {x28 = S x29};
                                 let {x33 = O};
                                 let {x32 = S x33};
                                 let {x31 = S x32};
                                 let {x36 = O};
                                 let {x35 = S x36};
                                 let {x37 = Nil};
                                 let {x34 = Cons x35 x37};
                                 let {x30 = Cons x31 x34};
                                 guard (x0 == Nil);
                                 let {x38 = x28};
                                 let {x39 = x30};
                                 let {x1 = Cons x38 x39};
                                 return x1},
                             do {let {x41 = O};
                                 let {x40 = S x41};
                                 (x42, x2) <- case x0 of
                                              {Cons y42 y2 -> return (y42, y2); _ -> mzero};
                                 guard (x42 == x40);
                                 x1 <- __my_clauseSolveIO x2;
                                 return x1}]
__my_clauseSolveIO x0 = msum [do {let {x45 = O};
                                  let {x44 = S x45};
                                  let {x43 = S x44};
                                  let {x48 = O};
                                  let {x47 = S x48};
                                  let {x49 = Nil};
                                  let {x46 = Cons x47 x49};
                                  guard (x0 == Nil);
                                  let {x50 = x43};
                                  let {x51 = x46};
                                  let {x1 = Cons x50 x51};
                                  return x1},
                              do {let {x54 = O};
                                  let {x53 = S x54};
                                  let {x52 = S x53};
                                  (x55, x2) <- case x0 of
                                               {Cons y55 y2 -> return (y55, y2); _ -> mzero};
                                  guard (x55 == x52);
                                  x1 <- ___my_clauseSolveIO x2;
                                  return x1}]
___my_clauseSolveIO x0 = msum [do {let {x57 = O};
                                   let {x56 = S x57};
                                   let {x58 = Nil};
                                   guard (x0 == Nil);
                                   let {x59 = x56};
                                   let {x60 = x58};
                                   let {x1 = Cons x59 x60};
                                   return x1},
                               do {let {x1 = Nil};
                                   let {x62 = O};
                                   let {x61 = S x62};
                                   let {x63 = Nil};
                                   (x64, x65) <- case x0 of
                                                 {Cons y64 y65 -> return (y64, y65); _ -> mzero};
                                   guard (x64 == x61);
                                   guard (x65 == x63);
                                   return x1}]
solveIOI x0 x2 = msum [do {guard (x0 == Nil);
                           x1 <- my_clauseSolveIO x2;
                           return x1},
                       do {let {x6 = O};
                           let {x5 = S x6};
                           let {x4 = S x5};
                           (x7, x3) <- case x0 of
                                       {Cons y7 y3 -> return (y7, y3); _ -> mzero};
                           guard (x7 == x4);
                           x1 <- solve_atomMy_clauseSolveIOI x3 x2;
                           return x1}]
my_clauseSolveIO x0 = msum [do {let {x1 = Nil};
                                let {x10 = O};
                                let {x9 = S x10};
                                let {x8 = S x9};
                                let {x13 = O};
                                let {x12 = S x13};
                                let {x17 = O};
                                let {x16 = S x17};
                                let {x15 = S x16};
                                let {x20 = O};
                                let {x19 = S x20};
                                let {x21 = Nil};
                                let {x18 = Cons x19 x21};
                                let {x14 = Cons x15 x18};
                                let {x11 = Cons x12 x14};
                                (x22, x23) <- case x0 of
                                              {Cons y22 y23 -> return (y22, y23); _ -> mzero};
                                guard (x22 == x8);
                                guard (x23 == x11);
                                return x1},
                            do {let {x26 = O};
                                let {x25 = S x26};
                                let {x24 = S x25};
                                let {x27 = x24};
                                x2 <- _my_clauseSolveOI x0;
                                let {x1 = Cons x27 x2};
                                return x1}]
_my_clauseSolveOI x1 = msum [do {let {x29 = O};
                                 let {x28 = S x29};
                                 let {x33 = O};
                                 let {x32 = S x33};
                                 let {x31 = S x32};
                                 let {x36 = O};
                                 let {x35 = S x36};
                                 let {x37 = Nil};
                                 let {x34 = Cons x35 x37};
                                 let {x30 = Cons x31 x34};
                                 let {x0 = Nil};
                                 (x38, x39) <- case x1 of
                                               {Cons y38 y39 -> return (y38, y39); _ -> mzero};
                                 guard (x38 == x28);
                                 guard (x39 == x30);
                                 return x0},
                             do {let {x41 = O};
                                 let {x40 = S x41};
                                 let {x42 = x40};
                                 x2 <- __my_clauseSolveOI x1;
                                 let {x0 = Cons x42 x2};
                                 return x0}]
__my_clauseSolveOI x1 = msum [do {let {x45 = O};
                                  let {x44 = S x45};
                                  let {x43 = S x44};
                                  let {x48 = O};
                                  let {x47 = S x48};
                                  let {x49 = Nil};
                                  let {x46 = Cons x47 x49};
                                  let {x0 = Nil};
                                  (x50, x51) <- case x1 of
                                                {Cons y50 y51 -> return (y50, y51); _ -> mzero};
                                  guard (x50 == x43);
                                  guard (x51 == x46);
                                  return x0},
                              do {let {x54 = O};
                                  let {x53 = S x54};
                                  let {x52 = S x53};
                                  let {x55 = x52};
                                  x2 <- ___my_clauseSolveOI x1;
                                  let {x0 = Cons x55 x2};
                                  return x0}]
___my_clauseSolveOI x1 = msum [do {let {x57 = O};
                                   let {x56 = S x57};
                                   let {x58 = Nil};
                                   let {x0 = Nil};
                                   (x59, x60) <- case x1 of
                                                 {Cons y59 y60 -> return (y59, y60); _ -> mzero};
                                   guard (x59 == x56);
                                   guard (x60 == x58);
                                   return x0},
                               do {let {x62 = O};
                                   let {x61 = S x62};
                                   let {x63 = Nil};
                                   guard (x1 == Nil);
                                   let {x64 = x61};
                                   let {x65 = x63};
                                   let {x0 = Cons x64 x65};
                                   return x0}]
solveIOO x0 gen__my_clauseSolveOO_x2 gen_my_clauseSolveOO_x2 = msum [do {guard (x0 == Nil);
                                                                         (x2,
                                                                          x1) <- my_clauseSolveOO gen_my_clauseSolveOO_x2;
                                                                         return (x1, x2)},
                                                                     do {let {x6 = O};
                                                                         let {x5 = S x6};
                                                                         let {x4 = S x5};
                                                                         (x7, x3) <- case x0 of
                                                                                     {Cons y7
                                                                                           y3 -> return (y7,
                                                                                                         y3);
                                                                                      _ -> mzero};
                                                                         guard (x7 == x4);
                                                                         (x1,
                                                                          x2) <- solve_atomMy_clauseSolveIOO x3 gen__my_clauseSolveOO_x2;
                                                                         return (x1, x2)}]
my_clauseSolveOO gen_my_clauseSolveOO_x2 = msum [do {let {x1 = Nil};
                                                     let {x10 = O};
                                                     let {x9 = S x10};
                                                     let {x8 = S x9};
                                                     let {x13 = O};
                                                     let {x12 = S x13};
                                                     let {x17 = O};
                                                     let {x16 = S x17};
                                                     let {x15 = S x16};
                                                     let {x20 = O};
                                                     let {x19 = S x20};
                                                     let {x21 = Nil};
                                                     let {x18 = Cons x19 x21};
                                                     let {x14 = Cons x15 x18};
                                                     let {x11 = Cons x12 x14};
                                                     let {x22 = x8};
                                                     let {x23 = x11};
                                                     let {x0 = Cons x22 x23};
                                                     return (x0, x1)},
                                                 do {let {x26 = O};
                                                     let {x25 = S x26};
                                                     let {x24 = S x25};
                                                     let {x27 = x24};
                                                     (x1, x2) <- do {x2 <- gen_my_clauseSolveOO_x2;
                                                                     let {x1 = Cons x27 x2};
                                                                     return (x1, x2)};
                                                     x0 <- _my_clauseSolveIO x2;
                                                     return (x0, x1)}]
solveOII x1 x2 = msum [do {my_clauseSolveII x2 x1;
                           let {x0 = Nil};
                           return x0},
                       do {let {x6 = O};
                           let {x5 = S x6};
                           let {x4 = S x5};
                           let {x7 = x4};
                           x3 <- solve_atomMy_clauseSolveOII x1 x2;
                           let {x0 = Cons x7 x3};
                           return x0}]
solveOIO x1 = msum [do {let {x0 = Nil};
                        x2 <- my_clauseSolveOI x1;
                        return (x0, x2)},
                    do {let {x6 = O};
                        let {x5 = S x6};
                        let {x4 = S x5};
                        let {x7 = x4};
                        (x3, x2) <- solve_atomMy_clauseSolveOIO x1;
                        let {x0 = Cons x7 x3};
                        return (x0, x2)}]
solveOOI x2 = msum [do {let {x0 = Nil};
                        x1 <- my_clauseSolveIO x2;
                        return (x0, x1)},
                    do {let {x6 = O};
                        let {x5 = S x6};
                        let {x4 = S x5};
                        let {x7 = x4};
                        (x3, x1) <- solve_atomMy_clauseSolveOOI x2;
                        let {x0 = Cons x7 x3};
                        return (x0, x1)}]
solveOOO gen__my_clauseSolveOO_x2 gen_my_clauseSolveOO_x2 gen_solveOOO_x3 = msum [do {let {x0 = Nil};
                                                                                      (x2,
                                                                                       x1) <- my_clauseSolveOO gen_my_clauseSolveOO_x2;
                                                                                      return (x0,
                                                                                              x1,
                                                                                              x2)},
                                                                                  do {let {x6 = O};
                                                                                      let {x5 = S x6};
                                                                                      let {x4 = S x5};
                                                                                      let {x7 = x4};
                                                                                      (x0,
                                                                                       x3) <- do {x3 <- gen_solveOOO_x3;
                                                                                                  let {x0 = Cons x7 x3};
                                                                                                  return (x0,
                                                                                                          x3)};
                                                                                      (x1,
                                                                                       x2) <- solve_atomMy_clauseSolveIOO x3 gen__my_clauseSolveOO_x2;
                                                                                      return (x0,
                                                                                              x1,
                                                                                              x2)}]
solve_atomMy_clauseSolveIII x0 x1 x2 = msum [do {_my_clauseSolveII x1 x2;
                                                 guard (x0 == Nil);
                                                 return ()},
                                             do {let {x67 = O};
                                                 let {x66 = S x67};
                                                 (x68, x3) <- case x0 of
                                                              {Cons y68 y3 -> return (y68, y3);
                                                               _ -> mzero};
                                                 guard (x68 == x66);
                                                 _solve_atomMy_clauseSolveIII x3 x1 x2;
                                                 return ()}]
_solve_atomMy_clauseSolveIII x0 x1 x2 = msum [do {__my_clauseSolveII x1 x2;
                                                  guard (x0 == Nil);
                                                  return ()},
                                              do {let {x71 = O};
                                                  let {x70 = S x71};
                                                  let {x69 = S x70};
                                                  (x72, x3) <- case x0 of
                                                               {Cons y72 y3 -> return (y72, y3);
                                                                _ -> mzero};
                                                  guard (x72 == x69);
                                                  __solve_atomMy_clauseSolveIII x3 x1 x2;
                                                  return ()}]
__solve_atomMy_clauseSolveIII x0 x1 x2 = msum [do {___my_clauseSolveII x1 x2;
                                                   guard (x0 == Nil);
                                                   return ()},
                                               do {let {x74 = O};
                                                   let {x73 = S x74};
                                                   guard (x2 == Nil);
                                                   (x75, x3) <- case x0 of
                                                                {Cons y75 y3 -> return (y75, y3);
                                                                 _ -> mzero};
                                                   guard (x75 == x73);
                                                   solve_atomII x3 x1;
                                                   return ()}]
solve_atomII x0 x1 = msum [do {guard (x1 == Nil);
                               guard (x0 == Nil);
                               return ()}]
solve_atomMy_clauseSolveIIO x0 x1 = msum [do {guard (x0 == Nil);
                                              x2 <- _my_clauseSolveIO x1;
                                              return x2},
                                          do {let {x67 = O};
                                              let {x66 = S x67};
                                              (x68, x3) <- case x0 of
                                                           {Cons y68 y3 -> return (y68, y3);
                                                            _ -> mzero};
                                              guard (x68 == x66);
                                              x2 <- _solve_atomMy_clauseSolveIIO x3 x1;
                                              return x2}]
_solve_atomMy_clauseSolveIIO x0 x1 = msum [do {guard (x0 == Nil);
                                               x2 <- __my_clauseSolveIO x1;
                                               return x2},
                                           do {let {x71 = O};
                                               let {x70 = S x71};
                                               let {x69 = S x70};
                                               (x72, x3) <- case x0 of
                                                            {Cons y72 y3 -> return (y72, y3);
                                                             _ -> mzero};
                                               guard (x72 == x69);
                                               x2 <- __solve_atomMy_clauseSolveIIO x3 x1;
                                               return x2}]
__solve_atomMy_clauseSolveIIO x0 x1 = msum [do {guard (x0 == Nil);
                                                x2 <- ___my_clauseSolveIO x1;
                                                return x2},
                                            do {let {x2 = Nil};
                                                let {x74 = O};
                                                let {x73 = S x74};
                                                (x75, x3) <- case x0 of
                                                             {Cons y75 y3 -> return (y75, y3);
                                                              _ -> mzero};
                                                guard (x75 == x73);
                                                solve_atomII x3 x1;
                                                return x2}]
solve_atomMy_clauseSolveIOI x0 x2 = msum [do {guard (x0 == Nil);
                                              x1 <- _my_clauseSolveOI x2;
                                              return x1},
                                          do {let {x67 = O};
                                              let {x66 = S x67};
                                              (x68, x3) <- case x0 of
                                                           {Cons y68 y3 -> return (y68, y3);
                                                            _ -> mzero};
                                              guard (x68 == x66);
                                              x1 <- _solve_atomMy_clauseSolveIOI x3 x2;
                                              return x1}]
_solve_atomMy_clauseSolveIOI x0 x2 = msum [do {guard (x0 == Nil);
                                               x1 <- __my_clauseSolveOI x2;
                                               return x1},
                                           do {let {x71 = O};
                                               let {x70 = S x71};
                                               let {x69 = S x70};
                                               (x72, x3) <- case x0 of
                                                            {Cons y72 y3 -> return (y72, y3);
                                                             _ -> mzero};
                                               guard (x72 == x69);
                                               x1 <- __solve_atomMy_clauseSolveIOI x3 x2;
                                               return x1}]
__solve_atomMy_clauseSolveIOI x0 x2 = msum [do {guard (x0 == Nil);
                                                x1 <- ___my_clauseSolveOI x2;
                                                return x1},
                                            do {let {x74 = O};
                                                let {x73 = S x74};
                                                guard (x2 == Nil);
                                                (x75, x3) <- case x0 of
                                                             {Cons y75 y3 -> return (y75, y3);
                                                              _ -> mzero};
                                                guard (x75 == x73);
                                                x1 <- solve_atomIO x3;
                                                return x1}]
solve_atomIO x0 = msum [do {let {x1 = Nil};
                            guard (x0 == Nil);
                            return x1}]
solve_atomMy_clauseSolveIOO x0 gen__my_clauseSolveOO_x2 = msum [do {guard (x0 == Nil);
                                                                    (x1,
                                                                     x2) <- _my_clauseSolveOO gen__my_clauseSolveOO_x2;
                                                                    return (x1, x2)},
                                                                do {let {x67 = O};
                                                                    let {x66 = S x67};
                                                                    (x68, x3) <- case x0 of
                                                                                 {Cons y68
                                                                                       y3 -> return (y68,
                                                                                                     y3);
                                                                                  _ -> mzero};
                                                                    guard (x68 == x66);
                                                                    (x1,
                                                                     x2) <- _solve_atomMy_clauseSolveIOO x3;
                                                                    return (x1, x2)}]
_my_clauseSolveOO gen__my_clauseSolveOO_x2 = msum [do {let {x29 = O};
                                                       let {x28 = S x29};
                                                       let {x33 = O};
                                                       let {x32 = S x33};
                                                       let {x31 = S x32};
                                                       let {x36 = O};
                                                       let {x35 = S x36};
                                                       let {x37 = Nil};
                                                       let {x34 = Cons x35 x37};
                                                       let {x30 = Cons x31 x34};
                                                       let {x0 = Nil};
                                                       let {x38 = x28};
                                                       let {x39 = x30};
                                                       let {x1 = Cons x38 x39};
                                                       return (x0, x1)},
                                                   do {let {x41 = O};
                                                       let {x40 = S x41};
                                                       let {x42 = x40};
                                                       (x0,
                                                        x2) <- do {x2 <- gen__my_clauseSolveOO_x2;
                                                                   let {x0 = Cons x42 x2};
                                                                   return (x0, x2)};
                                                       x1 <- __my_clauseSolveIO x2;
                                                       return (x0, x1)}]
_solve_atomMy_clauseSolveIOO x0 = msum [do {guard (x0 == Nil);
                                            (x1, x2) <- __my_clauseSolveOO;
                                            return (x1, x2)},
                                        do {let {x71 = O};
                                            let {x70 = S x71};
                                            let {x69 = S x70};
                                            (x72, x3) <- case x0 of
                                                         {Cons y72 y3 -> return (y72, y3);
                                                          _ -> mzero};
                                            guard (x72 == x69);
                                            (x1, x2) <- __solve_atomMy_clauseSolveIOO x3;
                                            return (x1, x2)}]
__my_clauseSolveOO = msum [do {let {x45 = O};
                               let {x44 = S x45};
                               let {x43 = S x44};
                               let {x48 = O};
                               let {x47 = S x48};
                               let {x49 = Nil};
                               let {x46 = Cons x47 x49};
                               let {x0 = Nil};
                               let {x50 = x43};
                               let {x51 = x46};
                               let {x1 = Cons x50 x51};
                               return (x0, x1)},
                           do {let {x54 = O};
                               let {x53 = S x54};
                               let {x52 = S x53};
                               let {x55 = x52};
                               (x2, x1) <- ___my_clauseSolveOO;
                               let {x0 = Cons x55 x2};
                               return (x0, x1)}]
___my_clauseSolveOO = msum [do {let {x57 = O};
                                let {x56 = S x57};
                                let {x58 = Nil};
                                let {x0 = Nil};
                                let {x59 = x56};
                                let {x60 = x58};
                                let {x1 = Cons x59 x60};
                                return (x0, x1)},
                            do {let {x1 = Nil};
                                let {x62 = O};
                                let {x61 = S x62};
                                let {x63 = Nil};
                                let {x64 = x61};
                                let {x65 = x63};
                                let {x0 = Cons x64 x65};
                                return (x0, x1)}]
__solve_atomMy_clauseSolveIOO x0 = msum [do {guard (x0 == Nil);
                                             (x1, x2) <- ___my_clauseSolveOO;
                                             return (x1, x2)},
                                         do {let {x2 = Nil};
                                             let {x74 = O};
                                             let {x73 = S x74};
                                             (x75, x3) <- case x0 of
                                                          {Cons y75 y3 -> return (y75, y3);
                                                           _ -> mzero};
                                             guard (x75 == x73);
                                             x1 <- solve_atomIO x3;
                                             return (x1, x2)}]
solve_atomMy_clauseSolveOII x1 x2 = msum [do {_my_clauseSolveII x1 x2;
                                              let {x0 = Nil};
                                              return x0},
                                          do {let {x67 = O};
                                              let {x66 = S x67};
                                              let {x68 = x66};
                                              x3 <- _solve_atomMy_clauseSolveOII x1 x2;
                                              let {x0 = Cons x68 x3};
                                              return x0}]
_solve_atomMy_clauseSolveOII x1 x2 = msum [do {__my_clauseSolveII x1 x2;
                                               let {x0 = Nil};
                                               return x0},
                                           do {let {x71 = O};
                                               let {x70 = S x71};
                                               let {x69 = S x70};
                                               let {x72 = x69};
                                               x3 <- __solve_atomMy_clauseSolveOII x1 x2;
                                               let {x0 = Cons x72 x3};
                                               return x0}]
__solve_atomMy_clauseSolveOII x1 x2 = msum [do {___my_clauseSolveII x1 x2;
                                                let {x0 = Nil};
                                                return x0},
                                            do {let {x74 = O};
                                                let {x73 = S x74};
                                                guard (x2 == Nil);
                                                let {x75 = x73};
                                                x3 <- solve_atomOI x1;
                                                let {x0 = Cons x75 x3};
                                                return x0}]
solve_atomOI x1 = msum [do {let {x0 = Nil};
                            guard (x1 == Nil);
                            return x0}]
solve_atomMy_clauseSolveOIO x1 = msum [do {let {x0 = Nil};
                                           x2 <- _my_clauseSolveIO x1;
                                           return (x0, x2)},
                                       do {let {x67 = O};
                                           let {x66 = S x67};
                                           let {x68 = x66};
                                           (x3, x2) <- _solve_atomMy_clauseSolveOIO x1;
                                           let {x0 = Cons x68 x3};
                                           return (x0, x2)}]
_solve_atomMy_clauseSolveOIO x1 = msum [do {let {x0 = Nil};
                                            x2 <- __my_clauseSolveIO x1;
                                            return (x0, x2)},
                                        do {let {x71 = O};
                                            let {x70 = S x71};
                                            let {x69 = S x70};
                                            let {x72 = x69};
                                            (x3, x2) <- __solve_atomMy_clauseSolveOIO x1;
                                            let {x0 = Cons x72 x3};
                                            return (x0, x2)}]
__solve_atomMy_clauseSolveOIO x1 = msum [do {let {x0 = Nil};
                                             x2 <- ___my_clauseSolveIO x1;
                                             return (x0, x2)},
                                         do {let {x2 = Nil};
                                             let {x74 = O};
                                             let {x73 = S x74};
                                             let {x75 = x73};
                                             x3 <- solve_atomOI x1;
                                             let {x0 = Cons x75 x3};
                                             return (x0, x2)}]
solve_atomMy_clauseSolveOOI x2 = msum [do {let {x0 = Nil};
                                           x1 <- _my_clauseSolveOI x2;
                                           return (x0, x1)},
                                       do {let {x67 = O};
                                           let {x66 = S x67};
                                           let {x68 = x66};
                                           (x3, x1) <- _solve_atomMy_clauseSolveOOI x2;
                                           let {x0 = Cons x68 x3};
                                           return (x0, x1)}]
_solve_atomMy_clauseSolveOOI x2 = msum [do {let {x0 = Nil};
                                            x1 <- __my_clauseSolveOI x2;
                                            return (x0, x1)},
                                        do {let {x71 = O};
                                            let {x70 = S x71};
                                            let {x69 = S x70};
                                            let {x72 = x69};
                                            (x3, x1) <- __solve_atomMy_clauseSolveOOI x2;
                                            let {x0 = Cons x72 x3};
                                            return (x0, x1)}]
__solve_atomMy_clauseSolveOOI x2 = msum [do {let {x0 = Nil};
                                             x1 <- ___my_clauseSolveOI x2;
                                             return (x0, x1)},
                                         do {let {x74 = O};
                                             let {x73 = S x74};
                                             guard (x2 == Nil);
                                             let {x75 = x73};
                                             (x3, x1) <- solve_atomOO;
                                             let {x0 = Cons x75 x3};
                                             return (x0, x1)}]
solve_atomOO = msum [do {let {x1 = Nil};
                         let {x0 = Nil};
                         return (x0, x1)}]