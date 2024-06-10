module Ex_Depth_simple where

import Stream
import Control.Monad
import Term


helpOO gen_clausIO_x6 gen_helpOO_x51 = Immature $ msum [do {let {x52 = O};
                                                 let {x56 = O};
                                                 let {x55 = S x56};
                                                 let {x54 = S x55};
                                                 let {x59 = O};
                                                 let {x58 = S x59};
                                                 let {x61 = O};
                                                 let {x63 = O};
                                                 let {x67 = O};
                                                 let {x66 = S x67};
                                                 let {x65 = S x66};
                                                 let {x70 = O};
                                                 let {x69 = S x70};
                                                 let {x71 = Nil};
                                                 let {x68 = Cons x69 x71};
                                                 let {x64 = Cons x65 x68};
                                                 let {x62 = Cons x63 x64};
                                                 let {x60 = Cons x61 x62};
                                                 let {x57 = Cons x58 x60};
                                                 let {x53 = Cons x54 x57};
                                                 let {x73 = O};
                                                 let {x76 = O};
                                                 let {x75 = S x76};
                                                 let {x80 = O};
                                                 let {x79 = S x80};
                                                 let {x78 = S x79};
                                                 let {x82 = O};
                                                 let {x83 = Nil};
                                                 let {x81 = Cons x82 x83};
                                                 let {x77 = Cons x78 x81};
                                                 let {x74 = Cons x75 x77};
                                                 let {x72 = Cons x73 x74};
                                                 let {x84 = Nil};
                                                 (x50, x51) <- do {x51 <- gen_helpOO_x51;
                                                                   let {x50 = Cons x51 x84};
                                                                   return (x50, x51)};
                                                 x1 <- case x51 of
                                                       {Test y52
                                                             y53
                                                             y72
                                                             y1 -> do {guard (x52 == y52);
                                                                       guard (x53 == y53);
                                                                       guard (x72 == y72);
                                                                       return y1};
                                                        _ -> mzero};
                                                 x0 <- solveIO x50 gen_clausIO_x6;
                                                 return (x0, x1)}]
solveIO x0 gen_clausIO_x6 = Immature $ msum [do {let {x49 = O};
                                      x1 <- solve1IIO x0 x49 gen_clausIO_x6;
                                      return x1}]
solve1IIO x0 x1 gen_clausIO_x6 = Immature $ msum [do {guard (x0 == Nil);
                                           let {x2 = x1};
                                           return x2},
                                       do {let {x8 = S x1};
                                           (x3, x4) <- case x0 of
                                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                           x5 <- clausIO x3 gen_clausIO_x6;
                                           x6 <- solve1IIO x5 x8 gen_clausIO_x6;
                                           x2 <- solve1IIO x4 x6 gen_clausIO_x6;
                                           return x2}]
clausIO x0 gen_clausIO_x6 = Immature $ msum [do {let {x1 = Nil};
                                      (x2, x4) <- case x0 of
                                                  {Member y2 y4 -> return (y2, y4); _ -> mzero};
                                      (x9, x3) <- case x4 of
                                                  {Cons y9 y3 -> return (y9, y3); _ -> mzero};
                                      guard (x9 == x2);
                                      return x1},
                                  do {let {x11 = Nil};
                                      (x2, x5) <- case x0 of
                                                  {Member y2 y5 -> return (y2, y5); _ -> mzero};
                                      (x4, x3) <- case x5 of
                                                  {Cons y4 y3 -> return (y4, y3); _ -> mzero};
                                      let {x10 = Member x2 x3};
                                      let {x12 = x10};
                                      let {x13 = x11};
                                      let {x1 = Cons x12 x13};
                                      return x1},
                                  do {let {x18 = Nil};
                                      (x2, x3, x4) <- case x0 of
                                                      {InBoth y2 y3 y4 -> return (y2, y3, y4);
                                                       _ -> mzero};
                                      let {x14 = Member x2 x3};
                                      let {x17 = x2};
                                      let {x16 = Member x17 x4};
                                      let {x15 = Cons x16 x18};
                                      let {x19 = x14};
                                      let {x20 = x15};
                                      let {x1 = Cons x19 x20};
                                      return x1},
                                  do {let {x3 = Nil};
                                      let {x1 = Nil};
                                      (x22, x23, x24) <- case x0 of
                                                         {App y22 y23 y24 -> return (y22, y23, y24);
                                                          _ -> mzero};
                                      guard (x22 == x3);
                                      let {x2 = x23};
                                      let {x21 = x2};
                                      guard (x24 == x21);
                                      return x1},
                                  do {let {x27 = Nil};
                                      (x6, x4, x7) <- case x0 of
                                                      {App y6 y4 y7 -> return (y6, y4, y7);
                                                       _ -> mzero};
                                      (x2, x3) <- case x6 of
                                                  {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                      (x25, x5) <- case x7 of
                                                   {Cons y25 y5 -> return (y25, y5); _ -> mzero};
                                      guard (x25 == x2);
                                      let {x26 = App x3 x4 x5};
                                      let {x28 = x26};
                                      let {x29 = x27};
                                      let {x1 = Cons x28 x29};
                                      return x1},
                                  do {let {x1 = Nil};
                                      (x2, x4, x3) <- case x0 of
                                                      {Delete y2 y4 y3 -> return (y2, y4, y3);
                                                       _ -> mzero};
                                      (x30, x31) <- case x4 of
                                                    {Cons y30 y31 -> return (y30, y31); _ -> mzero};
                                      guard (x30 == x2);
                                      guard (x31 == x3);
                                      return x1},
                                  do {let {x34 = Nil};
                                      (x2, x6, x7) <- case x0 of
                                                      {Delete y2 y6 y7 -> return (y2, y6, y7);
                                                       _ -> mzero};
                                      (x3, x4) <- case x6 of
                                                  {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                      (x32, x5) <- case x7 of
                                                   {Cons y32 y5 -> return (y32, y5); _ -> mzero};
                                      guard (x32 == x3);
                                      let {x33 = Delete x2 x4 x5};
                                      let {x35 = x33};
                                      let {x36 = x34};
                                      let {x1 = Cons x35 x36};
                                      return x1},
                                  do {let {x46 = Nil};
                                      (x2, x3, x4, x5) <- case x0 of
                                                          {Test y2 y3 y4 y5 -> return (y2,
                                                                                       y3,
                                                                                       y4,
                                                                                       y5);
                                                           _ -> mzero};
                                      let {x37 = InBoth x2 x3 x4};
                                      let {x40 = x2};
                                      let {x41 = x3};
                                      let {x45 = x4};
                                      let {x47 = x37};
                                      (x44, x6) <- do {x6 <- gen_clausIO_x6; return (x6, x6)};
                                      let {x39 = Delete x40 x41 x6};
                                      let {x43 = App x44 x45 x5};
                                      let {x42 = Cons x43 x46};
                                      let {x38 = Cons x39 x42};
                                      let {x48 = x38};
                                      let {x1 = Cons x47 x48};
                                      return x1}]
help gen_clausIO_x6 gen_helpOO_x51 = helpOO gen_clausIO_x6 gen_helpOO_x51