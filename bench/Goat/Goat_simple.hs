module Goat_simple where

import Stream
import Control.Monad
import Term

evalIOI x0 x2 gen_swapOO_x2 gen_swapOO_x3 = Immature $ msum [do {guard (x0 == x2);
                                                      let {x1 = Nil};
                                                      return x1},
                                                  do {(x3,
                                                       x5) <- stepIOO x0 gen_swapOO_x2 gen_swapOO_x3;
                                                      let {x78 = x3};
                                                      x4 <- evalIOI x5 x2 gen_swapOO_x2 gen_swapOO_x3;
                                                      let {x79 = x4};
                                                      let {x1 = Cons x78 x79};
                                                      return x1}]
stepIOO x0 gen_swapOO_x2 gen_swapOO_x3 = Immature $ msum [do {(x3,
                                                    x4) <- case x0 of
                                                           {Pair y3 y4 -> return (y3, y4);
                                                            _ -> mzero};
                                                   (x1,
                                                    x2) <- step0OOII x3 x4 gen_swapOO_x2 gen_swapOO_x3;
                                                   return (x1, x2)}]
step0OOII x3 x4 gen_swapOO_x2 gen_swapOO_x3 = Immature $ msum [do {let {x70 = Man};
                                                        let {x71 = Term.True};
                                                        getIII x3 x70 x71;
                                                        let {x72 = Man};
                                                        let {x73 = Term.False};
                                                        getIII x4 x72 x73;
                                                        (x2, x1) <- step'IIOO x3 x4;
                                                        return (x1, x2)},
                                                    do {let {x74 = Man};
                                                        let {x75 = Term.True};
                                                        getIII x4 x74 x75;
                                                        let {x76 = Man};
                                                        let {x77 = Term.False};
                                                        getIII x3 x76 x77;
                                                        (x5,
                                                         x2) <- swapOO gen_swapOO_x2 gen_swapOO_x3;
                                                        x1 <- step'IIIO x4 x3 x5;
                                                        return (x1, x2)}]
getIII x0 x1 x2 = Immature $ msum [do {(x12, x3, x4, x5) <- case x0 of
                                                 {Quad y12 y3 y4 y5 -> return (y12, y3, y4, y5);
                                                  _ -> mzero};
                            guard (x12 == x2);
                            guard (x1 == Goat);
                            return ()},
                        do {(x3, x13, x4, x5) <- case x0 of
                                                 {Quad y3 y13 y4 y5 -> return (y3, y13, y4, y5);
                                                  _ -> mzero};
                            guard (x13 == x2);
                            guard (x1 == Wolf);
                            return ()},
                        do {(x3, x4, x14, x5) <- case x0 of
                                                 {Quad y3 y4 y14 y5 -> return (y3, y4, y14, y5);
                                                  _ -> mzero};
                            guard (x14 == x2);
                            guard (x1 == Cabbage);
                            return ()},
                        do {(x3, x4, x5, x15) <- case x0 of
                                                 {Quad y3 y4 y5 y15 -> return (y3, y4, y5, y15);
                                                  _ -> mzero};
                            guard (x15 == x2);
                            guard (x1 == Man);
                            return ()}]
step'IIIO x0 x1 x2 = Immature $ msum [do {(x4, x5, x6, x7) <- case x0 of
                                                   {Quad y4 y5 y6 y7 -> return (y4, y5, y6, y7);
                                                    _ -> mzero};
                               (x8, x9, x10, x11) <- case x1 of
                                                     {Quad y8 y9 y10 y11 -> return (y8,
                                                                                    y9,
                                                                                    y10,
                                                                                    y11);
                                                      _ -> mzero};
                               x3 <- step'0IIOIIIIII x0 x2 x4 x5 x6 x8 x9 x10;
                               return x3}]
step'IIOO x0 x1 = Immature $ msum [do {(x4, x5, x6, x7) <- case x0 of
                                                {Quad y4 y5 y6 y7 -> return (y4, y5, y6, y7);
                                                 _ -> mzero};
                            (x8, x9, x10, x11) <- case x1 of
                                                  {Quad y8 y9 y10 y11 -> return (y8, y9, y10, y11);
                                                   _ -> mzero};
                            (x2, x3) <- step'0IOOIIIIII x0 x4 x5 x6 x8 x9 x10;
                            return (x2, x3)}]
step'0IIOIIIIII x0 x2 x4 x5 x6 x8 x9 x10 = Immature $ msum [do {safeI x2;
                                                     let {x3 = Term.Empty};
                                                     let {x35 = Term.False};
                                                     let {x34 = Quad x4 x5 x6 x35};
                                                     let {x37 = Term.True};
                                                     let {x36 = Quad x8 x9 x10 x37};
                                                     (x38, x39) <- case x2 of
                                                                   {Pair y38 y39 -> return (y38,
                                                                                            y39);
                                                                    _ -> mzero};
                                                     guard (x38 == x34);
                                                     guard (x39 == x36);
                                                     return x3},
                                                 do {safeI x2;
                                                     let {x3 = Goat};
                                                     let {x40 = Goat};
                                                     let {x41 = Term.True};
                                                     getIII x0 x40 x41;
                                                     let {x43 = Term.False};
                                                     let {x44 = Term.False};
                                                     let {x42 = Quad x43 x5 x6 x44};
                                                     let {x46 = Term.True};
                                                     let {x47 = Term.True};
                                                     let {x45 = Quad x46 x9 x10 x47};
                                                     (x48, x49) <- case x2 of
                                                                   {Pair y48 y49 -> return (y48,
                                                                                            y49);
                                                                    _ -> mzero};
                                                     guard (x48 == x42);
                                                     guard (x49 == x45);
                                                     return x3},
                                                 do {safeI x2;
                                                     let {x3 = Wolf};
                                                     let {x50 = Wolf};
                                                     let {x51 = Term.True};
                                                     getIII x0 x50 x51;
                                                     let {x53 = Term.False};
                                                     let {x54 = Term.False};
                                                     let {x52 = Quad x4 x53 x6 x54};
                                                     let {x56 = Term.True};
                                                     let {x57 = Term.True};
                                                     let {x55 = Quad x9 x56 x10 x57};
                                                     (x58, x59) <- case x2 of
                                                                   {Pair y58 y59 -> return (y58,
                                                                                            y59);
                                                                    _ -> mzero};
                                                     guard (x58 == x52);
                                                     guard (x59 == x55);
                                                     return x3},
                                                 do {safeI x2;
                                                     let {x3 = Cabbage};
                                                     let {x60 = Cabbage};
                                                     let {x61 = Term.True};
                                                     getIII x0 x60 x61;
                                                     let {x63 = Term.False};
                                                     let {x64 = Term.False};
                                                     let {x62 = Quad x4 x5 x63 x64};
                                                     let {x66 = Term.True};
                                                     let {x67 = Term.True};
                                                     let {x65 = Quad x8 x9 x66 x67};
                                                     (x68, x69) <- case x2 of
                                                                   {Pair y68 y69 -> return (y68,
                                                                                            y69);
                                                                    _ -> mzero};
                                                     guard (x68 == x62);
                                                     guard (x69 == x65);
                                                     return x3}]
safeI x0 = Immature $ msum [do {(x1, x2) <- case x0 of
                                 {Pair y1 y2 -> return (y1, y2); _ -> mzero};
                     safe'I x1;
                     safe'I x2;
                     return ()}]
safe'I x0 = Immature $ msum [do {let {x16 = Man};
                      let {x17 = Term.True};
                      getIII x0 x16 x17;
                      return ()},
                  do {safe'0I x0;
                      let {x30 = Man};
                      let {x31 = Term.False};
                      getIII x0 x30 x31;
                      return ()}]
safe'0I x0 = Immature $ msum [do {let {x18 = Goat};
                       let {x19 = Term.False};
                       getIII x0 x18 x19;
                       return ()},
                   do {safe'00I x0;
                       let {x28 = Goat};
                       let {x29 = Term.True};
                       getIII x0 x28 x29;
                       return ()}]
safe'00I x0 = Immature $ msum [do {let {x20 = Cabbage};
                        let {x21 = Term.True};
                        getIII x0 x20 x21;
                        let {x22 = Wolf};
                        let {x23 = Term.True};
                        getIII x0 x22 x23;
                        return ()},
                    do {let {x24 = Cabbage};
                        let {x25 = Term.False};
                        getIII x0 x24 x25;
                        let {x26 = Wolf};
                        let {x27 = Term.False};
                        getIII x0 x26 x27;
                        return ()}]
step'0IOOIIIIII x0 x4 x5 x6 x8 x9 x10 = Immature $ msum [do {let {x3 = Term.Empty};
                                                  let {x35 = Term.False};
                                                  let {x34 = Quad x4 x5 x6 x35};
                                                  let {x37 = Term.True};
                                                  let {x36 = Quad x8 x9 x10 x37};
                                                  let {x38 = x34};
                                                  let {x39 = x36};
                                                  let {x2 = Pair x38 x39};
                                                  safeI x2;
                                                  return (x2, x3)},
                                              do {let {x3 = Goat};
                                                  let {x40 = Goat};
                                                  let {x41 = Term.True};
                                                  getIII x0 x40 x41;
                                                  let {x43 = Term.False};
                                                  let {x44 = Term.False};
                                                  let {x42 = Quad x43 x5 x6 x44};
                                                  let {x46 = Term.True};
                                                  let {x47 = Term.True};
                                                  let {x45 = Quad x46 x9 x10 x47};
                                                  let {x48 = x42};
                                                  let {x49 = x45};
                                                  let {x2 = Pair x48 x49};
                                                  safeI x2;
                                                  return (x2, x3)},
                                              do {let {x3 = Wolf};
                                                  let {x50 = Wolf};
                                                  let {x51 = Term.True};
                                                  getIII x0 x50 x51;
                                                  let {x53 = Term.False};
                                                  let {x54 = Term.False};
                                                  let {x52 = Quad x4 x53 x6 x54};
                                                  let {x56 = Term.True};
                                                  let {x57 = Term.True};
                                                  let {x55 = Quad x9 x56 x10 x57};
                                                  let {x58 = x52};
                                                  let {x59 = x55};
                                                  let {x2 = Pair x58 x59};
                                                  safeI x2;
                                                  return (x2, x3)},
                                              do {let {x3 = Cabbage};
                                                  let {x60 = Cabbage};
                                                  let {x61 = Term.True};
                                                  getIII x0 x60 x61;
                                                  let {x63 = Term.False};
                                                  let {x64 = Term.False};
                                                  let {x62 = Quad x4 x5 x63 x64};
                                                  let {x66 = Term.True};
                                                  let {x67 = Term.True};
                                                  let {x65 = Quad x8 x9 x66 x67};
                                                  let {x68 = x62};
                                                  let {x69 = x65};
                                                  let {x2 = Pair x68 x69};
                                                  safeI x2;
                                                  return (x2, x3)}]
swapOO gen_swapOO_x2 gen_swapOO_x3 = Immature $ msum [do {(x32,
                                                x3) <- do {x3 <- gen_swapOO_x3; return (x3, x3)};
                                               (x33, x2) <- do {x2 <- gen_swapOO_x2;
                                                                return (x2, x2)};
                                               let {x0 = Pair x2 x3};
                                               let {x1 = Pair x32 x33};
                                               return (x0, x1)}]
eval x0 x2 gen_swapOO_x2 gen_swapOO_x3 = evalIOI x0 x2 gen_swapOO_x2 gen_swapOO_x3