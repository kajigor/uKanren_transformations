module Depth_for_cpd where

import Stream
import Control.Monad

data Term
    = Append
    | C Term Term
    | C Term Term Term
    | C Term Term Term Term
    | Cons
    | Cons Term Term
    | Member
    | Nil
    | O
    | S Term
    | True
    deriving (Show, Eq)
depthII x0 x1 gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOI_x12 gen_prog_clauseOI_x16 gen_prog_clauseOI_x17 gen_prog_clauseOI_x2 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9 = msum [do {depth0I x0;
                                                                                                                                                                                                                                                                                                                                               guard (x1 == O);
                                                                                                                                                                                                                                                                                                                                               return ()},
                                                                                                                                                                                                                                                                                                                                           do {depth1I x0;
                                                                                                                                                                                                                                                                                                                                               (x2,
                                                                                                                                                                                                                                                                                                                                                x4) <- depthOO gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                                                                                                               x5 <- maxIOI x4 x1;
                                                                                                                                                                                                                                                                                                                                               x3 <- depthOI x5 gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOI_x12 gen_prog_clauseOI_x16 gen_prog_clauseOI_x17 gen_prog_clauseOI_x2 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                                                                                                               return ()},
                                                                                                                                                                                                                                                                                                                                           do {x3 <- case x1 of
                                                                                                                                                                                                                                                                                                                                                     {S y3 -> return y3;
                                                                                                                                                                                                                                                                                                                                                      _ -> mzero};
                                                                                                                                                                                                                                                                                                                                               x2 <- prog_clauseIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9;
                                                                                                                                                                                                                                                                                                                                               depthII x2 x3 gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOI_x12 gen_prog_clauseOI_x16 gen_prog_clauseOI_x17 gen_prog_clauseOI_x2 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                                                                                                               return ()}]
depthIO x0 gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9 = msum [do {depth0I x0;
                                                                                                                                                                                                                                                     let {x1 = O};
                                                                                                                                                                                                                                                     return x1},
                                                                                                                                                                                                                                                 do {depth1I x0;
                                                                                                                                                                                                                                                     (x2,
                                                                                                                                                                                                                                                      x4) <- depthOO gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                     (x3,
                                                                                                                                                                                                                                                      x5) <- depthOO gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                     x1 <- maxIIO x4 x5;
                                                                                                                                                                                                                                                     return x1},
                                                                                                                                                                                                                                                 do {x2 <- prog_clauseIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9;
                                                                                                                                                                                                                                                     x3 <- depthIO x2 gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                     let {x1 = S x3};
                                                                                                                                                                                                                                                     return x1}]
depthOI x1 gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOI_x12 gen_prog_clauseOI_x16 gen_prog_clauseOI_x17 gen_prog_clauseOI_x2 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9 = msum [do {x0 <- depth0O;
                                                                                                                                                                                                                                                                                                                                            guard (x1 == O);
                                                                                                                                                                                                                                                                                                                                            return x0},
                                                                                                                                                                                                                                                                                                                                        do {x0 <- depth1O gen_depth1O_x2 gen_depth1O_x3;
                                                                                                                                                                                                                                                                                                                                            (x2,
                                                                                                                                                                                                                                                                                                                                             x4) <- depthOO gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                                                                                                            x5 <- maxIOI x4 x1;
                                                                                                                                                                                                                                                                                                                                            x3 <- depthOI x5 gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOI_x12 gen_prog_clauseOI_x16 gen_prog_clauseOI_x17 gen_prog_clauseOI_x2 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                                                                                                            return x0},
                                                                                                                                                                                                                                                                                                                                        do {x3 <- case x1 of
                                                                                                                                                                                                                                                                                                                                                  {S y3 -> return y3;
                                                                                                                                                                                                                                                                                                                                                   _ -> mzero};
                                                                                                                                                                                                                                                                                                                                            x2 <- depthOI x3 gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOI_x12 gen_prog_clauseOI_x16 gen_prog_clauseOI_x17 gen_prog_clauseOI_x2 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                                                                                                            x0 <- prog_clauseOI x2 gen_prog_clauseOI_x12 gen_prog_clauseOI_x16 gen_prog_clauseOI_x17 gen_prog_clauseOI_x2;
                                                                                                                                                                                                                                                                                                                                            return x0}]
depthOO gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9 = msum [do {x0 <- depth0O;
                                                                                                                                                                                                                                                  let {x1 = O};
                                                                                                                                                                                                                                                  return (x0,
                                                                                                                                                                                                                                                          x1)},
                                                                                                                                                                                                                                              do {x0 <- depth1O gen_depth1O_x2 gen_depth1O_x3;
                                                                                                                                                                                                                                                  (x2,
                                                                                                                                                                                                                                                   x4) <- depthOO gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                  (x3,
                                                                                                                                                                                                                                                   x5) <- depthOO gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                  x1 <- maxIIO x4 x5;
                                                                                                                                                                                                                                                  return (x0,
                                                                                                                                                                                                                                                          x1)},
                                                                                                                                                                                                                                              do {(x0,
                                                                                                                                                                                                                                                   x2) <- prog_clauseOO gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                  x3 <- depthIO x2 gen_depth1O_x2 gen_depth1O_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9;
                                                                                                                                                                                                                                                  let {x1 = S x3};
                                                                                                                                                                                                                                                  return (x0,
                                                                                                                                                                                                                                                          x1)}]
depth0I x0 = msum [do {(x4, x5) <- case x0 of
                                   {C y4 y5 -> return (y4, y5); _ -> mzero};
                       guard (x4 == True);
                       guard (x5 == Nil);
                       return ()}]
depth0O = msum [do {let {x4 = True};
                    let {x5 = Nil};
                    let {x0 = C x4 x5};
                    return x0}]
depth1I x0 = msum [do {(x6, x2, x3) <- case x0 of
                                       {C y6 y2 y3 -> return (y6, y2, y3); _ -> mzero};
                       guard (x6 == Cons);
                       return ()}]
depth1O gen_depth1O_x2 gen_depth1O_x3 = msum [do {let {x6 = Cons};
                                                  (x0, x2, x3) <- do {x2 <- gen_depth1O_x2;
                                                                      x3 <- gen_depth1O_x3;
                                                                      let {x0 = C x6 x2 x3};
                                                                      return (x0, x2, x3)};
                                                  return x0}]
maxIIO x0 x1 = msum [do {guard (x1 == O);
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
maxIOI x0 x2 = msum [do {let {x1 = O};
                         guard (x0 == x2);
                         return x1},
                     do {guard (x0 == O); let {x1 = x2}; return x1},
                     do {x3 <- case x0 of
                               {S y3 -> return y3; _ -> mzero};
                         x5 <- case x2 of
                               {S y5 -> return y5; _ -> mzero};
                         x4 <- maxIOI x3 x5;
                         let {x1 = S x4};
                         return x1}]
prog_clauseIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x9 = msum [do {(x7,
                                                                        x2,
                                                                        x3) <- case x0 of
                                                                               {C y7
                                                                                  y2
                                                                                  y3 -> return (y7,
                                                                                                y2,
                                                                                                y3);
                                                                                _ -> mzero};
                                                                       guard (x7 == Member);
                                                                       let {x8 = Append};
                                                                       (x1,
                                                                        x4,
                                                                        x9) <- do {x4 <- gen_prog_clauseIO_x4;
                                                                                   x9 <- gen_prog_clauseIO_x9;
                                                                                   let {x1 = C x8 x4 x9 x3};
                                                                                   return (x1,
                                                                                           x4,
                                                                                           x9)};
                                                                       x5 <- case x9 of
                                                                             {Cons y2
                                                                                   y5 -> do {guard (x2 == y2);
                                                                                             return y5};
                                                                              _ -> mzero};
                                                                       return x1},
                                                                   do {(x10,
                                                                        x11,
                                                                        x2,
                                                                        x12) <- case x0 of
                                                                                {C y10
                                                                                   y11
                                                                                   y2
                                                                                   y12 -> return (y10,
                                                                                                  y11,
                                                                                                  y2,
                                                                                                  y12);
                                                                                 _ -> mzero};
                                                                       guard (x10 == Append);
                                                                       guard (x11 == Nil);
                                                                       guard (x12 == x2);
                                                                       let {x13 = True};
                                                                       let {x14 = Nil};
                                                                       let {x1 = C x13 x14};
                                                                       return x1},
                                                                   do {(x15,
                                                                        x16,
                                                                        x4,
                                                                        x17) <- case x0 of
                                                                                {C y15
                                                                                   y16
                                                                                   y4
                                                                                   y17 -> return (y15,
                                                                                                  y16,
                                                                                                  y4,
                                                                                                  y17);
                                                                                 _ -> mzero};
                                                                       guard (x15 == Append);
                                                                       (x2, x3) <- case x16 of
                                                                                   {Cons y2
                                                                                         y3 -> return (y2,
                                                                                                       y3);
                                                                                    _ -> mzero};
                                                                       (x18, x5) <- case x17 of
                                                                                    {Cons y18
                                                                                          y5 -> return (y18,
                                                                                                        y5);
                                                                                     _ -> mzero};
                                                                       guard (x18 == x2);
                                                                       let {x19 = Append};
                                                                       let {x1 = C x19 x3 x4 x5};
                                                                       return x1}]
prog_clauseOI x1 gen_prog_clauseOI_x12 gen_prog_clauseOI_x16 gen_prog_clauseOI_x17 gen_prog_clauseOI_x2 = msum [do {let {x7 = Member};
                                                                                                                    (x8,
                                                                                                                     x4,
                                                                                                                     x9,
                                                                                                                     x3) <- case x1 of
                                                                                                                            {C y8
                                                                                                                               y4
                                                                                                                               y9
                                                                                                                               y3 -> return (y8,
                                                                                                                                             y4,
                                                                                                                                             y9,
                                                                                                                                             y3);
                                                                                                                             _ -> mzero};
                                                                                                                    guard (x8 == Append);
                                                                                                                    (x2,
                                                                                                                     x5) <- case x9 of
                                                                                                                            {Cons y2
                                                                                                                                  y5 -> return (y2,
                                                                                                                                                y5);
                                                                                                                             _ -> mzero};
                                                                                                                    let {x0 = C x7 x2 x3};
                                                                                                                    return x0},
                                                                                                                do {let {x10 = Append};
                                                                                                                    let {x11 = Nil};
                                                                                                                    (x13,
                                                                                                                     x14) <- case x1 of
                                                                                                                             {C y13
                                                                                                                                y14 -> return (y13,
                                                                                                                                               y14);
                                                                                                                              _ -> mzero};
                                                                                                                    guard (x13 == True);
                                                                                                                    guard (x14 == Nil);
                                                                                                                    (x0,
                                                                                                                     x2,
                                                                                                                     x12) <- do {x2 <- gen_prog_clauseOI_x2;
                                                                                                                                 x12 <- gen_prog_clauseOI_x12;
                                                                                                                                 let {x0 = C x10 x11 x2 x12};
                                                                                                                                 return (x0,
                                                                                                                                         x2,
                                                                                                                                         x12)};
                                                                                                                    guard (x12 == x2);
                                                                                                                    return x0},
                                                                                                                do {let {x15 = Append};
                                                                                                                    (x19,
                                                                                                                     x3,
                                                                                                                     x4,
                                                                                                                     x5) <- case x1 of
                                                                                                                            {C y19
                                                                                                                               y3
                                                                                                                               y4
                                                                                                                               y5 -> return (y19,
                                                                                                                                             y3,
                                                                                                                                             y4,
                                                                                                                                             y5);
                                                                                                                             _ -> mzero};
                                                                                                                    guard (x19 == Append);
                                                                                                                    (x0,
                                                                                                                     x16,
                                                                                                                     x17) <- do {x16 <- gen_prog_clauseOI_x16;
                                                                                                                                 x17 <- gen_prog_clauseOI_x17;
                                                                                                                                 let {x0 = C x15 x16 x4 x17};
                                                                                                                                 return (x0,
                                                                                                                                         x16,
                                                                                                                                         x17)};
                                                                                                                    x2 <- case x16 of
                                                                                                                          {Cons y2
                                                                                                                                y3 -> do {guard (x3 == y3);
                                                                                                                                          return y2};
                                                                                                                           _ -> mzero};
                                                                                                                    x18 <- case x17 of
                                                                                                                           {Cons y18
                                                                                                                                 y5 -> do {guard (x5 == y5);
                                                                                                                                           return y18};
                                                                                                                            _ -> mzero};
                                                                                                                    guard (x18 == x2);
                                                                                                                    return x0}]
prog_clauseOO gen_prog_clauseOO_x12 gen_prog_clauseOO_x16 gen_prog_clauseOO_x17 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x9 = msum [do {let {x7 = Member};
                                                                                                                                                                                let {x8 = Append};
                                                                                                                                                                                (x0,
                                                                                                                                                                                 x2,
                                                                                                                                                                                 x3) <- do {x2 <- gen_prog_clauseOO_x2;
                                                                                                                                                                                            x3 <- gen_prog_clauseOO_x3;
                                                                                                                                                                                            let {x0 = C x7 x2 x3};
                                                                                                                                                                                            return (x0,
                                                                                                                                                                                                    x2,
                                                                                                                                                                                                    x3)};
                                                                                                                                                                                (x1,
                                                                                                                                                                                 x4,
                                                                                                                                                                                 x9) <- do {x4 <- gen_prog_clauseOO_x4;
                                                                                                                                                                                            x9 <- gen_prog_clauseOO_x9;
                                                                                                                                                                                            let {x1 = C x8 x4 x9 x3};
                                                                                                                                                                                            return (x1,
                                                                                                                                                                                                    x4,
                                                                                                                                                                                                    x9)};
                                                                                                                                                                                x5 <- case x9 of
                                                                                                                                                                                      {Cons y2
                                                                                                                                                                                            y5 -> do {guard (x2 == y2);
                                                                                                                                                                                                      return y5};
                                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                                return (x0,
                                                                                                                                                                                        x1)},
                                                                                                                                                                            do {let {x10 = Append};
                                                                                                                                                                                let {x11 = Nil};
                                                                                                                                                                                let {x13 = True};
                                                                                                                                                                                let {x14 = Nil};
                                                                                                                                                                                let {x1 = C x13 x14};
                                                                                                                                                                                (x0,
                                                                                                                                                                                 x2,
                                                                                                                                                                                 x12) <- do {x2 <- gen_prog_clauseOO_x2;
                                                                                                                                                                                             x12 <- gen_prog_clauseOO_x12;
                                                                                                                                                                                             let {x0 = C x10 x11 x2 x12};
                                                                                                                                                                                             return (x0,
                                                                                                                                                                                                     x2,
                                                                                                                                                                                                     x12)};
                                                                                                                                                                                guard (x12 == x2);
                                                                                                                                                                                return (x0,
                                                                                                                                                                                        x1)},
                                                                                                                                                                            do {let {x15 = Append};
                                                                                                                                                                                let {x19 = Append};
                                                                                                                                                                                (x0,
                                                                                                                                                                                 x16,
                                                                                                                                                                                 x4,
                                                                                                                                                                                 x17) <- do {x16 <- gen_prog_clauseOO_x16;
                                                                                                                                                                                             x4 <- gen_prog_clauseOO_x4;
                                                                                                                                                                                             x17 <- gen_prog_clauseOO_x17;
                                                                                                                                                                                             let {x0 = C x15 x16 x4 x17};
                                                                                                                                                                                             return (x0,
                                                                                                                                                                                                     x16,
                                                                                                                                                                                                     x4,
                                                                                                                                                                                                     x17)};
                                                                                                                                                                                (x2,
                                                                                                                                                                                 x3) <- case x16 of
                                                                                                                                                                                        {Cons y2
                                                                                                                                                                                              y3 -> return (y2,
                                                                                                                                                                                                            y3);
                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                (x18,
                                                                                                                                                                                 x5) <- case x17 of
                                                                                                                                                                                        {Cons y18
                                                                                                                                                                                              y5 -> return (y18,
                                                                                                                                                                                                            y5);
                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                guard (x18 == x2);
                                                                                                                                                                                let {x1 = C x19 x3 x4 x5};
                                                                                                                                                                                return (x0,
                                                                                                                                                                                        x1)}]