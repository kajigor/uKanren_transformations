module NthOpt_unfold where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | None
    | Some Term
    deriving (Show, Eq)
nthOptII x0 x1 = msum [do {guard (x1 == None);
                           guard (x0 == Nil);
                           return ()},
                       do {guard (x1 == None);
                           (x2, x3) <- case x0 of
                                       {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                           guard (x3 == Nil);
                           return ()},
                       do {guard (x1 == None);
                           (x2, x4) <- case x0 of
                                       {Cons y2 y4 -> return (y2, y4); _ -> mzero};
                           (x3, x5) <- case x4 of
                                       {Cons y3 y5 -> return (y3, y5); _ -> mzero};
                           guard (x5 == Nil);
                           return ()},
                       do {guard (x1 == None);
                           (x2, x6) <- case x0 of
                                       {Cons y2 y6 -> return (y2, y6); _ -> mzero};
                           (x3, x7) <- case x6 of
                                       {Cons y3 y7 -> return (y3, y7); _ -> mzero};
                           (x4, x8) <- case x7 of
                                       {Cons y4 y8 -> return (y4, y8); _ -> mzero};
                           guard (x8 == Nil);
                           return ()},
                       do {x5 <- case x1 of
                                 {Some y5 -> return y5; _ -> mzero};
                           (x2, x9) <- case x0 of
                                       {Cons y2 y9 -> return (y2, y9); _ -> mzero};
                           (x3, x10) <- case x9 of
                                        {Cons y3 y10 -> return (y3, y10); _ -> mzero};
                           (x4, x11) <- case x10 of
                                        {Cons y4 y11 -> return (y4, y11); _ -> mzero};
                           x6 <- case x11 of
                                 {Cons y5 y6 -> do {guard (x5 == y5); return y6}; _ -> mzero};
                           return ()}]
nthOptIO x0 = msum [do {let {x1 = None};
                        guard (x0 == Nil);
                        return x1},
                    do {let {x1 = None};
                        (x2, x3) <- case x0 of
                                    {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                        guard (x3 == Nil);
                        return x1},
                    do {let {x1 = None};
                        (x2, x4) <- case x0 of
                                    {Cons y2 y4 -> return (y2, y4); _ -> mzero};
                        (x3, x5) <- case x4 of
                                    {Cons y3 y5 -> return (y3, y5); _ -> mzero};
                        guard (x5 == Nil);
                        return x1},
                    do {let {x1 = None};
                        (x2, x6) <- case x0 of
                                    {Cons y2 y6 -> return (y2, y6); _ -> mzero};
                        (x3, x7) <- case x6 of
                                    {Cons y3 y7 -> return (y3, y7); _ -> mzero};
                        (x4, x8) <- case x7 of
                                    {Cons y4 y8 -> return (y4, y8); _ -> mzero};
                        guard (x8 == Nil);
                        return x1},
                    do {(x2, x9) <- case x0 of
                                    {Cons y2 y9 -> return (y2, y9); _ -> mzero};
                        (x3, x10) <- case x9 of
                                     {Cons y3 y10 -> return (y3, y10); _ -> mzero};
                        (x4, x11) <- case x10 of
                                     {Cons y4 y11 -> return (y4, y11); _ -> mzero};
                        (x5, x6) <- case x11 of
                                    {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                        let {x1 = Some x5};
                        return x1}]
nthOptOI x1 gen_nthOptOI_x2 gen_nthOptOI_x3 gen_nthOptOI_x4 gen_nthOptOI_x6 = msum [do {guard (x1 == None);
                                                                                        let {x0 = Nil};
                                                                                        return x0},
                                                                                    do {guard (x1 == None);
                                                                                        let {x3 = Nil};
                                                                                        (x0,
                                                                                         x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                    let {x0 = Cons x2 x3};
                                                                                                    return (x0,
                                                                                                            x2)};
                                                                                        return x0},
                                                                                    do {guard (x1 == None);
                                                                                        let {x5 = Nil};
                                                                                        (x4,
                                                                                         x3) <- do {x3 <- gen_nthOptOI_x3;
                                                                                                    let {x4 = Cons x3 x5};
                                                                                                    return (x4,
                                                                                                            x3)};
                                                                                        (x0,
                                                                                         x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                    let {x0 = Cons x2 x4};
                                                                                                    return (x0,
                                                                                                            x2)};
                                                                                        return x0},
                                                                                    do {guard (x1 == None);
                                                                                        let {x8 = Nil};
                                                                                        (x7,
                                                                                         x4) <- do {x4 <- gen_nthOptOI_x4;
                                                                                                    let {x7 = Cons x4 x8};
                                                                                                    return (x7,
                                                                                                            x4)};
                                                                                        (x6,
                                                                                         x3) <- do {x3 <- gen_nthOptOI_x3;
                                                                                                    let {x6 = Cons x3 x7};
                                                                                                    return (x6,
                                                                                                            x3)};
                                                                                        (x0,
                                                                                         x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                    let {x0 = Cons x2 x6};
                                                                                                    return (x0,
                                                                                                            x2)};
                                                                                        return x0},
                                                                                    do {x5 <- case x1 of
                                                                                              {Some y5 -> return y5;
                                                                                               _ -> mzero};
                                                                                        (x11,
                                                                                         x6) <- do {x6 <- gen_nthOptOI_x6;
                                                                                                    let {x11 = Cons x5 x6};
                                                                                                    return (x11,
                                                                                                            x6)};
                                                                                        (x10,
                                                                                         x4) <- do {x4 <- gen_nthOptOI_x4;
                                                                                                    let {x10 = Cons x4 x11};
                                                                                                    return (x10,
                                                                                                            x4)};
                                                                                        (x9,
                                                                                         x3) <- do {x3 <- gen_nthOptOI_x3;
                                                                                                    let {x9 = Cons x3 x10};
                                                                                                    return (x9,
                                                                                                            x3)};
                                                                                        (x0,
                                                                                         x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                    let {x0 = Cons x2 x9};
                                                                                                    return (x0,
                                                                                                            x2)};
                                                                                        return x0}]
nthOptOO gen_nthOptOO_x2 gen_nthOptOO_x3 gen_nthOptOO_x4 gen_nthOptOO_x5 gen_nthOptOO_x9 = msum [do {let {x1 = None};
                                                                                                     let {x0 = Nil};
                                                                                                     return (x0,
                                                                                                             x1)},
                                                                                                 do {let {x1 = None};
                                                                                                     let {x3 = Nil};
                                                                                                     (x0,
                                                                                                      x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                                 let {x0 = Cons x2 x3};
                                                                                                                 return (x0,
                                                                                                                         x2)};
                                                                                                     return (x0,
                                                                                                             x1)},
                                                                                                 do {let {x1 = None};
                                                                                                     let {x5 = Nil};
                                                                                                     (x4,
                                                                                                      x3) <- do {x3 <- gen_nthOptOO_x3;
                                                                                                                 let {x4 = Cons x3 x5};
                                                                                                                 return (x4,
                                                                                                                         x3)};
                                                                                                     (x0,
                                                                                                      x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                                 let {x0 = Cons x2 x4};
                                                                                                                 return (x0,
                                                                                                                         x2)};
                                                                                                     return (x0,
                                                                                                             x1)},
                                                                                                 do {let {x1 = None};
                                                                                                     let {x8 = Nil};
                                                                                                     (x7,
                                                                                                      x4) <- do {x4 <- gen_nthOptOO_x4;
                                                                                                                 let {x7 = Cons x4 x8};
                                                                                                                 return (x7,
                                                                                                                         x4)};
                                                                                                     (x6,
                                                                                                      x3) <- do {x3 <- gen_nthOptOO_x3;
                                                                                                                 let {x6 = Cons x3 x7};
                                                                                                                 return (x6,
                                                                                                                         x3)};
                                                                                                     (x0,
                                                                                                      x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                                 let {x0 = Cons x2 x6};
                                                                                                                 return (x0,
                                                                                                                         x2)};
                                                                                                     return (x0,
                                                                                                             x1)},
                                                                                                 do {(x1,
                                                                                                      x5) <- do {x5 <- gen_nthOptOO_x5;
                                                                                                                 let {x1 = Some x5};
                                                                                                                 return (x1,
                                                                                                                         x5)};
                                                                                                     (x0,
                                                                                                      x2,
                                                                                                      x9) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                                 x9 <- gen_nthOptOO_x9;
                                                                                                                 let {x0 = Cons x2 x9};
                                                                                                                 return (x0,
                                                                                                                         x2,
                                                                                                                         x9)};
                                                                                                     (x3,
                                                                                                      x10) <- case x9 of
                                                                                                              {Cons y3
                                                                                                                    y10 -> return (y3,
                                                                                                                                   y10);
                                                                                                               _ -> mzero};
                                                                                                     (x4,
                                                                                                      x11) <- case x10 of
                                                                                                              {Cons y4
                                                                                                                    y11 -> return (y4,
                                                                                                                                   y11);
                                                                                                               _ -> mzero};
                                                                                                     x6 <- case x11 of
                                                                                                           {Cons y5
                                                                                                                 y6 -> do {guard (x5 == y5);
                                                                                                                           return y6};
                                                                                                            _ -> mzero};
                                                                                                     return (x0,
                                                                                                             x1)}]