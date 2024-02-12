module Rev_acc where

import Stream
import Control.Monad

data Term
    = A
    | B
    | C
    | Cons Term Term
    | D
    | Nil
    deriving (Show, Eq)
revI x0 = msum [do {let {x5 = Nil};
                    (x1, x2) <- case x0 of
                                {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                    let {x4 = Cons x1 x5};
                    _revII x2 x4;
                    return ()}]
_revII x0 x1 = msum [do {let {x6 = A};
                         let {x8 = B};
                         let {x10 = C};
                         let {x12 = D};
                         let {x13 = Nil};
                         let {x11 = Cons x12 x13};
                         let {x9 = Cons x10 x11};
                         let {x7 = Cons x8 x9};
                         (x14, x15) <- case x1 of
                                       {Cons y14 y15 -> return (y14, y15); _ -> mzero};
                         guard (x14 == x6);
                         guard (x15 == x7);
                         guard (x0 == Nil);
                         return ()},
                     do {(x2, x3) <- case x0 of
                                     {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                         let {x16 = Cons x2 x1};
                         _revII x3 x16;
                         return ()}]
revO gen__revOI_x2 gen_revO_x1 = msum [do {let {x5 = Nil};
                                           (x4, x1) <- do {x1 <- gen_revO_x1;
                                                           let {x4 = Cons x1 x5};
                                                           return (x4, x1)};
                                           x2 <- _revOI x4 gen__revOI_x2;
                                           let {x0 = Cons x1 x2};
                                           return x0}]
_revOI x1 gen__revOI_x2 = msum [do {let {x6 = A};
                                    let {x8 = B};
                                    let {x10 = C};
                                    let {x12 = D};
                                    let {x13 = Nil};
                                    let {x11 = Cons x12 x13};
                                    let {x9 = Cons x10 x11};
                                    let {x7 = Cons x8 x9};
                                    let {x0 = Nil};
                                    (x14, x15) <- case x1 of
                                                  {Cons y14 y15 -> return (y14, y15); _ -> mzero};
                                    guard (x14 == x6);
                                    guard (x15 == x7);
                                    return x0},
                                do {(x16, x2) <- do {x2 <- gen__revOI_x2;
                                                     let {x16 = Cons x2 x1};
                                                     return (x16, x2)};
                                    x3 <- _revOI x16 gen__revOI_x2;
                                    let {x0 = Cons x2 x3};
                                    return x0}]