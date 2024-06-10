module Transpose_clean where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
transposeI x0 = msum [do {let {x4 = O};
                          let {x7 = O};
                          let {x6 = S x7};
                          let {x10 = O};
                          let {x9 = S x10};
                          let {x11 = Nil};
                          let {x8 = Cons x9 x11};
                          let {x5 = Cons x6 x8};
                          let {x3 = Cons x4 x5};
                          let {x15 = O};
                          let {x14 = S x15};
                          let {x19 = O};
                          let {x18 = S x19};
                          let {x17 = S x18};
                          let {x23 = O};
                          let {x22 = S x23};
                          let {x21 = S x22};
                          let {x28 = O};
                          let {x27 = S x28};
                          let {x26 = S x27};
                          let {x30 = O};
                          let {x32 = O};
                          let {x33 = Nil};
                          (x34, x35) <- case x0 of
                                        {Cons y34 y35 -> return (y34, y35); _ -> mzero};
                          guard (x34 == x3);
                          let {x12 = x35};
                          (x13, x24) <- case x12 of
                                        {Cons y13 y24 -> return (y13, y24); _ -> mzero};
                          x16 <- case x13 of
                                 {Cons y14 y16 -> do {guard (x14 == y14); return y16}; _ -> mzero};
                          x20 <- case x16 of
                                 {Cons y17 y20 -> do {guard (x17 == y17); return y20}; _ -> mzero};
                          x1 <- case x20 of
                                {Cons y21 y1 -> do {guard (x21 == y21); return y1}; _ -> mzero};
                          x25 <- case x24 of
                                 {Cons y25 y33 -> do {guard (x33 == y33); return y25}; _ -> mzero};
                          x29 <- case x25 of
                                 {Cons y26 y29 -> do {guard (x26 == y26); return y29}; _ -> mzero};
                          x31 <- case x29 of
                                 {Cons y30 y31 -> do {guard (x30 == y30); return y31}; _ -> mzero};
                          x2 <- case x31 of
                                {Cons y32 y2 -> do {guard (x32 == y32); return y2}; _ -> mzero};
                          nullrowsII x1 x2;
                          return ()}]
nullrowsII x0 x1 = msum [do {guard (x1 == Nil);
                             guard (x0 == Nil);
                             return ()}]
transposeO = msum [do {let {x4 = O};
                       let {x7 = O};
                       let {x6 = S x7};
                       let {x10 = O};
                       let {x9 = S x10};
                       let {x11 = Nil};
                       let {x8 = Cons x9 x11};
                       let {x5 = Cons x6 x8};
                       let {x3 = Cons x4 x5};
                       let {x15 = O};
                       let {x14 = S x15};
                       let {x19 = O};
                       let {x18 = S x19};
                       let {x17 = S x18};
                       let {x23 = O};
                       let {x22 = S x23};
                       let {x21 = S x22};
                       let {x28 = O};
                       let {x27 = S x28};
                       let {x26 = S x27};
                       let {x30 = O};
                       let {x32 = O};
                       let {x33 = Nil};
                       let {x34 = x3};
                       (x1, x2) <- nullrowsOO;
                       let {x20 = Cons x21 x1};
                       let {x16 = Cons x17 x20};
                       let {x13 = Cons x14 x16};
                       let {x31 = Cons x32 x2};
                       let {x29 = Cons x30 x31};
                       let {x25 = Cons x26 x29};
                       let {x24 = Cons x25 x33};
                       let {x12 = Cons x13 x24};
                       let {x35 = x12};
                       let {x0 = Cons x34 x35};
                       return x0}]
nullrowsOO = msum [do {let {x1 = Nil};
                       let {x0 = Nil};
                       return (x0, x1)}]