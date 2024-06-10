module Upto.sum1_clean where

import Stream
import Control.Monad

data Term
    = O
    | S Term
    deriving (Show, Eq)
sumsquaresuptosdI x0 = msum [do {let {x30 = O};
                                 let {x29 = S x30};
                                 let {x28 = S x29};
                                 let {x27 = S x28};
                                 let {x26 = S x27};
                                 let {x25 = S x26};
                                 let {x24 = S x25};
                                 let {x23 = S x24};
                                 let {x22 = S x23};
                                 let {x21 = S x22};
                                 let {x20 = S x21};
                                 let {x19 = S x20};
                                 let {x18 = S x19};
                                 let {x17 = S x18};
                                 let {x16 = S x17};
                                 let {x15 = S x16};
                                 let {x14 = S x15};
                                 let {x13 = S x14};
                                 let {x12 = S x13};
                                 let {x11 = S x12};
                                 let {x10 = S x11};
                                 let {x9 = S x10};
                                 let {x8 = S x9};
                                 let {x7 = S x8};
                                 let {x6 = S x7};
                                 let {x5 = S x6};
                                 let {x4 = S x5};
                                 let {x3 = S x4};
                                 let {x2 = S x3};
                                 let {x1 = S x2};
                                 x31 <- case x0 of
                                        {S y31 -> return y31; _ -> mzero};
                                 guard (x31 == x1);
                                 return ()}]
sumsquaresuptosdO = msum [do {let {x30 = O};
                              let {x29 = S x30};
                              let {x28 = S x29};
                              let {x27 = S x28};
                              let {x26 = S x27};
                              let {x25 = S x26};
                              let {x24 = S x25};
                              let {x23 = S x24};
                              let {x22 = S x23};
                              let {x21 = S x22};
                              let {x20 = S x21};
                              let {x19 = S x20};
                              let {x18 = S x19};
                              let {x17 = S x18};
                              let {x16 = S x17};
                              let {x15 = S x16};
                              let {x14 = S x15};
                              let {x13 = S x14};
                              let {x12 = S x13};
                              let {x11 = S x12};
                              let {x10 = S x11};
                              let {x9 = S x10};
                              let {x8 = S x9};
                              let {x7 = S x8};
                              let {x6 = S x7};
                              let {x5 = S x6};
                              let {x4 = S x5};
                              let {x3 = S x4};
                              let {x2 = S x3};
                              let {x1 = S x2};
                              let {x31 = x1};
                              let {x0 = S x31};
                              return x0}]