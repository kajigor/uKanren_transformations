module Depth_clean where

import Stream
import Control.Monad

data Term
    = O
    | S Term
    deriving (Show, Eq)
depthsdI x0 = msum [do {let {x7 = O};
                        let {x6 = S x7};
                        let {x5 = S x6};
                        let {x4 = S x5};
                        let {x3 = S x4};
                        let {x2 = S x3};
                        let {x1 = S x2};
                        x8 <- case x0 of
                              {S y8 -> return y8; _ -> mzero};
                        guard (x8 == x1);
                        return ()},
                    do {let {x17 = O};
                        let {x16 = S x17};
                        let {x15 = S x16};
                        let {x14 = S x15};
                        let {x13 = S x14};
                        let {x12 = S x13};
                        let {x11 = S x12};
                        let {x10 = S x11};
                        let {x9 = S x10};
                        x18 <- case x0 of
                               {S y18 -> return y18; _ -> mzero};
                        guard (x18 == x9);
                        return ()},
                    do {let {x30 = O};
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
                        x31 <- case x0 of
                               {S y31 -> return y31; _ -> mzero};
                        guard (x31 == x19);
                        return ()},
                    do {let {x44 = O};
                        let {x43 = S x44};
                        let {x42 = S x43};
                        let {x41 = S x42};
                        let {x40 = S x41};
                        let {x39 = S x40};
                        let {x38 = S x39};
                        let {x37 = S x38};
                        let {x36 = S x37};
                        let {x35 = S x36};
                        let {x34 = S x35};
                        let {x33 = S x34};
                        let {x32 = S x33};
                        x45 <- case x0 of
                               {S y45 -> return y45; _ -> mzero};
                        guard (x45 == x32);
                        return ()}]
depthsdO = msum [do {let {x7 = O};
                     let {x6 = S x7};
                     let {x5 = S x6};
                     let {x4 = S x5};
                     let {x3 = S x4};
                     let {x2 = S x3};
                     let {x1 = S x2};
                     let {x8 = x1};
                     let {x0 = S x8};
                     return x0},
                 do {let {x17 = O};
                     let {x16 = S x17};
                     let {x15 = S x16};
                     let {x14 = S x15};
                     let {x13 = S x14};
                     let {x12 = S x13};
                     let {x11 = S x12};
                     let {x10 = S x11};
                     let {x9 = S x10};
                     let {x18 = x9};
                     let {x0 = S x18};
                     return x0},
                 do {let {x30 = O};
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
                     let {x31 = x19};
                     let {x0 = S x31};
                     return x0},
                 do {let {x44 = O};
                     let {x43 = S x44};
                     let {x42 = S x43};
                     let {x41 = S x42};
                     let {x40 = S x41};
                     let {x39 = S x40};
                     let {x38 = S x39};
                     let {x37 = S x38};
                     let {x36 = S x37};
                     let {x35 = S x36};
                     let {x34 = S x35};
                     let {x33 = S x34};
                     let {x32 = S x33};
                     let {x45 = x32};
                     let {x0 = S x45};
                     return x0}]