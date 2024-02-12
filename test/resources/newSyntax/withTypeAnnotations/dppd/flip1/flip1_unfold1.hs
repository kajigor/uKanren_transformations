module Flip where

import Stream
import Control.Monad

data Term
    = Leaf Term
    | Tree Term Term Term
    deriving (Show, Eq)
flipflipII x0 x1 = msum [do {x2 <- case x1 of
                                   {Leaf y2 -> return y2; _ -> mzero};
                             x8 <- case x0 of
                                   {Leaf y8 -> return y8; _ -> mzero};
                             guard (x8 == x2);
                             return ()},
                         do {(x3, x4, x5) <- case x1 of
                                             {Tree y3 y4 y5 -> return (y3, y4, y5); _ -> mzero};
                             (x7, x9, x10) <- case x0 of
                                              {Tree y7 y9 y10 -> return (y7, y9, y10); _ -> mzero};
                             guard (x9 == x4);
                             _flipFlipII x7 x3;
                             let {x6 = x10};
                             _flipFlipII x6 x5;
                             return ()}]
_flipFlipII x0 x1 = msum [do {x2 <- case x1 of
                                    {Leaf y2 -> return y2; _ -> mzero};
                              x11 <- case x0 of
                                     {Leaf y11 -> return y11; _ -> mzero};
                              guard (x11 == x2);
                              return ()},
                          do {(x3, x4, x5) <- case x1 of
                                              {Tree y3 y4 y5 -> return (y3, y4, y5); _ -> mzero};
                              (x7, x12, x13) <- case x0 of
                                                {Tree y7 y12 y13 -> return (y7, y12, y13);
                                                 _ -> mzero};
                              guard (x12 == x4);
                              _flipFlipII x7 x3;
                              let {x6 = x13};
                              _flipFlipII x6 x5;
                              return ()}]
flipflipIO x0 = msum [do {x8 <- case x0 of
                                {Leaf y8 -> return y8; _ -> mzero};
                          let {x2 = x8};
                          let {x1 = Leaf x2};
                          return x1},
                      do {(x7, x9, x10) <- case x0 of
                                           {Tree y7 y9 y10 -> return (y7, y9, y10); _ -> mzero};
                          let {x4 = x9};
                          let {x6 = x10};
                          x5 <- _flipFlipIO x6;
                          x3 <- _flipFlipIO x7;
                          let {x1 = Tree x3 x4 x5};
                          return x1}]
_flipFlipIO x0 = msum [do {x11 <- case x0 of
                                  {Leaf y11 -> return y11; _ -> mzero};
                           let {x2 = x11};
                           let {x1 = Leaf x2};
                           return x1},
                       do {(x7, x12, x13) <- case x0 of
                                             {Tree y7 y12 y13 -> return (y7, y12, y13); _ -> mzero};
                           let {x4 = x12};
                           let {x6 = x13};
                           x5 <- _flipFlipIO x6;
                           x3 <- _flipFlipIO x7;
                           let {x1 = Tree x3 x4 x5};
                           return x1}]
flipflipOI x1 = msum [do {x2 <- case x1 of
                                {Leaf y2 -> return y2; _ -> mzero};
                          let {x8 = x2};
                          let {x0 = Leaf x8};
                          return x0},
                      do {(x3, x4, x5) <- case x1 of
                                          {Tree y3 y4 y5 -> return (y3, y4, y5); _ -> mzero};
                          let {x9 = x4};
                          x6 <- _flipFlipOI x5;
                          let {x10 = x6};
                          x7 <- _flipFlipOI x3;
                          let {x0 = Tree x7 x9 x10};
                          return x0}]
_flipFlipOI x1 = msum [do {x2 <- case x1 of
                                 {Leaf y2 -> return y2; _ -> mzero};
                           let {x11 = x2};
                           let {x0 = Leaf x11};
                           return x0},
                       do {(x3, x4, x5) <- case x1 of
                                           {Tree y3 y4 y5 -> return (y3, y4, y5); _ -> mzero};
                           let {x12 = x4};
                           x6 <- _flipFlipOI x5;
                           let {x13 = x6};
                           x7 <- _flipFlipOI x3;
                           let {x0 = Tree x7 x12 x13};
                           return x0}]
flipflipOO gen_flipflipOO_x2 gen_flipflipOO_x3 gen_flipflipOO_x4 gen_flipflipOO_x6 = msum [do {(x8,
                                                                                                x2) <- do {x2 <- gen_flipflipOO_x2;
                                                                                                           return (x2,
                                                                                                                   x2)};
                                                                                               let {x1 = Leaf x2};
                                                                                               let {x0 = Leaf x8};
                                                                                               return (x0,
                                                                                                       x1)},
                                                                                           do {(x9,
                                                                                                x4) <- do {x4 <- gen_flipflipOO_x4;
                                                                                                           return (x4,
                                                                                                                   x4)};
                                                                                               (x10,
                                                                                                x6) <- do {x6 <- gen_flipflipOO_x6;
                                                                                                           return (x6,
                                                                                                                   x6)};
                                                                                               x5 <- _flipFlipIO x6;
                                                                                               (x1,
                                                                                                x3) <- do {x3 <- gen_flipflipOO_x3;
                                                                                                           let {x1 = Tree x3 x4 x5};
                                                                                                           return (x1,
                                                                                                                   x3)};
                                                                                               x7 <- _flipFlipOI x3;
                                                                                               let {x0 = Tree x7 x9 x10};
                                                                                               return (x0,
                                                                                                       x1)}]