module Multiply where

import Stream
import Control.Monad

data Term
    = Succ Term
    | Zero
    deriving (Show, Eq)
multiplyII x0 x1 = msum [do {let {x5 = Zero};
                             x2 <- case x1 of
                                   {Succ y2 -> return y2; _ -> mzero};
                             multiplyII x5 x2;
                             guard (x0 == Zero);
                             return ()},
                         do {let {x10 = Zero};
                             x9 <- case x1 of
                                   {Succ y9 -> return y9; _ -> mzero};
                             x11 <- case x0 of
                                    {Succ y11 -> return y11; _ -> mzero};
                             guard (x11 == x10);
                             let {x6 = x9};
                             x7 <- case x6 of
                                   {Succ y7 -> return y7; _ -> mzero};
                             x8 <- case x7 of
                                   {Succ y8 -> return y8; _ -> mzero};
                             x3 <- case x8 of
                                   {Succ y3 -> return y3; _ -> mzero};
                             _multiplyI x3;
                             return ()},
                         do {let {x15 = Zero};
                             let {x14 = Succ x15};
                             x13 <- case x1 of
                                    {Succ y13 -> return y13; _ -> mzero};
                             x16 <- case x0 of
                                    {Succ y16 -> return y16; _ -> mzero};
                             guard (x16 == x14);
                             let {x12 = x13};
                             x4 <- case x12 of
                                   {Succ y4 -> return y4; _ -> mzero};
                             __multiplyI x4;
                             return ()},
                         do {let {x20 = Zero};
                             let {x19 = Succ x20};
                             let {x18 = Succ x19};
                             let {x17 = Succ x18};
                             x2 <- case x1 of
                                   {Succ y2 -> return y2; _ -> mzero};
                             ___multiplyI x2;
                             x21 <- case x0 of
                                    {Succ y21 -> return y21; _ -> mzero};
                             guard (x21 == x17);
                             return ()}]
___multiplyI x0 = msum [do {guard (x0 == Zero); return ()}]
__multiplyI x0 = msum [do {guard (x0 == Zero); return ()}]
_multiplyI x0 = msum [do {guard (x0 == Zero); return ()}]
multiplyIO x0 = msum [do {let {x5 = Zero};
                          guard (x0 == Zero);
                          x2 <- multiplyIO x5;
                          let {x1 = Succ x2};
                          return x1},
                      do {let {x10 = Zero};
                          x11 <- case x0 of
                                 {Succ y11 -> return y11; _ -> mzero};
                          guard (x11 == x10);
                          x3 <- _multiplyO;
                          let {x8 = Succ x3};
                          let {x7 = Succ x8};
                          let {x6 = Succ x7};
                          let {x9 = x6};
                          let {x1 = Succ x9};
                          return x1},
                      do {let {x15 = Zero};
                          let {x14 = Succ x15};
                          x16 <- case x0 of
                                 {Succ y16 -> return y16; _ -> mzero};
                          guard (x16 == x14);
                          x4 <- __multiplyO;
                          let {x12 = Succ x4};
                          let {x13 = x12};
                          let {x1 = Succ x13};
                          return x1},
                      do {let {x20 = Zero};
                          let {x19 = Succ x20};
                          let {x18 = Succ x19};
                          let {x17 = Succ x18};
                          x21 <- case x0 of
                                 {Succ y21 -> return y21; _ -> mzero};
                          guard (x21 == x17);
                          x2 <- ___multiplyO;
                          let {x1 = Succ x2};
                          return x1}]
___multiplyO = msum [do {let {x0 = Zero}; return x0}]
__multiplyO = msum [do {let {x0 = Zero}; return x0}]
_multiplyO = msum [do {let {x0 = Zero}; return x0}]
multiplyOI x1 = msum [do {let {x0 = Zero};
                          let {x5 = Zero};
                          x2 <- case x1 of
                                {Succ y2 -> return y2; _ -> mzero};
                          multiplyII x5 x2;
                          return x0},
                      do {let {x10 = Zero};
                          x9 <- case x1 of
                                {Succ y9 -> return y9; _ -> mzero};
                          let {x6 = x9};
                          x7 <- case x6 of
                                {Succ y7 -> return y7; _ -> mzero};
                          x8 <- case x7 of
                                {Succ y8 -> return y8; _ -> mzero};
                          x3 <- case x8 of
                                {Succ y3 -> return y3; _ -> mzero};
                          _multiplyI x3;
                          let {x11 = x10};
                          let {x0 = Succ x11};
                          return x0},
                      do {let {x15 = Zero};
                          let {x14 = Succ x15};
                          x13 <- case x1 of
                                 {Succ y13 -> return y13; _ -> mzero};
                          let {x12 = x13};
                          x4 <- case x12 of
                                {Succ y4 -> return y4; _ -> mzero};
                          __multiplyI x4;
                          let {x16 = x14};
                          let {x0 = Succ x16};
                          return x0},
                      do {let {x20 = Zero};
                          let {x19 = Succ x20};
                          let {x18 = Succ x19};
                          let {x17 = Succ x18};
                          x2 <- case x1 of
                                {Succ y2 -> return y2; _ -> mzero};
                          ___multiplyI x2;
                          let {x21 = x17};
                          let {x0 = Succ x21};
                          return x0}]
multiplyOO = msum [do {let {x0 = Zero};
                       let {x5 = Zero};
                       x2 <- multiplyIO x5;
                       let {x1 = Succ x2};
                       return (x0, x1)},
                   do {let {x10 = Zero};
                       let {x11 = x10};
                       let {x0 = Succ x11};
                       x3 <- _multiplyO;
                       let {x8 = Succ x3};
                       let {x7 = Succ x8};
                       let {x6 = Succ x7};
                       let {x9 = x6};
                       let {x1 = Succ x9};
                       return (x0, x1)},
                   do {let {x15 = Zero};
                       let {x14 = Succ x15};
                       let {x16 = x14};
                       let {x0 = Succ x16};
                       x4 <- __multiplyO;
                       let {x12 = Succ x4};
                       let {x13 = x12};
                       let {x1 = Succ x13};
                       return (x0, x1)},
                   do {let {x20 = Zero};
                       let {x19 = Succ x20};
                       let {x18 = Succ x19};
                       let {x17 = Succ x18};
                       let {x21 = x17};
                       let {x0 = Succ x21};
                       x2 <- ___multiplyO;
                       let {x1 = Succ x2};
                       return (x0, x1)}]