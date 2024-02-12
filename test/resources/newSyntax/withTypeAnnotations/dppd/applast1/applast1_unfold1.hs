module Applast1 where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
applastoI x0 = msum [do {let {x6 = O};
                         let {x5 = S x6};
                         let {x8 = Nil};
                         (x7, x1) <- case x0 of
                                     {Cons y7 y1 -> return (y7, y1); _ -> mzero};
                         guard (x7 == x5);
                         _appendoII x1 x8;
                         return ()},
                     do {(x2, x1) <- case x0 of
                                     {Cons y2 y1 -> return (y2, y1); _ -> mzero};
                         appendoLastoI x1;
                         return ()}]
_appendoII x0 x1 = msum [do {let {x10 = O};
                             let {x11 = Nil};
                             (x12, x13) <- case x1 of
                                           {Cons y12 y13 -> return (y12, y13); _ -> mzero};
                             guard (x12 == x10);
                             guard (x13 == x11);
                             guard (x0 == Nil);
                             return ()},
                         do {(x2, x3) <- case x1 of
                                         {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                             (x14, x4) <- case x0 of
                                          {Cons y14 y4 -> return (y14, y4); _ -> mzero};
                             guard (x14 == x2);
                             _appendoII x4 x3;
                             return ()}]
appendoLastoI x0 = msum [do {(x1, x2) <- case x0 of
                                         {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                             x3 <- _appendoIO x2;
                             let {x9 = Cons x1 x3};
                             lastoI x9;
                             return ()}]
_appendoIO x0 = msum [do {let {x10 = O};
                          let {x11 = Nil};
                          guard (x0 == Nil);
                          let {x12 = x10};
                          let {x13 = x11};
                          let {x1 = Cons x12 x13};
                          return x1},
                      do {(x14, x4) <- case x0 of
                                       {Cons y14 y4 -> return (y14, y4); _ -> mzero};
                          let {x2 = x14};
                          x3 <- _appendoIO x4;
                          let {x1 = Cons x2 x3};
                          return x1}]
applastoO gen_applastoO_x2 gen_lastoO_x1 = msum [do {let {x6 = O};
                                                     let {x5 = S x6};
                                                     let {x8 = Nil};
                                                     let {x7 = x5};
                                                     x1 <- _appendoOI x8;
                                                     let {x0 = Cons x7 x1};
                                                     return x0},
                                                 do {x1 <- appendoLastoO gen_lastoO_x1;
                                                     (x0, x2) <- do {x2 <- gen_applastoO_x2;
                                                                     let {x0 = Cons x2 x1};
                                                                     return (x0, x2)};
                                                     return x0}]
_appendoOI x1 = msum [do {let {x10 = O};
                          let {x11 = Nil};
                          let {x0 = Nil};
                          (x12, x13) <- case x1 of
                                        {Cons y12 y13 -> return (y12, y13); _ -> mzero};
                          guard (x12 == x10);
                          guard (x13 == x11);
                          return x0},
                      do {(x2, x3) <- case x1 of
                                      {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                          let {x14 = x2};
                          x4 <- _appendoOI x3;
                          let {x0 = Cons x14 x4};
                          return x0}]
appendoLastoO gen_lastoO_x1 = msum [do {x9 <- lastoO gen_lastoO_x1;
                                        (x1, x3) <- case x9 of
                                                    {Cons y1 y3 -> return (y1, y3); _ -> mzero};
                                        x2 <- _appendoOI x3;
                                        let {x0 = Cons x1 x2};
                                        return x0}]
lastoI x0 = msum [do {let {x16 = O};
                      let {x15 = S x16};
                      let {x17 = Nil};
                      (x18, x19) <- case x0 of
                                    {Cons y18 y19 -> return (y18, y19); _ -> mzero};
                      guard (x18 == x15);
                      guard (x19 == x17);
                      return ()},
                  do {(x1, x2) <- case x0 of
                                  {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                      lastoI x2;
                      return ()}]
lastoO gen_lastoO_x1 = msum [do {let {x16 = O};
                                 let {x15 = S x16};
                                 let {x17 = Nil};
                                 let {x18 = x15};
                                 let {x19 = x17};
                                 let {x0 = Cons x18 x19};
                                 return x0},
                             do {x2 <- lastoO gen_lastoO_x1;
                                 (x0, x1) <- do {x1 <- gen_lastoO_x1;
                                                 let {x0 = Cons x1 x2};
                                                 return (x0, x1)};
                                 return x0}]