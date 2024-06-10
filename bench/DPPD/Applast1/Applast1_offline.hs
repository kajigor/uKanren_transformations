module Applast1_offline where

import Stream
import Control.Monad
import Term

applastoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                     do {let {x6 = Zero};
                         let {x5 = Succ x6};
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
_appendoII x0 x1 = Immature $ msum [do {let {x15 = Zero};
                             let {x14 = Succ x15};
                             let {x16 = Nil};
                             (x17, x18) <- case x1 of
                                           {Cons y17 y18 -> return (y17, y18); _ -> mzero};
                             guard (x17 == x14);
                             guard (x18 == x16);
                             guard (x0 == Nil);
                             return ()},
                         do {(x2, x3) <- case x1 of
                                         {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                             (x19, x4) <- case x0 of
                                          {Cons y19 y4 -> return (y19, y4); _ -> mzero};
                             guard (x19 == x2);
                             _appendoII x4 x3;
                             return ()}]
appendoLastoI x0 = Immature $ msum [do {let {x11 = Zero};
                             let {x10 = Succ x11};
                             let {x12 = Nil};
                             let {x9 = Cons x10 x12};
                             lastoI x9;
                             guard (x0 == Nil);
                             return ()},
                         do {(x1, x2) <- case x0 of
                                         {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                             x3 <- _appendoIO x2;
                             let {x13 = Cons x1 x3};
                             lastoI x13;
                             return ()}]
_appendoIO x0 = Immature $ msum [do {let {x15 = Zero};
                          let {x14 = Succ x15};
                          let {x16 = Nil};
                          guard (x0 == Nil);
                          let {x17 = x14};
                          let {x18 = x16};
                          let {x1 = Cons x17 x18};
                          return x1},
                      do {(x19, x4) <- case x0 of
                                       {Cons y19 y4 -> return (y19, y4); _ -> mzero};
                          let {x2 = x19};
                          x3 <- _appendoIO x4;
                          let {x1 = Cons x2 x3};
                          return x1}]
applastoO gen_applastoO_x2 gen_lastoO_x1 = Immature $ msum [do {let {x0 = Nil};
                                                     return x0},
                                                 do {let {x6 = Zero};
                                                     let {x5 = Succ x6};
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
_appendoOI x1 = Immature $ msum [do {let {x15 = Zero};
                          let {x14 = Succ x15};
                          let {x16 = Nil};
                          let {x0 = Nil};
                          (x17, x18) <- case x1 of
                                        {Cons y17 y18 -> return (y17, y18); _ -> mzero};
                          guard (x17 == x14);
                          guard (x18 == x16);
                          return x0},
                      do {(x2, x3) <- case x1 of
                                      {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                          let {x19 = x2};
                          x4 <- _appendoOI x3;
                          let {x0 = Cons x19 x4};
                          return x0}]
appendoLastoO gen_lastoO_x1 = Immature $ msum [do {let {x0 = Nil};
                                        let {x11 = Zero};
                                        let {x10 = Succ x11};
                                        let {x12 = Nil};
                                        let {x9 = Cons x10 x12};
                                        lastoI x9;
                                        return x0},
                                    do {x13 <- lastoO gen_lastoO_x1;
                                        (x1, x3) <- case x13 of
                                                    {Cons y1 y3 -> return (y1, y3); _ -> mzero};
                                        x2 <- _appendoOI x3;
                                        let {x0 = Cons x1 x2};
                                        return x0}]
lastoI x0 = Immature $ msum [do {let {x21 = Zero};
                      let {x20 = Succ x21};
                      let {x22 = Nil};
                      (x23, x24) <- case x0 of
                                    {Cons y23 y24 -> return (y23, y24); _ -> mzero};
                      guard (x23 == x20);
                      guard (x24 == x22);
                      return ()},
                  do {(x1, x2) <- case x0 of
                                  {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                      lastoI x2;
                      return ()}]
lastoO gen_lastoO_x1 = Immature $ msum [do {let {x21 = Zero};
                                 let {x20 = Succ x21};
                                 let {x22 = Nil};
                                 let {x23 = x20};
                                 let {x24 = x22};
                                 let {x0 = Cons x23 x24};
                                 return x0},
                             do {x2 <- lastoO gen_lastoO_x1;
                                 (x0, x1) <- do {x1 <- gen_lastoO_x1;
                                                 let {x0 = Cons x1 x2};
                                                 return (x0, x1)};
                                 return x0}]