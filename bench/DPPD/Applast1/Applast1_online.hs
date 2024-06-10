module Applast1_online where

import Stream
import Control.Monad
import Term

applastoI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                     do {(x1, x2) <- case x0 of
                                     {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                         appendoLastoII x1 x2;
                         return ()}]
appendoLastoII x0 x1 = Immature $ msum [do {guard (x1 == Nil); return ()},
                             do {(x2, x3) <- case x1 of
                                             {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                 appendoLastoII x2 x3;
                                 return ()}]
applastoO gen_appendoLastoOO_x0 = Immature $ msum [do {let {x0 = Nil};
                                            return x0},
                                        do {(x1, x2) <- appendoLastoOO gen_appendoLastoOO_x0;
                                            let {x0 = Cons x1 x2};
                                            return x0}]
appendoLastoOO gen_appendoLastoOO_x0 = Immature $ msum [do {let {x1 = Nil};
                                                 x0 <- gen_appendoLastoOO_x0;
                                                 return (x0, x1)},
                                             do {(x2, x3) <- appendoLastoOO gen_appendoLastoOO_x0;
                                                 let {x1 = Cons x2 x3};
                                                 x0 <- gen_appendoLastoOO_x0;
                                                 return (x0, x1)}]
