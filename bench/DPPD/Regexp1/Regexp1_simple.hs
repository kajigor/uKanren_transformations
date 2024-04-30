module Regexp1_simple where

import Stream
import Control.Monad
import Term

helpIIIIO x0 x1 x2 x3 gen_generate1IOIIO_x4 = Immature $ msum [do {let {x11 = Char x0};
                                                        let {x12 = Char x1};
                                                        let {x10 = Or x11 x12};
                                                        let {x14 = Char x2};
                                                        let {x15 = Char x3};
                                                        let {x13 = Or x14 x15};
                                                        let {x9 = Cat x10 x13};
                                                        let {x8 = Star x9};
                                                        let {x16 = Nil};
                                                        x4 <- generateIOI x8 x16 gen_generate1IOIIO_x4;
                                                        return x4}]
generateIOI x0 x2 gen_generate1IOIIO_x4 = Immature $ msum [do {guard (x0 == Term.Empty);
                                                    let {x1 = x2};
                                                    return x1},
                                                do {x3 <- case x0 of
                                                          {Char y3 -> return y3; _ -> mzero};
                                                    let {x6 = x3};
                                                    let {x7 = x2};
                                                    let {x1 = Cons x6 x7};
                                                    return x1},
                                                do {(x3, x4) <- case x0 of
                                                                {Or y3 y4 -> return (y3, y4);
                                                                 _ -> mzero};
                                                    x1 <- generate0OIII x2 x3 x4 gen_generate1IOIIO_x4;
                                                    return x1},
                                                do {(x3, x4) <- case x0 of
                                                                {Cat y3 y4 -> return (y3, y4);
                                                                 _ -> mzero};
                                                    x5 <- generateIOI x4 x2 gen_generate1IOIIO_x4;
                                                    x1 <- generateIOI x3 x5 gen_generate1IOIIO_x4;
                                                    return x1},
                                                do {x3 <- case x0 of
                                                          {Star y3 -> return y3; _ -> mzero};
                                                    (x1,
                                                     x4) <- generate1IOIIO x0 x2 x3 gen_generate1IOIIO_x4;
                                                    return x1}]
generate0OIII x2 x3 x4 gen_generate1IOIIO_x4 = Immature $ msum [do {x1 <- generateIOI x3 x2 gen_generate1IOIIO_x4;
                                                         return x1},
                                                     do {x1 <- generateIOI x4 x2 gen_generate1IOIIO_x4;
                                                         return x1}]
generate1IOIIO x0 x2 x3 gen_generate1IOIIO_x4 = Immature $ msum [do {let {x1 = x2};
                                                          x4 <- gen_generate1IOIIO_x4;
                                                          return (x1, x4)},
                                                      do {x4 <- generateIOI x0 x2 gen_generate1IOIIO_x4;
                                                          x1 <- generateIOI x3 x4 gen_generate1IOIIO_x4;
                                                          return (x1, x4)}]
help x0 x1 x2 x3 gen_generate1IOIIO_x4 = helpIIIIO x0 x1 x2 x3 gen_generate1IOIIO_x4