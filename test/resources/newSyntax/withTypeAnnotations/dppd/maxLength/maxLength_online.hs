module MaxLength_clean where

import Stream
import Control.Monad

data Term
    = Succ Term
    | Zero
    deriving (Show, Eq)
max_lengthII x0 x1 = msum [do {max1I x0;
                               let {x6 = Zero};
                               let {x5 = Succ x6};
                               let {x4 = Succ x5};
                               let {x3 = Succ x4};
                               let {x2 = Succ x3};
                               x7 <- case x1 of
                                     {Succ y7 -> return y7; _ -> mzero};
                               guard (x7 == x2);
                               return ()}]
max1I x0 = msum [do {_max1I x0; return ()}]
_max1I x0 = msum [do {__max1I x0; return ()}]
__max1I x0 = msum [do {___max1I x0; return ()}]
___max1I x0 = msum [do {let {x9 = Zero};
                        let {x8 = Succ x9};
                        x10 <- case x0 of
                               {Succ y10 -> return y10; _ -> mzero};
                        guard (x10 == x8);
                        return ()}]
max_lengthIO x0 = msum [do {max1I x0;
                            let {x6 = Zero};
                            let {x5 = Succ x6};
                            let {x4 = Succ x5};
                            let {x3 = Succ x4};
                            let {x2 = Succ x3};
                            let {x7 = x2};
                            let {x1 = Succ x7};
                            return x1}]
max_lengthOI x1 = msum [do {let {x6 = Zero};
                            let {x5 = Succ x6};
                            let {x4 = Succ x5};
                            let {x3 = Succ x4};
                            let {x2 = Succ x3};
                            x7 <- case x1 of
                                  {Succ y7 -> return y7; _ -> mzero};
                            guard (x7 == x2);
                            x0 <- max1O;
                            return x0}]
max1O = msum [do {x0 <- _max1O; return x0}]
_max1O = msum [do {x0 <- __max1O; return x0}]
__max1O = msum [do {x0 <- ___max1O; return x0}]
___max1O = msum [do {let {x9 = Zero};
                     let {x8 = Succ x9};
                     let {x10 = x8};
                     let {x0 = Succ x10};
                     return x0}]
max_lengthOO = msum [do {let {x6 = Zero};
                         let {x5 = Succ x6};
                         let {x4 = Succ x5};
                         let {x3 = Succ x4};
                         let {x2 = Succ x3};
                         let {x7 = x2};
                         let {x1 = Succ x7};
                         x0 <- max1O;
                         return (x0, x1)}]