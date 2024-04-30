module List4_unfold where

import Stream
import Control.Monad

data Term
    = Succ Term
    | Zero
    deriving (Show, Eq)
maxMinoII x0 x1 = msum [do {guard (x1 == Zero);
                            x2 <- case x0 of
                                  {Succ y2 -> return y2; _ -> mzero};
                            x3 <- case x2 of
                                  {Succ y3 -> return y3; _ -> mzero};
                            guard (x3 == Zero);
                            return ()}]
maxMinoIO x0 = msum [do {let {x1 = Zero};
                         x2 <- case x0 of
                               {Succ y2 -> return y2; _ -> mzero};
                         x3 <- case x2 of
                               {Succ y3 -> return y3; _ -> mzero};
                         guard (x3 == Zero);
                         return x1}]
maxMinoOI x1 = msum [do {guard (x1 == Zero);
                         let {x3 = Zero};
                         let {x2 = Succ x3};
                         let {x0 = Succ x2};
                         return x0}]
maxMinoOO = msum [do {let {x1 = Zero};
                      let {x3 = Zero};
                      let {x2 = Succ x3};
                      let {x0 = Succ x2};
                      return (x0, x1)}]