module List4_cpd_ans where

import Stream
import Control.Monad

data Term
    = Succ Term
    | Zero
    deriving (Show, Eq)
maxMinoII x0 x1 = msum [do {maxo1I x0; mino1I x1; return ()}]
maxMinoIO x0 = msum [do {maxo1I x0; x1 <- mino1O; return x1}]
maxMinoOI x1 = msum [do {x0 <- maxo1O; mino1I x1; return x0}]
maxMinoOO = msum [do {x0 <- maxo1O; x1 <- mino1O; return (x0, x1)}]
maxo1I x0 = msum [do {maxo1_1I x0; return ()}]
maxo1O = msum [do {x0 <- maxo1_1O; return x0}]
maxo1_1I x0 = msum [do {x2 <- case x0 of
                              {Succ y2 -> return y2; _ -> mzero};
                        x3 <- case x2 of
                              {Succ y3 -> return y3; _ -> mzero};
                        guard (x3 == Zero);
                        return ()}]
maxo1_1O = msum [do {let {x3 = Zero};
                     let {x2 = Succ x3};
                     let {x0 = Succ x2};
                     return x0}]
mino1I x0 = msum [do {mino1_1I x0; return ()}]
mino1O = msum [do {x0 <- mino1_1O; return x0}]
mino1_1I x0 = msum [do {guard (x0 == Zero); return ()}]
mino1_1O = msum [do {let {x0 = Zero}; return x0}]