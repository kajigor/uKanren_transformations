module Deforestation1_cpd_ans where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
rrI x0 = msum [do {rRI x0; return ()}]
rRI x0 = msum [do {rI x0; return ()}]
rI x0 = msum [do {(x1, x3) <- case x0 of
                              {Cons y1 y3 -> return (y1, y3); _ -> mzero};
                  x2 <- case x1 of
                        {S y2 -> return y2; _ -> mzero};
                  guard (x2 == O);
                  guard (x3 == Nil);
                  return ()}]
rrO = msum [do {x0 <- rRO; return x0}]
rRO = msum [do {x0 <- rO; return x0}]
rO = msum [do {let {x2 = O};
               let {x1 = S x2};
               let {x3 = Nil};
               let {x0 = Cons x1 x3};
               return x0}]