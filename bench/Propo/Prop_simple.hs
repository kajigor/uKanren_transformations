module Prop_simple where

import Stream
import Control.Monad
import Term


evaloIOI x0 x2 = Immature $ msum [do {let {x8 = x2};
                           let {x1 = Lit x8};
                           return x1},
                       do {x7 <- elemoOII x0 x2; let {x1 = Var x7}; return x1},
                       do {x5 <- notoOI x2;
                           x3 <- evaloIOI x0 x5;
                           let {x1 = Neg x3};
                           return x1},
                       do {(x5, x6) <- oroOOI x2;
                           x3 <- evaloIOI x0 x5;
                           x4 <- evaloIOI x0 x6;
                           let {x1 = Disj x3 x4};
                           return x1},
                       do {(x5, x6) <- andoOOI x2;
                           x3 <- evaloIOI x0 x5;
                           x4 <- evaloIOI x0 x6;
                           let {x1 = Conj x3 x4};
                           return x1},
                       do {(x5, x6) <- implicationoOOI x2;
                           x3 <- evaloIOI x0 x5;
                           x4 <- evaloIOI x0 x6;
                           let {x1 = Impl x3 x4};
                           return x1}]
andoOOI x2 = Immature $ msum [do {let {x0 = Trueo};
                       let {x1 = Trueo};
                       guard (x2 == Trueo);
                       return (x0, x1)},
                   do {let {x0 = Falso};
                       let {x1 = Trueo};
                       guard (x2 == Falso);
                       return (x0, x1)},
                   do {let {x0 = Trueo};
                       let {x1 = Falso};
                       guard (x2 == Falso);
                       return (x0, x1)},
                   do {let {x0 = Falso};
                       let {x1 = Falso};
                       guard (x2 == Falso);
                       return (x0, x1)}]
elemoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                           (x3, x4) <- case x1 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           guard (x2 == x3);
                           return x0},
                       do {(x3, x4) <- case x1 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           x5 <- elemoOII x4 x2;
                           let {x0 = Succ x5};
                           return x0}]
implicationoOOI x2 = Immature $ msum [do {let {x0 = Falso};
                               let {x1 = Trueo};
                               guard (x2 == Trueo);
                               return (x0, x1)},
                           do {let {x0 = Falso};
                               let {x1 = Falso};
                               guard (x2 == Trueo);
                               return (x0, x1)},
                           do {let {x0 = Trueo};
                               let {x1 = Trueo};
                               guard (x2 == Trueo);
                               return (x0, x1)},
                           do {let {x0 = Trueo};
                               let {x1 = Falso};
                               guard (x2 == Falso);
                               return (x0, x1)}]
notoOI x1 = Immature $ msum [do {let {x0 = Trueo};
                      guard (x1 == Falso);
                      return x0},
                  do {let {x0 = Falso}; guard (x1 == Trueo); return x0}]
oroOOI x2 = Immature $ msum [do {let {x0 = Trueo};
                      let {x1 = Trueo};
                      guard (x2 == Trueo);
                      return (x0, x1)},
                  do {let {x0 = Falso};
                      let {x1 = Trueo};
                      guard (x2 == Trueo);
                      return (x0, x1)},
                  do {let {x0 = Trueo};
                      let {x1 = Falso};
                      guard (x2 == Trueo);
                      return (x0, x1)},
                  do {let {x0 = Falso};
                      let {x1 = Falso};
                      guard (x2 == Falso);
                      return (x0, x1)}]
evalo x0 x2 = evaloIOI x0 x2