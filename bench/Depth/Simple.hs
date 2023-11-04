{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

module Simple where 

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

data Term
    = Append Term Term Term
    | Cons Term Term
    | Member Term Term
    | Nil
    | O
    | S Term
    | Trueo
    deriving (Show, Eq, Generic, DS.NFData)
    
depthII x0 x1 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5 = msum [do {guard (x0 == Trueo);
                                                                    guard (x1 == O);
                                                                    return ()},
                                                                do {(x2, x3) <- case x0 of
                                                                                {Cons y2
                                                                                      y3 -> return (y2,
                                                                                                    y3);
                                                                                 _ -> mzero};
                                                                    x4 <- depthIO x2 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5;
                                                                    x5 <- depthIO x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5;
                                                                    maxIII x4 x5 x1;
                                                                    return ()},
                                                                do {x3 <- case x1 of
                                                                          {S y3 -> return y3;
                                                                           _ -> mzero};
                                                                    x2 <- prog_clauseIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5;
                                                                    depthII x2 x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5;
                                                                    return ()}]
depthIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5 = msum [do {guard (x0 == Trueo);
                                                                 let {x1 = O};
                                                                 return x1},
                                                             do {(x2, x3) <- case x0 of
                                                                             {Cons y2
                                                                                   y3 -> return (y2,
                                                                                                 y3);
                                                                              _ -> mzero};
                                                                 x4 <- depthIO x2 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5;
                                                                 x5 <- depthIO x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5;
                                                                 x1 <- maxIIO x4 x5;
                                                                 return x1},
                                                             do {x2 <- prog_clauseIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5;
                                                                 x3 <- depthIO x2 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5;
                                                                 let {x1 = S x3};
                                                                 return x1}]
depthOI x1 gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5 gen_prog_clauseOI_x2 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x5 = msum [do {let {x0 = Trueo};
                                                                                                                                                                                                                          guard (x1 == O);
                                                                                                                                                                                                                          return x0},
                                                                                                                                                                                                                      do {(x2,
                                                                                                                                                                                                                           x4) <- depthOO gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x5;
                                                                                                                                                                                                                          x5 <- maxIOI x4 x1;
                                                                                                                                                                                                                          x3 <- depthOI x5 gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5 gen_prog_clauseOI_x2 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x5;
                                                                                                                                                                                                                          let {x0 = Cons x2 x3};
                                                                                                                                                                                                                          return x0},
                                                                                                                                                                                                                      do {x3 <- case x1 of
                                                                                                                                                                                                                                {S y3 -> return y3;
                                                                                                                                                                                                                                 _ -> mzero};
                                                                                                                                                                                                                          x2 <- depthOI x3 gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5 gen_prog_clauseOI_x2 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x5;
                                                                                                                                                                                                                          x0 <- prog_clauseOI x2 gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseOI_x2;
                                                                                                                                                                                                                          return x0}]
depthOO gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x5 = msum [do {let {x0 = Trueo};
                                                                                                                                                                                                  let {x1 = O};
                                                                                                                                                                                                  return (x0,
                                                                                                                                                                                                          x1)},
                                                                                                                                                                                              do {(x2,
                                                                                                                                                                                                   x4) <- depthOO gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x5;
                                                                                                                                                                                                  (x3,
                                                                                                                                                                                                   x5) <- depthOO gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x5;
                                                                                                                                                                                                  let {x0 = Cons x2 x3};
                                                                                                                                                                                                  x1 <- maxIIO x4 x5;
                                                                                                                                                                                                  return (x0,
                                                                                                                                                                                                          x1)},
                                                                                                                                                                                              do {(x0,
                                                                                                                                                                                                   x2) <- prog_clauseOO gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x5;
                                                                                                                                                                                                  x3 <- depthIO x2 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5;
                                                                                                                                                                                                  let {x1 = S x3};
                                                                                                                                                                                                  return (x0,
                                                                                                                                                                                                          x1)}]
maxIII x0 x1 x2 = msum [do {guard (x1 == O);
                            guard (x0 == x2);
                            return ()},
                        do {guard (x0 == O); guard (x1 == x2); return ()},
                        do {x3 <- case x0 of
                                  {S y3 -> return y3; _ -> mzero};
                            x4 <- case x1 of
                                  {S y4 -> return y4; _ -> mzero};
                            x5 <- case x2 of
                                  {S y5 -> return y5; _ -> mzero};
                            maxIII x3 x4 x5;
                            return ()}]
maxIIO x0 x1 = msum [do {guard (x1 == O);
                         let {x2 = x0};
                         return x2},
                     do {guard (x0 == O); let {x2 = x1}; return x2},
                     do {x3 <- case x0 of
                               {S y3 -> return y3; _ -> mzero};
                         x4 <- case x1 of
                               {S y4 -> return y4; _ -> mzero};
                         x5 <- maxIIO x3 x4;
                         let {x2 = S x5};
                         return x2}]
maxIOI x0 x2 = msum [do {let {x1 = O};
                         guard (x0 == x2);
                         return x1},
                     do {guard (x0 == O); let {x1 = x2}; return x1},
                     do {x3 <- case x0 of
                               {S y3 -> return y3; _ -> mzero};
                         x5 <- case x2 of
                               {S y5 -> return y5; _ -> mzero};
                         x4 <- maxIOI x3 x5;
                         let {x1 = S x4};
                         return x1}]
prog_clauseIO x0 gen_prog_clauseIO_x4 gen_prog_clauseIO_x5 = msum [do {(x2,
                                                                        x3) <- case x0 of
                                                                               {Member y2
                                                                                       y3 -> return (y2,
                                                                                                     y3);
                                                                                _ -> mzero};
                                                                       (x6,
                                                                        x5) <- do {x5 <- gen_prog_clauseIO_x5;
                                                                                   let {x6 = Cons x2 x5};
                                                                                   return (x6, x5)};
                                                                       (x1,
                                                                        x4) <- do {x4 <- gen_prog_clauseIO_x4;
                                                                                   let {x1 = Append x4 x6 x3};
                                                                                   return (x1, x4)};
                                                                       return x1},
                                                                   do {let {x1 = Trueo};
                                                                       (x2,
                                                                        x3,
                                                                        x4) <- prog_clause0IOOO x0;
                                                                       guard (x2 == x3);
                                                                       return x1},
                                                                   do {(x6, x4, x7) <- case x0 of
                                                                                       {Append y6
                                                                                               y4
                                                                                               y7 -> return (y6,
                                                                                                             y4,
                                                                                                             y7);
                                                                                        _ -> mzero};
                                                                       (x2, x3) <- case x6 of
                                                                                   {Cons y2
                                                                                         y3 -> return (y2,
                                                                                                       y3);
                                                                                    _ -> mzero};
                                                                       x5 <- case x7 of
                                                                             {Cons y2
                                                                                   y5 -> do {guard (x2 == y2);
                                                                                             return y5};
                                                                              _ -> mzero};
                                                                       let {x1 = Append x3 x4 x5};
                                                                       return x1}]
prog_clauseOI x1 gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseOI_x2 = msum [do {(x4,
                                                                                                   x6,
                                                                                                   x3) <- case x1 of
                                                                                                          {Append y4
                                                                                                                  y6
                                                                                                                  y3 -> return (y4,
                                                                                                                                y6,
                                                                                                                                y3);
                                                                                                           _ -> mzero};
                                                                                                  (x2,
                                                                                                   x5) <- case x6 of
                                                                                                          {Cons y2
                                                                                                                y5 -> return (y2,
                                                                                                                              y5);
                                                                                                           _ -> mzero};
                                                                                                  let {x0 = Member x2 x3};
                                                                                                  return x0},
                                                                                              do {guard (x1 == Trueo);
                                                                                                  (x0,
                                                                                                   x2,
                                                                                                   x3,
                                                                                                   x4) <- prog_clause0OOOO gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3;
                                                                                                  guard (x2 == x3);
                                                                                                  return x0},
                                                                                              do {(x3,
                                                                                                   x4,
                                                                                                   x5) <- case x1 of
                                                                                                          {Append y3
                                                                                                                  y4
                                                                                                                  y5 -> return (y3,
                                                                                                                                y4,
                                                                                                                                y5);
                                                                                                           _ -> mzero};
                                                                                                  (x6,
                                                                                                   x2) <- do {x2 <- gen_prog_clauseOI_x2;
                                                                                                              let {x6 = Cons x2 x3};
                                                                                                              return (x6,
                                                                                                                      x2)};
                                                                                                  let {x7 = Cons x2 x5};
                                                                                                  let {x0 = Append x6 x4 x7};
                                                                                                  return x0}]
prog_clauseOO gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 gen_prog_clauseOO_x2 gen_prog_clauseOO_x3 gen_prog_clauseOO_x4 gen_prog_clauseOO_x5 = msum [do {(x0,
                                                                                                                                                               x2,
                                                                                                                                                               x3) <- do {x2 <- gen_prog_clauseOO_x2;
                                                                                                                                                                          x3 <- gen_prog_clauseOO_x3;
                                                                                                                                                                          let {x0 = Member x2 x3};
                                                                                                                                                                          return (x0,
                                                                                                                                                                                  x2,
                                                                                                                                                                                  x3)};
                                                                                                                                                              (x6,
                                                                                                                                                               x5) <- do {x5 <- gen_prog_clauseOO_x5;
                                                                                                                                                                          let {x6 = Cons x2 x5};
                                                                                                                                                                          return (x6,
                                                                                                                                                                                  x5)};
                                                                                                                                                              (x1,
                                                                                                                                                               x4) <- do {x4 <- gen_prog_clauseOO_x4;
                                                                                                                                                                          let {x1 = Append x4 x6 x3};
                                                                                                                                                                          return (x1,
                                                                                                                                                                                  x4)};
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1)},
                                                                                                                                                          do {let {x1 = Trueo};
                                                                                                                                                              (x0,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x4) <- prog_clause0OOOO gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3;
                                                                                                                                                              guard (x2 == x3);
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1)},
                                                                                                                                                          do {(x6,
                                                                                                                                                               x2,
                                                                                                                                                               x3) <- do {x2 <- gen_prog_clauseOO_x2;
                                                                                                                                                                          x3 <- gen_prog_clauseOO_x3;
                                                                                                                                                                          let {x6 = Cons x2 x3};
                                                                                                                                                                          return (x6,
                                                                                                                                                                                  x2,
                                                                                                                                                                                  x3)};
                                                                                                                                                              (x7,
                                                                                                                                                               x5) <- do {x5 <- gen_prog_clauseOO_x5;
                                                                                                                                                                          let {x7 = Cons x2 x5};
                                                                                                                                                                          return (x7,
                                                                                                                                                                                  x5)};
                                                                                                                                                              (x0,
                                                                                                                                                               x4) <- do {x4 <- gen_prog_clauseOO_x4;
                                                                                                                                                                          let {x0 = Append x6 x4 x7};
                                                                                                                                                                          return (x0,
                                                                                                                                                                                  x4)};
                                                                                                                                                              let {x1 = Append x3 x4 x5};
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1)}]
prog_clause0IOOO x0 = msum [do {(x4, x2, x3) <- case x0 of
                                                {Append y4 y2 y3 -> return (y4, y2, y3);
                                                 _ -> mzero};
                                guard (x4 == Nil);
                                return (x2, x3, x4)}]
prog_clause0OOOO gen_prog_clause0OOOO_x2 gen_prog_clause0OOOO_x3 = msum [do {let {x4 = Nil};
                                                                             (x0,
                                                                              x2,
                                                                              x3) <- do {x2 <- gen_prog_clause0OOOO_x2;
                                                                                         x3 <- gen_prog_clause0OOOO_x3;
                                                                                         let {x0 = Append x4 x2 x3};
                                                                                         return (x0,
                                                                                                 x2,
                                                                                                 x3)};
                                                                             return (x0,
                                                                                     x2,
                                                                                     x3,
                                                                                     x4)}]