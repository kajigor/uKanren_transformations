module DoubleAppend_simple where

import Stream
import Control.Monad (msum, guard, MonadPlus)
import Term

--a = Nil
--b = Nil
--c = Nil
--
--tp1 :: [()]
--tp1 = (takeS 1) $ double_appendoIII a b c
--tp2 :: [Term]
--tp2 = (takeS 1) $ double_appendoIIO a b
--tp3 :: [Term]
--tp3 = (takeS 1) $ double_appendoIOI a c
--tp4 :: [Term]
--tp4 = (takeS 1) $ double_appendoOII b c
--tp5 :: [(Term, Term)]
--tp5 = (takeS 1) $ double_appendoIOO a
--tp6 :: [(Term, Term)]
--tp6 = (takeS 1) $ double_appendoOIO b
--tp7 :: [(Term, Term)]
--tp7 = (takeS 1) $ double_appendoOOI c natGen natGen natGen natGen natGen natGen
--tp8 :: [(Term, Term, Term)]
--tp8 = (takeS 1) $ double_appendoOOO natGen natGen natGen natGen natGen natGen


double_appendoIIII x0 x1 x2 x3 = msum [do {x4 <- appendoIIO x0 x1;
                                           appendoIII x4 x2 x3;
                                           return ()}]
appendoIII x0 x1 x2 = msum [do {guard (x1 == x2);
                                guard (x0 == Nil);
                                return ()},
                            do {(x3, x4) <- case x0 of
                                            {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                (x6, x5) <- case x2 of
                                            {Cons y6 y5 -> return (y6, y5); _ -> mzero};
                                guard (x6 == x3);
                                appendoIII x4 x1 x5;
                                return ()}]
appendoIIO x0 x1 = msum [do {guard (x0 == Nil);
                             let {x2 = x1};
                             return x2},
                         do {(x3, x4) <- case x0 of
                                         {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                             let {x6 = x3};
                             x5 <- appendoIIO x4 x1;
                             let {x2 = Cons x6 x5};
                             return x2}]
double_appendoIIIO x0 x1 x2 = msum [do {x4 <- appendoIIO x0 x1;
                                        x3 <- appendoIIO x4 x2;
                                        return x3}]
double_appendoIIOI x0 x1 x3 = msum [do {x4 <- appendoIIO x0 x1;
                                        x2 <- appendoIOI x4 x3;
                                        return x2}]
appendoIOI x0 x2 = msum [do {guard (x0 == Nil);
                             let {x1 = x2};
                             return x1},
                         do {(x3, x4) <- case x0 of
                                         {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                             (x6, x5) <- case x2 of
                                         {Cons y6 y5 -> return (y6, y5); _ -> mzero};
                             guard (x6 == x3);
                             x1 <- appendoIOI x4 x5;
                             return x1}]
double_appendoIIOO x0 x1 gen_appendoIOO_x2 = msum [do {x4 <- appendoIIO x0 x1;
                                                       (x2, x3) <- appendoIOO x4 gen_appendoIOO_x2;
                                                       return (x2, x3)}]
appendoIOO x0 gen_appendoIOO_x2 = msum [do {guard (x0 == Nil);
                                            (x1, x2) <- do {x2 <- gen_appendoIOO_x2;
                                                            return (x2, x2)};
                                            return (x1, x2)},
                                        do {(x3, x4) <- case x0 of
                                                        {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                            let {x6 = x3};
                                            (x1, x5) <- appendoIOO x4 gen_appendoIOO_x2;
                                            let {x2 = Cons x6 x5};
                                            return (x1, x2)}]
double_appendoIOII x0 x2 x3 = msum [do {x4 <- appendoOII x2 x3;
                                        x1 <- appendoIOI x0 x4;
                                        return x1}]
appendoOII x1 x2 = msum [do {guard (x1 == x2);
                             let {x0 = Nil};
                             return x0},
                         do {(x6, x5) <- case x2 of
                                         {Cons y6 y5 -> return (y6, y5); _ -> mzero};
                             let {x3 = x6};
                             x4 <- appendoOII x1 x5;
                             let {x0 = Cons x3 x4};
                             return x0}]
double_appendoIOIO x0 x2 gen_appendoIOO_x2 = msum [do {(x1,
                                                        x4) <- appendoIOO x0 gen_appendoIOO_x2;
                                                       x3 <- appendoIIO x4 x2;
                                                       return (x1, x3)}]
double_appendoIOOI x0 x3 gen_appendoIOO_x2 = msum [do {(x1,
                                                        x4) <- appendoIOO x0 gen_appendoIOO_x2;
                                                       x2 <- appendoIOI x4 x3;
                                                       return (x1, x2)}]
double_appendoIOOO x0 gen_appendoIOO_x2 = msum [do {(x1,
                                                     x4) <- appendoIOO x0 gen_appendoIOO_x2;
                                                    (x2, x3) <- appendoIOO x4 gen_appendoIOO_x2;
                                                    return (x1, x2, x3)}]
double_appendoOIII x1 x2 x3 = msum [do {x4 <- appendoOII x2 x3;
                                        x0 <- appendoOII x1 x4;
                                        return x0}]
double_appendoOIIO x1 x2 gen_appendoOIO_x3 = msum [do {(x0,
                                                        x4) <- appendoOIO x1 gen_appendoOIO_x3;
                                                       x3 <- appendoIIO x4 x2;
                                                       return (x0, x3)}]
appendoOIO x1 gen_appendoOIO_x3 = msum [do {let {x0 = Nil};
                                            let {x2 = x1};
                                            return (x0, x2)},
                                        do {(x4, x5) <- appendoOIO x1 gen_appendoOIO_x3;
                                            (x6, x3) <- do {x3 <- gen_appendoOIO_x3;
                                                            return (x3, x3)};
                                            let {x0 = Cons x3 x4};
                                            let {x2 = Cons x6 x5};
                                            return (x0, x2)}]
double_appendoOIOI x1 x3 gen_appendoOIO_x3 = msum [do {(x0,
                                                        x4) <- appendoOIO x1 gen_appendoOIO_x3;
                                                       x2 <- appendoIOI x4 x3;
                                                       return (x0, x2)}]
double_appendoOIOO x1 gen_appendoIOO_x2 gen_appendoOIO_x3 = msum [do {(x0,
                                                                       x4) <- appendoOIO x1 gen_appendoOIO_x3;
                                                                      (x2,
                                                                       x3) <- appendoIOO x4 gen_appendoIOO_x2;
                                                                      return (x0, x2, x3)}]
double_appendoOOII x2 x3 = msum [do {x4 <- appendoOII x2 x3;
                                     (x0, x1) <- appendoOOI x4;
                                     return (x0, x1)}]
appendoOOI x2 = msum [do {let {x0 = Nil};
                          let {x1 = x2};
                          return (x0, x1)},
                      do {(x6, x5) <- case x2 of
                                      {Cons y6 y5 -> return (y6, y5); _ -> mzero};
                          let {x3 = x6};
                          (x4, x1) <- appendoOOI x5;
                          let {x0 = Cons x3 x4};
                          return (x0, x1)}]
double_appendoOOIO x2 gen_appendoOIO_x3 = msum [do {(x4,
                                                     x3) <- appendoOIO x2 gen_appendoOIO_x3;
                                                    (x0, x1) <- appendoOOI x4;
                                                    return (x0, x1, x3)}]
double_appendoOOOI x3 = msum [do {(x4, x2) <- appendoOOI x3;
                                  (x0, x1) <- appendoOOI x4;
                                  return (x0, x1, x2)}]
double_appendoOOOO gen_appendoIOO_x2 gen_appendoOOO_x2 gen_appendoOOO_x3 = msum [do {(x0,
                                                                                      x1,
                                                                                      x4) <- appendoOOO gen_appendoOOO_x2 gen_appendoOOO_x3;
                                                                                     (x2,
                                                                                      x3) <- appendoIOO x4 gen_appendoIOO_x2;
                                                                                     return (x0,
                                                                                             x1,
                                                                                             x2,
                                                                                             x3)}]
appendoOOO gen_appendoOOO_x2 gen_appendoOOO_x3 = msum [do {let {x0 = Nil};
                                                           (x1, x2) <- do {x2 <- gen_appendoOOO_x2;
                                                                           return (x2, x2)};
                                                           return (x0, x1, x2)},
                                                       do {(x4,
                                                            x1,
                                                            x5) <- appendoOOO gen_appendoOOO_x2 gen_appendoOOO_x3;
                                                           (x6, x3) <- do {x3 <- gen_appendoOOO_x3;
                                                                           return (x3, x3)};
                                                           let {x0 = Cons x3 x4};
                                                           let {x2 = Cons x6 x5};
                                                           return (x0, x1, x2)}]