module NthOpt_cpd_ans where

import Stream
import Control.Monad (msum, guard, MonadPlus)
import Term

nthOptII :: MonadPlus m => Term -> Term -> m ()
nthOptII x0 x1 = msum [do {guard (x1 == None);
                           guard (x0 == Nil);
                           return ()},
                       do {(x2, x3) <- case x0 of
                                       {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                           _nthOptII x1 x3;
                           return ()}]
_nthOptII :: MonadPlus m => Term -> Term -> m ()
_nthOptII x0 x1 = msum [do {guard (x1 == Nil);
                            guard (x0 == None);
                            return ()},
                        do {(x2, x3) <- case x1 of
                                        {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                            __nthOptII x0 x3;
                            return ()}]
__nthOptII :: MonadPlus m => Term -> Term -> m ()
__nthOptII x0 x1 = msum [do {guard (x1 == Nil);
                             guard (x0 == None);
                             return ()},
                         do {(x2, x3) <- case x1 of
                                         {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                             ___nthOptII x0 x3;
                             return ()}]
___nthOptII :: MonadPlus m => Term -> Term -> m ()
___nthOptII x0 x1 = msum [do {guard (x1 == Nil);
                              guard (x0 == None);
                              return ()},
                          do {(x2, x3) <- case x1 of
                                          {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                              x4 <- case x0 of
                                    {Some y4 -> return y4; _ -> mzero};
                              guard (x4 == x2);
                              return ()}]
nthOptIO :: MonadPlus m => Term -> m Term
nthOptIO x0 = msum [do {let {x1 = None};
                        guard (x0 == Nil);
                        return x1},
                    do {(x2, x3) <- case x0 of
                                    {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                        x1 <- _nthOptOI x3;
                        return x1}]
_nthOptOI :: MonadPlus m => Term -> m Term
_nthOptOI x1 = msum [do {let {x0 = None};
                         guard (x1 == Nil);
                         return x0},
                     do {(x2, x3) <- case x1 of
                                     {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                         x0 <- __nthOptOI x3;
                         return x0}]
__nthOptOI :: MonadPlus m => Term -> m Term
__nthOptOI x1 = msum [do {let {x0 = None};
                          guard (x1 == Nil);
                          return x0},
                      do {(x2, x3) <- case x1 of
                                      {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                          x0 <- ___nthOptOI x3;
                          return x0}]
___nthOptOI :: MonadPlus m => Term -> m Term
___nthOptOI x1 = msum [do {let {x0 = None};
                           guard (x1 == Nil);
                           return x0},
                       do {(x2, x3) <- case x1 of
                                       {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                           let {x4 = x2};
                           let {x0 = Some x4};
                           return x0}]
nthOptOI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term
nthOptOI x1 gen____nthOptIO_x3 gen___nthOptIO_x2 gen__nthOptIO_x2 gen_nthOptOI_x2 = msum [do {let {x0 = Nil};
                                                                                              guard (x1 == None);
                                                                                              return x0},
                                                                                          do {x3 <- _nthOptIO x1 gen____nthOptIO_x3 gen___nthOptIO_x2 gen__nthOptIO_x2;
                                                                                              (x0,
                                                                                               x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                          let {x0 = Cons x2 x3};
                                                                                                          return (x0,
                                                                                                                  x2)};
                                                                                              return x0}]
_nthOptIO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term
_nthOptIO x0 gen____nthOptIO_x3 gen___nthOptIO_x2 gen__nthOptIO_x2 = msum [do {let {x1 = Nil};
                                                                               guard (x0 == None);
                                                                               return x1},
                                                                           do {x3 <- __nthOptIO x0 gen____nthOptIO_x3 gen___nthOptIO_x2;
                                                                               (x1,
                                                                                x2) <- do {x2 <- gen__nthOptIO_x2;
                                                                                           let {x1 = Cons x2 x3};
                                                                                           return (x1,
                                                                                                   x2)};
                                                                               return x1}]
__nthOptIO :: MonadPlus m => Term -> m Term -> m Term -> m Term
__nthOptIO x0 gen____nthOptIO_x3 gen___nthOptIO_x2 = msum [do {let {x1 = Nil};
                                                               guard (x0 == None);
                                                               return x1},
                                                           do {x3 <- ___nthOptIO x0 gen____nthOptIO_x3;
                                                               (x1,
                                                                x2) <- do {x2 <- gen___nthOptIO_x2;
                                                                           let {x1 = Cons x2 x3};
                                                                           return (x1, x2)};
                                                               return x1}]
___nthOptIO :: MonadPlus m => Term -> m Term -> m Term
___nthOptIO x0 gen____nthOptIO_x3 = msum [do {let {x1 = Nil};
                                              guard (x0 == None);
                                              return x1},
                                          do {x4 <- case x0 of
                                                    {Some y4 -> return y4; _ -> mzero};
                                              let {x2 = x4};
                                              (x1, x3) <- do {x3 <- gen____nthOptIO_x3;
                                                              let {x1 = Cons x2 x3};
                                                              return (x1, x3)};
                                              return x1}]
nthOptOO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
nthOptOO gen____nthOptOO_x2 gen____nthOptOO_x3 gen___nthOptOO_x2 gen__nthOptOO_x2 gen_nthOptOO_x2 = msum [do {let {x1 = None};
                                                                                                              let {x0 = Nil};
                                                                                                              return (x0,
                                                                                                                      x1)},
                                                                                                          do {(x1,
                                                                                                               x3) <- _nthOptOO gen____nthOptOO_x2 gen____nthOptOO_x3 gen___nthOptOO_x2 gen__nthOptOO_x2;
                                                                                                              (x0,
                                                                                                               x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                                          let {x0 = Cons x2 x3};
                                                                                                                          return (x0,
                                                                                                                                  x2)};
                                                                                                              return (x0,
                                                                                                                      x1)}]
_nthOptOO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m (Term, Term)
_nthOptOO gen____nthOptOO_x2 gen____nthOptOO_x3 gen___nthOptOO_x2 gen__nthOptOO_x2 = msum [do {let {x1 = Nil};
                                                                                               let {x0 = None};
                                                                                               return (x0,
                                                                                                       x1)},
                                                                                           do {(x0,
                                                                                                x3) <- __nthOptOO gen____nthOptOO_x2 gen____nthOptOO_x3 gen___nthOptOO_x2;
                                                                                               (x1,
                                                                                                x2) <- do {x2 <- gen__nthOptOO_x2;
                                                                                                           let {x1 = Cons x2 x3};
                                                                                                           return (x1,
                                                                                                                   x2)};
                                                                                               return (x0,
                                                                                                       x1)}]
__nthOptOO :: MonadPlus m => m Term -> m Term -> m Term -> m (Term, Term)
__nthOptOO gen____nthOptOO_x2 gen____nthOptOO_x3 gen___nthOptOO_x2 = msum [do {let {x1 = Nil};
                                                                               let {x0 = None};
                                                                               return (x0, x1)},
                                                                           do {(x0,
                                                                                x3) <- ___nthOptOO gen____nthOptOO_x2 gen____nthOptOO_x3;
                                                                               (x1,
                                                                                x2) <- do {x2 <- gen___nthOptOO_x2;
                                                                                           let {x1 = Cons x2 x3};
                                                                                           return (x1,
                                                                                                   x2)};
                                                                               return (x0, x1)}]
___nthOptOO :: MonadPlus m => m Term -> m Term -> m (Term, Term)
___nthOptOO gen____nthOptOO_x2 gen____nthOptOO_x3 = msum [do {let {x1 = Nil};
                                                              let {x0 = None};
                                                              return (x0, x1)},
                                                          do {(x4,
                                                               x2) <- do {x2 <- gen____nthOptOO_x2;
                                                                          return (x2, x2)};
                                                              let {x0 = Some x4};
                                                              (x1,
                                                               x3) <- do {x3 <- gen____nthOptOO_x3;
                                                                          let {x1 = Cons x2 x3};
                                                                          return (x1, x3)};
                                                              return (x0, x1)}]