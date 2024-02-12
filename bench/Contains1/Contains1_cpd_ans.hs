module Contains1_cpd_ans where

import Stream
import Control.Monad
import Term

containsoI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m ()
containsoI x0 gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8 = msum [do {containso0I x0;
                                                                                                                                                  (x1,
                                                                                                                                                   x2) <- newoConoOO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                                  return ()},
                                                                                                                                              do {(x3,
                                                                                                                                                   x4) <- case x0 of
                                                                                                                                                          {Cons y3
                                                                                                                                                                y4 -> return (y3,
                                                                                                                                                                              y4);
                                                                                                                                                           _ -> mzero};
                                                                                                                                                  cono1I x4 gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                                  return ()}]
cono1I :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m ()
cono1I x0 gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8 = msum [do {cono10I x0;
                                                                                                                                              (x1,
                                                                                                                                               x2) <- newoConoOO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                              return ()},
                                                                                                                                          do {(x3,
                                                                                                                                               x4) <- case x0 of
                                                                                                                                                      {Cons y3
                                                                                                                                                            y4 -> return (y3,
                                                                                                                                                                          y4);
                                                                                                                                                       _ -> mzero};
                                                                                                                                              cono1I x4 gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                              return ()}]
cono10I :: MonadPlus m => Term -> m ()
cono10I x0 = msum [do {(x10, x12) <- case x0 of
                                     {Cons y10 y12 -> return (y10, y12); _ -> mzero};
                       x11 <- case x10 of
                              {S y11 -> return y11; _ -> mzero};
                       guard (x11 == O);
                       (x1, x2) <- case x12 of
                                   {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                       return ()}]
containsoO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term
containsoO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8 gen_containso0O_x5 gen_containsoO_x3 = msum [do {x0 <- containso0O gen_containso0O_x5;
                                                                                                                                                                                    (x1,
                                                                                                                                                                                     x2) <- newoConoOO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                                                                    return x0},
                                                                                                                                                                                do {x4 <- cono1O gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                                                                    (x0,
                                                                                                                                                                                     x3) <- do {x3 <- gen_containsoO_x3;
                                                                                                                                                                                                let {x0 = Cons x3 x4};
                                                                                                                                                                                                return (x0,
                                                                                                                                                                                                        x3)};
                                                                                                                                                                                    return x0}]
cono1O :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m Term
cono1O gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8 = msum [do {x0 <- cono10O gen_cono10O_x12;
                                                                                                                                           (x1,
                                                                                                                                            x2) <- newoConoOO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                           return x0},
                                                                                                                                       do {x4 <- cono1O gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                           (x0,
                                                                                                                                            x3) <- do {x3 <- gen_cono1O_x3;
                                                                                                                                                       let {x0 = Cons x3 x4};
                                                                                                                                                       return (x0,
                                                                                                                                                               x3)};
                                                                                                                                           return x0}]
cono10O :: MonadPlus m => m Term -> m Term
cono10O gen_cono10O_x12 = msum [do {let {x11 = O};
                                    let {x10 = S x11};
                                    (x0, x12) <- do {x12 <- gen_cono10O_x12;
                                                     let {x0 = Cons x10 x12};
                                                     return (x0, x12)};
                                    (x1, x2) <- case x12 of
                                                {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                                    return x0}]
containso0I :: MonadPlus m => Term -> m ()
containso0I x0 = msum [do {(x3, x5) <- case x0 of
                                       {Cons y3 y5 -> return (y3, y5); _ -> mzero};
                           x4 <- case x3 of
                                 {S y4 -> return y4; _ -> mzero};
                           guard (x4 == O);
                           (x1, x2) <- case x5 of
                                       {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                           return ()}]
containso0O :: MonadPlus m => m Term -> m Term
containso0O gen_containso0O_x5 = msum [do {let {x4 = O};
                                           let {x3 = S x4};
                                           (x0, x5) <- do {x5 <- gen_containso0O_x5;
                                                           let {x0 = Cons x3 x5};
                                                           return (x0, x5)};
                                           (x1, x2) <- case x5 of
                                                       {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                                           return x0}]
newoConoOO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
newoConoOO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8 = msum [do {let {x0 = O};
                                                                                                                                               (x2,
                                                                                                                                                x3) <- newoCono1OO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                               let {x1 = Cons x2 x3};
                                                                                                                                               return (x0,
                                                                                                                                                       x1)},
                                                                                                                                           do {(x0,
                                                                                                                                                x1) <- appendo2Appendo2Appendo1Cono2OO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                               return (x0,
                                                                                                                                                       x1)}]
appendo2Appendo2Appendo1Cono2OO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
appendo2Appendo2Appendo1Cono2OO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8 = msum [do {x1 <- cono1O gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                                                    x0 <- gen_appendo2Appendo2Appendo1Cono2OO_x0;
                                                                                                                                                                    return (x0,
                                                                                                                                                                            x1)},
                                                                                                                                                                do {x0 <- appendo2O;
                                                                                                                                                                    (x2,
                                                                                                                                                                     x3) <- newoConoOO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                                                    let {x1 = Cons x2 x3};
                                                                                                                                                                    return (x0,
                                                                                                                                                                            x1)}]
appendo2O :: MonadPlus m => m Term
appendo2O = msum [do {let {x9 = O}; let {x0 = S x9}; return x0}]

newoCono1OO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
newoCono1OO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8 = msum [do {x0 <- newoCono10O;
                                                                                                                                                x1 <- conoO gen_conoO_x8;
                                                                                                                                                return (x0,
                                                                                                                                                        x1)},
                                                                                                                                            do {(x0,
                                                                                                                                                 x1) <- appendo2Appendo2Appendo1ConoOO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                                return (x0,
                                                                                                                                                        x1)}]
appendo2Appendo2Appendo1ConoOO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
appendo2Appendo2Appendo1ConoOO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8 = msum [do {x1 <- cono1O gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                                                   x0 <- gen_appendo2Appendo2Appendo1ConoOO_x0;
                                                                                                                                                                   return (x0,
                                                                                                                                                                           x1)},
                                                                                                                                                               do {(x0,
                                                                                                                                                                    x1) <- appendo2Appendo2Appendo1Cono1OO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                                                   return (x0,
                                                                                                                                                                           x1)}]
appendo2Appendo2Appendo1Cono1OO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
appendo2Appendo2Appendo1Cono1OO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8 = msum [do {x0 <- appendo2O;
                                                                                                                                                                    (x2,
                                                                                                                                                                     x3) <- newoConoOO gen_appendo2Appendo2Appendo1Cono2OO_x0 gen_appendo2Appendo2Appendo1ConoOO_x0 gen_cono10O_x12 gen_cono1O_x3 gen_conoO_x8;
                                                                                                                                                                    let {x1 = Cons x2 x3};
                                                                                                                                                                    return (x0,
                                                                                                                                                                            x1)}]
conoO :: (MonadPlus m, Eq a) => m a -> m a
conoO gen_conoO_x8 = msum [do {(x0, x8) <- do {x8 <- gen_conoO_x8;
                                               return (x8, x8)};
                               guard (x8 == x0);
                               return x0}]
newoCono10O :: MonadPlus m => m Term              
newoCono10O = msum [do {let {x7 = O};
                        let {x6 = S x7};
                        let {x0 = S x6};
                        return x0}]