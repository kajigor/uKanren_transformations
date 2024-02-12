module Sort_cpd_ans where

import Stream
import Control.Monad
import Term

sortoI :: MonadPlus m => Term -> m ()
sortoI x0 = msum [do {splitoSplitoSortoSortoAppendoSortoAppendoI x0;
                      return ()}]
                      
sortoO :: MonadPlus m => m Term
sortoO = msum [do {x0 <- splitoSplitoSortoSortoAppendoSortoAppendoO;
                   return x0}]
                   
splitoSplitoSortoSortoAppendoSortoAppendoI :: MonadPlus m => Term -> m ()
splitoSplitoSortoSortoAppendoSortoAppendoI x0 = msum [do {splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoI x0;
                                                          return ()}]
                                                          
splitoSplitoSortoSortoAppendoSortoAppendoO :: MonadPlus m => m Term
splitoSplitoSortoSortoAppendoSortoAppendoO = msum [do {x0 <- splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoO;
                                                       return x0}]
splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoI :: MonadPlus m => Term -> m ()
splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoI x0 = msum [do {_splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoI x0;
                                                                            return ()}]
_splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoI :: MonadPlus m => Term -> m ()
_splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoI x0 = msum [do {splitoSortoSortoAppendoAppendoI x0;
                                                                             return ()}]
splitoSortoSortoAppendoAppendoI :: MonadPlus m => Term -> m ()
splitoSortoSortoAppendoAppendoI x0 = msum [do {splitoSortoSplitoSortoSortoAppendoAppendoAppendoI x0;
                                               return ()}]
splitoSortoSplitoSortoSortoAppendoAppendoAppendoI :: MonadPlus m => Term -> m ()
splitoSortoSplitoSortoSortoAppendoAppendoAppendoI x0 = msum [do {let {x1 = Zero};
                                                                 let {x3 = Zero};
                                                                 let {x6 = Zero};
                                                                 let {x5 = Succ x6};
                                                                 let {x9 = Zero};
                                                                 let {x8 = Succ x9};
                                                                 let {x13 = Zero};
                                                                 let {x12 = Succ x13};
                                                                 let {x11 = Succ x12};
                                                                 let {x14 = Nil};
                                                                 let {x10 = Cons x11 x14};
                                                                 let {x7 = Cons x8 x10};
                                                                 let {x4 = Cons x5 x7};
                                                                 let {x2 = Cons x3 x4};
                                                                 (x15, x16) <- case x0 of
                                                                               {Cons y15
                                                                                     y16 -> return (y15,
                                                                                                    y16);
                                                                                _ -> mzero};
                                                                 guard (x15 == x1);
                                                                 guard (x16 == x2);
                                                                 return ()}]
splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoO :: MonadPlus m => m Term 
splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoO = msum [do {x0 <- _splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoO;
                                                                         return x0}]
_splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoO :: MonadPlus m => m Term 
_splitoSplitoSortoSortoAppendoSplitoSortoSortoAppendoAppendoO = msum [do {x0 <- splitoSortoSortoAppendoAppendoO;
                                                                          return x0}]
splitoSortoSortoAppendoAppendoO :: MonadPlus m => m Term 
splitoSortoSortoAppendoAppendoO = msum [do {x0 <- splitoSortoSplitoSortoSortoAppendoAppendoAppendoO;
                                            return x0}]
splitoSortoSplitoSortoSortoAppendoAppendoAppendoO :: MonadPlus m => m Term 
splitoSortoSplitoSortoSortoAppendoAppendoAppendoO = msum [do {let {x1 = Zero};
                                                              let {x3 = Zero};
                                                              let {x6 = Zero};
                                                              let {x5 = Succ x6};
                                                              let {x9 = Zero};
                                                              let {x8 = Succ x9};
                                                              let {x13 = Zero};
                                                              let {x12 = Succ x13};
                                                              let {x11 = Succ x12};
                                                              let {x14 = Nil};
                                                              let {x10 = Cons x11 x14};
                                                              let {x7 = Cons x8 x10};
                                                              let {x4 = Cons x5 x7};
                                                              let {x2 = Cons x3 x4};
                                                              let {x15 = x1};
                                                              let {x16 = x2};
                                                              let {x0 = Cons x15 x16};
                                                              return x0}]