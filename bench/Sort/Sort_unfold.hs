module Sort_unfold where

import Stream
import Control.Monad
import Term

sortoI :: MonadPlus m => Term -> m ()
sortoI x0 = msum [do {let {x1 = Zero};
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
                                    {Cons y15 y16 -> return (y15, y16); _ -> mzero};
                      guard (x15 == x1);
                      guard (x16 == x2);
                      return ()}]

sortoO :: MonadPlus m => m Term 
sortoO = msum [do {let {x1 = Zero};
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