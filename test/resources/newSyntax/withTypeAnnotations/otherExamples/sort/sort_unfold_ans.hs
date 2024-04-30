module Sort_unfold where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | Succ Term
    | Zero
    deriving (Show, Eq)
    
sortoI x0 = msum [do {(x1, x2) <- case x0 of
                                  {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                      guard (x1 == Zero);
                      (x3, x4) <- case x2 of
                                  {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                      guard (x3 == Zero);
                      (x5, x7) <- case x4 of
                                  {Cons y5 y7 -> return (y5, y7); _ -> mzero};
                      x6 <- case x5 of
                            {Succ y6 -> return y6; _ -> mzero};
                      guard (x6 == Zero);
                      (x8, x10) <- case x7 of
                                   {Cons y8 y10 -> return (y8, y10); _ -> mzero};
                      x9 <- case x8 of
                            {Succ y9 -> return y9; _ -> mzero};
                      guard (x9 == Zero);
                      (x11, x14) <- case x10 of
                                    {Cons y11 y14 -> return (y11, y14); _ -> mzero};
                      x12 <- case x11 of
                             {Succ y12 -> return y12; _ -> mzero};
                      x13 <- case x12 of
                             {Succ y13 -> return y13; _ -> mzero};
                      guard (x13 == Zero);
                      guard (x14 == Nil);
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
                   let {x0 = Cons x1 x2};
                   return x0}]
                   
                   
main :: IO() 
main = 
  print (takeS 1 sortoO)