module Upto_sum1_unfold where

import Term
import Stream
import Control.Monad

sumsquaresuptoI :: MonadPlus m => Term -> m ()
sumsquaresuptoI x0 = msum [do {uptoSquareSquaresSumI x0;
                               return ()}]
sumsquaresuptoO :: MonadPlus m => m Term 
sumsquaresuptoO = msum [do {x0 <- uptoSquareSquaresSumO;
                            return x0}]
uptoSquareSquaresSumI :: MonadPlus m => Term -> m ()
uptoSquareSquaresSumI x0 = msum [do {let {x8 = O};
                                     let {x7 = S x8};
                                     let {x6 = S x7};
                                     let {x13 = O};
                                     let {x12 = S x13};
                                     let {x11 = S x12};
                                     let {x10 = S x11};
                                     let {x9 = S x10};
                                     let {x24 = O};
                                     let {x23 = S x24};
                                     let {x22 = S x23};
                                     let {x21 = S x22};
                                     let {x20 = S x21};
                                     let {x19 = S x20};
                                     let {x18 = S x19};
                                     let {x17 = S x18};
                                     let {x16 = S x17};
                                     let {x15 = S x16};
                                     x14 <- sumIIO x0 x9;
                                     x25 <- case x14 of
                                            {Cons y15 y25 -> do {guard (x15 == y15); return y25};
                                             _ -> mzero};
                                     (x1, x2) <- case x25 of
                                                 {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                                     uptoSquareSquaresIII x6 x1 x2;
                                     return ()}]
sumIIO :: MonadPlus m => Term -> Term -> m Term
sumIIO x0 x1 = msum [do {let {x105 = O};
                         let {x104 = S x105};
                         guard (x1 == O);
                         x2 <- _sum1IOI x0 x104;
                         return x2},
                     do {x3 <- case x1 of
                               {S y3 -> return y3; _ -> mzero};
                         x2 <- _addSum1IOI x0 x3;
                         return x2}]
_addSum1IOI :: MonadPlus m => Term -> Term -> m Term
_addSum1IOI x0 x2 = msum [do {let {x108 = O};
                              let {x107 = S x108};
                              let {x106 = S x107};
                              guard (x2 == O);
                              x1 <- _sum1IOI x0 x106;
                              return x1},
                          do {let {x110 = O};
                              let {x109 = S x110};
                              x3 <- case x2 of
                                    {S y3 -> return y3; _ -> mzero};
                              x4 <- addIIO x109 x3;
                              let {x112 = S x4};
                              let {x111 = S x112};
                              x1 <- _sum1IOI x0 x111;
                              return x1}]
_sum1IOI :: MonadPlus m => Term -> Term -> m Term
_sum1IOI x0 x2 = msum [do {guard (x0 == x2);
                           let {x1 = Nil};
                           return x1},
                       do {(x3, x5) <- addIOO x2;
                           x4 <- _sum1IOI x0 x5;
                           let {x1 = Cons x3 x4};
                           return x1}]
addIIO :: MonadPlus m => Term -> Term -> m Term
addIIO x0 x1 = msum [do {guard (x1 == O);
                         let {x2 = x0};
                         return x2},
                     do {x4 <- case x1 of
                               {S y4 -> return y4; _ -> mzero};
                         x3 <- addIIO x0 x4;
                         let {x2 = S x3};
                         return x2}]
addIOO :: MonadPlus m => Term -> m (Term, Term)
addIOO x0 = msum [do {let {x1 = O};
                      let {x2 = x0};
                      return (x1, x2)},
                  do {(x4, x3) <- addIOO x0;
                      let {x2 = S x3};
                      let {x1 = S x4};
                      return (x1, x2)}]
uptoSquareSquaresIII :: MonadPlus m => Term -> Term -> Term -> m ()
uptoSquareSquaresIII x0 x1 x2 = msum [do {let {x28 = O};
                                          let {x27 = S x28};
                                          let {x26 = S x27};
                                          squareII x26 x1;
                                          let {x32 = O};
                                          let {x31 = S x32};
                                          let {x30 = S x31};
                                          let {x29 = S x30};
                                          (x3, x4) <- case x2 of
                                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                          uptoSquaresI x4;
                                          squareII x29 x3;
                                          guard (x0 == O);
                                          return ()},
                                      do {x5 <- case x0 of
                                                {S y5 -> return y5; _ -> mzero};
                                          leUptoSquareSquaresIII x1 x2 x5;
                                          return ()}]
leUptoSquareSquaresIII :: MonadPlus m => Term -> Term -> Term -> m ()
leUptoSquareSquaresIII x0 x1 x2 = msum [do {let {x90 = O};
                                            let {x89 = S x90};
                                            let {x88 = S x89};
                                            let {x87 = S x88};
                                            squareII x87 x0;
                                            let {x95 = O};
                                            let {x94 = S x95};
                                            let {x93 = S x94};
                                            let {x92 = S x93};
                                            let {x91 = S x92};
                                            guard (x2 == O);
                                            (x4, x86) <- case x1 of
                                                         {Cons y4 y86 -> return (y4, y86);
                                                          _ -> mzero};
                                            squareII x91 x4;
                                            let {x3 = x86};
                                            _uptoSquaresI x3;
                                            return ()},
                                        do {_uptoSquaresI x1;
                                            let {x96 = O};
                                            let {x102 = O};
                                            let {x101 = S x102};
                                            let {x100 = S x101};
                                            let {x99 = S x100};
                                            let {x98 = S x99};
                                            squareII x98 x0;
                                            x97 <- case x2 of
                                                   {S y97 -> return y97; _ -> mzero};
                                            guard (x97 == x96);
                                            return ()}]
_uptoSquaresI :: MonadPlus m => Term -> m ()
_uptoSquaresI x0 = msum [do {let {x103 = Nil};
                             squaresII x103 x0;
                             return ()}]
squareII :: MonadPlus m => Term -> Term -> m ()
squareII x0 x1 = msum [do {guard (x1 == O);
                           guard (x0 == O);
                           return ()},
                       do {x2 <- case x1 of
                                 {S y2 -> return y2; _ -> mzero};
                           x3 <- case x0 of
                                 {S y3 -> return y3; _ -> mzero};
                           let {x82 = x3};
                           let {x83 = x3};
                           _addMultiplyIIII x3 x2 x82 x83;
                           return ()}]
_addMultiplyIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m ()
_addMultiplyIIII x0 x1 x2 x3 = msum [do {multiplyIII x1 x2 x3;
                                         guard (x0 == O);
                                         return ()},
                                     do {x4 <- case x1 of
                                               {S y4 -> return y4; _ -> mzero};
                                         x5 <- case x0 of
                                               {S y5 -> return y5; _ -> mzero};
                                         _addMultiplyIIII x5 x4 x2 x3;
                                         return ()}]
multiplyIII :: MonadPlus m => Term -> Term -> Term -> m ()
multiplyIII x0 x1 x2 = msum [do {guard (x2 == O);
                                 guard (x0 == O);
                                 return ()},
                             do {let {x84 = S x1};
                                 x3 <- case x2 of
                                       {S y3 -> return y3; _ -> mzero};
                                 let {x85 = x1};
                                 _addMultiplyIIII x84 x0 x85 x3;
                                 return ()}]
squaresII :: MonadPlus m => Term -> Term -> m ()
squaresII x0 x1 = msum [do {guard (x1 == Nil);
                            guard (x0 == Nil);
                            return ()},
                        do {(x2, x3) <- case x1 of
                                        {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                            (x81, x5) <- case x0 of
                                         {Cons y81 y5 -> return (y81, y5); _ -> mzero};
                            squaresII x5 x3;
                            let {x4 = x81};
                            squareII x4 x2;
                            return ()}]
uptoSquareSquaresSumO :: MonadPlus m => m Term
uptoSquareSquaresSumO = msum [do {let {x8 = O};
                                  let {x7 = S x8};
                                  let {x6 = S x7};
                                  let {x13 = O};
                                  let {x12 = S x13};
                                  let {x11 = S x12};
                                  let {x10 = S x11};
                                  let {x9 = S x10};
                                  let {x24 = O};
                                  let {x23 = S x24};
                                  let {x22 = S x23};
                                  let {x21 = S x22};
                                  let {x20 = S x21};
                                  let {x19 = S x20};
                                  let {x18 = S x19};
                                  let {x17 = S x18};
                                  let {x16 = S x17};
                                  let {x15 = S x16};
                                  (x1, x2) <- uptoSquareSquaresIOO x6;
                                  let {x25 = Cons x1 x2};
                                  let {x14 = Cons x15 x25};
                                  x0 <- sumOII x9 x14;
                                  return x0}]
sumOII :: MonadPlus m => Term -> Term -> m Term
sumOII x1 x2 = msum [do {let {x105 = O};
                         let {x104 = S x105};
                         guard (x1 == O);
                         x0 <- _sum1OII x2 x104;
                         return x0},
                     do {x3 <- case x1 of
                               {S y3 -> return y3; _ -> mzero};
                         x0 <- _addSum1OII x2 x3;
                         return x0}]
_addSum1OII :: MonadPlus m => Term -> Term -> m Term
_addSum1OII x1 x2 = msum [do {let {x108 = O};
                              let {x107 = S x108};
                              let {x106 = S x107};
                              guard (x2 == O);
                              x0 <- _sum1OII x1 x106;
                              return x0},
                          do {let {x110 = O};
                              let {x109 = S x110};
                              x3 <- case x2 of
                                    {S y3 -> return y3; _ -> mzero};
                              x4 <- addIIO x109 x3;
                              let {x112 = S x4};
                              let {x111 = S x112};
                              x0 <- _sum1OII x1 x111;
                              return x0}]
_sum1OII :: MonadPlus m => Term -> Term -> m Term
_sum1OII x1 x2 = msum [do {guard (x1 == Nil);
                           let {x0 = x2};
                           return x0},
                       do {(x3, x4) <- case x1 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           x5 <- addIIO x2 x3;
                           x0 <- _sum1OII x4 x5;
                           return x0}]
                           
uptoSquareSquaresIOO :: MonadPlus m => Term -> m (Term, Term)
uptoSquareSquaresIOO x0 = msum [do {let {x28 = O};
                                    let {x27 = S x28};
                                    let {x26 = S x27};
                                    let {x32 = O};
                                    let {x31 = S x32};
                                    let {x30 = S x31};
                                    let {x29 = S x30};
                                    guard (x0 == O);
                                    x4 <- uptoSquaresO;
                                    x1 <- squareIO x26;
                                    x3 <- squareIO x29;
                                    let {x2 = Cons x3 x4};
                                    return (x1, x2)},
                                do {x5 <- case x0 of
                                          {S y5 -> return y5; _ -> mzero};
                                    (x1, x2) <- leUptoSquareSquaresOOI x5;
                                    return (x1, x2)}]
leUptoSquareSquaresOOI :: MonadPlus m => Term -> m (Term, Term)
leUptoSquareSquaresOOI x2 = msum [do {let {x90 = O};
                                      let {x89 = S x90};
                                      let {x88 = S x89};
                                      let {x87 = S x88};
                                      let {x95 = O};
                                      let {x94 = S x95};
                                      let {x93 = S x94};
                                      let {x92 = S x93};
                                      let {x91 = S x92};
                                      guard (x2 == O);
                                      x3 <- _uptoSquaresO;
                                      let {x86 = x3};
                                      x0 <- squareIO x87;
                                      x4 <- squareIO x91;
                                      let {x1 = Cons x4 x86};
                                      return (x0, x1)},
                                  do {let {x96 = O};
                                      let {x102 = O};
                                      let {x101 = S x102};
                                      let {x100 = S x101};
                                      let {x99 = S x100};
                                      let {x98 = S x99};
                                      x97 <- case x2 of
                                             {S y97 -> return y97; _ -> mzero};
                                      guard (x97 == x96);
                                      x1 <- _uptoSquaresO;
                                      x0 <- squareIO x98;
                                      return (x0, x1)}]
_uptoSquaresO :: MonadPlus m => m Term                                      
_uptoSquaresO = msum [do {let {x103 = Nil};
                          x0 <- squaresIO x103;
                          return x0}]
squareIO :: MonadPlus m => Term -> m Term
squareIO x0 = msum [do {let {x1 = O}; guard (x0 == O); return x1},
                    do {x3 <- case x0 of
                              {S y3 -> return y3; _ -> mzero};
                        let {x82 = x3};
                        let {x83 = x3};
                        x2 <- _addMultiplyIOII x3 x82 x83;
                        let {x1 = S x2};
                        return x1}]
_addMultiplyIOII :: MonadPlus m => Term -> Term -> Term -> m Term
_addMultiplyIOII x0 x2 x3 = msum [do {guard (x0 == O);
                                      x1 <- multiplyOII x2 x3;
                                      return x1},
                                  do {x5 <- case x0 of
                                            {S y5 -> return y5; _ -> mzero};
                                      x4 <- _addMultiplyIOII x5 x2 x3;
                                      let {x1 = S x4};
                                      return x1}]
multiplyOII :: MonadPlus m => Term -> Term -> m Term
multiplyOII x1 x2 = msum [do {let {x0 = O};
                              guard (x2 == O);
                              return x0},
                          do {let {x84 = S x1};
                              x3 <- case x2 of
                                    {S y3 -> return y3; _ -> mzero};
                              let {x85 = x1};
                              x0 <- _addMultiplyIOII x84 x85 x3;
                              return x0}]
squaresIO :: MonadPlus m => Term -> m Term
squaresIO x0 = msum [do {let {x1 = Nil};
                         guard (x0 == Nil);
                         return x1},
                     do {(x81, x5) <- case x0 of
                                      {Cons y81 y5 -> return (y81, y5); _ -> mzero};
                         let {x4 = x81};
                         x2 <- squareIO x4;
                         x3 <- squaresIO x5;
                         let {x1 = Cons x2 x3};
                         return x1}]
uptoSquaresI :: MonadPlus m => Term -> m ()
uptoSquaresI x0 = msum [do {let {x49 = O};
                            let {x48 = S x49};
                            let {x47 = S x48};
                            let {x46 = S x47};
                            let {x45 = S x46};
                            let {x44 = S x45};
                            let {x43 = S x44};
                            let {x42 = S x43};
                            let {x41 = S x42};
                            let {x40 = S x41};
                            let {x39 = S x40};
                            let {x38 = S x39};
                            let {x37 = S x38};
                            let {x36 = S x37};
                            let {x35 = S x36};
                            let {x34 = S x35};
                            let {x33 = S x34};
                            let {x50 = Nil};
                            (x51, x52) <- case x0 of
                                          {Cons y51 y52 -> return (y51, y52); _ -> mzero};
                            guard (x51 == x33);
                            guard (x52 == x50);
                            return ()},
                        do {let {x69 = O};
                            let {x68 = S x69};
                            let {x67 = S x68};
                            let {x66 = S x67};
                            let {x65 = S x66};
                            let {x64 = S x65};
                            let {x63 = S x64};
                            let {x62 = S x63};
                            let {x61 = S x62};
                            let {x60 = S x61};
                            let {x59 = S x60};
                            let {x58 = S x59};
                            let {x57 = S x58};
                            let {x56 = S x57};
                            let {x55 = S x56};
                            let {x54 = S x55};
                            let {x53 = S x54};
                            let {x76 = O};
                            let {x75 = S x76};
                            let {x74 = S x75};
                            let {x73 = S x74};
                            (x71, x72) <- case x0 of
                                          {Cons y71 y72 -> return (y71, y72); _ -> mzero};
                            guard (x71 == x53);
                            let {x70 = x72};
                            (x1, x2) <- case x70 of
                                        {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                            uptoSquareSquaresIII x73 x1 x2;
                            return ()}]
uptoSquaresO :: MonadPlus m => m Term                            
uptoSquaresO = msum [do {let {x49 = O};
                         let {x48 = S x49};
                         let {x47 = S x48};
                         let {x46 = S x47};
                         let {x45 = S x46};
                         let {x44 = S x45};
                         let {x43 = S x44};
                         let {x42 = S x43};
                         let {x41 = S x42};
                         let {x40 = S x41};
                         let {x39 = S x40};
                         let {x38 = S x39};
                         let {x37 = S x38};
                         let {x36 = S x37};
                         let {x35 = S x36};
                         let {x34 = S x35};
                         let {x33 = S x34};
                         let {x50 = Nil};
                         let {x51 = x33};
                         let {x52 = x50};
                         let {x0 = Cons x51 x52};
                         return x0},
                     do {let {x69 = O};
                         let {x68 = S x69};
                         let {x67 = S x68};
                         let {x66 = S x67};
                         let {x65 = S x66};
                         let {x64 = S x65};
                         let {x63 = S x64};
                         let {x62 = S x63};
                         let {x61 = S x62};
                         let {x60 = S x61};
                         let {x59 = S x60};
                         let {x58 = S x59};
                         let {x57 = S x58};
                         let {x56 = S x57};
                         let {x55 = S x56};
                         let {x54 = S x55};
                         let {x53 = S x54};
                         let {x76 = O};
                         let {x75 = S x76};
                         let {x74 = S x75};
                         let {x73 = S x74};
                         let {x71 = x53};
                         (x1, x2) <- uptoSquareSquaresIOO x73;
                         let {x70 = Cons x1 x2};
                         let {x72 = x70};
                         let {x0 = Cons x71 x72};
                         return x0}]