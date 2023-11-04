module Remove2_unfold where

import Stream
import Control.Monad
import Term

rrI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m ()
rrI x0 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x1,
                                                                                                                                                                                                                                                                   x8) <- case x0 of
                                                                                                                                                                                                                                                                          {Cons y1
                                                                                                                                                                                                                                                                                y8 -> return (y1,
                                                                                                                                                                                                                                                                                              y8);
                                                                                                                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                                                                                                                  (x2,
                                                                                                                                                                                                                                                                   x3) <- gGIOO x1 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                  let {x7 = Cons x2 x3};
                                                                                                                                                                                                                                                                  guard (x8 == x7);
                                                                                                                                                                                                                                                                  return ()}]
gGIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
gGIOO x0 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = x0};
                                                                                                                                                                                                                                                                    (x3,
                                                                                                                                                                                                                                                                     x4) <- hGIOO x1 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                    let {x2 = Cons x3 x4};
                                                                                                                                                                                                                                                                    return (x1,
                                                                                                                                                                                                                                                                            x2)},
                                                                                                                                                                                                                                                                do {(x1,
                                                                                                                                                                                                                                                                     x2) <- _neqHGIOO x0 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                    return (x1,
                                                                                                                                                                                                                                                                            x2)}]
_neqHGIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
_neqHGIOO x0 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x94 = Zero};
                                                                                                                                                                                                                                                                        guard (x0 == Zero);
                                                                                                                                                                                                                                                                        (x95,
                                                                                                                                                                                                                                                                         x2) <- hGIOO x94 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                        x3 <- case x95 of
                                                                                                                                                                                                                                                                              {Succ y3 -> return y3;
                                                                                                                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                                                                                                                        let {x1 = Succ x3};
                                                                                                                                                                                                                                                                        return (x1,
                                                                                                                                                                                                                                                                                x2)},
                                                                                                                                                                                                                                                                    do {let {x1 = Zero};
                                                                                                                                                                                                                                                                        let {x97 = Zero};
                                                                                                                                                                                                                                                                        x3 <- case x0 of
                                                                                                                                                                                                                                                                              {Succ y3 -> return y3;
                                                                                                                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                                                                                                                        let {x96 = Succ x3};
                                                                                                                                                                                                                                                                        x2 <- hGIIO x96 x97 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                        return (x1,
                                                                                                                                                                                                                                                                                x2)},
                                                                                                                                                                                                                                                                    do {x100 <- case x0 of
                                                                                                                                                                                                                                                                                {Succ y100 -> return y100;
                                                                                                                                                                                                                                                                                 _ -> mzero};
                                                                                                                                                                                                                                                                        let {x5 = x100};
                                                                                                                                                                                                                                                                        let {x98 = Succ x5};
                                                                                                                                                                                                                                                                        x4 <- neqIO x5 gen_neqIO_x2;
                                                                                                                                                                                                                                                                        let {x1 = Succ x4};
                                                                                                                                                                                                                                                                        let {x99 = Succ x4};
                                                                                                                                                                                                                                                                        x2 <- hGIIO x98 x99 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                        return (x1,
                                                                                                                                                                                                                                                                                x2)}]
hGIIO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term
hGIIO x0 x1 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x2 = Nil};
                                                                                                                                                                                                                                                                       let {x10 = Nil};
                                                                                                                                                                                                                                                                       let {x11 = Nil};
                                                                                                                                                                                                                                                                       let {x9 = x1};
                                                                                                                                                                                                                                                                       ______gIIIII x0 x1 x9 x10 x11;
                                                                                                                                                                                                                                                                       return x2},
                                                                                                                                                                                                                                                                   do {(x3,
                                                                                                                                                                                                                                                                        x4) <- _gGIIOO x0 x1 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                       let {x2 = Cons x3 x4};
                                                                                                                                                                                                                                                                       return x2}]
______gIIIII :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m ()
______gIIIII x0 x1 x2 x3 x4 = msum [do {guard (x1 == x0);
                                        let {x51 = Zero};
                                        let {x54 = Zero};
                                        let {x58 = Zero};
                                        let {x57 = Succ x58};
                                        let {x56 = Succ x57};
                                        let {x60 = Zero};
                                        let {x62 = Zero};
                                        let {x63 = Nil};
                                        let {x61 = Cons x62 x63};
                                        let {x59 = Cons x60 x61};
                                        let {x55 = Cons x56 x59};
                                        let {x53 = Cons x54 x55};
                                        fII x4 x53;
                                        x52 <- case x0 of
                                               {Succ y52 -> return y52; _ -> mzero};
                                        guard (x52 == x51);
                                        return ()},
                                    do {let {x64 = Zero};
                                        let {x66 = Cons x2 x3};
                                        let {x68 = Zero};
                                        let {x72 = Zero};
                                        let {x71 = Succ x72};
                                        let {x70 = Succ x71};
                                        let {x74 = Zero};
                                        let {x76 = Zero};
                                        let {x77 = Nil};
                                        let {x75 = Cons x76 x77};
                                        let {x73 = Cons x74 x75};
                                        let {x69 = Cons x70 x73};
                                        let {x67 = Cons x68 x69};
                                        fII x66 x67;
                                        guard (x1 == Zero);
                                        x65 <- case x0 of
                                               {Succ y65 -> return y65; _ -> mzero};
                                        guard (x65 == x64);
                                        return ()},
                                    do {let {x80 = Zero};
                                        let {x82 = Cons x2 x3};
                                        let {x84 = Zero};
                                        let {x88 = Zero};
                                        let {x87 = Succ x88};
                                        let {x86 = Succ x87};
                                        let {x90 = Zero};
                                        let {x92 = Zero};
                                        let {x93 = Nil};
                                        let {x91 = Cons x92 x93};
                                        let {x89 = Cons x90 x91};
                                        let {x85 = Cons x86 x89};
                                        let {x83 = Cons x84 x85};
                                        fII x82 x83;
                                        x79 <- case x1 of
                                               {Succ y79 -> return y79; _ -> mzero};
                                        x81 <- case x0 of
                                               {Succ y81 -> return y81; _ -> mzero};
                                        guard (x81 == x80);
                                        let {x78 = x79};
                                        x5 <- case x78 of
                                              {Succ y5 -> return y5; _ -> mzero};
                                        return ()}]
_gGIIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
_gGIIOO x0 x1 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x3,
                                                                                                                                                                                                                                                                          x2) <- fGIOO x0 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                         guard (x1 == x2);
                                                                                                                                                                                                                                                                         return (x2,
                                                                                                                                                                                                                                                                                 x3)},
                                                                                                                                                                                                                                                                     do {(x2,
                                                                                                                                                                                                                                                                          x3) <- neqHGIIOO x0 x1 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                         return (x2,
                                                                                                                                                                                                                                                                                 x3)}]
fII :: MonadPlus m => Term -> Term -> m ()
fII x0 x1 = msum [do {guard (x1 == Nil);
                      guard (x0 == Nil);
                      return ()},
                  do {(x2, x3) <- case x0 of
                                  {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                      hIII x2 x3 x1;
                      return ()}]
fGIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
fGIOO x0 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Nil};
                                                                                                                                                                                          let {x13 = Nil};
                                                                                                                                                                                          let {x14 = Nil};
                                                                                                                                                                                          (x2,
                                                                                                                                                                                           x12) <- ______gIOOII x0 x13 x14 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                                                                          guard (x12 == x2);
                                                                                                                                                                                          return (x1,
                                                                                                                                                                                                  x2)},
                                                                                                                                                                                      do {(x4,
                                                                                                                                                                                           x3,
                                                                                                                                                                                           x2,
                                                                                                                                                                                           x15) <- __hGIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                          guard (x15 == x2);
                                                                                                                                                                                          let {x1 = Cons x3 x4};
                                                                                                                                                                                          return (x1,
                                                                                                                                                                                                  x2)}]
______gIOOII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
______gIOOII x0 x3 x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_____gOIOO_x2 gen_neqOI_x2 = msum [do {let {x51 = Zero};
                                                                                                         let {x54 = Zero};
                                                                                                         let {x58 = Zero};
                                                                                                         let {x57 = Succ x58};
                                                                                                         let {x56 = Succ x57};
                                                                                                         let {x60 = Zero};
                                                                                                         let {x62 = Zero};
                                                                                                         let {x63 = Nil};
                                                                                                         let {x61 = Cons x62 x63};
                                                                                                         let {x59 = Cons x60 x61};
                                                                                                         let {x55 = Cons x56 x59};
                                                                                                         let {x53 = Cons x54 x55};
                                                                                                         fII x4 x53;
                                                                                                         x52 <- case x0 of
                                                                                                                {Succ y52 -> return y52;
                                                                                                                 _ -> mzero};
                                                                                                         guard (x52 == x51);
                                                                                                         let {x1 = x0};
                                                                                                         x2 <- gen_______gIOOII_x2;
                                                                                                         return (x1,
                                                                                                                 x2)},
                                                                                                     do {let {x1 = Zero};
                                                                                                         let {x64 = Zero};
                                                                                                         let {x68 = Zero};
                                                                                                         let {x72 = Zero};
                                                                                                         let {x71 = Succ x72};
                                                                                                         let {x70 = Succ x71};
                                                                                                         let {x74 = Zero};
                                                                                                         let {x76 = Zero};
                                                                                                         let {x77 = Nil};
                                                                                                         let {x75 = Cons x76 x77};
                                                                                                         let {x73 = Cons x74 x75};
                                                                                                         let {x69 = Cons x70 x73};
                                                                                                         let {x67 = Cons x68 x69};
                                                                                                         x65 <- case x0 of
                                                                                                                {Succ y65 -> return y65;
                                                                                                                 _ -> mzero};
                                                                                                         guard (x65 == x64);
                                                                                                         x66 <- fOI x67 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                         x2 <- case x66 of
                                                                                                               {Cons y2
                                                                                                                     y3 -> do {guard (x3 == y3);
                                                                                                                               return y2};
                                                                                                                _ -> mzero};
                                                                                                         return (x1,
                                                                                                                 x2)},
                                                                                                     do {let {x80 = Zero};
                                                                                                         let {x84 = Zero};
                                                                                                         let {x88 = Zero};
                                                                                                         let {x87 = Succ x88};
                                                                                                         let {x86 = Succ x87};
                                                                                                         let {x90 = Zero};
                                                                                                         let {x92 = Zero};
                                                                                                         let {x93 = Nil};
                                                                                                         let {x91 = Cons x92 x93};
                                                                                                         let {x89 = Cons x90 x91};
                                                                                                         let {x85 = Cons x86 x89};
                                                                                                         let {x83 = Cons x84 x85};
                                                                                                         x81 <- case x0 of
                                                                                                                {Succ y81 -> return y81;
                                                                                                                 _ -> mzero};
                                                                                                         guard (x81 == x80);
                                                                                                         x82 <- fOI x83 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                         x2 <- case x82 of
                                                                                                               {Cons y2
                                                                                                                     y3 -> do {guard (x3 == y3);
                                                                                                                               return y2};
                                                                                                                _ -> mzero};
                                                                                                         (x79,
                                                                                                          x78) <- do {x78 <- gen_______gIOOII_x78;
                                                                                                                      return (x78,
                                                                                                                              x78)};
                                                                                                         let {x1 = Succ x79};
                                                                                                         x5 <- case x78 of
                                                                                                               {Succ y5 -> return y5;
                                                                                                                _ -> mzero};
                                                                                                         return (x1,
                                                                                                                 x2)}]
__hGIOOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
__hGIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Nil};
                                                                                                                                                     let {x34 = Nil};
                                                                                                                                                     let {x37 = Nil};
                                                                                                                                                     (x3,
                                                                                                                                                      x4,
                                                                                                                                                      x33,
                                                                                                                                                      x35) <- ______gIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                                     x2 <- case x33 of
                                                                                                                                                           {Cons y2
                                                                                                                                                                 y34 -> do {guard (x34 == y34);
                                                                                                                                                                            return y2};
                                                                                                                                                            _ -> mzero};
                                                                                                                                                     x36 <- case x35 of
                                                                                                                                                            {Cons y36
                                                                                                                                                                  y37 -> do {guard (x37 == y37);
                                                                                                                                                                             return y36};
                                                                                                                                                             _ -> mzero};
                                                                                                                                                     guard (x36 == x2);
                                                                                                                                                     return (x1,
                                                                                                                                                             x2,
                                                                                                                                                             x3,
                                                                                                                                                             x4)},
                                                                                                                                                 do {(x2,
                                                                                                                                                      x3,
                                                                                                                                                      x4,
                                                                                                                                                      x5,
                                                                                                                                                      x6) <- ___gGIOOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                     let {x1 = Cons x5 x6};
                                                                                                                                                     return (x1,
                                                                                                                                                             x2,
                                                                                                                                                             x3,
                                                                                                                                                             x4)}]
______gIOOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
______gIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqOI_x2 = msum [do {let {x51 = Zero};
                                                                                                                                           let {x54 = Zero};
                                                                                                                                           let {x58 = Zero};
                                                                                                                                           let {x57 = Succ x58};
                                                                                                                                           let {x56 = Succ x57};
                                                                                                                                           let {x60 = Zero};
                                                                                                                                           let {x62 = Zero};
                                                                                                                                           let {x63 = Nil};
                                                                                                                                           let {x61 = Cons x62 x63};
                                                                                                                                           let {x59 = Cons x60 x61};
                                                                                                                                           let {x55 = Cons x56 x59};
                                                                                                                                           let {x53 = Cons x54 x55};
                                                                                                                                           x52 <- case x0 of
                                                                                                                                                  {Succ y52 -> return y52;
                                                                                                                                                   _ -> mzero};
                                                                                                                                           guard (x52 == x51);
                                                                                                                                           let {x1 = x0};
                                                                                                                                           x4 <- fOI x53 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                           x2 <- gen_______gIOOOO_x2;
                                                                                                                                           x3 <- gen_______gIOOOO_x3;
                                                                                                                                           return (x1,
                                                                                                                                                   x2,
                                                                                                                                                   x3,
                                                                                                                                                   x4)},
                                                                                                                                       do {let {x1 = Zero};
                                                                                                                                           let {x64 = Zero};
                                                                                                                                           let {x68 = Zero};
                                                                                                                                           let {x72 = Zero};
                                                                                                                                           let {x71 = Succ x72};
                                                                                                                                           let {x70 = Succ x71};
                                                                                                                                           let {x74 = Zero};
                                                                                                                                           let {x76 = Zero};
                                                                                                                                           let {x77 = Nil};
                                                                                                                                           let {x75 = Cons x76 x77};
                                                                                                                                           let {x73 = Cons x74 x75};
                                                                                                                                           let {x69 = Cons x70 x73};
                                                                                                                                           let {x67 = Cons x68 x69};
                                                                                                                                           x65 <- case x0 of
                                                                                                                                                  {Succ y65 -> return y65;
                                                                                                                                                   _ -> mzero};
                                                                                                                                           guard (x65 == x64);
                                                                                                                                           x66 <- fOI x67 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                           (x2,
                                                                                                                                            x3) <- case x66 of
                                                                                                                                                   {Cons y2
                                                                                                                                                         y3 -> return (y2,
                                                                                                                                                                       y3);
                                                                                                                                                    _ -> mzero};
                                                                                                                                           x4 <- gen_______gIOOOO_x4;
                                                                                                                                           return (x1,
                                                                                                                                                   x2,
                                                                                                                                                   x3,
                                                                                                                                                   x4)},
                                                                                                                                       do {let {x80 = Zero};
                                                                                                                                           let {x84 = Zero};
                                                                                                                                           let {x88 = Zero};
                                                                                                                                           let {x87 = Succ x88};
                                                                                                                                           let {x86 = Succ x87};
                                                                                                                                           let {x90 = Zero};
                                                                                                                                           let {x92 = Zero};
                                                                                                                                           let {x93 = Nil};
                                                                                                                                           let {x91 = Cons x92 x93};
                                                                                                                                           let {x89 = Cons x90 x91};
                                                                                                                                           let {x85 = Cons x86 x89};
                                                                                                                                           let {x83 = Cons x84 x85};
                                                                                                                                           x81 <- case x0 of
                                                                                                                                                  {Succ y81 -> return y81;
                                                                                                                                                   _ -> mzero};
                                                                                                                                           guard (x81 == x80);
                                                                                                                                           x82 <- fOI x83 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                           (x2,
                                                                                                                                            x3) <- case x82 of
                                                                                                                                                   {Cons y2
                                                                                                                                                         y3 -> return (y2,
                                                                                                                                                                       y3);
                                                                                                                                                    _ -> mzero};
                                                                                                                                           (x79,
                                                                                                                                            x78) <- do {x78 <- gen_______gIOOOO_x78;
                                                                                                                                                        return (x78,
                                                                                                                                                                x78)};
                                                                                                                                           let {x1 = Succ x79};
                                                                                                                                           x5 <- case x78 of
                                                                                                                                                 {Succ y5 -> return y5;
                                                                                                                                                  _ -> mzero};
                                                                                                                                           x4 <- gen_______gIOOOO_x4;
                                                                                                                                           return (x1,
                                                                                                                                                   x2,
                                                                                                                                                   x3,
                                                                                                                                                   x4)}]
___gGIOOOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term, Term)
___gGIOOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x2,
                                                                                                                                                        x3,
                                                                                                                                                        x5,
                                                                                                                                                        x4) <- _fGIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                                       let {x1 = x4};
                                                                                                                                                       return (x1,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x4,
                                                                                                                                                               x5)},
                                                                                                                                                   do {(x2,
                                                                                                                                                        x3,
                                                                                                                                                        x38,
                                                                                                                                                        x39) <- ______gIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                                       (x1,
                                                                                                                                                        x6) <- case x38 of
                                                                                                                                                               {Cons y1
                                                                                                                                                                     y6 -> return (y1,
                                                                                                                                                                                   y6);
                                                                                                                                                                _ -> mzero};
                                                                                                                                                       (x40,
                                                                                                                                                        x41) <- case x39 of
                                                                                                                                                                {Cons y40
                                                                                                                                                                      y41 -> return (y40,
                                                                                                                                                                                     y41);
                                                                                                                                                                 _ -> mzero};
                                                                                                                                                       guard (x40 == x1);
                                                                                                                                                       guard (x41 == x6);
                                                                                                                                                       x4 <- neqIO x1 gen_neqIO_x2;
                                                                                                                                                       x5 <- hIOI x4 x6 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                       return (x1,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x4,
                                                                                                                                                               x5)}]
_fGIOOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
_fGIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqOI_x2 = msum [do {let {x3 = Nil};
                                                                                                                                       let {x43 = Nil};
                                                                                                                                       let {x46 = Nil};
                                                                                                                                       (x1,
                                                                                                                                        x2,
                                                                                                                                        x42,
                                                                                                                                        x44) <- ______gIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                       x4 <- case x42 of
                                                                                                                                             {Cons y4
                                                                                                                                                   y43 -> do {guard (x43 == y43);
                                                                                                                                                              return y4};
                                                                                                                                              _ -> mzero};
                                                                                                                                       x45 <- case x44 of
                                                                                                                                              {Cons y45
                                                                                                                                                    y46 -> do {guard (x46 == y46);
                                                                                                                                                               return y45};
                                                                                                                                               _ -> mzero};
                                                                                                                                       guard (x45 == x4);
                                                                                                                                       return (x1,
                                                                                                                                               x2,
                                                                                                                                               x3,
                                                                                                                                               x4)},
                                                                                                                                   do {(x1,
                                                                                                                                        x2,
                                                                                                                                        x47,
                                                                                                                                        x48) <- ______gIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                       (x4,
                                                                                                                                        x7) <- case x47 of
                                                                                                                                               {Cons y4
                                                                                                                                                     y7 -> return (y4,
                                                                                                                                                                   y7);
                                                                                                                                                _ -> mzero};
                                                                                                                                       (x49,
                                                                                                                                        x50) <- case x48 of
                                                                                                                                                {Cons y49
                                                                                                                                                      y50 -> return (y49,
                                                                                                                                                                     y50);
                                                                                                                                                 _ -> mzero};
                                                                                                                                       guard (x49 == x4);
                                                                                                                                       guard (x50 == x7);
                                                                                                                                       (x5,
                                                                                                                                        x6) <- hOOI x7 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                       let {x3 = Cons x5 x6};
                                                                                                                                       return (x1,
                                                                                                                                               x2,
                                                                                                                                               x3,
                                                                                                                                               x4)}]
fOI :: MonadPlus m => Term -> m Term -> m Term -> m Term
fOI x1 gen_____gOIOO_x2 gen_neqOI_x2 = msum [do {let {x0 = Nil};
                                                 guard (x1 == Nil);
                                                 return x0},
                                             do {(x2, x3) <- hOOI x1 gen_____gOIOO_x2 gen_neqOI_x2;
                                                 let {x0 = Cons x2 x3};
                                                 return x0}]
hIII :: MonadPlus m => Term -> Term -> Term -> m ()
hIII x0 x1 x2 = msum [do {let {x16 = Nil};
                          (x17, x18) <- case x2 of
                                        {Cons y17 y18 -> return (y17, y18); _ -> mzero};
                          guard (x17 == x0);
                          guard (x18 == x16);
                          guard (x1 == Nil);
                          return ()},
                      do {(x3, x4) <- case x1 of
                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                          ____gIIII x0 x2 x3 x4;
                          return ()}]
____gIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m ()
____gIIII x0 x1 x2 x3 = msum [do {guard (x0 == x2);
                                  (x19, x4) <- case x1 of
                                               {Cons y19 y4 -> return (y19, y4); _ -> mzero};
                                  guard (x19 == x2);
                                  fII x3 x4;
                                  return ()},
                              do {neqII x0 x2;
                                  (x20, x4) <- case x1 of
                                               {Cons y20 y4 -> return (y20, y4); _ -> mzero};
                                  guard (x20 == x0);
                                  hIII x2 x3 x4;
                                  return ()}]
hIOI :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term
hIOI x0 x2 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x16 = Nil};
                                                                  let {x1 = Nil};
                                                                  (x17, x18) <- case x2 of
                                                                                {Cons y17
                                                                                      y18 -> return (y17,
                                                                                                     y18);
                                                                                 _ -> mzero};
                                                                  guard (x17 == x0);
                                                                  guard (x18 == x16);
                                                                  return x1},
                                                              do {(x3,
                                                                   x4) <- ____gIIOO x0 x2 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                  let {x1 = Cons x3 x4};
                                                                  return x1}]
____gIIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m (Term, Term)
____gIIOO x0 x1 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x19,
                                                                        x4) <- case x1 of
                                                                               {Cons y19
                                                                                     y4 -> return (y19,
                                                                                                   y4);
                                                                                _ -> mzero};
                                                                       x3 <- fOI x4 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                       let {x2 = x19};
                                                                       guard (x0 == x2);
                                                                       return (x2, x3)},
                                                                   do {(x20, x4) <- case x1 of
                                                                                    {Cons y20
                                                                                          y4 -> return (y20,
                                                                                                        y4);
                                                                                     _ -> mzero};
                                                                       guard (x20 == x0);
                                                                       x2 <- neqIO x0 gen_neqIO_x2;
                                                                       x3 <- hIOI x2 x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                       return (x2, x3)}]
hOOI :: MonadPlus m => Term -> m Term -> m Term -> m (Term, Term)
hOOI x2 gen_____gOIOO_x2 gen_neqOI_x2 = msum [do {let {x16 = Nil};
                                                  let {x1 = Nil};
                                                  (x17, x18) <- case x2 of
                                                                {Cons y17 y18 -> return (y17, y18);
                                                                 _ -> mzero};
                                                  guard (x18 == x16);
                                                  let {x0 = x17};
                                                  return (x0, x1)},
                                              do {(x0,
                                                   x3,
                                                   x4) <- ____gOIOO x2 gen_____gOIOO_x2 gen_neqOI_x2;
                                                  let {x1 = Cons x3 x4};
                                                  return (x0, x1)}]
____gOIOO :: MonadPlus m => Term -> m Term -> m Term -> m (Term, Term, Term)
____gOIOO x1 gen_____gOIOO_x2 gen_neqOI_x2 = msum [do {(x19,
                                                        x4) <- case x1 of
                                                               {Cons y19 y4 -> return (y19, y4);
                                                                _ -> mzero};
                                                       x3 <- fOI x4 gen_____gOIOO_x2 gen_neqOI_x2;
                                                       (x0, x2) <- do {x2 <- gen_____gOIOO_x2;
                                                                       return (x2, x2)};
                                                       guard (x19 == x2);
                                                       return (x0, x2, x3)},
                                                   do {(x20, x4) <- case x1 of
                                                                    {Cons y20 y4 -> return (y20,
                                                                                            y4);
                                                                     _ -> mzero};
                                                       (x2,
                                                        x3) <- hOOI x4 gen_____gOIOO_x2 gen_neqOI_x2;
                                                       x0 <- neqOI x2 gen_neqOI_x2;
                                                       guard (x20 == x0);
                                                       return (x0, x2, x3)}]
hGIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
hGIOO x0 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x2 = Nil};
                                                                                                                                                                                                                                                                    let {x10 = Nil};
                                                                                                                                                                                                                                                                    let {x11 = Nil};
                                                                                                                                                                                                                                                                    (x1,
                                                                                                                                                                                                                                                                     x9) <- ______gIOOII x0 x10 x11 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                    guard (x9 == x1);
                                                                                                                                                                                                                                                                    return (x1,
                                                                                                                                                                                                                                                                            x2)},
                                                                                                                                                                                                                                                                do {(x1,
                                                                                                                                                                                                                                                                     x3,
                                                                                                                                                                                                                                                                     x4) <- _gGIOOO x0 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                    let {x2 = Cons x3 x4};
                                                                                                                                                                                                                                                                    return (x1,
                                                                                                                                                                                                                                                                            x2)}]
_gGIOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
_gGIOOO x0 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x3,
                                                                                                                                                                                                                                                                       x2) <- fGIOO x0 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                      let {x1 = x2};
                                                                                                                                                                                                                                                                      return (x1,
                                                                                                                                                                                                                                                                              x2,
                                                                                                                                                                                                                                                                              x3)},
                                                                                                                                                                                                                                                                  do {(x1,
                                                                                                                                                                                                                                                                       x2,
                                                                                                                                                                                                                                                                       x3) <- neqHGIOOO x0 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                      return (x1,
                                                                                                                                                                                                                                                                              x2,
                                                                                                                                                                                                                                                                              x3)}]
neqII :: MonadPlus m => Term -> Term -> m ()
neqII x0 x1 = msum [do {x2 <- case x1 of
                              {Succ y2 -> return y2; _ -> mzero};
                        guard (x0 == Zero);
                        return ()},
                    do {guard (x1 == Zero);
                        x2 <- case x0 of
                              {Succ y2 -> return y2; _ -> mzero};
                        return ()},
                    do {x3 <- case x1 of
                              {Succ y3 -> return y3; _ -> mzero};
                        x4 <- case x0 of
                              {Succ y4 -> return y4; _ -> mzero};
                        neqII x4 x3;
                        return ()}]
neqIO :: MonadPlus m => Term -> m Term -> m Term
neqIO x0 gen_neqIO_x2 = msum [do {guard (x0 == Zero);
                                  (x1, x2) <- do {x2 <- gen_neqIO_x2;
                                                  let {x1 = Succ x2};
                                                  return (x1, x2)};
                                  return x1},
                              do {let {x1 = Zero};
                                  x2 <- case x0 of
                                        {Succ y2 -> return y2; _ -> mzero};
                                  return x1},
                              do {x4 <- case x0 of
                                        {Succ y4 -> return y4; _ -> mzero};
                                  x3 <- neqIO x4 gen_neqIO_x2;
                                  let {x1 = Succ x3};
                                  return x1}]
neqOI :: MonadPlus m => Term -> m Term -> m Term
neqOI x1 gen_neqOI_x2 = msum [do {let {x0 = Zero};
                                  x2 <- case x1 of
                                        {Succ y2 -> return y2; _ -> mzero};
                                  return x0},
                              do {guard (x1 == Zero);
                                  (x0, x2) <- do {x2 <- gen_neqOI_x2;
                                                  let {x0 = Succ x2};
                                                  return (x0, x2)};
                                  return x0},
                              do {x3 <- case x1 of
                                        {Succ y3 -> return y3; _ -> mzero};
                                  x4 <- neqOI x3 gen_neqOI_x2;
                                  let {x0 = Succ x4};
                                  return x0}]
neqHGIIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
neqHGIIOO x0 x1 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x22 = Zero};
                                                                                                                                                                                                                                  let {x23 = Zero};
                                                                                                                                                                                                                                  guard (x1 == Zero);
                                                                                                                                                                                                                                  (x3,
                                                                                                                                                                                                                                   x21) <- __hGIOOII x0 x22 x23 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                  x4 <- case x21 of
                                                                                                                                                                                                                                        {Succ y4 -> return y4;
                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                  let {x2 = Succ x4};
                                                                                                                                                                                                                                  return (x2,
                                                                                                                                                                                                                                          x3)},
                                                                                                                                                                                                                              do {let {x2 = Zero};
                                                                                                                                                                                                                                  let {x24 = Zero};
                                                                                                                                                                                                                                  x4 <- case x1 of
                                                                                                                                                                                                                                        {Succ y4 -> return y4;
                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                  let {x25 = Succ x4};
                                                                                                                                                                                                                                  let {x27 = x4};
                                                                                                                                                                                                                                  let {x26 = Succ x27};
                                                                                                                                                                                                                                  x3 <- __hGIOIII x0 x24 x25 x26 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                  return (x2,
                                                                                                                                                                                                                                          x3)},
                                                                                                                                                                                                                              do {x32 <- case x1 of
                                                                                                                                                                                                                                         {Succ y32 -> return y32;
                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                  (x3,
                                                                                                                                                                                                                                   x28,
                                                                                                                                                                                                                                   x29,
                                                                                                                                                                                                                                   x30) <- __hGIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                  x5 <- case x28 of
                                                                                                                                                                                                                                        {Succ y5 -> return y5;
                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                  let {x2 = Succ x5};
                                                                                                                                                                                                                                  x6 <- case x29 of
                                                                                                                                                                                                                                        {Succ y6 -> return y6;
                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                  guard (x32 == x6);
                                                                                                                                                                                                                                  neqII x6 x5;
                                                                                                                                                                                                                                  x31 <- case x30 of
                                                                                                                                                                                                                                         {Succ y31 -> return y31;
                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                  guard (x31 == x6);
                                                                                                                                                                                                                                  return (x2,
                                                                                                                                                                                                                                          x3)}]
__hGIOIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term
__hGIOIII x0 x2 x3 x4 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Nil};
                                                                                                                                                       let {x34 = Nil};
                                                                                                                                                       let {x33 = Cons x2 x34};
                                                                                                                                                       let {x37 = Nil};
                                                                                                                                                       let {x36 = x2};
                                                                                                                                                       let {x35 = Cons x36 x37};
                                                                                                                                                       ______gIIIII x0 x3 x4 x33 x35;
                                                                                                                                                       return x1},
                                                                                                                                                   do {(x5,
                                                                                                                                                        x6) <- ___gGIIIIOO x0 x2 x3 x4 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                       let {x1 = Cons x5 x6};
                                                                                                                                                       return x1}]
___gGIIIIOO :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
___gGIIIIOO x0 x1 x2 x3 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x5,
                                                                                                                                                          x4) <- _fGIIIOO x0 x2 x3 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                                         guard (x1 == x4);
                                                                                                                                                         return (x4,
                                                                                                                                                                 x5)},
                                                                                                                                                     do {let {x40 = x1};
                                                                                                                                                         x4 <- neqIO x1 gen_neqIO_x2;
                                                                                                                                                         (x5,
                                                                                                                                                          x6) <- hIOO x4 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                         let {x38 = Cons x1 x6};
                                                                                                                                                         let {x41 = x6};
                                                                                                                                                         let {x39 = Cons x40 x41};
                                                                                                                                                         ______gIIIII x0 x2 x3 x38 x39;
                                                                                                                                                         return (x4,
                                                                                                                                                                 x5)}]
__hGIOOII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
__hGIOOII x0 x3 x4 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Nil};
                                                                                                                  let {x34 = Nil};
                                                                                                                  let {x37 = Nil};
                                                                                                                  (x33,
                                                                                                                   x35) <- ______gIIIOO x0 x3 x4 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                  x2 <- case x33 of
                                                                                                                        {Cons y2
                                                                                                                              y34 -> do {guard (x34 == y34);
                                                                                                                                         return y2};
                                                                                                                         _ -> mzero};
                                                                                                                  x36 <- case x35 of
                                                                                                                         {Cons y36
                                                                                                                               y37 -> do {guard (x37 == y37);
                                                                                                                                          return y36};
                                                                                                                          _ -> mzero};
                                                                                                                  guard (x36 == x2);
                                                                                                                  return (x1,
                                                                                                                          x2)},
                                                                                                              do {(x2,
                                                                                                                   x5,
                                                                                                                   x6) <- ___gGIOIIOO x0 x3 x4 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                  let {x1 = Cons x5 x6};
                                                                                                                  return (x1,
                                                                                                                          x2)}]
______gIIIOO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
______gIIIOO x0 x1 x2 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqOI_x2 = msum [do {guard (x1 == x0);
                                                                                                        let {x51 = Zero};
                                                                                                        let {x54 = Zero};
                                                                                                        let {x58 = Zero};
                                                                                                        let {x57 = Succ x58};
                                                                                                        let {x56 = Succ x57};
                                                                                                        let {x60 = Zero};
                                                                                                        let {x62 = Zero};
                                                                                                        let {x63 = Nil};
                                                                                                        let {x61 = Cons x62 x63};
                                                                                                        let {x59 = Cons x60 x61};
                                                                                                        let {x55 = Cons x56 x59};
                                                                                                        let {x53 = Cons x54 x55};
                                                                                                        x52 <- case x0 of
                                                                                                               {Succ y52 -> return y52;
                                                                                                                _ -> mzero};
                                                                                                        guard (x52 == x51);
                                                                                                        x4 <- fOI x53 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                        x3 <- gen_______gIIIOO_x3;
                                                                                                        return (x3,
                                                                                                                x4)},
                                                                                                    do {let {x64 = Zero};
                                                                                                        let {x68 = Zero};
                                                                                                        let {x72 = Zero};
                                                                                                        let {x71 = Succ x72};
                                                                                                        let {x70 = Succ x71};
                                                                                                        let {x74 = Zero};
                                                                                                        let {x76 = Zero};
                                                                                                        let {x77 = Nil};
                                                                                                        let {x75 = Cons x76 x77};
                                                                                                        let {x73 = Cons x74 x75};
                                                                                                        let {x69 = Cons x70 x73};
                                                                                                        let {x67 = Cons x68 x69};
                                                                                                        guard (x1 == Zero);
                                                                                                        x65 <- case x0 of
                                                                                                               {Succ y65 -> return y65;
                                                                                                                _ -> mzero};
                                                                                                        guard (x65 == x64);
                                                                                                        x66 <- fOI x67 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                        x3 <- case x66 of
                                                                                                              {Cons y2
                                                                                                                    y3 -> do {guard (x2 == y2);
                                                                                                                              return y3};
                                                                                                               _ -> mzero};
                                                                                                        x4 <- gen_______gIIIOO_x4;
                                                                                                        return (x3,
                                                                                                                x4)},
                                                                                                    do {let {x80 = Zero};
                                                                                                        let {x84 = Zero};
                                                                                                        let {x88 = Zero};
                                                                                                        let {x87 = Succ x88};
                                                                                                        let {x86 = Succ x87};
                                                                                                        let {x90 = Zero};
                                                                                                        let {x92 = Zero};
                                                                                                        let {x93 = Nil};
                                                                                                        let {x91 = Cons x92 x93};
                                                                                                        let {x89 = Cons x90 x91};
                                                                                                        let {x85 = Cons x86 x89};
                                                                                                        let {x83 = Cons x84 x85};
                                                                                                        x79 <- case x1 of
                                                                                                               {Succ y79 -> return y79;
                                                                                                                _ -> mzero};
                                                                                                        x81 <- case x0 of
                                                                                                               {Succ y81 -> return y81;
                                                                                                                _ -> mzero};
                                                                                                        guard (x81 == x80);
                                                                                                        x82 <- fOI x83 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                        x3 <- case x82 of
                                                                                                              {Cons y2
                                                                                                                    y3 -> do {guard (x2 == y2);
                                                                                                                              return y3};
                                                                                                               _ -> mzero};
                                                                                                        let {x78 = x79};
                                                                                                        x5 <- case x78 of
                                                                                                              {Succ y5 -> return y5;
                                                                                                               _ -> mzero};
                                                                                                        x4 <- gen_______gIIIOO_x4;
                                                                                                        return (x3,
                                                                                                                x4)}]
___gGIOIIOO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
___gGIOIIOO x0 x2 x3 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x5,
                                                                                                                     x4) <- _fGIIIOO x0 x2 x3 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                    let {x1 = x4};
                                                                                                                    return (x1,
                                                                                                                            x4,
                                                                                                                            x5)},
                                                                                                                do {(x38,
                                                                                                                     x39) <- ______gIIIOO x0 x2 x3 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                    (x1,
                                                                                                                     x6) <- case x38 of
                                                                                                                            {Cons y1
                                                                                                                                  y6 -> return (y1,
                                                                                                                                                y6);
                                                                                                                             _ -> mzero};
                                                                                                                    (x40,
                                                                                                                     x41) <- case x39 of
                                                                                                                             {Cons y40
                                                                                                                                   y41 -> return (y40,
                                                                                                                                                  y41);
                                                                                                                              _ -> mzero};
                                                                                                                    guard (x40 == x1);
                                                                                                                    guard (x41 == x6);
                                                                                                                    x4 <- neqIO x1 gen_neqIO_x2;
                                                                                                                    x5 <- hIOI x4 x6 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                    return (x1,
                                                                                                                            x4,
                                                                                                                            x5)}]
_fGIIIOO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
_fGIIIOO x0 x1 x2 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqOI_x2 = msum [do {let {x3 = Nil};
                                                                                                    let {x43 = Nil};
                                                                                                    let {x46 = Nil};
                                                                                                    (x42,
                                                                                                     x44) <- ______gIIIOO x0 x1 x2 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                    x4 <- case x42 of
                                                                                                          {Cons y4
                                                                                                                y43 -> do {guard (x43 == y43);
                                                                                                                           return y4};
                                                                                                           _ -> mzero};
                                                                                                    x45 <- case x44 of
                                                                                                           {Cons y45
                                                                                                                 y46 -> do {guard (x46 == y46);
                                                                                                                            return y45};
                                                                                                            _ -> mzero};
                                                                                                    guard (x45 == x4);
                                                                                                    return (x3,
                                                                                                            x4)},
                                                                                                do {(x47,
                                                                                                     x48) <- ______gIIIOO x0 x1 x2 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                    (x4,
                                                                                                     x7) <- case x47 of
                                                                                                            {Cons y4
                                                                                                                  y7 -> return (y4,
                                                                                                                                y7);
                                                                                                             _ -> mzero};
                                                                                                    (x49,
                                                                                                     x50) <- case x48 of
                                                                                                             {Cons y49
                                                                                                                   y50 -> return (y49,
                                                                                                                                  y50);
                                                                                                              _ -> mzero};
                                                                                                    guard (x49 == x4);
                                                                                                    guard (x50 == x7);
                                                                                                    (x5,
                                                                                                     x6) <- hOOI x7 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                    let {x3 = Cons x5 x6};
                                                                                                    return (x3,
                                                                                                            x4)}]
hIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
hIOO x0 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x16 = Nil};
                                                                                                 let {x1 = Nil};
                                                                                                 let {x17 = x0};
                                                                                                 let {x18 = x16};
                                                                                                 let {x2 = Cons x17 x18};
                                                                                                 return (x1,
                                                                                                         x2)},
                                                                                             do {(x2,
                                                                                                  x3,
                                                                                                  x4) <- ____gIOOO x0 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                 let {x1 = Cons x3 x4};
                                                                                                 return (x1,
                                                                                                         x2)}]
____gIOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
____gIOOO x0 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x19,
                                                                                                       x2) <- do {x2 <- gen_____gIOOO_x2;
                                                                                                                  return (x2,
                                                                                                                          x2)};
                                                                                                      guard (x0 == x2);
                                                                                                      (x1,
                                                                                                       x4) <- do {x4 <- gen_____gIOOO_x4;
                                                                                                                  let {x1 = Cons x19 x4};
                                                                                                                  return (x1,
                                                                                                                          x4)};
                                                                                                      x3 <- fOI x4 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                      return (x1,
                                                                                                              x2,
                                                                                                              x3)},
                                                                                                  do {let {x20 = x0};
                                                                                                      x2 <- neqIO x0 gen_neqIO_x2;
                                                                                                      (x3,
                                                                                                       x4) <- hIOO x2 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                      let {x1 = Cons x20 x4};
                                                                                                      return (x1,
                                                                                                              x2,
                                                                                                              x3)}]
neqHGIOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
neqHGIOOO x0 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Zero};
                                                                                                                                                                                                                                                                        let {x22 = Zero};
                                                                                                                                                                                                                                                                        let {x23 = Zero};
                                                                                                                                                                                                                                                                        (x3,
                                                                                                                                                                                                                                                                         x21) <- __hGIOOII x0 x22 x23 gen_______gIIIOO_x3 gen_______gIIIOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                        x4 <- case x21 of
                                                                                                                                                                                                                                                                              {Succ y4 -> return y4;
                                                                                                                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                                                                                                                        let {x2 = Succ x4};
                                                                                                                                                                                                                                                                        return (x1,
                                                                                                                                                                                                                                                                                x2,
                                                                                                                                                                                                                                                                                x3)},
                                                                                                                                                                                                                                                                    do {let {x2 = Zero};
                                                                                                                                                                                                                                                                        let {x24 = Zero};
                                                                                                                                                                                                                                                                        (x3,
                                                                                                                                                                                                                                                                         x25,
                                                                                                                                                                                                                                                                         x26) <- __hGIOIOO x0 x24 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                        x4 <- case x25 of
                                                                                                                                                                                                                                                                              {Succ y4 -> return y4;
                                                                                                                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                                                                                                                        let {x1 = Succ x4};
                                                                                                                                                                                                                                                                        x27 <- case x26 of
                                                                                                                                                                                                                                                                               {Succ y27 -> return y27;
                                                                                                                                                                                                                                                                                _ -> mzero};
                                                                                                                                                                                                                                                                        guard (x27 == x4);
                                                                                                                                                                                                                                                                        return (x1,
                                                                                                                                                                                                                                                                                x2,
                                                                                                                                                                                                                                                                                x3)},
                                                                                                                                                                                                                                                                    do {(x3,
                                                                                                                                                                                                                                                                         x28,
                                                                                                                                                                                                                                                                         x29,
                                                                                                                                                                                                                                                                         x30) <- __hGIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                                                        x5 <- case x28 of
                                                                                                                                                                                                                                                                              {Succ y5 -> return y5;
                                                                                                                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                                                                                                                        let {x2 = Succ x5};
                                                                                                                                                                                                                                                                        x6 <- case x29 of
                                                                                                                                                                                                                                                                              {Succ y6 -> return y6;
                                                                                                                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                                                                                                                        neqII x6 x5;
                                                                                                                                                                                                                                                                        x31 <- case x30 of
                                                                                                                                                                                                                                                                               {Succ y31 -> return y31;
                                                                                                                                                                                                                                                                                _ -> mzero};
                                                                                                                                                                                                                                                                        guard (x31 == x6);
                                                                                                                                                                                                                                                                        let {x32 = x6};
                                                                                                                                                                                                                                                                        let {x1 = Succ x32};
                                                                                                                                                                                                                                                                        return (x1,
                                                                                                                                                                                                                                                                                x2,
                                                                                                                                                                                                                                                                                x3)}]
__hGIOIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
__hGIOIOO x0 x2 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Nil};
                                                                                                                                                                                                                                   let {x34 = Nil};
                                                                                                                                                                                                                                   let {x33 = Cons x2 x34};
                                                                                                                                                                                                                                   let {x37 = Nil};
                                                                                                                                                                                                                                   let {x36 = x2};
                                                                                                                                                                                                                                   let {x35 = Cons x36 x37};
                                                                                                                                                                                                                                   (x3,
                                                                                                                                                                                                                                    x4) <- ______gIOOII x0 x33 x35 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                           x3,
                                                                                                                                                                                                                                           x4)},
                                                                                                                                                                                                                               do {(x3,
                                                                                                                                                                                                                                    x4,
                                                                                                                                                                                                                                    x5,
                                                                                                                                                                                                                                    x6) <- ___gGIIOOOO x0 x2 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                   let {x1 = Cons x5 x6};
                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                           x3,
                                                                                                                                                                                                                                           x4)}]
___gGIIOOOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
___gGIIOOOO x0 x1 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x2,
                                                                                                                                                                                                                                      x3,
                                                                                                                                                                                                                                      x5,
                                                                                                                                                                                                                                      x4) <- _fGIOOOO x0 gen_______gIOOOO_x2 gen_______gIOOOO_x3 gen_______gIOOOO_x4 gen_______gIOOOO_x78 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                     guard (x1 == x4);
                                                                                                                                                                                                                                     return (x2,
                                                                                                                                                                                                                                             x3,
                                                                                                                                                                                                                                             x4,
                                                                                                                                                                                                                                             x5)},
                                                                                                                                                                                                                                 do {let {x40 = x1};
                                                                                                                                                                                                                                     x4 <- neqIO x1 gen_neqIO_x2;
                                                                                                                                                                                                                                     (x5,
                                                                                                                                                                                                                                      x6) <- hIOO x4 gen_____gIOOO_x2 gen_____gIOOO_x4 gen_____gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                     let {x38 = Cons x1 x6};
                                                                                                                                                                                                                                     let {x41 = x6};
                                                                                                                                                                                                                                     let {x39 = Cons x40 x41};
                                                                                                                                                                                                                                     (x2,
                                                                                                                                                                                                                                      x3) <- ______gIOOII x0 x38 x39 gen_______gIOOII_x2 gen_______gIOOII_x78 gen_____gOIOO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                     return (x2,
                                                                                                                                                                                                                                             x3,
                                                                                                                                                                                                                                             x4,
                                                                                                                                                                                                                                             x5)}]
rrO :: MonadPlus m => m Term -> m Term -> m Term
rrO gen_neqOI_x2 gen_rrO_x7 = msum [do {(x8,
                                         x7) <- do {x7 <- gen_rrO_x7; return (x7, x7)};
                                        (x2, x3) <- case x7 of
                                                    {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                        x1 <- gGOII x2 x3 gen_neqOI_x2;
                                        let {x0 = Cons x1 x8};
                                        return x0}]
gGOII :: MonadPlus m => Term -> Term -> m Term -> m Term
gGOII x1 x2 gen_neqOI_x2 = msum [do {(x3, x4) <- case x2 of
                                                 {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                     hGIII x1 x3 x4 gen_neqOI_x2;
                                     let {x0 = x1};
                                     return x0},
                                 do {x0 <- _neqHGOII x1 x2 gen_neqOI_x2; return x0}]
_neqHGOII :: MonadPlus m => Term -> Term -> m Term -> m Term
_neqHGOII x1 x2 gen_neqOI_x2 = msum [do {let {x0 = Zero};
                                         let {x94 = Zero};
                                         x3 <- case x1 of
                                               {Succ y3 -> return y3; _ -> mzero};
                                         let {x95 = Succ x3};
                                         hGIII x94 x95 x2 gen_neqOI_x2;
                                         return x0},
                                     do {let {x97 = Zero};
                                         guard (x1 == Zero);
                                         x96 <- hGOII x97 x2 gen_neqOI_x2;
                                         x3 <- case x96 of
                                               {Succ y3 -> return y3; _ -> mzero};
                                         let {x0 = Succ x3};
                                         return x0},
                                     do {x4 <- case x1 of
                                               {Succ y4 -> return y4; _ -> mzero};
                                         let {x99 = Succ x4};
                                         x98 <- hGOII x99 x2 gen_neqOI_x2;
                                         x5 <- case x98 of
                                               {Succ y5 -> return y5; _ -> mzero};
                                         neqII x5 x4;
                                         let {x100 = x5};
                                         let {x0 = Succ x100};
                                         return x0}]
hGIII :: MonadPlus m => Term -> Term -> Term -> m Term -> m ()
hGIII x0 x1 x2 gen_neqOI_x2 = msum [do {let {x10 = Nil};
                                        let {x11 = Nil};
                                        guard (x2 == Nil);
                                        let {x9 = x1};
                                        ______gIIIII x0 x1 x9 x10 x11;
                                        return ()},
                                    do {(x3, x4) <- case x2 of
                                                    {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                        _gGIIII x0 x1 x3 x4 gen_neqOI_x2;
                                        return ()}]
_gGIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m ()
_gGIIII x0 x1 x2 x3 gen_neqOI_x2 = msum [do {guard (x1 == x2);
                                             fGIII x0 x3 x2;
                                             return ()},
                                         do {neqHGIIII x0 x1 x2 x3 gen_neqOI_x2; return ()}]
fGIII :: MonadPlus m => Term -> Term -> Term -> m ()
fGIII x0 x1 x2 = msum [do {let {x13 = Nil};
                           let {x14 = Nil};
                           guard (x1 == Nil);
                           let {x12 = x2};
                           ______gIIIII x0 x2 x12 x13 x14;
                           return ()},
                       do {(x3, x4) <- case x1 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           let {x15 = x2};
                           __hGIIIII x0 x4 x3 x2 x15;
                           return ()}]
__hGIIIII :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m ()
__hGIIIII x0 x1 x2 x3 x4 = msum [do {let {x34 = Nil};
                                     let {x33 = Cons x2 x34};
                                     let {x37 = Nil};
                                     guard (x1 == Nil);
                                     let {x36 = x2};
                                     let {x35 = Cons x36 x37};
                                     ______gIIIII x0 x3 x4 x33 x35;
                                     return ()},
                                 do {(x5, x6) <- case x1 of
                                                 {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                     ___gGIIIIII x0 x2 x3 x4 x5 x6;
                                     return ()}]
___gGIIIIII :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> Term -> m ()
___gGIIIIII x0 x1 x2 x3 x4 x5 = msum [do {guard (x1 == x4);
                                          _fGIIIII x0 x2 x3 x5 x4;
                                          return ()},
                                      do {neqII x1 x4;
                                          let {x40 = x1};
                                          x6 <- hIIO x4 x5;
                                          let {x38 = Cons x1 x6};
                                          let {x41 = x6};
                                          let {x39 = Cons x40 x41};
                                          ______gIIIII x0 x2 x3 x38 x39;
                                          return ()}]
_fGIIIII :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m ()
_fGIIIII x0 x1 x2 x3 x4 = msum [do {let {x43 = Nil};
                                    let {x42 = Cons x4 x43};
                                    let {x46 = Nil};
                                    guard (x3 == Nil);
                                    let {x45 = x4};
                                    let {x44 = Cons x45 x46};
                                    ______gIIIII x0 x1 x2 x42 x44;
                                    return ()},
                                do {(x5, x6) <- case x3 of
                                                {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                    let {x49 = x4};
                                    x7 <- hIIO x5 x6;
                                    let {x47 = Cons x4 x7};
                                    let {x50 = x7};
                                    let {x48 = Cons x49 x50};
                                    ______gIIIII x0 x1 x2 x47 x48;
                                    return ()}]
hIIO :: MonadPlus m => Term -> Term -> m Term
hIIO x0 x1 = msum [do {let {x16 = Nil};
                       guard (x1 == Nil);
                       let {x17 = x0};
                       let {x18 = x16};
                       let {x2 = Cons x17 x18};
                       return x2},
                   do {(x3, x4) <- case x1 of
                                   {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                       x2 <- ____gIOII x0 x3 x4;
                       return x2}]
____gIOII :: MonadPlus m => Term -> Term -> Term -> m Term
____gIOII x0 x2 x3 = msum [do {guard (x0 == x2);
                               let {x19 = x2};
                               x4 <- fIO x3;
                               let {x1 = Cons x19 x4};
                               return x1},
                           do {neqII x0 x2;
                               let {x20 = x0};
                               x4 <- hIIO x2 x3;
                               let {x1 = Cons x20 x4};
                               return x1}]
fIO :: MonadPlus m => Term -> m Term
fIO x0 = msum [do {let {x1 = Nil}; guard (x0 == Nil); return x1},
               do {(x2, x3) <- case x0 of
                               {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                   x1 <- hIIO x2 x3;
                   return x1}]
hGOII :: MonadPlus m => Term -> Term -> m Term -> m Term
hGOII x1 x2 gen_neqOI_x2 = msum [do {let {x10 = Nil};
                                     let {x11 = Nil};
                                     guard (x2 == Nil);
                                     let {x9 = x1};
                                     x0 <- ______gOIIII x1 x9 x10 x11;
                                     return x0},
                                 do {(x3, x4) <- case x2 of
                                                 {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                     x0 <- _gGOIII x1 x3 x4 gen_neqOI_x2;
                                     return x0}]
______gOIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term
______gOIIII x1 x2 x3 x4 = msum [do {let {x51 = Zero};
                                     let {x54 = Zero};
                                     let {x58 = Zero};
                                     let {x57 = Succ x58};
                                     let {x56 = Succ x57};
                                     let {x60 = Zero};
                                     let {x62 = Zero};
                                     let {x63 = Nil};
                                     let {x61 = Cons x62 x63};
                                     let {x59 = Cons x60 x61};
                                     let {x55 = Cons x56 x59};
                                     let {x53 = Cons x54 x55};
                                     fII x4 x53;
                                     let {x52 = x51};
                                     let {x0 = Succ x52};
                                     guard (x1 == x0);
                                     return x0},
                                 do {let {x64 = Zero};
                                     let {x66 = Cons x2 x3};
                                     let {x68 = Zero};
                                     let {x72 = Zero};
                                     let {x71 = Succ x72};
                                     let {x70 = Succ x71};
                                     let {x74 = Zero};
                                     let {x76 = Zero};
                                     let {x77 = Nil};
                                     let {x75 = Cons x76 x77};
                                     let {x73 = Cons x74 x75};
                                     let {x69 = Cons x70 x73};
                                     let {x67 = Cons x68 x69};
                                     fII x66 x67;
                                     guard (x1 == Zero);
                                     let {x65 = x64};
                                     let {x0 = Succ x65};
                                     return x0},
                                 do {let {x80 = Zero};
                                     let {x82 = Cons x2 x3};
                                     let {x84 = Zero};
                                     let {x88 = Zero};
                                     let {x87 = Succ x88};
                                     let {x86 = Succ x87};
                                     let {x90 = Zero};
                                     let {x92 = Zero};
                                     let {x93 = Nil};
                                     let {x91 = Cons x92 x93};
                                     let {x89 = Cons x90 x91};
                                     let {x85 = Cons x86 x89};
                                     let {x83 = Cons x84 x85};
                                     fII x82 x83;
                                     x79 <- case x1 of
                                            {Succ y79 -> return y79; _ -> mzero};
                                     let {x81 = x80};
                                     let {x0 = Succ x81};
                                     let {x78 = x79};
                                     x5 <- case x78 of
                                           {Succ y5 -> return y5; _ -> mzero};
                                     return x0}]
_gGOIII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term
_gGOIII x1 x2 x3 gen_neqOI_x2 = msum [do {guard (x1 == x2);
                                          x0 <- fGOII x3 x2;
                                          return x0},
                                      do {x0 <- neqHGOIII x1 x2 x3 gen_neqOI_x2; return x0}]
fGOII :: MonadPlus m => Term -> Term -> m Term
fGOII x1 x2 = msum [do {let {x13 = Nil};
                        let {x14 = Nil};
                        guard (x1 == Nil);
                        let {x12 = x2};
                        x0 <- ______gOIIII x2 x12 x13 x14;
                        return x0},
                    do {(x3, x4) <- case x1 of
                                    {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                        let {x15 = x2};
                        x0 <- __hGOIIII x4 x3 x2 x15;
                        return x0}]
__hGOIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term
__hGOIIII x1 x2 x3 x4 = msum [do {let {x34 = Nil};
                                  let {x33 = Cons x2 x34};
                                  let {x37 = Nil};
                                  guard (x1 == Nil);
                                  let {x36 = x2};
                                  let {x35 = Cons x36 x37};
                                  x0 <- ______gOIIII x3 x4 x33 x35;
                                  return x0},
                              do {(x5, x6) <- case x1 of
                                              {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                  x0 <- ___gGOIIIII x2 x3 x4 x5 x6;
                                  return x0}]
___gGOIIIII :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m Term
___gGOIIIII x1 x2 x3 x4 x5 = msum [do {guard (x1 == x4);
                                       x0 <- _fGOIIII x2 x3 x5 x4;
                                       return x0},
                                   do {neqII x1 x4;
                                       let {x40 = x1};
                                       x6 <- hIIO x4 x5;
                                       let {x38 = Cons x1 x6};
                                       let {x41 = x6};
                                       let {x39 = Cons x40 x41};
                                       x0 <- ______gOIIII x2 x3 x38 x39;
                                       return x0}]
_fGOIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term
_fGOIIII x1 x2 x3 x4 = msum [do {let {x43 = Nil};
                                 let {x42 = Cons x4 x43};
                                 let {x46 = Nil};
                                 guard (x3 == Nil);
                                 let {x45 = x4};
                                 let {x44 = Cons x45 x46};
                                 x0 <- ______gOIIII x1 x2 x42 x44;
                                 return x0},
                             do {(x5, x6) <- case x3 of
                                             {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                 let {x49 = x4};
                                 x7 <- hIIO x5 x6;
                                 let {x47 = Cons x4 x7};
                                 let {x50 = x7};
                                 let {x48 = Cons x49 x50};
                                 x0 <- ______gOIIII x1 x2 x47 x48;
                                 return x0}]
neqHGIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m ()
neqHGIIII x0 x1 x2 x3 gen_neqOI_x2 = msum [do {let {x22 = Zero};
                                               let {x23 = Zero};
                                               x4 <- case x2 of
                                                     {Succ y4 -> return y4; _ -> mzero};
                                               let {x21 = Succ x4};
                                               __hGIIIII x0 x3 x21 x22 x23;
                                               guard (x1 == Zero);
                                               return ()},
                                           do {let {x24 = Zero};
                                               guard (x2 == Zero);
                                               x4 <- case x1 of
                                                     {Succ y4 -> return y4; _ -> mzero};
                                               let {x25 = Succ x4};
                                               let {x27 = x4};
                                               let {x26 = Succ x27};
                                               __hGIIIII x0 x3 x24 x25 x26;
                                               return ()},
                                           do {x5 <- case x2 of
                                                     {Succ y5 -> return y5; _ -> mzero};
                                               let {x28 = Succ x5};
                                               x32 <- case x1 of
                                                      {Succ y32 -> return y32; _ -> mzero};
                                               x6 <- neqOI x5 gen_neqOI_x2;
                                               guard (x32 == x6);
                                               let {x29 = Succ x6};
                                               let {x31 = x6};
                                               let {x30 = Succ x31};
                                               __hGIIIII x0 x3 x28 x29 x30;
                                               return ()}]
neqHGOIII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term
neqHGOIII x1 x2 x3 gen_neqOI_x2 = msum [do {let {x22 = Zero};
                                            let {x23 = Zero};
                                            x4 <- case x2 of
                                                  {Succ y4 -> return y4; _ -> mzero};
                                            let {x21 = Succ x4};
                                            guard (x1 == Zero);
                                            x0 <- __hGOIIII x3 x21 x22 x23;
                                            return x0},
                                        do {let {x24 = Zero};
                                            guard (x2 == Zero);
                                            x4 <- case x1 of
                                                  {Succ y4 -> return y4; _ -> mzero};
                                            let {x25 = Succ x4};
                                            let {x27 = x4};
                                            let {x26 = Succ x27};
                                            x0 <- __hGOIIII x3 x24 x25 x26;
                                            return x0},
                                        do {x5 <- case x2 of
                                                  {Succ y5 -> return y5; _ -> mzero};
                                            let {x28 = Succ x5};
                                            x32 <- case x1 of
                                                   {Succ y32 -> return y32; _ -> mzero};
                                            x6 <- neqOI x5 gen_neqOI_x2;
                                            guard (x32 == x6);
                                            let {x29 = Succ x6};
                                            let {x31 = x6};
                                            let {x30 = Succ x31};
                                            x0 <- __hGOIIII x3 x28 x29 x30;
                                            return x0}]