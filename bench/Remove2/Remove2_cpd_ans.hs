module Remove2_cpd_ans where

import Stream
import Control.Monad
import Term

rrI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m ()
rrI x0 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x1,
                                                                                                                                                                                                                                  x8) <- case x0 of
                                                                                                                                                                                                                                         {Cons y1
                                                                                                                                                                                                                                               y8 -> return (y1,
                                                                                                                                                                                                                                                             y8);
                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                 (x2,
                                                                                                                                                                                                                                  x3) <- gGIOO x1 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                 let {x7 = Cons x2 x3};
                                                                                                                                                                                                                                 guard (x8 == x7);
                                                                                                                                                                                                                                 return ()}]
gGIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
gGIOO x0 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = x0};
                                                                                                                                                                                                                                   (x3,
                                                                                                                                                                                                                                    x4) <- hGIOO x1 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                   let {x2 = Cons x3 x4};
                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                           x2)},
                                                                                                                                                                                                                               do {(x1,
                                                                                                                                                                                                                                    x2) <- _neqHGIOO x0 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                           x2)}]
_neqHGIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
_neqHGIOO x0 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x91 = Zero};
                                                                                                                                                                                                                                       guard (x0 == Zero);
                                                                                                                                                                                                                                       (x92,
                                                                                                                                                                                                                                        x2) <- hGIOO x91 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                       x3 <- case x92 of
                                                                                                                                                                                                                                             {Succ y3 -> return y3;
                                                                                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                                                                                       let {x1 = Succ x3};
                                                                                                                                                                                                                                       return (x1,
                                                                                                                                                                                                                                               x2)},
                                                                                                                                                                                                                                   do {let {x1 = Zero};
                                                                                                                                                                                                                                       let {x94 = Zero};
                                                                                                                                                                                                                                       x3 <- case x0 of
                                                                                                                                                                                                                                             {Succ y3 -> return y3;
                                                                                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                                                                                       let {x93 = Succ x3};
                                                                                                                                                                                                                                       x2 <- hGIIO x93 x94 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                       return (x1,
                                                                                                                                                                                                                                               x2)},
                                                                                                                                                                                                                                   do {x97 <- case x0 of
                                                                                                                                                                                                                                              {Succ y97 -> return y97;
                                                                                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                                                                                       let {x5 = x97};
                                                                                                                                                                                                                                       let {x95 = Succ x5};
                                                                                                                                                                                                                                       x4 <- neqIO x5 gen_neqIO_x2;
                                                                                                                                                                                                                                       let {x1 = Succ x4};
                                                                                                                                                                                                                                       let {x96 = Succ x4};
                                                                                                                                                                                                                                       x2 <- hGIIO x95 x96 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                       return (x1,
                                                                                                                                                                                                                                               x2)}]
hGIIO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term
hGIIO x0 x1 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x2 = Nil};
                                                                                                                                                                                                                                      let {x10 = Nil};
                                                                                                                                                                                                                                      let {x11 = Nil};
                                                                                                                                                                                                                                      let {x9 = x1};
                                                                                                                                                                                                                                      _______gIIIII x0 x1 x9 x10 x11;
                                                                                                                                                                                                                                      return x2},
                                                                                                                                                                                                                                  do {(x3,
                                                                                                                                                                                                                                       x4) <- _gGIIOO x0 x1 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                      let {x2 = Cons x3 x4};
                                                                                                                                                                                                                                      return x2}]
_______gIIIII :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m ()
_______gIIIII x0 x1 x2 x3 x4 = msum [do {guard (x1 == x0);
                                         let {x63 = Zero};
                                         let {x66 = Zero};
                                         let {x70 = Zero};
                                         let {x69 = Succ x70};
                                         let {x68 = Succ x69};
                                         let {x72 = Zero};
                                         let {x74 = Zero};
                                         let {x75 = Nil};
                                         let {x73 = Cons x74 x75};
                                         let {x71 = Cons x72 x73};
                                         let {x67 = Cons x68 x71};
                                         let {x65 = Cons x66 x67};
                                         fII x4 x65;
                                         x64 <- case x0 of
                                                {Succ y64 -> return y64; _ -> mzero};
                                         guard (x64 == x63);
                                         return ()},
                                     do {let {x77 = Zero};
                                         let {x76 = Succ x77};
                                         neqII x76 x1;
                                         let {x78 = Zero};
                                         let {x81 = Zero};
                                         let {x85 = Zero};
                                         let {x84 = Succ x85};
                                         let {x83 = Succ x84};
                                         let {x87 = Zero};
                                         let {x89 = Zero};
                                         let {x90 = Nil};
                                         let {x88 = Cons x89 x90};
                                         let {x86 = Cons x87 x88};
                                         let {x82 = Cons x83 x86};
                                         let {x80 = Cons x81 x82};
                                         (x5, x6) <- case x3 of
                                                     {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                         _____gIIII x2 x80 x5 x6;
                                         x79 <- case x0 of
                                                {Succ y79 -> return y79; _ -> mzero};
                                         guard (x79 == x78);
                                         return ()}]
_____gIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m ()
_____gIIII x0 x1 x2 x3 = msum [do {guard (x0 == x2);
                                   (x31, x4) <- case x1 of
                                                {Cons y31 y4 -> return (y31, y4); _ -> mzero};
                                   guard (x31 == x2);
                                   fII x3 x4;
                                   return ()},
                               do {neqII x0 x2;
                                   (x32, x4) <- case x1 of
                                                {Cons y32 y4 -> return (y32, y4); _ -> mzero};
                                   guard (x32 == x0);
                                   hIII x2 x3 x4;
                                   return ()}]
_gGIIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
_gGIIOO x0 x1 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x3,
                                                                                                                                                                                                                                         x2) <- fGIOO x0 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                        guard (x1 == x2);
                                                                                                                                                                                                                                        return (x2,
                                                                                                                                                                                                                                                x3)},
                                                                                                                                                                                                                                    do {(x2,
                                                                                                                                                                                                                                         x3) <- neqHGIIOO x0 x1 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
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
fGIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
fGIOO x0 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Nil};
                                                                                                                                                     let {x13 = Nil};
                                                                                                                                                     let {x14 = Nil};
                                                                                                                                                     (x2,
                                                                                                                                                      x12) <- _______gIOOII x0 x13 x14 gen________gIOOII_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                     guard (x12 == x2);
                                                                                                                                                     return (x1,
                                                                                                                                                             x2)},
                                                                                                                                                 do {(x4,
                                                                                                                                                      x3,
                                                                                                                                                      x2,
                                                                                                                                                      x15) <- __hGIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                     guard (x15 == x2);
                                                                                                                                                     let {x1 = Cons x3 x4};
                                                                                                                                                     return (x1,
                                                                                                                                                             x2)}]
_______gIOOII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m (Term, Term)
_______gIOOII x0 x3 x4 gen________gIOOII_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x63 = Zero};
                                                                                  let {x66 = Zero};
                                                                                  let {x70 = Zero};
                                                                                  let {x69 = Succ x70};
                                                                                  let {x68 = Succ x69};
                                                                                  let {x72 = Zero};
                                                                                  let {x74 = Zero};
                                                                                  let {x75 = Nil};
                                                                                  let {x73 = Cons x74 x75};
                                                                                  let {x71 = Cons x72 x73};
                                                                                  let {x67 = Cons x68 x71};
                                                                                  let {x65 = Cons x66 x67};
                                                                                  fII x4 x65;
                                                                                  x64 <- case x0 of
                                                                                         {Succ y64 -> return y64;
                                                                                          _ -> mzero};
                                                                                  guard (x64 == x63);
                                                                                  let {x1 = x0};
                                                                                  x2 <- gen________gIOOII_x2;
                                                                                  return (x1, x2)},
                                                                              do {let {x77 = Zero};
                                                                                  let {x76 = Succ x77};
                                                                                  let {x78 = Zero};
                                                                                  let {x81 = Zero};
                                                                                  let {x85 = Zero};
                                                                                  let {x84 = Succ x85};
                                                                                  let {x83 = Succ x84};
                                                                                  let {x87 = Zero};
                                                                                  let {x89 = Zero};
                                                                                  let {x90 = Nil};
                                                                                  let {x88 = Cons x89 x90};
                                                                                  let {x86 = Cons x87 x88};
                                                                                  let {x82 = Cons x83 x86};
                                                                                  let {x80 = Cons x81 x82};
                                                                                  (x5,
                                                                                   x6) <- case x3 of
                                                                                          {Cons y5
                                                                                                y6 -> return (y5,
                                                                                                              y6);
                                                                                           _ -> mzero};
                                                                                  x79 <- case x0 of
                                                                                         {Succ y79 -> return y79;
                                                                                          _ -> mzero};
                                                                                  guard (x79 == x78);
                                                                                  x1 <- neqIO x76 gen_neqIO_x2;
                                                                                  x2 <- _____gOIII x80 x5 x6 gen_neqOI_x2;
                                                                                  return (x1, x2)}]
_____gOIII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term
_____gOIII x1 x2 x3 gen_neqOI_x2 = msum [do {(x31,
                                              x4) <- case x1 of
                                                     {Cons y31 y4 -> return (y31, y4); _ -> mzero};
                                             guard (x31 == x2);
                                             fII x3 x4;
                                             let {x0 = x2};
                                             return x0},
                                         do {(x32, x4) <- case x1 of
                                                          {Cons y32 y4 -> return (y32, y4);
                                                           _ -> mzero};
                                             hIII x2 x3 x4;
                                             x0 <- neqOI x2 gen_neqOI_x2;
                                             guard (x32 == x0);
                                             return x0}]
__hGIOOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
__hGIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Nil};
                                                                                                                                    let {x46 = Nil};
                                                                                                                                    let {x49 = Nil};
                                                                                                                                    (x3,
                                                                                                                                     x4,
                                                                                                                                     x45,
                                                                                                                                     x47) <- _______gIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                    x2 <- case x45 of
                                                                                                                                          {Cons y2
                                                                                                                                                y46 -> do {guard (x46 == y46);
                                                                                                                                                           return y2};
                                                                                                                                           _ -> mzero};
                                                                                                                                    x48 <- case x47 of
                                                                                                                                           {Cons y48
                                                                                                                                                 y49 -> do {guard (x49 == y49);
                                                                                                                                                            return y48};
                                                                                                                                            _ -> mzero};
                                                                                                                                    guard (x48 == x2);
                                                                                                                                    return (x1,
                                                                                                                                            x2,
                                                                                                                                            x3,
                                                                                                                                            x4)},
                                                                                                                                do {(x2,
                                                                                                                                     x3,
                                                                                                                                     x4,
                                                                                                                                     x5,
                                                                                                                                     x6) <- ___gGIOOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                    let {x1 = Cons x5 x6};
                                                                                                                                    return (x1,
                                                                                                                                            x2,
                                                                                                                                            x3,
                                                                                                                                            x4)}]
_______gIOOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
_______gIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x63 = Zero};
                                                                                                                                        let {x66 = Zero};
                                                                                                                                        let {x70 = Zero};
                                                                                                                                        let {x69 = Succ x70};
                                                                                                                                        let {x68 = Succ x69};
                                                                                                                                        let {x72 = Zero};
                                                                                                                                        let {x74 = Zero};
                                                                                                                                        let {x75 = Nil};
                                                                                                                                        let {x73 = Cons x74 x75};
                                                                                                                                        let {x71 = Cons x72 x73};
                                                                                                                                        let {x67 = Cons x68 x71};
                                                                                                                                        let {x65 = Cons x66 x67};
                                                                                                                                        x64 <- case x0 of
                                                                                                                                               {Succ y64 -> return y64;
                                                                                                                                                _ -> mzero};
                                                                                                                                        guard (x64 == x63);
                                                                                                                                        let {x1 = x0};
                                                                                                                                        x4 <- fOI x65 gen______gOIOO_x2 gen_neqOI_x2;
                                                                                                                                        x2 <- gen________gIOOOO_x2;
                                                                                                                                        x3 <- gen________gIOOOO_x3;
                                                                                                                                        return (x1,
                                                                                                                                                x2,
                                                                                                                                                x3,
                                                                                                                                                x4)},
                                                                                                                                    do {let {x77 = Zero};
                                                                                                                                        let {x76 = Succ x77};
                                                                                                                                        let {x78 = Zero};
                                                                                                                                        let {x81 = Zero};
                                                                                                                                        let {x85 = Zero};
                                                                                                                                        let {x84 = Succ x85};
                                                                                                                                        let {x83 = Succ x84};
                                                                                                                                        let {x87 = Zero};
                                                                                                                                        let {x89 = Zero};
                                                                                                                                        let {x90 = Nil};
                                                                                                                                        let {x88 = Cons x89 x90};
                                                                                                                                        let {x86 = Cons x87 x88};
                                                                                                                                        let {x82 = Cons x83 x86};
                                                                                                                                        let {x80 = Cons x81 x82};
                                                                                                                                        x79 <- case x0 of
                                                                                                                                               {Succ y79 -> return y79;
                                                                                                                                                _ -> mzero};
                                                                                                                                        guard (x79 == x78);
                                                                                                                                        x1 <- neqIO x76 gen_neqIO_x2;
                                                                                                                                        (x2,
                                                                                                                                         x5,
                                                                                                                                         x6) <- _____gOIOO x80 gen______gOIOO_x2 gen_neqOI_x2;
                                                                                                                                        let {x3 = Cons x5 x6};
                                                                                                                                        x4 <- gen________gIOOOO_x4;
                                                                                                                                        return (x1,
                                                                                                                                                x2,
                                                                                                                                                x3,
                                                                                                                                                x4)}]
_____gOIOO :: MonadPlus m => Term -> m Term -> m Term -> m (Term, Term, Term)
_____gOIOO x1 gen______gOIOO_x2 gen_neqOI_x2 = msum [do {(x31,
                                                          x4) <- case x1 of
                                                                 {Cons y31 y4 -> return (y31, y4);
                                                                  _ -> mzero};
                                                         x3 <- fOI x4 gen______gOIOO_x2 gen_neqOI_x2;
                                                         (x0, x2) <- do {x2 <- gen______gOIOO_x2;
                                                                         return (x2, x2)};
                                                         guard (x31 == x2);
                                                         return (x0, x2, x3)},
                                                     do {(x32, x4) <- case x1 of
                                                                      {Cons y32 y4 -> return (y32,
                                                                                              y4);
                                                                       _ -> mzero};
                                                         (x2,
                                                          x3) <- hOOI x4 gen______gOIOO_x2 gen_neqOI_x2;
                                                         x0 <- neqOI x2 gen_neqOI_x2;
                                                         guard (x32 == x0);
                                                         return (x0, x2, x3)}]
___gGIOOOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term, Term)
___gGIOOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x2,
                                                                                                                                       x3,
                                                                                                                                       x5,
                                                                                                                                       x4) <- _fGIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                      let {x1 = x4};
                                                                                                                                      return (x1,
                                                                                                                                              x2,
                                                                                                                                              x3,
                                                                                                                                              x4,
                                                                                                                                              x5)},
                                                                                                                                  do {(x2,
                                                                                                                                       x3,
                                                                                                                                       x50,
                                                                                                                                       x51) <- _______gIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                      (x1,
                                                                                                                                       x6) <- case x50 of
                                                                                                                                              {Cons y1
                                                                                                                                                    y6 -> return (y1,
                                                                                                                                                                  y6);
                                                                                                                                               _ -> mzero};
                                                                                                                                      (x52,
                                                                                                                                       x53) <- case x51 of
                                                                                                                                               {Cons y52
                                                                                                                                                     y53 -> return (y52,
                                                                                                                                                                    y53);
                                                                                                                                                _ -> mzero};
                                                                                                                                      guard (x52 == x1);
                                                                                                                                      guard (x53 == x6);
                                                                                                                                      x4 <- neqIO x1 gen_neqIO_x2;
                                                                                                                                      x5 <- hIOI x4 x6 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                      return (x1,
                                                                                                                                              x2,
                                                                                                                                              x3,
                                                                                                                                              x4,
                                                                                                                                              x5)}]
_fGIOOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
_fGIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x3 = Nil};
                                                                                                                                   let {x55 = Nil};
                                                                                                                                   let {x58 = Nil};
                                                                                                                                   (x1,
                                                                                                                                    x2,
                                                                                                                                    x54,
                                                                                                                                    x56) <- _______gIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                   x4 <- case x54 of
                                                                                                                                         {Cons y4
                                                                                                                                               y55 -> do {guard (x55 == y55);
                                                                                                                                                          return y4};
                                                                                                                                          _ -> mzero};
                                                                                                                                   x57 <- case x56 of
                                                                                                                                          {Cons y57
                                                                                                                                                y58 -> do {guard (x58 == y58);
                                                                                                                                                           return y57};
                                                                                                                                           _ -> mzero};
                                                                                                                                   guard (x57 == x4);
                                                                                                                                   return (x1,
                                                                                                                                           x2,
                                                                                                                                           x3,
                                                                                                                                           x4)},
                                                                                                                               do {(x1,
                                                                                                                                    x2,
                                                                                                                                    x59,
                                                                                                                                    x60) <- _______gIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                   (x4,
                                                                                                                                    x7) <- case x59 of
                                                                                                                                           {Cons y4
                                                                                                                                                 y7 -> return (y4,
                                                                                                                                                               y7);
                                                                                                                                            _ -> mzero};
                                                                                                                                   (x61,
                                                                                                                                    x62) <- case x60 of
                                                                                                                                            {Cons y61
                                                                                                                                                  y62 -> return (y61,
                                                                                                                                                                 y62);
                                                                                                                                             _ -> mzero};
                                                                                                                                   guard (x61 == x4);
                                                                                                                                   guard (x62 == x7);
                                                                                                                                   (x5,
                                                                                                                                    x6) <- hOOI x7 gen______gOIOO_x2 gen_neqOI_x2;
                                                                                                                                   let {x3 = Cons x5 x6};
                                                                                                                                   return (x1,
                                                                                                                                           x2,
                                                                                                                                           x3,
                                                                                                                                           x4)}]
fOI :: MonadPlus m => Term -> m Term -> m Term -> m Term
fOI x1 gen______gOIOO_x2 gen_neqOI_x2 = msum [do {let {x0 = Nil};
                                                  guard (x1 == Nil);
                                                  return x0},
                                              do {(x2,
                                                   x3) <- hOOI x1 gen______gOIOO_x2 gen_neqOI_x2;
                                                  let {x0 = Cons x2 x3};
                                                  return x0}]
hIII :: MonadPlus m => Term -> Term -> Term -> m ()
hIII x0 x1 x2 = msum [do {let {x28 = Nil};
                          (x29, x30) <- case x2 of
                                        {Cons y29 y30 -> return (y29, y30); _ -> mzero};
                          guard (x29 == x0);
                          guard (x30 == x28);
                          guard (x1 == Nil);
                          return ()},
                      do {(x3, x4) <- case x1 of
                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                          _____gIIII x0 x2 x3 x4;
                          return ()}]
hIOI :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term
hIOI x0 x2 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x28 = Nil};
                                                                   let {x1 = Nil};
                                                                   (x29, x30) <- case x2 of
                                                                                 {Cons y29
                                                                                       y30 -> return (y29,
                                                                                                      y30);
                                                                                  _ -> mzero};
                                                                   guard (x29 == x0);
                                                                   guard (x30 == x28);
                                                                   return x1},
                                                               do {(x3,
                                                                    x4) <- _____gIIOO x0 x2 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                   let {x1 = Cons x3 x4};
                                                                   return x1}]
_____gIIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m (Term, Term)
_____gIIOO x0 x1 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x31,
                                                                          x4) <- case x1 of
                                                                                 {Cons y31
                                                                                       y4 -> return (y31,
                                                                                                     y4);
                                                                                  _ -> mzero};
                                                                         x3 <- fOI x4 gen______gOIOO_x2 gen_neqOI_x2;
                                                                         let {x2 = x31};
                                                                         guard (x0 == x2);
                                                                         return (x2, x3)},
                                                                     do {(x32, x4) <- case x1 of
                                                                                      {Cons y32
                                                                                            y4 -> return (y32,
                                                                                                          y4);
                                                                                       _ -> mzero};
                                                                         guard (x32 == x0);
                                                                         x2 <- neqIO x0 gen_neqIO_x2;
                                                                         x3 <- hIOI x2 x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                         return (x2, x3)}]
hOOI :: MonadPlus m => Term -> m Term -> m Term -> m (Term, Term)
hOOI x2 gen______gOIOO_x2 gen_neqOI_x2 = msum [do {let {x28 = Nil};
                                                   let {x1 = Nil};
                                                   (x29, x30) <- case x2 of
                                                                 {Cons y29 y30 -> return (y29, y30);
                                                                  _ -> mzero};
                                                   guard (x30 == x28);
                                                   let {x0 = x29};
                                                   return (x0, x1)},
                                               do {(x0,
                                                    x3,
                                                    x4) <- _____gOIOO x2 gen______gOIOO_x2 gen_neqOI_x2;
                                                   let {x1 = Cons x3 x4};
                                                   return (x0, x1)}]
hGIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
hGIOO x0 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x2 = Nil};
                                                                                                                                                                                                                                   let {x10 = Nil};
                                                                                                                                                                                                                                   let {x11 = Nil};
                                                                                                                                                                                                                                   (x1,
                                                                                                                                                                                                                                    x9) <- _______gIOOII x0 x10 x11 gen________gIOOII_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                   guard (x9 == x1);
                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                           x2)},
                                                                                                                                                                                                                               do {(x1,
                                                                                                                                                                                                                                    x3,
                                                                                                                                                                                                                                    x4) <- _gGIOOO x0 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                   let {x2 = Cons x3 x4};
                                                                                                                                                                                                                                   return (x1,
                                                                                                                                                                                                                                           x2)}]
_gGIOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
_gGIOOO x0 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x3,
                                                                                                                                                                                                                                      x2) <- fGIOO x0 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                     let {x1 = x2};
                                                                                                                                                                                                                                     return (x1,
                                                                                                                                                                                                                                             x2,
                                                                                                                                                                                                                                             x3)},
                                                                                                                                                                                                                                 do {(x1,
                                                                                                                                                                                                                                      x2,
                                                                                                                                                                                                                                      x3) <- neqHGIOOO x0 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
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
neqHGIIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
neqHGIIOO x0 x1 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x34 = Zero};
                                                                                                                                                                                                                     let {x35 = Zero};
                                                                                                                                                                                                                     guard (x1 == Zero);
                                                                                                                                                                                                                     (x3,
                                                                                                                                                                                                                      x33) <- __hGIOOII x0 x34 x35 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                     x4 <- case x33 of
                                                                                                                                                                                                                           {Succ y4 -> return y4;
                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                     let {x2 = Succ x4};
                                                                                                                                                                                                                     return (x2,
                                                                                                                                                                                                                             x3)},
                                                                                                                                                                                                                 do {let {x2 = Zero};
                                                                                                                                                                                                                     let {x36 = Zero};
                                                                                                                                                                                                                     x4 <- case x1 of
                                                                                                                                                                                                                           {Succ y4 -> return y4;
                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                     let {x37 = Succ x4};
                                                                                                                                                                                                                     let {x39 = x4};
                                                                                                                                                                                                                     let {x38 = Succ x39};
                                                                                                                                                                                                                     x3 <- __hGIOIII x0 x36 x37 x38 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                     return (x2,
                                                                                                                                                                                                                             x3)},
                                                                                                                                                                                                                 do {x44 <- case x1 of
                                                                                                                                                                                                                            {Succ y44 -> return y44;
                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                     (x3,
                                                                                                                                                                                                                      x40,
                                                                                                                                                                                                                      x41,
                                                                                                                                                                                                                      x42) <- __hGIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                     x5 <- case x40 of
                                                                                                                                                                                                                           {Succ y5 -> return y5;
                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                     let {x2 = Succ x5};
                                                                                                                                                                                                                     x6 <- case x41 of
                                                                                                                                                                                                                           {Succ y6 -> return y6;
                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                     guard (x44 == x6);
                                                                                                                                                                                                                     neqII x6 x5;
                                                                                                                                                                                                                     x43 <- case x42 of
                                                                                                                                                                                                                            {Succ y43 -> return y43;
                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                     guard (x43 == x6);
                                                                                                                                                                                                                     return (x2,
                                                                                                                                                                                                                             x3)}]
__hGIOIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term
__hGIOIII x0 x2 x3 x4 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Nil};
                                                                                                                                                            let {x46 = Nil};
                                                                                                                                                            let {x45 = Cons x2 x46};
                                                                                                                                                            let {x49 = Nil};
                                                                                                                                                            let {x48 = x2};
                                                                                                                                                            let {x47 = Cons x48 x49};
                                                                                                                                                            _______gIIIII x0 x3 x4 x45 x47;
                                                                                                                                                            return x1},
                                                                                                                                                        do {(x5,
                                                                                                                                                             x6) <- ___gGIIIIOO x0 x2 x3 x4 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                            let {x1 = Cons x5 x6};
                                                                                                                                                            return x1}]
___gGIIIIOO :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
___gGIIIIOO x0 x1 x2 x3 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x5,
                                                                                                                                                               x4) <- _fGIIIOO x0 x2 x3 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                              guard (x1 == x4);
                                                                                                                                                              return (x4,
                                                                                                                                                                      x5)},
                                                                                                                                                          do {let {x52 = x1};
                                                                                                                                                              x4 <- neqIO x1 gen_neqIO_x2;
                                                                                                                                                              (x5,
                                                                                                                                                               x6) <- hIOO x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                              let {x50 = Cons x1 x6};
                                                                                                                                                              let {x53 = x6};
                                                                                                                                                              let {x51 = Cons x52 x53};
                                                                                                                                                              _______gIIIII x0 x2 x3 x50 x51;
                                                                                                                                                              return (x4,
                                                                                                                                                                      x5)}]


__hGIOOII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
__hGIOOII x0 x3 x4 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Nil};
                                                                                                                     let {x46 = Nil};
                                                                                                                     let {x49 = Nil};
                                                                                                                     (x45,
                                                                                                                      x47) <- _______gIIIOO x0 x3 x4 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                     x2 <- case x45 of
                                                                                                                           {Cons y2
                                                                                                                                 y46 -> do {guard (x46 == y46);
                                                                                                                                            return y2};
                                                                                                                            _ -> mzero};
                                                                                                                     x48 <- case x47 of
                                                                                                                            {Cons y48
                                                                                                                                  y49 -> do {guard (x49 == y49);
                                                                                                                                             return y48};
                                                                                                                             _ -> mzero};
                                                                                                                     guard (x48 == x2);
                                                                                                                     return (x1,
                                                                                                                             x2)},
                                                                                                                 do {(x2,
                                                                                                                      x5,
                                                                                                                      x6) <- ___gGIOIIOO x0 x3 x4 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                     let {x1 = Cons x5 x6};
                                                                                                                     return (x1,
                                                                                                                             x2)}]


_______gIIIOO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
_______gIIIOO x0 x1 x2 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {guard (x1 == x0);
                                                                                                                         let {x63 = Zero};
                                                                                                                         let {x66 = Zero};
                                                                                                                         let {x70 = Zero};
                                                                                                                         let {x69 = Succ x70};
                                                                                                                         let {x68 = Succ x69};
                                                                                                                         let {x72 = Zero};
                                                                                                                         let {x74 = Zero};
                                                                                                                         let {x75 = Nil};
                                                                                                                         let {x73 = Cons x74 x75};
                                                                                                                         let {x71 = Cons x72 x73};
                                                                                                                         let {x67 = Cons x68 x71};
                                                                                                                         let {x65 = Cons x66 x67};
                                                                                                                         x64 <- case x0 of
                                                                                                                                {Succ y64 -> return y64;
                                                                                                                                 _ -> mzero};
                                                                                                                         guard (x64 == x63);
                                                                                                                         x4 <- fOI x65 gen______gOIOO_x2 gen_neqOI_x2;
                                                                                                                         x3 <- gen________gIIIOO_x3;
                                                                                                                         return (x3,
                                                                                                                                 x4)},
                                                                                                                     do {let {x77 = Zero};
                                                                                                                         let {x76 = Succ x77};
                                                                                                                         neqII x76 x1;
                                                                                                                         let {x78 = Zero};
                                                                                                                         let {x81 = Zero};
                                                                                                                         let {x85 = Zero};
                                                                                                                         let {x84 = Succ x85};
                                                                                                                         let {x83 = Succ x84};
                                                                                                                         let {x87 = Zero};
                                                                                                                         let {x89 = Zero};
                                                                                                                         let {x90 = Nil};
                                                                                                                         let {x88 = Cons x89 x90};
                                                                                                                         let {x86 = Cons x87 x88};
                                                                                                                         let {x82 = Cons x83 x86};
                                                                                                                         let {x80 = Cons x81 x82};
                                                                                                                         x79 <- case x0 of
                                                                                                                                {Succ y79 -> return y79;
                                                                                                                                 _ -> mzero};
                                                                                                                         guard (x79 == x78);
                                                                                                                         (x5,
                                                                                                                          x6) <- _____gIIOO x2 x80 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                         let {x3 = Cons x5 x6};
                                                                                                                         x4 <- gen________gIIIOO_x4;
                                                                                                                         return (x3,
                                                                                                                                 x4)}]
___gGIOIIOO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
___gGIOIIOO x0 x2 x3 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x5,
                                                                                                                        x4) <- _fGIIIOO x0 x2 x3 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                       let {x1 = x4};
                                                                                                                       return (x1,
                                                                                                                               x4,
                                                                                                                               x5)},
                                                                                                                   do {(x50,
                                                                                                                        x51) <- _______gIIIOO x0 x2 x3 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                       (x1,
                                                                                                                        x6) <- case x50 of
                                                                                                                               {Cons y1
                                                                                                                                     y6 -> return (y1,
                                                                                                                                                   y6);
                                                                                                                                _ -> mzero};
                                                                                                                       (x52,
                                                                                                                        x53) <- case x51 of
                                                                                                                                {Cons y52
                                                                                                                                      y53 -> return (y52,
                                                                                                                                                     y53);
                                                                                                                                 _ -> mzero};
                                                                                                                       guard (x52 == x1);
                                                                                                                       guard (x53 == x6);
                                                                                                                       x4 <- neqIO x1 gen_neqIO_x2;
                                                                                                                       x5 <- hIOI x4 x6 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                       return (x1,
                                                                                                                               x4,
                                                                                                                               x5)}]
_fGIIIOO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
_fGIIIOO x0 x1 x2 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x3 = Nil};
                                                                                                                    let {x55 = Nil};
                                                                                                                    let {x58 = Nil};
                                                                                                                    (x54,
                                                                                                                     x56) <- _______gIIIOO x0 x1 x2 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                    x4 <- case x54 of
                                                                                                                          {Cons y4
                                                                                                                                y55 -> do {guard (x55 == y55);
                                                                                                                                           return y4};
                                                                                                                           _ -> mzero};
                                                                                                                    x57 <- case x56 of
                                                                                                                           {Cons y57
                                                                                                                                 y58 -> do {guard (x58 == y58);
                                                                                                                                            return y57};
                                                                                                                            _ -> mzero};
                                                                                                                    guard (x57 == x4);
                                                                                                                    return (x3,
                                                                                                                            x4)},
                                                                                                                do {(x59,
                                                                                                                     x60) <- _______gIIIOO x0 x1 x2 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                    (x4,
                                                                                                                     x7) <- case x59 of
                                                                                                                            {Cons y4
                                                                                                                                  y7 -> return (y4,
                                                                                                                                                y7);
                                                                                                                             _ -> mzero};
                                                                                                                    (x61,
                                                                                                                     x62) <- case x60 of
                                                                                                                             {Cons y61
                                                                                                                                   y62 -> return (y61,
                                                                                                                                                  y62);
                                                                                                                              _ -> mzero};
                                                                                                                    guard (x61 == x4);
                                                                                                                    guard (x62 == x7);
                                                                                                                    (x5,
                                                                                                                     x6) <- hOOI x7 gen______gOIOO_x2 gen_neqOI_x2;
                                                                                                                    let {x3 = Cons x5 x6};
                                                                                                                    return (x3,
                                                                                                                            x4)}]
hIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
hIOO x0 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x28 = Nil};
                                                                                                    let {x1 = Nil};
                                                                                                    let {x29 = x0};
                                                                                                    let {x30 = x28};
                                                                                                    let {x2 = Cons x29 x30};
                                                                                                    return (x1,
                                                                                                            x2)},
                                                                                                do {(x2,
                                                                                                     x3,
                                                                                                     x4) <- _____gIOOO x0 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                    let {x1 = Cons x3 x4};
                                                                                                    return (x1,
                                                                                                            x2)}]
_____gIOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
_____gIOOO x0 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x31,
                                                                                                           x2) <- do {x2 <- gen______gIOOO_x2;
                                                                                                                      return (x2,
                                                                                                                              x2)};
                                                                                                          guard (x0 == x2);
                                                                                                          (x1,
                                                                                                           x4) <- do {x4 <- gen______gIOOO_x4;
                                                                                                                      let {x1 = Cons x31 x4};
                                                                                                                      return (x1,
                                                                                                                              x4)};
                                                                                                          x3 <- fOI x4 gen______gOIOO_x2 gen_neqOI_x2;
                                                                                                          return (x1,
                                                                                                                  x2,
                                                                                                                  x3)},
                                                                                                      do {let {x32 = x0};
                                                                                                          x2 <- neqIO x0 gen_neqIO_x2;
                                                                                                          (x3,
                                                                                                           x4) <- hIOO x2 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                          let {x1 = Cons x32 x4};
                                                                                                          return (x1,
                                                                                                                  x2,
                                                                                                                  x3)}]
neqHGIOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
neqHGIOOO x0 gen________gIIIOO_x3 gen________gIIIOO_x4 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Zero};
                                                                                                                                                                                                                                       let {x34 = Zero};
                                                                                                                                                                                                                                       let {x35 = Zero};
                                                                                                                                                                                                                                       (x3,
                                                                                                                                                                                                                                        x33) <- __hGIOOII x0 x34 x35 gen________gIIIOO_x3 gen________gIIIOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                       x4 <- case x33 of
                                                                                                                                                                                                                                             {Succ y4 -> return y4;
                                                                                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                                                                                       let {x2 = Succ x4};
                                                                                                                                                                                                                                       return (x1,
                                                                                                                                                                                                                                               x2,
                                                                                                                                                                                                                                               x3)},
                                                                                                                                                                                                                                   do {let {x2 = Zero};
                                                                                                                                                                                                                                       let {x36 = Zero};
                                                                                                                                                                                                                                       (x3,
                                                                                                                                                                                                                                        x37,
                                                                                                                                                                                                                                        x38) <- __hGIOIOO x0 x36 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                       x4 <- case x37 of
                                                                                                                                                                                                                                             {Succ y4 -> return y4;
                                                                                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                                                                                       let {x1 = Succ x4};
                                                                                                                                                                                                                                       x39 <- case x38 of
                                                                                                                                                                                                                                              {Succ y39 -> return y39;
                                                                                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                                                                                       guard (x39 == x4);
                                                                                                                                                                                                                                       return (x1,
                                                                                                                                                                                                                                               x2,
                                                                                                                                                                                                                                               x3)},
                                                                                                                                                                                                                                   do {(x3,
                                                                                                                                                                                                                                        x40,
                                                                                                                                                                                                                                        x41,
                                                                                                                                                                                                                                        x42) <- __hGIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                                                       x5 <- case x40 of
                                                                                                                                                                                                                                             {Succ y5 -> return y5;
                                                                                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                                                                                       let {x2 = Succ x5};
                                                                                                                                                                                                                                       x6 <- case x41 of
                                                                                                                                                                                                                                             {Succ y6 -> return y6;
                                                                                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                                                                                       neqII x6 x5;
                                                                                                                                                                                                                                       x43 <- case x42 of
                                                                                                                                                                                                                                              {Succ y43 -> return y43;
                                                                                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                                                                                       guard (x43 == x6);
                                                                                                                                                                                                                                       let {x44 = x6};
                                                                                                                                                                                                                                       let {x1 = Succ x44};
                                                                                                                                                                                                                                       return (x1,
                                                                                                                                                                                                                                               x2,
                                                                                                                                                                                                                                               x3)}]
__hGIOIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
__hGIOIOO x0 x2 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {let {x1 = Nil};
                                                                                                                                                                                                let {x46 = Nil};
                                                                                                                                                                                                let {x45 = Cons x2 x46};
                                                                                                                                                                                                let {x49 = Nil};
                                                                                                                                                                                                let {x48 = x2};
                                                                                                                                                                                                let {x47 = Cons x48 x49};
                                                                                                                                                                                                (x3,
                                                                                                                                                                                                 x4) <- _______gIOOII x0 x45 x47 gen________gIOOII_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                return (x1,
                                                                                                                                                                                                        x3,
                                                                                                                                                                                                        x4)},
                                                                                                                                                                                            do {(x3,
                                                                                                                                                                                                 x4,
                                                                                                                                                                                                 x5,
                                                                                                                                                                                                 x6) <- ___gGIIOOOO x0 x2 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                let {x1 = Cons x5 x6};
                                                                                                                                                                                                return (x1,
                                                                                                                                                                                                        x3,
                                                                                                                                                                                                        x4)}]
___gGIIOOOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
___gGIIOOOO x0 x1 gen________gIOOII_x2 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2 = msum [do {(x2,
                                                                                                                                                                                                   x3,
                                                                                                                                                                                                   x5,
                                                                                                                                                                                                   x4) <- _fGIOOOO x0 gen________gIOOOO_x2 gen________gIOOOO_x3 gen________gIOOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                  guard (x1 == x4);
                                                                                                                                                                                                  return (x2,
                                                                                                                                                                                                          x3,
                                                                                                                                                                                                          x4,
                                                                                                                                                                                                          x5)},
                                                                                                                                                                                              do {let {x52 = x1};
                                                                                                                                                                                                  x4 <- neqIO x1 gen_neqIO_x2;
                                                                                                                                                                                                  (x5,
                                                                                                                                                                                                   x6) <- hIOO x4 gen______gIOOO_x2 gen______gIOOO_x4 gen______gOIOO_x2 gen_neqIO_x2 gen_neqOI_x2;
                                                                                                                                                                                                  let {x50 = Cons x1 x6};
                                                                                                                                                                                                  let {x53 = x6};
                                                                                                                                                                                                  let {x51 = Cons x52 x53};
                                                                                                                                                                                                  (x2,
                                                                                                                                                                                                   x3) <- _______gIOOII x0 x50 x51 gen________gIOOII_x2 gen_neqIO_x2 gen_neqOI_x2;
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
                                         let {x91 = Zero};
                                         x3 <- case x1 of
                                               {Succ y3 -> return y3; _ -> mzero};
                                         let {x92 = Succ x3};
                                         hGIII x91 x92 x2 gen_neqOI_x2;
                                         return x0},
                                     do {let {x94 = Zero};
                                         guard (x1 == Zero);
                                         x93 <- hGOII x94 x2 gen_neqOI_x2;
                                         x3 <- case x93 of
                                               {Succ y3 -> return y3; _ -> mzero};
                                         let {x0 = Succ x3};
                                         return x0},
                                     do {x4 <- case x1 of
                                               {Succ y4 -> return y4; _ -> mzero};
                                         let {x96 = Succ x4};
                                         x95 <- hGOII x96 x2 gen_neqOI_x2;
                                         x5 <- case x95 of
                                               {Succ y5 -> return y5; _ -> mzero};
                                         neqII x5 x4;
                                         let {x97 = x5};
                                         let {x0 = Succ x97};
                                         return x0}]
hGIII :: MonadPlus m => Term -> Term -> Term -> m Term -> m ()
hGIII x0 x1 x2 gen_neqOI_x2 = msum [do {let {x10 = Nil};
                                        let {x11 = Nil};
                                        guard (x2 == Nil);
                                        let {x9 = x1};
                                        _______gIIIII x0 x1 x9 x10 x11;
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
                           _______gIIIII x0 x2 x12 x13 x14;
                           return ()},
                       do {(x3, x4) <- case x1 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           let {x15 = x2};
                           __hGIIIII x0 x4 x3 x2 x15;
                           return ()}]
__hGIIIII :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m ()
__hGIIIII x0 x1 x2 x3 x4 = msum [do {let {x46 = Nil};
                                     let {x45 = Cons x2 x46};
                                     let {x49 = Nil};
                                     guard (x1 == Nil);
                                     let {x48 = x2};
                                     let {x47 = Cons x48 x49};
                                     _______gIIIII x0 x3 x4 x45 x47;
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
                                          let {x52 = x1};
                                          x6 <- hIIO x4 x5;
                                          let {x50 = Cons x1 x6};
                                          let {x53 = x6};
                                          let {x51 = Cons x52 x53};
                                          _______gIIIII x0 x2 x3 x50 x51;
                                          return ()}]
_fGIIIII :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m ()
_fGIIIII x0 x1 x2 x3 x4 = msum [do {let {x55 = Nil};
                                    let {x54 = Cons x4 x55};
                                    let {x58 = Nil};
                                    guard (x3 == Nil);
                                    let {x57 = x4};
                                    let {x56 = Cons x57 x58};
                                    _______gIIIII x0 x1 x2 x54 x56;
                                    return ()},
                                do {(x5, x6) <- case x3 of
                                                {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                    let {x61 = x4};
                                    x7 <- hIIO x5 x6;
                                    let {x59 = Cons x4 x7};
                                    let {x62 = x7};
                                    let {x60 = Cons x61 x62};
                                    _______gIIIII x0 x1 x2 x59 x60;
                                    return ()}]
hIIO :: MonadPlus m => Term -> Term -> m Term
hIIO x0 x1 = msum [do {let {x28 = Nil};
                       guard (x1 == Nil);
                       let {x29 = x0};
                       let {x30 = x28};
                       let {x2 = Cons x29 x30};
                       return x2},
                   do {(x3, x4) <- case x1 of
                                   {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                       x2 <- _____gIOII x0 x3 x4;
                       return x2}]
_____gIOII :: MonadPlus m => Term -> Term -> Term -> m Term
_____gIOII x0 x2 x3 = msum [do {guard (x0 == x2);
                                let {x31 = x2};
                                x4 <- fIO x3;
                                let {x1 = Cons x31 x4};
                                return x1},
                            do {neqII x0 x2;
                                let {x32 = x0};
                                x4 <- hIIO x2 x3;
                                let {x1 = Cons x32 x4};
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
                                     x0 <- _______gOIIII x1 x9 x10 x11;
                                     return x0},
                                 do {(x3, x4) <- case x2 of
                                                 {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                                     x0 <- _gGOIII x1 x3 x4 gen_neqOI_x2;
                                     return x0}]
_______gOIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term
_______gOIIII x1 x2 x3 x4 = msum [do {let {x63 = Zero};
                                      let {x66 = Zero};
                                      let {x70 = Zero};
                                      let {x69 = Succ x70};
                                      let {x68 = Succ x69};
                                      let {x72 = Zero};
                                      let {x74 = Zero};
                                      let {x75 = Nil};
                                      let {x73 = Cons x74 x75};
                                      let {x71 = Cons x72 x73};
                                      let {x67 = Cons x68 x71};
                                      let {x65 = Cons x66 x67};
                                      fII x4 x65;
                                      let {x64 = x63};
                                      let {x0 = Succ x64};
                                      guard (x1 == x0);
                                      return x0},
                                  do {let {x77 = Zero};
                                      let {x76 = Succ x77};
                                      neqII x76 x1;
                                      let {x78 = Zero};
                                      let {x81 = Zero};
                                      let {x85 = Zero};
                                      let {x84 = Succ x85};
                                      let {x83 = Succ x84};
                                      let {x87 = Zero};
                                      let {x89 = Zero};
                                      let {x90 = Nil};
                                      let {x88 = Cons x89 x90};
                                      let {x86 = Cons x87 x88};
                                      let {x82 = Cons x83 x86};
                                      let {x80 = Cons x81 x82};
                                      (x5, x6) <- case x3 of
                                                  {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                      _____gIIII x2 x80 x5 x6;
                                      let {x79 = x78};
                                      let {x0 = Succ x79};
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
                        x0 <- _______gOIIII x2 x12 x13 x14;
                        return x0},
                    do {(x3, x4) <- case x1 of
                                    {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                        let {x15 = x2};
                        x0 <- __hGOIIII x4 x3 x2 x15;
                        return x0}]
__hGOIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term
__hGOIIII x1 x2 x3 x4 = msum [do {let {x46 = Nil};
                                  let {x45 = Cons x2 x46};
                                  let {x49 = Nil};
                                  guard (x1 == Nil);
                                  let {x48 = x2};
                                  let {x47 = Cons x48 x49};
                                  x0 <- _______gOIIII x3 x4 x45 x47;
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
                                       let {x52 = x1};
                                       x6 <- hIIO x4 x5;
                                       let {x50 = Cons x1 x6};
                                       let {x53 = x6};
                                       let {x51 = Cons x52 x53};
                                       x0 <- _______gOIIII x2 x3 x50 x51;
                                       return x0}]
_fGOIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term
_fGOIIII x1 x2 x3 x4 = msum [do {let {x55 = Nil};
                                 let {x54 = Cons x4 x55};
                                 let {x58 = Nil};
                                 guard (x3 == Nil);
                                 let {x57 = x4};
                                 let {x56 = Cons x57 x58};
                                 x0 <- _______gOIIII x1 x2 x54 x56;
                                 return x0},
                             do {(x5, x6) <- case x3 of
                                             {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                 let {x61 = x4};
                                 x7 <- hIIO x5 x6;
                                 let {x59 = Cons x4 x7};
                                 let {x62 = x7};
                                 let {x60 = Cons x61 x62};
                                 x0 <- _______gOIIII x1 x2 x59 x60;
                                 return x0}]
neqHGIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m ()
neqHGIIII x0 x1 x2 x3 gen_neqOI_x2 = msum [do {let {x34 = Zero};
                                               let {x35 = Zero};
                                               x4 <- case x2 of
                                                     {Succ y4 -> return y4; _ -> mzero};
                                               let {x33 = Succ x4};
                                               __hGIIIII x0 x3 x33 x34 x35;
                                               guard (x1 == Zero);
                                               return ()},
                                           do {let {x36 = Zero};
                                               guard (x2 == Zero);
                                               x4 <- case x1 of
                                                     {Succ y4 -> return y4; _ -> mzero};
                                               let {x37 = Succ x4};
                                               let {x39 = x4};
                                               let {x38 = Succ x39};
                                               __hGIIIII x0 x3 x36 x37 x38;
                                               return ()},
                                           do {x5 <- case x2 of
                                                     {Succ y5 -> return y5; _ -> mzero};
                                               let {x40 = Succ x5};
                                               x44 <- case x1 of
                                                      {Succ y44 -> return y44; _ -> mzero};
                                               x6 <- neqOI x5 gen_neqOI_x2;
                                               guard (x44 == x6);
                                               let {x41 = Succ x6};
                                               let {x43 = x6};
                                               let {x42 = Succ x43};
                                               __hGIIIII x0 x3 x40 x41 x42;
                                               return ()}]
neqHGOIII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term
neqHGOIII x1 x2 x3 gen_neqOI_x2 = msum [do {let {x34 = Zero};
                                            let {x35 = Zero};
                                            x4 <- case x2 of
                                                  {Succ y4 -> return y4; _ -> mzero};
                                            let {x33 = Succ x4};
                                            guard (x1 == Zero);
                                            x0 <- __hGOIIII x3 x33 x34 x35;
                                            return x0},
                                        do {let {x36 = Zero};
                                            guard (x2 == Zero);
                                            x4 <- case x1 of
                                                  {Succ y4 -> return y4; _ -> mzero};
                                            let {x37 = Succ x4};
                                            let {x39 = x4};
                                            let {x38 = Succ x39};
                                            x0 <- __hGOIIII x3 x36 x37 x38;
                                            return x0},
                                        do {x5 <- case x2 of
                                                  {Succ y5 -> return y5; _ -> mzero};
                                            let {x40 = Succ x5};
                                            x44 <- case x1 of
                                                   {Succ y44 -> return y44; _ -> mzero};
                                            x6 <- neqOI x5 gen_neqOI_x2;
                                            guard (x44 == x6);
                                            let {x41 = Succ x6};
                                            let {x43 = x6};
                                            let {x42 = Succ x43};
                                            x0 <- __hGOIIII x3 x40 x41 x42;
                                            return x0}]