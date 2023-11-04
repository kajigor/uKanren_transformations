module Regexp1_cpd_ans where

import Stream
import Control.Monad
import Term

generateIIIII :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m ()
generateIIIII x0 x1 x2 x3 x4 = msum [do {guard (x4 == Nil);
                                         return ()},
                                     do {x5 <- generateGenerateIIIIIO x0 x1 x2 x3 x4;
                                         generateIIIII x0 x1 x2 x3 x5;
                                         return ()}]
generateIIIIO :: MonadPlus m => Term -> Term -> Term -> Term -> m Term
generateIIIIO x0 x1 x2 x3 = msum [do {let {x4 = Nil}; return x4},
                                  do {x5 <- generateIIIIO x0 x1 x2 x3;
                                      x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                      return x4}]
generateIIIOI :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term
generateIIIOI x0 x1 x2 x4 gen__generateIOOI_x1 gen_generateIIIOI_x3 = msum [do {guard (x4 == Nil);
                                                                                x3 <- gen_generateIIIOI_x3;
                                                                                return x3},
                                                                            do {(x3,
                                                                                 x5) <- generateGenerateIIIOIO x0 x1 x2 x4 gen__generateIOOI_x1;
                                                                                generateIIIII x0 x1 x2 x3 x5;
                                                                                return x3}]
generateIIIOO :: MonadPlus m => Term -> Term -> Term -> m Term -> m (Term, Term)
generateIIIOO x0 x1 x2 gen_generateIIIOO_x3 = msum [do {let {x4 = Nil};
                                                        x3 <- gen_generateIIIOO_x3;
                                                        return (x3, x4)},
                                                    do {(x3,
                                                         x5) <- generateIIIOO x0 x1 x2 gen_generateIIIOO_x3;
                                                        x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                        return (x3, x4)}]
generateIIOII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term
generateIIOII x0 x1 x3 x4 gen__generateOIOI_x0 gen_generateIIOII_x2 = msum [do {guard (x4 == Nil);
                                                                                x2 <- gen_generateIIOII_x2;
                                                                                return x2},
                                                                            do {(x2,
                                                                                 x5) <- generateGenerateIIOIIO x0 x1 x3 x4 gen__generateOIOI_x0;
                                                                                generateIIIII x0 x1 x2 x3 x5;
                                                                                return x2}]
generateIIOIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m (Term, Term)
generateIIOIO x0 x1 x3 gen_generateIIOIO_x2 = msum [do {let {x4 = Nil};
                                                        x2 <- gen_generateIIOIO_x2;
                                                        return (x2, x4)},
                                                    do {(x2,
                                                         x5) <- generateIIOIO x0 x1 x3 gen_generateIIOIO_x2;
                                                        x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                        return (x2, x4)}]
generateIIOOI :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateIIOOI x0 x1 x4 gen__generateOOOI_x0 gen__generateOOOI_x1 gen_generateIIOOI_x2 gen_generateIIOOI_x3 = msum [do {guard (x4 == Nil);
                                                                                                                       x2 <- gen_generateIIOOI_x2;
                                                                                                                       x3 <- gen_generateIIOOI_x3;
                                                                                                                       return (x2,
                                                                                                                               x3)},
                                                                                                                   do {(x2,
                                                                                                                        x3,
                                                                                                                        x5) <- generateGenerateIIOOIO x0 x1 x4 gen__generateOOOI_x0 gen__generateOOOI_x1;
                                                                                                                       generateIIIII x0 x1 x2 x3 x5;
                                                                                                                       return (x2,
                                                                                                                               x3)}]
generateIIOOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3 = msum [do {let {x4 = Nil};
                                                                          x2 <- gen_generateIIOOO_x2;
                                                                          x3 <- gen_generateIIOOO_x3;
                                                                          return (x2, x3, x4)},
                                                                      do {(x2,
                                                                           x3,
                                                                           x5) <- generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3;
                                                                          x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                          return (x2, x3, x4)}]
generateIOIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term
generateIOIII x0 x2 x3 x4 gen_generateGenerateIOIIIO_x1 gen_generateIOIII_x1 = msum [do {guard (x4 == Nil);
                                                                                         x1 <- gen_generateIOIII_x1;
                                                                                         return x1},
                                                                                     do {(x1,
                                                                                          x5) <- generateGenerateIOIIIO x0 x2 x3 x4 gen_generateGenerateIOIIIO_x1;
                                                                                         generateIIIII x0 x1 x2 x3 x5;
                                                                                         return x1}]
generateIOIIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m (Term, Term)
generateIOIIO x0 x2 x3 gen_generateIOIIO_x1 = msum [do {let {x4 = Nil};
                                                        x1 <- gen_generateIOIIO_x1;
                                                        return (x1, x4)},
                                                    do {(x1,
                                                         x5) <- generateIOIIO x0 x2 x3 gen_generateIOIIO_x1;
                                                        x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                        return (x1, x4)}]
generateIOIOI :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateIOIOI x0 x2 x4 gen__generateIOOI_x1 gen_generateGenerateIOIOIO_x1 gen_generateIOIOI_x1 gen_generateIOIOI_x3 = msum [do {guard (x4 == Nil);
                                                                                                                                x1 <- gen_generateIOIOI_x1;
                                                                                                                                x3 <- gen_generateIOIOI_x3;
                                                                                                                                return (x1,
                                                                                                                                        x3)},
                                                                                                                            do {(x1,
                                                                                                                                 x3,
                                                                                                                                 x5) <- generateGenerateIOIOIO x0 x2 x4 gen__generateIOOI_x1 gen_generateGenerateIOIOIO_x1;
                                                                                                                                generateIIIII x0 x1 x2 x3 x5;
                                                                                                                                return (x1,
                                                                                                                                        x3)}]
generateIOIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3 = msum [do {let {x4 = Nil};
                                                                          x1 <- gen_generateIOIOO_x1;
                                                                          x3 <- gen_generateIOIOO_x3;
                                                                          return (x1, x3, x4)},
                                                                      do {(x1,
                                                                           x3,
                                                                           x5) <- generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3;
                                                                          x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                          return (x1, x3, x4)}]
generateIOOII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateIOOII x0 x3 x4 gen__generateOIOI_x0 gen_generateGenerateIOOIIO_x1 gen_generateIOOII_x1 gen_generateIOOII_x2 = msum [do {guard (x4 == Nil);
                                                                                                                                x1 <- gen_generateIOOII_x1;
                                                                                                                                x2 <- gen_generateIOOII_x2;
                                                                                                                                return (x1,
                                                                                                                                        x2)},
                                                                                                                            do {(x1,
                                                                                                                                 x2,
                                                                                                                                 x5) <- generateGenerateIOOIIO x0 x3 x4 gen__generateOIOI_x0 gen_generateGenerateIOOIIO_x1;
                                                                                                                                generateIIIII x0 x1 x2 x3 x5;
                                                                                                                                return (x1,
                                                                                                                                        x2)}]
generateIOOIO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2 = msum [do {let {x4 = Nil};
                                                                          x1 <- gen_generateIOOIO_x1;
                                                                          x2 <- gen_generateIOOIO_x2;
                                                                          return (x1, x2, x4)},
                                                                      do {(x1,
                                                                           x2,
                                                                           x5) <- generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2;
                                                                          x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                          return (x1, x2, x4)}]
generateIOOOI :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
generateIOOOI x0 x4 gen__generateOOOI_x0 gen__generateOOOI_x1 gen_generateGenerateIOOOIO_x1 gen_generateIOOOI_x1 gen_generateIOOOI_x2 gen_generateIOOOI_x3 = msum [do {guard (x4 == Nil);
                                                                                                                                                                       x1 <- gen_generateIOOOI_x1;
                                                                                                                                                                       x2 <- gen_generateIOOOI_x2;
                                                                                                                                                                       x3 <- gen_generateIOOOI_x3;
                                                                                                                                                                       return (x1,
                                                                                                                                                                               x2,
                                                                                                                                                                               x3)},
                                                                                                                                                                   do {(x1,
                                                                                                                                                                        x2,
                                                                                                                                                                        x3,
                                                                                                                                                                        x5) <- generateGenerateIOOOIO x0 x4 gen__generateOOOI_x0 gen__generateOOOI_x1 gen_generateGenerateIOOOIO_x1;
                                                                                                                                                                       generateIIIII x0 x1 x2 x3 x5;
                                                                                                                                                                       return (x1,
                                                                                                                                                                               x2,
                                                                                                                                                                               x3)}]
generateIOOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3 = msum [do {let {x4 = Nil};
                                                                                            x1 <- gen_generateIOOOO_x1;
                                                                                            x2 <- gen_generateIOOOO_x2;
                                                                                            x3 <- gen_generateIOOOO_x3;
                                                                                            return (x1,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {(x1,
                                                                                             x2,
                                                                                             x3,
                                                                                             x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                            x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                                            return (x1,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)}]
generateOIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term
generateOIIII x1 x2 x3 x4 gen_generateGenerateOIIIIO_x0 gen_generateOIIII_x0 = msum [do {guard (x4 == Nil);
                                                                                         x0 <- gen_generateOIIII_x0;
                                                                                         return x0},
                                                                                     do {(x0,
                                                                                          x5) <- generateGenerateOIIIIO x1 x2 x3 x4 gen_generateGenerateOIIIIO_x0;
                                                                                         generateIIIII x0 x1 x2 x3 x5;
                                                                                         return x0}]
generateOIIIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m (Term, Term)
generateOIIIO x1 x2 x3 gen_generateOIIIO_x0 = msum [do {let {x4 = Nil};
                                                        x0 <- gen_generateOIIIO_x0;
                                                        return (x0, x4)},
                                                    do {(x0,
                                                         x5) <- generateOIIIO x1 x2 x3 gen_generateOIIIO_x0;
                                                        x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                        return (x0, x4)}]
generateOIIOI :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateOIIOI x1 x2 x4 gen__generateIOOI_x1 gen_generateGenerateOIIOIO_x0 gen_generateOIIOI_x0 gen_generateOIIOI_x3 = msum [do {guard (x4 == Nil);
                                                                                                                                x0 <- gen_generateOIIOI_x0;
                                                                                                                                x3 <- gen_generateOIIOI_x3;
                                                                                                                                return (x0,
                                                                                                                                        x3)},
                                                                                                                            do {(x0,
                                                                                                                                 x3,
                                                                                                                                 x5) <- generateGenerateOIIOIO x1 x2 x4 gen__generateIOOI_x1 gen_generateGenerateOIIOIO_x0;
                                                                                                                                generateIIIII x0 x1 x2 x3 x5;
                                                                                                                                return (x0,
                                                                                                                                        x3)}]
generateOIIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateOIIOO x1 x2 gen_generateOIIOO_x0 gen_generateOIIOO_x3 = msum [do {let {x4 = Nil};
                                                                          x0 <- gen_generateOIIOO_x0;
                                                                          x3 <- gen_generateOIIOO_x3;
                                                                          return (x0, x3, x4)},
                                                                      do {(x0,
                                                                           x3,
                                                                           x5) <- generateOIIOO x1 x2 gen_generateOIIOO_x0 gen_generateOIIOO_x3;
                                                                          x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                          return (x0, x3, x4)}]
generateOIOII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateOIOII x1 x3 x4 gen__generateOIOI_x0 gen_generateGenerateOIOIIO_x0 gen_generateOIOII_x0 gen_generateOIOII_x2 = msum [do {guard (x4 == Nil);
                                                                                                                                x0 <- gen_generateOIOII_x0;
                                                                                                                                x2 <- gen_generateOIOII_x2;
                                                                                                                                return (x0,
                                                                                                                                        x2)},
                                                                                                                            do {(x0,
                                                                                                                                 x2,
                                                                                                                                 x5) <- generateGenerateOIOIIO x1 x3 x4 gen__generateOIOI_x0 gen_generateGenerateOIOIIO_x0;
                                                                                                                                generateIIIII x0 x1 x2 x3 x5;
                                                                                                                                return (x0,
                                                                                                                                        x2)}]
generateOIOIO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateOIOIO x1 x3 gen_generateOIOIO_x0 gen_generateOIOIO_x2 = msum [do {let {x4 = Nil};
                                                                          x0 <- gen_generateOIOIO_x0;
                                                                          x2 <- gen_generateOIOIO_x2;
                                                                          return (x0, x2, x4)},
                                                                      do {(x0,
                                                                           x2,
                                                                           x5) <- generateOIOIO x1 x3 gen_generateOIOIO_x0 gen_generateOIOIO_x2;
                                                                          x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                          return (x0, x2, x4)}]
generateOIOOI :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
generateOIOOI x1 x4 gen__generateOOOI_x0 gen__generateOOOI_x1 gen_generateGenerateOIOOIO_x0 gen_generateOIOOI_x0 gen_generateOIOOI_x2 gen_generateOIOOI_x3 = msum [do {guard (x4 == Nil);
                                                                                                                                                                       x0 <- gen_generateOIOOI_x0;
                                                                                                                                                                       x2 <- gen_generateOIOOI_x2;
                                                                                                                                                                       x3 <- gen_generateOIOOI_x3;
                                                                                                                                                                       return (x0,
                                                                                                                                                                               x2,
                                                                                                                                                                               x3)},
                                                                                                                                                                   do {(x0,
                                                                                                                                                                        x2,
                                                                                                                                                                        x3,
                                                                                                                                                                        x5) <- generateGenerateOIOOIO x1 x4 gen__generateOOOI_x0 gen__generateOOOI_x1 gen_generateGenerateOIOOIO_x0;
                                                                                                                                                                       generateIIIII x0 x1 x2 x3 x5;
                                                                                                                                                                       return (x0,
                                                                                                                                                                               x2,
                                                                                                                                                                               x3)}]
generateOIOOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3 = msum [do {let {x4 = Nil};
                                                                                            x0 <- gen_generateOIOOO_x0;
                                                                                            x2 <- gen_generateOIOOO_x2;
                                                                                            x3 <- gen_generateOIOOO_x3;
                                                                                            return (x0,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {(x0,
                                                                                             x2,
                                                                                             x3,
                                                                                             x5) <- generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3;
                                                                                            x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                                            return (x0,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)}]
generateOOIII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateOOIII x2 x3 x4 gen_generateGenerateOOIIIO_x0 gen_generateGenerateOOIIIO_x1 gen_generateOOIII_x0 gen_generateOOIII_x1 = msum [do {guard (x4 == Nil);
                                                                                                                                         x0 <- gen_generateOOIII_x0;
                                                                                                                                         x1 <- gen_generateOOIII_x1;
                                                                                                                                         return (x0,
                                                                                                                                                 x1)},
                                                                                                                                     do {(x0,
                                                                                                                                          x1,
                                                                                                                                          x5) <- generateGenerateOOIIIO x2 x3 x4 gen_generateGenerateOOIIIO_x0 gen_generateGenerateOOIIIO_x1;
                                                                                                                                         generateIIIII x0 x1 x2 x3 x5;
                                                                                                                                         return (x0,
                                                                                                                                                 x1)}]
generateOOIIO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateOOIIO x2 x3 gen_generateOOIIO_x0 gen_generateOOIIO_x1 = msum [do {let {x4 = Nil};
                                                                          x0 <- gen_generateOOIIO_x0;
                                                                          x1 <- gen_generateOOIIO_x1;
                                                                          return (x0, x1, x4)},
                                                                      do {(x0,
                                                                           x1,
                                                                           x5) <- generateOOIIO x2 x3 gen_generateOOIIO_x0 gen_generateOOIIO_x1;
                                                                          x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                          return (x0, x1, x4)}]
generateOOIOI :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
generateOOIOI x2 x4 gen__generateIOOI_x1 gen_generateGenerateOOIOIO_x0 gen_generateGenerateOOIOIO_x1 gen_generateOOIOI_x0 gen_generateOOIOI_x1 gen_generateOOIOI_x3 = msum [do {guard (x4 == Nil);
                                                                                                                                                                                x0 <- gen_generateOOIOI_x0;
                                                                                                                                                                                x1 <- gen_generateOOIOI_x1;
                                                                                                                                                                                x3 <- gen_generateOOIOI_x3;
                                                                                                                                                                                return (x0,
                                                                                                                                                                                        x1,
                                                                                                                                                                                        x3)},
                                                                                                                                                                            do {(x0,
                                                                                                                                                                                 x1,
                                                                                                                                                                                 x3,
                                                                                                                                                                                 x5) <- generateGenerateOOIOIO x2 x4 gen__generateIOOI_x1 gen_generateGenerateOOIOIO_x0 gen_generateGenerateOOIOIO_x1;
                                                                                                                                                                                generateIIIII x0 x1 x2 x3 x5;
                                                                                                                                                                                return (x0,
                                                                                                                                                                                        x1,
                                                                                                                                                                                        x3)}]
generateOOIOO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
generateOOIOO x2 gen_generateOOIOO_x0 gen_generateOOIOO_x1 gen_generateOOIOO_x3 = msum [do {let {x4 = Nil};
                                                                                            x0 <- gen_generateOOIOO_x0;
                                                                                            x1 <- gen_generateOOIOO_x1;
                                                                                            x3 <- gen_generateOOIOO_x3;
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {(x0,
                                                                                             x1,
                                                                                             x3,
                                                                                             x5) <- generateOOIOO x2 gen_generateOOIOO_x0 gen_generateOOIOO_x1 gen_generateOOIOO_x3;
                                                                                            x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x3,
                                                                                                    x4)}]
generateOOOII :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
generateOOOII x3 x4 gen__generateOIOI_x0 gen_generateGenerateOOOIIO_x0 gen_generateGenerateOOOIIO_x1 gen_generateOOOII_x0 gen_generateOOOII_x1 gen_generateOOOII_x2 = msum [do {guard (x4 == Nil);
                                                                                                                                                                                x0 <- gen_generateOOOII_x0;
                                                                                                                                                                                x1 <- gen_generateOOOII_x1;
                                                                                                                                                                                x2 <- gen_generateOOOII_x2;
                                                                                                                                                                                return (x0,
                                                                                                                                                                                        x1,
                                                                                                                                                                                        x2)},
                                                                                                                                                                            do {(x0,
                                                                                                                                                                                 x1,
                                                                                                                                                                                 x2,
                                                                                                                                                                                 x5) <- generateGenerateOOOIIO x3 x4 gen__generateOIOI_x0 gen_generateGenerateOOOIIO_x0 gen_generateGenerateOOOIIO_x1;
                                                                                                                                                                                generateIIIII x0 x1 x2 x3 x5;
                                                                                                                                                                                return (x0,
                                                                                                                                                                                        x1,
                                                                                                                                                                                        x2)}]
generateOOOIO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
generateOOOIO x3 gen_generateOOOIO_x0 gen_generateOOOIO_x1 gen_generateOOOIO_x2 = msum [do {let {x4 = Nil};
                                                                                            x0 <- gen_generateOOOIO_x0;
                                                                                            x1 <- gen_generateOOOIO_x1;
                                                                                            x2 <- gen_generateOOOIO_x2;
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x2,
                                                                                                    x4)},
                                                                                        do {(x0,
                                                                                             x1,
                                                                                             x2,
                                                                                             x5) <- generateOOOIO x3 gen_generateOOOIO_x0 gen_generateOOOIO_x1 gen_generateOOOIO_x2;
                                                                                            x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x2,
                                                                                                    x4)}]
generateOOOOI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
generateOOOOI x4 gen__generateOOOI_x0 gen__generateOOOI_x1 gen_generateGenerateOOOOIO_x0 gen_generateGenerateOOOOIO_x1 gen_generateOOOOI_x0 gen_generateOOOOI_x1 gen_generateOOOOI_x2 gen_generateOOOOI_x3 = msum [do {guard (x4 == Nil);
                                                                                                                                                                                                                       x0 <- gen_generateOOOOI_x0;
                                                                                                                                                                                                                       x1 <- gen_generateOOOOI_x1;
                                                                                                                                                                                                                       x2 <- gen_generateOOOOI_x2;
                                                                                                                                                                                                                       x3 <- gen_generateOOOOI_x3;
                                                                                                                                                                                                                       return (x0,
                                                                                                                                                                                                                               x1,
                                                                                                                                                                                                                               x2,
                                                                                                                                                                                                                               x3)},
                                                                                                                                                                                                                   do {(x0,
                                                                                                                                                                                                                        x1,
                                                                                                                                                                                                                        x2,
                                                                                                                                                                                                                        x3,
                                                                                                                                                                                                                        x5) <- generateGenerateOOOOIO x4 gen__generateOOOI_x0 gen__generateOOOI_x1 gen_generateGenerateOOOOIO_x0 gen_generateGenerateOOOOIO_x1;
                                                                                                                                                                                                                       generateIIIII x0 x1 x2 x3 x5;
                                                                                                                                                                                                                       return (x0,
                                                                                                                                                                                                                               x1,
                                                                                                                                                                                                                               x2,
                                                                                                                                                                                                                               x3)}]
generateOOOOO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term, Term)
generateOOOOO gen_generateOOOOO_x0 gen_generateOOOOO_x1 gen_generateOOOOO_x2 gen_generateOOOOO_x3 = msum [do {let {x4 = Nil};
                                                                                                              x0 <- gen_generateOOOOO_x0;
                                                                                                              x1 <- gen_generateOOOOO_x1;
                                                                                                              x2 <- gen_generateOOOOO_x2;
                                                                                                              x3 <- gen_generateOOOOO_x3;
                                                                                                              return (x0,
                                                                                                                      x1,
                                                                                                                      x2,
                                                                                                                      x3,
                                                                                                                      x4)},
                                                                                                          do {(x0,
                                                                                                               x1,
                                                                                                               x2,
                                                                                                               x3,
                                                                                                               x5) <- generateOOOOO gen_generateOOOOO_x0 gen_generateOOOOO_x1 gen_generateOOOOO_x2 gen_generateOOOOO_x3;
                                                                                                              x4 <- generateGenerateIIIIOI x0 x1 x2 x3 x5;
                                                                                                              return (x0,
                                                                                                                      x1,
                                                                                                                      x2,
                                                                                                                      x3,
                                                                                                                      x4)}]
generateGenerateIIIIIO :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m Term
generateGenerateIIIIIO x0 x1 x2 x3 x4 = msum [do {(x7,
                                                   x6) <- case x4 of
                                                          {Cons y7 y6 -> return (y7, y6);
                                                           _ -> mzero};
                                                  guard (x7 == x0);
                                                  x5 <- _generateIIOI x2 x3 x6;
                                                  return x5},
                                              do {(x8, x6) <- case x4 of
                                                              {Cons y8 y6 -> return (y8, y6);
                                                               _ -> mzero};
                                                  guard (x8 == x1);
                                                  x5 <- _generateIIOI x2 x3 x6;
                                                  return x5}]
_generateIIOI :: MonadPlus m => Term -> Term -> Term -> m Term
_generateIIOI x0 x1 x3 = msum [do {(x9, x10) <- case x3 of
                                                {Cons y9 y10 -> return (y9, y10); _ -> mzero};
                                   guard (x9 == x0);
                                   let {x2 = x10};
                                   return x2},
                               do {(x11, x12) <- case x3 of
                                                 {Cons y11 y12 -> return (y11, y12); _ -> mzero};
                                   guard (x11 == x1);
                                   let {x2 = x12};
                                   return x2}]
generateGenerateIIIIOI :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m Term
generateGenerateIIIIOI x0 x1 x2 x3 x5 = msum [do {let {x7 = x0};
                                                  x6 <- _generateIIIO x2 x3 x5;
                                                  let {x4 = Cons x7 x6};
                                                  return x4},
                                              do {let {x8 = x1};
                                                  x6 <- _generateIIIO x2 x3 x5;
                                                  let {x4 = Cons x8 x6};
                                                  return x4}]
_generateIIIO :: MonadPlus m => Term -> Term -> Term -> m Term
_generateIIIO x0 x1 x2 = msum [do {let {x9 = x0};
                                   let {x10 = x2};
                                   let {x3 = Cons x9 x10};
                                   return x3},
                               do {let {x11 = x1};
                                   let {x12 = x2};
                                   let {x3 = Cons x11 x12};
                                   return x3}]
generateGenerateIIIOIO :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m (Term, Term)
generateGenerateIIIOIO x0 x1 x2 x4 gen__generateIOOI_x1 = msum [do {(x7,
                                                                     x6) <- case x4 of
                                                                            {Cons y7
                                                                                  y6 -> return (y7,
                                                                                                y6);
                                                                             _ -> mzero};
                                                                    guard (x7 == x0);
                                                                    (x3,
                                                                     x5) <- _generateIOOI x2 x6 gen__generateIOOI_x1;
                                                                    return (x3, x5)},
                                                                do {(x8, x6) <- case x4 of
                                                                                {Cons y8
                                                                                      y6 -> return (y8,
                                                                                                    y6);
                                                                                 _ -> mzero};
                                                                    guard (x8 == x1);
                                                                    (x3,
                                                                     x5) <- _generateIOOI x2 x6 gen__generateIOOI_x1;
                                                                    return (x3, x5)}]
_generateIOOI :: MonadPlus m => Term -> Term -> m Term -> m (Term, Term)
_generateIOOI x0 x3 gen__generateIOOI_x1 = msum [do {(x9,
                                                      x10) <- case x3 of
                                                              {Cons y9 y10 -> return (y9, y10);
                                                               _ -> mzero};
                                                     guard (x9 == x0);
                                                     let {x2 = x10};
                                                     x1 <- gen__generateIOOI_x1;
                                                     return (x1, x2)},
                                                 do {(x11, x12) <- case x3 of
                                                                   {Cons y11 y12 -> return (y11,
                                                                                            y12);
                                                                    _ -> mzero};
                                                     let {x1 = x11};
                                                     let {x2 = x12};
                                                     return (x1, x2)}]
generateGenerateIIOIIO :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m (Term, Term)
generateGenerateIIOIIO x0 x1 x3 x4 gen__generateOIOI_x0 = msum [do {(x7,
                                                                     x6) <- case x4 of
                                                                            {Cons y7
                                                                                  y6 -> return (y7,
                                                                                                y6);
                                                                             _ -> mzero};
                                                                    guard (x7 == x0);
                                                                    (x2,
                                                                     x5) <- _generateOIOI x3 x6 gen__generateOIOI_x0;
                                                                    return (x2, x5)},
                                                                do {(x8, x6) <- case x4 of
                                                                                {Cons y8
                                                                                      y6 -> return (y8,
                                                                                                    y6);
                                                                                 _ -> mzero};
                                                                    guard (x8 == x1);
                                                                    (x2,
                                                                     x5) <- _generateOIOI x3 x6 gen__generateOIOI_x0;
                                                                    return (x2, x5)}]
_generateOIOI :: MonadPlus m => Term -> Term -> m Term -> m (Term, Term)
_generateOIOI x1 x3 gen__generateOIOI_x0 = msum [do {(x9,
                                                      x10) <- case x3 of
                                                              {Cons y9 y10 -> return (y9, y10);
                                                               _ -> mzero};
                                                     let {x0 = x9};
                                                     let {x2 = x10};
                                                     return (x0, x2)},
                                                 do {(x11, x12) <- case x3 of
                                                                   {Cons y11 y12 -> return (y11,
                                                                                            y12);
                                                                    _ -> mzero};
                                                     guard (x11 == x1);
                                                     let {x2 = x12};
                                                     x0 <- gen__generateOIOI_x0;
                                                     return (x0, x2)}]
generateGenerateIIOOIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateGenerateIIOOIO x0 x1 x4 gen__generateOOOI_x0 gen__generateOOOI_x1 = msum [do {(x7,
                                                                                       x6) <- case x4 of
                                                                                              {Cons y7
                                                                                                    y6 -> return (y7,
                                                                                                                  y6);
                                                                                               _ -> mzero};
                                                                                      guard (x7 == x0);
                                                                                      (x2,
                                                                                       x3,
                                                                                       x5) <- _generateOOOI x6 gen__generateOOOI_x0 gen__generateOOOI_x1;
                                                                                      return (x2,
                                                                                              x3,
                                                                                              x5)},
                                                                                  do {(x8,
                                                                                       x6) <- case x4 of
                                                                                              {Cons y8
                                                                                                    y6 -> return (y8,
                                                                                                                  y6);
                                                                                               _ -> mzero};
                                                                                      guard (x8 == x1);
                                                                                      (x2,
                                                                                       x3,
                                                                                       x5) <- _generateOOOI x6 gen__generateOOOI_x0 gen__generateOOOI_x1;
                                                                                      return (x2,
                                                                                              x3,
                                                                                              x5)}]
_generateOOOI :: MonadPlus m => Term -> m Term -> m Term -> m (Term, Term, Term)
_generateOOOI x3 gen__generateOOOI_x0 gen__generateOOOI_x1 = msum [do {(x9,
                                                                        x10) <- case x3 of
                                                                                {Cons y9
                                                                                      y10 -> return (y9,
                                                                                                     y10);
                                                                                 _ -> mzero};
                                                                       let {x0 = x9};
                                                                       let {x2 = x10};
                                                                       x1 <- gen__generateOOOI_x1;
                                                                       return (x0, x1, x2)},
                                                                   do {(x11, x12) <- case x3 of
                                                                                     {Cons y11
                                                                                           y12 -> return (y11,
                                                                                                          y12);
                                                                                      _ -> mzero};
                                                                       let {x1 = x11};
                                                                       let {x2 = x12};
                                                                       x0 <- gen__generateOOOI_x0;
                                                                       return (x0, x1, x2)}]
generateGenerateIOIIIO :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m (Term, Term)
generateGenerateIOIIIO x0 x2 x3 x4 gen_generateGenerateIOIIIO_x1 = msum [do {(x7,
                                                                              x6) <- case x4 of
                                                                                     {Cons y7
                                                                                           y6 -> return (y7,
                                                                                                         y6);
                                                                                      _ -> mzero};
                                                                             guard (x7 == x0);
                                                                             x5 <- _generateIIOI x2 x3 x6;
                                                                             x1 <- gen_generateGenerateIOIIIO_x1;
                                                                             return (x1, x5)},
                                                                         do {(x8, x6) <- case x4 of
                                                                                         {Cons y8
                                                                                               y6 -> return (y8,
                                                                                                             y6);
                                                                                          _ -> mzero};
                                                                             x5 <- _generateIIOI x2 x3 x6;
                                                                             let {x1 = x8};
                                                                             return (x1, x5)}]
generateGenerateIOIOIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateGenerateIOIOIO x0 x2 x4 gen__generateIOOI_x1 gen_generateGenerateIOIOIO_x1 = msum [do {(x7,
                                                                                                x6) <- case x4 of
                                                                                                       {Cons y7
                                                                                                             y6 -> return (y7,
                                                                                                                           y6);
                                                                                                        _ -> mzero};
                                                                                               guard (x7 == x0);
                                                                                               (x3,
                                                                                                x5) <- _generateIOOI x2 x6 gen__generateIOOI_x1;
                                                                                               x1 <- gen_generateGenerateIOIOIO_x1;
                                                                                               return (x1,
                                                                                                       x3,
                                                                                                       x5)},
                                                                                           do {(x8,
                                                                                                x6) <- case x4 of
                                                                                                       {Cons y8
                                                                                                             y6 -> return (y8,
                                                                                                                           y6);
                                                                                                        _ -> mzero};
                                                                                               (x3,
                                                                                                x5) <- _generateIOOI x2 x6 gen__generateIOOI_x1;
                                                                                               let {x1 = x8};
                                                                                               return (x1,
                                                                                                       x3,
                                                                                                       x5)}]
generateGenerateIOOIIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateGenerateIOOIIO x0 x3 x4 gen__generateOIOI_x0 gen_generateGenerateIOOIIO_x1 = msum [do {(x7,
                                                                                                x6) <- case x4 of
                                                                                                       {Cons y7
                                                                                                             y6 -> return (y7,
                                                                                                                           y6);
                                                                                                        _ -> mzero};
                                                                                               guard (x7 == x0);
                                                                                               (x2,
                                                                                                x5) <- _generateOIOI x3 x6 gen__generateOIOI_x0;
                                                                                               x1 <- gen_generateGenerateIOOIIO_x1;
                                                                                               return (x1,
                                                                                                       x2,
                                                                                                       x5)},
                                                                                           do {(x8,
                                                                                                x6) <- case x4 of
                                                                                                       {Cons y8
                                                                                                             y6 -> return (y8,
                                                                                                                           y6);
                                                                                                        _ -> mzero};
                                                                                               (x2,
                                                                                                x5) <- _generateOIOI x3 x6 gen__generateOIOI_x0;
                                                                                               let {x1 = x8};
                                                                                               return (x1,
                                                                                                       x2,
                                                                                                       x5)}]
generateGenerateIOOOIO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
generateGenerateIOOOIO x0 x4 gen__generateOOOI_x0 gen__generateOOOI_x1 gen_generateGenerateIOOOIO_x1 = msum [do {(x7,
                                                                                                                  x6) <- case x4 of
                                                                                                                         {Cons y7
                                                                                                                               y6 -> return (y7,
                                                                                                                                             y6);
                                                                                                                          _ -> mzero};
                                                                                                                 guard (x7 == x0);
                                                                                                                 (x2,
                                                                                                                  x3,
                                                                                                                  x5) <- _generateOOOI x6 gen__generateOOOI_x0 gen__generateOOOI_x1;
                                                                                                                 x1 <- gen_generateGenerateIOOOIO_x1;
                                                                                                                 return (x1,
                                                                                                                         x2,
                                                                                                                         x3,
                                                                                                                         x5)},
                                                                                                             do {(x8,
                                                                                                                  x6) <- case x4 of
                                                                                                                         {Cons y8
                                                                                                                               y6 -> return (y8,
                                                                                                                                             y6);
                                                                                                                          _ -> mzero};
                                                                                                                 (x2,
                                                                                                                  x3,
                                                                                                                  x5) <- _generateOOOI x6 gen__generateOOOI_x0 gen__generateOOOI_x1;
                                                                                                                 let {x1 = x8};
                                                                                                                 return (x1,
                                                                                                                         x2,
                                                                                                                         x3,
                                                                                                                         x5)}]
generateGenerateOIIIIO :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m (Term, Term)
generateGenerateOIIIIO x1 x2 x3 x4 gen_generateGenerateOIIIIO_x0 = msum [do {(x7,
                                                                              x6) <- case x4 of
                                                                                     {Cons y7
                                                                                           y6 -> return (y7,
                                                                                                         y6);
                                                                                      _ -> mzero};
                                                                             x5 <- _generateIIOI x2 x3 x6;
                                                                             let {x0 = x7};
                                                                             return (x0, x5)},
                                                                         do {(x8, x6) <- case x4 of
                                                                                         {Cons y8
                                                                                               y6 -> return (y8,
                                                                                                             y6);
                                                                                          _ -> mzero};
                                                                             guard (x8 == x1);
                                                                             x5 <- _generateIIOI x2 x3 x6;
                                                                             x0 <- gen_generateGenerateOIIIIO_x0;
                                                                             return (x0, x5)}]
generateGenerateOIIOIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateGenerateOIIOIO x1 x2 x4 gen__generateIOOI_x1 gen_generateGenerateOIIOIO_x0 = msum [do {(x7,
                                                                                                x6) <- case x4 of
                                                                                                       {Cons y7
                                                                                                             y6 -> return (y7,
                                                                                                                           y6);
                                                                                                        _ -> mzero};
                                                                                               (x3,
                                                                                                x5) <- _generateIOOI x2 x6 gen__generateIOOI_x1;
                                                                                               let {x0 = x7};
                                                                                               return (x0,
                                                                                                       x3,
                                                                                                       x5)},
                                                                                           do {(x8,
                                                                                                x6) <- case x4 of
                                                                                                       {Cons y8
                                                                                                             y6 -> return (y8,
                                                                                                                           y6);
                                                                                                        _ -> mzero};
                                                                                               guard (x8 == x1);
                                                                                               (x3,
                                                                                                x5) <- _generateIOOI x2 x6 gen__generateIOOI_x1;
                                                                                               x0 <- gen_generateGenerateOIIOIO_x0;
                                                                                               return (x0,
                                                                                                       x3,
                                                                                                       x5)}]
generateGenerateOIOIIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateGenerateOIOIIO x1 x3 x4 gen__generateOIOI_x0 gen_generateGenerateOIOIIO_x0 = msum [do {(x7,
                                                                                                x6) <- case x4 of
                                                                                                       {Cons y7
                                                                                                             y6 -> return (y7,
                                                                                                                           y6);
                                                                                                        _ -> mzero};
                                                                                               (x2,
                                                                                                x5) <- _generateOIOI x3 x6 gen__generateOIOI_x0;
                                                                                               let {x0 = x7};
                                                                                               return (x0,
                                                                                                       x2,
                                                                                                       x5)},
                                                                                           do {(x8,
                                                                                                x6) <- case x4 of
                                                                                                       {Cons y8
                                                                                                             y6 -> return (y8,
                                                                                                                           y6);
                                                                                                        _ -> mzero};
                                                                                               guard (x8 == x1);
                                                                                               (x2,
                                                                                                x5) <- _generateOIOI x3 x6 gen__generateOIOI_x0;
                                                                                               x0 <- gen_generateGenerateOIOIIO_x0;
                                                                                               return (x0,
                                                                                                       x2,
                                                                                                       x5)}]
generateGenerateOIOOIO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
generateGenerateOIOOIO x1 x4 gen__generateOOOI_x0 gen__generateOOOI_x1 gen_generateGenerateOIOOIO_x0 = msum [do {(x7,
                                                                                                                  x6) <- case x4 of
                                                                                                                         {Cons y7
                                                                                                                               y6 -> return (y7,
                                                                                                                                             y6);
                                                                                                                          _ -> mzero};
                                                                                                                 (x2,
                                                                                                                  x3,
                                                                                                                  x5) <- _generateOOOI x6 gen__generateOOOI_x0 gen__generateOOOI_x1;
                                                                                                                 let {x0 = x7};
                                                                                                                 return (x0,
                                                                                                                         x2,
                                                                                                                         x3,
                                                                                                                         x5)},
                                                                                                             do {(x8,
                                                                                                                  x6) <- case x4 of
                                                                                                                         {Cons y8
                                                                                                                               y6 -> return (y8,
                                                                                                                                             y6);
                                                                                                                          _ -> mzero};
                                                                                                                 guard (x8 == x1);
                                                                                                                 (x2,
                                                                                                                  x3,
                                                                                                                  x5) <- _generateOOOI x6 gen__generateOOOI_x0 gen__generateOOOI_x1;
                                                                                                                 x0 <- gen_generateGenerateOIOOIO_x0;
                                                                                                                 return (x0,
                                                                                                                         x2,
                                                                                                                         x3,
                                                                                                                         x5)}]
generateGenerateOOIIIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateGenerateOOIIIO x2 x3 x4 gen_generateGenerateOOIIIO_x0 gen_generateGenerateOOIIIO_x1 = msum [do {(x7,
                                                                                                         x6) <- case x4 of
                                                                                                                {Cons y7
                                                                                                                      y6 -> return (y7,
                                                                                                                                    y6);
                                                                                                                 _ -> mzero};
                                                                                                        x5 <- _generateIIOI x2 x3 x6;
                                                                                                        let {x0 = x7};
                                                                                                        x1 <- gen_generateGenerateOOIIIO_x1;
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x5)},
                                                                                                    do {(x8,
                                                                                                         x6) <- case x4 of
                                                                                                                {Cons y8
                                                                                                                      y6 -> return (y8,
                                                                                                                                    y6);
                                                                                                                 _ -> mzero};
                                                                                                        x5 <- _generateIIOI x2 x3 x6;
                                                                                                        let {x1 = x8};
                                                                                                        x0 <- gen_generateGenerateOOIIIO_x0;
                                                                                                        return (x0,
                                                                                                                x1,
                                                                                                                x5)}]
generateGenerateOOIOIO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
generateGenerateOOIOIO x2 x4 gen__generateIOOI_x1 gen_generateGenerateOOIOIO_x0 gen_generateGenerateOOIOIO_x1 = msum [do {(x7,
                                                                                                                           x6) <- case x4 of
                                                                                                                                  {Cons y7
                                                                                                                                        y6 -> return (y7,
                                                                                                                                                      y6);
                                                                                                                                   _ -> mzero};
                                                                                                                          (x3,
                                                                                                                           x5) <- _generateIOOI x2 x6 gen__generateIOOI_x1;
                                                                                                                          let {x0 = x7};
                                                                                                                          x1 <- gen_generateGenerateOOIOIO_x1;
                                                                                                                          return (x0,
                                                                                                                                  x1,
                                                                                                                                  x3,
                                                                                                                                  x5)},
                                                                                                                      do {(x8,
                                                                                                                           x6) <- case x4 of
                                                                                                                                  {Cons y8
                                                                                                                                        y6 -> return (y8,
                                                                                                                                                      y6);
                                                                                                                                   _ -> mzero};
                                                                                                                          (x3,
                                                                                                                           x5) <- _generateIOOI x2 x6 gen__generateIOOI_x1;
                                                                                                                          let {x1 = x8};
                                                                                                                          x0 <- gen_generateGenerateOOIOIO_x0;
                                                                                                                          return (x0,
                                                                                                                                  x1,
                                                                                                                                  x3,
                                                                                                                                  x5)}]
generateGenerateOOOIIO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
generateGenerateOOOIIO x3 x4 gen__generateOIOI_x0 gen_generateGenerateOOOIIO_x0 gen_generateGenerateOOOIIO_x1 = msum [do {(x7,
                                                                                                                           x6) <- case x4 of
                                                                                                                                  {Cons y7
                                                                                                                                        y6 -> return (y7,
                                                                                                                                                      y6);
                                                                                                                                   _ -> mzero};
                                                                                                                          (x2,
                                                                                                                           x5) <- _generateOIOI x3 x6 gen__generateOIOI_x0;
                                                                                                                          let {x0 = x7};
                                                                                                                          x1 <- gen_generateGenerateOOOIIO_x1;
                                                                                                                          return (x0,
                                                                                                                                  x1,
                                                                                                                                  x2,
                                                                                                                                  x5)},
                                                                                                                      do {(x8,
                                                                                                                           x6) <- case x4 of
                                                                                                                                  {Cons y8
                                                                                                                                        y6 -> return (y8,
                                                                                                                                                      y6);
                                                                                                                                   _ -> mzero};
                                                                                                                          (x2,
                                                                                                                           x5) <- _generateOIOI x3 x6 gen__generateOIOI_x0;
                                                                                                                          let {x1 = x8};
                                                                                                                          x0 <- gen_generateGenerateOOOIIO_x0;
                                                                                                                          return (x0,
                                                                                                                                  x1,
                                                                                                                                  x2,
                                                                                                                                  x5)}]
generateGenerateOOOOIO :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term, Term)
generateGenerateOOOOIO x4 gen__generateOOOI_x0 gen__generateOOOI_x1 gen_generateGenerateOOOOIO_x0 gen_generateGenerateOOOOIO_x1 = msum [do {(x7,
                                                                                                                                             x6) <- case x4 of
                                                                                                                                                    {Cons y7
                                                                                                                                                          y6 -> return (y7,
                                                                                                                                                                        y6);
                                                                                                                                                     _ -> mzero};
                                                                                                                                            (x2,
                                                                                                                                             x3,
                                                                                                                                             x5) <- _generateOOOI x6 gen__generateOOOI_x0 gen__generateOOOI_x1;
                                                                                                                                            let {x0 = x7};
                                                                                                                                            x1 <- gen_generateGenerateOOOOIO_x1;
                                                                                                                                            return (x0,
                                                                                                                                                    x1,
                                                                                                                                                    x2,
                                                                                                                                                    x3,
                                                                                                                                                    x5)},
                                                                                                                                        do {(x8,
                                                                                                                                             x6) <- case x4 of
                                                                                                                                                    {Cons y8
                                                                                                                                                          y6 -> return (y8,
                                                                                                                                                                        y6);
                                                                                                                                                     _ -> mzero};
                                                                                                                                            (x2,
                                                                                                                                             x3,
                                                                                                                                             x5) <- _generateOOOI x6 gen__generateOOOI_x0 gen__generateOOOI_x1;
                                                                                                                                            let {x1 = x8};
                                                                                                                                            x0 <- gen_generateGenerateOOOOIO_x0;
                                                                                                                                            return (x0,
                                                                                                                                                    x1,
                                                                                                                                                    x2,
                                                                                                                                                    x3,
                                                                                                                                                    x5)}]