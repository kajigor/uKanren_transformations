module Regexp1_unfold where

import Stream
import Control.Monad
import Term

generateIIIII :: MonadPlus m => Term -> Term -> Term -> Term -> Term -> m ()
generateIIIII x0 x1 x2 x3 x4 = msum [do {guard (x4 == Nil);
                                         return ()},
                                     do {(x7, x8) <- case x4 of
                                                     {Cons y7 y8 -> return (y7, y8); _ -> mzero};
                                         guard (x7 == x0);
                                         x5 <- generateIIIIO x0 x1 x2 x3;
                                         let {x6 = Cons x2 x5};
                                         guard (x8 == x6);
                                         return ()},
                                     do {(x10, x11) <- case x4 of
                                                       {Cons y10 y11 -> return (y10, y11);
                                                        _ -> mzero};
                                         guard (x10 == x0);
                                         x5 <- generateIIIIO x0 x1 x2 x3;
                                         let {x9 = Cons x3 x5};
                                         guard (x11 == x9);
                                         return ()},
                                     do {(x13, x14) <- case x4 of
                                                       {Cons y13 y14 -> return (y13, y14);
                                                        _ -> mzero};
                                         guard (x13 == x1);
                                         x5 <- generateIIIIO x0 x1 x2 x3;
                                         let {x12 = Cons x2 x5};
                                         guard (x14 == x12);
                                         return ()},
                                     do {(x16, x17) <- case x4 of
                                                       {Cons y16 y17 -> return (y16, y17);
                                                        _ -> mzero};
                                         guard (x16 == x1);
                                         x5 <- generateIIIIO x0 x1 x2 x3;
                                         let {x15 = Cons x3 x5};
                                         guard (x17 == x15);
                                         return ()}]
generateIIIIO :: MonadPlus m => Term -> Term -> Term -> Term -> m Term
generateIIIIO x0 x1 x2 x3 = msum [do {let {x4 = Nil}; return x4},
                                  do {let {x7 = x0};
                                      x5 <- generateIIIIO x0 x1 x2 x3;
                                      let {x6 = Cons x2 x5};
                                      let {x8 = x6};
                                      let {x4 = Cons x7 x8};
                                      return x4},
                                  do {let {x10 = x0};
                                      x5 <- generateIIIIO x0 x1 x2 x3;
                                      let {x9 = Cons x3 x5};
                                      let {x11 = x9};
                                      let {x4 = Cons x10 x11};
                                      return x4},
                                  do {let {x13 = x1};
                                      x5 <- generateIIIIO x0 x1 x2 x3;
                                      let {x12 = Cons x2 x5};
                                      let {x14 = x12};
                                      let {x4 = Cons x13 x14};
                                      return x4},
                                  do {let {x16 = x1};
                                      x5 <- generateIIIIO x0 x1 x2 x3;
                                      let {x15 = Cons x3 x5};
                                      let {x17 = x15};
                                      let {x4 = Cons x16 x17};
                                      return x4}]
generateIIIOI :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term
generateIIIOI x0 x1 x2 x4 gen_generateIIIOI_x3 gen_generateIIIOO_x3 = msum [do {guard (x4 == Nil);
                                                                                x3 <- gen_generateIIIOI_x3;
                                                                                return x3},
                                                                            do {(x7,
                                                                                 x8) <- case x4 of
                                                                                        {Cons y7
                                                                                              y8 -> return (y7,
                                                                                                            y8);
                                                                                         _ -> mzero};
                                                                                guard (x7 == x0);
                                                                                (x3,
                                                                                 x5) <- generateIIIOO x0 x1 x2 gen_generateIIIOO_x3;
                                                                                let {x6 = Cons x2 x5};
                                                                                guard (x8 == x6);
                                                                                return x3},
                                                                            do {(x10,
                                                                                 x11) <- case x4 of
                                                                                         {Cons y10
                                                                                               y11 -> return (y10,
                                                                                                              y11);
                                                                                          _ -> mzero};
                                                                                guard (x10 == x0);
                                                                                (x3,
                                                                                 x5) <- generateIIIOO x0 x1 x2 gen_generateIIIOO_x3;
                                                                                let {x9 = Cons x3 x5};
                                                                                guard (x11 == x9);
                                                                                return x3},
                                                                            do {(x13,
                                                                                 x14) <- case x4 of
                                                                                         {Cons y13
                                                                                               y14 -> return (y13,
                                                                                                              y14);
                                                                                          _ -> mzero};
                                                                                guard (x13 == x1);
                                                                                (x3,
                                                                                 x5) <- generateIIIOO x0 x1 x2 gen_generateIIIOO_x3;
                                                                                let {x12 = Cons x2 x5};
                                                                                guard (x14 == x12);
                                                                                return x3},
                                                                            do {(x16,
                                                                                 x17) <- case x4 of
                                                                                         {Cons y16
                                                                                               y17 -> return (y16,
                                                                                                              y17);
                                                                                          _ -> mzero};
                                                                                guard (x16 == x1);
                                                                                (x3,
                                                                                 x5) <- generateIIIOO x0 x1 x2 gen_generateIIIOO_x3;
                                                                                let {x15 = Cons x3 x5};
                                                                                guard (x17 == x15);
                                                                                return x3}]
generateIIIOO :: MonadPlus m => Term -> Term -> Term -> m Term -> m (Term, Term)
generateIIIOO x0 x1 x2 gen_generateIIIOO_x3 = msum [do {let {x4 = Nil};
                                                        x3 <- gen_generateIIIOO_x3;
                                                        return (x3, x4)},
                                                    do {let {x7 = x0};
                                                        (x3,
                                                         x5) <- generateIIIOO x0 x1 x2 gen_generateIIIOO_x3;
                                                        let {x6 = Cons x2 x5};
                                                        let {x8 = x6};
                                                        let {x4 = Cons x7 x8};
                                                        return (x3, x4)},
                                                    do {let {x10 = x0};
                                                        (x3,
                                                         x5) <- generateIIIOO x0 x1 x2 gen_generateIIIOO_x3;
                                                        let {x9 = Cons x3 x5};
                                                        let {x11 = x9};
                                                        let {x4 = Cons x10 x11};
                                                        return (x3, x4)},
                                                    do {let {x13 = x1};
                                                        (x3,
                                                         x5) <- generateIIIOO x0 x1 x2 gen_generateIIIOO_x3;
                                                        let {x12 = Cons x2 x5};
                                                        let {x14 = x12};
                                                        let {x4 = Cons x13 x14};
                                                        return (x3, x4)},
                                                    do {let {x16 = x1};
                                                        (x3,
                                                         x5) <- generateIIIOO x0 x1 x2 gen_generateIIIOO_x3;
                                                        let {x15 = Cons x3 x5};
                                                        let {x17 = x15};
                                                        let {x4 = Cons x16 x17};
                                                        return (x3, x4)}]
generateIIOII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term
generateIIOII x0 x1 x3 x4 gen_generateIIOII_x2 gen_generateIIOIO_x2 = msum [do {guard (x4 == Nil);
                                                                                x2 <- gen_generateIIOII_x2;
                                                                                return x2},
                                                                            do {(x7,
                                                                                 x8) <- case x4 of
                                                                                        {Cons y7
                                                                                              y8 -> return (y7,
                                                                                                            y8);
                                                                                         _ -> mzero};
                                                                                guard (x7 == x0);
                                                                                (x2,
                                                                                 x5) <- generateIIOIO x0 x1 x3 gen_generateIIOIO_x2;
                                                                                let {x6 = Cons x2 x5};
                                                                                guard (x8 == x6);
                                                                                return x2},
                                                                            do {(x10,
                                                                                 x11) <- case x4 of
                                                                                         {Cons y10
                                                                                               y11 -> return (y10,
                                                                                                              y11);
                                                                                          _ -> mzero};
                                                                                guard (x10 == x0);
                                                                                (x2,
                                                                                 x5) <- generateIIOIO x0 x1 x3 gen_generateIIOIO_x2;
                                                                                let {x9 = Cons x3 x5};
                                                                                guard (x11 == x9);
                                                                                return x2},
                                                                            do {(x13,
                                                                                 x14) <- case x4 of
                                                                                         {Cons y13
                                                                                               y14 -> return (y13,
                                                                                                              y14);
                                                                                          _ -> mzero};
                                                                                guard (x13 == x1);
                                                                                (x2,
                                                                                 x5) <- generateIIOIO x0 x1 x3 gen_generateIIOIO_x2;
                                                                                let {x12 = Cons x2 x5};
                                                                                guard (x14 == x12);
                                                                                return x2},
                                                                            do {(x16,
                                                                                 x17) <- case x4 of
                                                                                         {Cons y16
                                                                                               y17 -> return (y16,
                                                                                                              y17);
                                                                                          _ -> mzero};
                                                                                guard (x16 == x1);
                                                                                (x2,
                                                                                 x5) <- generateIIOIO x0 x1 x3 gen_generateIIOIO_x2;
                                                                                let {x15 = Cons x3 x5};
                                                                                guard (x17 == x15);
                                                                                return x2}]
generateIIOIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m (Term, Term)
generateIIOIO x0 x1 x3 gen_generateIIOIO_x2 = msum [do {let {x4 = Nil};
                                                        x2 <- gen_generateIIOIO_x2;
                                                        return (x2, x4)},
                                                    do {let {x7 = x0};
                                                        (x2,
                                                         x5) <- generateIIOIO x0 x1 x3 gen_generateIIOIO_x2;
                                                        let {x6 = Cons x2 x5};
                                                        let {x8 = x6};
                                                        let {x4 = Cons x7 x8};
                                                        return (x2, x4)},
                                                    do {let {x10 = x0};
                                                        (x2,
                                                         x5) <- generateIIOIO x0 x1 x3 gen_generateIIOIO_x2;
                                                        let {x9 = Cons x3 x5};
                                                        let {x11 = x9};
                                                        let {x4 = Cons x10 x11};
                                                        return (x2, x4)},
                                                    do {let {x13 = x1};
                                                        (x2,
                                                         x5) <- generateIIOIO x0 x1 x3 gen_generateIIOIO_x2;
                                                        let {x12 = Cons x2 x5};
                                                        let {x14 = x12};
                                                        let {x4 = Cons x13 x14};
                                                        return (x2, x4)},
                                                    do {let {x16 = x1};
                                                        (x2,
                                                         x5) <- generateIIOIO x0 x1 x3 gen_generateIIOIO_x2;
                                                        let {x15 = Cons x3 x5};
                                                        let {x17 = x15};
                                                        let {x4 = Cons x16 x17};
                                                        return (x2, x4)}]
generateIIOOI :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateIIOOI x0 x1 x4 gen_generateIIOOI_x2 gen_generateIIOOI_x3 gen_generateIIOOO_x2 gen_generateIIOOO_x3 = msum [do {guard (x4 == Nil);
                                                                                                                       x2 <- gen_generateIIOOI_x2;
                                                                                                                       x3 <- gen_generateIIOOI_x3;
                                                                                                                       return (x2,
                                                                                                                               x3)},
                                                                                                                   do {(x7,
                                                                                                                        x8) <- case x4 of
                                                                                                                               {Cons y7
                                                                                                                                     y8 -> return (y7,
                                                                                                                                                   y8);
                                                                                                                                _ -> mzero};
                                                                                                                       guard (x7 == x0);
                                                                                                                       (x2,
                                                                                                                        x3,
                                                                                                                        x5) <- generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3;
                                                                                                                       let {x6 = Cons x2 x5};
                                                                                                                       guard (x8 == x6);
                                                                                                                       return (x2,
                                                                                                                               x3)},
                                                                                                                   do {(x10,
                                                                                                                        x11) <- case x4 of
                                                                                                                                {Cons y10
                                                                                                                                      y11 -> return (y10,
                                                                                                                                                     y11);
                                                                                                                                 _ -> mzero};
                                                                                                                       guard (x10 == x0);
                                                                                                                       (x2,
                                                                                                                        x3,
                                                                                                                        x5) <- generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3;
                                                                                                                       let {x9 = Cons x3 x5};
                                                                                                                       guard (x11 == x9);
                                                                                                                       return (x2,
                                                                                                                               x3)},
                                                                                                                   do {(x13,
                                                                                                                        x14) <- case x4 of
                                                                                                                                {Cons y13
                                                                                                                                      y14 -> return (y13,
                                                                                                                                                     y14);
                                                                                                                                 _ -> mzero};
                                                                                                                       guard (x13 == x1);
                                                                                                                       (x2,
                                                                                                                        x3,
                                                                                                                        x5) <- generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3;
                                                                                                                       let {x12 = Cons x2 x5};
                                                                                                                       guard (x14 == x12);
                                                                                                                       return (x2,
                                                                                                                               x3)},
                                                                                                                   do {(x16,
                                                                                                                        x17) <- case x4 of
                                                                                                                                {Cons y16
                                                                                                                                      y17 -> return (y16,
                                                                                                                                                     y17);
                                                                                                                                 _ -> mzero};
                                                                                                                       guard (x16 == x1);
                                                                                                                       (x2,
                                                                                                                        x3,
                                                                                                                        x5) <- generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3;
                                                                                                                       let {x15 = Cons x3 x5};
                                                                                                                       guard (x17 == x15);
                                                                                                                       return (x2,
                                                                                                                               x3)}]
generateIIOOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3 = msum [do {let {x4 = Nil};
                                                                          x2 <- gen_generateIIOOO_x2;
                                                                          x3 <- gen_generateIIOOO_x3;
                                                                          return (x2, x3, x4)},
                                                                      do {let {x7 = x0};
                                                                          (x2,
                                                                           x3,
                                                                           x5) <- generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3;
                                                                          let {x6 = Cons x2 x5};
                                                                          let {x8 = x6};
                                                                          let {x4 = Cons x7 x8};
                                                                          return (x2, x3, x4)},
                                                                      do {let {x10 = x0};
                                                                          (x2,
                                                                           x3,
                                                                           x5) <- generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3;
                                                                          let {x9 = Cons x3 x5};
                                                                          let {x11 = x9};
                                                                          let {x4 = Cons x10 x11};
                                                                          return (x2, x3, x4)},
                                                                      do {let {x13 = x1};
                                                                          (x2,
                                                                           x3,
                                                                           x5) <- generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3;
                                                                          let {x12 = Cons x2 x5};
                                                                          let {x14 = x12};
                                                                          let {x4 = Cons x13 x14};
                                                                          return (x2, x3, x4)},
                                                                      do {let {x16 = x1};
                                                                          (x2,
                                                                           x3,
                                                                           x5) <- generateIIOOO x0 x1 gen_generateIIOOO_x2 gen_generateIIOOO_x3;
                                                                          let {x15 = Cons x3 x5};
                                                                          let {x17 = x15};
                                                                          let {x4 = Cons x16 x17};
                                                                          return (x2, x3, x4)}]
generateIOIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term
generateIOIII x0 x2 x3 x4 gen_generateIOIII_x1 gen_generateIOIIO_x1 = msum [do {guard (x4 == Nil);
                                                                                x1 <- gen_generateIOIII_x1;
                                                                                return x1},
                                                                            do {(x7,
                                                                                 x8) <- case x4 of
                                                                                        {Cons y7
                                                                                              y8 -> return (y7,
                                                                                                            y8);
                                                                                         _ -> mzero};
                                                                                guard (x7 == x0);
                                                                                (x1,
                                                                                 x5) <- generateIOIIO x0 x2 x3 gen_generateIOIIO_x1;
                                                                                let {x6 = Cons x2 x5};
                                                                                guard (x8 == x6);
                                                                                return x1},
                                                                            do {(x10,
                                                                                 x11) <- case x4 of
                                                                                         {Cons y10
                                                                                               y11 -> return (y10,
                                                                                                              y11);
                                                                                          _ -> mzero};
                                                                                guard (x10 == x0);
                                                                                (x1,
                                                                                 x5) <- generateIOIIO x0 x2 x3 gen_generateIOIIO_x1;
                                                                                let {x9 = Cons x3 x5};
                                                                                guard (x11 == x9);
                                                                                return x1},
                                                                            do {(x13,
                                                                                 x14) <- case x4 of
                                                                                         {Cons y13
                                                                                               y14 -> return (y13,
                                                                                                              y14);
                                                                                          _ -> mzero};
                                                                                (x1,
                                                                                 x5) <- generateIOIIO x0 x2 x3 gen_generateIOIIO_x1;
                                                                                guard (x13 == x1);
                                                                                let {x12 = Cons x2 x5};
                                                                                guard (x14 == x12);
                                                                                return x1},
                                                                            do {(x16,
                                                                                 x17) <- case x4 of
                                                                                         {Cons y16
                                                                                               y17 -> return (y16,
                                                                                                              y17);
                                                                                          _ -> mzero};
                                                                                (x1,
                                                                                 x5) <- generateIOIIO x0 x2 x3 gen_generateIOIIO_x1;
                                                                                guard (x16 == x1);
                                                                                let {x15 = Cons x3 x5};
                                                                                guard (x17 == x15);
                                                                                return x1}]
generateIOIIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m (Term, Term)
generateIOIIO x0 x2 x3 gen_generateIOIIO_x1 = msum [do {let {x4 = Nil};
                                                        x1 <- gen_generateIOIIO_x1;
                                                        return (x1, x4)},
                                                    do {let {x7 = x0};
                                                        (x1,
                                                         x5) <- generateIOIIO x0 x2 x3 gen_generateIOIIO_x1;
                                                        let {x6 = Cons x2 x5};
                                                        let {x8 = x6};
                                                        let {x4 = Cons x7 x8};
                                                        return (x1, x4)},
                                                    do {let {x10 = x0};
                                                        (x1,
                                                         x5) <- generateIOIIO x0 x2 x3 gen_generateIOIIO_x1;
                                                        let {x9 = Cons x3 x5};
                                                        let {x11 = x9};
                                                        let {x4 = Cons x10 x11};
                                                        return (x1, x4)},
                                                    do {(x1,
                                                         x5) <- generateIOIIO x0 x2 x3 gen_generateIOIIO_x1;
                                                        let {x12 = Cons x2 x5};
                                                        let {x13 = x1};
                                                        let {x14 = x12};
                                                        let {x4 = Cons x13 x14};
                                                        return (x1, x4)},
                                                    do {(x1,
                                                         x5) <- generateIOIIO x0 x2 x3 gen_generateIOIIO_x1;
                                                        let {x15 = Cons x3 x5};
                                                        let {x16 = x1};
                                                        let {x17 = x15};
                                                        let {x4 = Cons x16 x17};
                                                        return (x1, x4)}]
generateIOIOI :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateIOIOI x0 x2 x4 gen_generateIOIOI_x1 gen_generateIOIOI_x3 gen_generateIOIOO_x1 gen_generateIOIOO_x3 = msum [do {guard (x4 == Nil);
                                                                                                                       x1 <- gen_generateIOIOI_x1;
                                                                                                                       x3 <- gen_generateIOIOI_x3;
                                                                                                                       return (x1,
                                                                                                                               x3)},
                                                                                                                   do {(x7,
                                                                                                                        x8) <- case x4 of
                                                                                                                               {Cons y7
                                                                                                                                     y8 -> return (y7,
                                                                                                                                                   y8);
                                                                                                                                _ -> mzero};
                                                                                                                       guard (x7 == x0);
                                                                                                                       (x1,
                                                                                                                        x3,
                                                                                                                        x5) <- generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3;
                                                                                                                       let {x6 = Cons x2 x5};
                                                                                                                       guard (x8 == x6);
                                                                                                                       return (x1,
                                                                                                                               x3)},
                                                                                                                   do {(x10,
                                                                                                                        x11) <- case x4 of
                                                                                                                                {Cons y10
                                                                                                                                      y11 -> return (y10,
                                                                                                                                                     y11);
                                                                                                                                 _ -> mzero};
                                                                                                                       guard (x10 == x0);
                                                                                                                       (x1,
                                                                                                                        x3,
                                                                                                                        x5) <- generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3;
                                                                                                                       let {x9 = Cons x3 x5};
                                                                                                                       guard (x11 == x9);
                                                                                                                       return (x1,
                                                                                                                               x3)},
                                                                                                                   do {(x13,
                                                                                                                        x14) <- case x4 of
                                                                                                                                {Cons y13
                                                                                                                                      y14 -> return (y13,
                                                                                                                                                     y14);
                                                                                                                                 _ -> mzero};
                                                                                                                       (x1,
                                                                                                                        x3,
                                                                                                                        x5) <- generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3;
                                                                                                                       guard (x13 == x1);
                                                                                                                       let {x12 = Cons x2 x5};
                                                                                                                       guard (x14 == x12);
                                                                                                                       return (x1,
                                                                                                                               x3)},
                                                                                                                   do {(x16,
                                                                                                                        x17) <- case x4 of
                                                                                                                                {Cons y16
                                                                                                                                      y17 -> return (y16,
                                                                                                                                                     y17);
                                                                                                                                 _ -> mzero};
                                                                                                                       (x1,
                                                                                                                        x3,
                                                                                                                        x5) <- generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3;
                                                                                                                       guard (x16 == x1);
                                                                                                                       let {x15 = Cons x3 x5};
                                                                                                                       guard (x17 == x15);
                                                                                                                       return (x1,
                                                                                                                               x3)}]
generateIOIOO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3 = msum [do {let {x4 = Nil};
                                                                          x1 <- gen_generateIOIOO_x1;
                                                                          x3 <- gen_generateIOIOO_x3;
                                                                          return (x1, x3, x4)},
                                                                      do {let {x7 = x0};
                                                                          (x1,
                                                                           x3,
                                                                           x5) <- generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3;
                                                                          let {x6 = Cons x2 x5};
                                                                          let {x8 = x6};
                                                                          let {x4 = Cons x7 x8};
                                                                          return (x1, x3, x4)},
                                                                      do {let {x10 = x0};
                                                                          (x1,
                                                                           x3,
                                                                           x5) <- generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3;
                                                                          let {x9 = Cons x3 x5};
                                                                          let {x11 = x9};
                                                                          let {x4 = Cons x10 x11};
                                                                          return (x1, x3, x4)},
                                                                      do {(x1,
                                                                           x3,
                                                                           x5) <- generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3;
                                                                          let {x12 = Cons x2 x5};
                                                                          let {x13 = x1};
                                                                          let {x14 = x12};
                                                                          let {x4 = Cons x13 x14};
                                                                          return (x1, x3, x4)},
                                                                      do {(x1,
                                                                           x3,
                                                                           x5) <- generateIOIOO x0 x2 gen_generateIOIOO_x1 gen_generateIOIOO_x3;
                                                                          let {x15 = Cons x3 x5};
                                                                          let {x16 = x1};
                                                                          let {x17 = x15};
                                                                          let {x4 = Cons x16 x17};
                                                                          return (x1, x3, x4)}]
generateIOOII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateIOOII x0 x3 x4 gen_generateIOOII_x1 gen_generateIOOII_x2 gen_generateIOOIO_x1 gen_generateIOOIO_x2 = msum [do {guard (x4 == Nil);
                                                                                                                       x1 <- gen_generateIOOII_x1;
                                                                                                                       x2 <- gen_generateIOOII_x2;
                                                                                                                       return (x1,
                                                                                                                               x2)},
                                                                                                                   do {(x7,
                                                                                                                        x8) <- case x4 of
                                                                                                                               {Cons y7
                                                                                                                                     y8 -> return (y7,
                                                                                                                                                   y8);
                                                                                                                                _ -> mzero};
                                                                                                                       guard (x7 == x0);
                                                                                                                       (x1,
                                                                                                                        x2,
                                                                                                                        x5) <- generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2;
                                                                                                                       let {x6 = Cons x2 x5};
                                                                                                                       guard (x8 == x6);
                                                                                                                       return (x1,
                                                                                                                               x2)},
                                                                                                                   do {(x10,
                                                                                                                        x11) <- case x4 of
                                                                                                                                {Cons y10
                                                                                                                                      y11 -> return (y10,
                                                                                                                                                     y11);
                                                                                                                                 _ -> mzero};
                                                                                                                       guard (x10 == x0);
                                                                                                                       (x1,
                                                                                                                        x2,
                                                                                                                        x5) <- generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2;
                                                                                                                       let {x9 = Cons x3 x5};
                                                                                                                       guard (x11 == x9);
                                                                                                                       return (x1,
                                                                                                                               x2)},
                                                                                                                   do {(x13,
                                                                                                                        x14) <- case x4 of
                                                                                                                                {Cons y13
                                                                                                                                      y14 -> return (y13,
                                                                                                                                                     y14);
                                                                                                                                 _ -> mzero};
                                                                                                                       (x1,
                                                                                                                        x2,
                                                                                                                        x5) <- generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2;
                                                                                                                       guard (x13 == x1);
                                                                                                                       let {x12 = Cons x2 x5};
                                                                                                                       guard (x14 == x12);
                                                                                                                       return (x1,
                                                                                                                               x2)},
                                                                                                                   do {(x16,
                                                                                                                        x17) <- case x4 of
                                                                                                                                {Cons y16
                                                                                                                                      y17 -> return (y16,
                                                                                                                                                     y17);
                                                                                                                                 _ -> mzero};
                                                                                                                       (x1,
                                                                                                                        x2,
                                                                                                                        x5) <- generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2;
                                                                                                                       guard (x16 == x1);
                                                                                                                       let {x15 = Cons x3 x5};
                                                                                                                       guard (x17 == x15);
                                                                                                                       return (x1,
                                                                                                                               x2)}]
generateIOOIO :: MonadPlus m => Term -> Term -> m Term -> m Term -> m (Term, Term, Term)
generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2 = msum [do {let {x4 = Nil};
                                                                          x1 <- gen_generateIOOIO_x1;
                                                                          x2 <- gen_generateIOOIO_x2;
                                                                          return (x1, x2, x4)},
                                                                      do {let {x7 = x0};
                                                                          (x1,
                                                                           x2,
                                                                           x5) <- generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2;
                                                                          let {x6 = Cons x2 x5};
                                                                          let {x8 = x6};
                                                                          let {x4 = Cons x7 x8};
                                                                          return (x1, x2, x4)},
                                                                      do {let {x10 = x0};
                                                                          (x1,
                                                                           x2,
                                                                           x5) <- generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2;
                                                                          let {x9 = Cons x3 x5};
                                                                          let {x11 = x9};
                                                                          let {x4 = Cons x10 x11};
                                                                          return (x1, x2, x4)},
                                                                      do {(x1,
                                                                           x2,
                                                                           x5) <- generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2;
                                                                          let {x12 = Cons x2 x5};
                                                                          let {x13 = x1};
                                                                          let {x14 = x12};
                                                                          let {x4 = Cons x13 x14};
                                                                          return (x1, x2, x4)},
                                                                      do {(x1,
                                                                           x2,
                                                                           x5) <- generateIOOIO x0 x3 gen_generateIOOIO_x1 gen_generateIOOIO_x2;
                                                                          let {x15 = Cons x3 x5};
                                                                          let {x16 = x1};
                                                                          let {x17 = x15};
                                                                          let {x4 = Cons x16 x17};
                                                                          return (x1, x2, x4)}]
generateIOOOI :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
generateIOOOI x0 x4 gen_generateIOOOI_x1 gen_generateIOOOI_x2 gen_generateIOOOI_x3 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3 = msum [do {guard (x4 == Nil);
                                                                                                                                                              x1 <- gen_generateIOOOI_x1;
                                                                                                                                                              x2 <- gen_generateIOOOI_x2;
                                                                                                                                                              x3 <- gen_generateIOOOI_x3;
                                                                                                                                                              return (x1,
                                                                                                                                                                      x2,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x7,
                                                                                                                                                               x8) <- case x4 of
                                                                                                                                                                      {Cons y7
                                                                                                                                                                            y8 -> return (y7,
                                                                                                                                                                                          y8);
                                                                                                                                                                       _ -> mzero};
                                                                                                                                                              guard (x7 == x0);
                                                                                                                                                              (x1,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                                                                                              let {x6 = Cons x2 x5};
                                                                                                                                                              guard (x8 == x6);
                                                                                                                                                              return (x1,
                                                                                                                                                                      x2,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x10,
                                                                                                                                                               x11) <- case x4 of
                                                                                                                                                                       {Cons y10
                                                                                                                                                                             y11 -> return (y10,
                                                                                                                                                                                            y11);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              guard (x10 == x0);
                                                                                                                                                              (x1,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                                                                                              let {x9 = Cons x3 x5};
                                                                                                                                                              guard (x11 == x9);
                                                                                                                                                              return (x1,
                                                                                                                                                                      x2,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x13,
                                                                                                                                                               x14) <- case x4 of
                                                                                                                                                                       {Cons y13
                                                                                                                                                                             y14 -> return (y13,
                                                                                                                                                                                            y14);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              (x1,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                                                                                              guard (x13 == x1);
                                                                                                                                                              let {x12 = Cons x2 x5};
                                                                                                                                                              guard (x14 == x12);
                                                                                                                                                              return (x1,
                                                                                                                                                                      x2,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x16,
                                                                                                                                                               x17) <- case x4 of
                                                                                                                                                                       {Cons y16
                                                                                                                                                                             y17 -> return (y16,
                                                                                                                                                                                            y17);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              (x1,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                                                                                              guard (x16 == x1);
                                                                                                                                                              let {x15 = Cons x3 x5};
                                                                                                                                                              guard (x17 == x15);
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
                                                                                        do {let {x7 = x0};
                                                                                            (x1,
                                                                                             x2,
                                                                                             x3,
                                                                                             x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                            let {x6 = Cons x2 x5};
                                                                                            let {x8 = x6};
                                                                                            let {x4 = Cons x7 x8};
                                                                                            return (x1,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {let {x10 = x0};
                                                                                            (x1,
                                                                                             x2,
                                                                                             x3,
                                                                                             x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                            let {x9 = Cons x3 x5};
                                                                                            let {x11 = x9};
                                                                                            let {x4 = Cons x10 x11};
                                                                                            return (x1,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {(x1,
                                                                                             x2,
                                                                                             x3,
                                                                                             x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                            let {x12 = Cons x2 x5};
                                                                                            let {x13 = x1};
                                                                                            let {x14 = x12};
                                                                                            let {x4 = Cons x13 x14};
                                                                                            return (x1,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {(x1,
                                                                                             x2,
                                                                                             x3,
                                                                                             x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                            let {x15 = Cons x3 x5};
                                                                                            let {x16 = x1};
                                                                                            let {x17 = x15};
                                                                                            let {x4 = Cons x16 x17};
                                                                                            return (x1,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)}]
generateOIIII :: MonadPlus m => Term -> Term -> Term -> Term -> m Term -> m Term -> m Term
generateOIIII x1 x2 x3 x4 gen_generateOIIII_x0 gen_generateOIIIO_x0 = msum [do {guard (x4 == Nil);
                                                                                x0 <- gen_generateOIIII_x0;
                                                                                return x0},
                                                                            do {(x7,
                                                                                 x8) <- case x4 of
                                                                                        {Cons y7
                                                                                              y8 -> return (y7,
                                                                                                            y8);
                                                                                         _ -> mzero};
                                                                                (x0,
                                                                                 x5) <- generateOIIIO x1 x2 x3 gen_generateOIIIO_x0;
                                                                                guard (x7 == x0);
                                                                                let {x6 = Cons x2 x5};
                                                                                guard (x8 == x6);
                                                                                return x0},
                                                                            do {(x10,
                                                                                 x11) <- case x4 of
                                                                                         {Cons y10
                                                                                               y11 -> return (y10,
                                                                                                              y11);
                                                                                          _ -> mzero};
                                                                                (x0,
                                                                                 x5) <- generateOIIIO x1 x2 x3 gen_generateOIIIO_x0;
                                                                                guard (x10 == x0);
                                                                                let {x9 = Cons x3 x5};
                                                                                guard (x11 == x9);
                                                                                return x0},
                                                                            do {(x13,
                                                                                 x14) <- case x4 of
                                                                                         {Cons y13
                                                                                               y14 -> return (y13,
                                                                                                              y14);
                                                                                          _ -> mzero};
                                                                                guard (x13 == x1);
                                                                                (x0,
                                                                                 x5) <- generateOIIIO x1 x2 x3 gen_generateOIIIO_x0;
                                                                                let {x12 = Cons x2 x5};
                                                                                guard (x14 == x12);
                                                                                return x0},
                                                                            do {(x16,
                                                                                 x17) <- case x4 of
                                                                                         {Cons y16
                                                                                               y17 -> return (y16,
                                                                                                              y17);
                                                                                          _ -> mzero};
                                                                                guard (x16 == x1);
                                                                                (x0,
                                                                                 x5) <- generateOIIIO x1 x2 x3 gen_generateOIIIO_x0;
                                                                                let {x15 = Cons x3 x5};
                                                                                guard (x17 == x15);
                                                                                return x0}]
generateOIIIO :: MonadPlus m => Term -> Term -> Term -> m Term -> m (Term, Term)
generateOIIIO x1 x2 x3 gen_generateOIIIO_x0 = msum [do {let {x4 = Nil};
                                                        x0 <- gen_generateOIIIO_x0;
                                                        return (x0, x4)},
                                                    do {(x0,
                                                         x5) <- generateOIIIO x1 x2 x3 gen_generateOIIIO_x0;
                                                        let {x6 = Cons x2 x5};
                                                        let {x7 = x0};
                                                        let {x8 = x6};
                                                        let {x4 = Cons x7 x8};
                                                        return (x0, x4)},
                                                    do {(x0,
                                                         x5) <- generateOIIIO x1 x2 x3 gen_generateOIIIO_x0;
                                                        let {x9 = Cons x3 x5};
                                                        let {x10 = x0};
                                                        let {x11 = x9};
                                                        let {x4 = Cons x10 x11};
                                                        return (x0, x4)},
                                                    do {let {x13 = x1};
                                                        (x0,
                                                         x5) <- generateOIIIO x1 x2 x3 gen_generateOIIIO_x0;
                                                        let {x12 = Cons x2 x5};
                                                        let {x14 = x12};
                                                        let {x4 = Cons x13 x14};
                                                        return (x0, x4)},
                                                    do {let {x16 = x1};
                                                        (x0,
                                                         x5) <- generateOIIIO x1 x2 x3 gen_generateOIIIO_x0;
                                                        let {x15 = Cons x3 x5};
                                                        let {x17 = x15};
                                                        let {x4 = Cons x16 x17};
                                                        return (x0, x4)}]
generateOIIOI :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateOIIOI x1 x2 x4 gen_generateOIIOI_x0 gen_generateOIIOI_x3 gen_generateOIIOO_x0 gen_generateOIIOO_x3 = msum [do {guard (x4 == Nil);
                                                                                                                       x0 <- gen_generateOIIOI_x0;
                                                                                                                       x3 <- gen_generateOIIOI_x3;
                                                                                                                       return (x0,
                                                                                                                               x3)},
                                                                                                                   do {(x7,
                                                                                                                        x8) <- case x4 of
                                                                                                                               {Cons y7
                                                                                                                                     y8 -> return (y7,
                                                                                                                                                   y8);
                                                                                                                                _ -> mzero};
                                                                                                                       (x0,
                                                                                                                        x3,
                                                                                                                        x5) <- generateOIIOO x1 x2 gen_generateOIIOO_x0 gen_generateOIIOO_x3;
                                                                                                                       guard (x7 == x0);
                                                                                                                       let {x6 = Cons x2 x5};
                                                                                                                       guard (x8 == x6);
                                                                                                                       return (x0,
                                                                                                                               x3)},
                                                                                                                   do {(x10,
                                                                                                                        x11) <- case x4 of
                                                                                                                                {Cons y10
                                                                                                                                      y11 -> return (y10,
                                                                                                                                                     y11);
                                                                                                                                 _ -> mzero};
                                                                                                                       (x0,
                                                                                                                        x3,
                                                                                                                        x5) <- generateOIIOO x1 x2 gen_generateOIIOO_x0 gen_generateOIIOO_x3;
                                                                                                                       guard (x10 == x0);
                                                                                                                       let {x9 = Cons x3 x5};
                                                                                                                       guard (x11 == x9);
                                                                                                                       return (x0,
                                                                                                                               x3)},
                                                                                                                   do {(x13,
                                                                                                                        x14) <- case x4 of
                                                                                                                                {Cons y13
                                                                                                                                      y14 -> return (y13,
                                                                                                                                                     y14);
                                                                                                                                 _ -> mzero};
                                                                                                                       guard (x13 == x1);
                                                                                                                       (x0,
                                                                                                                        x3,
                                                                                                                        x5) <- generateOIIOO x1 x2 gen_generateOIIOO_x0 gen_generateOIIOO_x3;
                                                                                                                       let {x12 = Cons x2 x5};
                                                                                                                       guard (x14 == x12);
                                                                                                                       return (x0,
                                                                                                                               x3)},
                                                                                                                   do {(x16,
                                                                                                                        x17) <- case x4 of
                                                                                                                                {Cons y16
                                                                                                                                      y17 -> return (y16,
                                                                                                                                                     y17);
                                                                                                                                 _ -> mzero};
                                                                                                                       guard (x16 == x1);
                                                                                                                       (x0,
                                                                                                                        x3,
                                                                                                                        x5) <- generateOIIOO x1 x2 gen_generateOIIOO_x0 gen_generateOIIOO_x3;
                                                                                                                       let {x15 = Cons x3 x5};
                                                                                                                       guard (x17 == x15);
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
                                                                          let {x6 = Cons x2 x5};
                                                                          let {x7 = x0};
                                                                          let {x8 = x6};
                                                                          let {x4 = Cons x7 x8};
                                                                          return (x0, x3, x4)},
                                                                      do {(x0,
                                                                           x3,
                                                                           x5) <- generateOIIOO x1 x2 gen_generateOIIOO_x0 gen_generateOIIOO_x3;
                                                                          let {x9 = Cons x3 x5};
                                                                          let {x10 = x0};
                                                                          let {x11 = x9};
                                                                          let {x4 = Cons x10 x11};
                                                                          return (x0, x3, x4)},
                                                                      do {let {x13 = x1};
                                                                          (x0,
                                                                           x3,
                                                                           x5) <- generateOIIOO x1 x2 gen_generateOIIOO_x0 gen_generateOIIOO_x3;
                                                                          let {x12 = Cons x2 x5};
                                                                          let {x14 = x12};
                                                                          let {x4 = Cons x13 x14};
                                                                          return (x0, x3, x4)},
                                                                      do {let {x16 = x1};
                                                                          (x0,
                                                                           x3,
                                                                           x5) <- generateOIIOO x1 x2 gen_generateOIIOO_x0 gen_generateOIIOO_x3;
                                                                          let {x15 = Cons x3 x5};
                                                                          let {x17 = x15};
                                                                          let {x4 = Cons x16 x17};
                                                                          return (x0, x3, x4)}]
generateOIOII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateOIOII x1 x3 x4 gen_generateOIOII_x0 gen_generateOIOII_x2 gen_generateOIOIO_x0 gen_generateOIOIO_x2 = msum [do {guard (x4 == Nil);
                                                                                                                       x0 <- gen_generateOIOII_x0;
                                                                                                                       x2 <- gen_generateOIOII_x2;
                                                                                                                       return (x0,
                                                                                                                               x2)},
                                                                                                                   do {(x7,
                                                                                                                        x8) <- case x4 of
                                                                                                                               {Cons y7
                                                                                                                                     y8 -> return (y7,
                                                                                                                                                   y8);
                                                                                                                                _ -> mzero};
                                                                                                                       (x0,
                                                                                                                        x2,
                                                                                                                        x5) <- generateOIOIO x1 x3 gen_generateOIOIO_x0 gen_generateOIOIO_x2;
                                                                                                                       guard (x7 == x0);
                                                                                                                       let {x6 = Cons x2 x5};
                                                                                                                       guard (x8 == x6);
                                                                                                                       return (x0,
                                                                                                                               x2)},
                                                                                                                   do {(x10,
                                                                                                                        x11) <- case x4 of
                                                                                                                                {Cons y10
                                                                                                                                      y11 -> return (y10,
                                                                                                                                                     y11);
                                                                                                                                 _ -> mzero};
                                                                                                                       (x0,
                                                                                                                        x2,
                                                                                                                        x5) <- generateOIOIO x1 x3 gen_generateOIOIO_x0 gen_generateOIOIO_x2;
                                                                                                                       guard (x10 == x0);
                                                                                                                       let {x9 = Cons x3 x5};
                                                                                                                       guard (x11 == x9);
                                                                                                                       return (x0,
                                                                                                                               x2)},
                                                                                                                   do {(x13,
                                                                                                                        x14) <- case x4 of
                                                                                                                                {Cons y13
                                                                                                                                      y14 -> return (y13,
                                                                                                                                                     y14);
                                                                                                                                 _ -> mzero};
                                                                                                                       guard (x13 == x1);
                                                                                                                       (x0,
                                                                                                                        x2,
                                                                                                                        x5) <- generateOIOIO x1 x3 gen_generateOIOIO_x0 gen_generateOIOIO_x2;
                                                                                                                       let {x12 = Cons x2 x5};
                                                                                                                       guard (x14 == x12);
                                                                                                                       return (x0,
                                                                                                                               x2)},
                                                                                                                   do {(x16,
                                                                                                                        x17) <- case x4 of
                                                                                                                                {Cons y16
                                                                                                                                      y17 -> return (y16,
                                                                                                                                                     y17);
                                                                                                                                 _ -> mzero};
                                                                                                                       guard (x16 == x1);
                                                                                                                       (x0,
                                                                                                                        x2,
                                                                                                                        x5) <- generateOIOIO x1 x3 gen_generateOIOIO_x0 gen_generateOIOIO_x2;
                                                                                                                       let {x15 = Cons x3 x5};
                                                                                                                       guard (x17 == x15);
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
                                                                          let {x6 = Cons x2 x5};
                                                                          let {x7 = x0};
                                                                          let {x8 = x6};
                                                                          let {x4 = Cons x7 x8};
                                                                          return (x0, x2, x4)},
                                                                      do {(x0,
                                                                           x2,
                                                                           x5) <- generateOIOIO x1 x3 gen_generateOIOIO_x0 gen_generateOIOIO_x2;
                                                                          let {x9 = Cons x3 x5};
                                                                          let {x10 = x0};
                                                                          let {x11 = x9};
                                                                          let {x4 = Cons x10 x11};
                                                                          return (x0, x2, x4)},
                                                                      do {let {x13 = x1};
                                                                          (x0,
                                                                           x2,
                                                                           x5) <- generateOIOIO x1 x3 gen_generateOIOIO_x0 gen_generateOIOIO_x2;
                                                                          let {x12 = Cons x2 x5};
                                                                          let {x14 = x12};
                                                                          let {x4 = Cons x13 x14};
                                                                          return (x0, x2, x4)},
                                                                      do {let {x16 = x1};
                                                                          (x0,
                                                                           x2,
                                                                           x5) <- generateOIOIO x1 x3 gen_generateOIOIO_x0 gen_generateOIOIO_x2;
                                                                          let {x15 = Cons x3 x5};
                                                                          let {x17 = x15};
                                                                          let {x4 = Cons x16 x17};
                                                                          return (x0, x2, x4)}]
generateOIOOI :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
generateOIOOI x1 x4 gen_generateOIOOI_x0 gen_generateOIOOI_x2 gen_generateOIOOI_x3 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3 = msum [do {guard (x4 == Nil);
                                                                                                                                                              x0 <- gen_generateOIOOI_x0;
                                                                                                                                                              x2 <- gen_generateOIOOI_x2;
                                                                                                                                                              x3 <- gen_generateOIOOI_x3;
                                                                                                                                                              return (x0,
                                                                                                                                                                      x2,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x7,
                                                                                                                                                               x8) <- case x4 of
                                                                                                                                                                      {Cons y7
                                                                                                                                                                            y8 -> return (y7,
                                                                                                                                                                                          y8);
                                                                                                                                                                       _ -> mzero};
                                                                                                                                                              (x0,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3;
                                                                                                                                                              guard (x7 == x0);
                                                                                                                                                              let {x6 = Cons x2 x5};
                                                                                                                                                              guard (x8 == x6);
                                                                                                                                                              return (x0,
                                                                                                                                                                      x2,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x10,
                                                                                                                                                               x11) <- case x4 of
                                                                                                                                                                       {Cons y10
                                                                                                                                                                             y11 -> return (y10,
                                                                                                                                                                                            y11);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              (x0,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3;
                                                                                                                                                              guard (x10 == x0);
                                                                                                                                                              let {x9 = Cons x3 x5};
                                                                                                                                                              guard (x11 == x9);
                                                                                                                                                              return (x0,
                                                                                                                                                                      x2,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x13,
                                                                                                                                                               x14) <- case x4 of
                                                                                                                                                                       {Cons y13
                                                                                                                                                                             y14 -> return (y13,
                                                                                                                                                                                            y14);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              guard (x13 == x1);
                                                                                                                                                              (x0,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3;
                                                                                                                                                              let {x12 = Cons x2 x5};
                                                                                                                                                              guard (x14 == x12);
                                                                                                                                                              return (x0,
                                                                                                                                                                      x2,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x16,
                                                                                                                                                               x17) <- case x4 of
                                                                                                                                                                       {Cons y16
                                                                                                                                                                             y17 -> return (y16,
                                                                                                                                                                                            y17);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              guard (x16 == x1);
                                                                                                                                                              (x0,
                                                                                                                                                               x2,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3;
                                                                                                                                                              let {x15 = Cons x3 x5};
                                                                                                                                                              guard (x17 == x15);
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
                                                                                            let {x6 = Cons x2 x5};
                                                                                            let {x7 = x0};
                                                                                            let {x8 = x6};
                                                                                            let {x4 = Cons x7 x8};
                                                                                            return (x0,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {(x0,
                                                                                             x2,
                                                                                             x3,
                                                                                             x5) <- generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3;
                                                                                            let {x9 = Cons x3 x5};
                                                                                            let {x10 = x0};
                                                                                            let {x11 = x9};
                                                                                            let {x4 = Cons x10 x11};
                                                                                            return (x0,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {let {x13 = x1};
                                                                                            (x0,
                                                                                             x2,
                                                                                             x3,
                                                                                             x5) <- generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3;
                                                                                            let {x12 = Cons x2 x5};
                                                                                            let {x14 = x12};
                                                                                            let {x4 = Cons x13 x14};
                                                                                            return (x0,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {let {x16 = x1};
                                                                                            (x0,
                                                                                             x2,
                                                                                             x3,
                                                                                             x5) <- generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3;
                                                                                            let {x15 = Cons x3 x5};
                                                                                            let {x17 = x15};
                                                                                            let {x4 = Cons x16 x17};
                                                                                            return (x0,
                                                                                                    x2,
                                                                                                    x3,
                                                                                                    x4)}]
generateOOIII :: MonadPlus m => Term -> Term -> Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term)
generateOOIII x2 x3 x4 gen_generateOOIII_x0 gen_generateOOIII_x1 gen_generateOOIIO_x0 gen_generateOOIIO_x1 = msum [do {guard (x4 == Nil);
                                                                                                                       x0 <- gen_generateOOIII_x0;
                                                                                                                       x1 <- gen_generateOOIII_x1;
                                                                                                                       return (x0,
                                                                                                                               x1)},
                                                                                                                   do {(x7,
                                                                                                                        x8) <- case x4 of
                                                                                                                               {Cons y7
                                                                                                                                     y8 -> return (y7,
                                                                                                                                                   y8);
                                                                                                                                _ -> mzero};
                                                                                                                       (x0,
                                                                                                                        x1,
                                                                                                                        x5) <- generateOOIIO x2 x3 gen_generateOOIIO_x0 gen_generateOOIIO_x1;
                                                                                                                       guard (x7 == x0);
                                                                                                                       let {x6 = Cons x2 x5};
                                                                                                                       guard (x8 == x6);
                                                                                                                       return (x0,
                                                                                                                               x1)},
                                                                                                                   do {(x10,
                                                                                                                        x11) <- case x4 of
                                                                                                                                {Cons y10
                                                                                                                                      y11 -> return (y10,
                                                                                                                                                     y11);
                                                                                                                                 _ -> mzero};
                                                                                                                       (x0,
                                                                                                                        x1,
                                                                                                                        x5) <- generateOOIIO x2 x3 gen_generateOOIIO_x0 gen_generateOOIIO_x1;
                                                                                                                       guard (x10 == x0);
                                                                                                                       let {x9 = Cons x3 x5};
                                                                                                                       guard (x11 == x9);
                                                                                                                       return (x0,
                                                                                                                               x1)},
                                                                                                                   do {(x13,
                                                                                                                        x14) <- case x4 of
                                                                                                                                {Cons y13
                                                                                                                                      y14 -> return (y13,
                                                                                                                                                     y14);
                                                                                                                                 _ -> mzero};
                                                                                                                       (x0,
                                                                                                                        x1,
                                                                                                                        x5) <- generateOOIIO x2 x3 gen_generateOOIIO_x0 gen_generateOOIIO_x1;
                                                                                                                       guard (x13 == x1);
                                                                                                                       let {x12 = Cons x2 x5};
                                                                                                                       guard (x14 == x12);
                                                                                                                       return (x0,
                                                                                                                               x1)},
                                                                                                                   do {(x16,
                                                                                                                        x17) <- case x4 of
                                                                                                                                {Cons y16
                                                                                                                                      y17 -> return (y16,
                                                                                                                                                     y17);
                                                                                                                                 _ -> mzero};
                                                                                                                       (x0,
                                                                                                                        x1,
                                                                                                                        x5) <- generateOOIIO x2 x3 gen_generateOOIIO_x0 gen_generateOOIIO_x1;
                                                                                                                       guard (x16 == x1);
                                                                                                                       let {x15 = Cons x3 x5};
                                                                                                                       guard (x17 == x15);
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
                                                                          let {x6 = Cons x2 x5};
                                                                          let {x7 = x0};
                                                                          let {x8 = x6};
                                                                          let {x4 = Cons x7 x8};
                                                                          return (x0, x1, x4)},
                                                                      do {(x0,
                                                                           x1,
                                                                           x5) <- generateOOIIO x2 x3 gen_generateOOIIO_x0 gen_generateOOIIO_x1;
                                                                          let {x9 = Cons x3 x5};
                                                                          let {x10 = x0};
                                                                          let {x11 = x9};
                                                                          let {x4 = Cons x10 x11};
                                                                          return (x0, x1, x4)},
                                                                      do {(x0,
                                                                           x1,
                                                                           x5) <- generateOOIIO x2 x3 gen_generateOOIIO_x0 gen_generateOOIIO_x1;
                                                                          let {x12 = Cons x2 x5};
                                                                          let {x13 = x1};
                                                                          let {x14 = x12};
                                                                          let {x4 = Cons x13 x14};
                                                                          return (x0, x1, x4)},
                                                                      do {(x0,
                                                                           x1,
                                                                           x5) <- generateOOIIO x2 x3 gen_generateOOIIO_x0 gen_generateOOIIO_x1;
                                                                          let {x15 = Cons x3 x5};
                                                                          let {x16 = x1};
                                                                          let {x17 = x15};
                                                                          let {x4 = Cons x16 x17};
                                                                          return (x0, x1, x4)}]
generateOOIOI :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
generateOOIOI x2 x4 gen_generateOOIOI_x0 gen_generateOOIOI_x1 gen_generateOOIOI_x3 gen_generateOOIOO_x0 gen_generateOOIOO_x1 gen_generateOOIOO_x3 = msum [do {guard (x4 == Nil);
                                                                                                                                                              x0 <- gen_generateOOIOI_x0;
                                                                                                                                                              x1 <- gen_generateOOIOI_x1;
                                                                                                                                                              x3 <- gen_generateOOIOI_x3;
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x7,
                                                                                                                                                               x8) <- case x4 of
                                                                                                                                                                      {Cons y7
                                                                                                                                                                            y8 -> return (y7,
                                                                                                                                                                                          y8);
                                                                                                                                                                       _ -> mzero};
                                                                                                                                                              (x0,
                                                                                                                                                               x1,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateOOIOO x2 gen_generateOOIOO_x0 gen_generateOOIOO_x1 gen_generateOOIOO_x3;
                                                                                                                                                              guard (x7 == x0);
                                                                                                                                                              let {x6 = Cons x2 x5};
                                                                                                                                                              guard (x8 == x6);
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x10,
                                                                                                                                                               x11) <- case x4 of
                                                                                                                                                                       {Cons y10
                                                                                                                                                                             y11 -> return (y10,
                                                                                                                                                                                            y11);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              (x0,
                                                                                                                                                               x1,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateOOIOO x2 gen_generateOOIOO_x0 gen_generateOOIOO_x1 gen_generateOOIOO_x3;
                                                                                                                                                              guard (x10 == x0);
                                                                                                                                                              let {x9 = Cons x3 x5};
                                                                                                                                                              guard (x11 == x9);
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x13,
                                                                                                                                                               x14) <- case x4 of
                                                                                                                                                                       {Cons y13
                                                                                                                                                                             y14 -> return (y13,
                                                                                                                                                                                            y14);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              (x0,
                                                                                                                                                               x1,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateOOIOO x2 gen_generateOOIOO_x0 gen_generateOOIOO_x1 gen_generateOOIOO_x3;
                                                                                                                                                              guard (x13 == x1);
                                                                                                                                                              let {x12 = Cons x2 x5};
                                                                                                                                                              guard (x14 == x12);
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1,
                                                                                                                                                                      x3)},
                                                                                                                                                          do {(x16,
                                                                                                                                                               x17) <- case x4 of
                                                                                                                                                                       {Cons y16
                                                                                                                                                                             y17 -> return (y16,
                                                                                                                                                                                            y17);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              (x0,
                                                                                                                                                               x1,
                                                                                                                                                               x3,
                                                                                                                                                               x5) <- generateOOIOO x2 gen_generateOOIOO_x0 gen_generateOOIOO_x1 gen_generateOOIOO_x3;
                                                                                                                                                              guard (x16 == x1);
                                                                                                                                                              let {x15 = Cons x3 x5};
                                                                                                                                                              guard (x17 == x15);
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
                                                                                            let {x6 = Cons x2 x5};
                                                                                            let {x7 = x0};
                                                                                            let {x8 = x6};
                                                                                            let {x4 = Cons x7 x8};
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {(x0,
                                                                                             x1,
                                                                                             x3,
                                                                                             x5) <- generateOOIOO x2 gen_generateOOIOO_x0 gen_generateOOIOO_x1 gen_generateOOIOO_x3;
                                                                                            let {x9 = Cons x3 x5};
                                                                                            let {x10 = x0};
                                                                                            let {x11 = x9};
                                                                                            let {x4 = Cons x10 x11};
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {(x0,
                                                                                             x1,
                                                                                             x3,
                                                                                             x5) <- generateOOIOO x2 gen_generateOOIOO_x0 gen_generateOOIOO_x1 gen_generateOOIOO_x3;
                                                                                            let {x12 = Cons x2 x5};
                                                                                            let {x13 = x1};
                                                                                            let {x14 = x12};
                                                                                            let {x4 = Cons x13 x14};
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x3,
                                                                                                    x4)},
                                                                                        do {(x0,
                                                                                             x1,
                                                                                             x3,
                                                                                             x5) <- generateOOIOO x2 gen_generateOOIOO_x0 gen_generateOOIOO_x1 gen_generateOOIOO_x3;
                                                                                            let {x15 = Cons x3 x5};
                                                                                            let {x16 = x1};
                                                                                            let {x17 = x15};
                                                                                            let {x4 = Cons x16 x17};
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x3,
                                                                                                    x4)}]
generateOOOII :: MonadPlus m => Term -> Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
generateOOOII x3 x4 gen_generateOOOII_x0 gen_generateOOOII_x1 gen_generateOOOII_x2 gen_generateOOOIO_x0 gen_generateOOOIO_x1 gen_generateOOOIO_x2 = msum [do {guard (x4 == Nil);
                                                                                                                                                              x0 <- gen_generateOOOII_x0;
                                                                                                                                                              x1 <- gen_generateOOOII_x1;
                                                                                                                                                              x2 <- gen_generateOOOII_x2;
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1,
                                                                                                                                                                      x2)},
                                                                                                                                                          do {(x7,
                                                                                                                                                               x8) <- case x4 of
                                                                                                                                                                      {Cons y7
                                                                                                                                                                            y8 -> return (y7,
                                                                                                                                                                                          y8);
                                                                                                                                                                       _ -> mzero};
                                                                                                                                                              (x0,
                                                                                                                                                               x1,
                                                                                                                                                               x2,
                                                                                                                                                               x5) <- generateOOOIO x3 gen_generateOOOIO_x0 gen_generateOOOIO_x1 gen_generateOOOIO_x2;
                                                                                                                                                              guard (x7 == x0);
                                                                                                                                                              let {x6 = Cons x2 x5};
                                                                                                                                                              guard (x8 == x6);
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1,
                                                                                                                                                                      x2)},
                                                                                                                                                          do {(x10,
                                                                                                                                                               x11) <- case x4 of
                                                                                                                                                                       {Cons y10
                                                                                                                                                                             y11 -> return (y10,
                                                                                                                                                                                            y11);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              (x0,
                                                                                                                                                               x1,
                                                                                                                                                               x2,
                                                                                                                                                               x5) <- generateOOOIO x3 gen_generateOOOIO_x0 gen_generateOOOIO_x1 gen_generateOOOIO_x2;
                                                                                                                                                              guard (x10 == x0);
                                                                                                                                                              let {x9 = Cons x3 x5};
                                                                                                                                                              guard (x11 == x9);
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1,
                                                                                                                                                                      x2)},
                                                                                                                                                          do {(x13,
                                                                                                                                                               x14) <- case x4 of
                                                                                                                                                                       {Cons y13
                                                                                                                                                                             y14 -> return (y13,
                                                                                                                                                                                            y14);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              (x0,
                                                                                                                                                               x1,
                                                                                                                                                               x2,
                                                                                                                                                               x5) <- generateOOOIO x3 gen_generateOOOIO_x0 gen_generateOOOIO_x1 gen_generateOOOIO_x2;
                                                                                                                                                              guard (x13 == x1);
                                                                                                                                                              let {x12 = Cons x2 x5};
                                                                                                                                                              guard (x14 == x12);
                                                                                                                                                              return (x0,
                                                                                                                                                                      x1,
                                                                                                                                                                      x2)},
                                                                                                                                                          do {(x16,
                                                                                                                                                               x17) <- case x4 of
                                                                                                                                                                       {Cons y16
                                                                                                                                                                             y17 -> return (y16,
                                                                                                                                                                                            y17);
                                                                                                                                                                        _ -> mzero};
                                                                                                                                                              (x0,
                                                                                                                                                               x1,
                                                                                                                                                               x2,
                                                                                                                                                               x5) <- generateOOOIO x3 gen_generateOOOIO_x0 gen_generateOOOIO_x1 gen_generateOOOIO_x2;
                                                                                                                                                              guard (x16 == x1);
                                                                                                                                                              let {x15 = Cons x3 x5};
                                                                                                                                                              guard (x17 == x15);
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
                                                                                            let {x6 = Cons x2 x5};
                                                                                            let {x7 = x0};
                                                                                            let {x8 = x6};
                                                                                            let {x4 = Cons x7 x8};
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x2,
                                                                                                    x4)},
                                                                                        do {(x0,
                                                                                             x1,
                                                                                             x2,
                                                                                             x5) <- generateOOOIO x3 gen_generateOOOIO_x0 gen_generateOOOIO_x1 gen_generateOOOIO_x2;
                                                                                            let {x9 = Cons x3 x5};
                                                                                            let {x10 = x0};
                                                                                            let {x11 = x9};
                                                                                            let {x4 = Cons x10 x11};
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x2,
                                                                                                    x4)},
                                                                                        do {(x0,
                                                                                             x1,
                                                                                             x2,
                                                                                             x5) <- generateOOOIO x3 gen_generateOOOIO_x0 gen_generateOOOIO_x1 gen_generateOOOIO_x2;
                                                                                            let {x12 = Cons x2 x5};
                                                                                            let {x13 = x1};
                                                                                            let {x14 = x12};
                                                                                            let {x4 = Cons x13 x14};
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x2,
                                                                                                    x4)},
                                                                                        do {(x0,
                                                                                             x1,
                                                                                             x2,
                                                                                             x5) <- generateOOOIO x3 gen_generateOOOIO_x0 gen_generateOOOIO_x1 gen_generateOOOIO_x2;
                                                                                            let {x15 = Cons x3 x5};
                                                                                            let {x16 = x1};
                                                                                            let {x17 = x15};
                                                                                            let {x4 = Cons x16 x17};
                                                                                            return (x0,
                                                                                                    x1,
                                                                                                    x2,
                                                                                                    x4)}]
generateOOOOI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term, Term)
generateOOOOI x4 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3 gen_generateOOOOI_x0 gen_generateOOOOI_x1 gen_generateOOOOI_x2 gen_generateOOOOI_x3 = msum [do {guard (x4 == Nil);
                                                                                                                                                                                                                                               x0 <- gen_generateOOOOI_x0;
                                                                                                                                                                                                                                               x1 <- gen_generateOOOOI_x1;
                                                                                                                                                                                                                                               x2 <- gen_generateOOOOI_x2;
                                                                                                                                                                                                                                               x3 <- gen_generateOOOOI_x3;
                                                                                                                                                                                                                                               return (x0,
                                                                                                                                                                                                                                                       x1,
                                                                                                                                                                                                                                                       x2,
                                                                                                                                                                                                                                                       x3)},
                                                                                                                                                                                                                                           do {(x7,
                                                                                                                                                                                                                                                x8) <- case x4 of
                                                                                                                                                                                                                                                       {Cons y7
                                                                                                                                                                                                                                                             y8 -> return (y7,
                                                                                                                                                                                                                                                                           y8);
                                                                                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                                                                               let {x0 = x7};
                                                                                                                                                                                                                                               (x1,
                                                                                                                                                                                                                                                x2,
                                                                                                                                                                                                                                                x3,
                                                                                                                                                                                                                                                x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                                                                                                                                                                               let {x6 = Cons x2 x5};
                                                                                                                                                                                                                                               guard (x8 == x6);
                                                                                                                                                                                                                                               return (x0,
                                                                                                                                                                                                                                                       x1,
                                                                                                                                                                                                                                                       x2,
                                                                                                                                                                                                                                                       x3)},
                                                                                                                                                                                                                                           do {(x10,
                                                                                                                                                                                                                                                x11) <- case x4 of
                                                                                                                                                                                                                                                        {Cons y10
                                                                                                                                                                                                                                                              y11 -> return (y10,
                                                                                                                                                                                                                                                                             y11);
                                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                               let {x0 = x10};
                                                                                                                                                                                                                                               (x1,
                                                                                                                                                                                                                                                x2,
                                                                                                                                                                                                                                                x3,
                                                                                                                                                                                                                                                x5) <- generateIOOOO x0 gen_generateIOOOO_x1 gen_generateIOOOO_x2 gen_generateIOOOO_x3;
                                                                                                                                                                                                                                               let {x9 = Cons x3 x5};
                                                                                                                                                                                                                                               guard (x11 == x9);
                                                                                                                                                                                                                                               return (x0,
                                                                                                                                                                                                                                                       x1,
                                                                                                                                                                                                                                                       x2,
                                                                                                                                                                                                                                                       x3)},
                                                                                                                                                                                                                                           do {(x13,
                                                                                                                                                                                                                                                x14) <- case x4 of
                                                                                                                                                                                                                                                        {Cons y13
                                                                                                                                                                                                                                                              y14 -> return (y13,
                                                                                                                                                                                                                                                                             y14);
                                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                               let {x1 = x13};
                                                                                                                                                                                                                                               (x0,
                                                                                                                                                                                                                                                x2,
                                                                                                                                                                                                                                                x3,
                                                                                                                                                                                                                                                x5) <- generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3;
                                                                                                                                                                                                                                               let {x12 = Cons x2 x5};
                                                                                                                                                                                                                                               guard (x14 == x12);
                                                                                                                                                                                                                                               return (x0,
                                                                                                                                                                                                                                                       x1,
                                                                                                                                                                                                                                                       x2,
                                                                                                                                                                                                                                                       x3)},
                                                                                                                                                                                                                                           do {(x16,
                                                                                                                                                                                                                                                x17) <- case x4 of
                                                                                                                                                                                                                                                        {Cons y16
                                                                                                                                                                                                                                                              y17 -> return (y16,
                                                                                                                                                                                                                                                                             y17);
                                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                               let {x1 = x16};
                                                                                                                                                                                                                                               (x0,
                                                                                                                                                                                                                                                x2,
                                                                                                                                                                                                                                                x3,
                                                                                                                                                                                                                                                x5) <- generateOIOOO x1 gen_generateOIOOO_x0 gen_generateOIOOO_x2 gen_generateOIOOO_x3;
                                                                                                                                                                                                                                               let {x15 = Cons x3 x5};
                                                                                                                                                                                                                                               guard (x17 == x15);
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
                                                                                                              let {x6 = Cons x2 x5};
                                                                                                              let {x7 = x0};
                                                                                                              let {x8 = x6};
                                                                                                              let {x4 = Cons x7 x8};
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
                                                                                                              let {x9 = Cons x3 x5};
                                                                                                              let {x10 = x0};
                                                                                                              let {x11 = x9};
                                                                                                              let {x4 = Cons x10 x11};
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
                                                                                                              let {x12 = Cons x2 x5};
                                                                                                              let {x13 = x1};
                                                                                                              let {x14 = x12};
                                                                                                              let {x4 = Cons x13 x14};
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
                                                                                                              let {x15 = Cons x3 x5};
                                                                                                              let {x16 = x1};
                                                                                                              let {x17 = x15};
                                                                                                              let {x4 = Cons x16 x17};
                                                                                                              return (x0,
                                                                                                                      x1,
                                                                                                                      x2,
                                                                                                                      x3,
                                                                                                                      x4)}]