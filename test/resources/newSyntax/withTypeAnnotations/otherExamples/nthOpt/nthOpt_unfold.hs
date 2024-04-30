module NthOpt_unfold where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | None
    | Some Term
    deriving (Show, Eq)
nthOptII :: MonadPlus m => Term -> Term -> m ()
nthOptII x0 x1 = msum [do {guard (x1 == None);
                           guard (x0 == Nil);
                           return ()},
                       do {let {x7 = Nil};
                           guard (x1 == None);
                           (x2, x8) <- case x0 of
                                       {Cons y2 y8 -> return (y2, y8); _ -> mzero};
                           guard (x8 == x7);
                           return ()},
                       do {let {x10 = Nil};
                           guard (x1 == None);
                           (x2, x11) <- case x0 of
                                        {Cons y2 y11 -> return (y2, y11); _ -> mzero};
                           let {x9 = x11};
                           x3 <- case x9 of
                                 {Cons y3 y10 -> do {guard (x10 == y10); return y3}; _ -> mzero};
                           return ()},
                       do {let {x14 = Nil};
                           guard (x1 == None);
                           (x2, x15) <- case x0 of
                                        {Cons y2 y15 -> return (y2, y15); _ -> mzero};
                           let {x12 = x15};
                           (x3, x13) <- case x12 of
                                        {Cons y3 y13 -> return (y3, y13); _ -> mzero};
                           x4 <- case x13 of
                                 {Cons y4 y14 -> do {guard (x14 == y14); return y4}; _ -> mzero};
                           return ()},
                       do {x5 <- case x1 of
                                 {Some y5 -> return y5; _ -> mzero};
                           (x2, x19) <- case x0 of
                                        {Cons y2 y19 -> return (y2, y19); _ -> mzero};
                           let {x16 = x19};
                           (x3, x17) <- case x16 of
                                        {Cons y3 y17 -> return (y3, y17); _ -> mzero};
                           (x4, x18) <- case x17 of
                                        {Cons y4 y18 -> return (y4, y18); _ -> mzero};
                           x6 <- case x18 of
                                 {Cons y5 y6 -> do {guard (x5 == y5); return y6}; _ -> mzero};
                           return ()}]
nthOptIO :: MonadPlus m => Term -> m Term
nthOptIO x0 = msum [do {let {x1 = None};
                        guard (x0 == Nil);
                        return x1},
                    do {let {x1 = None};
                        let {x7 = Nil};
                        (x2, x8) <- case x0 of
                                    {Cons y2 y8 -> return (y2, y8); _ -> mzero};
                        guard (x8 == x7);
                        return x1},
                    do {let {x1 = None};
                        let {x10 = Nil};
                        (x2, x11) <- case x0 of
                                     {Cons y2 y11 -> return (y2, y11); _ -> mzero};
                        let {x9 = x11};
                        x3 <- case x9 of
                              {Cons y3 y10 -> do {guard (x10 == y10); return y3}; _ -> mzero};
                        return x1},
                    do {let {x1 = None};
                        let {x14 = Nil};
                        (x2, x15) <- case x0 of
                                     {Cons y2 y15 -> return (y2, y15); _ -> mzero};
                        let {x12 = x15};
                        (x3, x13) <- case x12 of
                                     {Cons y3 y13 -> return (y3, y13); _ -> mzero};
                        x4 <- case x13 of
                              {Cons y4 y14 -> do {guard (x14 == y14); return y4}; _ -> mzero};
                        return x1},
                    do {(x2, x19) <- case x0 of
                                     {Cons y2 y19 -> return (y2, y19); _ -> mzero};
                        let {x16 = x19};
                        (x3, x17) <- case x16 of
                                     {Cons y3 y17 -> return (y3, y17); _ -> mzero};
                        (x4, x18) <- case x17 of
                                     {Cons y4 y18 -> return (y4, y18); _ -> mzero};
                        (x5, x6) <- case x18 of
                                    {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                        let {x1 = Some x5};
                        return x1}]
nthOptOI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term
nthOptOI x1 gen_nthOptOI_x12 gen_nthOptOI_x16 gen_nthOptOI_x2 gen_nthOptOI_x9 = msum [do {let {x0 = Nil};
                                                                                          guard (x1 == None);
                                                                                          return x0},
                                                                                      do {let {x7 = Nil};
                                                                                          guard (x1 == None);
                                                                                          let {x8 = x7};
                                                                                          (x0,
                                                                                           x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                      let {x0 = Cons x2 x8};
                                                                                                      return (x0,
                                                                                                              x2)};
                                                                                          return x0},
                                                                                      do {let {x10 = Nil};
                                                                                          guard (x1 == None);
                                                                                          (x11,
                                                                                           x9) <- do {x9 <- gen_nthOptOI_x9;
                                                                                                      return (x9,
                                                                                                              x9)};
                                                                                          x3 <- case x9 of
                                                                                                {Cons y3
                                                                                                      y10 -> do {guard (x10 == y10);
                                                                                                                 return y3};
                                                                                                 _ -> mzero};
                                                                                          (x0,
                                                                                           x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                      let {x0 = Cons x2 x11};
                                                                                                      return (x0,
                                                                                                              x2)};
                                                                                          return x0},
                                                                                      do {let {x14 = Nil};
                                                                                          guard (x1 == None);
                                                                                          (x15,
                                                                                           x12) <- do {x12 <- gen_nthOptOI_x12;
                                                                                                       return (x12,
                                                                                                               x12)};
                                                                                          (x3,
                                                                                           x13) <- case x12 of
                                                                                                   {Cons y3
                                                                                                         y13 -> return (y3,
                                                                                                                        y13);
                                                                                                    _ -> mzero};
                                                                                          x4 <- case x13 of
                                                                                                {Cons y4
                                                                                                      y14 -> do {guard (x14 == y14);
                                                                                                                 return y4};
                                                                                                 _ -> mzero};
                                                                                          (x0,
                                                                                           x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                      let {x0 = Cons x2 x15};
                                                                                                      return (x0,
                                                                                                              x2)};
                                                                                          return x0},
                                                                                      do {x5 <- case x1 of
                                                                                                {Some y5 -> return y5;
                                                                                                 _ -> mzero};
                                                                                          (x19,
                                                                                           x16) <- do {x16 <- gen_nthOptOI_x16;
                                                                                                       return (x16,
                                                                                                               x16)};
                                                                                          (x3,
                                                                                           x17) <- case x16 of
                                                                                                   {Cons y3
                                                                                                         y17 -> return (y3,
                                                                                                                        y17);
                                                                                                    _ -> mzero};
                                                                                          (x4,
                                                                                           x18) <- case x17 of
                                                                                                   {Cons y4
                                                                                                         y18 -> return (y4,
                                                                                                                        y18);
                                                                                                    _ -> mzero};
                                                                                          x6 <- case x18 of
                                                                                                {Cons y5
                                                                                                      y6 -> do {guard (x5 == y5);
                                                                                                                return y6};
                                                                                                 _ -> mzero};
                                                                                          (x0,
                                                                                           x2) <- do {x2 <- gen_nthOptOI_x2;
                                                                                                      let {x0 = Cons x2 x19};
                                                                                                      return (x0,
                                                                                                              x2)};
                                                                                          return x0}]
nthOptOO :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m (Term, Term)
nthOptOO gen_nthOptOO_x12 gen_nthOptOO_x16 gen_nthOptOO_x2 gen_nthOptOO_x9 = msum [do {let {x1 = None};
                                                                                       let {x0 = Nil};
                                                                                       return (x0,
                                                                                               x1)},
                                                                                   do {let {x1 = None};
                                                                                       let {x7 = Nil};
                                                                                       let {x8 = x7};
                                                                                       (x0,
                                                                                        x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                   let {x0 = Cons x2 x8};
                                                                                                   return (x0,
                                                                                                           x2)};
                                                                                       return (x0,
                                                                                               x1)},
                                                                                   do {let {x1 = None};
                                                                                       let {x10 = Nil};
                                                                                       (x11,
                                                                                        x9) <- do {x9 <- gen_nthOptOO_x9;
                                                                                                   return (x9,
                                                                                                           x9)};
                                                                                       x3 <- case x9 of
                                                                                             {Cons y3
                                                                                                   y10 -> do {guard (x10 == y10);
                                                                                                              return y3};
                                                                                              _ -> mzero};
                                                                                       (x0,
                                                                                        x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                   let {x0 = Cons x2 x11};
                                                                                                   return (x0,
                                                                                                           x2)};
                                                                                       return (x0,
                                                                                               x1)},
                                                                                   do {let {x1 = None};
                                                                                       let {x14 = Nil};
                                                                                       (x15,
                                                                                        x12) <- do {x12 <- gen_nthOptOO_x12;
                                                                                                    return (x12,
                                                                                                            x12)};
                                                                                       (x3,
                                                                                        x13) <- case x12 of
                                                                                                {Cons y3
                                                                                                      y13 -> return (y3,
                                                                                                                     y13);
                                                                                                 _ -> mzero};
                                                                                       x4 <- case x13 of
                                                                                             {Cons y4
                                                                                                   y14 -> do {guard (x14 == y14);
                                                                                                              return y4};
                                                                                              _ -> mzero};
                                                                                       (x0,
                                                                                        x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                   let {x0 = Cons x2 x15};
                                                                                                   return (x0,
                                                                                                           x2)};
                                                                                       return (x0,
                                                                                               x1)},
                                                                                   do {(x19,
                                                                                        x16) <- do {x16 <- gen_nthOptOO_x16;
                                                                                                    return (x16,
                                                                                                            x16)};
                                                                                       (x3,
                                                                                        x17) <- case x16 of
                                                                                                {Cons y3
                                                                                                      y17 -> return (y3,
                                                                                                                     y17);
                                                                                                 _ -> mzero};
                                                                                       (x4,
                                                                                        x18) <- case x17 of
                                                                                                {Cons y4
                                                                                                      y18 -> return (y4,
                                                                                                                     y18);
                                                                                                 _ -> mzero};
                                                                                       (x5,
                                                                                        x6) <- case x18 of
                                                                                               {Cons y5
                                                                                                     y6 -> return (y5,
                                                                                                                   y6);
                                                                                                _ -> mzero};
                                                                                       let {x1 = Some x5};
                                                                                       (x0,
                                                                                        x2) <- do {x2 <- gen_nthOptOO_x2;
                                                                                                   let {x0 = Cons x2 x19};
                                                                                                   return (x0,
                                                                                                           x2)};
                                                                                       return (x0,
                                                                                               x1)}]