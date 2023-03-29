data Term = O | S Term deriving (Show, Eq)
muloOOI x2 = msum [do {let {x0 = O};
                       case x2 of
                           O -> do return ()
                           _ -> mzero;
                       return (x0, x1)},
                   do {(x1, x4) <- addoOOI x2;
                       x3 <- muloOII x1 x4;
                       let {x0 = S x3};
                       return (x0, x1)}]
addoOOI x2 = msum [do {let {x0 = O};
                       let {x1 = x2};
                       return (x0, x1)},
                   do {(x3, x4) <- addoOOI x2;
                       let {x0 = S x3};
                       x1 <- case x4 of
                                 S y1 -> do return y1
                                 _ -> mzero;
                       return (x0, x1)}]
muloOII x1 x2 = msum [do {let {x0 = O};
                          case x2 of
                              O -> do return ()
                              _ -> mzero;
                          return x0},
                      do {x4 <- addoIOI x1 x2;
                          x3 <- muloOII x1 x4;
                          let {x0 = S x3};
                          return x0}]
addoIOI x0 x2 = msum [do {case x0 of
                              O -> do return ()
                              _ -> mzero;
                          let {x1 = x2};
                          return x1},
                      do {x3 <- case x0 of
                                    S y3 -> do return y3
                                    _ -> mzero;
                          x4 <- addoIOI x3 x2;
                          x1 <- case x4 of
                                    S y1 -> do return y1
                                    _ -> mzero;
                          return x1}]
mulo x2 = muloOOI x2
