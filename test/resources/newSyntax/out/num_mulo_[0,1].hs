data Term = O | S Term deriving (Show, Eq)
muloIIO x0 x1 = msum [do {case x0 of
                              O -> do return ()
                              _ -> mzero;
                          let {x2 = O};
                          return x2},
                      do {x3 <- case x0 of
                                    S y3 -> do return y3
                                    _ -> mzero;
                          x4 <- muloIIO x3 x1;
                          x2 <- addoIIO x1 x4;
                          return x2}]
addoIIO x0 x1 = msum [do {case x0 of
                              O -> do return ()
                              _ -> mzero;
                          let {x2 = x1};
                          return x2},
                      do {x3 <- case x0 of
                                    S y3 -> do return y3
                                    _ -> mzero;
                          let {x4 = S x1};
                          x2 <- addoIIO x3 x4;
                          return x2}]
mulo x0 x1 = muloIIO x0 x1
