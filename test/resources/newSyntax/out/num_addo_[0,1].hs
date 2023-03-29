data Term = O | S Term deriving (Show, Eq)
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
addo x0 x1 = addoIIO x0 x1
