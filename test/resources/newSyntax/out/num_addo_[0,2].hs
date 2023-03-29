data Term = O | S Term deriving (Show, Eq)
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
addo x0 x2 = addoIOI x0 x2
