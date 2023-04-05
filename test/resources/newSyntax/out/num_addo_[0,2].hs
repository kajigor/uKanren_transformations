data Term
    = O
    | S Term
    deriving (Show, Eq)
addoIOI x0 x2 = msum [do {case x0 of
                          {O -> return (); _ -> mzero};
                          let {x1 = x2};
                          return x1},
                      do {x3 <- case x0 of
                                {S y3 -> return y3; _ -> mzero};
                          x4 <- case x2 of
                                {S y4 -> return y4; _ -> mzero};
                          x1 <- addoIOI x3 x4;
                          return x1}]
addo x0 x2 = addoIOI x0 x2
