data Term
    = O
    | S Term
    deriving (Show, Eq)
addoIIO x0 x1 = msum [do {case x0 of
                          {O -> return (); _ -> mzero};
                          let {x2 = x1};
                          return x2},
                      do {x3 <- case x0 of
                                {S y3 -> return y3; _ -> mzero};
                          x4 <- addoIIO x3 x1;
                          let {x2 = S x4};
                          return x2}]
addo x0 x1 = addoIIO x0 x1
