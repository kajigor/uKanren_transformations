data Term
    = O
    | S Term
    deriving (Show, Eq)
addoOII x1 x2 = msum [do {let {x0 = O};
                          guard (x2 == x1);
                          return x0},
                      do {x4 <- case x2 of
                                {S y4 -> return y4; _ -> mzero};
                          x3 <- addoOII x1 x4;
                          let {x0 = S x3};
                          return x0}]
addo x1 x2 = addoOII x1 x2
