data Term = O | S Term deriving (Show, Eq)
addoOOI x2 = msum [do {let {x0 = O};
                       let {x1 = x2};
                       return (x0, x1)},
                   do {(x3, x4) <- addoOOI x2;
                       let {x0 = S x3};
                       x1 <- case x4 of
                                 S y1 -> do return y1
                                 _ -> mzero;
                       return (x0, x1)}]
addo x2 = addoOOI x2
