data Term
    = O
    | S Term
    deriving (Show, Eq)
addoOOI x2 = msum [do {let {x0 = O};
                       let {x1 = x2};
                       return (x0, x1)},
                   do {x4 <- case x2 of
                             {S y4 -> return y4; _ -> mzero};
                       (x3, x1) <- addoOOI x4;
                       let {x0 = S x3};
                       return (x0, x1)}]
addo x2 = addoOOI x2
