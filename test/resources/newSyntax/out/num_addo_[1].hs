data Term
    = O
    | S Term
    deriving (Show, Eq)
addoOIO x1 = msum [do {let {x0 = O};
                       let {x2 = x1};
                       return (x0, x2)},
                   do {(x3, x4) <- addoOIO x1;
                       let {x0 = S x3};
                       let {x2 = S x4};
                       return (x0, x2)}]
addo x1 = addoOIO x1
