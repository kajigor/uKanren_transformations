data Term = O | S Term deriving (Show, Eq)
addoOIO x1 = msum [do {let {x0 = O};
                       let {x2 = x1};
                       return (x0, x2)},
                   do {let {x4 = S x1};
                       (x3, x2) <- addoOIO x4;
                       let {x0 = S x3};
                       return (x0, x2)}]
addo x1 = addoOIO x1
