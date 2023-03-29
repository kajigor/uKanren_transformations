data Term = O | S Term deriving (Show, Eq)
addoOII x1 x2 = msum [do {let {x0 = O};
                          guard (x2 == x1);
                          return x0},
                      do {let {x4 = S x1};
                          x3 <- addoOII x4 x2;
                          let {x0 = S x3};
                          return x0}]
addo x1 x2 = addoOII x1 x2
