data Term
    = O
    | S Term
    deriving (Show, Eq)
muloOOI x2 = msum [do {let {x0 = O};
                       case x2 of
                       {O -> return (); _ -> mzero};
                       return (x0, x1)},
                   do {(x1, x4) <- addoOOI x2;
                       x3 <- muloOII x1 x4;
                       let {x0 = S x3};
                       return (x0, x1)}]
addoOOI x2 = msum [do {let {x0 = O};
                       let {x1 = x2};
                       return (x0, x1)},
                   do {x4 <- case x2 of
                             {S y4 -> return y4; _ -> mzero};
                       (x3, x1) <- addoOOI x4;
                       let {x0 = S x3};
                       return (x0, x1)}]
muloOII x1 x2 = msum [do {let {x0 = O};
                          case x2 of
                          {O -> return (); _ -> mzero};
                          return x0},
                      do {x4 <- addoIOI x1 x2;
                          x3 <- muloOII x1 x4;
                          let {x0 = S x3};
                          return x0}]
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
mulo x2 = muloOOI x2
