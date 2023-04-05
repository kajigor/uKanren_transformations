data Term
    = O
    | S Term
    deriving (Show, Eq)
addoIII x0 x1 x2 = msum [do {case x0 of
                             {O -> return (); _ -> mzero};
                             guard (x2 == x1);
                             return ()},
                         do {x3 <- case x0 of
                                   {S y3 -> return y3; _ -> mzero};
                             x4 <- case x2 of
                                   {S y4 -> return y4; _ -> mzero};
                             addoIII x3 x1 x4;
                             return ()}]
addo x0 x1 x2 = addoIII x0 x1 x2
