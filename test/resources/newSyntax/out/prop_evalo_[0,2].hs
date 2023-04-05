data Term
    = Conj Term Term
    | Disj Term Term
    | Neg Term
    | Var Term
    | Trueo
    | Falso
    | Zero
    | Cons Term Term
    | Succ Term
    deriving (Show, Eq)
evaloIOI x0 x2 = msum [do {(x5, x6) <- andoOOI x2;
                           x3 <- Immature (evaloIOI x0 x5);
                           x4 <- Immature (evaloIOI x0 x6);
                           let {x1 = Conj x3 x4};
                           return x1},
                       do {(x5, x6) <- oroOOI x2;
                           x3 <- Immature (evaloIOI x0 x5);
                           x4 <- Immature (evaloIOI x0 x6);
                           let {x1 = Disj x3 x4};
                           return x1},
                       do {x5 <- notoOI x2;
                           x3 <- Immature (evaloIOI x0 x5);
                           let {x1 = Neg x3};
                           return x1},
                       do {x7 <- elemoOII x0 x2; let {x1 = Var x7}; return x1}]
andoOOI x2 = msum [do {let {x0 = Trueo};
                       let {x1 = Trueo};
                       case x2 of
                       {Trueo -> return (); _ -> mzero};
                       return (x0, x1)},
                   do {let {x0 = Falso};
                       let {x1 = Trueo};
                       case x2 of
                       {Falso -> return (); _ -> mzero};
                       return (x0, x1)},
                   do {let {x0 = Trueo};
                       let {x1 = Falso};
                       case x2 of
                       {Falso -> return (); _ -> mzero};
                       return (x0, x1)},
                   do {let {x0 = Falso};
                       let {x1 = Falso};
                       case x2 of
                       {Falso -> return (); _ -> mzero};
                       return (x0, x1)}]
oroOOI x2 = msum [do {let {x0 = Trueo};
                      let {x1 = Trueo};
                      case x2 of
                      {Trueo -> return (); _ -> mzero};
                      return (x0, x1)},
                  do {let {x0 = Falso};
                      let {x1 = Trueo};
                      case x2 of
                      {Trueo -> return (); _ -> mzero};
                      return (x0, x1)},
                  do {let {x0 = Trueo};
                      let {x1 = Falso};
                      case x2 of
                      {Trueo -> return (); _ -> mzero};
                      return (x0, x1)},
                  do {let {x0 = Falso};
                      let {x1 = Falso};
                      case x2 of
                      {Falso -> return (); _ -> mzero};
                      return (x0, x1)}]
notoOI x1 = msum [do {let {x0 = Trueo};
                      case x1 of
                      {Falso -> return (); _ -> mzero};
                      return x0},
                  do {let {x0 = Falso};
                      case x1 of
                      {Trueo -> return (); _ -> mzero};
                      return x0}]
elemoOII x1 x2 = msum [do {let {x0 = Zero};
                           (x3, x4) <- case x1 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           guard (x2 == x3);
                           return x0},
                       do {(x3, x4) <- case x1 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           x5 <- elemoOII x4 x2;
                           let {x0 = Succ x5};
                           return x0}]
evalo x0 x2 = evaloIOI x0 x2
