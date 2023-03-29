data Term
    = Nil
    | Cons Term Term
    | State Term Term
    | Side Term Term Term Term
    | Trueo
    | Falso
    | Empty
    | Goat
    | Wolf
    | Cabbage
    deriving (Show, Eq)
evalIOI x0 x2 = msum [do {let {x1 = Nil};
                          guard (x0 == x2);
                          return x1},
                      do {(x3, x5) <- stepIOO x0;
                          x4 <- evalIOI x5 x2;
                          let {x1 = Cons x3 x4};
                          return x1}]
stepIOO x0 = msum [do {(x3, x4) <- case x0 of
                                       State y3 y4 -> do return (y3, y4)
                                       _ -> mzero;
                       (x1, x2) <- step0OOII x3 x4;
                       return (x1, x2)}]
step0OOII x2 x3 = msum [do {isManI x3;
                            noManI x4;
                            (x1, x2) <- step'IIOO x3 x4;
                            return (x0, x1)},
                        do {isManI x4;
                            noManI x3;
                            (x1, x2) <- step02OOII x3 x4;
                            return (x0, x1)}]
isManI x0 = msum [do {(x1, x2, x3, x33) <- case x0 of
                                               Side y1 y2 y3 y33 -> do return (y1, y2, y3, y33)
                                               _ -> mzero;
                      case x33 of
                          Trueo -> do return ()
                          _ -> mzero;
                      return ()}]
noManI x0 = msum [do {(x1, x2, x3, x34) <- case x0 of
                                               Side y1 y2 y3 y34 -> do return (y1, y2, y3, y34)
                                               _ -> mzero;
                      case x34 of
                          Falso -> do return ()
                          _ -> mzero;
                      return ()}]
step'IIOO x0 x1 = msum [do {(x5, x6, x7, x4) <- case x0 of
                                                    Side y5 y6 y7 y4 -> do return (y5, y6, y7, y4)
                                                    _ -> mzero;
                            (x9, x10, x11, x8) <- case x1 of
                                                      Side y9 y10 y11 y8 -> do return (y9,
                                                                                       y10,
                                                                                       y11,
                                                                                       y8)
                                                      _ -> mzero;
                            (x2, x3) <- step'0IOOIIIIIII x0 x5 x6 x7 x8 x9 x10 x11;
                            return (x2, x3)}]
step02OOII x2 x3 = msum [do {(x1, x5) <- step'IIOO x4 x3;
                             x2 <- swapIO x5;
                             return (x0, x1)}]
step'0IOOIIIIIII x0 x3 x4 x5 x6 x7 x8 x9 = msum [do {let {x2 = Empty};
                                                     let {x3 = State x5 x7};
                                                     case x5 of
                                                         Side y5 y6 y7 y6 -> do {guard (x5 == y5);
                                                                                 guard (x6 == y6);
                                                                                 guard (x7 == y7);
                                                                                 guard (x6 == y6);
                                                                                 return ()}
                                                         _ -> mzero;
                                                     case x6 of
                                                         Falso -> do return ()
                                                         _ -> mzero;
                                                     case x7 of
                                                         Side y9 y10 y11 y8 -> do {guard (x9 == y9);
                                                                                   guard (x10 == y10);
                                                                                   guard (x11 == y11);
                                                                                   guard (x8 == y8);
                                                                                   return ()}
                                                         _ -> mzero;
                                                     case x8 of
                                                         Trueo -> do return ()
                                                         _ -> mzero;
                                                     safeI x3;
                                                     return (x1, x2)},
                                                 do {let {x2 = Goat};
                                                     isGoatI x0;
                                                     x3 <- step'00OIIIII x6 x7 x9 x10 x11;
                                                     safeI x3;
                                                     return (x1, x2)},
                                                 do {let {x2 = Wolf};
                                                     isWolfI x0;
                                                     x3 <- step'01OIIII x5 x7 x9 x11;
                                                     safeI x3;
                                                     return (x1, x2)},
                                                 do {let {x2 = Cabbage};
                                                     isCabbageI x0;
                                                     x3 <- step'02OIIII x5 x6 x9 x10;
                                                     safeI x3;
                                                     return (x1, x2)}]
safeI x0 = msum [do {(x1, x2) <- case x0 of
                                     State y1 y2 -> do return (y1, y2)
                                     _ -> mzero;
                     safe'I x1;
                     safe'I x2;
                     return ()}]
isGoatI x0 = msum [do {(x27, x1, x2, x3) <- case x0 of
                                                Side y27 y1 y2 y3 -> do return (y27, y1, y2, y3)
                                                _ -> mzero;
                       case x27 of
                           Trueo -> do return ()
                           _ -> mzero;
                       return ()}]
step'00OIIIII x1 x2 x3 x4 x5 = msum [do {case x9 of
                                             Side y10 y6 y7 y11 -> do {guard (x10 == y10);
                                                                       guard (x6 == y6);
                                                                       guard (x7 == y7);
                                                                       guard (x11 == y11);
                                                                       return ()}
                                             _ -> mzero;
                                         case x10 of
                                             Falso -> do return ()
                                             _ -> mzero;
                                         case x11 of
                                             Falso -> do return ()
                                             _ -> mzero;
                                         let {x13 = Trueo};
                                         let {x14 = Trueo};
                                         let {x12 = Side x13 x10 x11 x14};
                                         let {x3 = State x9 x12};
                                         return x0}]
isWolfI x0 = msum [do {(x1, x29, x3, x4) <- case x0 of
                                                Side y1 y29 y3 y4 -> do return (y1, y29, y3, y4)
                                                _ -> mzero;
                       case x29 of
                           Trueo -> do return ()
                           _ -> mzero;
                       return ()}]
step'01OIIII x1 x2 x3 x4 = msum [do {let {x16 = Falso};
                                     let {x17 = Falso};
                                     let {x15 = Side x5 x16 x7 x17};
                                     let {x19 = Trueo};
                                     let {x20 = Trueo};
                                     let {x18 = Side x9 x19 x11 x20};
                                     let {x3 = State x15 x18};
                                     return x0}]
isCabbageI x0 = msum [do {(x1, x2, x31, x3) <- case x0 of
                                                   Side y1 y2 y31 y3 -> do return (y1, y2, y31, y3)
                                                   _ -> mzero;
                          case x31 of
                              Trueo -> do return ()
                              _ -> mzero;
                          return ()}]
step'02OIIII x1 x2 x3 x4 = msum [do {let {x22 = Falso};
                                     let {x23 = Falso};
                                     let {x21 = Side x5 x6 x22 x23};
                                     let {x25 = Trueo};
                                     let {x26 = Trueo};
                                     let {x24 = Side x9 x10 x25 x26};
                                     let {x3 = State x21 x24};
                                     return x0}]
safe'I x0 = msum [do {isManI x0; return ()},
                  do {noManI x0; safe'0I x0; return ()}]
safe'0I x0 = msum [do {noGoatI x0; return ()},
                   do {isGoatI x0; safe'00I x0; return ()}]
noGoatI x0 = msum [do {(x28, x1, x2, x3) <- case x0 of
                                                Side y28 y1 y2 y3 -> do return (y28, y1, y2, y3)
                                                _ -> mzero;
                       case x28 of
                           Falso -> do return ()
                           _ -> mzero;
                       return ()}]
safe'00I x0 = msum [do {noCabbageI x0; noWolfI x0; return ()},
                    do {isCabbageI x0; isWolfI x0; return ()}]
noCabbageI x0 = msum [do {(x1, x2, x32, x3) <- case x0 of
                                                   Side y1 y2 y32 y3 -> do return (y1, y2, y32, y3)
                                                   _ -> mzero;
                          case x32 of
                              Falso -> do return ()
                              _ -> mzero;
                          return ()}]
noWolfI x0 = msum [do {(x1, x30, x2, x3) <- case x0 of
                                                Side y1 y30 y2 y3 -> do return (y1, y30, y2, y3)
                                                _ -> mzero;
                       case x30 of
                           Falso -> do return ()
                           _ -> mzero;
                       return ()}]
swapIO x0 = msum [do {(x2, x3) <- case x0 of
                                      State y2 y3 -> do return (y2, y3)
                                      _ -> mzero;
                      let {x1 = State x3 x2};
                      return x1}]
eval x0 x2 = evalIOI x0 x2
