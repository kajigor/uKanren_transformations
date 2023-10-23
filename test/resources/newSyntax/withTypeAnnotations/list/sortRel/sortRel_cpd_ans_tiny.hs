module SortRel_cpd_ans_tiny where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | Succ Term
    | Zero
    deriving (Show, Eq)
sortoI x0 gen_fn181O_x0 gen_fn191O_x0 gen_fn61O_x0 gen_fn91O_x0 gen_minmaxoMinmaxo1O_x0 = msum [do {sorto0I x0;
                                                                                                    (x1,
                                                                                                     x2,
                                                                                                     x3,
                                                                                                     x4) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOOO gen_fn181O_x0 gen_fn191O_x0 gen_fn61O_x0 gen_fn91O_x0 gen_minmaxoMinmaxo1O_x0;
                                                                                                    return ()},
                                                                                                do {(x1,
                                                                                                     x2,
                                                                                                     x3,
                                                                                                     x4) <- sorto1IOOOO x0;
                                                                                                    fn5IIII x1 x2 x3 x4;
                                                                                                    return ()}]
fn5IIII x0 x1 x2 x3 = msum [do {x4 <- fn4OIII x1 x2 x3;
                                fn19II x0 x4;
                                return ()}]
fn19II x0 x1 = msum [do {fn191I x1; guard (x0 == Zero); return ()},
                     do {guard (x1 == Zero);
                         x24 <- case x0 of
                                {Succ y24 -> return y24; _ -> mzero};
                         x25 <- case x24 of
                                {Succ y25 -> return y25; _ -> mzero};
                         guard (x25 == Zero);
                         return ()}]
fn191I x0 = msum [do {x22 <- case x0 of
                             {Succ y22 -> return y22; _ -> mzero};
                      x23 <- case x22 of
                             {Succ y23 -> return y23; _ -> mzero};
                      guard (x23 == Zero);
                      return ()}]
fn4OIII x1 x2 x3 = msum [do {x0 <- fn3OIII x1 x2 x3; return x0},
                         do {x0 <- fn2OIII x1 x2 x3; return x0}]
fn2OIII x1 x2 x3 = msum [do {fn22I x1;
                             x0 <- fn1OII x2 x3;
                             return x0}]
fn1OII x1 x2 = msum [do {let {x0 = Zero}; fn12II x1 x2; return x0}]
fn12II x0 x1 = msum [do {guard (x1 == Zero);
                         guard (x0 == Zero);
                         return ()}]
fn22I x0 = msum [do {x43 <- case x0 of
                            {Succ y43 -> return y43; _ -> mzero};
                     guard (x43 == Zero);
                     return ()}]
fn3OIII x1 x2 x3 = msum [do {guard (x1 == Zero);
                             let {x0 = x1};
                             fn6II x2 x3;
                             return x0}]
fn6II x0 x1 = msum [do {fn61I x1;
                        guard (x0 == Zero);
                        fn62;
                        return ()},
                    do {guard (x1 == Zero);
                        x42 <- case x0 of
                               {Succ y42 -> return y42; _ -> mzero};
                        guard (x42 == Zero);
                        return ()}]
fn61I x0 = msum [do {x40 <- case x0 of
                            {Succ y40 -> return y40; _ -> mzero};
                     guard (x40 == Zero);
                     return ()}]
fn62 = msum [do {x41 <- leoO; guard (x41 == Zero); return ()}]
leoO = msum [do {let {x0 = Zero}; return x0},
             do {let {x14 = Zero}; let {x0 = Succ x14}; return x0}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOOO gen_fn181O_x0 gen_fn191O_x0 gen_fn61O_x0 gen_fn91O_x0 gen_minmaxoMinmaxo1O_x0 = msum [do {(x4,
                                                                                                                                   x1,
                                                                                                                                   x2,
                                                                                                                                   x3) <- minmaxoMinmaxoMinmaxoOOOO gen_fn181O_x0 gen_fn191O_x0 gen_fn61O_x0 gen_minmaxoMinmaxo1O_x0;
                                                                                                                                  x0 <- fn12OI x4;
                                                                                                                                  return (x0,
                                                                                                                                          x1,
                                                                                                                                          x2,
                                                                                                                                          x3)},
                                                                                                                              do {(x4,
                                                                                                                                   x1,
                                                                                                                                   x2,
                                                                                                                                   x3) <- fn11OOOO gen_fn191O_x0 gen_fn91O_x0;
                                                                                                                                  x0 <- fn6OI x4;
                                                                                                                                  return (x0,
                                                                                                                                          x1,
                                                                                                                                          x2,
                                                                                                                                          x3)}]
fn11OOOO gen_fn191O_x0 gen_fn91O_x0 = msum [do {(x0,
                                                 x1,
                                                 x2,
                                                 x3) <- fn10OOOO gen_fn191O_x0;
                                                return (x0, x1, x2, x3)},
                                            do {(x0, x1, x2, x3) <- fn9OOOO gen_fn91O_x0;
                                                return (x0, x1, x2, x3)}]
fn10OOOO gen_fn191O_x0 = msum [do {let {x1 = Zero};
                                   let {x0 = x1};
                                   (x2, x3) <- fn19OO gen_fn191O_x0;
                                   return (x0, x1, x2, x3)}]
fn12OI x1 = msum [do {guard (x1 == Zero);
                      let {x0 = Zero};
                      return x0}]
fn19OO gen_fn191O_x0 = msum [do {x1 <- fn191O gen_fn191O_x0;
                                 let {x0 = Zero};
                                 return (x0, x1)},
                             do {let {x1 = Zero};
                                 let {x25 = Zero};
                                 let {x24 = Succ x25};
                                 let {x0 = Succ x24};
                                 return (x0, x1)}]
fn191O gen_fn191O_x0 = msum [do {let {x23 = Zero};
                                 let {x22 = Succ x23};
                                 let {x1 = Succ x22};
                                 x0 <- gen_fn191O_x0;
                                 return x0}]
fn6OI x1 = msum [do {fn61I x1; let {x0 = Zero}; fn62; return x0},
                 do {guard (x1 == Zero);
                     let {x42 = Zero};
                     let {x0 = Succ x42};
                     return x0}]
fn9OOOO gen_fn91O_x0 = msum [do {x1 <- fn91O gen_fn91O_x0;
                                 (x0, x2, x3) <- fn8OOO;
                                 return (x0, x1, x2, x3)}]
fn8OOO = msum [do {let {x0 = Zero};
                   (x1, x2) <- fn12OO;
                   return (x0, x1, x2)}]
fn12OO = msum [do {let {x1 = Zero};
                   let {x0 = Zero};
                   return (x0, x1)}]
fn91O gen_fn91O_x0 = msum [do {let {x39 = Zero};
                               let {x38 = Succ x39};
                               let {x1 = Succ x38};
                               x0 <- gen_fn91O_x0;
                               return x0}]
minmaxoMinmaxoMinmaxoOOOO gen_fn181O_x0 gen_fn191O_x0 gen_fn61O_x0 gen_minmaxoMinmaxo1O_x0 = msum [do {(x0,
                                                                                                        x1,
                                                                                                        x2,
                                                                                                        x3) <- minmaxoMinmaxoOOOO gen_fn191O_x0 gen_minmaxoMinmaxo1O_x0;
                                                                                                       return (x0,
                                                                                                               x1,
                                                                                                               x2,
                                                                                                               x3)},
                                                                                                   do {(x0,
                                                                                                        x1,
                                                                                                        x2,
                                                                                                        x3) <- fn18OOOO gen_fn181O_x0 gen_fn61O_x0;
                                                                                                       return (x0,
                                                                                                               x1,
                                                                                                               x2,
                                                                                                               x3)}]
fn18OOOO gen_fn181O_x0 gen_fn61O_x0 = msum [do {x1 <- fn17O;
                                                let {x0 = x1};
                                                (x2, x3) <- fn16OO;
                                                return (x0, x1, x2, x3)},
                                            do {x1 <- fn181O gen_fn181O_x0;
                                                (x0, x2, x3) <- fn15OOO gen_fn61O_x0;
                                                return (x0, x1, x2, x3)}]
fn15OOO gen_fn61O_x0 = msum [do {let {x0 = Zero};
                                 (x1, x2) <- fn6OO gen_fn61O_x0;
                                 return (x0, x1, x2)},
                             do {(x1, x2, x3) <- fn14OOO;
                                 let {x0 = Succ x3};
                                 return (x0, x1, x2)}]
fn14OOO = msum [do {let {x2 = Zero};
                    (x0, x1) <- fn13OO;
                    return (x0, x1, x2)}]
fn13OO = msum [do {let {x34 = Zero};
                   let {x1 = Succ x34};
                   let {x35 = Zero};
                   let {x0 = Succ x35};
                   fn131;
                   return (x0, x1)}]
fn131 = msum [do {x36 <- leoO;
                  x37 <- case x36 of
                         {Succ y37 -> return y37; _ -> mzero};
                  guard (x37 == Zero);
                  return ()}]
fn16OO = msum [do {let {x28 = Zero};
                   let {x1 = Succ x28};
                   let {x30 = Zero};
                   let {x29 = Succ x30};
                   let {x0 = Succ x29};
                   fn161;
                   return (x0, x1)}]
fn161 = msum [do {x31 <- leoO;
                  x32 <- case x31 of
                         {Succ y32 -> return y32; _ -> mzero};
                  x33 <- case x32 of
                         {Succ y33 -> return y33; _ -> mzero};
                  guard (x33 == Zero);
                  return ()}]
fn17O = msum [do {let {x0 = Zero}; return x0},
              do {x1 <- leoO; let {x0 = Succ x1}; return x0}]
fn181O gen_fn181O_x0 = msum [do {let {x27 = Zero};
                                 let {x26 = Succ x27};
                                 let {x1 = Succ x26};
                                 x0 <- gen_fn181O_x0;
                                 return x0}]
fn6OO gen_fn61O_x0 = msum [do {x1 <- fn61O gen_fn61O_x0;
                               let {x0 = Zero};
                               fn62;
                               return (x0, x1)},
                           do {let {x1 = Zero};
                               let {x42 = Zero};
                               let {x0 = Succ x42};
                               return (x0, x1)}]
fn61O gen_fn61O_x0 = msum [do {let {x40 = Zero};
                               let {x1 = Succ x40};
                               x0 <- gen_fn61O_x0;
                               return x0}]
minmaxoMinmaxoOOOO gen_fn191O_x0 gen_minmaxoMinmaxo1O_x0 = msum [do {x1 <- leoO;
                                                                     let {x0 = x1};
                                                                     (x2, x3) <- minmaxoOO;
                                                                     return (x0, x1, x2, x3)},
                                                                 do {x1 <- minmaxoMinmaxo1O gen_minmaxoMinmaxo1O_x0;
                                                                     (x0,
                                                                      x2,
                                                                      x3) <- fn20OOO gen_fn191O_x0;
                                                                     return (x0, x1, x2, x3)}]
fn20OOO gen_fn191O_x0 = msum [do {let {x0 = Zero};
                                  (x1, x2) <- fn19OO gen_fn191O_x0;
                                  return (x0, x1, x2)}]
minmaxoOO = msum [do {let {x16 = Zero};
                      let {x15 = Succ x16};
                      let {x1 = Succ x15};
                      let {x17 = Zero};
                      let {x0 = Succ x17};
                      minmaxo1;
                      return (x0, x1)},
                  do {let {x19 = Zero};
                      let {x1 = Succ x19};
                      let {x21 = Zero};
                      let {x20 = Succ x21};
                      let {x0 = Succ x20};
                      return (x0, x1)}]
minmaxo1 = msum [do {x18 <- leoO; guard (x18 == Zero); return ()}]
minmaxoMinmaxo1O gen_minmaxoMinmaxo1O_x0 = msum [do {let {x13 = Zero};
                                                     let {x1 = Succ x13};
                                                     x0 <- gen_minmaxoMinmaxo1O_x0;
                                                     return x0}]
sortoO gen_fn181O_x0 gen_fn191O_x0 gen_fn61O_x0 gen_fn91O_x0 gen_minmaxoMinmaxo1O_x0 gen_sorto0O_x1 gen_sorto0O_x2 gen_sorto0O_x3 gen_sorto0O_x4 gen_sorto1OOOOO_x1 gen_sorto1OOOOO_x2 gen_sorto1OOOOO_x3 gen_sorto1OOOOO_x4 = msum [do {x0 <- sorto0O gen_sorto0O_x1 gen_sorto0O_x2 gen_sorto0O_x3 gen_sorto0O_x4;
                                                                                                                                                                                                                                         (x1,
                                                                                                                                                                                                                                          x2,
                                                                                                                                                                                                                                          x3,
                                                                                                                                                                                                                                          x4) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOOO gen_fn181O_x0 gen_fn191O_x0 gen_fn61O_x0 gen_fn91O_x0 gen_minmaxoMinmaxo1O_x0;
                                                                                                                                                                                                                                         return x0},
                                                                                                                                                                                                                                     do {(x0,
                                                                                                                                                                                                                                          x1,
                                                                                                                                                                                                                                          x2,
                                                                                                                                                                                                                                          x3,
                                                                                                                                                                                                                                          x4) <- sorto1OOOOO gen_sorto1OOOOO_x1 gen_sorto1OOOOO_x2 gen_sorto1OOOOO_x3 gen_sorto1OOOOO_x4;
                                                                                                                                                                                                                                         fn5IIII x1 x2 x3 x4;
                                                                                                                                                                                                                                         return x0}]
sorto0I x0 = msum [do {(x1, x5) <- case x0 of
                                   {Cons y1 y5 -> return (y1, y5); _ -> mzero};
                       (x2, x6) <- case x5 of
                                   {Cons y2 y6 -> return (y2, y6); _ -> mzero};
                       (x3, x7) <- case x6 of
                                   {Cons y3 y7 -> return (y3, y7); _ -> mzero};
                       (x4, x8) <- case x7 of
                                   {Cons y4 y8 -> return (y4, y8); _ -> mzero};
                       guard (x8 == Nil);
                       return ()}]
sorto0O gen_sorto0O_x1 gen_sorto0O_x2 gen_sorto0O_x3 gen_sorto0O_x4 = msum [do {let {x8 = Nil};
                                                                                (x7,
                                                                                 x4) <- do {x4 <- gen_sorto0O_x4;
                                                                                            let {x7 = Cons x4 x8};
                                                                                            return (x7,
                                                                                                    x4)};
                                                                                (x6,
                                                                                 x3) <- do {x3 <- gen_sorto0O_x3;
                                                                                            let {x6 = Cons x3 x7};
                                                                                            return (x6,
                                                                                                    x3)};
                                                                                (x5,
                                                                                 x2) <- do {x2 <- gen_sorto0O_x2;
                                                                                            let {x5 = Cons x2 x6};
                                                                                            return (x5,
                                                                                                    x2)};
                                                                                (x0,
                                                                                 x1) <- do {x1 <- gen_sorto0O_x1;
                                                                                            let {x0 = Cons x1 x5};
                                                                                            return (x0,
                                                                                                    x1)};
                                                                                return x0}]
sorto1IOOOO x0 = msum [do {(x1, x9) <- case x0 of
                                       {Cons y1 y9 -> return (y1, y9); _ -> mzero};
                           (x2, x10) <- case x9 of
                                        {Cons y2 y10 -> return (y2, y10); _ -> mzero};
                           (x3, x11) <- case x10 of
                                        {Cons y3 y11 -> return (y3, y11); _ -> mzero};
                           (x4, x12) <- case x11 of
                                        {Cons y4 y12 -> return (y4, y12); _ -> mzero};
                           guard (x12 == Nil);
                           return (x1, x2, x3, x4)}]
sorto1OOOOO gen_sorto1OOOOO_x1 gen_sorto1OOOOO_x2 gen_sorto1OOOOO_x3 gen_sorto1OOOOO_x4 = msum [do {let {x12 = Nil};
                                                                                                    (x11,
                                                                                                     x4) <- do {x4 <- gen_sorto1OOOOO_x4;
                                                                                                                let {x11 = Cons x4 x12};
                                                                                                                return (x11,
                                                                                                                        x4)};
                                                                                                    (x10,
                                                                                                     x3) <- do {x3 <- gen_sorto1OOOOO_x3;
                                                                                                                let {x10 = Cons x3 x11};
                                                                                                                return (x10,
                                                                                                                        x3)};
                                                                                                    (x9,
                                                                                                     x2) <- do {x2 <- gen_sorto1OOOOO_x2;
                                                                                                                let {x9 = Cons x2 x10};
                                                                                                                return (x9,
                                                                                                                        x2)};
                                                                                                    (x0,
                                                                                                     x1) <- do {x1 <- gen_sorto1OOOOO_x1;
                                                                                                                let {x0 = Cons x1 x9};
                                                                                                                return (x0,
                                                                                                                        x1)};
                                                                                                    return (x0,
                                                                                                            x1,
                                                                                                            x2,
                                                                                                            x3,
                                                                                                            x4)}]