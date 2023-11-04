module RelSort_cpd_ans where

import Stream
import Control.Monad
import Term

sortoI :: MonadPlus m => Term -> m ()
sortoI x0 = msum [do {let {x8 = Nil};
                      (x1, x9) <- case x0 of
                                  {Cons y1 y9 -> return (y1, y9); _ -> mzero};
                      (x2, x3, x4) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOOO x1;
                      let {x7 = Cons x4 x8};
                      let {x6 = Cons x3 x7};
                      let {x5 = Cons x2 x6};
                      guard (x9 == x5);
                      return ()},
                  do {let {x13 = Nil};
                      (x1, x14) <- case x0 of
                                   {Cons y1 y14 -> return (y1, y14); _ -> mzero};
                      (x2, x3, x4) <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOOO x1;
                      let {x12 = Cons x4 x13};
                      let {x11 = Cons x3 x12};
                      let {x10 = Cons x2 x11};
                      guard (x14 == x10);
                      return ()}]
                      
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOOO x0 = msum [do {x4 <- _minmaxoIO x0;
                                                        (x1,
                                                         x2,
                                                         x3) <- ___minmaxoMinmaxoMinmaxoIOOO x4;
                                                        return (x1, x2, x3)}]
___minmaxoMinmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
___minmaxoMinmaxoMinmaxoIOOO x0 = msum [do {(x1,
                                             x2,
                                             x3) <- ____minmaxoMinmaxoIOOO x0;
                                            return (x1, x2, x3)},
                                        do {(x1, x2, x3) <- _____minmaxoMinmaxoIOOO x0;
                                            return (x1, x2, x3)}]
_____minmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
_____minmaxoMinmaxoIOOO x0 = msum [do {x1 <- leoO;
                                       guard (x0 == x1);
                                       (x2, x3) <- _________minmaxoOO;
                                       return (x1, x2, x3)},
                                   do {let {x138 = Zero};
                                       let {x137 = Succ x138};
                                       let {x139 = x137};
                                       let {x1 = Succ x139};
                                       (x2, x3) <- __________gtoMinmaxoIOO x0;
                                       return (x1, x2, x3)}]
__________gtoMinmaxoIOO :: MonadPlus m => Term -> m (Term, Term)
__________gtoMinmaxoIOO x0 = msum [do {guard (x0 == Zero);
                                       (x1, x2) <- ______minmaxoOO;
                                       return (x1, x2)},
                                   do {x3 <- case x0 of
                                             {Succ y3 -> return y3; _ -> mzero};
                                       (x1, x2) <- ___________gtoMinmaxoOOI x3;
                                       return (x1, x2)}]
___________gtoMinmaxoOOI :: MonadPlus m => Term -> m (Term, Term)
___________gtoMinmaxoOOI x2 = msum [do {guard (x2 == Zero);
                                        (x0, x1) <- ________minmaxoOO;
                                        return (x0, x1)}]
_________minmaxoOO :: MonadPlus m => m (Term, Term)
_________minmaxoOO = msum [do {let {x120 = Zero};
                               let {x123 = Zero};
                               let {x122 = Succ x123};
                               let {x127 = Zero};
                               let {x126 = Succ x127};
                               let {x125 = Succ x126};
                               _leoI x125;
                               let {x121 = x120};
                               let {x1 = Succ x121};
                               let {x124 = x122};
                               let {x0 = Succ x124};
                               return (x0, x1)}]
________minmaxoOO :: MonadPlus m => m (Term, Term)
________minmaxoOO = msum [do {let {x114 = Zero};
                              let {x116 = Zero};
                              let {x119 = Zero};
                              let {x118 = Succ x119};
                              _leoI x118;
                              let {x115 = x114};
                              let {x1 = Succ x115};
                              let {x117 = x116};
                              let {x0 = Succ x117};
                              return (x0, x1)}]
______minmaxoOO :: MonadPlus m => m (Term, Term)
______minmaxoOO = msum [do {let {x93 = Zero};
                            let {x0 = Zero};
                            let {x95 = Zero};
                            _leoI x95;
                            let {x94 = x93};
                            let {x1 = Succ x94};
                            return (x0, x1)},
                        do {let {x1 = Zero};
                            let {x96 = Zero};
                            let {x97 = x96};
                            let {x0 = Succ x97};
                            return (x0, x1)}]
____minmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
____minmaxoMinmaxoIOOO x0 = msum [do {x1 <- _leoO;
                                      guard (x0 == x1);
                                      (x2, x3) <- ____minmaxoOO;
                                      return (x1, x2, x3)},
                                  do {let {x135 = Zero};
                                      let {x136 = x135};
                                      let {x1 = Succ x136};
                                      (x2, x3) <- _________gtoMinmaxoIOO x0;
                                      return (x1, x2, x3)}]
_________gtoMinmaxoIOO :: MonadPlus m => Term -> m (Term, Term)
_________gtoMinmaxoIOO x0 = msum [do {guard (x0 == Zero);
                                      (x1, x2) <- __________minmaxoOO;
                                      return (x1, x2)}]
__________minmaxoOO :: MonadPlus m => m (Term, Term)
__________minmaxoOO = msum [do {let {x129 = Zero};
                                let {x128 = Succ x129};
                                let {x0 = Zero};
                                let {x131 = Zero};
                                leoI x131;
                                let {x130 = x128};
                                let {x1 = Succ x130};
                                return (x0, x1)},
                            do {let {x1 = Zero};
                                let {x133 = Zero};
                                let {x132 = Succ x133};
                                let {x134 = x132};
                                let {x0 = Succ x134};
                                return (x0, x1)}]
____minmaxoOO :: MonadPlus m => m (Term, Term)
____minmaxoOO = msum [do {let {x73 = Zero};
                          let {x72 = Succ x73};
                          let {x75 = Zero};
                          let {x78 = Zero};
                          let {x77 = Succ x78};
                          leoI x77;
                          let {x74 = x72};
                          let {x1 = Succ x74};
                          let {x76 = x75};
                          let {x0 = Succ x76};
                          return (x0, x1)},
                      do {let {x79 = Zero};
                          let {x82 = Zero};
                          let {x81 = Succ x82};
                          let {x80 = x79};
                          let {x1 = Succ x80};
                          let {x83 = x81};
                          let {x0 = Succ x83};
                          return (x0, x1)}]
_leoI :: MonadPlus m => Term -> m ()
_leoI x0 = msum [do {guard (x0 == Zero); return ()},
                 do {let {x18 = Zero};
                     x19 <- case x0 of
                            {Succ y19 -> return y19; _ -> mzero};
                     guard (x19 == x18);
                     return ()}]
_leoO :: MonadPlus m => m Term
_leoO = msum [do {let {x0 = Zero}; return x0},
              do {let {x18 = Zero};
                  let {x19 = x18};
                  let {x0 = Succ x19};
                  return x0}]
_minmaxoIO :: MonadPlus m => Term -> m Term
_minmaxoIO x0 = msum [do {let {x38 = Zero};
                          let {x37 = Succ x38};
                          let {x36 = Succ x37};
                          guard (x0 == Zero);
                          let {x39 = x36};
                          let {x1 = Succ x39};
                          return x1},
                      do {let {x1 = Zero};
                          let {x42 = Zero};
                          let {x41 = Succ x42};
                          let {x40 = Succ x41};
                          x43 <- case x0 of
                                 {Succ y43 -> return y43; _ -> mzero};
                          guard (x43 == x40);
                          return x1}]
leoI :: MonadPlus m => Term -> m ()
leoI x0 = msum [do {guard (x0 == Zero); return ()},
                do {x1 <- case x0 of
                          {Succ y1 -> return y1; _ -> mzero};
                    _leoI x1;
                    return ()}]
leoO :: MonadPlus m => m Term
leoO = msum [do {let {x0 = Zero}; return x0},
             do {x1 <- _leoO; let {x0 = Succ x1}; return x0}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOOO x0 = msum [do {x4 <- ______minmaxoIO x0;
                                                       (x1, x2, x3) <- minmaxoMinmaxoMinmaxoIOOO x4;
                                                       return (x1, x2, x3)},
                                                   do {x4 <- __________minmaxoIO x0;
                                                       (x1,
                                                        x2,
                                                        x3) <- _minmaxoMinmaxoMinmaxoIOOO x4;
                                                       return (x1, x2, x3)}]
__________minmaxoIO :: MonadPlus m => Term -> m (Term)
__________minmaxoIO x0 = msum [do {let {x129 = Zero};
                                   let {x128 = Succ x129};
                                   let {x131 = Zero};
                                   leoI x131;
                                   guard (x0 == Zero);
                                   let {x130 = x128};
                                   let {x1 = Succ x130};
                                   return x1},
                               do {let {x1 = Zero};
                                   let {x133 = Zero};
                                   let {x132 = Succ x133};
                                   x134 <- case x0 of
                                           {Succ y134 -> return y134; _ -> mzero};
                                   guard (x134 == x132);
                                   return x1}]
______minmaxoIO :: MonadPlus m => Term -> m (Term)
______minmaxoIO x0 = msum [do {let {x93 = Zero};
                               let {x95 = Zero};
                               _leoI x95;
                               guard (x0 == Zero);
                               let {x94 = x93};
                               let {x1 = Succ x94};
                               return x1},
                           do {let {x1 = Zero};
                               let {x96 = Zero};
                               x97 <- case x0 of
                                      {Succ y97 -> return y97; _ -> mzero};
                               guard (x97 == x96);
                               return x1}]
_minmaxoMinmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
_minmaxoMinmaxoMinmaxoIOOO x0 = msum [do {(x1,
                                           x2,
                                           x3) <- __minmaxoMinmaxoIOOO x0;
                                          return (x1, x2, x3)},
                                      do {(x1, x2, x3) <- ___minmaxoMinmaxoIOOO x0;
                                          return (x1, x2, x3)}]
___minmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
___minmaxoMinmaxoIOOO x0 = msum [do {x1 <- __leoO;
                                     guard (x0 == x1);
                                     (x2, x3) <- _______minmaxoOO;
                                     return (x1, x2, x3)},
                                 do {let {x102 = Zero};
                                     let {x101 = Succ x102};
                                     let {x100 = Succ x101};
                                     let {x103 = x100};
                                     let {x1 = Succ x103};
                                     (x2, x3) <- ______gtoMinmaxoIOO x0;
                                     return (x1, x2, x3)}]
_______minmaxoOO :: MonadPlus m => m (Term, Term)
_______minmaxoOO = msum [do {let {x104 = Zero};
                             let {x108 = Zero};
                             let {x107 = Succ x108};
                             let {x106 = Succ x107};
                             let {x113 = Zero};
                             let {x112 = Succ x113};
                             let {x111 = Succ x112};
                             let {x110 = Succ x111};
                             _leoI x110;
                             let {x105 = x104};
                             let {x1 = Succ x105};
                             let {x109 = x106};
                             let {x0 = Succ x109};
                             return (x0, x1)}]
______gtoMinmaxoIOO :: MonadPlus m => Term -> m (Term, Term)
______gtoMinmaxoIOO x0 = msum [do {guard (x0 == Zero);
                                   (x1, x2) <- ______minmaxoOO;
                                   return (x1, x2)},
                               do {x3 <- case x0 of
                                         {Succ y3 -> return y3; _ -> mzero};
                                   (x1, x2) <- _______gtoMinmaxoOOI x3;
                                   return (x1, x2)}]
_______gtoMinmaxoOOI :: MonadPlus m => Term -> m (Term, Term)
_______gtoMinmaxoOOI x2 = msum [do {guard (x2 == Zero);
                                    (x0, x1) <- ________minmaxoOO;
                                    return (x0, x1)},
                                do {x3 <- case x2 of
                                          {Succ y3 -> return y3; _ -> mzero};
                                    (x0, x1) <- ________gtoMinmaxoOOI x3;
                                    return (x0, x1)}]
________gtoMinmaxoOOI :: MonadPlus m => Term -> m (Term, Term)
________gtoMinmaxoOOI x2 = msum [do {guard (x2 == Zero);
                                     (x0, x1) <- _________minmaxoOO;
                                     return (x0, x1)}]
__leoO :: MonadPlus m => m (Term)
__leoO = msum [do {let {x0 = Zero}; return x0},
               do {x1 <- leoO; let {x0 = Succ x1}; return x0}]
__minmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
__minmaxoMinmaxoIOOO x0 = msum [do {x1 <- _leoO;
                                    guard (x0 == x1);
                                    (x2, x3) <- __minmaxoOO;
                                    return (x1, x2, x3)},
                                do {let {x98 = Zero};
                                    let {x99 = x98};
                                    let {x1 = Succ x99};
                                    (x2, x3) <- _____gtoMinmaxoIOO x0;
                                    return (x1, x2, x3)}]
_____gtoMinmaxoIOO :: MonadPlus m => Term -> m (Term, Term)
_____gtoMinmaxoIOO x0 = msum [do {guard (x0 == Zero);
                                  (x1, x2) <- _minmaxoOO;
                                  return (x1, x2)}]
__minmaxoOO :: MonadPlus m => m (Term, Term)
__minmaxoOO = msum [do {let {x46 = Zero};
                        let {x45 = Succ x46};
                        let {x44 = Succ x45};
                        let {x48 = Zero};
                        let {x50 = Zero};
                        leoI x50;
                        let {x47 = x44};
                        let {x1 = Succ x47};
                        let {x49 = x48};
                        let {x0 = Succ x49};
                        return (x0, x1)},
                    do {let {x51 = Zero};
                        let {x55 = Zero};
                        let {x54 = Succ x55};
                        let {x53 = Succ x54};
                        let {x52 = x51};
                        let {x1 = Succ x52};
                        let {x56 = x53};
                        let {x0 = Succ x56};
                        return (x0, x1)}]
_minmaxoOO :: MonadPlus m => m (Term, Term)
_minmaxoOO = msum [do {let {x38 = Zero};
                       let {x37 = Succ x38};
                       let {x36 = Succ x37};
                       let {x0 = Zero};
                       let {x39 = x36};
                       let {x1 = Succ x39};
                       return (x0, x1)},
                   do {let {x1 = Zero};
                       let {x42 = Zero};
                       let {x41 = Succ x42};
                       let {x40 = Succ x41};
                       let {x43 = x40};
                       let {x0 = Succ x43};
                       return (x0, x1)}]
minmaxoMinmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
minmaxoMinmaxoMinmaxoIOOO x0 = msum [do {(x1,
                                          x2,
                                          x3) <- minmaxoMinmaxoIOOO x0;
                                         return (x1, x2, x3)},
                                     do {(x1, x2, x3) <- _minmaxoMinmaxoIOOO x0;
                                         return (x1, x2, x3)}]
_minmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
_minmaxoMinmaxoIOOO x0 = msum [do {x1 <- __leoO;
                                   guard (x0 == x1);
                                   (x2, x3) <- ___minmaxoOO;
                                   return (x1, x2, x3)},
                               do {let {x59 = Zero};
                                   let {x58 = Succ x59};
                                   let {x57 = Succ x58};
                                   let {x60 = x57};
                                   let {x1 = Succ x60};
                                   (x2, x3) <- __gtoMinmaxoIOO x0;
                                   return (x1, x2, x3)}]
___minmaxoOO :: MonadPlus m => m (Term, Term)
___minmaxoOO = msum [do {let {x62 = Zero};
                         let {x61 = Succ x62};
                         let {x66 = Zero};
                         let {x65 = Succ x66};
                         let {x64 = Succ x65};
                         let {x71 = Zero};
                         let {x70 = Succ x71};
                         let {x69 = Succ x70};
                         let {x68 = Succ x69};
                         leoI x68;
                         let {x63 = x61};
                         let {x1 = Succ x63};
                         let {x67 = x64};
                         let {x0 = Succ x67};
                         return (x0, x1)}]
__gtoMinmaxoIOO :: MonadPlus m => Term -> m (Term, Term)
__gtoMinmaxoIOO x0 = msum [do {guard (x0 == Zero);
                               (x1, x2) <- __________minmaxoOO;
                               return (x1, x2)},
                           do {x3 <- case x0 of
                                     {Succ y3 -> return y3; _ -> mzero};
                               (x1, x2) <- ___gtoMinmaxoOOI x3;
                               return (x1, x2)}]
___gtoMinmaxoOOI :: MonadPlus m => Term -> m (Term, Term)
___gtoMinmaxoOOI x2 = msum [do {guard (x2 == Zero);
                                (x0, x1) <- ____minmaxoOO;
                                return (x0, x1)},
                            do {x3 <- case x2 of
                                      {Succ y3 -> return y3; _ -> mzero};
                                (x0, x1) <- ____gtoMinmaxoOOI x3;
                                return (x0, x1)}]
____gtoMinmaxoOOI :: MonadPlus m => Term -> m (Term, Term)
____gtoMinmaxoOOI x2 = msum [do {guard (x2 == Zero);
                                 (x0, x1) <- _____minmaxoOO;
                                 return (x0, x1)}]
_____minmaxoOO :: MonadPlus m => m (Term, Term)
_____minmaxoOO = msum [do {let {x85 = Zero};
                           let {x84 = Succ x85};
                           let {x88 = Zero};
                           let {x87 = Succ x88};
                           let {x92 = Zero};
                           let {x91 = Succ x92};
                           let {x90 = Succ x91};
                           leoI x90;
                           let {x86 = x84};
                           let {x1 = Succ x86};
                           let {x89 = x87};
                           let {x0 = Succ x89};
                           return (x0, x1)}]
minmaxoMinmaxoIOOO :: MonadPlus m => Term -> m (Term, Term, Term)
minmaxoMinmaxoIOOO x0 = msum [do {x1 <- leoO;
                                  guard (x0 == x1);
                                  (x2, x3) <- minmaxoOO;
                                  return (x1, x2, x3)},
                              do {let {x16 = Zero};
                                  let {x15 = Succ x16};
                                  let {x17 = x15};
                                  let {x1 = Succ x17};
                                  (x2, x3) <- gtoMinmaxoIOO x0;
                                  return (x1, x2, x3)}]
gtoMinmaxoIOO :: MonadPlus m => Term -> m (Term, Term)
gtoMinmaxoIOO x0 = msum [do {guard (x0 == Zero);
                             (x1, x2) <- _minmaxoOO;
                             return (x1, x2)},
                         do {x3 <- case x0 of
                                   {Succ y3 -> return y3; _ -> mzero};
                             (x1, x2) <- _gtoMinmaxoOOI x3;
                             return (x1, x2)}]
_gtoMinmaxoOOI :: MonadPlus m => Term -> m (Term, Term)
_gtoMinmaxoOOI x2 = msum [do {guard (x2 == Zero);
                              (x0, x1) <- __minmaxoOO;
                              return (x0, x1)}]
                              
minmaxoOO :: MonadPlus m => m (Term, Term)
minmaxoOO = msum [do {let {x22 = Zero};
                      let {x21 = Succ x22};
                      let {x20 = Succ x21};
                      let {x25 = Zero};
                      let {x24 = Succ x25};
                      let {x28 = Zero};
                      let {x27 = Succ x28};
                      leoI x27;
                      let {x23 = x20};
                      let {x1 = Succ x23};
                      let {x26 = x24};
                      let {x0 = Succ x26};
                      return (x0, x1)},
                  do {let {x30 = Zero};
                      let {x29 = Succ x30};
                      let {x34 = Zero};
                      let {x33 = Succ x34};
                      let {x32 = Succ x33};
                      let {x31 = x29};
                      let {x1 = Succ x31};
                      let {x35 = x32};
                      let {x0 = Succ x35};
                      return (x0, x1)}]
sortoO :: MonadPlus m => m Term -> m Term -> m Term
sortoO gen_sortoO_x10 gen_sortoO_x5 = msum [do {let {x8 = Nil};
                                                (x9, x5) <- do {x5 <- gen_sortoO_x5;
                                                                return (x5, x5)};
                                                (x2, x6) <- case x5 of
                                                            {Cons y2 y6 -> return (y2, y6);
                                                             _ -> mzero};
                                                (x3, x7) <- case x6 of
                                                            {Cons y3 y7 -> return (y3, y7);
                                                             _ -> mzero};
                                                x4 <- case x7 of
                                                      {Cons y4 y8 -> do {guard (x8 == y8);
                                                                         return y4};
                                                       _ -> mzero};
                                                x1 <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x2 x3 x4;
                                                let {x0 = Cons x1 x9};
                                                return x0},
                                            do {let {x13 = Nil};
                                                (x14, x10) <- do {x10 <- gen_sortoO_x10;
                                                                  return (x10, x10)};
                                                (x2, x11) <- case x10 of
                                                             {Cons y2 y11 -> return (y2, y11);
                                                              _ -> mzero};
                                                (x3, x12) <- case x11 of
                                                             {Cons y3 y12 -> return (y3, y12);
                                                              _ -> mzero};
                                                x4 <- case x12 of
                                                      {Cons y4 y13 -> do {guard (x13 == y13);
                                                                          return y4};
                                                       _ -> mzero};
                                                x1 <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x2 x3 x4;
                                                let {x0 = Cons x1 x14};
                                                return x0}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x4 <- ___minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                              x0 <- _minmaxoOI x4;
                                                              return x0}]
___minmaxoMinmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
___minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- ____minmaxoMinmaxoOIII x1 x2 x3;
                                                  return x0},
                                              do {x0 <- _____minmaxoMinmaxoOIII x1 x2 x3;
                                                  return x0}]
_____minmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
_____minmaxoMinmaxoOIII x1 x2 x3 = msum [do {leoI x1;
                                             _________minmaxoII x2 x3;
                                             let {x0 = x1};
                                             return x0},
                                         do {let {x138 = Zero};
                                             let {x137 = Succ x138};
                                             x139 <- case x1 of
                                                     {Succ y139 -> return y139; _ -> mzero};
                                             guard (x139 == x137);
                                             x0 <- __________gtoMinmaxoOII x2 x3;
                                             return x0}]
__________gtoMinmaxoOII :: MonadPlus m => Term -> Term -> m Term
__________gtoMinmaxoOII x1 x2 = msum [do {______minmaxoII x1 x2;
                                          let {x0 = Zero};
                                          return x0},
                                      do {x3 <- ___________gtoMinmaxoIIO x1 x2;
                                          let {x0 = Succ x3};
                                          return x0}]
___________gtoMinmaxoIIO :: MonadPlus m => Term -> Term -> m Term
___________gtoMinmaxoIIO x0 x1 = msum [do {________minmaxoII x0 x1;
                                           let {x2 = Zero};
                                           return x2}]
_________minmaxoII :: MonadPlus m => Term -> Term -> m ()
_________minmaxoII x0 x1 = msum [do {let {x120 = Zero};
                                     let {x123 = Zero};
                                     let {x122 = Succ x123};
                                     let {x127 = Zero};
                                     let {x126 = Succ x127};
                                     let {x125 = Succ x126};
                                     _leoI x125;
                                     x121 <- case x1 of
                                             {Succ y121 -> return y121; _ -> mzero};
                                     guard (x121 == x120);
                                     x124 <- case x0 of
                                             {Succ y124 -> return y124; _ -> mzero};
                                     guard (x124 == x122);
                                     return ()}]
________minmaxoII :: MonadPlus m => Term -> Term -> m ()
________minmaxoII x0 x1 = msum [do {let {x114 = Zero};
                                    let {x116 = Zero};
                                    let {x119 = Zero};
                                    let {x118 = Succ x119};
                                    _leoI x118;
                                    x115 <- case x1 of
                                            {Succ y115 -> return y115; _ -> mzero};
                                    guard (x115 == x114);
                                    x117 <- case x0 of
                                            {Succ y117 -> return y117; _ -> mzero};
                                    guard (x117 == x116);
                                    return ()}]
______minmaxoII :: MonadPlus m => Term -> Term -> m ()
______minmaxoII x0 x1 = msum [do {let {x93 = Zero};
                                  let {x95 = Zero};
                                  _leoI x95;
                                  x94 <- case x1 of
                                         {Succ y94 -> return y94; _ -> mzero};
                                  guard (x94 == x93);
                                  guard (x0 == Zero);
                                  return ()},
                              do {let {x96 = Zero};
                                  guard (x1 == Zero);
                                  x97 <- case x0 of
                                         {Succ y97 -> return y97; _ -> mzero};
                                  guard (x97 == x96);
                                  return ()}]
____minmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
____minmaxoMinmaxoOIII x1 x2 x3 = msum [do {_leoI x1;
                                            ____minmaxoII x2 x3;
                                            let {x0 = x1};
                                            return x0},
                                        do {let {x135 = Zero};
                                            x136 <- case x1 of
                                                    {Succ y136 -> return y136; _ -> mzero};
                                            guard (x136 == x135);
                                            x0 <- _________gtoMinmaxoOII x2 x3;
                                            return x0}]
_________gtoMinmaxoOII :: MonadPlus m => Term -> Term -> m Term
_________gtoMinmaxoOII x1 x2 = msum [do {__________minmaxoII x1 x2;
                                         let {x0 = Zero};
                                         return x0}]
__________minmaxoII :: MonadPlus m => Term -> Term -> m ()
__________minmaxoII x0 x1 = msum [do {let {x129 = Zero};
                                      let {x128 = Succ x129};
                                      let {x131 = Zero};
                                      leoI x131;
                                      x130 <- case x1 of
                                              {Succ y130 -> return y130; _ -> mzero};
                                      guard (x130 == x128);
                                      guard (x0 == Zero);
                                      return ()},
                                  do {let {x133 = Zero};
                                      let {x132 = Succ x133};
                                      guard (x1 == Zero);
                                      x134 <- case x0 of
                                              {Succ y134 -> return y134; _ -> mzero};
                                      guard (x134 == x132);
                                      return ()}]
____minmaxoII :: MonadPlus m => Term -> Term -> m ()
____minmaxoII x0 x1 = msum [do {let {x73 = Zero};
                                let {x72 = Succ x73};
                                let {x75 = Zero};
                                let {x78 = Zero};
                                let {x77 = Succ x78};
                                leoI x77;
                                x74 <- case x1 of
                                       {Succ y74 -> return y74; _ -> mzero};
                                guard (x74 == x72);
                                x76 <- case x0 of
                                       {Succ y76 -> return y76; _ -> mzero};
                                guard (x76 == x75);
                                return ()},
                            do {let {x79 = Zero};
                                let {x82 = Zero};
                                let {x81 = Succ x82};
                                x80 <- case x1 of
                                       {Succ y80 -> return y80; _ -> mzero};
                                guard (x80 == x79);
                                x83 <- case x0 of
                                       {Succ y83 -> return y83; _ -> mzero};
                                guard (x83 == x81);
                                return ()}]
_minmaxoOI :: MonadPlus m => Term -> m Term
_minmaxoOI x1 = msum [do {let {x38 = Zero};
                          let {x37 = Succ x38};
                          let {x36 = Succ x37};
                          let {x0 = Zero};
                          x39 <- case x1 of
                                 {Succ y39 -> return y39; _ -> mzero};
                          guard (x39 == x36);
                          return x0},
                      do {let {x42 = Zero};
                          let {x41 = Succ x42};
                          let {x40 = Succ x41};
                          guard (x1 == Zero);
                          let {x43 = x40};
                          let {x0 = Succ x43};
                          return x0}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x4 <- minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                             x0 <- ______minmaxoOI x4;
                                                             return x0},
                                                         do {x4 <- _minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                             x0 <- __________minmaxoOI x4;
                                                             return x0}]
__________minmaxoOI :: MonadPlus m => Term -> m Term
__________minmaxoOI x1 = msum [do {let {x129 = Zero};
                                   let {x128 = Succ x129};
                                   let {x0 = Zero};
                                   let {x131 = Zero};
                                   leoI x131;
                                   x130 <- case x1 of
                                           {Succ y130 -> return y130; _ -> mzero};
                                   guard (x130 == x128);
                                   return x0},
                               do {let {x133 = Zero};
                                   let {x132 = Succ x133};
                                   guard (x1 == Zero);
                                   let {x134 = x132};
                                   let {x0 = Succ x134};
                                   return x0}]
______minmaxoOI :: MonadPlus m => Term -> m Term
______minmaxoOI x1 = msum [do {let {x93 = Zero};
                               let {x0 = Zero};
                               let {x95 = Zero};
                               _leoI x95;
                               x94 <- case x1 of
                                      {Succ y94 -> return y94; _ -> mzero};
                               guard (x94 == x93);
                               return x0},
                           do {let {x96 = Zero};
                               guard (x1 == Zero);
                               let {x97 = x96};
                               let {x0 = Succ x97};
                               return x0}]
_minmaxoMinmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
_minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- __minmaxoMinmaxoOIII x1 x2 x3;
                                                return x0},
                                            do {x0 <- ___minmaxoMinmaxoOIII x1 x2 x3; return x0}]
___minmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
___minmaxoMinmaxoOIII x1 x2 x3 = msum [do {__leoI x1;
                                           _______minmaxoII x2 x3;
                                           let {x0 = x1};
                                           return x0},
                                       do {let {x102 = Zero};
                                           let {x101 = Succ x102};
                                           let {x100 = Succ x101};
                                           x103 <- case x1 of
                                                   {Succ y103 -> return y103; _ -> mzero};
                                           guard (x103 == x100);
                                           x0 <- ______gtoMinmaxoOII x2 x3;
                                           return x0}]
_______minmaxoII :: MonadPlus m => Term -> Term -> m ()
_______minmaxoII x0 x1 = msum [do {let {x104 = Zero};
                                   let {x108 = Zero};
                                   let {x107 = Succ x108};
                                   let {x106 = Succ x107};
                                   let {x113 = Zero};
                                   let {x112 = Succ x113};
                                   let {x111 = Succ x112};
                                   let {x110 = Succ x111};
                                   _leoI x110;
                                   x105 <- case x1 of
                                           {Succ y105 -> return y105; _ -> mzero};
                                   guard (x105 == x104);
                                   x109 <- case x0 of
                                           {Succ y109 -> return y109; _ -> mzero};
                                   guard (x109 == x106);
                                   return ()}]
______gtoMinmaxoOII :: MonadPlus m => Term -> Term -> m Term
______gtoMinmaxoOII x1 x2 = msum [do {______minmaxoII x1 x2;
                                      let {x0 = Zero};
                                      return x0},
                                  do {x3 <- _______gtoMinmaxoIIO x1 x2;
                                      let {x0 = Succ x3};
                                      return x0}]
_______gtoMinmaxoIIO :: MonadPlus m => Term -> Term -> m Term
_______gtoMinmaxoIIO x0 x1 = msum [do {________minmaxoII x0 x1;
                                       let {x2 = Zero};
                                       return x2},
                                   do {x3 <- ________gtoMinmaxoIIO x0 x1;
                                       let {x2 = Succ x3};
                                       return x2}]
________gtoMinmaxoIIO :: MonadPlus m => Term -> Term -> m Term
________gtoMinmaxoIIO x0 x1 = msum [do {_________minmaxoII x0 x1;
                                        let {x2 = Zero};
                                        return x2}]
__leoI :: MonadPlus m => Term -> m ()
__leoI x0 = msum [do {guard (x0 == Zero); return ()},
                  do {x1 <- case x0 of
                            {Succ y1 -> return y1; _ -> mzero};
                      leoI x1;
                      return ()}]
__minmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
__minmaxoMinmaxoOIII x1 x2 x3 = msum [do {_leoI x1;
                                          __minmaxoII x2 x3;
                                          let {x0 = x1};
                                          return x0},
                                      do {let {x98 = Zero};
                                          x99 <- case x1 of
                                                 {Succ y99 -> return y99; _ -> mzero};
                                          guard (x99 == x98);
                                          x0 <- _____gtoMinmaxoOII x2 x3;
                                          return x0}]
_____gtoMinmaxoOII :: MonadPlus m => Term -> Term -> m Term
_____gtoMinmaxoOII x1 x2 = msum [do {_minmaxoII x1 x2;
                                     let {x0 = Zero};
                                     return x0}]
__minmaxoII :: MonadPlus m => Term -> Term -> m ()
__minmaxoII x0 x1 = msum [do {let {x46 = Zero};
                              let {x45 = Succ x46};
                              let {x44 = Succ x45};
                              let {x48 = Zero};
                              let {x50 = Zero};
                              leoI x50;
                              x47 <- case x1 of
                                     {Succ y47 -> return y47; _ -> mzero};
                              guard (x47 == x44);
                              x49 <- case x0 of
                                     {Succ y49 -> return y49; _ -> mzero};
                              guard (x49 == x48);
                              return ()},
                          do {let {x51 = Zero};
                              let {x55 = Zero};
                              let {x54 = Succ x55};
                              let {x53 = Succ x54};
                              x52 <- case x1 of
                                     {Succ y52 -> return y52; _ -> mzero};
                              guard (x52 == x51);
                              x56 <- case x0 of
                                     {Succ y56 -> return y56; _ -> mzero};
                              guard (x56 == x53);
                              return ()}]
_minmaxoII :: MonadPlus m => Term -> Term -> m ()
_minmaxoII x0 x1 = msum [do {let {x38 = Zero};
                             let {x37 = Succ x38};
                             let {x36 = Succ x37};
                             x39 <- case x1 of
                                    {Succ y39 -> return y39; _ -> mzero};
                             guard (x39 == x36);
                             guard (x0 == Zero);
                             return ()},
                         do {let {x42 = Zero};
                             let {x41 = Succ x42};
                             let {x40 = Succ x41};
                             guard (x1 == Zero);
                             x43 <- case x0 of
                                    {Succ y43 -> return y43; _ -> mzero};
                             guard (x43 == x40);
                             return ()}]
minmaxoMinmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- minmaxoMinmaxoOIII x1 x2 x3;
                                               return x0},
                                           do {x0 <- _minmaxoMinmaxoOIII x1 x2 x3; return x0}]
_minmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
_minmaxoMinmaxoOIII x1 x2 x3 = msum [do {__leoI x1;
                                         ___minmaxoII x2 x3;
                                         let {x0 = x1};
                                         return x0},
                                     do {let {x59 = Zero};
                                         let {x58 = Succ x59};
                                         let {x57 = Succ x58};
                                         x60 <- case x1 of
                                                {Succ y60 -> return y60; _ -> mzero};
                                         guard (x60 == x57);
                                         x0 <- __gtoMinmaxoOII x2 x3;
                                         return x0}]
___minmaxoII :: MonadPlus m => Term -> Term -> m ()
___minmaxoII x0 x1 = msum [do {let {x62 = Zero};
                               let {x61 = Succ x62};
                               let {x66 = Zero};
                               let {x65 = Succ x66};
                               let {x64 = Succ x65};
                               let {x71 = Zero};
                               let {x70 = Succ x71};
                               let {x69 = Succ x70};
                               let {x68 = Succ x69};
                               leoI x68;
                               x63 <- case x1 of
                                      {Succ y63 -> return y63; _ -> mzero};
                               guard (x63 == x61);
                               x67 <- case x0 of
                                      {Succ y67 -> return y67; _ -> mzero};
                               guard (x67 == x64);
                               return ()}]
__gtoMinmaxoOII :: MonadPlus m => Term -> Term -> m Term
__gtoMinmaxoOII x1 x2 = msum [do {__________minmaxoII x1 x2;
                                  let {x0 = Zero};
                                  return x0},
                              do {x3 <- ___gtoMinmaxoIIO x1 x2; let {x0 = Succ x3}; return x0}]
___gtoMinmaxoIIO :: MonadPlus m => Term -> Term -> m Term
___gtoMinmaxoIIO x0 x1 = msum [do {____minmaxoII x0 x1;
                                   let {x2 = Zero};
                                   return x2},
                               do {x3 <- ____gtoMinmaxoIIO x0 x1; let {x2 = Succ x3}; return x2}]
____gtoMinmaxoIIO :: MonadPlus m => Term -> Term -> m Term
____gtoMinmaxoIIO x0 x1 = msum [do {_____minmaxoII x0 x1;
                                    let {x2 = Zero};
                                    return x2}]
_____minmaxoII :: MonadPlus m => Term -> Term -> m ()
_____minmaxoII x0 x1 = msum [do {let {x85 = Zero};
                                 let {x84 = Succ x85};
                                 let {x88 = Zero};
                                 let {x87 = Succ x88};
                                 let {x92 = Zero};
                                 let {x91 = Succ x92};
                                 let {x90 = Succ x91};
                                 leoI x90;
                                 x86 <- case x1 of
                                        {Succ y86 -> return y86; _ -> mzero};
                                 guard (x86 == x84);
                                 x89 <- case x0 of
                                        {Succ y89 -> return y89; _ -> mzero};
                                 guard (x89 == x87);
                                 return ()}]
minmaxoMinmaxoOIII :: MonadPlus m => Term -> Term -> Term -> m Term
minmaxoMinmaxoOIII x1 x2 x3 = msum [do {leoI x1;
                                        minmaxoII x2 x3;
                                        let {x0 = x1};
                                        return x0},
                                    do {let {x16 = Zero};
                                        let {x15 = Succ x16};
                                        x17 <- case x1 of
                                               {Succ y17 -> return y17; _ -> mzero};
                                        guard (x17 == x15);
                                        x0 <- gtoMinmaxoOII x2 x3;
                                        return x0}]
gtoMinmaxoOII :: MonadPlus m => Term -> Term -> m Term
gtoMinmaxoOII x1 x2 = msum [do {_minmaxoII x1 x2;
                                let {x0 = Zero};
                                return x0},
                            do {x3 <- _gtoMinmaxoIIO x1 x2; let {x0 = Succ x3}; return x0}]
_gtoMinmaxoIIO :: MonadPlus m => Term -> Term -> m Term
_gtoMinmaxoIIO x0 x1 = msum [do {__minmaxoII x0 x1;
                                 let {x2 = Zero};
                                 return x2}]
minmaxoII :: MonadPlus m => Term -> Term -> m ()
minmaxoII x0 x1 = msum [do {let {x22 = Zero};
                            let {x21 = Succ x22};
                            let {x20 = Succ x21};
                            let {x25 = Zero};
                            let {x24 = Succ x25};
                            let {x28 = Zero};
                            let {x27 = Succ x28};
                            leoI x27;
                            x23 <- case x1 of
                                   {Succ y23 -> return y23; _ -> mzero};
                            guard (x23 == x20);
                            x26 <- case x0 of
                                   {Succ y26 -> return y26; _ -> mzero};
                            guard (x26 == x24);
                            return ()},
                        do {let {x30 = Zero};
                            let {x29 = Succ x30};
                            let {x34 = Zero};
                            let {x33 = Succ x34};
                            let {x32 = Succ x33};
                            x31 <- case x1 of
                                   {Succ y31 -> return y31; _ -> mzero};
                            guard (x31 == x29);
                            x35 <- case x0 of
                                   {Succ y35 -> return y35; _ -> mzero};
                            guard (x35 == x32);
                            return ()}]