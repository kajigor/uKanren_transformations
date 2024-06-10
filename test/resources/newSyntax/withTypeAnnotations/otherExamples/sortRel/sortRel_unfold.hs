module SortRel_unfold where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | Succ Term
    | Zero
    deriving (Show, Eq)
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
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOOO x0 = msum [do {x4 <- _minmaxoIO x0;
                                                        (x1,
                                                         x2,
                                                         x3) <- ___minmaxoMinmaxoMinmaxoIOOO x4;
                                                        return (x1, x2, x3)}]
___minmaxoMinmaxoMinmaxoIOOO x0 = msum [do {(x1,
                                             x2,
                                             x3) <- ____minmaxoMinmaxoIOOO x0;
                                            return (x1, x2, x3)},
                                        do {(x1, x2, x3) <- _____minmaxoMinmaxoIOOO x0;
                                            return (x1, x2, x3)}]
_____minmaxoMinmaxoIOOO x0 = msum [do {let {x128 = Zero};
                                       let {x127 = Succ x128};
                                       guard (x0 == Zero);
                                       let {x129 = x127};
                                       let {x1 = Succ x129};
                                       (x2, x3) <- ______minmaxoOO;
                                       return (x1, x2, x3)},
                                   do {let {x131 = Zero};
                                       let {x130 = Succ x131};
                                       let {x133 = Zero};
                                       x134 <- case x0 of
                                               {Succ y134 -> return y134; _ -> mzero};
                                       guard (x134 == x133);
                                       let {x132 = x130};
                                       let {x1 = Succ x132};
                                       (x2, x3) <- ________minmaxoOO;
                                       return (x1, x2, x3)}]
________minmaxoOO = msum [do {let {x113 = Zero};
                              let {x115 = Zero};
                              let {x114 = x113};
                              let {x1 = Succ x114};
                              let {x116 = x115};
                              let {x0 = Succ x116};
                              return (x0, x1)}]
______minmaxoOO = msum [do {let {x95 = Zero};
                            let {x0 = Zero};
                            let {x96 = x95};
                            let {x1 = Succ x96};
                            return (x0, x1)},
                        do {let {x1 = Zero};
                            let {x97 = Zero};
                            let {x98 = x97};
                            let {x0 = Succ x98};
                            return (x0, x1)}]
____minmaxoMinmaxoIOOO x0 = msum [do {let {x1 = Zero};
                                      guard (x0 == x1);
                                      (x2, x3) <- ____minmaxoOO;
                                      return (x1, x2, x3)},
                                  do {let {x123 = Zero};
                                      let {x124 = x123};
                                      let {x1 = Succ x124};
                                      guard (x0 == x1);
                                      (x2, x3) <- ____minmaxoOO;
                                      return (x1, x2, x3)},
                                  do {let {x125 = Zero};
                                      guard (x0 == Zero);
                                      let {x126 = x125};
                                      let {x1 = Succ x126};
                                      (x2, x3) <- __________minmaxoOO;
                                      return (x1, x2, x3)}]
__________minmaxoOO = msum [do {let {x118 = Zero};
                                let {x117 = Succ x118};
                                let {x0 = Zero};
                                let {x119 = x117};
                                let {x1 = Succ x119};
                                return (x0, x1)},
                            do {let {x1 = Zero};
                                let {x121 = Zero};
                                let {x120 = Succ x121};
                                let {x122 = x120};
                                let {x0 = Succ x122};
                                return (x0, x1)}]
____minmaxoOO = msum [do {let {x80 = Zero};
                          let {x79 = Succ x80};
                          let {x82 = Zero};
                          let {x81 = x79};
                          let {x1 = Succ x81};
                          let {x83 = x82};
                          let {x0 = Succ x83};
                          return (x0, x1)},
                      do {let {x84 = Zero};
                          let {x87 = Zero};
                          let {x86 = Succ x87};
                          let {x85 = x84};
                          let {x1 = Succ x85};
                          let {x88 = x86};
                          let {x0 = Succ x88};
                          return (x0, x1)}]
_minmaxoIO x0 = msum [do {let {x44 = Zero};
                          let {x43 = Succ x44};
                          let {x42 = Succ x43};
                          guard (x0 == Zero);
                          let {x45 = x42};
                          let {x1 = Succ x45};
                          return x1},
                      do {let {x1 = Zero};
                          let {x48 = Zero};
                          let {x47 = Succ x48};
                          let {x46 = Succ x47};
                          x49 <- case x0 of
                                 {Succ y49 -> return y49; _ -> mzero};
                          guard (x49 == x46);
                          return x1}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOOO x0 = msum [do {x4 <- ______minmaxoIO x0;
                                                       (x1, x2, x3) <- minmaxoMinmaxoMinmaxoIOOO x4;
                                                       return (x1, x2, x3)},
                                                   do {x4 <- __________minmaxoIO x0;
                                                       (x1,
                                                        x2,
                                                        x3) <- _minmaxoMinmaxoMinmaxoIOOO x4;
                                                       return (x1, x2, x3)}]
__________minmaxoIO x0 = msum [do {let {x118 = Zero};
                                   let {x117 = Succ x118};
                                   guard (x0 == Zero);
                                   let {x119 = x117};
                                   let {x1 = Succ x119};
                                   return x1},
                               do {let {x1 = Zero};
                                   let {x121 = Zero};
                                   let {x120 = Succ x121};
                                   x122 <- case x0 of
                                           {Succ y122 -> return y122; _ -> mzero};
                                   guard (x122 == x120);
                                   return x1}]
______minmaxoIO x0 = msum [do {let {x95 = Zero};
                               guard (x0 == Zero);
                               let {x96 = x95};
                               let {x1 = Succ x96};
                               return x1},
                           do {let {x1 = Zero};
                               let {x97 = Zero};
                               x98 <- case x0 of
                                      {Succ y98 -> return y98; _ -> mzero};
                               guard (x98 == x97);
                               return x1}]
_minmaxoMinmaxoMinmaxoIOOO x0 = msum [do {(x1,
                                           x2,
                                           x3) <- __minmaxoMinmaxoIOOO x0;
                                          return (x1, x2, x3)},
                                      do {(x1, x2, x3) <- ___minmaxoMinmaxoIOOO x0;
                                          return (x1, x2, x3)}]
___minmaxoMinmaxoIOOO x0 = msum [do {let {x105 = Zero};
                                     let {x104 = Succ x105};
                                     let {x103 = Succ x104};
                                     guard (x0 == Zero);
                                     let {x106 = x103};
                                     let {x1 = Succ x106};
                                     (x2, x3) <- ______minmaxoOO;
                                     return (x1, x2, x3)},
                                 do {let {x109 = Zero};
                                     let {x108 = Succ x109};
                                     let {x107 = Succ x108};
                                     let {x111 = Zero};
                                     x112 <- case x0 of
                                             {Succ y112 -> return y112; _ -> mzero};
                                     guard (x112 == x111);
                                     let {x110 = x107};
                                     let {x1 = Succ x110};
                                     (x2, x3) <- ________minmaxoOO;
                                     return (x1, x2, x3)}]
__minmaxoMinmaxoIOOO x0 = msum [do {let {x1 = Zero};
                                    guard (x0 == x1);
                                    (x2, x3) <- __minmaxoOO;
                                    return (x1, x2, x3)},
                                do {let {x99 = Zero};
                                    let {x100 = x99};
                                    let {x1 = Succ x100};
                                    guard (x0 == x1);
                                    (x2, x3) <- __minmaxoOO;
                                    return (x1, x2, x3)},
                                do {let {x101 = Zero};
                                    guard (x0 == Zero);
                                    let {x102 = x101};
                                    let {x1 = Succ x102};
                                    (x2, x3) <- _minmaxoOO;
                                    return (x1, x2, x3)}]
__minmaxoOO = msum [do {let {x52 = Zero};
                        let {x51 = Succ x52};
                        let {x50 = Succ x51};
                        let {x54 = Zero};
                        let {x53 = x50};
                        let {x1 = Succ x53};
                        let {x55 = x54};
                        let {x0 = Succ x55};
                        return (x0, x1)},
                    do {let {x56 = Zero};
                        let {x60 = Zero};
                        let {x59 = Succ x60};
                        let {x58 = Succ x59};
                        let {x57 = x56};
                        let {x1 = Succ x57};
                        let {x61 = x58};
                        let {x0 = Succ x61};
                        return (x0, x1)}]
_minmaxoOO = msum [do {let {x44 = Zero};
                       let {x43 = Succ x44};
                       let {x42 = Succ x43};
                       let {x0 = Zero};
                       let {x45 = x42};
                       let {x1 = Succ x45};
                       return (x0, x1)},
                   do {let {x1 = Zero};
                       let {x48 = Zero};
                       let {x47 = Succ x48};
                       let {x46 = Succ x47};
                       let {x49 = x46};
                       let {x0 = Succ x49};
                       return (x0, x1)}]
minmaxoMinmaxoMinmaxoIOOO x0 = msum [do {(x1,
                                          x2,
                                          x3) <- minmaxoMinmaxoIOOO x0;
                                         return (x1, x2, x3)},
                                     do {(x1, x2, x3) <- _minmaxoMinmaxoIOOO x0;
                                         return (x1, x2, x3)}]
_minmaxoMinmaxoIOOO x0 = msum [do {let {x64 = Zero};
                                   let {x63 = Succ x64};
                                   let {x62 = Succ x63};
                                   guard (x0 == Zero);
                                   let {x65 = x62};
                                   let {x1 = Succ x65};
                                   (x2, x3) <- __________minmaxoOO;
                                   return (x1, x2, x3)},
                               do {let {x68 = Zero};
                                   let {x67 = Succ x68};
                                   let {x66 = Succ x67};
                                   let {x70 = Zero};
                                   x71 <- case x0 of
                                          {Succ y71 -> return y71; _ -> mzero};
                                   guard (x71 == x70);
                                   let {x69 = x66};
                                   let {x1 = Succ x69};
                                   (x2, x3) <- ____minmaxoOO;
                                   return (x1, x2, x3)},
                               do {let {x74 = Zero};
                                   let {x73 = Succ x74};
                                   let {x72 = Succ x73};
                                   let {x77 = Zero};
                                   let {x76 = Succ x77};
                                   x78 <- case x0 of
                                          {Succ y78 -> return y78; _ -> mzero};
                                   guard (x78 == x76);
                                   let {x75 = x72};
                                   let {x1 = Succ x75};
                                   (x2, x3) <- _____minmaxoOO;
                                   return (x1, x2, x3)}]
_____minmaxoOO = msum [do {let {x90 = Zero};
                           let {x89 = Succ x90};
                           let {x93 = Zero};
                           let {x92 = Succ x93};
                           let {x91 = x89};
                           let {x1 = Succ x91};
                           let {x94 = x92};
                           let {x0 = Succ x94};
                           return (x0, x1)}]
minmaxoMinmaxoIOOO x0 = msum [do {let {x1 = Zero};
                                  guard (x0 == x1);
                                  (x2, x3) <- minmaxoOO;
                                  return (x1, x2, x3)},
                              do {let {x15 = Zero};
                                  let {x16 = x15};
                                  let {x1 = Succ x16};
                                  guard (x0 == x1);
                                  (x2, x3) <- minmaxoOO;
                                  return (x1, x2, x3)},
                              do {let {x18 = Zero};
                                  let {x17 = Succ x18};
                                  let {x19 = x17};
                                  let {x1 = Succ x19};
                                  guard (x0 == x1);
                                  (x2, x3) <- minmaxoOO;
                                  return (x1, x2, x3)},
                              do {let {x21 = Zero};
                                  let {x20 = Succ x21};
                                  guard (x0 == Zero);
                                  let {x22 = x20};
                                  let {x1 = Succ x22};
                                  (x2, x3) <- _minmaxoOO;
                                  return (x1, x2, x3)},
                              do {let {x24 = Zero};
                                  let {x23 = Succ x24};
                                  let {x26 = Zero};
                                  x27 <- case x0 of
                                         {Succ y27 -> return y27; _ -> mzero};
                                  guard (x27 == x26);
                                  let {x25 = x23};
                                  let {x1 = Succ x25};
                                  (x2, x3) <- __minmaxoOO;
                                  return (x1, x2, x3)}]
minmaxoOO = msum [do {let {x30 = Zero};
                      let {x29 = Succ x30};
                      let {x28 = Succ x29};
                      let {x33 = Zero};
                      let {x32 = Succ x33};
                      let {x31 = x28};
                      let {x1 = Succ x31};
                      let {x34 = x32};
                      let {x0 = Succ x34};
                      return (x0, x1)},
                  do {let {x36 = Zero};
                      let {x35 = Succ x36};
                      let {x40 = Zero};
                      let {x39 = Succ x40};
                      let {x38 = Succ x39};
                      let {x37 = x35};
                      let {x1 = Succ x37};
                      let {x41 = x38};
                      let {x0 = Succ x41};
                      return (x0, x1)}]
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
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x4 <- ___minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                              x0 <- _minmaxoOI x4;
                                                              return x0}]
___minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- ____minmaxoMinmaxoOIII x1 x2 x3;
                                                  return x0},
                                              do {x0 <- _____minmaxoMinmaxoOIII x1 x2 x3;
                                                  return x0}]
_____minmaxoMinmaxoOIII x1 x2 x3 = msum [do {______minmaxoII x2 x3;
                                             let {x128 = Zero};
                                             let {x127 = Succ x128};
                                             let {x0 = Zero};
                                             x129 <- case x1 of
                                                     {Succ y129 -> return y129; _ -> mzero};
                                             guard (x129 == x127);
                                             return x0},
                                         do {________minmaxoII x2 x3;
                                             let {x131 = Zero};
                                             let {x130 = Succ x131};
                                             let {x133 = Zero};
                                             x132 <- case x1 of
                                                     {Succ y132 -> return y132; _ -> mzero};
                                             guard (x132 == x130);
                                             let {x134 = x133};
                                             let {x0 = Succ x134};
                                             return x0}]
________minmaxoII x0 x1 = msum [do {let {x113 = Zero};
                                    let {x115 = Zero};
                                    x114 <- case x1 of
                                            {Succ y114 -> return y114; _ -> mzero};
                                    guard (x114 == x113);
                                    x116 <- case x0 of
                                            {Succ y116 -> return y116; _ -> mzero};
                                    guard (x116 == x115);
                                    return ()}]
______minmaxoII x0 x1 = msum [do {let {x95 = Zero};
                                  x96 <- case x1 of
                                         {Succ y96 -> return y96; _ -> mzero};
                                  guard (x96 == x95);
                                  guard (x0 == Zero);
                                  return ()},
                              do {let {x97 = Zero};
                                  guard (x1 == Zero);
                                  x98 <- case x0 of
                                         {Succ y98 -> return y98; _ -> mzero};
                                  guard (x98 == x97);
                                  return ()}]
____minmaxoMinmaxoOIII x1 x2 x3 = msum [do {____minmaxoII x2 x3;
                                            guard (x1 == Zero);
                                            let {x0 = x1};
                                            return x0},
                                        do {____minmaxoII x2 x3;
                                            let {x123 = Zero};
                                            x124 <- case x1 of
                                                    {Succ y124 -> return y124; _ -> mzero};
                                            guard (x124 == x123);
                                            let {x0 = x1};
                                            return x0},
                                        do {__________minmaxoII x2 x3;
                                            let {x125 = Zero};
                                            let {x0 = Zero};
                                            x126 <- case x1 of
                                                    {Succ y126 -> return y126; _ -> mzero};
                                            guard (x126 == x125);
                                            return x0}]
__________minmaxoII x0 x1 = msum [do {let {x118 = Zero};
                                      let {x117 = Succ x118};
                                      x119 <- case x1 of
                                              {Succ y119 -> return y119; _ -> mzero};
                                      guard (x119 == x117);
                                      guard (x0 == Zero);
                                      return ()},
                                  do {let {x121 = Zero};
                                      let {x120 = Succ x121};
                                      guard (x1 == Zero);
                                      x122 <- case x0 of
                                              {Succ y122 -> return y122; _ -> mzero};
                                      guard (x122 == x120);
                                      return ()}]
____minmaxoII x0 x1 = msum [do {let {x80 = Zero};
                                let {x79 = Succ x80};
                                let {x82 = Zero};
                                x81 <- case x1 of
                                       {Succ y81 -> return y81; _ -> mzero};
                                guard (x81 == x79);
                                x83 <- case x0 of
                                       {Succ y83 -> return y83; _ -> mzero};
                                guard (x83 == x82);
                                return ()},
                            do {let {x84 = Zero};
                                let {x87 = Zero};
                                let {x86 = Succ x87};
                                x85 <- case x1 of
                                       {Succ y85 -> return y85; _ -> mzero};
                                guard (x85 == x84);
                                x88 <- case x0 of
                                       {Succ y88 -> return y88; _ -> mzero};
                                guard (x88 == x86);
                                return ()}]
_minmaxoOI x1 = msum [do {let {x44 = Zero};
                          let {x43 = Succ x44};
                          let {x42 = Succ x43};
                          let {x0 = Zero};
                          x45 <- case x1 of
                                 {Succ y45 -> return y45; _ -> mzero};
                          guard (x45 == x42);
                          return x0},
                      do {let {x48 = Zero};
                          let {x47 = Succ x48};
                          let {x46 = Succ x47};
                          guard (x1 == Zero);
                          let {x49 = x46};
                          let {x0 = Succ x49};
                          return x0}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x4 <- minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                             x0 <- ______minmaxoOI x4;
                                                             return x0},
                                                         do {x4 <- _minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                             x0 <- __________minmaxoOI x4;
                                                             return x0}]
__________minmaxoOI x1 = msum [do {let {x118 = Zero};
                                   let {x117 = Succ x118};
                                   let {x0 = Zero};
                                   x119 <- case x1 of
                                           {Succ y119 -> return y119; _ -> mzero};
                                   guard (x119 == x117);
                                   return x0},
                               do {let {x121 = Zero};
                                   let {x120 = Succ x121};
                                   guard (x1 == Zero);
                                   let {x122 = x120};
                                   let {x0 = Succ x122};
                                   return x0}]
______minmaxoOI x1 = msum [do {let {x95 = Zero};
                               let {x0 = Zero};
                               x96 <- case x1 of
                                      {Succ y96 -> return y96; _ -> mzero};
                               guard (x96 == x95);
                               return x0},
                           do {let {x97 = Zero};
                               guard (x1 == Zero);
                               let {x98 = x97};
                               let {x0 = Succ x98};
                               return x0}]
_minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- __minmaxoMinmaxoOIII x1 x2 x3;
                                                return x0},
                                            do {x0 <- ___minmaxoMinmaxoOIII x1 x2 x3; return x0}]
___minmaxoMinmaxoOIII x1 x2 x3 = msum [do {______minmaxoII x2 x3;
                                           let {x105 = Zero};
                                           let {x104 = Succ x105};
                                           let {x103 = Succ x104};
                                           let {x0 = Zero};
                                           x106 <- case x1 of
                                                   {Succ y106 -> return y106; _ -> mzero};
                                           guard (x106 == x103);
                                           return x0},
                                       do {________minmaxoII x2 x3;
                                           let {x109 = Zero};
                                           let {x108 = Succ x109};
                                           let {x107 = Succ x108};
                                           let {x111 = Zero};
                                           x110 <- case x1 of
                                                   {Succ y110 -> return y110; _ -> mzero};
                                           guard (x110 == x107);
                                           let {x112 = x111};
                                           let {x0 = Succ x112};
                                           return x0}]
__minmaxoMinmaxoOIII x1 x2 x3 = msum [do {__minmaxoII x2 x3;
                                          guard (x1 == Zero);
                                          let {x0 = x1};
                                          return x0},
                                      do {__minmaxoII x2 x3;
                                          let {x99 = Zero};
                                          x100 <- case x1 of
                                                  {Succ y100 -> return y100; _ -> mzero};
                                          guard (x100 == x99);
                                          let {x0 = x1};
                                          return x0},
                                      do {_minmaxoII x2 x3;
                                          let {x101 = Zero};
                                          let {x0 = Zero};
                                          x102 <- case x1 of
                                                  {Succ y102 -> return y102; _ -> mzero};
                                          guard (x102 == x101);
                                          return x0}]
__minmaxoII x0 x1 = msum [do {let {x52 = Zero};
                              let {x51 = Succ x52};
                              let {x50 = Succ x51};
                              let {x54 = Zero};
                              x53 <- case x1 of
                                     {Succ y53 -> return y53; _ -> mzero};
                              guard (x53 == x50);
                              x55 <- case x0 of
                                     {Succ y55 -> return y55; _ -> mzero};
                              guard (x55 == x54);
                              return ()},
                          do {let {x56 = Zero};
                              let {x60 = Zero};
                              let {x59 = Succ x60};
                              let {x58 = Succ x59};
                              x57 <- case x1 of
                                     {Succ y57 -> return y57; _ -> mzero};
                              guard (x57 == x56);
                              x61 <- case x0 of
                                     {Succ y61 -> return y61; _ -> mzero};
                              guard (x61 == x58);
                              return ()}]
_minmaxoII x0 x1 = msum [do {let {x44 = Zero};
                             let {x43 = Succ x44};
                             let {x42 = Succ x43};
                             x45 <- case x1 of
                                    {Succ y45 -> return y45; _ -> mzero};
                             guard (x45 == x42);
                             guard (x0 == Zero);
                             return ()},
                         do {let {x48 = Zero};
                             let {x47 = Succ x48};
                             let {x46 = Succ x47};
                             guard (x1 == Zero);
                             x49 <- case x0 of
                                    {Succ y49 -> return y49; _ -> mzero};
                             guard (x49 == x46);
                             return ()}]
minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- minmaxoMinmaxoOIII x1 x2 x3;
                                               return x0},
                                           do {x0 <- _minmaxoMinmaxoOIII x1 x2 x3; return x0}]
_minmaxoMinmaxoOIII x1 x2 x3 = msum [do {__________minmaxoII x2 x3;
                                         let {x64 = Zero};
                                         let {x63 = Succ x64};
                                         let {x62 = Succ x63};
                                         let {x0 = Zero};
                                         x65 <- case x1 of
                                                {Succ y65 -> return y65; _ -> mzero};
                                         guard (x65 == x62);
                                         return x0},
                                     do {____minmaxoII x2 x3;
                                         let {x68 = Zero};
                                         let {x67 = Succ x68};
                                         let {x66 = Succ x67};
                                         let {x70 = Zero};
                                         x69 <- case x1 of
                                                {Succ y69 -> return y69; _ -> mzero};
                                         guard (x69 == x66);
                                         let {x71 = x70};
                                         let {x0 = Succ x71};
                                         return x0},
                                     do {_____minmaxoII x2 x3;
                                         let {x74 = Zero};
                                         let {x73 = Succ x74};
                                         let {x72 = Succ x73};
                                         let {x77 = Zero};
                                         let {x76 = Succ x77};
                                         x75 <- case x1 of
                                                {Succ y75 -> return y75; _ -> mzero};
                                         guard (x75 == x72);
                                         let {x78 = x76};
                                         let {x0 = Succ x78};
                                         return x0}]
_____minmaxoII x0 x1 = msum [do {let {x90 = Zero};
                                 let {x89 = Succ x90};
                                 let {x93 = Zero};
                                 let {x92 = Succ x93};
                                 x91 <- case x1 of
                                        {Succ y91 -> return y91; _ -> mzero};
                                 guard (x91 == x89);
                                 x94 <- case x0 of
                                        {Succ y94 -> return y94; _ -> mzero};
                                 guard (x94 == x92);
                                 return ()}]
minmaxoMinmaxoOIII x1 x2 x3 = msum [do {minmaxoII x2 x3;
                                        guard (x1 == Zero);
                                        let {x0 = x1};
                                        return x0},
                                    do {minmaxoII x2 x3;
                                        let {x15 = Zero};
                                        x16 <- case x1 of
                                               {Succ y16 -> return y16; _ -> mzero};
                                        guard (x16 == x15);
                                        let {x0 = x1};
                                        return x0},
                                    do {minmaxoII x2 x3;
                                        let {x18 = Zero};
                                        let {x17 = Succ x18};
                                        x19 <- case x1 of
                                               {Succ y19 -> return y19; _ -> mzero};
                                        guard (x19 == x17);
                                        let {x0 = x1};
                                        return x0},
                                    do {_minmaxoII x2 x3;
                                        let {x21 = Zero};
                                        let {x20 = Succ x21};
                                        let {x0 = Zero};
                                        x22 <- case x1 of
                                               {Succ y22 -> return y22; _ -> mzero};
                                        guard (x22 == x20);
                                        return x0},
                                    do {__minmaxoII x2 x3;
                                        let {x24 = Zero};
                                        let {x23 = Succ x24};
                                        let {x26 = Zero};
                                        x25 <- case x1 of
                                               {Succ y25 -> return y25; _ -> mzero};
                                        guard (x25 == x23);
                                        let {x27 = x26};
                                        let {x0 = Succ x27};
                                        return x0}]
minmaxoII x0 x1 = msum [do {let {x30 = Zero};
                            let {x29 = Succ x30};
                            let {x28 = Succ x29};
                            let {x33 = Zero};
                            let {x32 = Succ x33};
                            x31 <- case x1 of
                                   {Succ y31 -> return y31; _ -> mzero};
                            guard (x31 == x28);
                            x34 <- case x0 of
                                   {Succ y34 -> return y34; _ -> mzero};
                            guard (x34 == x32);
                            return ()},
                        do {let {x36 = Zero};
                            let {x35 = Succ x36};
                            let {x40 = Zero};
                            let {x39 = Succ x40};
                            let {x38 = Succ x39};
                            x37 <- case x1 of
                                   {Succ y37 -> return y37; _ -> mzero};
                            guard (x37 == x35);
                            x41 <- case x0 of
                                   {Succ y41 -> return y41; _ -> mzero};
                            guard (x41 == x38);
                            return ()}]