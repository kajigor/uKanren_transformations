module SortRel where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | Succ Term
    | Zero
    deriving (Show, Eq)
sortoI x0 = msum [do {let {x11 = Nil};
                      (x1, x12) <- case x0 of
                                   {Cons y1 y12 -> return (y1, y12); _ -> mzero};
                      let {x8 = x12};
                      (x2, x9) <- case x8 of
                                  {Cons y2 y9 -> return (y2, y9); _ -> mzero};
                      (x3, x10) <- case x9 of
                                   {Cons y3 y10 -> return (y3, y10); _ -> mzero};
                      x4 <- case x10 of
                            {Cons y4 y11 -> do {guard (x11 == y11); return y4}; _ -> mzero};
                      minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x1 x2 x3 x4;
                      return ()},
                  do {let {x16 = Nil};
                      (x1, x17) <- case x0 of
                                   {Cons y1 y17 -> return (y1, y17); _ -> mzero};
                      let {x13 = x17};
                      (x2, x14) <- case x13 of
                                   {Cons y2 y14 -> return (y2, y14); _ -> mzero};
                      (x3, x15) <- case x14 of
                                   {Cons y3 y15 -> return (y3, y15); _ -> mzero};
                      x4 <- case x15 of
                            {Cons y4 y16 -> do {guard (x16 == y16); return y4}; _ -> mzero};
                      _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x1 x2 x3 x4;
                      return ()}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x0 x1 x2 x3 = msum [do {x4 <- ___minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                                 _minmaxoII x0 x4;
                                                                 return ()}]
___minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- ____minmaxoMinmaxoOIII x1 x2 x3;
                                                  return x0},
                                              do {x0 <- _____minmaxoMinmaxoOIII x1 x2 x3;
                                                  return x0}]
_____minmaxoMinmaxoOIII x1 x2 x3 = msum [do {leoI x1;
                                             _________minmaxoII x2 x3;
                                             let {x0 = x1};
                                             return x0},
                                         do {let {x141 = Zero};
                                             let {x140 = Succ x141};
                                             x142 <- case x1 of
                                                     {Succ y142 -> return y142; _ -> mzero};
                                             guard (x142 == x140);
                                             x0 <- __________gtoMinmaxoOII x2 x3;
                                             return x0}]
__________gtoMinmaxoOII x1 x2 = msum [do {______minmaxoII x1 x2;
                                          let {x0 = Zero};
                                          return x0},
                                      do {x3 <- ___________gtoMinmaxoIIO x1 x2;
                                          let {x0 = Succ x3};
                                          return x0}]
___________gtoMinmaxoIIO x0 x1 = msum [do {________minmaxoII x0 x1;
                                           let {x2 = Zero};
                                           return x2}]
_________minmaxoII x0 x1 = msum [do {let {x123 = Zero};
                                     let {x126 = Zero};
                                     let {x125 = Succ x126};
                                     let {x130 = Zero};
                                     let {x129 = Succ x130};
                                     let {x128 = Succ x129};
                                     _leoI x128;
                                     x124 <- case x1 of
                                             {Succ y124 -> return y124; _ -> mzero};
                                     guard (x124 == x123);
                                     x127 <- case x0 of
                                             {Succ y127 -> return y127; _ -> mzero};
                                     guard (x127 == x125);
                                     return ()}]
________minmaxoII x0 x1 = msum [do {let {x117 = Zero};
                                    let {x119 = Zero};
                                    let {x122 = Zero};
                                    let {x121 = Succ x122};
                                    _leoI x121;
                                    x118 <- case x1 of
                                            {Succ y118 -> return y118; _ -> mzero};
                                    guard (x118 == x117);
                                    x120 <- case x0 of
                                            {Succ y120 -> return y120; _ -> mzero};
                                    guard (x120 == x119);
                                    return ()}]
______minmaxoII x0 x1 = msum [do {let {x96 = Zero};
                                  let {x98 = Zero};
                                  _leoI x98;
                                  x97 <- case x1 of
                                         {Succ y97 -> return y97; _ -> mzero};
                                  guard (x97 == x96);
                                  guard (x0 == Zero);
                                  return ()},
                              do {let {x99 = Zero};
                                  guard (x1 == Zero);
                                  x100 <- case x0 of
                                          {Succ y100 -> return y100; _ -> mzero};
                                  guard (x100 == x99);
                                  return ()}]
____minmaxoMinmaxoOIII x1 x2 x3 = msum [do {_leoI x1;
                                            ____minmaxoII x2 x3;
                                            let {x0 = x1};
                                            return x0},
                                        do {let {x138 = Zero};
                                            x139 <- case x1 of
                                                    {Succ y139 -> return y139; _ -> mzero};
                                            guard (x139 == x138);
                                            x0 <- _________gtoMinmaxoOII x2 x3;
                                            return x0}]
_________gtoMinmaxoOII x1 x2 = msum [do {__________minmaxoII x1 x2;
                                         let {x0 = Zero};
                                         return x0}]
__________minmaxoII x0 x1 = msum [do {let {x132 = Zero};
                                      let {x131 = Succ x132};
                                      let {x134 = Zero};
                                      leoI x134;
                                      x133 <- case x1 of
                                              {Succ y133 -> return y133; _ -> mzero};
                                      guard (x133 == x131);
                                      guard (x0 == Zero);
                                      return ()},
                                  do {let {x136 = Zero};
                                      let {x135 = Succ x136};
                                      guard (x1 == Zero);
                                      x137 <- case x0 of
                                              {Succ y137 -> return y137; _ -> mzero};
                                      guard (x137 == x135);
                                      return ()}]
____minmaxoII x0 x1 = msum [do {let {x76 = Zero};
                                let {x75 = Succ x76};
                                let {x78 = Zero};
                                let {x81 = Zero};
                                let {x80 = Succ x81};
                                leoI x80;
                                x77 <- case x1 of
                                       {Succ y77 -> return y77; _ -> mzero};
                                guard (x77 == x75);
                                x79 <- case x0 of
                                       {Succ y79 -> return y79; _ -> mzero};
                                guard (x79 == x78);
                                return ()},
                            do {let {x82 = Zero};
                                let {x85 = Zero};
                                let {x84 = Succ x85};
                                x83 <- case x1 of
                                       {Succ y83 -> return y83; _ -> mzero};
                                guard (x83 == x82);
                                x86 <- case x0 of
                                       {Succ y86 -> return y86; _ -> mzero};
                                guard (x86 == x84);
                                return ()}]
_leoI x0 = msum [do {guard (x0 == Zero); return ()},
                 do {let {x21 = Zero};
                     x22 <- case x0 of
                            {Succ y22 -> return y22; _ -> mzero};
                     guard (x22 == x21);
                     return ()}]
_minmaxoII x0 x1 = msum [do {let {x41 = Zero};
                             let {x40 = Succ x41};
                             let {x39 = Succ x40};
                             x42 <- case x1 of
                                    {Succ y42 -> return y42; _ -> mzero};
                             guard (x42 == x39);
                             guard (x0 == Zero);
                             return ()},
                         do {let {x45 = Zero};
                             let {x44 = Succ x45};
                             let {x43 = Succ x44};
                             guard (x1 == Zero);
                             x46 <- case x0 of
                                    {Succ y46 -> return y46; _ -> mzero};
                             guard (x46 == x43);
                             return ()}]
leoI x0 = msum [do {guard (x0 == Zero); return ()},
                do {x1 <- case x0 of
                          {Succ y1 -> return y1; _ -> mzero};
                    _leoI x1;
                    return ()}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x0 x1 x2 x3 = msum [do {x4 <- minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                                ______minmaxoII x0 x4;
                                                                return ()},
                                                            do {x4 <- _minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                                __________minmaxoII x0 x4;
                                                                return ()}]
_minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- __minmaxoMinmaxoOIII x1 x2 x3;
                                                return x0},
                                            do {x0 <- ___minmaxoMinmaxoOIII x1 x2 x3; return x0}]
___minmaxoMinmaxoOIII x1 x2 x3 = msum [do {__leoI x1;
                                           _______minmaxoII x2 x3;
                                           let {x0 = x1};
                                           return x0},
                                       do {let {x105 = Zero};
                                           let {x104 = Succ x105};
                                           let {x103 = Succ x104};
                                           x106 <- case x1 of
                                                   {Succ y106 -> return y106; _ -> mzero};
                                           guard (x106 == x103);
                                           x0 <- ______gtoMinmaxoOII x2 x3;
                                           return x0}]
_______minmaxoII x0 x1 = msum [do {let {x107 = Zero};
                                   let {x111 = Zero};
                                   let {x110 = Succ x111};
                                   let {x109 = Succ x110};
                                   let {x116 = Zero};
                                   let {x115 = Succ x116};
                                   let {x114 = Succ x115};
                                   let {x113 = Succ x114};
                                   _leoI x113;
                                   x108 <- case x1 of
                                           {Succ y108 -> return y108; _ -> mzero};
                                   guard (x108 == x107);
                                   x112 <- case x0 of
                                           {Succ y112 -> return y112; _ -> mzero};
                                   guard (x112 == x109);
                                   return ()}]
______gtoMinmaxoOII x1 x2 = msum [do {______minmaxoII x1 x2;
                                      let {x0 = Zero};
                                      return x0},
                                  do {x3 <- _______gtoMinmaxoIIO x1 x2;
                                      let {x0 = Succ x3};
                                      return x0}]
_______gtoMinmaxoIIO x0 x1 = msum [do {________minmaxoII x0 x1;
                                       let {x2 = Zero};
                                       return x2},
                                   do {x3 <- ________gtoMinmaxoIIO x0 x1;
                                       let {x2 = Succ x3};
                                       return x2}]
________gtoMinmaxoIIO x0 x1 = msum [do {_________minmaxoII x0 x1;
                                        let {x2 = Zero};
                                        return x2}]
__leoI x0 = msum [do {guard (x0 == Zero); return ()},
                  do {x1 <- case x0 of
                            {Succ y1 -> return y1; _ -> mzero};
                      leoI x1;
                      return ()}]
__minmaxoMinmaxoOIII x1 x2 x3 = msum [do {_leoI x1;
                                          __minmaxoII x2 x3;
                                          let {x0 = x1};
                                          return x0},
                                      do {let {x101 = Zero};
                                          x102 <- case x1 of
                                                  {Succ y102 -> return y102; _ -> mzero};
                                          guard (x102 == x101);
                                          x0 <- _____gtoMinmaxoOII x2 x3;
                                          return x0}]
_____gtoMinmaxoOII x1 x2 = msum [do {_minmaxoII x1 x2;
                                     let {x0 = Zero};
                                     return x0}]
__minmaxoII x0 x1 = msum [do {let {x49 = Zero};
                              let {x48 = Succ x49};
                              let {x47 = Succ x48};
                              let {x51 = Zero};
                              let {x53 = Zero};
                              leoI x53;
                              x50 <- case x1 of
                                     {Succ y50 -> return y50; _ -> mzero};
                              guard (x50 == x47);
                              x52 <- case x0 of
                                     {Succ y52 -> return y52; _ -> mzero};
                              guard (x52 == x51);
                              return ()},
                          do {let {x54 = Zero};
                              let {x58 = Zero};
                              let {x57 = Succ x58};
                              let {x56 = Succ x57};
                              x55 <- case x1 of
                                     {Succ y55 -> return y55; _ -> mzero};
                              guard (x55 == x54);
                              x59 <- case x0 of
                                     {Succ y59 -> return y59; _ -> mzero};
                              guard (x59 == x56);
                              return ()}]
minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- minmaxoMinmaxoOIII x1 x2 x3;
                                               return x0},
                                           do {x0 <- _minmaxoMinmaxoOIII x1 x2 x3; return x0}]
_minmaxoMinmaxoOIII x1 x2 x3 = msum [do {__leoI x1;
                                         ___minmaxoII x2 x3;
                                         let {x0 = x1};
                                         return x0},
                                     do {let {x62 = Zero};
                                         let {x61 = Succ x62};
                                         let {x60 = Succ x61};
                                         x63 <- case x1 of
                                                {Succ y63 -> return y63; _ -> mzero};
                                         guard (x63 == x60);
                                         x0 <- __gtoMinmaxoOII x2 x3;
                                         return x0}]
___minmaxoII x0 x1 = msum [do {let {x65 = Zero};
                               let {x64 = Succ x65};
                               let {x69 = Zero};
                               let {x68 = Succ x69};
                               let {x67 = Succ x68};
                               let {x74 = Zero};
                               let {x73 = Succ x74};
                               let {x72 = Succ x73};
                               let {x71 = Succ x72};
                               leoI x71;
                               x66 <- case x1 of
                                      {Succ y66 -> return y66; _ -> mzero};
                               guard (x66 == x64);
                               x70 <- case x0 of
                                      {Succ y70 -> return y70; _ -> mzero};
                               guard (x70 == x67);
                               return ()}]
__gtoMinmaxoOII x1 x2 = msum [do {__________minmaxoII x1 x2;
                                  let {x0 = Zero};
                                  return x0},
                              do {x3 <- ___gtoMinmaxoIIO x1 x2; let {x0 = Succ x3}; return x0}]
___gtoMinmaxoIIO x0 x1 = msum [do {____minmaxoII x0 x1;
                                   let {x2 = Zero};
                                   return x2},
                               do {x3 <- ____gtoMinmaxoIIO x0 x1; let {x2 = Succ x3}; return x2}]
____gtoMinmaxoIIO x0 x1 = msum [do {_____minmaxoII x0 x1;
                                    let {x2 = Zero};
                                    return x2}]
_____minmaxoII x0 x1 = msum [do {let {x88 = Zero};
                                 let {x87 = Succ x88};
                                 let {x91 = Zero};
                                 let {x90 = Succ x91};
                                 let {x95 = Zero};
                                 let {x94 = Succ x95};
                                 let {x93 = Succ x94};
                                 leoI x93;
                                 x89 <- case x1 of
                                        {Succ y89 -> return y89; _ -> mzero};
                                 guard (x89 == x87);
                                 x92 <- case x0 of
                                        {Succ y92 -> return y92; _ -> mzero};
                                 guard (x92 == x90);
                                 return ()}]
minmaxoMinmaxoOIII x1 x2 x3 = msum [do {leoI x1;
                                        minmaxoII x2 x3;
                                        let {x0 = x1};
                                        return x0},
                                    do {let {x19 = Zero};
                                        let {x18 = Succ x19};
                                        x20 <- case x1 of
                                               {Succ y20 -> return y20; _ -> mzero};
                                        guard (x20 == x18);
                                        x0 <- gtoMinmaxoOII x2 x3;
                                        return x0}]
gtoMinmaxoOII x1 x2 = msum [do {_minmaxoII x1 x2;
                                let {x0 = Zero};
                                return x0},
                            do {x3 <- _gtoMinmaxoIIO x1 x2; let {x0 = Succ x3}; return x0}]
_gtoMinmaxoIIO x0 x1 = msum [do {__minmaxoII x0 x1;
                                 let {x2 = Zero};
                                 return x2}]
minmaxoII x0 x1 = msum [do {let {x25 = Zero};
                            let {x24 = Succ x25};
                            let {x23 = Succ x24};
                            let {x28 = Zero};
                            let {x27 = Succ x28};
                            let {x31 = Zero};
                            let {x30 = Succ x31};
                            leoI x30;
                            x26 <- case x1 of
                                   {Succ y26 -> return y26; _ -> mzero};
                            guard (x26 == x23);
                            x29 <- case x0 of
                                   {Succ y29 -> return y29; _ -> mzero};
                            guard (x29 == x27);
                            return ()},
                        do {let {x33 = Zero};
                            let {x32 = Succ x33};
                            let {x37 = Zero};
                            let {x36 = Succ x37};
                            let {x35 = Succ x36};
                            x34 <- case x1 of
                                   {Succ y34 -> return y34; _ -> mzero};
                            guard (x34 == x32);
                            x38 <- case x0 of
                                   {Succ y38 -> return y38; _ -> mzero};
                            guard (x38 == x35);
                            return ()}]
sortoO gen_sortoO_x13 gen_sortoO_x8 = msum [do {let {x11 = Nil};
                                                (x12, x8) <- do {x8 <- gen_sortoO_x8;
                                                                 return (x8, x8)};
                                                (x2, x9) <- case x8 of
                                                            {Cons y2 y9 -> return (y2, y9);
                                                             _ -> mzero};
                                                (x3, x10) <- case x9 of
                                                             {Cons y3 y10 -> return (y3, y10);
                                                              _ -> mzero};
                                                x4 <- case x10 of
                                                      {Cons y4 y11 -> do {guard (x11 == y11);
                                                                          return y4};
                                                       _ -> mzero};
                                                x1 <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x2 x3 x4;
                                                let {x0 = Cons x1 x12};
                                                return x0},
                                            do {let {x16 = Nil};
                                                (x17, x13) <- do {x13 <- gen_sortoO_x13;
                                                                  return (x13, x13)};
                                                (x2, x14) <- case x13 of
                                                             {Cons y2 y14 -> return (y2, y14);
                                                              _ -> mzero};
                                                (x3, x15) <- case x14 of
                                                             {Cons y3 y15 -> return (y3, y15);
                                                              _ -> mzero};
                                                x4 <- case x15 of
                                                      {Cons y4 y16 -> do {guard (x16 == y16);
                                                                          return y4};
                                                       _ -> mzero};
                                                x1 <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x2 x3 x4;
                                                let {x0 = Cons x1 x17};
                                                return x0}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x4 <- ___minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                              x0 <- _minmaxoOI x4;
                                                              return x0}]
_minmaxoOI x1 = msum [do {let {x41 = Zero};
                          let {x40 = Succ x41};
                          let {x39 = Succ x40};
                          let {x0 = Zero};
                          x42 <- case x1 of
                                 {Succ y42 -> return y42; _ -> mzero};
                          guard (x42 == x39);
                          return x0},
                      do {let {x45 = Zero};
                          let {x44 = Succ x45};
                          let {x43 = Succ x44};
                          guard (x1 == Zero);
                          let {x46 = x43};
                          let {x0 = Succ x46};
                          return x0}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x4 <- minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                             x0 <- ______minmaxoOI x4;
                                                             return x0},
                                                         do {x4 <- _minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                             x0 <- __________minmaxoOI x4;
                                                             return x0}]
__________minmaxoOI x1 = msum [do {let {x132 = Zero};
                                   let {x131 = Succ x132};
                                   let {x0 = Zero};
                                   let {x134 = Zero};
                                   leoI x134;
                                   x133 <- case x1 of
                                           {Succ y133 -> return y133; _ -> mzero};
                                   guard (x133 == x131);
                                   return x0},
                               do {let {x136 = Zero};
                                   let {x135 = Succ x136};
                                   guard (x1 == Zero);
                                   let {x137 = x135};
                                   let {x0 = Succ x137};
                                   return x0}]
______minmaxoOI x1 = msum [do {let {x96 = Zero};
                               let {x0 = Zero};
                               let {x98 = Zero};
                               _leoI x98;
                               x97 <- case x1 of
                                      {Succ y97 -> return y97; _ -> mzero};
                               guard (x97 == x96);
                               return x0},
                           do {let {x99 = Zero};
                               guard (x1 == Zero);
                               let {x100 = x99};
                               let {x0 = Succ x100};
                               return x0}]