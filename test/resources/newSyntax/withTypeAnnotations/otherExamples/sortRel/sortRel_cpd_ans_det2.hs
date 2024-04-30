module SortRel_cpd_ans_det2 where

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
                      let {x5 = x9};
                      (x2, x6) <- case x5 of
                                  {Cons y2 y6 -> return (y2, y6); _ -> mzero};
                      (x3, x7) <- case x6 of
                                  {Cons y3 y7 -> return (y3, y7); _ -> mzero};
                      x4 <- case x7 of
                            {Cons y4 y8 -> do {guard (x8 == y8); return y4}; _ -> mzero};
                      minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x1 x2 x3 x4;
                      return ()},
                  do {let {x13 = Nil};
                      (x1, x14) <- case x0 of
                                   {Cons y1 y14 -> return (y1, y14); _ -> mzero};
                      let {x10 = x14};
                      (x2, x11) <- case x10 of
                                   {Cons y2 y11 -> return (y2, y11); _ -> mzero};
                      (x3, x12) <- case x11 of
                                   {Cons y3 y12 -> return (y3, y12); _ -> mzero};
                      x4 <- case x12 of
                            {Cons y4 y13 -> do {guard (x13 == y13); return y4}; _ -> mzero};
                      _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x1 x2 x3 x4;
                      return ()}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x0 x1 x2 x3 = msum [do {____minmaxoMinmaxo_minmaxoIIII x0 x1 x2 x3;
                                                                 return ()},
                                                             do {_____minmaxoMinmaxo_minmaxoIIII x0 x1 x2 x3;
                                                                 return ()}]
_____minmaxoMinmaxo_minmaxoIIII x0 x1 x2 x3 = msum [do {let {x146 = Zero};
                                                        let {x149 = Zero};
                                                        let {x148 = Succ x149};
                                                        let {x153 = Zero};
                                                        let {x152 = Succ x153};
                                                        let {x151 = Succ x152};
                                                        x147 <- case x3 of
                                                                {Succ y147 -> return y147;
                                                                 _ -> mzero};
                                                        guard (x147 == x146);
                                                        guard (x2 == Zero);
                                                        x150 <- case x1 of
                                                                {Succ y150 -> return y150;
                                                                 _ -> mzero};
                                                        guard (x150 == x148);
                                                        x154 <- case x0 of
                                                                {Succ y154 -> return y154;
                                                                 _ -> mzero};
                                                        guard (x154 == x151);
                                                        return ()},
                                                    do {let {x155 = Zero};
                                                        let {x158 = Zero};
                                                        let {x157 = Succ x158};
                                                        let {x162 = Zero};
                                                        let {x161 = Succ x162};
                                                        let {x160 = Succ x161};
                                                        guard (x3 == Zero);
                                                        x156 <- case x2 of
                                                                {Succ y156 -> return y156;
                                                                 _ -> mzero};
                                                        guard (x156 == x155);
                                                        x159 <- case x1 of
                                                                {Succ y159 -> return y159;
                                                                 _ -> mzero};
                                                        guard (x159 == x157);
                                                        x163 <- case x0 of
                                                                {Succ y163 -> return y163;
                                                                 _ -> mzero};
                                                        guard (x163 == x160);
                                                        return ()}]
____minmaxoMinmaxo_minmaxoIIII x0 x1 x2 x3 = msum [do {_leo_minmaxoII x0 x1;
                                                       ____minmaxoII x2 x3;
                                                       return ()},
                                                   do {_________gtoMinmaxo_minmaxoIII x0 x2 x3;
                                                       let {x131 = Zero};
                                                       x132 <- case x1 of
                                                               {Succ y132 -> return y132;
                                                                _ -> mzero};
                                                       guard (x132 == x131);
                                                       return ()}]
_________gtoMinmaxo_minmaxoIII x0 x1 x2 = msum [do {let {x139 = Zero};
                                                    let {x138 = Succ x139};
                                                    let {x141 = Zero};
                                                    _minmaxoII x0 x141;
                                                    x140 <- case x2 of
                                                            {Succ y140 -> return y140; _ -> mzero};
                                                    guard (x140 == x138);
                                                    guard (x1 == Zero);
                                                    return ()},
                                                do {let {x143 = Zero};
                                                    let {x142 = Succ x143};
                                                    let {x145 = Zero};
                                                    _minmaxoII x0 x145;
                                                    guard (x2 == Zero);
                                                    x144 <- case x1 of
                                                            {Succ y144 -> return y144; _ -> mzero};
                                                    guard (x144 == x142);
                                                    return ()}]
____minmaxoII x0 x1 = msum [do {let {x75 = Zero};
                                let {x74 = Succ x75};
                                let {x77 = Zero};
                                x76 <- case x1 of
                                       {Succ y76 -> return y76; _ -> mzero};
                                guard (x76 == x74);
                                x78 <- case x0 of
                                       {Succ y78 -> return y78; _ -> mzero};
                                guard (x78 == x77);
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
_leo_minmaxoII x0 x1 = msum [do {let {x133 = Zero};
                                 _minmaxoII x0 x133;
                                 guard (x1 == Zero);
                                 return ()},
                             do {let {x134 = Zero};
                                 let {x137 = Zero};
                                 let {x136 = Succ x137};
                                 _minmaxoII x0 x136;
                                 x135 <- case x1 of
                                         {Succ y135 -> return y135; _ -> mzero};
                                 guard (x135 == x134);
                                 return ()}]
_minmaxoII x0 x1 = msum [do {let {x40 = Zero};
                             let {x39 = Succ x40};
                             let {x38 = Succ x39};
                             x41 <- case x1 of
                                    {Succ y41 -> return y41; _ -> mzero};
                             guard (x41 == x38);
                             guard (x0 == Zero);
                             return ()},
                         do {let {x44 = Zero};
                             let {x43 = Succ x44};
                             let {x42 = Succ x43};
                             guard (x1 == Zero);
                             x45 <- case x0 of
                                    {Succ y45 -> return y45; _ -> mzero};
                             guard (x45 == x42);
                             return ()}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x0 x1 x2 x3 = msum [do {minmaxoMinmaxoMinmaxo______minmaxoIIII x0 x1 x2 x3;
                                                                return ()},
                                                            do {_minmaxoMinmaxoMinmaxo__________minmaxoIIII x0 x1 x2 x3;
                                                                return ()}]
_minmaxoMinmaxoMinmaxo__________minmaxoIIII x0 x1 x2 x3 = msum [do {__minmaxoMinmaxo__________minmaxoIIII x0 x1 x2 x3;
                                                                    return ()},
                                                                do {___minmaxoMinmaxo__________minmaxoIIII x0 x1 x2 x3;
                                                                    return ()}]
___minmaxoMinmaxo__________minmaxoIIII x0 x1 x2 x3 = msum [do {let {x113 = Zero};
                                                               let {x117 = Zero};
                                                               let {x116 = Succ x117};
                                                               let {x115 = Succ x116};
                                                               let {x120 = Zero};
                                                               let {x119 = Succ x120};
                                                               x114 <- case x3 of
                                                                       {Succ y114 -> return y114;
                                                                        _ -> mzero};
                                                               guard (x114 == x113);
                                                               guard (x2 == Zero);
                                                               x118 <- case x1 of
                                                                       {Succ y118 -> return y118;
                                                                        _ -> mzero};
                                                               guard (x118 == x115);
                                                               x121 <- case x0 of
                                                                       {Succ y121 -> return y121;
                                                                        _ -> mzero};
                                                               guard (x121 == x119);
                                                               return ()},
                                                           do {let {x122 = Zero};
                                                               let {x126 = Zero};
                                                               let {x125 = Succ x126};
                                                               let {x124 = Succ x125};
                                                               let {x129 = Zero};
                                                               let {x128 = Succ x129};
                                                               guard (x3 == Zero);
                                                               x123 <- case x2 of
                                                                       {Succ y123 -> return y123;
                                                                        _ -> mzero};
                                                               guard (x123 == x122);
                                                               x127 <- case x1 of
                                                                       {Succ y127 -> return y127;
                                                                        _ -> mzero};
                                                               guard (x127 == x124);
                                                               x130 <- case x0 of
                                                                       {Succ y130 -> return y130;
                                                                        _ -> mzero};
                                                               guard (x130 == x128);
                                                               return ()}]
__minmaxoMinmaxo__________minmaxoIIII x0 x1 x2 x3 = msum [do {_leo__________minmaxoII x0 x1;
                                                              __minmaxoII x2 x3;
                                                              return ()},
                                                          do {_____gtoMinmaxo__________minmaxoIII x0 x2 x3;
                                                              let {x84 = Zero};
                                                              x85 <- case x1 of
                                                                     {Succ y85 -> return y85;
                                                                      _ -> mzero};
                                                              guard (x85 == x84);
                                                              return ()}]
_____gtoMinmaxo__________minmaxoIII x0 x1 x2 = msum [do {let {x105 = Zero};
                                                         let {x104 = Succ x105};
                                                         let {x103 = Succ x104};
                                                         let {x107 = Zero};
                                                         __________minmaxoII x0 x107;
                                                         x106 <- case x2 of
                                                                 {Succ y106 -> return y106;
                                                                  _ -> mzero};
                                                         guard (x106 == x103);
                                                         guard (x1 == Zero);
                                                         return ()},
                                                     do {let {x110 = Zero};
                                                         let {x109 = Succ x110};
                                                         let {x108 = Succ x109};
                                                         let {x112 = Zero};
                                                         __________minmaxoII x0 x112;
                                                         guard (x2 == Zero);
                                                         x111 <- case x1 of
                                                                 {Succ y111 -> return y111;
                                                                  _ -> mzero};
                                                         guard (x111 == x108);
                                                         return ()}]
__________minmaxoII x0 x1 = msum [do {let {x69 = Zero};
                                      let {x68 = Succ x69};
                                      x70 <- case x1 of
                                             {Succ y70 -> return y70; _ -> mzero};
                                      guard (x70 == x68);
                                      guard (x0 == Zero);
                                      return ()},
                                  do {let {x72 = Zero};
                                      let {x71 = Succ x72};
                                      guard (x1 == Zero);
                                      x73 <- case x0 of
                                             {Succ y73 -> return y73; _ -> mzero};
                                      guard (x73 == x71);
                                      return ()}]
__minmaxoII x0 x1 = msum [do {let {x93 = Zero};
                              let {x92 = Succ x93};
                              let {x91 = Succ x92};
                              let {x95 = Zero};
                              x94 <- case x1 of
                                     {Succ y94 -> return y94; _ -> mzero};
                              guard (x94 == x91);
                              x96 <- case x0 of
                                     {Succ y96 -> return y96; _ -> mzero};
                              guard (x96 == x95);
                              return ()},
                          do {let {x97 = Zero};
                              let {x101 = Zero};
                              let {x100 = Succ x101};
                              let {x99 = Succ x100};
                              x98 <- case x1 of
                                     {Succ y98 -> return y98; _ -> mzero};
                              guard (x98 == x97);
                              x102 <- case x0 of
                                      {Succ y102 -> return y102; _ -> mzero};
                              guard (x102 == x99);
                              return ()}]
_leo__________minmaxoII x0 x1 = msum [do {let {x86 = Zero};
                                          __________minmaxoII x0 x86;
                                          guard (x1 == Zero);
                                          return ()},
                                      do {let {x87 = Zero};
                                          let {x90 = Zero};
                                          let {x89 = Succ x90};
                                          __________minmaxoII x0 x89;
                                          x88 <- case x1 of
                                                 {Succ y88 -> return y88; _ -> mzero};
                                          guard (x88 == x87);
                                          return ()}]
minmaxoMinmaxoMinmaxo______minmaxoIIII x0 x1 x2 x3 = msum [do {minmaxoMinmaxo______minmaxoIIII x0 x1 x2 x3;
                                                               return ()},
                                                           do {_minmaxoMinmaxo______minmaxoIIII x0 x1 x2 x3;
                                                               return ()}]
_minmaxoMinmaxo______minmaxoIIII x0 x1 x2 x3 = msum [do {__________minmaxoII x2 x3;
                                                         let {x60 = Zero};
                                                         let {x59 = Succ x60};
                                                         let {x58 = Succ x59};
                                                         let {x62 = Zero};
                                                         x61 <- case x1 of
                                                                {Succ y61 -> return y61;
                                                                 _ -> mzero};
                                                         guard (x61 == x58);
                                                         x63 <- case x0 of
                                                                {Succ y63 -> return y63;
                                                                 _ -> mzero};
                                                         guard (x63 == x62);
                                                         return ()},
                                                     do {____minmaxoII x2 x3;
                                                         let {x66 = Zero};
                                                         let {x65 = Succ x66};
                                                         let {x64 = Succ x65};
                                                         x67 <- case x1 of
                                                                {Succ y67 -> return y67;
                                                                 _ -> mzero};
                                                         guard (x67 == x64);
                                                         guard (x0 == Zero);
                                                         return ()}]
minmaxoMinmaxo______minmaxoIIII x0 x1 x2 x3 = msum [do {leo______minmaxoII x0 x1;
                                                        minmaxoII x2 x3;
                                                        return ()},
                                                    do {gtoMinmaxo______minmaxoIII x0 x2 x3;
                                                        let {x16 = Zero};
                                                        let {x15 = Succ x16};
                                                        x17 <- case x1 of
                                                               {Succ y17 -> return y17; _ -> mzero};
                                                        guard (x17 == x15);
                                                        return ()}]
gtoMinmaxo______minmaxoIII x0 x1 x2 = msum [do {_minmaxoII x1 x2;
                                                let {x36 = Zero};
                                                x37 <- case x0 of
                                                       {Succ y37 -> return y37; _ -> mzero};
                                                guard (x37 == x36);
                                                return ()},
                                            do {_gtoMinmaxoII x1 x2; guard (x0 == Zero); return ()}]
_gtoMinmaxoII x0 x1 = msum [do {let {x48 = Zero};
                                let {x47 = Succ x48};
                                let {x46 = Succ x47};
                                let {x50 = Zero};
                                x49 <- case x1 of
                                       {Succ y49 -> return y49; _ -> mzero};
                                guard (x49 == x46);
                                x51 <- case x0 of
                                       {Succ y51 -> return y51; _ -> mzero};
                                guard (x51 == x50);
                                return ()},
                            do {let {x52 = Zero};
                                let {x56 = Zero};
                                let {x55 = Succ x56};
                                let {x54 = Succ x55};
                                x53 <- case x1 of
                                       {Succ y53 -> return y53; _ -> mzero};
                                guard (x53 == x52);
                                x57 <- case x0 of
                                       {Succ y57 -> return y57; _ -> mzero};
                                guard (x57 == x54);
                                return ()}]
leo______minmaxoII x0 x1 = msum [do {let {x18 = Zero};
                                     guard (x1 == Zero);
                                     x19 <- case x0 of
                                            {Succ y19 -> return y19; _ -> mzero};
                                     guard (x19 == x18);
                                     return ()},
                                 do {let {x20 = Zero};
                                     x21 <- case x1 of
                                            {Succ y21 -> return y21; _ -> mzero};
                                     guard (x21 == x20);
                                     guard (x0 == Zero);
                                     return ()}]
minmaxoII x0 x1 = msum [do {let {x24 = Zero};
                            let {x23 = Succ x24};
                            let {x22 = Succ x23};
                            let {x27 = Zero};
                            let {x26 = Succ x27};
                            x25 <- case x1 of
                                   {Succ y25 -> return y25; _ -> mzero};
                            guard (x25 == x22);
                            x28 <- case x0 of
                                   {Succ y28 -> return y28; _ -> mzero};
                            guard (x28 == x26);
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
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- ____minmaxoMinmaxo_minmaxoOIII x1 x2 x3;
                                                              return x0},
                                                          do {x0 <- _____minmaxoMinmaxo_minmaxoOIII x1 x2 x3;
                                                              return x0}]
_____minmaxoMinmaxo_minmaxoOIII x1 x2 x3 = msum [do {let {x146 = Zero};
                                                     let {x149 = Zero};
                                                     let {x148 = Succ x149};
                                                     let {x153 = Zero};
                                                     let {x152 = Succ x153};
                                                     let {x151 = Succ x152};
                                                     x147 <- case x3 of
                                                             {Succ y147 -> return y147; _ -> mzero};
                                                     guard (x147 == x146);
                                                     guard (x2 == Zero);
                                                     x150 <- case x1 of
                                                             {Succ y150 -> return y150; _ -> mzero};
                                                     guard (x150 == x148);
                                                     let {x154 = x151};
                                                     let {x0 = Succ x154};
                                                     return x0},
                                                 do {let {x155 = Zero};
                                                     let {x158 = Zero};
                                                     let {x157 = Succ x158};
                                                     let {x162 = Zero};
                                                     let {x161 = Succ x162};
                                                     let {x160 = Succ x161};
                                                     guard (x3 == Zero);
                                                     x156 <- case x2 of
                                                             {Succ y156 -> return y156; _ -> mzero};
                                                     guard (x156 == x155);
                                                     x159 <- case x1 of
                                                             {Succ y159 -> return y159; _ -> mzero};
                                                     guard (x159 == x157);
                                                     let {x163 = x160};
                                                     let {x0 = Succ x163};
                                                     return x0}]
____minmaxoMinmaxo_minmaxoOIII x1 x2 x3 = msum [do {____minmaxoII x2 x3;
                                                    x0 <- _leo_minmaxoOI x1;
                                                    return x0},
                                                do {let {x131 = Zero};
                                                    x132 <- case x1 of
                                                            {Succ y132 -> return y132; _ -> mzero};
                                                    guard (x132 == x131);
                                                    x0 <- _________gtoMinmaxo_minmaxoOII x2 x3;
                                                    return x0}]
_________gtoMinmaxo_minmaxoOII x1 x2 = msum [do {let {x139 = Zero};
                                                 let {x138 = Succ x139};
                                                 let {x141 = Zero};
                                                 x140 <- case x2 of
                                                         {Succ y140 -> return y140; _ -> mzero};
                                                 guard (x140 == x138);
                                                 guard (x1 == Zero);
                                                 x0 <- _minmaxoOI x141;
                                                 return x0},
                                             do {let {x143 = Zero};
                                                 let {x142 = Succ x143};
                                                 let {x145 = Zero};
                                                 guard (x2 == Zero);
                                                 x144 <- case x1 of
                                                         {Succ y144 -> return y144; _ -> mzero};
                                                 guard (x144 == x142);
                                                 x0 <- _minmaxoOI x145;
                                                 return x0}]
_leo_minmaxoOI x1 = msum [do {let {x133 = Zero};
                              guard (x1 == Zero);
                              x0 <- _minmaxoOI x133;
                              return x0},
                          do {let {x134 = Zero};
                              let {x137 = Zero};
                              let {x136 = Succ x137};
                              x135 <- case x1 of
                                      {Succ y135 -> return y135; _ -> mzero};
                              guard (x135 == x134);
                              x0 <- _minmaxoOI x136;
                              return x0}]
_minmaxoOI x1 = msum [do {let {x40 = Zero};
                          let {x39 = Succ x40};
                          let {x38 = Succ x39};
                          let {x0 = Zero};
                          x41 <- case x1 of
                                 {Succ y41 -> return y41; _ -> mzero};
                          guard (x41 == x38);
                          return x0},
                      do {let {x44 = Zero};
                          let {x43 = Succ x44};
                          let {x42 = Succ x43};
                          guard (x1 == Zero);
                          let {x45 = x42};
                          let {x0 = Succ x45};
                          return x0}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 = msum [do {x0 <- minmaxoMinmaxoMinmaxo______minmaxoOIII x1 x2 x3;
                                                             return x0},
                                                         do {x0 <- _minmaxoMinmaxoMinmaxo__________minmaxoOIII x1 x2 x3;
                                                             return x0}]
_minmaxoMinmaxoMinmaxo__________minmaxoOIII x1 x2 x3 = msum [do {x0 <- __minmaxoMinmaxo__________minmaxoOIII x1 x2 x3;
                                                                 return x0},
                                                             do {x0 <- ___minmaxoMinmaxo__________minmaxoOIII x1 x2 x3;
                                                                 return x0}]
___minmaxoMinmaxo__________minmaxoOIII x1 x2 x3 = msum [do {let {x113 = Zero};
                                                            let {x117 = Zero};
                                                            let {x116 = Succ x117};
                                                            let {x115 = Succ x116};
                                                            let {x120 = Zero};
                                                            let {x119 = Succ x120};
                                                            x114 <- case x3 of
                                                                    {Succ y114 -> return y114;
                                                                     _ -> mzero};
                                                            guard (x114 == x113);
                                                            guard (x2 == Zero);
                                                            x118 <- case x1 of
                                                                    {Succ y118 -> return y118;
                                                                     _ -> mzero};
                                                            guard (x118 == x115);
                                                            let {x121 = x119};
                                                            let {x0 = Succ x121};
                                                            return x0},
                                                        do {let {x122 = Zero};
                                                            let {x126 = Zero};
                                                            let {x125 = Succ x126};
                                                            let {x124 = Succ x125};
                                                            let {x129 = Zero};
                                                            let {x128 = Succ x129};
                                                            guard (x3 == Zero);
                                                            x123 <- case x2 of
                                                                    {Succ y123 -> return y123;
                                                                     _ -> mzero};
                                                            guard (x123 == x122);
                                                            x127 <- case x1 of
                                                                    {Succ y127 -> return y127;
                                                                     _ -> mzero};
                                                            guard (x127 == x124);
                                                            let {x130 = x128};
                                                            let {x0 = Succ x130};
                                                            return x0}]
__minmaxoMinmaxo__________minmaxoOIII x1 x2 x3 = msum [do {__minmaxoII x2 x3;
                                                           x0 <- _leo__________minmaxoOI x1;
                                                           return x0},
                                                       do {let {x84 = Zero};
                                                           x85 <- case x1 of
                                                                  {Succ y85 -> return y85;
                                                                   _ -> mzero};
                                                           guard (x85 == x84);
                                                           x0 <- _____gtoMinmaxo__________minmaxoOII x2 x3;
                                                           return x0}]
_____gtoMinmaxo__________minmaxoOII x1 x2 = msum [do {let {x105 = Zero};
                                                      let {x104 = Succ x105};
                                                      let {x103 = Succ x104};
                                                      let {x107 = Zero};
                                                      x106 <- case x2 of
                                                              {Succ y106 -> return y106;
                                                               _ -> mzero};
                                                      guard (x106 == x103);
                                                      guard (x1 == Zero);
                                                      x0 <- __________minmaxoOI x107;
                                                      return x0},
                                                  do {let {x110 = Zero};
                                                      let {x109 = Succ x110};
                                                      let {x108 = Succ x109};
                                                      let {x112 = Zero};
                                                      guard (x2 == Zero);
                                                      x111 <- case x1 of
                                                              {Succ y111 -> return y111;
                                                               _ -> mzero};
                                                      guard (x111 == x108);
                                                      x0 <- __________minmaxoOI x112;
                                                      return x0}]
__________minmaxoOI x1 = msum [do {let {x69 = Zero};
                                   let {x68 = Succ x69};
                                   let {x0 = Zero};
                                   x70 <- case x1 of
                                          {Succ y70 -> return y70; _ -> mzero};
                                   guard (x70 == x68);
                                   return x0},
                               do {let {x72 = Zero};
                                   let {x71 = Succ x72};
                                   guard (x1 == Zero);
                                   let {x73 = x71};
                                   let {x0 = Succ x73};
                                   return x0}]
_leo__________minmaxoOI x1 = msum [do {let {x86 = Zero};
                                       guard (x1 == Zero);
                                       x0 <- __________minmaxoOI x86;
                                       return x0},
                                   do {let {x87 = Zero};
                                       let {x90 = Zero};
                                       let {x89 = Succ x90};
                                       x88 <- case x1 of
                                              {Succ y88 -> return y88; _ -> mzero};
                                       guard (x88 == x87);
                                       x0 <- __________minmaxoOI x89;
                                       return x0}]
minmaxoMinmaxoMinmaxo______minmaxoOIII x1 x2 x3 = msum [do {x0 <- minmaxoMinmaxo______minmaxoOIII x1 x2 x3;
                                                            return x0},
                                                        do {x0 <- _minmaxoMinmaxo______minmaxoOIII x1 x2 x3;
                                                            return x0}]
_minmaxoMinmaxo______minmaxoOIII x1 x2 x3 = msum [do {__________minmaxoII x2 x3;
                                                      let {x60 = Zero};
                                                      let {x59 = Succ x60};
                                                      let {x58 = Succ x59};
                                                      let {x62 = Zero};
                                                      x61 <- case x1 of
                                                             {Succ y61 -> return y61; _ -> mzero};
                                                      guard (x61 == x58);
                                                      let {x63 = x62};
                                                      let {x0 = Succ x63};
                                                      return x0},
                                                  do {____minmaxoII x2 x3;
                                                      let {x66 = Zero};
                                                      let {x65 = Succ x66};
                                                      let {x64 = Succ x65};
                                                      let {x0 = Zero};
                                                      x67 <- case x1 of
                                                             {Succ y67 -> return y67; _ -> mzero};
                                                      guard (x67 == x64);
                                                      return x0}]
minmaxoMinmaxo______minmaxoOIII x1 x2 x3 = msum [do {minmaxoII x2 x3;
                                                     x0 <- leo______minmaxoOI x1;
                                                     return x0},
                                                 do {let {x16 = Zero};
                                                     let {x15 = Succ x16};
                                                     x17 <- case x1 of
                                                            {Succ y17 -> return y17; _ -> mzero};
                                                     guard (x17 == x15);
                                                     x0 <- gtoMinmaxo______minmaxoOII x2 x3;
                                                     return x0}]
gtoMinmaxo______minmaxoOII x1 x2 = msum [do {_minmaxoII x1 x2;
                                             let {x36 = Zero};
                                             let {x37 = x36};
                                             let {x0 = Succ x37};
                                             return x0},
                                         do {_gtoMinmaxoII x1 x2; let {x0 = Zero}; return x0}]
leo______minmaxoOI x1 = msum [do {let {x18 = Zero};
                                  guard (x1 == Zero);
                                  let {x19 = x18};
                                  let {x0 = Succ x19};
                                  return x0},
                              do {let {x20 = Zero};
                                  let {x0 = Zero};
                                  x21 <- case x1 of
                                         {Succ y21 -> return y21; _ -> mzero};
                                  guard (x21 == x20);
                                  return x0}]