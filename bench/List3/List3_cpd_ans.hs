module List3_cpd_ans where

import Stream
import Control.Monad (msum, guard, MonadPlus)
import Term

a = Nil
b = Nil
c = Nil

tp1 :: [()]
tp1 = (takeS 1) $ helpIII a b c
tp2 :: [Term]
tp2 = (takeS 1) $ helpIIO a b natGen
tp3 :: [Term]
tp3 = (takeS 1) $ helpIOI a c
tp4 :: [Term]
tp4 = (takeS 1) $ helpOII b c
tp5 :: [(Term, Term)]
tp5 = (takeS 1) $ helpIOO a natGen
tp6 :: [(Term, Term)]
tp6 = (takeS 1) $ helpOIO b natGen
tp7 :: [(Term, Term)]
tp7 = (takeS 1) $ helpOOI c natGen
tp8 :: [(Term, Term, Term)]
tp8 = (takeS 1) $ helpOOO natGen natGen

helpIII x0 x1 x2 = msum [do {appendoII x0 x1;
                             let {x15 = Zero};
                             let {x14 = Succ x15};
                             let {x13 = Succ x14};
                             (x25, x26) <- case x2 of
                                           {Cons y25 y26 -> return (y25, y26); _ -> mzero};
                             guard (x25 == x13);
                             let {x16 = x26};
                             (x3, x17) <- case x16 of
                                          {Cons y3 y17 -> return (y3, y17); _ -> mzero};
                             (x4, x18) <- case x17 of
                                          {Cons y4 y18 -> return (y4, y18); _ -> mzero};
                             (x5, x19) <- case x18 of
                                          {Cons y5 y19 -> return (y5, y19); _ -> mzero};
                             (x6, x20) <- case x19 of
                                          {Cons y6 y20 -> return (y6, y20); _ -> mzero};
                             (x7, x21) <- case x20 of
                                          {Cons y7 y21 -> return (y7, y21); _ -> mzero};
                             (x8, x22) <- case x21 of
                                          {Cons y8 y22 -> return (y8, y22); _ -> mzero};
                             (x9, x23) <- case x22 of
                                          {Cons y9 y23 -> return (y9, y23); _ -> mzero};
                             (x10, x24) <- case x23 of
                                           {Cons y10 y24 -> return (y10, y24); _ -> mzero};
                             (x11, x12) <- case x24 of
                                           {Cons y11 y12 -> return (y11, y12); _ -> mzero};
                             appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIIII x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
                             return ()}]
appendoII x0 x1 = msum [do {let {x41 = Zero};
                            let {x40 = Succ x41};
                            let {x39 = Succ x40};
                            let {x43 = Zero};
                            let {x46 = Zero};
                            let {x45 = Succ x46};
                            let {x49 = Zero};
                            let {x48 = Succ x49};
                            let {x51 = Zero};
                            let {x55 = Zero};
                            let {x54 = Succ x55};
                            let {x53 = Succ x54};
                            let {x58 = Zero};
                            let {x57 = Succ x58};
                            let {x60 = Zero};
                            let {x62 = Zero};
                            let {x66 = Zero};
                            let {x65 = Succ x66};
                            let {x64 = Succ x65};
                            let {x67 = Nil};
                            let {x63 = Cons x64 x67};
                            let {x61 = Cons x62 x63};
                            let {x59 = Cons x60 x61};
                            let {x56 = Cons x57 x59};
                            let {x52 = Cons x53 x56};
                            let {x50 = Cons x51 x52};
                            let {x47 = Cons x48 x50};
                            let {x44 = Cons x45 x47};
                            let {x42 = Cons x43 x44};
                            (x68, x69) <- case x1 of
                                          {Cons y68 y69 -> return (y68, y69); _ -> mzero};
                            guard (x68 == x39);
                            guard (x69 == x42);
                            guard (x0 == Nil);
                            return ()},
                        do {let {x72 = Zero};
                            let {x71 = Succ x72};
                            let {x70 = Succ x71};
                            (x73, x2) <- case x0 of
                                         {Cons y73 y2 -> return (y73, y2); _ -> mzero};
                            guard (x73 == x70);
                            _appendoII x1 x2;
                            return ()}]
_appendoII x0 x1 = msum [do {let {x74 = Zero};
                             let {x77 = Zero};
                             let {x76 = Succ x77};
                             let {x80 = Zero};
                             let {x79 = Succ x80};
                             let {x82 = Zero};
                             let {x86 = Zero};
                             let {x85 = Succ x86};
                             let {x84 = Succ x85};
                             let {x89 = Zero};
                             let {x88 = Succ x89};
                             let {x91 = Zero};
                             let {x93 = Zero};
                             let {x97 = Zero};
                             let {x96 = Succ x97};
                             let {x95 = Succ x96};
                             let {x98 = Nil};
                             let {x94 = Cons x95 x98};
                             let {x92 = Cons x93 x94};
                             let {x90 = Cons x91 x92};
                             let {x87 = Cons x88 x90};
                             let {x83 = Cons x84 x87};
                             let {x81 = Cons x82 x83};
                             let {x78 = Cons x79 x81};
                             let {x75 = Cons x76 x78};
                             guard (x1 == Nil);
                             (x99, x100) <- case x0 of
                                            {Cons y99 y100 -> return (y99, y100); _ -> mzero};
                             guard (x99 == x74);
                             guard (x100 == x75);
                             return ()},
                         do {let {x101 = Zero};
                             (x102, x2) <- case x1 of
                                           {Cons y102 y2 -> return (y102, y2); _ -> mzero};
                             guard (x102 == x101);
                             __appendoII x0 x2;
                             return ()}]
__appendoII x0 x1 = msum [do {let {x104 = Zero};
                              let {x103 = Succ x104};
                              let {x107 = Zero};
                              let {x106 = Succ x107};
                              let {x109 = Zero};
                              let {x113 = Zero};
                              let {x112 = Succ x113};
                              let {x111 = Succ x112};
                              let {x116 = Zero};
                              let {x115 = Succ x116};
                              let {x118 = Zero};
                              let {x120 = Zero};
                              let {x124 = Zero};
                              let {x123 = Succ x124};
                              let {x122 = Succ x123};
                              let {x125 = Nil};
                              let {x121 = Cons x122 x125};
                              let {x119 = Cons x120 x121};
                              let {x117 = Cons x118 x119};
                              let {x114 = Cons x115 x117};
                              let {x110 = Cons x111 x114};
                              let {x108 = Cons x109 x110};
                              let {x105 = Cons x106 x108};
                              guard (x1 == Nil);
                              (x126, x127) <- case x0 of
                                              {Cons y126 y127 -> return (y126, y127); _ -> mzero};
                              guard (x126 == x103);
                              guard (x127 == x105);
                              return ()},
                          do {let {x129 = Zero};
                              let {x128 = Succ x129};
                              (x130, x2) <- case x1 of
                                            {Cons y130 y2 -> return (y130, y2); _ -> mzero};
                              guard (x130 == x128);
                              ___appendoII x0 x2;
                              return ()}]
___appendoII x0 x1 = msum [do {let {x132 = Zero};
                               let {x131 = Succ x132};
                               let {x134 = Zero};
                               let {x138 = Zero};
                               let {x137 = Succ x138};
                               let {x136 = Succ x137};
                               let {x141 = Zero};
                               let {x140 = Succ x141};
                               let {x143 = Zero};
                               let {x145 = Zero};
                               let {x149 = Zero};
                               let {x148 = Succ x149};
                               let {x147 = Succ x148};
                               let {x150 = Nil};
                               let {x146 = Cons x147 x150};
                               let {x144 = Cons x145 x146};
                               let {x142 = Cons x143 x144};
                               let {x139 = Cons x140 x142};
                               let {x135 = Cons x136 x139};
                               let {x133 = Cons x134 x135};
                               guard (x1 == Nil);
                               (x151, x152) <- case x0 of
                                               {Cons y151 y152 -> return (y151, y152); _ -> mzero};
                               guard (x151 == x131);
                               guard (x152 == x133);
                               return ()},
                           do {let {x154 = Zero};
                               let {x153 = Succ x154};
                               (x155, x2) <- case x1 of
                                             {Cons y155 y2 -> return (y155, y2); _ -> mzero};
                               guard (x155 == x153);
                               ____appendoII x0 x2;
                               return ()}]
____appendoII x0 x1 = msum [do {let {x156 = Zero};
                                let {x160 = Zero};
                                let {x159 = Succ x160};
                                let {x158 = Succ x159};
                                let {x163 = Zero};
                                let {x162 = Succ x163};
                                let {x165 = Zero};
                                let {x167 = Zero};
                                let {x171 = Zero};
                                let {x170 = Succ x171};
                                let {x169 = Succ x170};
                                let {x172 = Nil};
                                let {x168 = Cons x169 x172};
                                let {x166 = Cons x167 x168};
                                let {x164 = Cons x165 x166};
                                let {x161 = Cons x162 x164};
                                let {x157 = Cons x158 x161};
                                guard (x1 == Nil);
                                (x173, x174) <- case x0 of
                                                {Cons y173 y174 -> return (y173, y174); _ -> mzero};
                                guard (x173 == x156);
                                guard (x174 == x157);
                                return ()},
                            do {let {x175 = Zero};
                                (x176, x2) <- case x1 of
                                              {Cons y176 y2 -> return (y176, y2); _ -> mzero};
                                guard (x176 == x175);
                                _____appendoII x0 x2;
                                return ()}]
_____appendoII x0 x1 = msum [do {let {x179 = Zero};
                                 let {x178 = Succ x179};
                                 let {x177 = Succ x178};
                                 let {x182 = Zero};
                                 let {x181 = Succ x182};
                                 let {x184 = Zero};
                                 let {x186 = Zero};
                                 let {x190 = Zero};
                                 let {x189 = Succ x190};
                                 let {x188 = Succ x189};
                                 let {x191 = Nil};
                                 let {x187 = Cons x188 x191};
                                 let {x185 = Cons x186 x187};
                                 let {x183 = Cons x184 x185};
                                 let {x180 = Cons x181 x183};
                                 guard (x1 == Nil);
                                 (x192, x193) <- case x0 of
                                                 {Cons y192 y193 -> return (y192, y193);
                                                  _ -> mzero};
                                 guard (x192 == x177);
                                 guard (x193 == x180);
                                 return ()},
                             do {let {x196 = Zero};
                                 let {x195 = Succ x196};
                                 let {x194 = Succ x195};
                                 (x197, x2) <- case x1 of
                                               {Cons y197 y2 -> return (y197, y2); _ -> mzero};
                                 guard (x197 == x194);
                                 ______appendoII x0 x2;
                                 return ()}]
______appendoII x0 x1 = msum [do {let {x199 = Zero};
                                  let {x198 = Succ x199};
                                  let {x201 = Zero};
                                  let {x203 = Zero};
                                  let {x207 = Zero};
                                  let {x206 = Succ x207};
                                  let {x205 = Succ x206};
                                  let {x208 = Nil};
                                  let {x204 = Cons x205 x208};
                                  let {x202 = Cons x203 x204};
                                  let {x200 = Cons x201 x202};
                                  guard (x1 == Nil);
                                  (x209, x210) <- case x0 of
                                                  {Cons y209 y210 -> return (y209, y210);
                                                   _ -> mzero};
                                  guard (x209 == x198);
                                  guard (x210 == x200);
                                  return ()},
                              do {let {x212 = Zero};
                                  let {x211 = Succ x212};
                                  (x213, x2) <- case x1 of
                                                {Cons y213 y2 -> return (y213, y2); _ -> mzero};
                                  guard (x213 == x211);
                                  _______appendoII x0 x2;
                                  return ()}]
_______appendoII x0 x1 = msum [do {let {x214 = Zero};
                                   let {x216 = Zero};
                                   let {x220 = Zero};
                                   let {x219 = Succ x220};
                                   let {x218 = Succ x219};
                                   let {x221 = Nil};
                                   let {x217 = Cons x218 x221};
                                   let {x215 = Cons x216 x217};
                                   guard (x1 == Nil);
                                   (x222, x223) <- case x0 of
                                                   {Cons y222 y223 -> return (y222, y223);
                                                    _ -> mzero};
                                   guard (x222 == x214);
                                   guard (x223 == x215);
                                   return ()},
                               do {let {x224 = Zero};
                                   (x225, x2) <- case x1 of
                                                 {Cons y225 y2 -> return (y225, y2); _ -> mzero};
                                   guard (x225 == x224);
                                   ________appendoII x0 x2;
                                   return ()}]
________appendoII x0 x1 = msum [do {let {x226 = Zero};
                                    let {x230 = Zero};
                                    let {x229 = Succ x230};
                                    let {x228 = Succ x229};
                                    let {x231 = Nil};
                                    let {x227 = Cons x228 x231};
                                    guard (x1 == Nil);
                                    (x232, x233) <- case x0 of
                                                    {Cons y232 y233 -> return (y232, y233);
                                                     _ -> mzero};
                                    guard (x232 == x226);
                                    guard (x233 == x227);
                                    return ()},
                                do {let {x234 = Zero};
                                    (x235, x2) <- case x1 of
                                                  {Cons y235 y2 -> return (y235, y2); _ -> mzero};
                                    guard (x235 == x234);
                                    _________appendoII x0 x2;
                                    return ()}]
_________appendoII x0 x1 = msum [do {let {x238 = Zero};
                                     let {x237 = Succ x238};
                                     let {x236 = Succ x237};
                                     let {x239 = Nil};
                                     guard (x1 == Nil);
                                     (x240, x241) <- case x0 of
                                                     {Cons y240 y241 -> return (y240, y241);
                                                      _ -> mzero};
                                     guard (x240 == x236);
                                     guard (x241 == x239);
                                     return ()},
                                 do {let {x244 = Zero};
                                     let {x243 = Succ x244};
                                     let {x242 = Succ x243};
                                     let {x245 = Nil};
                                     (x246, x247) <- case x1 of
                                                     {Cons y246 y247 -> return (y246, y247);
                                                      _ -> mzero};
                                     guard (x246 == x242);
                                     guard (x247 == x245);
                                     guard (x0 == Nil);
                                     return ()}]
appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIIII x0 x1 x2 x3 x4 x5 x6 x7 x8 x9 = msum [do {appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIII x1 x2 x3 x4 x5 x6 x7 x8 x9;
                                                                                                                           guard (x0 == Zero);
                                                                                                                           return ()}]
appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIII x0 x1 x2 x3 x4 x5 x6 x7 x8 = msum [do {appendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIII x1 x2 x3 x4 x5 x6 x7 x8;
                                                                                                                guard (x0 == Zero);
                                                                                                                return ()}]
appendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIII x0 x1 x2 x3 x4 x5 x6 x7 = msum [do {appendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIII x1 x2 x3 x4 x5 x6 x7;
                                                                                                     let {x27 = Zero};
                                                                                                     x28 <- case x0 of
                                                                                                            {Succ y28 -> return y28;
                                                                                                             _ -> mzero};
                                                                                                     guard (x28 == x27);
                                                                                                     return ()}]
appendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIII x0 x1 x2 x3 x4 x5 x6 = msum [do {appendoAppendoAppendoAppendoAppendoReversoIIIIII x1 x2 x3 x4 x5 x6;
                                                                                          let {x30 = Zero};
                                                                                          let {x29 = Succ x30};
                                                                                          x31 <- case x0 of
                                                                                                 {Succ y31 -> return y31;
                                                                                                  _ -> mzero};
                                                                                          guard (x31 == x29);
                                                                                          return ()}]
appendoAppendoAppendoAppendoAppendoReversoIIIIII x0 x1 x2 x3 x4 x5 = msum [do {appendoAppendoAppendoAppendoReversoIIIII x1 x2 x3 x4 x5;
                                                                               guard (x0 == Zero);
                                                                               return ()}]
appendoAppendoAppendoAppendoReversoIIIII x0 x1 x2 x3 x4 = msum [do {appendoAppendoAppendoReversoIIII x1 x2 x3 x4;
                                                                    let {x32 = Zero};
                                                                    x33 <- case x0 of
                                                                           {Succ y33 -> return y33;
                                                                            _ -> mzero};
                                                                    guard (x33 == x32);
                                                                    return ()}]
appendoAppendoAppendoReversoIIII x0 x1 x2 x3 = msum [do {appendoAppendoReversoIII x1 x2 x3;
                                                         let {x34 = Zero};
                                                         x35 <- case x0 of
                                                                {Succ y35 -> return y35;
                                                                 _ -> mzero};
                                                         guard (x35 == x34);
                                                         return ()}]
appendoAppendoReversoIII x0 x1 x2 = msum [do {appendoReversoII x1 x2;
                                              guard (x0 == Zero);
                                              return ()}]
appendoReversoII x0 x1 = msum [do {reversoI x1;
                                   let {x37 = Zero};
                                   let {x36 = Succ x37};
                                   x38 <- case x0 of
                                          {Succ y38 -> return y38; _ -> mzero};
                                   guard (x38 == x36);
                                   return ()}]
helpIIO x0 x1 gen_helpIIO_x16 = msum [do {appendoII x0 x1;
                                          let {x15 = Zero};
                                          let {x14 = Succ x15};
                                          let {x13 = Succ x14};
                                          let {x25 = x13};
                                          (x26, x16) <- do {x16 <- gen_helpIIO_x16;
                                                            return (x16, x16)};
                                          let {x2 = Cons x25 x26};
                                          (x3, x17) <- case x16 of
                                                       {Cons y3 y17 -> return (y3, y17);
                                                        _ -> mzero};
                                          (x4, x18) <- case x17 of
                                                       {Cons y4 y18 -> return (y4, y18);
                                                        _ -> mzero};
                                          (x5, x19) <- case x18 of
                                                       {Cons y5 y19 -> return (y5, y19);
                                                        _ -> mzero};
                                          (x6, x20) <- case x19 of
                                                       {Cons y6 y20 -> return (y6, y20);
                                                        _ -> mzero};
                                          (x7, x21) <- case x20 of
                                                       {Cons y7 y21 -> return (y7, y21);
                                                        _ -> mzero};
                                          (x8, x22) <- case x21 of
                                                       {Cons y8 y22 -> return (y8, y22);
                                                        _ -> mzero};
                                          (x9, x23) <- case x22 of
                                                       {Cons y9 y23 -> return (y9, y23);
                                                        _ -> mzero};
                                          (x10, x24) <- case x23 of
                                                        {Cons y10 y24 -> return (y10, y24);
                                                         _ -> mzero};
                                          (x11, x12) <- case x24 of
                                                        {Cons y11 y12 -> return (y11, y12);
                                                         _ -> mzero};
                                          appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIIII x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
                                          return x2}]
helpIOI x0 x2 = msum [do {let {x15 = Zero};
                          let {x14 = Succ x15};
                          let {x13 = Succ x14};
                          (x25, x26) <- case x2 of
                                        {Cons y25 y26 -> return (y25, y26); _ -> mzero};
                          guard (x25 == x13);
                          let {x16 = x26};
                          (x3, x17) <- case x16 of
                                       {Cons y3 y17 -> return (y3, y17); _ -> mzero};
                          (x4, x18) <- case x17 of
                                       {Cons y4 y18 -> return (y4, y18); _ -> mzero};
                          (x5, x19) <- case x18 of
                                       {Cons y5 y19 -> return (y5, y19); _ -> mzero};
                          (x6, x20) <- case x19 of
                                       {Cons y6 y20 -> return (y6, y20); _ -> mzero};
                          (x7, x21) <- case x20 of
                                       {Cons y7 y21 -> return (y7, y21); _ -> mzero};
                          (x8, x22) <- case x21 of
                                       {Cons y8 y22 -> return (y8, y22); _ -> mzero};
                          (x9, x23) <- case x22 of
                                       {Cons y9 y23 -> return (y9, y23); _ -> mzero};
                          (x10, x24) <- case x23 of
                                        {Cons y10 y24 -> return (y10, y24); _ -> mzero};
                          (x11, x12) <- case x24 of
                                        {Cons y11 y12 -> return (y11, y12); _ -> mzero};
                          appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIIII x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
                          x1 <- appendoIO x0;
                          return x1}]
appendoIO x0 = msum [do {let {x41 = Zero};
                         let {x40 = Succ x41};
                         let {x39 = Succ x40};
                         let {x43 = Zero};
                         let {x46 = Zero};
                         let {x45 = Succ x46};
                         let {x49 = Zero};
                         let {x48 = Succ x49};
                         let {x51 = Zero};
                         let {x55 = Zero};
                         let {x54 = Succ x55};
                         let {x53 = Succ x54};
                         let {x58 = Zero};
                         let {x57 = Succ x58};
                         let {x60 = Zero};
                         let {x62 = Zero};
                         let {x66 = Zero};
                         let {x65 = Succ x66};
                         let {x64 = Succ x65};
                         let {x67 = Nil};
                         let {x63 = Cons x64 x67};
                         let {x61 = Cons x62 x63};
                         let {x59 = Cons x60 x61};
                         let {x56 = Cons x57 x59};
                         let {x52 = Cons x53 x56};
                         let {x50 = Cons x51 x52};
                         let {x47 = Cons x48 x50};
                         let {x44 = Cons x45 x47};
                         let {x42 = Cons x43 x44};
                         guard (x0 == Nil);
                         let {x68 = x39};
                         let {x69 = x42};
                         let {x1 = Cons x68 x69};
                         return x1},
                     do {let {x72 = Zero};
                         let {x71 = Succ x72};
                         let {x70 = Succ x71};
                         (x73, x2) <- case x0 of
                                      {Cons y73 y2 -> return (y73, y2); _ -> mzero};
                         guard (x73 == x70);
                         x1 <- _appendoOI x2;
                         return x1}]
_appendoOI x1 = msum [do {let {x74 = Zero};
                          let {x77 = Zero};
                          let {x76 = Succ x77};
                          let {x80 = Zero};
                          let {x79 = Succ x80};
                          let {x82 = Zero};
                          let {x86 = Zero};
                          let {x85 = Succ x86};
                          let {x84 = Succ x85};
                          let {x89 = Zero};
                          let {x88 = Succ x89};
                          let {x91 = Zero};
                          let {x93 = Zero};
                          let {x97 = Zero};
                          let {x96 = Succ x97};
                          let {x95 = Succ x96};
                          let {x98 = Nil};
                          let {x94 = Cons x95 x98};
                          let {x92 = Cons x93 x94};
                          let {x90 = Cons x91 x92};
                          let {x87 = Cons x88 x90};
                          let {x83 = Cons x84 x87};
                          let {x81 = Cons x82 x83};
                          let {x78 = Cons x79 x81};
                          let {x75 = Cons x76 x78};
                          guard (x1 == Nil);
                          let {x99 = x74};
                          let {x100 = x75};
                          let {x0 = Cons x99 x100};
                          return x0},
                      do {let {x101 = Zero};
                          (x102, x2) <- case x1 of
                                        {Cons y102 y2 -> return (y102, y2); _ -> mzero};
                          guard (x102 == x101);
                          x0 <- __appendoOI x2;
                          return x0}]
__appendoOI x1 = msum [do {let {x104 = Zero};
                           let {x103 = Succ x104};
                           let {x107 = Zero};
                           let {x106 = Succ x107};
                           let {x109 = Zero};
                           let {x113 = Zero};
                           let {x112 = Succ x113};
                           let {x111 = Succ x112};
                           let {x116 = Zero};
                           let {x115 = Succ x116};
                           let {x118 = Zero};
                           let {x120 = Zero};
                           let {x124 = Zero};
                           let {x123 = Succ x124};
                           let {x122 = Succ x123};
                           let {x125 = Nil};
                           let {x121 = Cons x122 x125};
                           let {x119 = Cons x120 x121};
                           let {x117 = Cons x118 x119};
                           let {x114 = Cons x115 x117};
                           let {x110 = Cons x111 x114};
                           let {x108 = Cons x109 x110};
                           let {x105 = Cons x106 x108};
                           guard (x1 == Nil);
                           let {x126 = x103};
                           let {x127 = x105};
                           let {x0 = Cons x126 x127};
                           return x0},
                       do {let {x129 = Zero};
                           let {x128 = Succ x129};
                           (x130, x2) <- case x1 of
                                         {Cons y130 y2 -> return (y130, y2); _ -> mzero};
                           guard (x130 == x128);
                           x0 <- ___appendoOI x2;
                           return x0}]
___appendoOI x1 = msum [do {let {x132 = Zero};
                            let {x131 = Succ x132};
                            let {x134 = Zero};
                            let {x138 = Zero};
                            let {x137 = Succ x138};
                            let {x136 = Succ x137};
                            let {x141 = Zero};
                            let {x140 = Succ x141};
                            let {x143 = Zero};
                            let {x145 = Zero};
                            let {x149 = Zero};
                            let {x148 = Succ x149};
                            let {x147 = Succ x148};
                            let {x150 = Nil};
                            let {x146 = Cons x147 x150};
                            let {x144 = Cons x145 x146};
                            let {x142 = Cons x143 x144};
                            let {x139 = Cons x140 x142};
                            let {x135 = Cons x136 x139};
                            let {x133 = Cons x134 x135};
                            guard (x1 == Nil);
                            let {x151 = x131};
                            let {x152 = x133};
                            let {x0 = Cons x151 x152};
                            return x0},
                        do {let {x154 = Zero};
                            let {x153 = Succ x154};
                            (x155, x2) <- case x1 of
                                          {Cons y155 y2 -> return (y155, y2); _ -> mzero};
                            guard (x155 == x153);
                            x0 <- ____appendoOI x2;
                            return x0}]
____appendoOI x1 = msum [do {let {x156 = Zero};
                             let {x160 = Zero};
                             let {x159 = Succ x160};
                             let {x158 = Succ x159};
                             let {x163 = Zero};
                             let {x162 = Succ x163};
                             let {x165 = Zero};
                             let {x167 = Zero};
                             let {x171 = Zero};
                             let {x170 = Succ x171};
                             let {x169 = Succ x170};
                             let {x172 = Nil};
                             let {x168 = Cons x169 x172};
                             let {x166 = Cons x167 x168};
                             let {x164 = Cons x165 x166};
                             let {x161 = Cons x162 x164};
                             let {x157 = Cons x158 x161};
                             guard (x1 == Nil);
                             let {x173 = x156};
                             let {x174 = x157};
                             let {x0 = Cons x173 x174};
                             return x0},
                         do {let {x175 = Zero};
                             (x176, x2) <- case x1 of
                                           {Cons y176 y2 -> return (y176, y2); _ -> mzero};
                             guard (x176 == x175);
                             x0 <- _____appendoOI x2;
                             return x0}]
_____appendoOI x1 = msum [do {let {x179 = Zero};
                              let {x178 = Succ x179};
                              let {x177 = Succ x178};
                              let {x182 = Zero};
                              let {x181 = Succ x182};
                              let {x184 = Zero};
                              let {x186 = Zero};
                              let {x190 = Zero};
                              let {x189 = Succ x190};
                              let {x188 = Succ x189};
                              let {x191 = Nil};
                              let {x187 = Cons x188 x191};
                              let {x185 = Cons x186 x187};
                              let {x183 = Cons x184 x185};
                              let {x180 = Cons x181 x183};
                              guard (x1 == Nil);
                              let {x192 = x177};
                              let {x193 = x180};
                              let {x0 = Cons x192 x193};
                              return x0},
                          do {let {x196 = Zero};
                              let {x195 = Succ x196};
                              let {x194 = Succ x195};
                              (x197, x2) <- case x1 of
                                            {Cons y197 y2 -> return (y197, y2); _ -> mzero};
                              guard (x197 == x194);
                              x0 <- ______appendoOI x2;
                              return x0}]
______appendoOI x1 = msum [do {let {x199 = Zero};
                               let {x198 = Succ x199};
                               let {x201 = Zero};
                               let {x203 = Zero};
                               let {x207 = Zero};
                               let {x206 = Succ x207};
                               let {x205 = Succ x206};
                               let {x208 = Nil};
                               let {x204 = Cons x205 x208};
                               let {x202 = Cons x203 x204};
                               let {x200 = Cons x201 x202};
                               guard (x1 == Nil);
                               let {x209 = x198};
                               let {x210 = x200};
                               let {x0 = Cons x209 x210};
                               return x0},
                           do {let {x212 = Zero};
                               let {x211 = Succ x212};
                               (x213, x2) <- case x1 of
                                             {Cons y213 y2 -> return (y213, y2); _ -> mzero};
                               guard (x213 == x211);
                               x0 <- _______appendoOI x2;
                               return x0}]
_______appendoOI x1 = msum [do {let {x214 = Zero};
                                let {x216 = Zero};
                                let {x220 = Zero};
                                let {x219 = Succ x220};
                                let {x218 = Succ x219};
                                let {x221 = Nil};
                                let {x217 = Cons x218 x221};
                                let {x215 = Cons x216 x217};
                                guard (x1 == Nil);
                                let {x222 = x214};
                                let {x223 = x215};
                                let {x0 = Cons x222 x223};
                                return x0},
                            do {let {x224 = Zero};
                                (x225, x2) <- case x1 of
                                              {Cons y225 y2 -> return (y225, y2); _ -> mzero};
                                guard (x225 == x224);
                                x0 <- ________appendoOI x2;
                                return x0}]
________appendoOI x1 = msum [do {let {x226 = Zero};
                                 let {x230 = Zero};
                                 let {x229 = Succ x230};
                                 let {x228 = Succ x229};
                                 let {x231 = Nil};
                                 let {x227 = Cons x228 x231};
                                 guard (x1 == Nil);
                                 let {x232 = x226};
                                 let {x233 = x227};
                                 let {x0 = Cons x232 x233};
                                 return x0},
                             do {let {x234 = Zero};
                                 (x235, x2) <- case x1 of
                                               {Cons y235 y2 -> return (y235, y2); _ -> mzero};
                                 guard (x235 == x234);
                                 x0 <- _________appendoOI x2;
                                 return x0}]
_________appendoOI x1 = msum [do {let {x238 = Zero};
                                  let {x237 = Succ x238};
                                  let {x236 = Succ x237};
                                  let {x239 = Nil};
                                  guard (x1 == Nil);
                                  let {x240 = x236};
                                  let {x241 = x239};
                                  let {x0 = Cons x240 x241};
                                  return x0},
                              do {let {x244 = Zero};
                                  let {x243 = Succ x244};
                                  let {x242 = Succ x243};
                                  let {x245 = Nil};
                                  let {x0 = Nil};
                                  (x246, x247) <- case x1 of
                                                  {Cons y246 y247 -> return (y246, y247);
                                                   _ -> mzero};
                                  guard (x246 == x242);
                                  guard (x247 == x245);
                                  return x0}]
helpIOO x0 gen_helpIOO_x16 = msum [do {let {x15 = Zero};
                                       let {x14 = Succ x15};
                                       let {x13 = Succ x14};
                                       let {x25 = x13};
                                       x1 <- appendoIO x0;
                                       (x26, x16) <- do {x16 <- gen_helpIOO_x16; return (x16, x16)};
                                       let {x2 = Cons x25 x26};
                                       (x3, x17) <- case x16 of
                                                    {Cons y3 y17 -> return (y3, y17); _ -> mzero};
                                       (x4, x18) <- case x17 of
                                                    {Cons y4 y18 -> return (y4, y18); _ -> mzero};
                                       (x5, x19) <- case x18 of
                                                    {Cons y5 y19 -> return (y5, y19); _ -> mzero};
                                       (x6, x20) <- case x19 of
                                                    {Cons y6 y20 -> return (y6, y20); _ -> mzero};
                                       (x7, x21) <- case x20 of
                                                    {Cons y7 y21 -> return (y7, y21); _ -> mzero};
                                       (x8, x22) <- case x21 of
                                                    {Cons y8 y22 -> return (y8, y22); _ -> mzero};
                                       (x9, x23) <- case x22 of
                                                    {Cons y9 y23 -> return (y9, y23); _ -> mzero};
                                       (x10, x24) <- case x23 of
                                                     {Cons y10 y24 -> return (y10, y24);
                                                      _ -> mzero};
                                       (x11, x12) <- case x24 of
                                                     {Cons y11 y12 -> return (y11, y12);
                                                      _ -> mzero};
                                       appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIIII x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
                                       return (x1, x2)}]
helpOII x1 x2 = msum [do {let {x15 = Zero};
                          let {x14 = Succ x15};
                          let {x13 = Succ x14};
                          (x25, x26) <- case x2 of
                                        {Cons y25 y26 -> return (y25, y26); _ -> mzero};
                          guard (x25 == x13);
                          let {x16 = x26};
                          (x3, x17) <- case x16 of
                                       {Cons y3 y17 -> return (y3, y17); _ -> mzero};
                          (x4, x18) <- case x17 of
                                       {Cons y4 y18 -> return (y4, y18); _ -> mzero};
                          (x5, x19) <- case x18 of
                                       {Cons y5 y19 -> return (y5, y19); _ -> mzero};
                          (x6, x20) <- case x19 of
                                       {Cons y6 y20 -> return (y6, y20); _ -> mzero};
                          (x7, x21) <- case x20 of
                                       {Cons y7 y21 -> return (y7, y21); _ -> mzero};
                          (x8, x22) <- case x21 of
                                       {Cons y8 y22 -> return (y8, y22); _ -> mzero};
                          (x9, x23) <- case x22 of
                                       {Cons y9 y23 -> return (y9, y23); _ -> mzero};
                          (x10, x24) <- case x23 of
                                        {Cons y10 y24 -> return (y10, y24); _ -> mzero};
                          (x11, x12) <- case x24 of
                                        {Cons y11 y12 -> return (y11, y12); _ -> mzero};
                          appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIIII x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
                          x0 <- appendoOI x1;
                          return x0}]
appendoOI x1 = msum [do {let {x41 = Zero};
                         let {x40 = Succ x41};
                         let {x39 = Succ x40};
                         let {x43 = Zero};
                         let {x46 = Zero};
                         let {x45 = Succ x46};
                         let {x49 = Zero};
                         let {x48 = Succ x49};
                         let {x51 = Zero};
                         let {x55 = Zero};
                         let {x54 = Succ x55};
                         let {x53 = Succ x54};
                         let {x58 = Zero};
                         let {x57 = Succ x58};
                         let {x60 = Zero};
                         let {x62 = Zero};
                         let {x66 = Zero};
                         let {x65 = Succ x66};
                         let {x64 = Succ x65};
                         let {x67 = Nil};
                         let {x63 = Cons x64 x67};
                         let {x61 = Cons x62 x63};
                         let {x59 = Cons x60 x61};
                         let {x56 = Cons x57 x59};
                         let {x52 = Cons x53 x56};
                         let {x50 = Cons x51 x52};
                         let {x47 = Cons x48 x50};
                         let {x44 = Cons x45 x47};
                         let {x42 = Cons x43 x44};
                         let {x0 = Nil};
                         (x68, x69) <- case x1 of
                                       {Cons y68 y69 -> return (y68, y69); _ -> mzero};
                         guard (x68 == x39);
                         guard (x69 == x42);
                         return x0},
                     do {let {x72 = Zero};
                         let {x71 = Succ x72};
                         let {x70 = Succ x71};
                         let {x73 = x70};
                         x2 <- _appendoIO x1;
                         let {x0 = Cons x73 x2};
                         return x0}]
_appendoIO x0 = msum [do {let {x1 = Nil};
                          let {x74 = Zero};
                          let {x77 = Zero};
                          let {x76 = Succ x77};
                          let {x80 = Zero};
                          let {x79 = Succ x80};
                          let {x82 = Zero};
                          let {x86 = Zero};
                          let {x85 = Succ x86};
                          let {x84 = Succ x85};
                          let {x89 = Zero};
                          let {x88 = Succ x89};
                          let {x91 = Zero};
                          let {x93 = Zero};
                          let {x97 = Zero};
                          let {x96 = Succ x97};
                          let {x95 = Succ x96};
                          let {x98 = Nil};
                          let {x94 = Cons x95 x98};
                          let {x92 = Cons x93 x94};
                          let {x90 = Cons x91 x92};
                          let {x87 = Cons x88 x90};
                          let {x83 = Cons x84 x87};
                          let {x81 = Cons x82 x83};
                          let {x78 = Cons x79 x81};
                          let {x75 = Cons x76 x78};
                          (x99, x100) <- case x0 of
                                         {Cons y99 y100 -> return (y99, y100); _ -> mzero};
                          guard (x99 == x74);
                          guard (x100 == x75);
                          return x1},
                      do {let {x101 = Zero};
                          let {x102 = x101};
                          x2 <- __appendoIO x0;
                          let {x1 = Cons x102 x2};
                          return x1}]
__appendoIO x0 = msum [do {let {x1 = Nil};
                           let {x104 = Zero};
                           let {x103 = Succ x104};
                           let {x107 = Zero};
                           let {x106 = Succ x107};
                           let {x109 = Zero};
                           let {x113 = Zero};
                           let {x112 = Succ x113};
                           let {x111 = Succ x112};
                           let {x116 = Zero};
                           let {x115 = Succ x116};
                           let {x118 = Zero};
                           let {x120 = Zero};
                           let {x124 = Zero};
                           let {x123 = Succ x124};
                           let {x122 = Succ x123};
                           let {x125 = Nil};
                           let {x121 = Cons x122 x125};
                           let {x119 = Cons x120 x121};
                           let {x117 = Cons x118 x119};
                           let {x114 = Cons x115 x117};
                           let {x110 = Cons x111 x114};
                           let {x108 = Cons x109 x110};
                           let {x105 = Cons x106 x108};
                           (x126, x127) <- case x0 of
                                           {Cons y126 y127 -> return (y126, y127); _ -> mzero};
                           guard (x126 == x103);
                           guard (x127 == x105);
                           return x1},
                       do {let {x129 = Zero};
                           let {x128 = Succ x129};
                           let {x130 = x128};
                           x2 <- ___appendoIO x0;
                           let {x1 = Cons x130 x2};
                           return x1}]
___appendoIO x0 = msum [do {let {x1 = Nil};
                            let {x132 = Zero};
                            let {x131 = Succ x132};
                            let {x134 = Zero};
                            let {x138 = Zero};
                            let {x137 = Succ x138};
                            let {x136 = Succ x137};
                            let {x141 = Zero};
                            let {x140 = Succ x141};
                            let {x143 = Zero};
                            let {x145 = Zero};
                            let {x149 = Zero};
                            let {x148 = Succ x149};
                            let {x147 = Succ x148};
                            let {x150 = Nil};
                            let {x146 = Cons x147 x150};
                            let {x144 = Cons x145 x146};
                            let {x142 = Cons x143 x144};
                            let {x139 = Cons x140 x142};
                            let {x135 = Cons x136 x139};
                            let {x133 = Cons x134 x135};
                            (x151, x152) <- case x0 of
                                            {Cons y151 y152 -> return (y151, y152); _ -> mzero};
                            guard (x151 == x131);
                            guard (x152 == x133);
                            return x1},
                        do {let {x154 = Zero};
                            let {x153 = Succ x154};
                            let {x155 = x153};
                            x2 <- ____appendoIO x0;
                            let {x1 = Cons x155 x2};
                            return x1}]
____appendoIO x0 = msum [do {let {x1 = Nil};
                             let {x156 = Zero};
                             let {x160 = Zero};
                             let {x159 = Succ x160};
                             let {x158 = Succ x159};
                             let {x163 = Zero};
                             let {x162 = Succ x163};
                             let {x165 = Zero};
                             let {x167 = Zero};
                             let {x171 = Zero};
                             let {x170 = Succ x171};
                             let {x169 = Succ x170};
                             let {x172 = Nil};
                             let {x168 = Cons x169 x172};
                             let {x166 = Cons x167 x168};
                             let {x164 = Cons x165 x166};
                             let {x161 = Cons x162 x164};
                             let {x157 = Cons x158 x161};
                             (x173, x174) <- case x0 of
                                             {Cons y173 y174 -> return (y173, y174); _ -> mzero};
                             guard (x173 == x156);
                             guard (x174 == x157);
                             return x1},
                         do {let {x175 = Zero};
                             let {x176 = x175};
                             x2 <- _____appendoIO x0;
                             let {x1 = Cons x176 x2};
                             return x1}]
_____appendoIO x0 = msum [do {let {x1 = Nil};
                              let {x179 = Zero};
                              let {x178 = Succ x179};
                              let {x177 = Succ x178};
                              let {x182 = Zero};
                              let {x181 = Succ x182};
                              let {x184 = Zero};
                              let {x186 = Zero};
                              let {x190 = Zero};
                              let {x189 = Succ x190};
                              let {x188 = Succ x189};
                              let {x191 = Nil};
                              let {x187 = Cons x188 x191};
                              let {x185 = Cons x186 x187};
                              let {x183 = Cons x184 x185};
                              let {x180 = Cons x181 x183};
                              (x192, x193) <- case x0 of
                                              {Cons y192 y193 -> return (y192, y193); _ -> mzero};
                              guard (x192 == x177);
                              guard (x193 == x180);
                              return x1},
                          do {let {x196 = Zero};
                              let {x195 = Succ x196};
                              let {x194 = Succ x195};
                              let {x197 = x194};
                              x2 <- ______appendoIO x0;
                              let {x1 = Cons x197 x2};
                              return x1}]
______appendoIO x0 = msum [do {let {x1 = Nil};
                               let {x199 = Zero};
                               let {x198 = Succ x199};
                               let {x201 = Zero};
                               let {x203 = Zero};
                               let {x207 = Zero};
                               let {x206 = Succ x207};
                               let {x205 = Succ x206};
                               let {x208 = Nil};
                               let {x204 = Cons x205 x208};
                               let {x202 = Cons x203 x204};
                               let {x200 = Cons x201 x202};
                               (x209, x210) <- case x0 of
                                               {Cons y209 y210 -> return (y209, y210); _ -> mzero};
                               guard (x209 == x198);
                               guard (x210 == x200);
                               return x1},
                           do {let {x212 = Zero};
                               let {x211 = Succ x212};
                               let {x213 = x211};
                               x2 <- _______appendoIO x0;
                               let {x1 = Cons x213 x2};
                               return x1}]
_______appendoIO x0 = msum [do {let {x1 = Nil};
                                let {x214 = Zero};
                                let {x216 = Zero};
                                let {x220 = Zero};
                                let {x219 = Succ x220};
                                let {x218 = Succ x219};
                                let {x221 = Nil};
                                let {x217 = Cons x218 x221};
                                let {x215 = Cons x216 x217};
                                (x222, x223) <- case x0 of
                                                {Cons y222 y223 -> return (y222, y223); _ -> mzero};
                                guard (x222 == x214);
                                guard (x223 == x215);
                                return x1},
                            do {let {x224 = Zero};
                                let {x225 = x224};
                                x2 <- ________appendoIO x0;
                                let {x1 = Cons x225 x2};
                                return x1}]
________appendoIO x0 = msum [do {let {x1 = Nil};
                                 let {x226 = Zero};
                                 let {x230 = Zero};
                                 let {x229 = Succ x230};
                                 let {x228 = Succ x229};
                                 let {x231 = Nil};
                                 let {x227 = Cons x228 x231};
                                 (x232, x233) <- case x0 of
                                                 {Cons y232 y233 -> return (y232, y233);
                                                  _ -> mzero};
                                 guard (x232 == x226);
                                 guard (x233 == x227);
                                 return x1},
                             do {let {x234 = Zero};
                                 let {x235 = x234};
                                 x2 <- _________appendoIO x0;
                                 let {x1 = Cons x235 x2};
                                 return x1}]
_________appendoIO x0 = msum [do {let {x1 = Nil};
                                  let {x238 = Zero};
                                  let {x237 = Succ x238};
                                  let {x236 = Succ x237};
                                  let {x239 = Nil};
                                  (x240, x241) <- case x0 of
                                                  {Cons y240 y241 -> return (y240, y241);
                                                   _ -> mzero};
                                  guard (x240 == x236);
                                  guard (x241 == x239);
                                  return x1},
                              do {let {x244 = Zero};
                                  let {x243 = Succ x244};
                                  let {x242 = Succ x243};
                                  let {x245 = Nil};
                                  guard (x0 == Nil);
                                  let {x246 = x242};
                                  let {x247 = x245};
                                  let {x1 = Cons x246 x247};
                                  return x1}]
helpOIO x1 gen_helpOIO_x16 = msum [do {let {x15 = Zero};
                                       let {x14 = Succ x15};
                                       let {x13 = Succ x14};
                                       let {x25 = x13};
                                       x0 <- appendoOI x1;
                                       (x26, x16) <- do {x16 <- gen_helpOIO_x16; return (x16, x16)};
                                       let {x2 = Cons x25 x26};
                                       (x3, x17) <- case x16 of
                                                    {Cons y3 y17 -> return (y3, y17); _ -> mzero};
                                       (x4, x18) <- case x17 of
                                                    {Cons y4 y18 -> return (y4, y18); _ -> mzero};
                                       (x5, x19) <- case x18 of
                                                    {Cons y5 y19 -> return (y5, y19); _ -> mzero};
                                       (x6, x20) <- case x19 of
                                                    {Cons y6 y20 -> return (y6, y20); _ -> mzero};
                                       (x7, x21) <- case x20 of
                                                    {Cons y7 y21 -> return (y7, y21); _ -> mzero};
                                       (x8, x22) <- case x21 of
                                                    {Cons y8 y22 -> return (y8, y22); _ -> mzero};
                                       (x9, x23) <- case x22 of
                                                    {Cons y9 y23 -> return (y9, y23); _ -> mzero};
                                       (x10, x24) <- case x23 of
                                                     {Cons y10 y24 -> return (y10, y24);
                                                      _ -> mzero};
                                       (x11, x12) <- case x24 of
                                                     {Cons y11 y12 -> return (y11, y12);
                                                      _ -> mzero};
                                       appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIIII x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
                                       return (x0, x2)}]
helpOOI x2 gen_appendoOO_x2 = msum [do {let {x15 = Zero};
                                        let {x14 = Succ x15};
                                        let {x13 = Succ x14};
                                        (x25, x26) <- case x2 of
                                                      {Cons y25 y26 -> return (y25, y26);
                                                       _ -> mzero};
                                        guard (x25 == x13);
                                        let {x16 = x26};
                                        (x3, x17) <- case x16 of
                                                     {Cons y3 y17 -> return (y3, y17); _ -> mzero};
                                        (x4, x18) <- case x17 of
                                                     {Cons y4 y18 -> return (y4, y18); _ -> mzero};
                                        (x5, x19) <- case x18 of
                                                     {Cons y5 y19 -> return (y5, y19); _ -> mzero};
                                        (x6, x20) <- case x19 of
                                                     {Cons y6 y20 -> return (y6, y20); _ -> mzero};
                                        (x7, x21) <- case x20 of
                                                     {Cons y7 y21 -> return (y7, y21); _ -> mzero};
                                        (x8, x22) <- case x21 of
                                                     {Cons y8 y22 -> return (y8, y22); _ -> mzero};
                                        (x9, x23) <- case x22 of
                                                     {Cons y9 y23 -> return (y9, y23); _ -> mzero};
                                        (x10, x24) <- case x23 of
                                                      {Cons y10 y24 -> return (y10, y24);
                                                       _ -> mzero};
                                        (x11, x12) <- case x24 of
                                                      {Cons y11 y12 -> return (y11, y12);
                                                       _ -> mzero};
                                        appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIIII x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
                                        (x0, x1) <- appendoOO gen_appendoOO_x2;
                                        return (x0, x1)}]
appendoOO gen_appendoOO_x2 = msum [do {let {x41 = Zero};
                                       let {x40 = Succ x41};
                                       let {x39 = Succ x40};
                                       let {x43 = Zero};
                                       let {x46 = Zero};
                                       let {x45 = Succ x46};
                                       let {x49 = Zero};
                                       let {x48 = Succ x49};
                                       let {x51 = Zero};
                                       let {x55 = Zero};
                                       let {x54 = Succ x55};
                                       let {x53 = Succ x54};
                                       let {x58 = Zero};
                                       let {x57 = Succ x58};
                                       let {x60 = Zero};
                                       let {x62 = Zero};
                                       let {x66 = Zero};
                                       let {x65 = Succ x66};
                                       let {x64 = Succ x65};
                                       let {x67 = Nil};
                                       let {x63 = Cons x64 x67};
                                       let {x61 = Cons x62 x63};
                                       let {x59 = Cons x60 x61};
                                       let {x56 = Cons x57 x59};
                                       let {x52 = Cons x53 x56};
                                       let {x50 = Cons x51 x52};
                                       let {x47 = Cons x48 x50};
                                       let {x44 = Cons x45 x47};
                                       let {x42 = Cons x43 x44};
                                       let {x0 = Nil};
                                       let {x68 = x39};
                                       let {x69 = x42};
                                       let {x1 = Cons x68 x69};
                                       return (x0, x1)},
                                   do {let {x72 = Zero};
                                       let {x71 = Succ x72};
                                       let {x70 = Succ x71};
                                       let {x73 = x70};
                                       (x0, x2) <- do {x2 <- gen_appendoOO_x2;
                                                       let {x0 = Cons x73 x2};
                                                       return (x0, x2)};
                                       x1 <- _appendoOI x2;
                                       return (x0, x1)}]
helpOOO gen_appendoOO_x2 gen_helpOOO_x16 = msum [do {let {x15 = Zero};
                                                     let {x14 = Succ x15};
                                                     let {x13 = Succ x14};
                                                     let {x25 = x13};
                                                     (x26, x16) <- do {x16 <- gen_helpOOO_x16;
                                                                       return (x16, x16)};
                                                     let {x2 = Cons x25 x26};
                                                     (x3, x17) <- case x16 of
                                                                  {Cons y3 y17 -> return (y3, y17);
                                                                   _ -> mzero};
                                                     (x4, x18) <- case x17 of
                                                                  {Cons y4 y18 -> return (y4, y18);
                                                                   _ -> mzero};
                                                     (x5, x19) <- case x18 of
                                                                  {Cons y5 y19 -> return (y5, y19);
                                                                   _ -> mzero};
                                                     (x6, x20) <- case x19 of
                                                                  {Cons y6 y20 -> return (y6, y20);
                                                                   _ -> mzero};
                                                     (x7, x21) <- case x20 of
                                                                  {Cons y7 y21 -> return (y7, y21);
                                                                   _ -> mzero};
                                                     (x8, x22) <- case x21 of
                                                                  {Cons y8 y22 -> return (y8, y22);
                                                                   _ -> mzero};
                                                     (x9, x23) <- case x22 of
                                                                  {Cons y9 y23 -> return (y9, y23);
                                                                   _ -> mzero};
                                                     (x10, x24) <- case x23 of
                                                                   {Cons y10 y24 -> return (y10,
                                                                                            y24);
                                                                    _ -> mzero};
                                                     (x11, x12) <- case x24 of
                                                                   {Cons y11 y12 -> return (y11,
                                                                                            y12);
                                                                    _ -> mzero};
                                                     appendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoAppendoReversoIIIIIIIIII x3 x4 x5 x6 x7 x8 x9 x10 x11 x12;
                                                     (x0, x1) <- appendoOO gen_appendoOO_x2;
                                                     return (x0, x1, x2)}]
reversoI x0 = msum [do {guard (x0 == Nil); return ()}]