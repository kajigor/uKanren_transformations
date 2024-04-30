module List3 where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | Succ Term
    | Zero
    deriving (Show, Eq)
helpIII x0 x1 x2 = msum [do {let {x17 = Zero};
                             let {x16 = Succ x17};
                             let {x15 = Succ x16};
                             let {x19 = Zero};
                             let {x21 = Zero};
                             let {x24 = Zero};
                             let {x23 = Succ x24};
                             let {x28 = Zero};
                             let {x27 = Succ x28};
                             let {x26 = Succ x27};
                             let {x30 = Zero};
                             let {x33 = Zero};
                             let {x32 = Succ x33};
                             let {x36 = Zero};
                             let {x35 = Succ x36};
                             let {x38 = Zero};
                             let {x42 = Zero};
                             let {x41 = Succ x42};
                             let {x40 = Succ x41};
                             let {x47 = Zero};
                             let {x46 = Succ x47};
                             let {x45 = Succ x46};
                             let {x49 = Zero};
                             let {x52 = Zero};
                             let {x51 = Succ x52};
                             let {x55 = Zero};
                             let {x54 = Succ x55};
                             let {x57 = Zero};
                             let {x61 = Zero};
                             let {x60 = Succ x61};
                             let {x59 = Succ x60};
                             let {x64 = Zero};
                             let {x63 = Succ x64};
                             let {x66 = Zero};
                             let {x68 = Zero};
                             let {x72 = Zero};
                             let {x71 = Succ x72};
                             let {x70 = Succ x71};
                             let {x73 = Nil};
                             let {x69 = Cons x70 x73};
                             let {x67 = Cons x68 x69};
                             let {x65 = Cons x66 x67};
                             let {x62 = Cons x63 x65};
                             let {x58 = Cons x59 x62};
                             let {x56 = Cons x57 x58};
                             let {x53 = Cons x54 x56};
                             let {x50 = Cons x51 x53};
                             let {x48 = Cons x49 x50};
                             (x43, x44) <- case x2 of
                                           {Cons y43 y44 -> return (y43, y44); _ -> mzero};
                             guard (x43 == x15);
                             (x74, x75) <- case x1 of
                                           {Cons y74 y75 -> return (y74, y75); _ -> mzero};
                             guard (x74 == x45);
                             guard (x75 == x48);
                             guard (x0 == Nil);
                             let {x18 = x44};
                             x20 <- case x18 of
                                    {Cons y19 y20 -> do {guard (x19 == y19); return y20};
                                     _ -> mzero};
                             x22 <- case x20 of
                                    {Cons y21 y22 -> do {guard (x21 == y21); return y22};
                                     _ -> mzero};
                             x25 <- case x22 of
                                    {Cons y23 y25 -> do {guard (x23 == y23); return y25};
                                     _ -> mzero};
                             x29 <- case x25 of
                                    {Cons y26 y29 -> do {guard (x26 == y26); return y29};
                                     _ -> mzero};
                             x31 <- case x29 of
                                    {Cons y30 y31 -> do {guard (x30 == y30); return y31};
                                     _ -> mzero};
                             x34 <- case x31 of
                                    {Cons y32 y34 -> do {guard (x32 == y32); return y34};
                                     _ -> mzero};
                             x37 <- case x34 of
                                    {Cons y35 y37 -> do {guard (x35 == y35); return y37};
                                     _ -> mzero};
                             x39 <- case x37 of
                                    {Cons y38 y39 -> do {guard (x38 == y38); return y39};
                                     _ -> mzero};
                             x3 <- case x39 of
                                   {Cons y40 y3 -> do {guard (x40 == y40); return y3}; _ -> mzero};
                             reversoI x3;
                             return ()},
                         do {let {x78 = Zero};
                             let {x77 = Succ x78};
                             let {x76 = Succ x77};
                             let {x80 = Zero};
                             let {x82 = Zero};
                             let {x85 = Zero};
                             let {x84 = Succ x85};
                             let {x89 = Zero};
                             let {x88 = Succ x89};
                             let {x87 = Succ x88};
                             let {x91 = Zero};
                             let {x94 = Zero};
                             let {x93 = Succ x94};
                             let {x97 = Zero};
                             let {x96 = Succ x97};
                             let {x99 = Zero};
                             let {x103 = Zero};
                             let {x102 = Succ x103};
                             let {x101 = Succ x102};
                             let {x108 = Zero};
                             let {x107 = Succ x108};
                             let {x106 = Succ x107};
                             (x104, x105) <- case x2 of
                                             {Cons y104 y105 -> return (y104, y105); _ -> mzero};
                             guard (x104 == x76);
                             (x109, x4) <- case x0 of
                                           {Cons y109 y4 -> return (y109, y4); _ -> mzero};
                             guard (x109 == x106);
                             appendoII x1 x4;
                             let {x79 = x105};
                             x81 <- case x79 of
                                    {Cons y80 y81 -> do {guard (x80 == y80); return y81};
                                     _ -> mzero};
                             x83 <- case x81 of
                                    {Cons y82 y83 -> do {guard (x82 == y82); return y83};
                                     _ -> mzero};
                             x86 <- case x83 of
                                    {Cons y84 y86 -> do {guard (x84 == y84); return y86};
                                     _ -> mzero};
                             x90 <- case x86 of
                                    {Cons y87 y90 -> do {guard (x87 == y87); return y90};
                                     _ -> mzero};
                             x92 <- case x90 of
                                    {Cons y91 y92 -> do {guard (x91 == y91); return y92};
                                     _ -> mzero};
                             x95 <- case x92 of
                                    {Cons y93 y95 -> do {guard (x93 == y93); return y95};
                                     _ -> mzero};
                             x98 <- case x95 of
                                    {Cons y96 y98 -> do {guard (x96 == y96); return y98};
                                     _ -> mzero};
                             x100 <- case x98 of
                                     {Cons y99 y100 -> do {guard (x99 == y99); return y100};
                                      _ -> mzero};
                             x3 <- case x100 of
                                   {Cons y101 y3 -> do {guard (x101 == y101); return y3};
                                    _ -> mzero};
                             reversoI x3;
                             return ()}]
appendoII x0 x1 = msum [do {let {x110 = Zero};
                            let {x113 = Zero};
                            let {x112 = Succ x113};
                            let {x116 = Zero};
                            let {x115 = Succ x116};
                            let {x118 = Zero};
                            let {x122 = Zero};
                            let {x121 = Succ x122};
                            let {x120 = Succ x121};
                            let {x125 = Zero};
                            let {x124 = Succ x125};
                            let {x127 = Zero};
                            let {x129 = Zero};
                            let {x133 = Zero};
                            let {x132 = Succ x133};
                            let {x131 = Succ x132};
                            let {x134 = Nil};
                            let {x130 = Cons x131 x134};
                            let {x128 = Cons x129 x130};
                            let {x126 = Cons x127 x128};
                            let {x123 = Cons x124 x126};
                            let {x119 = Cons x120 x123};
                            let {x117 = Cons x118 x119};
                            let {x114 = Cons x115 x117};
                            let {x111 = Cons x112 x114};
                            guard (x1 == Nil);
                            (x135, x136) <- case x0 of
                                            {Cons y135 y136 -> return (y135, y136); _ -> mzero};
                            guard (x135 == x110);
                            guard (x136 == x111);
                            return ()},
                        do {let {x137 = Zero};
                            (x138, x2) <- case x1 of
                                          {Cons y138 y2 -> return (y138, y2); _ -> mzero};
                            guard (x138 == x137);
                            _appendoII x0 x2;
                            return ()}]
_appendoII x0 x1 = msum [do {let {x140 = Zero};
                             let {x139 = Succ x140};
                             let {x143 = Zero};
                             let {x142 = Succ x143};
                             let {x145 = Zero};
                             let {x149 = Zero};
                             let {x148 = Succ x149};
                             let {x147 = Succ x148};
                             let {x152 = Zero};
                             let {x151 = Succ x152};
                             let {x154 = Zero};
                             let {x156 = Zero};
                             let {x160 = Zero};
                             let {x159 = Succ x160};
                             let {x158 = Succ x159};
                             let {x161 = Nil};
                             let {x157 = Cons x158 x161};
                             let {x155 = Cons x156 x157};
                             let {x153 = Cons x154 x155};
                             let {x150 = Cons x151 x153};
                             let {x146 = Cons x147 x150};
                             let {x144 = Cons x145 x146};
                             let {x141 = Cons x142 x144};
                             guard (x1 == Nil);
                             (x162, x163) <- case x0 of
                                             {Cons y162 y163 -> return (y162, y163); _ -> mzero};
                             guard (x162 == x139);
                             guard (x163 == x141);
                             return ()},
                         do {let {x165 = Zero};
                             let {x164 = Succ x165};
                             (x166, x2) <- case x1 of
                                           {Cons y166 y2 -> return (y166, y2); _ -> mzero};
                             guard (x166 == x164);
                             __appendoII x0 x2;
                             return ()}]
__appendoII x0 x1 = msum [do {let {x168 = Zero};
                              let {x167 = Succ x168};
                              let {x170 = Zero};
                              let {x174 = Zero};
                              let {x173 = Succ x174};
                              let {x172 = Succ x173};
                              let {x177 = Zero};
                              let {x176 = Succ x177};
                              let {x179 = Zero};
                              let {x181 = Zero};
                              let {x185 = Zero};
                              let {x184 = Succ x185};
                              let {x183 = Succ x184};
                              let {x186 = Nil};
                              let {x182 = Cons x183 x186};
                              let {x180 = Cons x181 x182};
                              let {x178 = Cons x179 x180};
                              let {x175 = Cons x176 x178};
                              let {x171 = Cons x172 x175};
                              let {x169 = Cons x170 x171};
                              guard (x1 == Nil);
                              (x187, x188) <- case x0 of
                                              {Cons y187 y188 -> return (y187, y188); _ -> mzero};
                              guard (x187 == x167);
                              guard (x188 == x169);
                              return ()},
                          do {let {x190 = Zero};
                              let {x189 = Succ x190};
                              (x191, x2) <- case x1 of
                                            {Cons y191 y2 -> return (y191, y2); _ -> mzero};
                              guard (x191 == x189);
                              ___appendoII x0 x2;
                              return ()}]
___appendoII x0 x1 = msum [do {let {x192 = Zero};
                               let {x196 = Zero};
                               let {x195 = Succ x196};
                               let {x194 = Succ x195};
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
                               let {x197 = Cons x198 x200};
                               let {x193 = Cons x194 x197};
                               guard (x1 == Nil);
                               (x209, x210) <- case x0 of
                                               {Cons y209 y210 -> return (y209, y210); _ -> mzero};
                               guard (x209 == x192);
                               guard (x210 == x193);
                               return ()},
                           do {let {x211 = Zero};
                               (x212, x2) <- case x1 of
                                             {Cons y212 y2 -> return (y212, y2); _ -> mzero};
                               guard (x212 == x211);
                               ____appendoII x0 x2;
                               return ()}]
____appendoII x0 x1 = msum [do {let {x215 = Zero};
                                let {x214 = Succ x215};
                                let {x213 = Succ x214};
                                let {x218 = Zero};
                                let {x217 = Succ x218};
                                let {x220 = Zero};
                                let {x222 = Zero};
                                let {x226 = Zero};
                                let {x225 = Succ x226};
                                let {x224 = Succ x225};
                                let {x227 = Nil};
                                let {x223 = Cons x224 x227};
                                let {x221 = Cons x222 x223};
                                let {x219 = Cons x220 x221};
                                let {x216 = Cons x217 x219};
                                guard (x1 == Nil);
                                (x228, x229) <- case x0 of
                                                {Cons y228 y229 -> return (y228, y229); _ -> mzero};
                                guard (x228 == x213);
                                guard (x229 == x216);
                                return ()},
                            do {let {x232 = Zero};
                                let {x231 = Succ x232};
                                let {x230 = Succ x231};
                                (x233, x2) <- case x1 of
                                              {Cons y233 y2 -> return (y233, y2); _ -> mzero};
                                guard (x233 == x230);
                                _____appendoII x0 x2;
                                return ()}]
_____appendoII x0 x1 = msum [do {let {x235 = Zero};
                                 let {x234 = Succ x235};
                                 let {x237 = Zero};
                                 let {x239 = Zero};
                                 let {x243 = Zero};
                                 let {x242 = Succ x243};
                                 let {x241 = Succ x242};
                                 let {x244 = Nil};
                                 let {x240 = Cons x241 x244};
                                 let {x238 = Cons x239 x240};
                                 let {x236 = Cons x237 x238};
                                 guard (x1 == Nil);
                                 (x245, x246) <- case x0 of
                                                 {Cons y245 y246 -> return (y245, y246);
                                                  _ -> mzero};
                                 guard (x245 == x234);
                                 guard (x246 == x236);
                                 return ()},
                             do {let {x248 = Zero};
                                 let {x247 = Succ x248};
                                 (x249, x2) <- case x1 of
                                               {Cons y249 y2 -> return (y249, y2); _ -> mzero};
                                 guard (x249 == x247);
                                 ______appendoII x0 x2;
                                 return ()}]
______appendoII x0 x1 = msum [do {let {x250 = Zero};
                                  let {x252 = Zero};
                                  let {x256 = Zero};
                                  let {x255 = Succ x256};
                                  let {x254 = Succ x255};
                                  let {x257 = Nil};
                                  let {x253 = Cons x254 x257};
                                  let {x251 = Cons x252 x253};
                                  guard (x1 == Nil);
                                  (x258, x259) <- case x0 of
                                                  {Cons y258 y259 -> return (y258, y259);
                                                   _ -> mzero};
                                  guard (x258 == x250);
                                  guard (x259 == x251);
                                  return ()},
                              do {let {x260 = Zero};
                                  (x261, x2) <- case x1 of
                                                {Cons y261 y2 -> return (y261, y2); _ -> mzero};
                                  guard (x261 == x260);
                                  _______appendoII x0 x2;
                                  return ()}]
_______appendoII x0 x1 = msum [do {let {x262 = Zero};
                                   let {x266 = Zero};
                                   let {x265 = Succ x266};
                                   let {x264 = Succ x265};
                                   let {x267 = Nil};
                                   let {x263 = Cons x264 x267};
                                   guard (x1 == Nil);
                                   (x268, x269) <- case x0 of
                                                   {Cons y268 y269 -> return (y268, y269);
                                                    _ -> mzero};
                                   guard (x268 == x262);
                                   guard (x269 == x263);
                                   return ()},
                               do {let {x270 = Zero};
                                   (x271, x2) <- case x1 of
                                                 {Cons y271 y2 -> return (y271, y2); _ -> mzero};
                                   guard (x271 == x270);
                                   ________appendoII x0 x2;
                                   return ()}]
________appendoII x0 x1 = msum [do {let {x274 = Zero};
                                    let {x273 = Succ x274};
                                    let {x272 = Succ x273};
                                    let {x275 = Nil};
                                    guard (x1 == Nil);
                                    (x276, x277) <- case x0 of
                                                    {Cons y276 y277 -> return (y276, y277);
                                                     _ -> mzero};
                                    guard (x276 == x272);
                                    guard (x277 == x275);
                                    return ()},
                                do {let {x280 = Zero};
                                    let {x279 = Succ x280};
                                    let {x278 = Succ x279};
                                    let {x281 = Nil};
                                    (x282, x283) <- case x1 of
                                                    {Cons y282 y283 -> return (y282, y283);
                                                     _ -> mzero};
                                    guard (x282 == x278);
                                    guard (x283 == x281);
                                    guard (x0 == Nil);
                                    return ()}]
helpIIO x0 x1 = msum [do {let {x17 = Zero};
                          let {x16 = Succ x17};
                          let {x15 = Succ x16};
                          let {x19 = Zero};
                          let {x21 = Zero};
                          let {x24 = Zero};
                          let {x23 = Succ x24};
                          let {x28 = Zero};
                          let {x27 = Succ x28};
                          let {x26 = Succ x27};
                          let {x30 = Zero};
                          let {x33 = Zero};
                          let {x32 = Succ x33};
                          let {x36 = Zero};
                          let {x35 = Succ x36};
                          let {x38 = Zero};
                          let {x42 = Zero};
                          let {x41 = Succ x42};
                          let {x40 = Succ x41};
                          let {x47 = Zero};
                          let {x46 = Succ x47};
                          let {x45 = Succ x46};
                          let {x49 = Zero};
                          let {x52 = Zero};
                          let {x51 = Succ x52};
                          let {x55 = Zero};
                          let {x54 = Succ x55};
                          let {x57 = Zero};
                          let {x61 = Zero};
                          let {x60 = Succ x61};
                          let {x59 = Succ x60};
                          let {x64 = Zero};
                          let {x63 = Succ x64};
                          let {x66 = Zero};
                          let {x68 = Zero};
                          let {x72 = Zero};
                          let {x71 = Succ x72};
                          let {x70 = Succ x71};
                          let {x73 = Nil};
                          let {x69 = Cons x70 x73};
                          let {x67 = Cons x68 x69};
                          let {x65 = Cons x66 x67};
                          let {x62 = Cons x63 x65};
                          let {x58 = Cons x59 x62};
                          let {x56 = Cons x57 x58};
                          let {x53 = Cons x54 x56};
                          let {x50 = Cons x51 x53};
                          let {x48 = Cons x49 x50};
                          (x74, x75) <- case x1 of
                                        {Cons y74 y75 -> return (y74, y75); _ -> mzero};
                          guard (x74 == x45);
                          guard (x75 == x48);
                          guard (x0 == Nil);
                          let {x43 = x15};
                          x3 <- reversoO;
                          let {x39 = Cons x40 x3};
                          let {x37 = Cons x38 x39};
                          let {x34 = Cons x35 x37};
                          let {x31 = Cons x32 x34};
                          let {x29 = Cons x30 x31};
                          let {x25 = Cons x26 x29};
                          let {x22 = Cons x23 x25};
                          let {x20 = Cons x21 x22};
                          let {x18 = Cons x19 x20};
                          let {x44 = x18};
                          let {x2 = Cons x43 x44};
                          return x2},
                      do {let {x78 = Zero};
                          let {x77 = Succ x78};
                          let {x76 = Succ x77};
                          let {x80 = Zero};
                          let {x82 = Zero};
                          let {x85 = Zero};
                          let {x84 = Succ x85};
                          let {x89 = Zero};
                          let {x88 = Succ x89};
                          let {x87 = Succ x88};
                          let {x91 = Zero};
                          let {x94 = Zero};
                          let {x93 = Succ x94};
                          let {x97 = Zero};
                          let {x96 = Succ x97};
                          let {x99 = Zero};
                          let {x103 = Zero};
                          let {x102 = Succ x103};
                          let {x101 = Succ x102};
                          let {x108 = Zero};
                          let {x107 = Succ x108};
                          let {x106 = Succ x107};
                          (x109, x4) <- case x0 of
                                        {Cons y109 y4 -> return (y109, y4); _ -> mzero};
                          guard (x109 == x106);
                          appendoII x1 x4;
                          let {x104 = x76};
                          x3 <- reversoO;
                          let {x100 = Cons x101 x3};
                          let {x98 = Cons x99 x100};
                          let {x95 = Cons x96 x98};
                          let {x92 = Cons x93 x95};
                          let {x90 = Cons x91 x92};
                          let {x86 = Cons x87 x90};
                          let {x83 = Cons x84 x86};
                          let {x81 = Cons x82 x83};
                          let {x79 = Cons x80 x81};
                          let {x105 = x79};
                          let {x2 = Cons x104 x105};
                          return x2}]
helpIOI x0 x2 = msum [do {let {x17 = Zero};
                          let {x16 = Succ x17};
                          let {x15 = Succ x16};
                          let {x19 = Zero};
                          let {x21 = Zero};
                          let {x24 = Zero};
                          let {x23 = Succ x24};
                          let {x28 = Zero};
                          let {x27 = Succ x28};
                          let {x26 = Succ x27};
                          let {x30 = Zero};
                          let {x33 = Zero};
                          let {x32 = Succ x33};
                          let {x36 = Zero};
                          let {x35 = Succ x36};
                          let {x38 = Zero};
                          let {x42 = Zero};
                          let {x41 = Succ x42};
                          let {x40 = Succ x41};
                          let {x47 = Zero};
                          let {x46 = Succ x47};
                          let {x45 = Succ x46};
                          let {x49 = Zero};
                          let {x52 = Zero};
                          let {x51 = Succ x52};
                          let {x55 = Zero};
                          let {x54 = Succ x55};
                          let {x57 = Zero};
                          let {x61 = Zero};
                          let {x60 = Succ x61};
                          let {x59 = Succ x60};
                          let {x64 = Zero};
                          let {x63 = Succ x64};
                          let {x66 = Zero};
                          let {x68 = Zero};
                          let {x72 = Zero};
                          let {x71 = Succ x72};
                          let {x70 = Succ x71};
                          let {x73 = Nil};
                          let {x69 = Cons x70 x73};
                          let {x67 = Cons x68 x69};
                          let {x65 = Cons x66 x67};
                          let {x62 = Cons x63 x65};
                          let {x58 = Cons x59 x62};
                          let {x56 = Cons x57 x58};
                          let {x53 = Cons x54 x56};
                          let {x50 = Cons x51 x53};
                          let {x48 = Cons x49 x50};
                          (x43, x44) <- case x2 of
                                        {Cons y43 y44 -> return (y43, y44); _ -> mzero};
                          guard (x43 == x15);
                          guard (x0 == Nil);
                          let {x18 = x44};
                          x20 <- case x18 of
                                 {Cons y19 y20 -> do {guard (x19 == y19); return y20}; _ -> mzero};
                          x22 <- case x20 of
                                 {Cons y21 y22 -> do {guard (x21 == y21); return y22}; _ -> mzero};
                          x25 <- case x22 of
                                 {Cons y23 y25 -> do {guard (x23 == y23); return y25}; _ -> mzero};
                          x29 <- case x25 of
                                 {Cons y26 y29 -> do {guard (x26 == y26); return y29}; _ -> mzero};
                          x31 <- case x29 of
                                 {Cons y30 y31 -> do {guard (x30 == y30); return y31}; _ -> mzero};
                          x34 <- case x31 of
                                 {Cons y32 y34 -> do {guard (x32 == y32); return y34}; _ -> mzero};
                          x37 <- case x34 of
                                 {Cons y35 y37 -> do {guard (x35 == y35); return y37}; _ -> mzero};
                          x39 <- case x37 of
                                 {Cons y38 y39 -> do {guard (x38 == y38); return y39}; _ -> mzero};
                          x3 <- case x39 of
                                {Cons y40 y3 -> do {guard (x40 == y40); return y3}; _ -> mzero};
                          reversoI x3;
                          let {x74 = x45};
                          let {x75 = x48};
                          let {x1 = Cons x74 x75};
                          return x1},
                      do {let {x78 = Zero};
                          let {x77 = Succ x78};
                          let {x76 = Succ x77};
                          let {x80 = Zero};
                          let {x82 = Zero};
                          let {x85 = Zero};
                          let {x84 = Succ x85};
                          let {x89 = Zero};
                          let {x88 = Succ x89};
                          let {x87 = Succ x88};
                          let {x91 = Zero};
                          let {x94 = Zero};
                          let {x93 = Succ x94};
                          let {x97 = Zero};
                          let {x96 = Succ x97};
                          let {x99 = Zero};
                          let {x103 = Zero};
                          let {x102 = Succ x103};
                          let {x101 = Succ x102};
                          let {x108 = Zero};
                          let {x107 = Succ x108};
                          let {x106 = Succ x107};
                          (x104, x105) <- case x2 of
                                          {Cons y104 y105 -> return (y104, y105); _ -> mzero};
                          guard (x104 == x76);
                          (x109, x4) <- case x0 of
                                        {Cons y109 y4 -> return (y109, y4); _ -> mzero};
                          guard (x109 == x106);
                          let {x79 = x105};
                          x81 <- case x79 of
                                 {Cons y80 y81 -> do {guard (x80 == y80); return y81}; _ -> mzero};
                          x83 <- case x81 of
                                 {Cons y82 y83 -> do {guard (x82 == y82); return y83}; _ -> mzero};
                          x86 <- case x83 of
                                 {Cons y84 y86 -> do {guard (x84 == y84); return y86}; _ -> mzero};
                          x90 <- case x86 of
                                 {Cons y87 y90 -> do {guard (x87 == y87); return y90}; _ -> mzero};
                          x92 <- case x90 of
                                 {Cons y91 y92 -> do {guard (x91 == y91); return y92}; _ -> mzero};
                          x95 <- case x92 of
                                 {Cons y93 y95 -> do {guard (x93 == y93); return y95}; _ -> mzero};
                          x98 <- case x95 of
                                 {Cons y96 y98 -> do {guard (x96 == y96); return y98}; _ -> mzero};
                          x100 <- case x98 of
                                  {Cons y99 y100 -> do {guard (x99 == y99); return y100};
                                   _ -> mzero};
                          x3 <- case x100 of
                                {Cons y101 y3 -> do {guard (x101 == y101); return y3}; _ -> mzero};
                          reversoI x3;
                          x1 <- appendoOI x4;
                          return x1}]
appendoOI x1 = msum [do {let {x110 = Zero};
                         let {x113 = Zero};
                         let {x112 = Succ x113};
                         let {x116 = Zero};
                         let {x115 = Succ x116};
                         let {x118 = Zero};
                         let {x122 = Zero};
                         let {x121 = Succ x122};
                         let {x120 = Succ x121};
                         let {x125 = Zero};
                         let {x124 = Succ x125};
                         let {x127 = Zero};
                         let {x129 = Zero};
                         let {x133 = Zero};
                         let {x132 = Succ x133};
                         let {x131 = Succ x132};
                         let {x134 = Nil};
                         let {x130 = Cons x131 x134};
                         let {x128 = Cons x129 x130};
                         let {x126 = Cons x127 x128};
                         let {x123 = Cons x124 x126};
                         let {x119 = Cons x120 x123};
                         let {x117 = Cons x118 x119};
                         let {x114 = Cons x115 x117};
                         let {x111 = Cons x112 x114};
                         guard (x1 == Nil);
                         let {x135 = x110};
                         let {x136 = x111};
                         let {x0 = Cons x135 x136};
                         return x0},
                     do {let {x137 = Zero};
                         (x138, x2) <- case x1 of
                                       {Cons y138 y2 -> return (y138, y2); _ -> mzero};
                         guard (x138 == x137);
                         x0 <- _appendoOI x2;
                         return x0}]
_appendoOI x1 = msum [do {let {x140 = Zero};
                          let {x139 = Succ x140};
                          let {x143 = Zero};
                          let {x142 = Succ x143};
                          let {x145 = Zero};
                          let {x149 = Zero};
                          let {x148 = Succ x149};
                          let {x147 = Succ x148};
                          let {x152 = Zero};
                          let {x151 = Succ x152};
                          let {x154 = Zero};
                          let {x156 = Zero};
                          let {x160 = Zero};
                          let {x159 = Succ x160};
                          let {x158 = Succ x159};
                          let {x161 = Nil};
                          let {x157 = Cons x158 x161};
                          let {x155 = Cons x156 x157};
                          let {x153 = Cons x154 x155};
                          let {x150 = Cons x151 x153};
                          let {x146 = Cons x147 x150};
                          let {x144 = Cons x145 x146};
                          let {x141 = Cons x142 x144};
                          guard (x1 == Nil);
                          let {x162 = x139};
                          let {x163 = x141};
                          let {x0 = Cons x162 x163};
                          return x0},
                      do {let {x165 = Zero};
                          let {x164 = Succ x165};
                          (x166, x2) <- case x1 of
                                        {Cons y166 y2 -> return (y166, y2); _ -> mzero};
                          guard (x166 == x164);
                          x0 <- __appendoOI x2;
                          return x0}]
__appendoOI x1 = msum [do {let {x168 = Zero};
                           let {x167 = Succ x168};
                           let {x170 = Zero};
                           let {x174 = Zero};
                           let {x173 = Succ x174};
                           let {x172 = Succ x173};
                           let {x177 = Zero};
                           let {x176 = Succ x177};
                           let {x179 = Zero};
                           let {x181 = Zero};
                           let {x185 = Zero};
                           let {x184 = Succ x185};
                           let {x183 = Succ x184};
                           let {x186 = Nil};
                           let {x182 = Cons x183 x186};
                           let {x180 = Cons x181 x182};
                           let {x178 = Cons x179 x180};
                           let {x175 = Cons x176 x178};
                           let {x171 = Cons x172 x175};
                           let {x169 = Cons x170 x171};
                           guard (x1 == Nil);
                           let {x187 = x167};
                           let {x188 = x169};
                           let {x0 = Cons x187 x188};
                           return x0},
                       do {let {x190 = Zero};
                           let {x189 = Succ x190};
                           (x191, x2) <- case x1 of
                                         {Cons y191 y2 -> return (y191, y2); _ -> mzero};
                           guard (x191 == x189);
                           x0 <- ___appendoOI x2;
                           return x0}]
___appendoOI x1 = msum [do {let {x192 = Zero};
                            let {x196 = Zero};
                            let {x195 = Succ x196};
                            let {x194 = Succ x195};
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
                            let {x197 = Cons x198 x200};
                            let {x193 = Cons x194 x197};
                            guard (x1 == Nil);
                            let {x209 = x192};
                            let {x210 = x193};
                            let {x0 = Cons x209 x210};
                            return x0},
                        do {let {x211 = Zero};
                            (x212, x2) <- case x1 of
                                          {Cons y212 y2 -> return (y212, y2); _ -> mzero};
                            guard (x212 == x211);
                            x0 <- ____appendoOI x2;
                            return x0}]
____appendoOI x1 = msum [do {let {x215 = Zero};
                             let {x214 = Succ x215};
                             let {x213 = Succ x214};
                             let {x218 = Zero};
                             let {x217 = Succ x218};
                             let {x220 = Zero};
                             let {x222 = Zero};
                             let {x226 = Zero};
                             let {x225 = Succ x226};
                             let {x224 = Succ x225};
                             let {x227 = Nil};
                             let {x223 = Cons x224 x227};
                             let {x221 = Cons x222 x223};
                             let {x219 = Cons x220 x221};
                             let {x216 = Cons x217 x219};
                             guard (x1 == Nil);
                             let {x228 = x213};
                             let {x229 = x216};
                             let {x0 = Cons x228 x229};
                             return x0},
                         do {let {x232 = Zero};
                             let {x231 = Succ x232};
                             let {x230 = Succ x231};
                             (x233, x2) <- case x1 of
                                           {Cons y233 y2 -> return (y233, y2); _ -> mzero};
                             guard (x233 == x230);
                             x0 <- _____appendoOI x2;
                             return x0}]
_____appendoOI x1 = msum [do {let {x235 = Zero};
                              let {x234 = Succ x235};
                              let {x237 = Zero};
                              let {x239 = Zero};
                              let {x243 = Zero};
                              let {x242 = Succ x243};
                              let {x241 = Succ x242};
                              let {x244 = Nil};
                              let {x240 = Cons x241 x244};
                              let {x238 = Cons x239 x240};
                              let {x236 = Cons x237 x238};
                              guard (x1 == Nil);
                              let {x245 = x234};
                              let {x246 = x236};
                              let {x0 = Cons x245 x246};
                              return x0},
                          do {let {x248 = Zero};
                              let {x247 = Succ x248};
                              (x249, x2) <- case x1 of
                                            {Cons y249 y2 -> return (y249, y2); _ -> mzero};
                              guard (x249 == x247);
                              x0 <- ______appendoOI x2;
                              return x0}]
______appendoOI x1 = msum [do {let {x250 = Zero};
                               let {x252 = Zero};
                               let {x256 = Zero};
                               let {x255 = Succ x256};
                               let {x254 = Succ x255};
                               let {x257 = Nil};
                               let {x253 = Cons x254 x257};
                               let {x251 = Cons x252 x253};
                               guard (x1 == Nil);
                               let {x258 = x250};
                               let {x259 = x251};
                               let {x0 = Cons x258 x259};
                               return x0},
                           do {let {x260 = Zero};
                               (x261, x2) <- case x1 of
                                             {Cons y261 y2 -> return (y261, y2); _ -> mzero};
                               guard (x261 == x260);
                               x0 <- _______appendoOI x2;
                               return x0}]
_______appendoOI x1 = msum [do {let {x262 = Zero};
                                let {x266 = Zero};
                                let {x265 = Succ x266};
                                let {x264 = Succ x265};
                                let {x267 = Nil};
                                let {x263 = Cons x264 x267};
                                guard (x1 == Nil);
                                let {x268 = x262};
                                let {x269 = x263};
                                let {x0 = Cons x268 x269};
                                return x0},
                            do {let {x270 = Zero};
                                (x271, x2) <- case x1 of
                                              {Cons y271 y2 -> return (y271, y2); _ -> mzero};
                                guard (x271 == x270);
                                x0 <- ________appendoOI x2;
                                return x0}]
________appendoOI x1 = msum [do {let {x274 = Zero};
                                 let {x273 = Succ x274};
                                 let {x272 = Succ x273};
                                 let {x275 = Nil};
                                 guard (x1 == Nil);
                                 let {x276 = x272};
                                 let {x277 = x275};
                                 let {x0 = Cons x276 x277};
                                 return x0},
                             do {let {x280 = Zero};
                                 let {x279 = Succ x280};
                                 let {x278 = Succ x279};
                                 let {x281 = Nil};
                                 let {x0 = Nil};
                                 (x282, x283) <- case x1 of
                                                 {Cons y282 y283 -> return (y282, y283);
                                                  _ -> mzero};
                                 guard (x282 == x278);
                                 guard (x283 == x281);
                                 return x0}]
helpIOO x0 = msum [do {let {x17 = Zero};
                       let {x16 = Succ x17};
                       let {x15 = Succ x16};
                       let {x19 = Zero};
                       let {x21 = Zero};
                       let {x24 = Zero};
                       let {x23 = Succ x24};
                       let {x28 = Zero};
                       let {x27 = Succ x28};
                       let {x26 = Succ x27};
                       let {x30 = Zero};
                       let {x33 = Zero};
                       let {x32 = Succ x33};
                       let {x36 = Zero};
                       let {x35 = Succ x36};
                       let {x38 = Zero};
                       let {x42 = Zero};
                       let {x41 = Succ x42};
                       let {x40 = Succ x41};
                       let {x47 = Zero};
                       let {x46 = Succ x47};
                       let {x45 = Succ x46};
                       let {x49 = Zero};
                       let {x52 = Zero};
                       let {x51 = Succ x52};
                       let {x55 = Zero};
                       let {x54 = Succ x55};
                       let {x57 = Zero};
                       let {x61 = Zero};
                       let {x60 = Succ x61};
                       let {x59 = Succ x60};
                       let {x64 = Zero};
                       let {x63 = Succ x64};
                       let {x66 = Zero};
                       let {x68 = Zero};
                       let {x72 = Zero};
                       let {x71 = Succ x72};
                       let {x70 = Succ x71};
                       let {x73 = Nil};
                       let {x69 = Cons x70 x73};
                       let {x67 = Cons x68 x69};
                       let {x65 = Cons x66 x67};
                       let {x62 = Cons x63 x65};
                       let {x58 = Cons x59 x62};
                       let {x56 = Cons x57 x58};
                       let {x53 = Cons x54 x56};
                       let {x50 = Cons x51 x53};
                       let {x48 = Cons x49 x50};
                       guard (x0 == Nil);
                       let {x43 = x15};
                       let {x74 = x45};
                       let {x75 = x48};
                       let {x1 = Cons x74 x75};
                       x3 <- reversoO;
                       let {x39 = Cons x40 x3};
                       let {x37 = Cons x38 x39};
                       let {x34 = Cons x35 x37};
                       let {x31 = Cons x32 x34};
                       let {x29 = Cons x30 x31};
                       let {x25 = Cons x26 x29};
                       let {x22 = Cons x23 x25};
                       let {x20 = Cons x21 x22};
                       let {x18 = Cons x19 x20};
                       let {x44 = x18};
                       let {x2 = Cons x43 x44};
                       return (x1, x2)},
                   do {let {x78 = Zero};
                       let {x77 = Succ x78};
                       let {x76 = Succ x77};
                       let {x80 = Zero};
                       let {x82 = Zero};
                       let {x85 = Zero};
                       let {x84 = Succ x85};
                       let {x89 = Zero};
                       let {x88 = Succ x89};
                       let {x87 = Succ x88};
                       let {x91 = Zero};
                       let {x94 = Zero};
                       let {x93 = Succ x94};
                       let {x97 = Zero};
                       let {x96 = Succ x97};
                       let {x99 = Zero};
                       let {x103 = Zero};
                       let {x102 = Succ x103};
                       let {x101 = Succ x102};
                       let {x108 = Zero};
                       let {x107 = Succ x108};
                       let {x106 = Succ x107};
                       (x109, x4) <- case x0 of
                                     {Cons y109 y4 -> return (y109, y4); _ -> mzero};
                       guard (x109 == x106);
                       let {x104 = x76};
                       x3 <- reversoO;
                       let {x100 = Cons x101 x3};
                       let {x98 = Cons x99 x100};
                       let {x95 = Cons x96 x98};
                       let {x92 = Cons x93 x95};
                       let {x90 = Cons x91 x92};
                       let {x86 = Cons x87 x90};
                       let {x83 = Cons x84 x86};
                       let {x81 = Cons x82 x83};
                       let {x79 = Cons x80 x81};
                       let {x105 = x79};
                       let {x2 = Cons x104 x105};
                       x1 <- appendoOI x4;
                       return (x1, x2)}]
helpOII x1 x2 = msum [do {let {x17 = Zero};
                          let {x16 = Succ x17};
                          let {x15 = Succ x16};
                          let {x19 = Zero};
                          let {x21 = Zero};
                          let {x24 = Zero};
                          let {x23 = Succ x24};
                          let {x28 = Zero};
                          let {x27 = Succ x28};
                          let {x26 = Succ x27};
                          let {x30 = Zero};
                          let {x33 = Zero};
                          let {x32 = Succ x33};
                          let {x36 = Zero};
                          let {x35 = Succ x36};
                          let {x38 = Zero};
                          let {x42 = Zero};
                          let {x41 = Succ x42};
                          let {x40 = Succ x41};
                          let {x47 = Zero};
                          let {x46 = Succ x47};
                          let {x45 = Succ x46};
                          let {x49 = Zero};
                          let {x52 = Zero};
                          let {x51 = Succ x52};
                          let {x55 = Zero};
                          let {x54 = Succ x55};
                          let {x57 = Zero};
                          let {x61 = Zero};
                          let {x60 = Succ x61};
                          let {x59 = Succ x60};
                          let {x64 = Zero};
                          let {x63 = Succ x64};
                          let {x66 = Zero};
                          let {x68 = Zero};
                          let {x72 = Zero};
                          let {x71 = Succ x72};
                          let {x70 = Succ x71};
                          let {x73 = Nil};
                          let {x69 = Cons x70 x73};
                          let {x67 = Cons x68 x69};
                          let {x65 = Cons x66 x67};
                          let {x62 = Cons x63 x65};
                          let {x58 = Cons x59 x62};
                          let {x56 = Cons x57 x58};
                          let {x53 = Cons x54 x56};
                          let {x50 = Cons x51 x53};
                          let {x48 = Cons x49 x50};
                          let {x0 = Nil};
                          (x43, x44) <- case x2 of
                                        {Cons y43 y44 -> return (y43, y44); _ -> mzero};
                          guard (x43 == x15);
                          (x74, x75) <- case x1 of
                                        {Cons y74 y75 -> return (y74, y75); _ -> mzero};
                          guard (x74 == x45);
                          guard (x75 == x48);
                          let {x18 = x44};
                          x20 <- case x18 of
                                 {Cons y19 y20 -> do {guard (x19 == y19); return y20}; _ -> mzero};
                          x22 <- case x20 of
                                 {Cons y21 y22 -> do {guard (x21 == y21); return y22}; _ -> mzero};
                          x25 <- case x22 of
                                 {Cons y23 y25 -> do {guard (x23 == y23); return y25}; _ -> mzero};
                          x29 <- case x25 of
                                 {Cons y26 y29 -> do {guard (x26 == y26); return y29}; _ -> mzero};
                          x31 <- case x29 of
                                 {Cons y30 y31 -> do {guard (x30 == y30); return y31}; _ -> mzero};
                          x34 <- case x31 of
                                 {Cons y32 y34 -> do {guard (x32 == y32); return y34}; _ -> mzero};
                          x37 <- case x34 of
                                 {Cons y35 y37 -> do {guard (x35 == y35); return y37}; _ -> mzero};
                          x39 <- case x37 of
                                 {Cons y38 y39 -> do {guard (x38 == y38); return y39}; _ -> mzero};
                          x3 <- case x39 of
                                {Cons y40 y3 -> do {guard (x40 == y40); return y3}; _ -> mzero};
                          reversoI x3;
                          return x0},
                      do {let {x78 = Zero};
                          let {x77 = Succ x78};
                          let {x76 = Succ x77};
                          let {x80 = Zero};
                          let {x82 = Zero};
                          let {x85 = Zero};
                          let {x84 = Succ x85};
                          let {x89 = Zero};
                          let {x88 = Succ x89};
                          let {x87 = Succ x88};
                          let {x91 = Zero};
                          let {x94 = Zero};
                          let {x93 = Succ x94};
                          let {x97 = Zero};
                          let {x96 = Succ x97};
                          let {x99 = Zero};
                          let {x103 = Zero};
                          let {x102 = Succ x103};
                          let {x101 = Succ x102};
                          let {x108 = Zero};
                          let {x107 = Succ x108};
                          let {x106 = Succ x107};
                          (x104, x105) <- case x2 of
                                          {Cons y104 y105 -> return (y104, y105); _ -> mzero};
                          guard (x104 == x76);
                          let {x79 = x105};
                          x81 <- case x79 of
                                 {Cons y80 y81 -> do {guard (x80 == y80); return y81}; _ -> mzero};
                          x83 <- case x81 of
                                 {Cons y82 y83 -> do {guard (x82 == y82); return y83}; _ -> mzero};
                          x86 <- case x83 of
                                 {Cons y84 y86 -> do {guard (x84 == y84); return y86}; _ -> mzero};
                          x90 <- case x86 of
                                 {Cons y87 y90 -> do {guard (x87 == y87); return y90}; _ -> mzero};
                          x92 <- case x90 of
                                 {Cons y91 y92 -> do {guard (x91 == y91); return y92}; _ -> mzero};
                          x95 <- case x92 of
                                 {Cons y93 y95 -> do {guard (x93 == y93); return y95}; _ -> mzero};
                          x98 <- case x95 of
                                 {Cons y96 y98 -> do {guard (x96 == y96); return y98}; _ -> mzero};
                          x100 <- case x98 of
                                  {Cons y99 y100 -> do {guard (x99 == y99); return y100};
                                   _ -> mzero};
                          x3 <- case x100 of
                                {Cons y101 y3 -> do {guard (x101 == y101); return y3}; _ -> mzero};
                          reversoI x3;
                          let {x109 = x106};
                          x4 <- appendoIO x1;
                          let {x0 = Cons x109 x4};
                          return x0}]
appendoIO x0 = msum [do {let {x1 = Nil};
                         let {x110 = Zero};
                         let {x113 = Zero};
                         let {x112 = Succ x113};
                         let {x116 = Zero};
                         let {x115 = Succ x116};
                         let {x118 = Zero};
                         let {x122 = Zero};
                         let {x121 = Succ x122};
                         let {x120 = Succ x121};
                         let {x125 = Zero};
                         let {x124 = Succ x125};
                         let {x127 = Zero};
                         let {x129 = Zero};
                         let {x133 = Zero};
                         let {x132 = Succ x133};
                         let {x131 = Succ x132};
                         let {x134 = Nil};
                         let {x130 = Cons x131 x134};
                         let {x128 = Cons x129 x130};
                         let {x126 = Cons x127 x128};
                         let {x123 = Cons x124 x126};
                         let {x119 = Cons x120 x123};
                         let {x117 = Cons x118 x119};
                         let {x114 = Cons x115 x117};
                         let {x111 = Cons x112 x114};
                         (x135, x136) <- case x0 of
                                         {Cons y135 y136 -> return (y135, y136); _ -> mzero};
                         guard (x135 == x110);
                         guard (x136 == x111);
                         return x1},
                     do {let {x137 = Zero};
                         let {x138 = x137};
                         x2 <- _appendoIO x0;
                         let {x1 = Cons x138 x2};
                         return x1}]
_appendoIO x0 = msum [do {let {x1 = Nil};
                          let {x140 = Zero};
                          let {x139 = Succ x140};
                          let {x143 = Zero};
                          let {x142 = Succ x143};
                          let {x145 = Zero};
                          let {x149 = Zero};
                          let {x148 = Succ x149};
                          let {x147 = Succ x148};
                          let {x152 = Zero};
                          let {x151 = Succ x152};
                          let {x154 = Zero};
                          let {x156 = Zero};
                          let {x160 = Zero};
                          let {x159 = Succ x160};
                          let {x158 = Succ x159};
                          let {x161 = Nil};
                          let {x157 = Cons x158 x161};
                          let {x155 = Cons x156 x157};
                          let {x153 = Cons x154 x155};
                          let {x150 = Cons x151 x153};
                          let {x146 = Cons x147 x150};
                          let {x144 = Cons x145 x146};
                          let {x141 = Cons x142 x144};
                          (x162, x163) <- case x0 of
                                          {Cons y162 y163 -> return (y162, y163); _ -> mzero};
                          guard (x162 == x139);
                          guard (x163 == x141);
                          return x1},
                      do {let {x165 = Zero};
                          let {x164 = Succ x165};
                          let {x166 = x164};
                          x2 <- __appendoIO x0;
                          let {x1 = Cons x166 x2};
                          return x1}]
__appendoIO x0 = msum [do {let {x1 = Nil};
                           let {x168 = Zero};
                           let {x167 = Succ x168};
                           let {x170 = Zero};
                           let {x174 = Zero};
                           let {x173 = Succ x174};
                           let {x172 = Succ x173};
                           let {x177 = Zero};
                           let {x176 = Succ x177};
                           let {x179 = Zero};
                           let {x181 = Zero};
                           let {x185 = Zero};
                           let {x184 = Succ x185};
                           let {x183 = Succ x184};
                           let {x186 = Nil};
                           let {x182 = Cons x183 x186};
                           let {x180 = Cons x181 x182};
                           let {x178 = Cons x179 x180};
                           let {x175 = Cons x176 x178};
                           let {x171 = Cons x172 x175};
                           let {x169 = Cons x170 x171};
                           (x187, x188) <- case x0 of
                                           {Cons y187 y188 -> return (y187, y188); _ -> mzero};
                           guard (x187 == x167);
                           guard (x188 == x169);
                           return x1},
                       do {let {x190 = Zero};
                           let {x189 = Succ x190};
                           let {x191 = x189};
                           x2 <- ___appendoIO x0;
                           let {x1 = Cons x191 x2};
                           return x1}]
___appendoIO x0 = msum [do {let {x1 = Nil};
                            let {x192 = Zero};
                            let {x196 = Zero};
                            let {x195 = Succ x196};
                            let {x194 = Succ x195};
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
                            let {x197 = Cons x198 x200};
                            let {x193 = Cons x194 x197};
                            (x209, x210) <- case x0 of
                                            {Cons y209 y210 -> return (y209, y210); _ -> mzero};
                            guard (x209 == x192);
                            guard (x210 == x193);
                            return x1},
                        do {let {x211 = Zero};
                            let {x212 = x211};
                            x2 <- ____appendoIO x0;
                            let {x1 = Cons x212 x2};
                            return x1}]
____appendoIO x0 = msum [do {let {x1 = Nil};
                             let {x215 = Zero};
                             let {x214 = Succ x215};
                             let {x213 = Succ x214};
                             let {x218 = Zero};
                             let {x217 = Succ x218};
                             let {x220 = Zero};
                             let {x222 = Zero};
                             let {x226 = Zero};
                             let {x225 = Succ x226};
                             let {x224 = Succ x225};
                             let {x227 = Nil};
                             let {x223 = Cons x224 x227};
                             let {x221 = Cons x222 x223};
                             let {x219 = Cons x220 x221};
                             let {x216 = Cons x217 x219};
                             (x228, x229) <- case x0 of
                                             {Cons y228 y229 -> return (y228, y229); _ -> mzero};
                             guard (x228 == x213);
                             guard (x229 == x216);
                             return x1},
                         do {let {x232 = Zero};
                             let {x231 = Succ x232};
                             let {x230 = Succ x231};
                             let {x233 = x230};
                             x2 <- _____appendoIO x0;
                             let {x1 = Cons x233 x2};
                             return x1}]
_____appendoIO x0 = msum [do {let {x1 = Nil};
                              let {x235 = Zero};
                              let {x234 = Succ x235};
                              let {x237 = Zero};
                              let {x239 = Zero};
                              let {x243 = Zero};
                              let {x242 = Succ x243};
                              let {x241 = Succ x242};
                              let {x244 = Nil};
                              let {x240 = Cons x241 x244};
                              let {x238 = Cons x239 x240};
                              let {x236 = Cons x237 x238};
                              (x245, x246) <- case x0 of
                                              {Cons y245 y246 -> return (y245, y246); _ -> mzero};
                              guard (x245 == x234);
                              guard (x246 == x236);
                              return x1},
                          do {let {x248 = Zero};
                              let {x247 = Succ x248};
                              let {x249 = x247};
                              x2 <- ______appendoIO x0;
                              let {x1 = Cons x249 x2};
                              return x1}]
______appendoIO x0 = msum [do {let {x1 = Nil};
                               let {x250 = Zero};
                               let {x252 = Zero};
                               let {x256 = Zero};
                               let {x255 = Succ x256};
                               let {x254 = Succ x255};
                               let {x257 = Nil};
                               let {x253 = Cons x254 x257};
                               let {x251 = Cons x252 x253};
                               (x258, x259) <- case x0 of
                                               {Cons y258 y259 -> return (y258, y259); _ -> mzero};
                               guard (x258 == x250);
                               guard (x259 == x251);
                               return x1},
                           do {let {x260 = Zero};
                               let {x261 = x260};
                               x2 <- _______appendoIO x0;
                               let {x1 = Cons x261 x2};
                               return x1}]
_______appendoIO x0 = msum [do {let {x1 = Nil};
                                let {x262 = Zero};
                                let {x266 = Zero};
                                let {x265 = Succ x266};
                                let {x264 = Succ x265};
                                let {x267 = Nil};
                                let {x263 = Cons x264 x267};
                                (x268, x269) <- case x0 of
                                                {Cons y268 y269 -> return (y268, y269); _ -> mzero};
                                guard (x268 == x262);
                                guard (x269 == x263);
                                return x1},
                            do {let {x270 = Zero};
                                let {x271 = x270};
                                x2 <- ________appendoIO x0;
                                let {x1 = Cons x271 x2};
                                return x1}]
________appendoIO x0 = msum [do {let {x1 = Nil};
                                 let {x274 = Zero};
                                 let {x273 = Succ x274};
                                 let {x272 = Succ x273};
                                 let {x275 = Nil};
                                 (x276, x277) <- case x0 of
                                                 {Cons y276 y277 -> return (y276, y277);
                                                  _ -> mzero};
                                 guard (x276 == x272);
                                 guard (x277 == x275);
                                 return x1},
                             do {let {x280 = Zero};
                                 let {x279 = Succ x280};
                                 let {x278 = Succ x279};
                                 let {x281 = Nil};
                                 guard (x0 == Nil);
                                 let {x282 = x278};
                                 let {x283 = x281};
                                 let {x1 = Cons x282 x283};
                                 return x1}]
helpOIO x1 = msum [do {let {x17 = Zero};
                       let {x16 = Succ x17};
                       let {x15 = Succ x16};
                       let {x19 = Zero};
                       let {x21 = Zero};
                       let {x24 = Zero};
                       let {x23 = Succ x24};
                       let {x28 = Zero};
                       let {x27 = Succ x28};
                       let {x26 = Succ x27};
                       let {x30 = Zero};
                       let {x33 = Zero};
                       let {x32 = Succ x33};
                       let {x36 = Zero};
                       let {x35 = Succ x36};
                       let {x38 = Zero};
                       let {x42 = Zero};
                       let {x41 = Succ x42};
                       let {x40 = Succ x41};
                       let {x47 = Zero};
                       let {x46 = Succ x47};
                       let {x45 = Succ x46};
                       let {x49 = Zero};
                       let {x52 = Zero};
                       let {x51 = Succ x52};
                       let {x55 = Zero};
                       let {x54 = Succ x55};
                       let {x57 = Zero};
                       let {x61 = Zero};
                       let {x60 = Succ x61};
                       let {x59 = Succ x60};
                       let {x64 = Zero};
                       let {x63 = Succ x64};
                       let {x66 = Zero};
                       let {x68 = Zero};
                       let {x72 = Zero};
                       let {x71 = Succ x72};
                       let {x70 = Succ x71};
                       let {x73 = Nil};
                       let {x69 = Cons x70 x73};
                       let {x67 = Cons x68 x69};
                       let {x65 = Cons x66 x67};
                       let {x62 = Cons x63 x65};
                       let {x58 = Cons x59 x62};
                       let {x56 = Cons x57 x58};
                       let {x53 = Cons x54 x56};
                       let {x50 = Cons x51 x53};
                       let {x48 = Cons x49 x50};
                       let {x0 = Nil};
                       (x74, x75) <- case x1 of
                                     {Cons y74 y75 -> return (y74, y75); _ -> mzero};
                       guard (x74 == x45);
                       guard (x75 == x48);
                       let {x43 = x15};
                       x3 <- reversoO;
                       let {x39 = Cons x40 x3};
                       let {x37 = Cons x38 x39};
                       let {x34 = Cons x35 x37};
                       let {x31 = Cons x32 x34};
                       let {x29 = Cons x30 x31};
                       let {x25 = Cons x26 x29};
                       let {x22 = Cons x23 x25};
                       let {x20 = Cons x21 x22};
                       let {x18 = Cons x19 x20};
                       let {x44 = x18};
                       let {x2 = Cons x43 x44};
                       return (x0, x2)},
                   do {let {x78 = Zero};
                       let {x77 = Succ x78};
                       let {x76 = Succ x77};
                       let {x80 = Zero};
                       let {x82 = Zero};
                       let {x85 = Zero};
                       let {x84 = Succ x85};
                       let {x89 = Zero};
                       let {x88 = Succ x89};
                       let {x87 = Succ x88};
                       let {x91 = Zero};
                       let {x94 = Zero};
                       let {x93 = Succ x94};
                       let {x97 = Zero};
                       let {x96 = Succ x97};
                       let {x99 = Zero};
                       let {x103 = Zero};
                       let {x102 = Succ x103};
                       let {x101 = Succ x102};
                       let {x108 = Zero};
                       let {x107 = Succ x108};
                       let {x106 = Succ x107};
                       let {x104 = x76};
                       let {x109 = x106};
                       x3 <- reversoO;
                       let {x100 = Cons x101 x3};
                       let {x98 = Cons x99 x100};
                       let {x95 = Cons x96 x98};
                       let {x92 = Cons x93 x95};
                       let {x90 = Cons x91 x92};
                       let {x86 = Cons x87 x90};
                       let {x83 = Cons x84 x86};
                       let {x81 = Cons x82 x83};
                       let {x79 = Cons x80 x81};
                       let {x105 = x79};
                       let {x2 = Cons x104 x105};
                       x4 <- appendoIO x1;
                       let {x0 = Cons x109 x4};
                       return (x0, x2)}]
helpOOI x2 gen_helpOOI_x4 = msum [do {let {x17 = Zero};
                                      let {x16 = Succ x17};
                                      let {x15 = Succ x16};
                                      let {x19 = Zero};
                                      let {x21 = Zero};
                                      let {x24 = Zero};
                                      let {x23 = Succ x24};
                                      let {x28 = Zero};
                                      let {x27 = Succ x28};
                                      let {x26 = Succ x27};
                                      let {x30 = Zero};
                                      let {x33 = Zero};
                                      let {x32 = Succ x33};
                                      let {x36 = Zero};
                                      let {x35 = Succ x36};
                                      let {x38 = Zero};
                                      let {x42 = Zero};
                                      let {x41 = Succ x42};
                                      let {x40 = Succ x41};
                                      let {x47 = Zero};
                                      let {x46 = Succ x47};
                                      let {x45 = Succ x46};
                                      let {x49 = Zero};
                                      let {x52 = Zero};
                                      let {x51 = Succ x52};
                                      let {x55 = Zero};
                                      let {x54 = Succ x55};
                                      let {x57 = Zero};
                                      let {x61 = Zero};
                                      let {x60 = Succ x61};
                                      let {x59 = Succ x60};
                                      let {x64 = Zero};
                                      let {x63 = Succ x64};
                                      let {x66 = Zero};
                                      let {x68 = Zero};
                                      let {x72 = Zero};
                                      let {x71 = Succ x72};
                                      let {x70 = Succ x71};
                                      let {x73 = Nil};
                                      let {x69 = Cons x70 x73};
                                      let {x67 = Cons x68 x69};
                                      let {x65 = Cons x66 x67};
                                      let {x62 = Cons x63 x65};
                                      let {x58 = Cons x59 x62};
                                      let {x56 = Cons x57 x58};
                                      let {x53 = Cons x54 x56};
                                      let {x50 = Cons x51 x53};
                                      let {x48 = Cons x49 x50};
                                      let {x0 = Nil};
                                      (x43, x44) <- case x2 of
                                                    {Cons y43 y44 -> return (y43, y44); _ -> mzero};
                                      guard (x43 == x15);
                                      let {x18 = x44};
                                      x20 <- case x18 of
                                             {Cons y19 y20 -> do {guard (x19 == y19); return y20};
                                              _ -> mzero};
                                      x22 <- case x20 of
                                             {Cons y21 y22 -> do {guard (x21 == y21); return y22};
                                              _ -> mzero};
                                      x25 <- case x22 of
                                             {Cons y23 y25 -> do {guard (x23 == y23); return y25};
                                              _ -> mzero};
                                      x29 <- case x25 of
                                             {Cons y26 y29 -> do {guard (x26 == y26); return y29};
                                              _ -> mzero};
                                      x31 <- case x29 of
                                             {Cons y30 y31 -> do {guard (x30 == y30); return y31};
                                              _ -> mzero};
                                      x34 <- case x31 of
                                             {Cons y32 y34 -> do {guard (x32 == y32); return y34};
                                              _ -> mzero};
                                      x37 <- case x34 of
                                             {Cons y35 y37 -> do {guard (x35 == y35); return y37};
                                              _ -> mzero};
                                      x39 <- case x37 of
                                             {Cons y38 y39 -> do {guard (x38 == y38); return y39};
                                              _ -> mzero};
                                      x3 <- case x39 of
                                            {Cons y40 y3 -> do {guard (x40 == y40); return y3};
                                             _ -> mzero};
                                      reversoI x3;
                                      let {x74 = x45};
                                      let {x75 = x48};
                                      let {x1 = Cons x74 x75};
                                      return (x0, x1)},
                                  do {let {x78 = Zero};
                                      let {x77 = Succ x78};
                                      let {x76 = Succ x77};
                                      let {x80 = Zero};
                                      let {x82 = Zero};
                                      let {x85 = Zero};
                                      let {x84 = Succ x85};
                                      let {x89 = Zero};
                                      let {x88 = Succ x89};
                                      let {x87 = Succ x88};
                                      let {x91 = Zero};
                                      let {x94 = Zero};
                                      let {x93 = Succ x94};
                                      let {x97 = Zero};
                                      let {x96 = Succ x97};
                                      let {x99 = Zero};
                                      let {x103 = Zero};
                                      let {x102 = Succ x103};
                                      let {x101 = Succ x102};
                                      let {x108 = Zero};
                                      let {x107 = Succ x108};
                                      let {x106 = Succ x107};
                                      (x104, x105) <- case x2 of
                                                      {Cons y104 y105 -> return (y104, y105);
                                                       _ -> mzero};
                                      guard (x104 == x76);
                                      let {x79 = x105};
                                      x81 <- case x79 of
                                             {Cons y80 y81 -> do {guard (x80 == y80); return y81};
                                              _ -> mzero};
                                      x83 <- case x81 of
                                             {Cons y82 y83 -> do {guard (x82 == y82); return y83};
                                              _ -> mzero};
                                      x86 <- case x83 of
                                             {Cons y84 y86 -> do {guard (x84 == y84); return y86};
                                              _ -> mzero};
                                      x90 <- case x86 of
                                             {Cons y87 y90 -> do {guard (x87 == y87); return y90};
                                              _ -> mzero};
                                      x92 <- case x90 of
                                             {Cons y91 y92 -> do {guard (x91 == y91); return y92};
                                              _ -> mzero};
                                      x95 <- case x92 of
                                             {Cons y93 y95 -> do {guard (x93 == y93); return y95};
                                              _ -> mzero};
                                      x98 <- case x95 of
                                             {Cons y96 y98 -> do {guard (x96 == y96); return y98};
                                              _ -> mzero};
                                      x100 <- case x98 of
                                              {Cons y99 y100 -> do {guard (x99 == y99);
                                                                    return y100};
                                               _ -> mzero};
                                      x3 <- case x100 of
                                            {Cons y101 y3 -> do {guard (x101 == y101); return y3};
                                             _ -> mzero};
                                      reversoI x3;
                                      let {x109 = x106};
                                      (x0, x4) <- do {x4 <- gen_helpOOI_x4;
                                                      let {x0 = Cons x109 x4};
                                                      return (x0, x4)};
                                      x1 <- appendoOI x4;
                                      return (x0, x1)}]
helpOOO gen_helpOOO_x4 = msum [do {let {x17 = Zero};
                                   let {x16 = Succ x17};
                                   let {x15 = Succ x16};
                                   let {x19 = Zero};
                                   let {x21 = Zero};
                                   let {x24 = Zero};
                                   let {x23 = Succ x24};
                                   let {x28 = Zero};
                                   let {x27 = Succ x28};
                                   let {x26 = Succ x27};
                                   let {x30 = Zero};
                                   let {x33 = Zero};
                                   let {x32 = Succ x33};
                                   let {x36 = Zero};
                                   let {x35 = Succ x36};
                                   let {x38 = Zero};
                                   let {x42 = Zero};
                                   let {x41 = Succ x42};
                                   let {x40 = Succ x41};
                                   let {x47 = Zero};
                                   let {x46 = Succ x47};
                                   let {x45 = Succ x46};
                                   let {x49 = Zero};
                                   let {x52 = Zero};
                                   let {x51 = Succ x52};
                                   let {x55 = Zero};
                                   let {x54 = Succ x55};
                                   let {x57 = Zero};
                                   let {x61 = Zero};
                                   let {x60 = Succ x61};
                                   let {x59 = Succ x60};
                                   let {x64 = Zero};
                                   let {x63 = Succ x64};
                                   let {x66 = Zero};
                                   let {x68 = Zero};
                                   let {x72 = Zero};
                                   let {x71 = Succ x72};
                                   let {x70 = Succ x71};
                                   let {x73 = Nil};
                                   let {x69 = Cons x70 x73};
                                   let {x67 = Cons x68 x69};
                                   let {x65 = Cons x66 x67};
                                   let {x62 = Cons x63 x65};
                                   let {x58 = Cons x59 x62};
                                   let {x56 = Cons x57 x58};
                                   let {x53 = Cons x54 x56};
                                   let {x50 = Cons x51 x53};
                                   let {x48 = Cons x49 x50};
                                   let {x0 = Nil};
                                   let {x43 = x15};
                                   let {x74 = x45};
                                   let {x75 = x48};
                                   let {x1 = Cons x74 x75};
                                   x3 <- reversoO;
                                   let {x39 = Cons x40 x3};
                                   let {x37 = Cons x38 x39};
                                   let {x34 = Cons x35 x37};
                                   let {x31 = Cons x32 x34};
                                   let {x29 = Cons x30 x31};
                                   let {x25 = Cons x26 x29};
                                   let {x22 = Cons x23 x25};
                                   let {x20 = Cons x21 x22};
                                   let {x18 = Cons x19 x20};
                                   let {x44 = x18};
                                   let {x2 = Cons x43 x44};
                                   return (x0, x1, x2)},
                               do {let {x78 = Zero};
                                   let {x77 = Succ x78};
                                   let {x76 = Succ x77};
                                   let {x80 = Zero};
                                   let {x82 = Zero};
                                   let {x85 = Zero};
                                   let {x84 = Succ x85};
                                   let {x89 = Zero};
                                   let {x88 = Succ x89};
                                   let {x87 = Succ x88};
                                   let {x91 = Zero};
                                   let {x94 = Zero};
                                   let {x93 = Succ x94};
                                   let {x97 = Zero};
                                   let {x96 = Succ x97};
                                   let {x99 = Zero};
                                   let {x103 = Zero};
                                   let {x102 = Succ x103};
                                   let {x101 = Succ x102};
                                   let {x108 = Zero};
                                   let {x107 = Succ x108};
                                   let {x106 = Succ x107};
                                   let {x104 = x76};
                                   let {x109 = x106};
                                   x3 <- reversoO;
                                   let {x100 = Cons x101 x3};
                                   let {x98 = Cons x99 x100};
                                   let {x95 = Cons x96 x98};
                                   let {x92 = Cons x93 x95};
                                   let {x90 = Cons x91 x92};
                                   let {x86 = Cons x87 x90};
                                   let {x83 = Cons x84 x86};
                                   let {x81 = Cons x82 x83};
                                   let {x79 = Cons x80 x81};
                                   let {x105 = x79};
                                   let {x2 = Cons x104 x105};
                                   (x0, x4) <- do {x4 <- gen_helpOOO_x4;
                                                   let {x0 = Cons x109 x4};
                                                   return (x0, x4)};
                                   x1 <- appendoOI x4;
                                   return (x0, x1, x2)}]
reversoI x0 = msum [do {guard (x0 == Nil); return ()}]
reversoO = msum [do {let {x0 = Nil}; return x0}]