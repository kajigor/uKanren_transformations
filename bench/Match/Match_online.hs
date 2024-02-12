module Match_online where

import Stream
import Control.Monad
import Term


tp1 :: Term -> [()]
tp1 a = (takeS 1) $ matchoI a
tp2 :: [Term]
tp2 = (takeS 1) $ matchoO

matchoI x0 = msum [do {guard (x0 == Nil); return ()},
                   do {appendoAppendoI x0; return ()}]
appendoAppendoI x0 = msum [do {appendoI x0; return ()},
                           do {_appendoAppendoI x0; return ()}]
_appendoAppendoI x0 = msum [do {_appendoI x0; return ()},
                            do {__appendoAppendoI x0; return ()}]
__appendoAppendoI x0 = msum [do {___appendoI x0; return ()},
                             do {___appendoAppendoI x0; return ()}]
___appendoI x0 = msum [do {let {x23 = O};
                           let {x22 = S x23};
                           let {x21 = S x22};
                           let {x26 = O};
                           let {x25 = S x26};
                           let {x29 = O};
                           let {x28 = S x29};
                           let {x30 = Nil};
                           let {x27 = Cons x28 x30};
                           let {x24 = Cons x25 x27};
                           (x31, x32) <- case x0 of
                                         {Cons y31 y32 -> return (y31, y32); _ -> mzero};
                           guard (x31 == x21);
                           guard (x32 == x24);
                           return ()},
                       do {____appendoI x0; return ()}]
____appendoI x0 = msum [do {let {x34 = O};
                            let {x33 = S x34};
                            let {x37 = O};
                            let {x36 = S x37};
                            let {x38 = Nil};
                            let {x35 = Cons x36 x38};
                            (x39, x40) <- case x0 of
                                          {Cons y39 y40 -> return (y39, y40); _ -> mzero};
                            guard (x39 == x33);
                            guard (x40 == x35);
                            return ()},
                        do {__appendoI x0; return ()}]
___appendoAppendoI x0 = msum [do {_____appendoI x0; return ()},
                              do {____appendoAppendoI x0; return ()}]
_____appendoI x0 = msum [do {let {x43 = O};
                             let {x42 = S x43};
                             let {x41 = S x42};
                             let {x46 = O};
                             let {x45 = S x46};
                             let {x49 = O};
                             let {x48 = S x49};
                             let {x51 = O};
                             let {x52 = Nil};
                             let {x50 = Cons x51 x52};
                             let {x47 = Cons x48 x50};
                             let {x44 = Cons x45 x47};
                             (x53, x54) <- case x0 of
                                           {Cons y53 y54 -> return (y53, y54); _ -> mzero};
                             guard (x53 == x41);
                             guard (x54 == x44);
                             return ()},
                         do {______appendoI x0; return ()}]
______appendoI x0 = msum [do {let {x56 = O};
                              let {x55 = S x56};
                              let {x59 = O};
                              let {x58 = S x59};
                              let {x61 = O};
                              let {x62 = Nil};
                              let {x60 = Cons x61 x62};
                              let {x57 = Cons x58 x60};
                              (x63, x64) <- case x0 of
                                            {Cons y63 y64 -> return (y63, y64); _ -> mzero};
                              guard (x63 == x55);
                              guard (x64 == x57);
                              return ()},
                          do {_______appendoI x0; return ()}]
_______appendoI x0 = msum [do {let {x66 = O};
                               let {x65 = S x66};
                               let {x68 = O};
                               let {x69 = Nil};
                               let {x67 = Cons x68 x69};
                               (x70, x71) <- case x0 of
                                             {Cons y70 y71 -> return (y70, y71); _ -> mzero};
                               guard (x70 == x65);
                               guard (x71 == x67);
                               return ()},
                           do {________appendoI x0; return ()}]
________appendoI x0 = msum [do {let {x72 = O};
                                let {x73 = Nil};
                                (x74, x75) <- case x0 of
                                              {Cons y74 y75 -> return (y74, y75); _ -> mzero};
                                guard (x74 == x72);
                                guard (x75 == x73);
                                return ()},
                            do {guard (x0 == Nil); return ()}]
____appendoAppendoI x0 = msum [do {_________appendoI x0;
                                   return ()},
                               do {_____appendoAppendoI x0; return ()}]
_________appendoI x0 = msum [do {let {x78 = O};
                                 let {x77 = S x78};
                                 let {x76 = S x77};
                                 let {x81 = O};
                                 let {x80 = S x81};
                                 let {x84 = O};
                                 let {x83 = S x84};
                                 let {x86 = O};
                                 let {x88 = O};
                                 let {x89 = Nil};
                                 let {x87 = Cons x88 x89};
                                 let {x85 = Cons x86 x87};
                                 let {x82 = Cons x83 x85};
                                 let {x79 = Cons x80 x82};
                                 (x90, x91) <- case x0 of
                                               {Cons y90 y91 -> return (y90, y91); _ -> mzero};
                                 guard (x90 == x76);
                                 guard (x91 == x79);
                                 return ()},
                             do {__________appendoI x0; return ()}]
__________appendoI x0 = msum [do {let {x93 = O};
                                  let {x92 = S x93};
                                  let {x96 = O};
                                  let {x95 = S x96};
                                  let {x98 = O};
                                  let {x100 = O};
                                  let {x101 = Nil};
                                  let {x99 = Cons x100 x101};
                                  let {x97 = Cons x98 x99};
                                  let {x94 = Cons x95 x97};
                                  (x102, x103) <- case x0 of
                                                  {Cons y102 y103 -> return (y102, y103);
                                                   _ -> mzero};
                                  guard (x102 == x92);
                                  guard (x103 == x94);
                                  return ()},
                              do {___________appendoI x0; return ()}]
___________appendoI x0 = msum [do {let {x105 = O};
                                   let {x104 = S x105};
                                   let {x107 = O};
                                   let {x109 = O};
                                   let {x110 = Nil};
                                   let {x108 = Cons x109 x110};
                                   let {x106 = Cons x107 x108};
                                   (x111, x112) <- case x0 of
                                                   {Cons y111 y112 -> return (y111, y112);
                                                    _ -> mzero};
                                   guard (x111 == x104);
                                   guard (x112 == x106);
                                   return ()},
                               do {____________appendoI x0; return ()}]
____________appendoI x0 = msum [do {let {x113 = O};
                                    let {x115 = O};
                                    let {x116 = Nil};
                                    let {x114 = Cons x115 x116};
                                    (x117, x118) <- case x0 of
                                                    {Cons y117 y118 -> return (y117, y118);
                                                     _ -> mzero};
                                    guard (x117 == x113);
                                    guard (x118 == x114);
                                    return ()},
                                do {________appendoI x0; return ()}]
_____appendoAppendoI x0 = msum [do {_____________appendoI x0;
                                    return ()},
                                do {______appendoAppendoI x0; return ()}]
_____________appendoI x0 = msum [do {let {x121 = O};
                                     let {x120 = S x121};
                                     let {x119 = S x120};
                                     let {x124 = O};
                                     let {x123 = S x124};
                                     let {x127 = O};
                                     let {x126 = S x127};
                                     let {x129 = O};
                                     let {x131 = O};
                                     let {x135 = O};
                                     let {x134 = S x135};
                                     let {x133 = S x134};
                                     let {x136 = Nil};
                                     let {x132 = Cons x133 x136};
                                     let {x130 = Cons x131 x132};
                                     let {x128 = Cons x129 x130};
                                     let {x125 = Cons x126 x128};
                                     let {x122 = Cons x123 x125};
                                     (x137, x138) <- case x0 of
                                                     {Cons y137 y138 -> return (y137, y138);
                                                      _ -> mzero};
                                     guard (x137 == x119);
                                     guard (x138 == x122);
                                     return ()},
                                 do {______________appendoI x0; return ()}]
______________appendoI x0 = msum [do {let {x140 = O};
                                      let {x139 = S x140};
                                      let {x143 = O};
                                      let {x142 = S x143};
                                      let {x145 = O};
                                      let {x147 = O};
                                      let {x151 = O};
                                      let {x150 = S x151};
                                      let {x149 = S x150};
                                      let {x152 = Nil};
                                      let {x148 = Cons x149 x152};
                                      let {x146 = Cons x147 x148};
                                      let {x144 = Cons x145 x146};
                                      let {x141 = Cons x142 x144};
                                      (x153, x154) <- case x0 of
                                                      {Cons y153 y154 -> return (y153, y154);
                                                       _ -> mzero};
                                      guard (x153 == x139);
                                      guard (x154 == x141);
                                      return ()},
                                  do {_______________appendoI x0; return ()}]
_______________appendoI x0 = msum [do {let {x156 = O};
                                       let {x155 = S x156};
                                       let {x158 = O};
                                       let {x160 = O};
                                       let {x164 = O};
                                       let {x163 = S x164};
                                       let {x162 = S x163};
                                       let {x165 = Nil};
                                       let {x161 = Cons x162 x165};
                                       let {x159 = Cons x160 x161};
                                       let {x157 = Cons x158 x159};
                                       (x166, x167) <- case x0 of
                                                       {Cons y166 y167 -> return (y166, y167);
                                                        _ -> mzero};
                                       guard (x166 == x155);
                                       guard (x167 == x157);
                                       return ()},
                                   do {________________appendoI x0; return ()}]
________________appendoI x0 = msum [do {let {x168 = O};
                                        let {x170 = O};
                                        let {x174 = O};
                                        let {x173 = S x174};
                                        let {x172 = S x173};
                                        let {x175 = Nil};
                                        let {x171 = Cons x172 x175};
                                        let {x169 = Cons x170 x171};
                                        (x176, x177) <- case x0 of
                                                        {Cons y176 y177 -> return (y176, y177);
                                                         _ -> mzero};
                                        guard (x176 == x168);
                                        guard (x177 == x169);
                                        return ()},
                                    do {_________________appendoI x0; return ()}]
_________________appendoI x0 = msum [do {let {x178 = O};
                                         let {x182 = O};
                                         let {x181 = S x182};
                                         let {x180 = S x181};
                                         let {x183 = Nil};
                                         let {x179 = Cons x180 x183};
                                         (x184, x185) <- case x0 of
                                                         {Cons y184 y185 -> return (y184, y185);
                                                          _ -> mzero};
                                         guard (x184 == x178);
                                         guard (x185 == x179);
                                         return ()},
                                     do {appendoI x0; return ()}]
______appendoAppendoI x0 = msum [do {__________________appendoI x0;
                                     return ()},
                                 do {________________________appendoI x0; return ()}]
________________________appendoI x0 = msum [do {let {x273 = O};
                                                let {x272 = S x273};
                                                let {x271 = S x272};
                                                let {x276 = O};
                                                let {x275 = S x276};
                                                let {x279 = O};
                                                let {x278 = S x279};
                                                let {x281 = O};
                                                let {x283 = O};
                                                let {x287 = O};
                                                let {x286 = S x287};
                                                let {x285 = S x286};
                                                let {x289 = O};
                                                let {x292 = O};
                                                let {x291 = S x292};
                                                let {x293 = Nil};
                                                let {x290 = Cons x291 x293};
                                                let {x288 = Cons x289 x290};
                                                let {x284 = Cons x285 x288};
                                                let {x282 = Cons x283 x284};
                                                let {x280 = Cons x281 x282};
                                                let {x277 = Cons x278 x280};
                                                let {x274 = Cons x275 x277};
                                                (x294, x295) <- case x0 of
                                                                {Cons y294 y295 -> return (y294,
                                                                                           y295);
                                                                 _ -> mzero};
                                                guard (x294 == x271);
                                                guard (x295 == x274);
                                                return ()},
                                            do {_________________________appendoI x0; return ()}]
_________________________appendoI x0 = msum [do {let {x297 = O};
                                                 let {x296 = S x297};
                                                 let {x300 = O};
                                                 let {x299 = S x300};
                                                 let {x302 = O};
                                                 let {x304 = O};
                                                 let {x308 = O};
                                                 let {x307 = S x308};
                                                 let {x306 = S x307};
                                                 let {x310 = O};
                                                 let {x313 = O};
                                                 let {x312 = S x313};
                                                 let {x314 = Nil};
                                                 let {x311 = Cons x312 x314};
                                                 let {x309 = Cons x310 x311};
                                                 let {x305 = Cons x306 x309};
                                                 let {x303 = Cons x304 x305};
                                                 let {x301 = Cons x302 x303};
                                                 let {x298 = Cons x299 x301};
                                                 (x315, x316) <- case x0 of
                                                                 {Cons y315 y316 -> return (y315,
                                                                                            y316);
                                                                  _ -> mzero};
                                                 guard (x315 == x296);
                                                 guard (x316 == x298);
                                                 return ()},
                                             do {__________________________appendoI x0; return ()}]
__________________________appendoI x0 = msum [do {let {x318 = O};
                                                  let {x317 = S x318};
                                                  let {x320 = O};
                                                  let {x322 = O};
                                                  let {x326 = O};
                                                  let {x325 = S x326};
                                                  let {x324 = S x325};
                                                  let {x328 = O};
                                                  let {x331 = O};
                                                  let {x330 = S x331};
                                                  let {x332 = Nil};
                                                  let {x329 = Cons x330 x332};
                                                  let {x327 = Cons x328 x329};
                                                  let {x323 = Cons x324 x327};
                                                  let {x321 = Cons x322 x323};
                                                  let {x319 = Cons x320 x321};
                                                  (x333, x334) <- case x0 of
                                                                  {Cons y333 y334 -> return (y333,
                                                                                             y334);
                                                                   _ -> mzero};
                                                  guard (x333 == x317);
                                                  guard (x334 == x319);
                                                  return ()},
                                              do {___________________________appendoI x0;
                                                  return ()}]
___________________________appendoI x0 = msum [do {let {x335 = O};
                                                   let {x337 = O};
                                                   let {x341 = O};
                                                   let {x340 = S x341};
                                                   let {x339 = S x340};
                                                   let {x343 = O};
                                                   let {x346 = O};
                                                   let {x345 = S x346};
                                                   let {x347 = Nil};
                                                   let {x344 = Cons x345 x347};
                                                   let {x342 = Cons x343 x344};
                                                   let {x338 = Cons x339 x342};
                                                   let {x336 = Cons x337 x338};
                                                   (x348, x349) <- case x0 of
                                                                   {Cons y348 y349 -> return (y348,
                                                                                              y349);
                                                                    _ -> mzero};
                                                   guard (x348 == x335);
                                                   guard (x349 == x336);
                                                   return ()},
                                               do {____________________________appendoI x0;
                                                   return ()}]
____________________________appendoI x0 = msum [do {let {x350 = O};
                                                    let {x354 = O};
                                                    let {x353 = S x354};
                                                    let {x352 = S x353};
                                                    let {x356 = O};
                                                    let {x359 = O};
                                                    let {x358 = S x359};
                                                    let {x360 = Nil};
                                                    let {x357 = Cons x358 x360};
                                                    let {x355 = Cons x356 x357};
                                                    let {x351 = Cons x352 x355};
                                                    (x361, x362) <- case x0 of
                                                                    {Cons y361 y362 -> return (y361,
                                                                                               y362);
                                                                     _ -> mzero};
                                                    guard (x361 == x350);
                                                    guard (x362 == x351);
                                                    return ()},
                                                do {_____________________________appendoI x0;
                                                    return ()}]
_____________________________appendoI x0 = msum [do {let {x365 = O};
                                                     let {x364 = S x365};
                                                     let {x363 = S x364};
                                                     let {x367 = O};
                                                     let {x370 = O};
                                                     let {x369 = S x370};
                                                     let {x371 = Nil};
                                                     let {x368 = Cons x369 x371};
                                                     let {x366 = Cons x367 x368};
                                                     (x372, x373) <- case x0 of
                                                                     {Cons y372
                                                                           y373 -> return (y372,
                                                                                           y373);
                                                                      _ -> mzero};
                                                     guard (x372 == x363);
                                                     guard (x373 == x366);
                                                     return ()},
                                                 do {______________________________appendoI x0;
                                                     return ()}]
______________________________appendoI x0 = msum [do {let {x374 = O};
                                                      let {x377 = O};
                                                      let {x376 = S x377};
                                                      let {x378 = Nil};
                                                      let {x375 = Cons x376 x378};
                                                      (x379, x380) <- case x0 of
                                                                      {Cons y379
                                                                            y380 -> return (y379,
                                                                                            y380);
                                                                       _ -> mzero};
                                                      guard (x379 == x374);
                                                      guard (x380 == x375);
                                                      return ()},
                                                  do {__appendoI x0; return ()}]
__________________appendoI x0 = msum [do {let {x188 = O};
                                          let {x187 = S x188};
                                          let {x186 = S x187};
                                          let {x191 = O};
                                          let {x190 = S x191};
                                          let {x194 = O};
                                          let {x193 = S x194};
                                          let {x196 = O};
                                          let {x198 = O};
                                          let {x202 = O};
                                          let {x201 = S x202};
                                          let {x200 = S x201};
                                          let {x204 = O};
                                          let {x205 = Nil};
                                          let {x203 = Cons x204 x205};
                                          let {x199 = Cons x200 x203};
                                          let {x197 = Cons x198 x199};
                                          let {x195 = Cons x196 x197};
                                          let {x192 = Cons x193 x195};
                                          let {x189 = Cons x190 x192};
                                          (x206, x207) <- case x0 of
                                                          {Cons y206 y207 -> return (y206, y207);
                                                           _ -> mzero};
                                          guard (x206 == x186);
                                          guard (x207 == x189);
                                          return ()},
                                      do {___________________appendoI x0; return ()}]
___________________appendoI x0 = msum [do {let {x209 = O};
                                           let {x208 = S x209};
                                           let {x212 = O};
                                           let {x211 = S x212};
                                           let {x214 = O};
                                           let {x216 = O};
                                           let {x220 = O};
                                           let {x219 = S x220};
                                           let {x218 = S x219};
                                           let {x222 = O};
                                           let {x223 = Nil};
                                           let {x221 = Cons x222 x223};
                                           let {x217 = Cons x218 x221};
                                           let {x215 = Cons x216 x217};
                                           let {x213 = Cons x214 x215};
                                           let {x210 = Cons x211 x213};
                                           (x224, x225) <- case x0 of
                                                           {Cons y224 y225 -> return (y224, y225);
                                                            _ -> mzero};
                                           guard (x224 == x208);
                                           guard (x225 == x210);
                                           return ()},
                                       do {____________________appendoI x0; return ()}]
____________________appendoI x0 = msum [do {let {x227 = O};
                                            let {x226 = S x227};
                                            let {x229 = O};
                                            let {x231 = O};
                                            let {x235 = O};
                                            let {x234 = S x235};
                                            let {x233 = S x234};
                                            let {x237 = O};
                                            let {x238 = Nil};
                                            let {x236 = Cons x237 x238};
                                            let {x232 = Cons x233 x236};
                                            let {x230 = Cons x231 x232};
                                            let {x228 = Cons x229 x230};
                                            (x239, x240) <- case x0 of
                                                            {Cons y239 y240 -> return (y239, y240);
                                                             _ -> mzero};
                                            guard (x239 == x226);
                                            guard (x240 == x228);
                                            return ()},
                                        do {_____________________appendoI x0; return ()}]
_____________________appendoI x0 = msum [do {let {x241 = O};
                                             let {x243 = O};
                                             let {x247 = O};
                                             let {x246 = S x247};
                                             let {x245 = S x246};
                                             let {x249 = O};
                                             let {x250 = Nil};
                                             let {x248 = Cons x249 x250};
                                             let {x244 = Cons x245 x248};
                                             let {x242 = Cons x243 x244};
                                             (x251, x252) <- case x0 of
                                                             {Cons y251 y252 -> return (y251, y252);
                                                              _ -> mzero};
                                             guard (x251 == x241);
                                             guard (x252 == x242);
                                             return ()},
                                         do {______________________appendoI x0; return ()}]
______________________appendoI x0 = msum [do {let {x253 = O};
                                              let {x257 = O};
                                              let {x256 = S x257};
                                              let {x255 = S x256};
                                              let {x259 = O};
                                              let {x260 = Nil};
                                              let {x258 = Cons x259 x260};
                                              let {x254 = Cons x255 x258};
                                              (x261, x262) <- case x0 of
                                                              {Cons y261 y262 -> return (y261,
                                                                                         y262);
                                                               _ -> mzero};
                                              guard (x261 == x253);
                                              guard (x262 == x254);
                                              return ()},
                                          do {_______________________appendoI x0; return ()}]
_______________________appendoI x0 = msum [do {let {x265 = O};
                                               let {x264 = S x265};
                                               let {x263 = S x264};
                                               let {x267 = O};
                                               let {x268 = Nil};
                                               let {x266 = Cons x267 x268};
                                               (x269, x270) <- case x0 of
                                                               {Cons y269 y270 -> return (y269,
                                                                                          y270);
                                                                _ -> mzero};
                                               guard (x269 == x263);
                                               guard (x270 == x266);
                                               return ()},
                                           do {________appendoI x0; return ()}]
__appendoI x0 = msum [do {let {x17 = O};
                          let {x16 = S x17};
                          let {x18 = Nil};
                          (x19, x20) <- case x0 of
                                        {Cons y19 y20 -> return (y19, y20); _ -> mzero};
                          guard (x19 == x16);
                          guard (x20 == x18);
                          return ()},
                      do {guard (x0 == Nil); return ()}]
_appendoI x0 = msum [do {let {x9 = O};
                         let {x8 = S x9};
                         let {x7 = S x8};
                         let {x12 = O};
                         let {x11 = S x12};
                         let {x13 = Nil};
                         let {x10 = Cons x11 x13};
                         (x14, x15) <- case x0 of
                                       {Cons y14 y15 -> return (y14, y15); _ -> mzero};
                         guard (x14 == x7);
                         guard (x15 == x10);
                         return ()},
                     do {__appendoI x0; return ()}]
appendoI x0 = msum [do {let {x3 = O};
                        let {x2 = S x3};
                        let {x1 = S x2};
                        let {x4 = Nil};
                        (x5, x6) <- case x0 of
                                    {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                        guard (x5 == x1);
                        guard (x6 == x4);
                        return ()},
                    do {guard (x0 == Nil); return ()}]
matchoO = msum [do {let {x0 = Nil}; return x0},
                do {x0 <- appendoAppendoO; return x0}]
appendoAppendoO = msum [do {x0 <- appendoO; return x0},
                        do {x0 <- _appendoAppendoO; return x0}]
_appendoAppendoO = msum [do {x0 <- _appendoO; return x0},
                         do {x0 <- __appendoAppendoO; return x0}]
__appendoAppendoO = msum [do {x0 <- ___appendoO; return x0},
                          do {x0 <- ___appendoAppendoO; return x0}]
___appendoO = msum [do {let {x23 = O};
                        let {x22 = S x23};
                        let {x21 = S x22};
                        let {x26 = O};
                        let {x25 = S x26};
                        let {x29 = O};
                        let {x28 = S x29};
                        let {x30 = Nil};
                        let {x27 = Cons x28 x30};
                        let {x24 = Cons x25 x27};
                        let {x31 = x21};
                        let {x32 = x24};
                        let {x0 = Cons x31 x32};
                        return x0},
                    do {x0 <- ____appendoO; return x0}]
____appendoO = msum [do {let {x34 = O};
                         let {x33 = S x34};
                         let {x37 = O};
                         let {x36 = S x37};
                         let {x38 = Nil};
                         let {x35 = Cons x36 x38};
                         let {x39 = x33};
                         let {x40 = x35};
                         let {x0 = Cons x39 x40};
                         return x0},
                     do {x0 <- __appendoO; return x0}]
___appendoAppendoO = msum [do {x0 <- _____appendoO; return x0},
                           do {x0 <- ____appendoAppendoO; return x0}]
_____appendoO = msum [do {let {x43 = O};
                          let {x42 = S x43};
                          let {x41 = S x42};
                          let {x46 = O};
                          let {x45 = S x46};
                          let {x49 = O};
                          let {x48 = S x49};
                          let {x51 = O};
                          let {x52 = Nil};
                          let {x50 = Cons x51 x52};
                          let {x47 = Cons x48 x50};
                          let {x44 = Cons x45 x47};
                          let {x53 = x41};
                          let {x54 = x44};
                          let {x0 = Cons x53 x54};
                          return x0},
                      do {x0 <- ______appendoO; return x0}]
______appendoO = msum [do {let {x56 = O};
                           let {x55 = S x56};
                           let {x59 = O};
                           let {x58 = S x59};
                           let {x61 = O};
                           let {x62 = Nil};
                           let {x60 = Cons x61 x62};
                           let {x57 = Cons x58 x60};
                           let {x63 = x55};
                           let {x64 = x57};
                           let {x0 = Cons x63 x64};
                           return x0},
                       do {x0 <- _______appendoO; return x0}]
_______appendoO = msum [do {let {x66 = O};
                            let {x65 = S x66};
                            let {x68 = O};
                            let {x69 = Nil};
                            let {x67 = Cons x68 x69};
                            let {x70 = x65};
                            let {x71 = x67};
                            let {x0 = Cons x70 x71};
                            return x0},
                        do {x0 <- ________appendoO; return x0}]
________appendoO = msum [do {let {x72 = O};
                             let {x73 = Nil};
                             let {x74 = x72};
                             let {x75 = x73};
                             let {x0 = Cons x74 x75};
                             return x0},
                         do {let {x0 = Nil}; return x0}]
____appendoAppendoO = msum [do {x0 <- _________appendoO;
                                return x0},
                            do {x0 <- _____appendoAppendoO; return x0}]
_________appendoO = msum [do {let {x78 = O};
                              let {x77 = S x78};
                              let {x76 = S x77};
                              let {x81 = O};
                              let {x80 = S x81};
                              let {x84 = O};
                              let {x83 = S x84};
                              let {x86 = O};
                              let {x88 = O};
                              let {x89 = Nil};
                              let {x87 = Cons x88 x89};
                              let {x85 = Cons x86 x87};
                              let {x82 = Cons x83 x85};
                              let {x79 = Cons x80 x82};
                              let {x90 = x76};
                              let {x91 = x79};
                              let {x0 = Cons x90 x91};
                              return x0},
                          do {x0 <- __________appendoO; return x0}]
__________appendoO = msum [do {let {x93 = O};
                               let {x92 = S x93};
                               let {x96 = O};
                               let {x95 = S x96};
                               let {x98 = O};
                               let {x100 = O};
                               let {x101 = Nil};
                               let {x99 = Cons x100 x101};
                               let {x97 = Cons x98 x99};
                               let {x94 = Cons x95 x97};
                               let {x102 = x92};
                               let {x103 = x94};
                               let {x0 = Cons x102 x103};
                               return x0},
                           do {x0 <- ___________appendoO; return x0}]
___________appendoO = msum [do {let {x105 = O};
                                let {x104 = S x105};
                                let {x107 = O};
                                let {x109 = O};
                                let {x110 = Nil};
                                let {x108 = Cons x109 x110};
                                let {x106 = Cons x107 x108};
                                let {x111 = x104};
                                let {x112 = x106};
                                let {x0 = Cons x111 x112};
                                return x0},
                            do {x0 <- ____________appendoO; return x0}]
____________appendoO = msum [do {let {x113 = O};
                                 let {x115 = O};
                                 let {x116 = Nil};
                                 let {x114 = Cons x115 x116};
                                 let {x117 = x113};
                                 let {x118 = x114};
                                 let {x0 = Cons x117 x118};
                                 return x0},
                             do {x0 <- ________appendoO; return x0}]
_____appendoAppendoO = msum [do {x0 <- _____________appendoO;
                                 return x0},
                             do {x0 <- ______appendoAppendoO; return x0}]
_____________appendoO = msum [do {let {x121 = O};
                                  let {x120 = S x121};
                                  let {x119 = S x120};
                                  let {x124 = O};
                                  let {x123 = S x124};
                                  let {x127 = O};
                                  let {x126 = S x127};
                                  let {x129 = O};
                                  let {x131 = O};
                                  let {x135 = O};
                                  let {x134 = S x135};
                                  let {x133 = S x134};
                                  let {x136 = Nil};
                                  let {x132 = Cons x133 x136};
                                  let {x130 = Cons x131 x132};
                                  let {x128 = Cons x129 x130};
                                  let {x125 = Cons x126 x128};
                                  let {x122 = Cons x123 x125};
                                  let {x137 = x119};
                                  let {x138 = x122};
                                  let {x0 = Cons x137 x138};
                                  return x0},
                              do {x0 <- ______________appendoO; return x0}]
______________appendoO = msum [do {let {x140 = O};
                                   let {x139 = S x140};
                                   let {x143 = O};
                                   let {x142 = S x143};
                                   let {x145 = O};
                                   let {x147 = O};
                                   let {x151 = O};
                                   let {x150 = S x151};
                                   let {x149 = S x150};
                                   let {x152 = Nil};
                                   let {x148 = Cons x149 x152};
                                   let {x146 = Cons x147 x148};
                                   let {x144 = Cons x145 x146};
                                   let {x141 = Cons x142 x144};
                                   let {x153 = x139};
                                   let {x154 = x141};
                                   let {x0 = Cons x153 x154};
                                   return x0},
                               do {x0 <- _______________appendoO; return x0}]
_______________appendoO = msum [do {let {x156 = O};
                                    let {x155 = S x156};
                                    let {x158 = O};
                                    let {x160 = O};
                                    let {x164 = O};
                                    let {x163 = S x164};
                                    let {x162 = S x163};
                                    let {x165 = Nil};
                                    let {x161 = Cons x162 x165};
                                    let {x159 = Cons x160 x161};
                                    let {x157 = Cons x158 x159};
                                    let {x166 = x155};
                                    let {x167 = x157};
                                    let {x0 = Cons x166 x167};
                                    return x0},
                                do {x0 <- ________________appendoO; return x0}]
________________appendoO = msum [do {let {x168 = O};
                                     let {x170 = O};
                                     let {x174 = O};
                                     let {x173 = S x174};
                                     let {x172 = S x173};
                                     let {x175 = Nil};
                                     let {x171 = Cons x172 x175};
                                     let {x169 = Cons x170 x171};
                                     let {x176 = x168};
                                     let {x177 = x169};
                                     let {x0 = Cons x176 x177};
                                     return x0},
                                 do {x0 <- _________________appendoO; return x0}]
_________________appendoO = msum [do {let {x178 = O};
                                      let {x182 = O};
                                      let {x181 = S x182};
                                      let {x180 = S x181};
                                      let {x183 = Nil};
                                      let {x179 = Cons x180 x183};
                                      let {x184 = x178};
                                      let {x185 = x179};
                                      let {x0 = Cons x184 x185};
                                      return x0},
                                  do {x0 <- appendoO; return x0}]
______appendoAppendoO = msum [do {x0 <- __________________appendoO;
                                  return x0},
                              do {x0 <- ________________________appendoO; return x0}]
________________________appendoO = msum [do {let {x273 = O};
                                             let {x272 = S x273};
                                             let {x271 = S x272};
                                             let {x276 = O};
                                             let {x275 = S x276};
                                             let {x279 = O};
                                             let {x278 = S x279};
                                             let {x281 = O};
                                             let {x283 = O};
                                             let {x287 = O};
                                             let {x286 = S x287};
                                             let {x285 = S x286};
                                             let {x289 = O};
                                             let {x292 = O};
                                             let {x291 = S x292};
                                             let {x293 = Nil};
                                             let {x290 = Cons x291 x293};
                                             let {x288 = Cons x289 x290};
                                             let {x284 = Cons x285 x288};
                                             let {x282 = Cons x283 x284};
                                             let {x280 = Cons x281 x282};
                                             let {x277 = Cons x278 x280};
                                             let {x274 = Cons x275 x277};
                                             let {x294 = x271};
                                             let {x295 = x274};
                                             let {x0 = Cons x294 x295};
                                             return x0},
                                         do {x0 <- _________________________appendoO; return x0}]
_________________________appendoO = msum [do {let {x297 = O};
                                              let {x296 = S x297};
                                              let {x300 = O};
                                              let {x299 = S x300};
                                              let {x302 = O};
                                              let {x304 = O};
                                              let {x308 = O};
                                              let {x307 = S x308};
                                              let {x306 = S x307};
                                              let {x310 = O};
                                              let {x313 = O};
                                              let {x312 = S x313};
                                              let {x314 = Nil};
                                              let {x311 = Cons x312 x314};
                                              let {x309 = Cons x310 x311};
                                              let {x305 = Cons x306 x309};
                                              let {x303 = Cons x304 x305};
                                              let {x301 = Cons x302 x303};
                                              let {x298 = Cons x299 x301};
                                              let {x315 = x296};
                                              let {x316 = x298};
                                              let {x0 = Cons x315 x316};
                                              return x0},
                                          do {x0 <- __________________________appendoO; return x0}]
__________________________appendoO = msum [do {let {x318 = O};
                                               let {x317 = S x318};
                                               let {x320 = O};
                                               let {x322 = O};
                                               let {x326 = O};
                                               let {x325 = S x326};
                                               let {x324 = S x325};
                                               let {x328 = O};
                                               let {x331 = O};
                                               let {x330 = S x331};
                                               let {x332 = Nil};
                                               let {x329 = Cons x330 x332};
                                               let {x327 = Cons x328 x329};
                                               let {x323 = Cons x324 x327};
                                               let {x321 = Cons x322 x323};
                                               let {x319 = Cons x320 x321};
                                               let {x333 = x317};
                                               let {x334 = x319};
                                               let {x0 = Cons x333 x334};
                                               return x0},
                                           do {x0 <- ___________________________appendoO;
                                               return x0}]
___________________________appendoO = msum [do {let {x335 = O};
                                                let {x337 = O};
                                                let {x341 = O};
                                                let {x340 = S x341};
                                                let {x339 = S x340};
                                                let {x343 = O};
                                                let {x346 = O};
                                                let {x345 = S x346};
                                                let {x347 = Nil};
                                                let {x344 = Cons x345 x347};
                                                let {x342 = Cons x343 x344};
                                                let {x338 = Cons x339 x342};
                                                let {x336 = Cons x337 x338};
                                                let {x348 = x335};
                                                let {x349 = x336};
                                                let {x0 = Cons x348 x349};
                                                return x0},
                                            do {x0 <- ____________________________appendoO;
                                                return x0}]
____________________________appendoO = msum [do {let {x350 = O};
                                                 let {x354 = O};
                                                 let {x353 = S x354};
                                                 let {x352 = S x353};
                                                 let {x356 = O};
                                                 let {x359 = O};
                                                 let {x358 = S x359};
                                                 let {x360 = Nil};
                                                 let {x357 = Cons x358 x360};
                                                 let {x355 = Cons x356 x357};
                                                 let {x351 = Cons x352 x355};
                                                 let {x361 = x350};
                                                 let {x362 = x351};
                                                 let {x0 = Cons x361 x362};
                                                 return x0},
                                             do {x0 <- _____________________________appendoO;
                                                 return x0}]
_____________________________appendoO = msum [do {let {x365 = O};
                                                  let {x364 = S x365};
                                                  let {x363 = S x364};
                                                  let {x367 = O};
                                                  let {x370 = O};
                                                  let {x369 = S x370};
                                                  let {x371 = Nil};
                                                  let {x368 = Cons x369 x371};
                                                  let {x366 = Cons x367 x368};
                                                  let {x372 = x363};
                                                  let {x373 = x366};
                                                  let {x0 = Cons x372 x373};
                                                  return x0},
                                              do {x0 <- ______________________________appendoO;
                                                  return x0}]
______________________________appendoO = msum [do {let {x374 = O};
                                                   let {x377 = O};
                                                   let {x376 = S x377};
                                                   let {x378 = Nil};
                                                   let {x375 = Cons x376 x378};
                                                   let {x379 = x374};
                                                   let {x380 = x375};
                                                   let {x0 = Cons x379 x380};
                                                   return x0},
                                               do {x0 <- __appendoO; return x0}]
__________________appendoO = msum [do {let {x188 = O};
                                       let {x187 = S x188};
                                       let {x186 = S x187};
                                       let {x191 = O};
                                       let {x190 = S x191};
                                       let {x194 = O};
                                       let {x193 = S x194};
                                       let {x196 = O};
                                       let {x198 = O};
                                       let {x202 = O};
                                       let {x201 = S x202};
                                       let {x200 = S x201};
                                       let {x204 = O};
                                       let {x205 = Nil};
                                       let {x203 = Cons x204 x205};
                                       let {x199 = Cons x200 x203};
                                       let {x197 = Cons x198 x199};
                                       let {x195 = Cons x196 x197};
                                       let {x192 = Cons x193 x195};
                                       let {x189 = Cons x190 x192};
                                       let {x206 = x186};
                                       let {x207 = x189};
                                       let {x0 = Cons x206 x207};
                                       return x0},
                                   do {x0 <- ___________________appendoO; return x0}]
___________________appendoO = msum [do {let {x209 = O};
                                        let {x208 = S x209};
                                        let {x212 = O};
                                        let {x211 = S x212};
                                        let {x214 = O};
                                        let {x216 = O};
                                        let {x220 = O};
                                        let {x219 = S x220};
                                        let {x218 = S x219};
                                        let {x222 = O};
                                        let {x223 = Nil};
                                        let {x221 = Cons x222 x223};
                                        let {x217 = Cons x218 x221};
                                        let {x215 = Cons x216 x217};
                                        let {x213 = Cons x214 x215};
                                        let {x210 = Cons x211 x213};
                                        let {x224 = x208};
                                        let {x225 = x210};
                                        let {x0 = Cons x224 x225};
                                        return x0},
                                    do {x0 <- ____________________appendoO; return x0}]
____________________appendoO = msum [do {let {x227 = O};
                                         let {x226 = S x227};
                                         let {x229 = O};
                                         let {x231 = O};
                                         let {x235 = O};
                                         let {x234 = S x235};
                                         let {x233 = S x234};
                                         let {x237 = O};
                                         let {x238 = Nil};
                                         let {x236 = Cons x237 x238};
                                         let {x232 = Cons x233 x236};
                                         let {x230 = Cons x231 x232};
                                         let {x228 = Cons x229 x230};
                                         let {x239 = x226};
                                         let {x240 = x228};
                                         let {x0 = Cons x239 x240};
                                         return x0},
                                     do {x0 <- _____________________appendoO; return x0}]
_____________________appendoO = msum [do {let {x241 = O};
                                          let {x243 = O};
                                          let {x247 = O};
                                          let {x246 = S x247};
                                          let {x245 = S x246};
                                          let {x249 = O};
                                          let {x250 = Nil};
                                          let {x248 = Cons x249 x250};
                                          let {x244 = Cons x245 x248};
                                          let {x242 = Cons x243 x244};
                                          let {x251 = x241};
                                          let {x252 = x242};
                                          let {x0 = Cons x251 x252};
                                          return x0},
                                      do {x0 <- ______________________appendoO; return x0}]
______________________appendoO = msum [do {let {x253 = O};
                                           let {x257 = O};
                                           let {x256 = S x257};
                                           let {x255 = S x256};
                                           let {x259 = O};
                                           let {x260 = Nil};
                                           let {x258 = Cons x259 x260};
                                           let {x254 = Cons x255 x258};
                                           let {x261 = x253};
                                           let {x262 = x254};
                                           let {x0 = Cons x261 x262};
                                           return x0},
                                       do {x0 <- _______________________appendoO; return x0}]
_______________________appendoO = msum [do {let {x265 = O};
                                            let {x264 = S x265};
                                            let {x263 = S x264};
                                            let {x267 = O};
                                            let {x268 = Nil};
                                            let {x266 = Cons x267 x268};
                                            let {x269 = x263};
                                            let {x270 = x266};
                                            let {x0 = Cons x269 x270};
                                            return x0},
                                        do {x0 <- ________appendoO; return x0}]
__appendoO = msum [do {let {x17 = O};
                       let {x16 = S x17};
                       let {x18 = Nil};
                       let {x19 = x16};
                       let {x20 = x18};
                       let {x0 = Cons x19 x20};
                       return x0},
                   do {let {x0 = Nil}; return x0}]
_appendoO = msum [do {let {x9 = O};
                      let {x8 = S x9};
                      let {x7 = S x8};
                      let {x12 = O};
                      let {x11 = S x12};
                      let {x13 = Nil};
                      let {x10 = Cons x11 x13};
                      let {x14 = x7};
                      let {x15 = x10};
                      let {x0 = Cons x14 x15};
                      return x0},
                  do {x0 <- __appendoO; return x0}]
appendoO = msum [do {let {x3 = O};
                     let {x2 = S x3};
                     let {x1 = S x2};
                     let {x4 = Nil};
                     let {x5 = x1};
                     let {x6 = x4};
                     let {x0 = Cons x5 x6};
                     return x0},
                 do {let {x0 = Nil}; return x0}]