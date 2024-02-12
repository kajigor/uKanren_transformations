module Hanoi_offline where

import Stream
import Control.Monad
import Term

tp1 :: Term -> [()]
tp1 a = (takeS 1) $ checkI a
tp2 :: [Term]
tp2 = (takeS 1) $ checkO

checkI x0 = Immature $ msum [do {let {x3 = One};
                      let {x4 = Two};
                      let {x2 = Pair x3 x4};
                      (x5, x1) <- case x0 of
                                  {Cons y5 y1 -> return (y5, y1); _ -> mzero};
                      guard (x5 == x2);
                      _checkI x1;
                      return ()},
                  do {let {x7 = One};
                      let {x8 = Thr};
                      let {x6 = Pair x7 x8};
                      (x9, x1) <- case x0 of
                                  {Cons y9 y1 -> return (y9, y1); _ -> mzero};
                      guard (x9 == x6);
                      __________________________checkI x1;
                      return ()}]
__________________________checkI x0 = Immature $ msum [do {let {x303 = One};
                                                let {x304 = Two};
                                                let {x302 = Pair x303 x304};
                                                (x305, x1) <- case x0 of
                                                              {Cons y305 y1 -> return (y305, y1);
                                                               _ -> mzero};
                                                guard (x305 == x302);
                                                _____checkI x1;
                                                return ()},
                                            do {let {x307 = Thr};
                                                let {x308 = Two};
                                                let {x306 = Pair x307 x308};
                                                (x309, x1) <- case x0 of
                                                              {Cons y309 y1 -> return (y309, y1);
                                                               _ -> mzero};
                                                guard (x309 == x306);
                                                _checkI x1;
                                                return ()},
                                            do {let {x311 = Thr};
                                                let {x312 = One};
                                                let {x310 = Pair x311 x312};
                                                (x313, x1) <- case x0 of
                                                              {Cons y313 y1 -> return (y313, y1);
                                                               _ -> mzero};
                                                guard (x313 == x310);
                                                checkI x1;
                                                return ()}]
_____checkI x0 = Immature $ msum [do {let {x59 = Two};
                           let {x60 = One};
                           let {x58 = Pair x59 x60};
                           (x61, x1) <- case x0 of
                                        {Cons y61 y1 -> return (y61, y1); _ -> mzero};
                           guard (x61 == x58);
                           __________________________checkI x1;
                           return ()},
                       do {let {x63 = Thr};
                           let {x64 = One};
                           let {x62 = Pair x63 x64};
                           (x65, x1) <- case x0 of
                                        {Cons y65 y1 -> return (y65, y1); _ -> mzero};
                           guard (x65 == x62);
                           ____checkI x1;
                           return ()},
                       do {let {x67 = Thr};
                           let {x68 = Two};
                           let {x66 = Pair x67 x68};
                           (x69, x1) <- case x0 of
                                        {Cons y69 y1 -> return (y69, y1); _ -> mzero};
                           guard (x69 == x66);
                           ______checkI x1;
                           return ()}]
______checkI x0 = Immature $ msum [do {let {x71 = One};
                            let {x72 = Thr};
                            let {x70 = Pair x71 x72};
                            (x73, x1) <- case x0 of
                                         {Cons y73 y1 -> return (y73, y1); _ -> mzero};
                            guard (x73 == x70);
                            _______checkI x1;
                            return ()},
                        do {let {x75 = Two};
                            let {x76 = Thr};
                            let {x74 = Pair x75 x76};
                            (x77, x1) <- case x0 of
                                         {Cons y77 y1 -> return (y77, y1); _ -> mzero};
                            guard (x77 == x74);
                            _____checkI x1;
                            return ()},
                        do {let {x79 = Two};
                            let {x80 = One};
                            let {x78 = Pair x79 x80};
                            (x81, x1) <- case x0 of
                                         {Cons y81 y1 -> return (y81, y1); _ -> mzero};
                            guard (x81 == x78);
                            ____checkI x1;
                            return ()}]
_______checkI x0 = Immature $ msum [do {let {x83 = Two};
                             let {x84 = One};
                             let {x82 = Pair x83 x84};
                             (x85, x1) <- case x0 of
                                          {Cons y85 y1 -> return (y85, y1); _ -> mzero};
                             guard (x85 == x82);
                             ________checkI x1;
                             return ()},
                         do {let {x87 = Thr};
                             let {x88 = One};
                             let {x86 = Pair x87 x88};
                             (x89, x1) <- case x0 of
                                          {Cons y89 y1 -> return (y89, y1); _ -> mzero};
                             guard (x89 == x86);
                             ______checkI x1;
                             return ()},
                         do {let {x91 = Two};
                             let {x92 = Thr};
                             let {x90 = Pair x91 x92};
                             (x93, x1) <- case x0 of
                                          {Cons y93 y1 -> return (y93, y1); _ -> mzero};
                             guard (x93 == x90);
                             ________________________checkI x1;
                             return ()}]
________________________checkI x0 = Immature $ msum [do {let {x279 = Two};
                                              let {x280 = One};
                                              let {x278 = Pair x279 x280};
                                              (x281, x1) <- case x0 of
                                                            {Cons y281 y1 -> return (y281, y1);
                                                             _ -> mzero};
                                              guard (x281 == x278);
                                              ______________________checkI x1;
                                              return ()},
                                          do {let {x283 = Thr};
                                              let {x284 = One};
                                              let {x282 = Pair x283 x284};
                                              (x285, x1) <- case x0 of
                                                            {Cons y285 y1 -> return (y285, y1);
                                                             _ -> mzero};
                                              guard (x285 == x282);
                                              ________checkI x1;
                                              return ()},
                                          do {let {x287 = Thr};
                                              let {x288 = Two};
                                              let {x286 = Pair x287 x288};
                                              (x289, x1) <- case x0 of
                                                            {Cons y289 y1 -> return (y289, y1);
                                                             _ -> mzero};
                                              guard (x289 == x286);
                                              _______checkI x1;
                                              return ()}]
______________________checkI x0 = Immature $ msum [do {let {x259 = One};
                                            let {x260 = Two};
                                            let {x258 = Pair x259 x260};
                                            (x261, x1) <- case x0 of
                                                          {Cons y261 y1 -> return (y261, y1);
                                                           _ -> mzero};
                                            guard (x261 == x258);
                                            ________________________checkI x1;
                                            return ()},
                                        do {let {x263 = Thr};
                                            let {x264 = Two};
                                            let {x262 = Pair x263 x264};
                                            (x265, x1) <- case x0 of
                                                          {Cons y265 y1 -> return (y265, y1);
                                                           _ -> mzero};
                                            guard (x265 == x262);
                                            ___________checkI x1;
                                            return ()},
                                        do {let {x267 = Thr};
                                            let {x268 = One};
                                            let {x266 = Pair x267 x268};
                                            (x269, x1) <- case x0 of
                                                          {Cons y269 y1 -> return (y269, y1);
                                                           _ -> mzero};
                                            guard (x269 == x266);
                                            ____________checkI x1;
                                            return ()}]
____________checkI x0 = Immature $ msum [do {let {x143 = One};
                                  let {x144 = Two};
                                  let {x142 = Pair x143 x144};
                                  (x145, x1) <- case x0 of
                                                {Cons y145 y1 -> return (y145, y1); _ -> mzero};
                                  guard (x145 == x142);
                                  ___________checkI x1;
                                  return ()},
                              do {let {x147 = Thr};
                                  let {x148 = Two};
                                  let {x146 = Pair x147 x148};
                                  (x149, x1) <- case x0 of
                                                {Cons y149 y1 -> return (y149, y1); _ -> mzero};
                                  guard (x149 == x146);
                                  _____________checkI x1;
                                  return ()},
                              do {let {x151 = One};
                                  let {x152 = Thr};
                                  let {x150 = Pair x151 x152};
                                  (x153, x1) <- case x0 of
                                                {Cons y153 y1 -> return (y153, y1); _ -> mzero};
                                  guard (x153 == x150);
                                  ______________________checkI x1;
                                  return ()}]
_____________checkI x0 = Immature $ msum [do {let {x155 = One};
                                   let {x156 = Thr};
                                   let {x154 = Pair x155 x156};
                                   (x157, x1) <- case x0 of
                                                 {Cons y157 y1 -> return (y157, y1); _ -> mzero};
                                   guard (x157 == x154);
                                   ______________checkI x1;
                                   return ()},
                               do {let {x159 = Two};
                                   let {x160 = Thr};
                                   let {x158 = Pair x159 x160};
                                   (x161, x1) <- case x0 of
                                                 {Cons y161 y1 -> return (y161, y1); _ -> mzero};
                                   guard (x161 == x158);
                                   ____________checkI x1;
                                   return ()},
                               do {let {x163 = One};
                                   let {x164 = Two};
                                   let {x162 = Pair x163 x164};
                                   (x165, x1) <- case x0 of
                                                 {Cons y165 y1 -> return (y165, y1); _ -> mzero};
                                   guard (x165 == x162);
                                   _____________________checkI x1;
                                   return ()}]
_____________________checkI x0 = Immature $ msum [do {let {x247 = One};
                                           let {x248 = Thr};
                                           let {x246 = Pair x247 x248};
                                           (x249, x1) <- case x0 of
                                                         {Cons y249 y1 -> return (y249, y1);
                                                          _ -> mzero};
                                           guard (x249 == x246);
                                           __________________checkI x1;
                                           return ()},
                                       do {let {x251 = Two};
                                           let {x252 = Thr};
                                           let {x250 = Pair x251 x252};
                                           (x253, x1) <- case x0 of
                                                         {Cons y253 y1 -> return (y253, y1);
                                                          _ -> mzero};
                                           guard (x253 == x250);
                                           ______________checkI x1;
                                           return ()},
                                       do {let {x255 = Two};
                                           let {x256 = One};
                                           let {x254 = Pair x255 x256};
                                           (x257, x1) <- case x0 of
                                                         {Cons y257 y1 -> return (y257, y1);
                                                          _ -> mzero};
                                           guard (x257 == x254);
                                           _____________checkI x1;
                                           return ()}]
__________________checkI x0 = Immature $ msum [do {let {x215 = Two};
                                        let {x216 = One};
                                        let {x214 = Pair x215 x216};
                                        (x217, x1) <- case x0 of
                                                      {Cons y217 y1 -> return (y217, y1);
                                                       _ -> mzero};
                                        guard (x217 == x214);
                                        _________________checkI x1;
                                        return ()},
                                    do {let {x219 = Thr};
                                        let {x220 = One};
                                        let {x218 = Pair x219 x220};
                                        (x221, x1) <- case x0 of
                                                      {Cons y221 y1 -> return (y221, y1);
                                                       _ -> mzero};
                                        guard (x221 == x218);
                                        _____________________checkI x1;
                                        return ()},
                                    do {let {x223 = Two};
                                        let {x224 = Thr};
                                        let {x222 = Pair x223 x224};
                                        (x225, x1) <- case x0 of
                                                      {Cons y225 y1 -> return (y225, y1);
                                                       _ -> mzero};
                                        guard (x225 == x222);
                                        ___________________checkI x1;
                                        return ()}]
___________________checkI x0 = Immature $ msum [do {let {x227 = Two};
                                         let {x228 = One};
                                         let {x226 = Pair x227 x228};
                                         (x229, x1) <- case x0 of
                                                       {Cons y229 y1 -> return (y229, y1);
                                                        _ -> mzero};
                                         guard (x229 == x226);
                                         _________________________checkI x1;
                                         return ()},
                                     do {let {x231 = Thr};
                                         let {x232 = One};
                                         let {x230 = Pair x231 x232};
                                         (x233, x1) <- case x0 of
                                                       {Cons y233 y1 -> return (y233, y1);
                                                        _ -> mzero};
                                         guard (x233 == x230);
                                         _________________checkI x1;
                                         return ()},
                                     do {let {x235 = Thr};
                                         let {x236 = Two};
                                         let {x234 = Pair x235 x236};
                                         (x237, x1) <- case x0 of
                                                       {Cons y237 y1 -> return (y237, y1);
                                                        _ -> mzero};
                                         guard (x237 == x234);
                                         __________________checkI x1;
                                         return ()}]
_________________________checkI x0 = Immature $ msum [do {let {x291 = One};
                                               let {x292 = Two};
                                               let {x290 = Pair x291 x292};
                                               (x293, x1) <- case x0 of
                                                             {Cons y293 y1 -> return (y293, y1);
                                                              _ -> mzero};
                                               guard (x293 == x290);
                                               ___________________checkI x1;
                                               return ()},
                                           do {let {x295 = Thr};
                                               let {x296 = Two};
                                               let {x294 = Pair x295 x296};
                                               (x297, x1) <- case x0 of
                                                             {Cons y297 y1 -> return (y297, y1);
                                                              _ -> mzero};
                                               guard (x297 == x294);
                                               __checkI x1;
                                               return ()},
                                           do {let {x299 = Thr};
                                               let {x300 = One};
                                               let {x298 = Pair x299 x300};
                                               (x301, x1) <- case x0 of
                                                             {Cons y301 y1 -> return (y301, y1);
                                                              _ -> mzero};
                                               guard (x301 == x298);
                                               ___checkI x1;
                                               return ()}]
_________________checkI x0 = Immature $ msum [do {let {x203 = One};
                                       let {x204 = Two};
                                       let {x202 = Pair x203 x204};
                                       (x205, x1) <- case x0 of
                                                     {Cons y205 y1 -> return (y205, y1);
                                                      _ -> mzero};
                                       guard (x205 == x202);
                                       __________________checkI x1;
                                       return ()},
                                   do {let {x207 = One};
                                       let {x208 = Thr};
                                       let {x206 = Pair x207 x208};
                                       (x209, x1) <- case x0 of
                                                     {Cons y209 y1 -> return (y209, y1);
                                                      _ -> mzero};
                                       guard (x209 == x206);
                                       ___________________checkI x1;
                                       return ()},
                                   do {let {x211 = Thr};
                                       let {x212 = Two};
                                       let {x210 = Pair x211 x212};
                                       (x213, x1) <- case x0 of
                                                     {Cons y213 y1 -> return (y213, y1);
                                                      _ -> mzero};
                                       guard (x213 == x210);
                                       ________________checkI x1;
                                       return ()}]
________________checkI x0 = Immature $ msum [do {let {x191 = One};
                                      let {x192 = Thr};
                                      let {x190 = Pair x191 x192};
                                      (x193, x1) <- case x0 of
                                                    {Cons y193 y1 -> return (y193, y1); _ -> mzero};
                                      guard (x193 == x190);
                                      _______________checkI x1;
                                      return ()},
                                  do {let {x195 = Two};
                                      let {x196 = Thr};
                                      let {x194 = Pair x195 x196};
                                      (x197, x1) <- case x0 of
                                                    {Cons y197 y1 -> return (y197, y1); _ -> mzero};
                                      guard (x197 == x194);
                                      _________________checkI x1;
                                      return ()},
                                  do {let {x199 = One};
                                      let {x200 = Two};
                                      let {x198 = Pair x199 x200};
                                      (x201, x1) <- case x0 of
                                                    {Cons y201 y1 -> return (y201, y1); _ -> mzero};
                                      guard (x201 == x198);
                                      ____________________checkI x1;
                                      return ()}]
____________________checkI x0 = Immature $ msum [do {let {x239 = Two};
                                          let {x240 = One};
                                          let {x238 = Pair x239 x240};
                                          (x241, x1) <- case x0 of
                                                        {Cons y241 y1 -> return (y241, y1);
                                                         _ -> mzero};
                                          guard (x241 == x238);
                                          ________________checkI x1;
                                          return ()},
                                      do {let {x243 = Two};
                                          let {x244 = Thr};
                                          let {x242 = Pair x243 x244};
                                          (x245, x1) <- case x0 of
                                                        {Cons y245 y1 -> return (y245, y1);
                                                         _ -> mzero};
                                          guard (x245 == x242);
                                          _______________checkI x1;
                                          return ()}]
_______________checkI x0 = Immature $ msum [do {let {x179 = Two};
                                     let {x180 = One};
                                     let {x178 = Pair x179 x180};
                                     (x181, x1) <- case x0 of
                                                   {Cons y181 y1 -> return (y181, y1); _ -> mzero};
                                     guard (x181 == x178);
                                     ______________checkI x1;
                                     return ()},
                                 do {let {x183 = Thr};
                                     let {x184 = One};
                                     let {x182 = Pair x183 x184};
                                     (x185, x1) <- case x0 of
                                                   {Cons y185 y1 -> return (y185, y1); _ -> mzero};
                                     guard (x185 == x182);
                                     ________________checkI x1;
                                     return ()},
                                 do {let {x187 = Thr};
                                     let {x188 = Two};
                                     let {x186 = Pair x187 x188};
                                     (x189, x1) <- case x0 of
                                                   {Cons y189 y1 -> return (y189, y1); _ -> mzero};
                                     guard (x189 == x186);
                                     ____________________checkI x1;
                                     return ()}]
______________checkI x0 = Immature $ msum [do {let {x167 = One};
                                    let {x168 = Two};
                                    let {x166 = Pair x167 x168};
                                    (x169, x1) <- case x0 of
                                                  {Cons y169 y1 -> return (y169, y1); _ -> mzero};
                                    guard (x169 == x166);
                                    _______________checkI x1;
                                    return ()},
                                do {let {x171 = Thr};
                                    let {x172 = One};
                                    let {x170 = Pair x171 x172};
                                    (x173, x1) <- case x0 of
                                                  {Cons y173 y1 -> return (y173, y1); _ -> mzero};
                                    guard (x173 == x170);
                                    _____________checkI x1;
                                    return ()},
                                do {let {x175 = Thr};
                                    let {x176 = Two};
                                    let {x174 = Pair x175 x176};
                                    (x177, x1) <- case x0 of
                                                  {Cons y177 y1 -> return (y177, y1); _ -> mzero};
                                    guard (x177 == x174);
                                    _____________________checkI x1;
                                    return ()}]
___________checkI x0 = Immature $ msum [do {let {x131 = One};
                                 let {x132 = Thr};
                                 let {x130 = Pair x131 x132};
                                 (x133, x1) <- case x0 of
                                               {Cons y133 y1 -> return (y133, y1); _ -> mzero};
                                 guard (x133 == x130);
                                 __________checkI x1;
                                 return ()},
                             do {let {x135 = Two};
                                 let {x136 = One};
                                 let {x134 = Pair x135 x136};
                                 (x137, x1) <- case x0 of
                                               {Cons y137 y1 -> return (y137, y1); _ -> mzero};
                                 guard (x137 == x134);
                                 ____________checkI x1;
                                 return ()},
                             do {let {x139 = Two};
                                 let {x140 = Thr};
                                 let {x138 = Pair x139 x140};
                                 (x141, x1) <- case x0 of
                                               {Cons y141 y1 -> return (y141, y1); _ -> mzero};
                                 guard (x141 == x138);
                                 ______________________checkI x1;
                                 return ()}]
__________checkI x0 = Immature $ msum [do {let {x119 = Two};
                                let {x120 = One};
                                let {x118 = Pair x119 x120};
                                (x121, x1) <- case x0 of
                                              {Cons y121 y1 -> return (y121, y1); _ -> mzero};
                                guard (x121 == x118);
                                _________checkI x1;
                                return ()},
                            do {let {x123 = Thr};
                                let {x124 = One};
                                let {x122 = Pair x123 x124};
                                (x125, x1) <- case x0 of
                                              {Cons y125 y1 -> return (y125, y1); _ -> mzero};
                                guard (x125 == x122);
                                ___________checkI x1;
                                return ()},
                            do {let {x127 = Two};
                                let {x128 = Thr};
                                let {x126 = Pair x127 x128};
                                (x129, x1) <- case x0 of
                                              {Cons y129 y1 -> return (y129, y1); _ -> mzero};
                                guard (x129 == x126);
                                _______________________checkI x1;
                                return ()}]
_______________________checkI x0 = Immature $ msum [do {guard (x0 == Nil);
                                             return ()},
                                         do {let {x271 = Thr};
                                             let {x272 = One};
                                             let {x270 = Pair x271 x272};
                                             (x273, x1) <- case x0 of
                                                           {Cons y273 y1 -> return (y273, y1);
                                                            _ -> mzero};
                                             guard (x273 == x270);
                                             _________checkI x1;
                                             return ()},
                                         do {let {x275 = Thr};
                                             let {x276 = Two};
                                             let {x274 = Pair x275 x276};
                                             (x277, x1) <- case x0 of
                                                           {Cons y277 y1 -> return (y277, y1);
                                                            _ -> mzero};
                                             guard (x277 == x274);
                                             __________checkI x1;
                                             return ()}]
_________checkI x0 = Immature $ msum [do {let {x107 = One};
                               let {x108 = Two};
                               let {x106 = Pair x107 x108};
                               (x109, x1) <- case x0 of
                                             {Cons y109 y1 -> return (y109, y1); _ -> mzero};
                               guard (x109 == x106);
                               __________checkI x1;
                               return ()},
                           do {let {x111 = Thr};
                               let {x112 = Two};
                               let {x110 = Pair x111 x112};
                               (x113, x1) <- case x0 of
                                             {Cons y113 y1 -> return (y113, y1); _ -> mzero};
                               guard (x113 == x110);
                               ________checkI x1;
                               return ()},
                           do {let {x115 = One};
                               let {x116 = Thr};
                               let {x114 = Pair x115 x116};
                               (x117, x1) <- case x0 of
                                             {Cons y117 y1 -> return (y117, y1); _ -> mzero};
                               guard (x117 == x114);
                               _______________________checkI x1;
                               return ()}]
________checkI x0 = Immature $ msum [do {let {x95 = One};
                              let {x96 = Two};
                              let {x94 = Pair x95 x96};
                              (x97, x1) <- case x0 of
                                           {Cons y97 y1 -> return (y97, y1); _ -> mzero};
                              guard (x97 == x94);
                              _______checkI x1;
                              return ()},
                          do {let {x99 = One};
                              let {x100 = Thr};
                              let {x98 = Pair x99 x100};
                              (x101, x1) <- case x0 of
                                            {Cons y101 y1 -> return (y101, y1); _ -> mzero};
                              guard (x101 == x98);
                              ________________________checkI x1;
                              return ()},
                          do {let {x103 = Two};
                              let {x104 = Thr};
                              let {x102 = Pair x103 x104};
                              (x105, x1) <- case x0 of
                                            {Cons y105 y1 -> return (y105, y1); _ -> mzero};
                              guard (x105 == x102);
                              _________checkI x1;
                              return ()}]
____checkI x0 = Immature $ msum [do {let {x47 = One};
                          let {x48 = Thr};
                          let {x46 = Pair x47 x48};
                          (x49, x1) <- case x0 of
                                       {Cons y49 y1 -> return (y49, y1); _ -> mzero};
                          guard (x49 == x46);
                          _____checkI x1;
                          return ()},
                      do {let {x51 = Two};
                          let {x52 = Thr};
                          let {x50 = Pair x51 x52};
                          (x53, x1) <- case x0 of
                                       {Cons y53 y1 -> return (y53, y1); _ -> mzero};
                          guard (x53 == x50);
                          ___checkI x1;
                          return ()},
                      do {let {x55 = One};
                          let {x56 = Two};
                          let {x54 = Pair x55 x56};
                          (x57, x1) <- case x0 of
                                       {Cons y57 y1 -> return (y57, y1); _ -> mzero};
                          guard (x57 == x54);
                          ______checkI x1;
                          return ()}]
___checkI x0 = Immature $ msum [do {let {x35 = One};
                         let {x36 = Two};
                         let {x34 = Pair x35 x36};
                         (x37, x1) <- case x0 of
                                      {Cons y37 y1 -> return (y37, y1); _ -> mzero};
                         guard (x37 == x34);
                         __checkI x1;
                         return ()},
                     do {let {x39 = Thr};
                         let {x40 = Two};
                         let {x38 = Pair x39 x40};
                         (x41, x1) <- case x0 of
                                      {Cons y41 y1 -> return (y41, y1); _ -> mzero};
                         guard (x41 == x38);
                         ____checkI x1;
                         return ()},
                     do {let {x43 = One};
                         let {x44 = Thr};
                         let {x42 = Pair x43 x44};
                         (x45, x1) <- case x0 of
                                      {Cons y45 y1 -> return (y45, y1); _ -> mzero};
                         guard (x45 == x42);
                         _________________________checkI x1;
                         return ()}]
__checkI x0 = Immature $ msum [do {let {x23 = Two};
                        let {x24 = One};
                        let {x22 = Pair x23 x24};
                        (x25, x1) <- case x0 of
                                     {Cons y25 y1 -> return (y25, y1); _ -> mzero};
                        guard (x25 == x22);
                        ___checkI x1;
                        return ()},
                    do {let {x27 = Two};
                        let {x28 = Thr};
                        let {x26 = Pair x27 x28};
                        (x29, x1) <- case x0 of
                                     {Cons y29 y1 -> return (y29, y1); _ -> mzero};
                        guard (x29 == x26);
                        _________________________checkI x1;
                        return ()},
                    do {let {x31 = Thr};
                        let {x32 = One};
                        let {x30 = Pair x31 x32};
                        (x33, x1) <- case x0 of
                                     {Cons y33 y1 -> return (y33, y1); _ -> mzero};
                        guard (x33 == x30);
                        _checkI x1;
                        return ()}]
_checkI x0 = Immature $ msum [do {let {x11 = One};
                       let {x12 = Thr};
                       let {x10 = Pair x11 x12};
                       (x13, x1) <- case x0 of
                                    {Cons y13 y1 -> return (y13, y1); _ -> mzero};
                       guard (x13 == x10);
                       __checkI x1;
                       return ()},
                   do {let {x15 = Two};
                       let {x16 = Thr};
                       let {x14 = Pair x15 x16};
                       (x17, x1) <- case x0 of
                                    {Cons y17 y1 -> return (y17, y1); _ -> mzero};
                       guard (x17 == x14);
                       __________________________checkI x1;
                       return ()},
                   do {let {x19 = Two};
                       let {x20 = One};
                       let {x18 = Pair x19 x20};
                       (x21, x1) <- case x0 of
                                    {Cons y21 y1 -> return (y21, y1); _ -> mzero};
                       guard (x21 == x18);
                       checkI x1;
                       return ()}]
checkO = Immature $ msum [do {let {x3 = One};
                   let {x4 = Two};
                   let {x2 = Pair x3 x4};
                   let {x5 = x2};
                   x1 <- _checkO;
                   let {x0 = Cons x5 x1};
                   return x0},
               do {let {x7 = One};
                   let {x8 = Thr};
                   let {x6 = Pair x7 x8};
                   let {x9 = x6};
                   x1 <- __________________________checkO;
                   let {x0 = Cons x9 x1};
                   return x0}]
__________________________checkO = Immature $ msum [do {let {x303 = One};
                                             let {x304 = Two};
                                             let {x302 = Pair x303 x304};
                                             let {x305 = x302};
                                             x1 <- _____checkO;
                                             let {x0 = Cons x305 x1};
                                             return x0},
                                         do {let {x307 = Thr};
                                             let {x308 = Two};
                                             let {x306 = Pair x307 x308};
                                             let {x309 = x306};
                                             x1 <- _checkO;
                                             let {x0 = Cons x309 x1};
                                             return x0},
                                         do {let {x311 = Thr};
                                             let {x312 = One};
                                             let {x310 = Pair x311 x312};
                                             let {x313 = x310};
                                             x1 <- checkO;
                                             let {x0 = Cons x313 x1};
                                             return x0}]
_____checkO = Immature $ msum [do {let {x59 = Two};
                        let {x60 = One};
                        let {x58 = Pair x59 x60};
                        let {x61 = x58};
                        x1 <- __________________________checkO;
                        let {x0 = Cons x61 x1};
                        return x0},
                    do {let {x63 = Thr};
                        let {x64 = One};
                        let {x62 = Pair x63 x64};
                        let {x65 = x62};
                        x1 <- ____checkO;
                        let {x0 = Cons x65 x1};
                        return x0},
                    do {let {x67 = Thr};
                        let {x68 = Two};
                        let {x66 = Pair x67 x68};
                        let {x69 = x66};
                        x1 <- ______checkO;
                        let {x0 = Cons x69 x1};
                        return x0}]
______checkO = Immature $ msum [do {let {x71 = One};
                         let {x72 = Thr};
                         let {x70 = Pair x71 x72};
                         let {x73 = x70};
                         x1 <- _______checkO;
                         let {x0 = Cons x73 x1};
                         return x0},
                     do {let {x75 = Two};
                         let {x76 = Thr};
                         let {x74 = Pair x75 x76};
                         let {x77 = x74};
                         x1 <- _____checkO;
                         let {x0 = Cons x77 x1};
                         return x0},
                     do {let {x79 = Two};
                         let {x80 = One};
                         let {x78 = Pair x79 x80};
                         let {x81 = x78};
                         x1 <- ____checkO;
                         let {x0 = Cons x81 x1};
                         return x0}]
_______checkO = Immature $ msum [do {let {x83 = Two};
                          let {x84 = One};
                          let {x82 = Pair x83 x84};
                          let {x85 = x82};
                          x1 <- ________checkO;
                          let {x0 = Cons x85 x1};
                          return x0},
                      do {let {x87 = Thr};
                          let {x88 = One};
                          let {x86 = Pair x87 x88};
                          let {x89 = x86};
                          x1 <- ______checkO;
                          let {x0 = Cons x89 x1};
                          return x0},
                      do {let {x91 = Two};
                          let {x92 = Thr};
                          let {x90 = Pair x91 x92};
                          let {x93 = x90};
                          x1 <- ________________________checkO;
                          let {x0 = Cons x93 x1};
                          return x0}]
________________________checkO = Immature $ msum [do {let {x279 = Two};
                                           let {x280 = One};
                                           let {x278 = Pair x279 x280};
                                           let {x281 = x278};
                                           x1 <- ______________________checkO;
                                           let {x0 = Cons x281 x1};
                                           return x0},
                                       do {let {x283 = Thr};
                                           let {x284 = One};
                                           let {x282 = Pair x283 x284};
                                           let {x285 = x282};
                                           x1 <- ________checkO;
                                           let {x0 = Cons x285 x1};
                                           return x0},
                                       do {let {x287 = Thr};
                                           let {x288 = Two};
                                           let {x286 = Pair x287 x288};
                                           let {x289 = x286};
                                           x1 <- _______checkO;
                                           let {x0 = Cons x289 x1};
                                           return x0}]
______________________checkO = Immature $ msum [do {let {x259 = One};
                                         let {x260 = Two};
                                         let {x258 = Pair x259 x260};
                                         let {x261 = x258};
                                         x1 <- ________________________checkO;
                                         let {x0 = Cons x261 x1};
                                         return x0},
                                     do {let {x263 = Thr};
                                         let {x264 = Two};
                                         let {x262 = Pair x263 x264};
                                         let {x265 = x262};
                                         x1 <- ___________checkO;
                                         let {x0 = Cons x265 x1};
                                         return x0},
                                     do {let {x267 = Thr};
                                         let {x268 = One};
                                         let {x266 = Pair x267 x268};
                                         let {x269 = x266};
                                         x1 <- ____________checkO;
                                         let {x0 = Cons x269 x1};
                                         return x0}]
____________checkO = Immature $ msum [do {let {x143 = One};
                               let {x144 = Two};
                               let {x142 = Pair x143 x144};
                               let {x145 = x142};
                               x1 <- ___________checkO;
                               let {x0 = Cons x145 x1};
                               return x0},
                           do {let {x147 = Thr};
                               let {x148 = Two};
                               let {x146 = Pair x147 x148};
                               let {x149 = x146};
                               x1 <- _____________checkO;
                               let {x0 = Cons x149 x1};
                               return x0},
                           do {let {x151 = One};
                               let {x152 = Thr};
                               let {x150 = Pair x151 x152};
                               let {x153 = x150};
                               x1 <- ______________________checkO;
                               let {x0 = Cons x153 x1};
                               return x0}]
_____________checkO = Immature $ msum [do {let {x155 = One};
                                let {x156 = Thr};
                                let {x154 = Pair x155 x156};
                                let {x157 = x154};
                                x1 <- ______________checkO;
                                let {x0 = Cons x157 x1};
                                return x0},
                            do {let {x159 = Two};
                                let {x160 = Thr};
                                let {x158 = Pair x159 x160};
                                let {x161 = x158};
                                x1 <- ____________checkO;
                                let {x0 = Cons x161 x1};
                                return x0},
                            do {let {x163 = One};
                                let {x164 = Two};
                                let {x162 = Pair x163 x164};
                                let {x165 = x162};
                                x1 <- _____________________checkO;
                                let {x0 = Cons x165 x1};
                                return x0}]
_____________________checkO = Immature $ msum [do {let {x247 = One};
                                        let {x248 = Thr};
                                        let {x246 = Pair x247 x248};
                                        let {x249 = x246};
                                        x1 <- __________________checkO;
                                        let {x0 = Cons x249 x1};
                                        return x0},
                                    do {let {x251 = Two};
                                        let {x252 = Thr};
                                        let {x250 = Pair x251 x252};
                                        let {x253 = x250};
                                        x1 <- ______________checkO;
                                        let {x0 = Cons x253 x1};
                                        return x0},
                                    do {let {x255 = Two};
                                        let {x256 = One};
                                        let {x254 = Pair x255 x256};
                                        let {x257 = x254};
                                        x1 <- _____________checkO;
                                        let {x0 = Cons x257 x1};
                                        return x0}]
__________________checkO = Immature $ msum [do {let {x215 = Two};
                                     let {x216 = One};
                                     let {x214 = Pair x215 x216};
                                     let {x217 = x214};
                                     x1 <- _________________checkO;
                                     let {x0 = Cons x217 x1};
                                     return x0},
                                 do {let {x219 = Thr};
                                     let {x220 = One};
                                     let {x218 = Pair x219 x220};
                                     let {x221 = x218};
                                     x1 <- _____________________checkO;
                                     let {x0 = Cons x221 x1};
                                     return x0},
                                 do {let {x223 = Two};
                                     let {x224 = Thr};
                                     let {x222 = Pair x223 x224};
                                     let {x225 = x222};
                                     x1 <- ___________________checkO;
                                     let {x0 = Cons x225 x1};
                                     return x0}]
___________________checkO = Immature $ msum [do {let {x227 = Two};
                                      let {x228 = One};
                                      let {x226 = Pair x227 x228};
                                      let {x229 = x226};
                                      x1 <- _________________________checkO;
                                      let {x0 = Cons x229 x1};
                                      return x0},
                                  do {let {x231 = Thr};
                                      let {x232 = One};
                                      let {x230 = Pair x231 x232};
                                      let {x233 = x230};
                                      x1 <- _________________checkO;
                                      let {x0 = Cons x233 x1};
                                      return x0},
                                  do {let {x235 = Thr};
                                      let {x236 = Two};
                                      let {x234 = Pair x235 x236};
                                      let {x237 = x234};
                                      x1 <- __________________checkO;
                                      let {x0 = Cons x237 x1};
                                      return x0}]
_________________________checkO = Immature $ msum [do {let {x291 = One};
                                            let {x292 = Two};
                                            let {x290 = Pair x291 x292};
                                            let {x293 = x290};
                                            x1 <- ___________________checkO;
                                            let {x0 = Cons x293 x1};
                                            return x0},
                                        do {let {x295 = Thr};
                                            let {x296 = Two};
                                            let {x294 = Pair x295 x296};
                                            let {x297 = x294};
                                            x1 <- __checkO;
                                            let {x0 = Cons x297 x1};
                                            return x0},
                                        do {let {x299 = Thr};
                                            let {x300 = One};
                                            let {x298 = Pair x299 x300};
                                            let {x301 = x298};
                                            x1 <- ___checkO;
                                            let {x0 = Cons x301 x1};
                                            return x0}]
_________________checkO = Immature $ msum [do {let {x203 = One};
                                    let {x204 = Two};
                                    let {x202 = Pair x203 x204};
                                    let {x205 = x202};
                                    x1 <- __________________checkO;
                                    let {x0 = Cons x205 x1};
                                    return x0},
                                do {let {x207 = One};
                                    let {x208 = Thr};
                                    let {x206 = Pair x207 x208};
                                    let {x209 = x206};
                                    x1 <- ___________________checkO;
                                    let {x0 = Cons x209 x1};
                                    return x0},
                                do {let {x211 = Thr};
                                    let {x212 = Two};
                                    let {x210 = Pair x211 x212};
                                    let {x213 = x210};
                                    x1 <- ________________checkO;
                                    let {x0 = Cons x213 x1};
                                    return x0}]
________________checkO = Immature $ msum [do {let {x191 = One};
                                   let {x192 = Thr};
                                   let {x190 = Pair x191 x192};
                                   let {x193 = x190};
                                   x1 <- _______________checkO;
                                   let {x0 = Cons x193 x1};
                                   return x0},
                               do {let {x195 = Two};
                                   let {x196 = Thr};
                                   let {x194 = Pair x195 x196};
                                   let {x197 = x194};
                                   x1 <- _________________checkO;
                                   let {x0 = Cons x197 x1};
                                   return x0},
                               do {let {x199 = One};
                                   let {x200 = Two};
                                   let {x198 = Pair x199 x200};
                                   let {x201 = x198};
                                   x1 <- ____________________checkO;
                                   let {x0 = Cons x201 x1};
                                   return x0}]
____________________checkO = Immature $ msum [do {let {x239 = Two};
                                       let {x240 = One};
                                       let {x238 = Pair x239 x240};
                                       let {x241 = x238};
                                       x1 <- ________________checkO;
                                       let {x0 = Cons x241 x1};
                                       return x0},
                                   do {let {x243 = Two};
                                       let {x244 = Thr};
                                       let {x242 = Pair x243 x244};
                                       let {x245 = x242};
                                       x1 <- _______________checkO;
                                       let {x0 = Cons x245 x1};
                                       return x0}]
_______________checkO = Immature $ msum [do {let {x179 = Two};
                                  let {x180 = One};
                                  let {x178 = Pair x179 x180};
                                  let {x181 = x178};
                                  x1 <- ______________checkO;
                                  let {x0 = Cons x181 x1};
                                  return x0},
                              do {let {x183 = Thr};
                                  let {x184 = One};
                                  let {x182 = Pair x183 x184};
                                  let {x185 = x182};
                                  x1 <- ________________checkO;
                                  let {x0 = Cons x185 x1};
                                  return x0},
                              do {let {x187 = Thr};
                                  let {x188 = Two};
                                  let {x186 = Pair x187 x188};
                                  let {x189 = x186};
                                  x1 <- ____________________checkO;
                                  let {x0 = Cons x189 x1};
                                  return x0}]
______________checkO = Immature $ msum [do {let {x167 = One};
                                 let {x168 = Two};
                                 let {x166 = Pair x167 x168};
                                 let {x169 = x166};
                                 x1 <- _______________checkO;
                                 let {x0 = Cons x169 x1};
                                 return x0},
                             do {let {x171 = Thr};
                                 let {x172 = One};
                                 let {x170 = Pair x171 x172};
                                 let {x173 = x170};
                                 x1 <- _____________checkO;
                                 let {x0 = Cons x173 x1};
                                 return x0},
                             do {let {x175 = Thr};
                                 let {x176 = Two};
                                 let {x174 = Pair x175 x176};
                                 let {x177 = x174};
                                 x1 <- _____________________checkO;
                                 let {x0 = Cons x177 x1};
                                 return x0}]
___________checkO = Immature $ msum [do {let {x131 = One};
                              let {x132 = Thr};
                              let {x130 = Pair x131 x132};
                              let {x133 = x130};
                              x1 <- __________checkO;
                              let {x0 = Cons x133 x1};
                              return x0},
                          do {let {x135 = Two};
                              let {x136 = One};
                              let {x134 = Pair x135 x136};
                              let {x137 = x134};
                              x1 <- ____________checkO;
                              let {x0 = Cons x137 x1};
                              return x0},
                          do {let {x139 = Two};
                              let {x140 = Thr};
                              let {x138 = Pair x139 x140};
                              let {x141 = x138};
                              x1 <- ______________________checkO;
                              let {x0 = Cons x141 x1};
                              return x0}]
__________checkO = Immature $ msum [do {let {x119 = Two};
                             let {x120 = One};
                             let {x118 = Pair x119 x120};
                             let {x121 = x118};
                             x1 <- _________checkO;
                             let {x0 = Cons x121 x1};
                             return x0},
                         do {let {x123 = Thr};
                             let {x124 = One};
                             let {x122 = Pair x123 x124};
                             let {x125 = x122};
                             x1 <- ___________checkO;
                             let {x0 = Cons x125 x1};
                             return x0},
                         do {let {x127 = Two};
                             let {x128 = Thr};
                             let {x126 = Pair x127 x128};
                             let {x129 = x126};
                             x1 <- _______________________checkO;
                             let {x0 = Cons x129 x1};
                             return x0}]
_______________________checkO = Immature $ msum [do {let {x0 = Nil};
                                          return x0},
                                      do {let {x271 = Thr};
                                          let {x272 = One};
                                          let {x270 = Pair x271 x272};
                                          let {x273 = x270};
                                          x1 <- _________checkO;
                                          let {x0 = Cons x273 x1};
                                          return x0},
                                      do {let {x275 = Thr};
                                          let {x276 = Two};
                                          let {x274 = Pair x275 x276};
                                          let {x277 = x274};
                                          x1 <- __________checkO;
                                          let {x0 = Cons x277 x1};
                                          return x0}]
_________checkO = Immature $ msum [do {let {x107 = One};
                            let {x108 = Two};
                            let {x106 = Pair x107 x108};
                            let {x109 = x106};
                            x1 <- __________checkO;
                            let {x0 = Cons x109 x1};
                            return x0},
                        do {let {x111 = Thr};
                            let {x112 = Two};
                            let {x110 = Pair x111 x112};
                            let {x113 = x110};
                            x1 <- ________checkO;
                            let {x0 = Cons x113 x1};
                            return x0},
                        do {let {x115 = One};
                            let {x116 = Thr};
                            let {x114 = Pair x115 x116};
                            let {x117 = x114};
                            x1 <- _______________________checkO;
                            let {x0 = Cons x117 x1};
                            return x0}]
________checkO = Immature $ msum [do {let {x95 = One};
                           let {x96 = Two};
                           let {x94 = Pair x95 x96};
                           let {x97 = x94};
                           x1 <- _______checkO;
                           let {x0 = Cons x97 x1};
                           return x0},
                       do {let {x99 = One};
                           let {x100 = Thr};
                           let {x98 = Pair x99 x100};
                           let {x101 = x98};
                           x1 <- ________________________checkO;
                           let {x0 = Cons x101 x1};
                           return x0},
                       do {let {x103 = Two};
                           let {x104 = Thr};
                           let {x102 = Pair x103 x104};
                           let {x105 = x102};
                           x1 <- _________checkO;
                           let {x0 = Cons x105 x1};
                           return x0}]
____checkO = Immature $ msum [do {let {x47 = One};
                       let {x48 = Thr};
                       let {x46 = Pair x47 x48};
                       let {x49 = x46};
                       x1 <- _____checkO;
                       let {x0 = Cons x49 x1};
                       return x0},
                   do {let {x51 = Two};
                       let {x52 = Thr};
                       let {x50 = Pair x51 x52};
                       let {x53 = x50};
                       x1 <- ___checkO;
                       let {x0 = Cons x53 x1};
                       return x0},
                   do {let {x55 = One};
                       let {x56 = Two};
                       let {x54 = Pair x55 x56};
                       let {x57 = x54};
                       x1 <- ______checkO;
                       let {x0 = Cons x57 x1};
                       return x0}]
___checkO = Immature $ msum [do {let {x35 = One};
                      let {x36 = Two};
                      let {x34 = Pair x35 x36};
                      let {x37 = x34};
                      x1 <- __checkO;
                      let {x0 = Cons x37 x1};
                      return x0},
                  do {let {x39 = Thr};
                      let {x40 = Two};
                      let {x38 = Pair x39 x40};
                      let {x41 = x38};
                      x1 <- ____checkO;
                      let {x0 = Cons x41 x1};
                      return x0},
                  do {let {x43 = One};
                      let {x44 = Thr};
                      let {x42 = Pair x43 x44};
                      let {x45 = x42};
                      x1 <- _________________________checkO;
                      let {x0 = Cons x45 x1};
                      return x0}]
__checkO = Immature $ msum [do {let {x23 = Two};
                     let {x24 = One};
                     let {x22 = Pair x23 x24};
                     let {x25 = x22};
                     x1 <- ___checkO;
                     let {x0 = Cons x25 x1};
                     return x0},
                 do {let {x27 = Two};
                     let {x28 = Thr};
                     let {x26 = Pair x27 x28};
                     let {x29 = x26};
                     x1 <- _________________________checkO;
                     let {x0 = Cons x29 x1};
                     return x0},
                 do {let {x31 = Thr};
                     let {x32 = One};
                     let {x30 = Pair x31 x32};
                     let {x33 = x30};
                     x1 <- _checkO;
                     let {x0 = Cons x33 x1};
                     return x0}]
_checkO = Immature $ msum [do {let {x11 = One};
                    let {x12 = Thr};
                    let {x10 = Pair x11 x12};
                    let {x13 = x10};
                    x1 <- __checkO;
                    let {x0 = Cons x13 x1};
                    return x0},
                do {let {x15 = Two};
                    let {x16 = Thr};
                    let {x14 = Pair x15 x16};
                    let {x17 = x14};
                    x1 <- __________________________checkO;
                    let {x0 = Cons x17 x1};
                    return x0},
                do {let {x19 = Two};
                    let {x20 = One};
                    let {x18 = Pair x19 x20};
                    let {x21 = x18};
                    x1 <- checkO;
                    let {x0 = Cons x21 x1};
                    return x0}]