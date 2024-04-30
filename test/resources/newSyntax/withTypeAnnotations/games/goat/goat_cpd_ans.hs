module Goat_cpd_ans where

import Stream
import Control.Monad

data Term
    = Cabbage
    | Cons Term Term
    | Empty
    | False
    | Goat
    | Nil
    | Pair Term Term
    | Quad Term Term Term Term
    | True
    | Wolf
    deriving (Show, Eq)
evalI x0 = msum [do {(x1, x2) <- case x0 of
                                 {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                     step_EvalII x1 x2;
                     return ()}]
evalO = msum [do {(x1, x2) <- step_EvalOO;
                  let {x0 = Cons x1 x2};
                  return x0}]
step_EvalII x0 x1 = msum [do {let {x11 = True};
                              let {x12 = True};
                              let {x13 = True};
                              _________safe_III x11 x12 x13;
                              let {x14 = False};
                              let {x15 = False};
                              let {x16 = False};
                              __________safe_III x14 x15 x16;
                              (x2, x3) <- case x1 of
                                          {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                              stepEvalII x2 x3;
                              guard (x0 == Empty);
                              return ()},
                          do {let {x17 = False};
                              let {x18 = True};
                              let {x19 = True};
                              _________safe_III x17 x18 x19;
                              let {x20 = True};
                              let {x21 = False};
                              let {x22 = False};
                              __________safe_III x20 x21 x22;
                              (x4, x5) <- case x1 of
                                          {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                              __stepEvalII x4 x5;
                              guard (x0 == Goat);
                              return ()},
                          do {let {x23 = True};
                              let {x24 = False};
                              let {x25 = True};
                              _________safe_III x23 x24 x25;
                              let {x26 = False};
                              let {x27 = True};
                              let {x28 = False};
                              __________safe_III x26 x27 x28;
                              (x4, x5) <- case x1 of
                                          {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                              ________________stepEvalII x4 x5;
                              guard (x0 == Wolf);
                              return ()},
                          do {let {x29 = True};
                              let {x30 = True};
                              let {x31 = False};
                              _________safe_III x29 x30 x31;
                              let {x32 = False};
                              let {x33 = False};
                              let {x34 = True};
                              __________safe_III x32 x33 x34;
                              (x4, x5) <- case x1 of
                                          {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                              ____________________stepEvalII x4 x5;
                              guard (x0 == Cabbage);
                              return ()}]
____________________stepEvalII x0 x1 = msum [do {__________________step_EvalII x0 x1;
                                                 return ()}]
__________________step_EvalII x0 x1 = msum [do {let {x352 = False};
                                                let {x353 = False};
                                                let {x354 = True};
                                                _________safe_III x352 x353 x354;
                                                let {x355 = True};
                                                let {x356 = True};
                                                let {x357 = False};
                                                __________safe_III x355 x356 x357;
                                                (x2, x3) <- case x1 of
                                                            {Cons y2 y3 -> return (y2, y3);
                                                             _ -> mzero};
                                                ______________stepEvalII x2 x3;
                                                guard (x0 == Empty);
                                                return ()},
                                            do {let {x358 = False};
                                                let {x359 = False};
                                                let {x360 = False};
                                                _________safe_III x358 x359 x360;
                                                let {x361 = True};
                                                let {x362 = True};
                                                let {x363 = True};
                                                __________safe_III x361 x362 x363;
                                                (x4, x5) <- case x1 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                _stepEvalII x4 x5;
                                                guard (x0 == Cabbage);
                                                return ()}]
________________stepEvalII x0 x1 = msum [do {______________step_EvalII x0 x1;
                                             return ()}]
______________stepEvalII x0 x1 = msum [do {_____________step_EvalII x0 x1;
                                           return ()}]
______________step_EvalII x0 x1 = msum [do {let {x281 = False};
                                            let {x282 = True};
                                            let {x283 = False};
                                            _________safe_III x281 x282 x283;
                                            let {x284 = True};
                                            let {x285 = False};
                                            let {x286 = True};
                                            __________safe_III x284 x285 x286;
                                            (x2, x3) <- case x1 of
                                                        {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                            _________________stepEvalII x2 x3;
                                            guard (x0 == Empty);
                                            return ()},
                                        do {let {x287 = False};
                                            let {x288 = False};
                                            let {x289 = False};
                                            _________safe_III x287 x288 x289;
                                            let {x290 = False};
                                            let {x291 = True};
                                            let {x292 = True};
                                            __________safe_III x290 x291 x292;
                                            (x4, x5) <- case x1 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                            __________stepEvalII x4 x5;
                                            guard (x0 == Wolf);
                                            return ()}]
_________________stepEvalII x0 x1 = msum [do {_______________step_EvalII x0 x1;
                                              return ()}]
_______________step_EvalII x0 x1 = msum [do {let {x293 = True};
                                             let {x294 = False};
                                             let {x295 = True};
                                             _________safe_III x293 x294 x295;
                                             let {x296 = False};
                                             let {x297 = True};
                                             let {x298 = False};
                                             __________safe_III x296 x297 x298;
                                             (x2, x3) <- case x1 of
                                                         {Cons y2 y3 -> return (y2, y3);
                                                          _ -> mzero};
                                             ________________stepEvalII x2 x3;
                                             guard (x0 == Empty);
                                             return ()},
                                         do {let {x299 = False};
                                             let {x300 = False};
                                             let {x301 = True};
                                             _________safe_III x299 x300 x301;
                                             let {x302 = True};
                                             let {x303 = True};
                                             let {x304 = False};
                                             __________safe_III x302 x303 x304;
                                             (x4, x5) <- case x1 of
                                                         {Cons y4 y5 -> return (y4, y5);
                                                          _ -> mzero};
                                             __________________stepEvalII x4 x5;
                                             guard (x0 == Goat);
                                             return ()},
                                         do {let {x305 = True};
                                             let {x306 = False};
                                             let {x307 = False};
                                             _________safe_III x305 x306 x307;
                                             let {x308 = False};
                                             let {x309 = True};
                                             let {x310 = True};
                                             __________safe_III x308 x309 x310;
                                             (x4, x5) <- case x1 of
                                                         {Cons y4 y5 -> return (y4, y5);
                                                          _ -> mzero};
                                             _______________stepEvalII x4 x5;
                                             guard (x0 == Cabbage);
                                             return ()}]
__________________stepEvalII x0 x1 = msum [do {________________step_EvalII x0 x1;
                                               return ()}]
________________step_EvalII x0 x1 = msum [do {let {x311 = True};
                                              let {x312 = True};
                                              let {x313 = False};
                                              _________safe_III x311 x312 x313;
                                              let {x314 = False};
                                              let {x315 = False};
                                              let {x316 = True};
                                              __________safe_III x314 x315 x316;
                                              (x2, x3) <- case x1 of
                                                          {Cons y2 y3 -> return (y2, y3);
                                                           _ -> mzero};
                                              ___________________stepEvalII x2 x3;
                                              guard (x0 == Empty);
                                              return ()},
                                          do {let {x317 = False};
                                              let {x318 = True};
                                              let {x319 = False};
                                              _________safe_III x317 x318 x319;
                                              let {x320 = True};
                                              let {x321 = False};
                                              let {x322 = True};
                                              __________safe_III x320 x321 x322;
                                              (x4, x5) <- case x1 of
                                                          {Cons y4 y5 -> return (y4, y5);
                                                           _ -> mzero};
                                              _________________stepEvalII x4 x5;
                                              guard (x0 == Goat);
                                              return ()},
                                          do {let {x323 = True};
                                              let {x324 = False};
                                              let {x325 = False};
                                              _________safe_III x323 x324 x325;
                                              let {x326 = False};
                                              let {x327 = True};
                                              let {x328 = True};
                                              __________safe_III x326 x327 x328;
                                              (x4, x5) <- case x1 of
                                                          {Cons y4 y5 -> return (y4, y5);
                                                           _ -> mzero};
                                              ___stepEvalII x4 x5;
                                              guard (x0 == Wolf);
                                              return ()}]
___________________stepEvalII x0 x1 = msum [do {_________________step_EvalII x0 x1;
                                                return ()}]
_________________step_EvalII x0 x1 = msum [do {let {x329 = False};
                                               let {x330 = False};
                                               let {x331 = True};
                                               _________safe_III x329 x330 x331;
                                               let {x332 = True};
                                               let {x333 = True};
                                               let {x334 = False};
                                               __________safe_III x332 x333 x334;
                                               (x2, x3) <- case x1 of
                                                           {Cons y2 y3 -> return (y2, y3);
                                                            _ -> mzero};
                                               __________________stepEvalII x2 x3;
                                               guard (x0 == Empty);
                                               return ()},
                                           do {let {x335 = False};
                                               let {x336 = False};
                                               let {x337 = False};
                                               _________safe_III x335 x336 x337;
                                               let {x338 = True};
                                               let {x339 = True};
                                               let {x340 = True};
                                               __________safe_III x338 x339 x340;
                                               let {x343 = False};
                                               let {x344 = False};
                                               let {x345 = False};
                                               let {x346 = False};
                                               let {x342 = Quad x343 x344 x345 x346};
                                               let {x348 = True};
                                               let {x349 = True};
                                               let {x350 = True};
                                               let {x351 = True};
                                               let {x347 = Quad x348 x349 x350 x351};
                                               let {x341 = Pair x342 x347};
                                               _evalII x341 x1;
                                               guard (x0 == Cabbage);
                                               return ()}]
_______________stepEvalII x0 x1 = msum [do {let {x274 = False};
                                            let {x275 = True};
                                            let {x276 = True};
                                            let {x277 = True};
                                            let {x278 = False};
                                            let {x279 = False};
                                            x273 <- _step_IOIIIIII x0 x274 x275 x276 x277 x278 x279;
                                            (x2, x3) <- case x273 of
                                                        {Pair y2 y3 -> return (y2, y3); _ -> mzero};
                                            let {x280 = Pair x3 x2};
                                            _evalII x280 x1;
                                            return ()}]
_____________step_EvalII x0 x1 = msum [do {let {x255 = True};
                                           let {x256 = True};
                                           let {x257 = False};
                                           _________safe_III x255 x256 x257;
                                           let {x258 = False};
                                           let {x259 = False};
                                           let {x260 = True};
                                           __________safe_III x258 x259 x260;
                                           (x2, x3) <- case x1 of
                                                       {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                           ____________________stepEvalII x2 x3;
                                           guard (x0 == Empty);
                                           return ()},
                                       do {let {x261 = False};
                                           let {x262 = True};
                                           let {x263 = False};
                                           _________safe_III x261 x262 x263;
                                           let {x264 = True};
                                           let {x265 = False};
                                           let {x266 = True};
                                           __________safe_III x264 x265 x266;
                                           (x4, x5) <- case x1 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                           ____________stepEvalII x4 x5;
                                           guard (x0 == Goat);
                                           return ()},
                                       do {let {x267 = True};
                                           let {x268 = False};
                                           let {x269 = False};
                                           _________safe_III x267 x268 x269;
                                           let {x270 = False};
                                           let {x271 = True};
                                           let {x272 = True};
                                           __________safe_III x270 x271 x272;
                                           (x4, x5) <- case x1 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                           _______________stepEvalII x4 x5;
                                           guard (x0 == Wolf);
                                           return ()}]
____________stepEvalII x0 x1 = msum [do {___________step_EvalII x0 x1;
                                         return ()}]
___________step_EvalII x0 x1 = msum [do {let {x225 = True};
                                         let {x226 = False};
                                         let {x227 = True};
                                         _________safe_III x225 x226 x227;
                                         let {x228 = False};
                                         let {x229 = True};
                                         let {x230 = False};
                                         __________safe_III x228 x229 x230;
                                         (x2, x3) <- case x1 of
                                                     {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                         _____________stepEvalII x2 x3;
                                         guard (x0 == Empty);
                                         return ()},
                                     do {let {x231 = False};
                                         let {x232 = False};
                                         let {x233 = True};
                                         _________safe_III x231 x232 x233;
                                         let {x234 = True};
                                         let {x235 = True};
                                         let {x236 = False};
                                         __________safe_III x234 x235 x236;
                                         (x4, x5) <- case x1 of
                                                     {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                         ______________stepEvalII x4 x5;
                                         guard (x0 == Goat);
                                         return ()},
                                     do {let {x237 = True};
                                         let {x238 = False};
                                         let {x239 = False};
                                         _________safe_III x237 x238 x239;
                                         let {x240 = False};
                                         let {x241 = True};
                                         let {x242 = True};
                                         __________safe_III x240 x241 x242;
                                         (x4, x5) <- case x1 of
                                                     {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                         ___stepEvalII x4 x5;
                                         guard (x0 == Cabbage);
                                         return ()}]
_____________stepEvalII x0 x1 = msum [do {____________step_EvalII x0 x1;
                                          return ()}]
____________step_EvalII x0 x1 = msum [do {let {x243 = False};
                                          let {x244 = True};
                                          let {x245 = False};
                                          _________safe_III x243 x244 x245;
                                          let {x246 = True};
                                          let {x247 = False};
                                          let {x248 = True};
                                          __________safe_III x246 x247 x248;
                                          (x2, x3) <- case x1 of
                                                      {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                          ____________stepEvalII x2 x3;
                                          guard (x0 == Empty);
                                          return ()},
                                      do {let {x249 = False};
                                          let {x250 = False};
                                          let {x251 = False};
                                          _________safe_III x249 x250 x251;
                                          let {x252 = False};
                                          let {x253 = True};
                                          let {x254 = True};
                                          __________safe_III x252 x253 x254;
                                          (x4, x5) <- case x1 of
                                                      {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                          ______stepEvalII x4 x5;
                                          guard (x0 == Wolf);
                                          return ()}]
__________safe_III x0 x1 x2 = msum [do {let {x200 = x0};
                                        guard (x200 == x0);
                                        return ()}]
__________stepEvalII x0 x1 = msum [do {_________step_EvalII x0 x1;
                                       return ()}]
_________safe_III x0 x1 x2 = msum [do {guard (x0 == False);
                                       return ()},
                                   do {guard (x2 == True);
                                       guard (x1 == True);
                                       guard (x0 == True);
                                       return ()},
                                   do {guard (x2 == False);
                                       guard (x1 == False);
                                       guard (x0 == True);
                                       return ()}]
_________step_EvalII x0 x1 = msum [do {let {x201 = False};
                                       let {x202 = True};
                                       let {x203 = True};
                                       _________safe_III x201 x202 x203;
                                       let {x204 = False};
                                       let {x205 = False};
                                       let {x206 = False};
                                       __________safe_III x204 x205 x206;
                                       (x2, x3) <- case x1 of
                                                   {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                       ___________stepEvalII x2 x3;
                                       guard (x0 == Empty);
                                       return ()},
                                   do {let {x207 = False};
                                       let {x208 = False};
                                       let {x209 = True};
                                       _________safe_III x207 x208 x209;
                                       let {x210 = False};
                                       let {x211 = True};
                                       let {x212 = False};
                                       __________safe_III x210 x211 x212;
                                       (x4, x5) <- case x1 of
                                                   {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                       ____stepEvalII x4 x5;
                                       guard (x0 == Wolf);
                                       return ()},
                                   do {let {x213 = False};
                                       let {x214 = True};
                                       let {x215 = False};
                                       _________safe_III x213 x214 x215;
                                       let {x216 = False};
                                       let {x217 = False};
                                       let {x218 = True};
                                       __________safe_III x216 x217 x218;
                                       (x4, x5) <- case x1 of
                                                   {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                       _________stepEvalII x4 x5;
                                       guard (x0 == Cabbage);
                                       return ()}]
___________stepEvalII x0 x1 = msum [do {__________step_EvalII x0 x1;
                                        return ()}]
__________step_EvalII x0 x1 = msum [do {let {x219 = False};
                                        let {x220 = False};
                                        let {x221 = False};
                                        _________safe_III x219 x220 x221;
                                        let {x222 = False};
                                        let {x223 = True};
                                        let {x224 = True};
                                        __________safe_III x222 x223 x224;
                                        (x2, x3) <- case x1 of
                                                    {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                        __________stepEvalII x2 x3;
                                        guard (x0 == Empty);
                                        return ()}]
_________stepEvalII x0 x1 = msum [do {let {x132 = False};
                                      let {x133 = False};
                                      let {x134 = True};
                                      let {x135 = False};
                                      let {x136 = True};
                                      let {x137 = False};
                                      x131 <- _step_IOIIIIII x0 x132 x133 x134 x135 x136 x137;
                                      (x2, x3) <- case x131 of
                                                  {Pair y2 y3 -> return (y2, y3); _ -> mzero};
                                      let {x138 = Pair x3 x2};
                                      _evalII x138 x1;
                                      return ()}]
______stepEvalII x0 x1 = msum [do {______step_EvalII x0 x1;
                                   return ()}]
______step_EvalII x0 x1 = msum [do {let {x95 = False};
                                    let {x96 = True};
                                    let {x97 = True};
                                    _________safe_III x95 x96 x97;
                                    let {x98 = False};
                                    let {x99 = False};
                                    let {x100 = False};
                                    __________safe_III x98 x99 x100;
                                    (x2, x3) <- case x1 of
                                                {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                    _______stepEvalII x2 x3;
                                    guard (x0 == Empty);
                                    return ()},
                                do {let {x101 = False};
                                    let {x102 = False};
                                    let {x103 = True};
                                    _________safe_III x101 x102 x103;
                                    let {x104 = False};
                                    let {x105 = True};
                                    let {x106 = False};
                                    __________safe_III x104 x105 x106;
                                    (x4, x5) <- case x1 of
                                                {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                    ________stepEvalII x4 x5;
                                    guard (x0 == Wolf);
                                    return ()},
                                do {let {x107 = False};
                                    let {x108 = True};
                                    let {x109 = False};
                                    _________safe_III x107 x108 x109;
                                    let {x110 = False};
                                    let {x111 = False};
                                    let {x112 = True};
                                    __________safe_III x110 x111 x112;
                                    (x4, x5) <- case x1 of
                                                {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                    _____stepEvalII x4 x5;
                                    guard (x0 == Cabbage);
                                    return ()}]
________stepEvalII x0 x1 = msum [do {________step_EvalII x0 x1;
                                     return ()}]
________step_EvalII x0 x1 = msum [do {let {x119 = False};
                                      let {x120 = True};
                                      let {x121 = False};
                                      _________safe_III x119 x120 x121;
                                      let {x122 = False};
                                      let {x123 = False};
                                      let {x124 = True};
                                      __________safe_III x122 x123 x124;
                                      (x2, x3) <- case x1 of
                                                  {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                      _________stepEvalII x2 x3;
                                      guard (x0 == Empty);
                                      return ()},
                                  do {let {x125 = False};
                                      let {x126 = False};
                                      let {x127 = False};
                                      _________safe_III x125 x126 x127;
                                      let {x128 = False};
                                      let {x129 = True};
                                      let {x130 = True};
                                      __________safe_III x128 x129 x130;
                                      (x4, x5) <- case x1 of
                                                  {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                      ______stepEvalII x4 x5;
                                      guard (x0 == Wolf);
                                      return ()}]
_______stepEvalII x0 x1 = msum [do {_______step_EvalII x0 x1;
                                    return ()}]
_______step_EvalII x0 x1 = msum [do {let {x113 = False};
                                     let {x114 = False};
                                     let {x115 = False};
                                     _________safe_III x113 x114 x115;
                                     let {x116 = False};
                                     let {x117 = True};
                                     let {x118 = True};
                                     __________safe_III x116 x117 x118;
                                     (x2, x3) <- case x1 of
                                                 {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                     ______stepEvalII x2 x3;
                                     guard (x0 == Empty);
                                     return ()}]
_____stepEvalII x0 x1 = msum [do {_____step_EvalII x0 x1;
                                  return ()}]
_____step_EvalII x0 x1 = msum [do {let {x83 = False};
                                   let {x84 = False};
                                   let {x85 = True};
                                   _________safe_III x83 x84 x85;
                                   let {x86 = False};
                                   let {x87 = True};
                                   let {x88 = False};
                                   __________safe_III x86 x87 x88;
                                   (x2, x3) <- case x1 of
                                               {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                   ____stepEvalII x2 x3;
                                   guard (x0 == Empty);
                                   return ()},
                               do {let {x89 = False};
                                   let {x90 = False};
                                   let {x91 = False};
                                   _________safe_III x89 x90 x91;
                                   let {x92 = False};
                                   let {x93 = True};
                                   let {x94 = True};
                                   __________safe_III x92 x93 x94;
                                   (x4, x5) <- case x1 of
                                               {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                   ______stepEvalII x4 x5;
                                   guard (x0 == Cabbage);
                                   return ()}]
____stepEvalII x0 x1 = msum [do {____step_EvalII x0 x1; return ()}]
____step_EvalII x0 x1 = msum [do {let {x71 = False};
                                  let {x72 = True};
                                  let {x73 = False};
                                  _________safe_III x71 x72 x73;
                                  let {x74 = False};
                                  let {x75 = False};
                                  let {x76 = True};
                                  __________safe_III x74 x75 x76;
                                  (x2, x3) <- case x1 of
                                              {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                  _____stepEvalII x2 x3;
                                  guard (x0 == Empty);
                                  return ()},
                              do {let {x77 = False};
                                  let {x78 = False};
                                  let {x79 = False};
                                  _________safe_III x77 x78 x79;
                                  let {x80 = False};
                                  let {x81 = True};
                                  let {x82 = True};
                                  __________safe_III x80 x81 x82;
                                  (x4, x5) <- case x1 of
                                              {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                  __________stepEvalII x4 x5;
                                  guard (x0 == Wolf);
                                  return ()}]
___stepEvalII x0 x1 = msum [do {___step_EvalII x0 x1; return ()}]
___step_EvalII x0 x1 = msum [do {let {x53 = False};
                                 let {x54 = True};
                                 let {x55 = True};
                                 _________safe_III x53 x54 x55;
                                 let {x56 = True};
                                 let {x57 = False};
                                 let {x58 = False};
                                 __________safe_III x56 x57 x58;
                                 (x2, x3) <- case x1 of
                                             {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                 __stepEvalII x2 x3;
                                 guard (x0 == Empty);
                                 return ()},
                             do {let {x59 = False};
                                 let {x60 = False};
                                 let {x61 = True};
                                 _________safe_III x59 x60 x61;
                                 let {x62 = False};
                                 let {x63 = True};
                                 let {x64 = False};
                                 __________safe_III x62 x63 x64;
                                 (x4, x5) <- case x1 of
                                             {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                 ____stepEvalII x4 x5;
                                 guard (x0 == Wolf);
                                 return ()},
                             do {let {x65 = False};
                                 let {x66 = True};
                                 let {x67 = False};
                                 _________safe_III x65 x66 x67;
                                 let {x68 = True};
                                 let {x69 = False};
                                 let {x70 = True};
                                 __________safe_III x68 x69 x70;
                                 (x4, x5) <- case x1 of
                                             {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                 ____________stepEvalII x4 x5;
                                 guard (x0 == Cabbage);
                                 return ()}]
__stepEvalII x0 x1 = msum [do {__step_EvalII x0 x1; return ()}]
__step_EvalII x0 x1 = msum [do {let {x41 = True};
                                let {x42 = False};
                                let {x43 = False};
                                _________safe_III x41 x42 x43;
                                let {x44 = False};
                                let {x45 = True};
                                let {x46 = True};
                                __________safe_III x44 x45 x46;
                                (x2, x3) <- case x1 of
                                            {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                ___stepEvalII x2 x3;
                                guard (x0 == Empty);
                                return ()},
                            do {let {x47 = False};
                                let {x48 = False};
                                let {x49 = False};
                                _________safe_III x47 x48 x49;
                                let {x50 = True};
                                let {x51 = True};
                                let {x52 = True};
                                __________safe_III x50 x51 x52;
                                (x4, x5) <- case x1 of
                                            {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                _stepEvalII x4 x5;
                                guard (x0 == Goat);
                                return ()}]
_evalII x0 x1 = msum [do {let {x140 = False};
                          let {x141 = False};
                          let {x142 = False};
                          let {x143 = False};
                          let {x139 = Quad x140 x141 x142 x143};
                          let {x145 = True};
                          let {x146 = True};
                          let {x147 = True};
                          let {x148 = True};
                          let {x144 = Quad x145 x146 x147 x148};
                          guard (x1 == Nil);
                          (x149, x150) <- case x0 of
                                          {Pair y149 y150 -> return (y149, y150); _ -> mzero};
                          guard (x149 == x139);
                          guard (x150 == x144);
                          return ()},
                      do {(x2, x3) <- case x1 of
                                      {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                          x4 <- stepIIO x0 x2;
                          _evalII x4 x3;
                          return ()}]
_stepEvalII x0 x1 = msum [do {step_EvalII x0 x1; return ()}]
_step_IOIIIIII x0 x2 x3 x4 x5 x6 x7 = msum [do {_________safe_III x2 x3 x4;
                                                __________safe_III x5 x6 x7;
                                                let {x165 = False};
                                                let {x164 = Quad x2 x3 x4 x165};
                                                let {x167 = True};
                                                let {x166 = Quad x5 x6 x7 x167};
                                                guard (x0 == Empty);
                                                let {x168 = x164};
                                                let {x169 = x166};
                                                let {x1 = Pair x168 x169};
                                                return x1},
                                            do {let {x170 = False};
                                                _________safe_III x170 x3 x4;
                                                let {x172 = False};
                                                let {x173 = False};
                                                let {x171 = Quad x172 x3 x4 x173};
                                                let {x175 = True};
                                                let {x176 = True};
                                                let {x174 = Quad x175 x6 x7 x176};
                                                let {x179 = True};
                                                __________safe_III x179 x6 x7;
                                                guard (x2 == True);
                                                guard (x0 == Goat);
                                                let {x177 = x171};
                                                let {x178 = x174};
                                                let {x1 = Pair x177 x178};
                                                return x1},
                                            do {let {x180 = False};
                                                _________safe_III x2 x180 x4;
                                                let {x182 = False};
                                                let {x183 = False};
                                                let {x181 = Quad x2 x182 x4 x183};
                                                let {x185 = True};
                                                let {x186 = True};
                                                let {x184 = Quad x6 x185 x7 x186};
                                                let {x189 = True};
                                                __________safe_III x6 x189 x7;
                                                guard (x3 == True);
                                                guard (x0 == Wolf);
                                                let {x187 = x181};
                                                let {x188 = x184};
                                                let {x1 = Pair x187 x188};
                                                return x1},
                                            do {let {x190 = False};
                                                _________safe_III x2 x3 x190;
                                                let {x192 = False};
                                                let {x193 = False};
                                                let {x191 = Quad x2 x3 x192 x193};
                                                let {x195 = True};
                                                let {x196 = True};
                                                let {x194 = Quad x5 x6 x195 x196};
                                                let {x199 = True};
                                                __________safe_III x5 x6 x199;
                                                guard (x4 == True);
                                                guard (x0 == Cabbage);
                                                let {x197 = x191};
                                                let {x198 = x194};
                                                let {x1 = Pair x197 x198};
                                                return x1}]
stepIIO x0 x1 = msum [do {let {x152 = True};
                          let {x154 = False};
                          (x155, x156) <- case x0 of
                                          {Pair y155 y156 -> return (y155, y156); _ -> mzero};
                          let {x151 = x155};
                          (x3, x4, x5) <- case x151 of
                                          {Quad y3 y4 y5 y152 -> do {guard (x152 == y152);
                                                                     return (y3, y4, y5)};
                                           _ -> mzero};
                          let {x153 = x156};
                          (x6, x7, x8) <- case x153 of
                                          {Quad y6 y7 y8 y154 -> do {guard (x154 == y154);
                                                                     return (y6, y7, y8)};
                                           _ -> mzero};
                          x2 <- _step_IOIIIIII x1 x3 x4 x5 x6 x7 x8;
                          return x2},
                      do {let {x158 = False};
                          let {x160 = True};
                          (x161, x162) <- case x0 of
                                          {Pair y161 y162 -> return (y161, y162); _ -> mzero};
                          let {x157 = x161};
                          (x6, x7, x8) <- case x157 of
                                          {Quad y6 y7 y8 y158 -> do {guard (x158 == y158);
                                                                     return (y6, y7, y8)};
                                           _ -> mzero};
                          let {x159 = x162};
                          (x3, x4, x5) <- case x159 of
                                          {Quad y3 y4 y5 y160 -> do {guard (x160 == y160);
                                                                     return (y3, y4, y5)};
                                           _ -> mzero};
                          x163 <- _step_IOIIIIII x1 x3 x4 x5 x6 x7 x8;
                          (x10, x9) <- case x163 of
                                       {Pair y10 y9 -> return (y10, y9); _ -> mzero};
                          let {x2 = Pair x9 x10};
                          return x2}]
stepEvalII x0 x1 = msum [do {_step_EvalII x0 x1; return ()}]
_step_EvalII x0 x1 = msum [do {let {x35 = False};
                               let {x36 = False};
                               let {x37 = False};
                               _________safe_III x35 x36 x37;
                               let {x38 = True};
                               let {x39 = True};
                               let {x40 = True};
                               __________safe_III x38 x39 x40;
                               (x2, x3) <- case x1 of
                                           {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                               _stepEvalII x2 x3;
                               guard (x0 == Empty);
                               return ()}]
step_EvalOO = msum [do {let {x0 = Empty};
                        let {x11 = True};
                        let {x12 = True};
                        let {x13 = True};
                        _________safe_III x11 x12 x13;
                        let {x14 = False};
                        let {x15 = False};
                        let {x16 = False};
                        __________safe_III x14 x15 x16;
                        (x2, x3) <- stepEvalOO;
                        let {x1 = Cons x2 x3};
                        return (x0, x1)},
                    do {let {x0 = Goat};
                        let {x17 = False};
                        let {x18 = True};
                        let {x19 = True};
                        _________safe_III x17 x18 x19;
                        let {x20 = True};
                        let {x21 = False};
                        let {x22 = False};
                        __________safe_III x20 x21 x22;
                        (x4, x5) <- __stepEvalOO;
                        let {x1 = Cons x4 x5};
                        return (x0, x1)},
                    do {let {x0 = Wolf};
                        let {x23 = True};
                        let {x24 = False};
                        let {x25 = True};
                        _________safe_III x23 x24 x25;
                        let {x26 = False};
                        let {x27 = True};
                        let {x28 = False};
                        __________safe_III x26 x27 x28;
                        (x4, x5) <- ________________stepEvalOO;
                        let {x1 = Cons x4 x5};
                        return (x0, x1)},
                    do {let {x0 = Cabbage};
                        let {x29 = True};
                        let {x30 = True};
                        let {x31 = False};
                        _________safe_III x29 x30 x31;
                        let {x32 = False};
                        let {x33 = False};
                        let {x34 = True};
                        __________safe_III x32 x33 x34;
                        (x4, x5) <- ____________________stepEvalOO;
                        let {x1 = Cons x4 x5};
                        return (x0, x1)}]
____________________stepEvalOO = msum [do {(x0,
                                            x1) <- __________________step_EvalOO;
                                           return (x0, x1)}]
__________________step_EvalOO = msum [do {let {x0 = Empty};
                                          let {x352 = False};
                                          let {x353 = False};
                                          let {x354 = True};
                                          _________safe_III x352 x353 x354;
                                          let {x355 = True};
                                          let {x356 = True};
                                          let {x357 = False};
                                          __________safe_III x355 x356 x357;
                                          (x2, x3) <- ______________stepEvalOO;
                                          let {x1 = Cons x2 x3};
                                          return (x0, x1)},
                                      do {let {x0 = Cabbage};
                                          let {x358 = False};
                                          let {x359 = False};
                                          let {x360 = False};
                                          _________safe_III x358 x359 x360;
                                          let {x361 = True};
                                          let {x362 = True};
                                          let {x363 = True};
                                          __________safe_III x361 x362 x363;
                                          (x4, x5) <- _stepEvalOO;
                                          let {x1 = Cons x4 x5};
                                          return (x0, x1)}]
________________stepEvalOO = msum [do {(x0,
                                        x1) <- ______________step_EvalOO;
                                       return (x0, x1)}]
______________stepEvalOO = msum [do {(x0,
                                      x1) <- _____________step_EvalOO;
                                     return (x0, x1)}]
______________step_EvalOO = msum [do {let {x0 = Empty};
                                      let {x281 = False};
                                      let {x282 = True};
                                      let {x283 = False};
                                      _________safe_III x281 x282 x283;
                                      let {x284 = True};
                                      let {x285 = False};
                                      let {x286 = True};
                                      __________safe_III x284 x285 x286;
                                      (x2, x3) <- _________________stepEvalOO;
                                      let {x1 = Cons x2 x3};
                                      return (x0, x1)},
                                  do {let {x0 = Wolf};
                                      let {x287 = False};
                                      let {x288 = False};
                                      let {x289 = False};
                                      _________safe_III x287 x288 x289;
                                      let {x290 = False};
                                      let {x291 = True};
                                      let {x292 = True};
                                      __________safe_III x290 x291 x292;
                                      (x4, x5) <- __________stepEvalOO;
                                      let {x1 = Cons x4 x5};
                                      return (x0, x1)}]
_________________stepEvalOO = msum [do {(x0,
                                         x1) <- _______________step_EvalOO;
                                        return (x0, x1)}]
_______________step_EvalOO = msum [do {let {x0 = Empty};
                                       let {x293 = True};
                                       let {x294 = False};
                                       let {x295 = True};
                                       _________safe_III x293 x294 x295;
                                       let {x296 = False};
                                       let {x297 = True};
                                       let {x298 = False};
                                       __________safe_III x296 x297 x298;
                                       (x2, x3) <- ________________stepEvalOO;
                                       let {x1 = Cons x2 x3};
                                       return (x0, x1)},
                                   do {let {x0 = Goat};
                                       let {x299 = False};
                                       let {x300 = False};
                                       let {x301 = True};
                                       _________safe_III x299 x300 x301;
                                       let {x302 = True};
                                       let {x303 = True};
                                       let {x304 = False};
                                       __________safe_III x302 x303 x304;
                                       (x4, x5) <- __________________stepEvalOO;
                                       let {x1 = Cons x4 x5};
                                       return (x0, x1)},
                                   do {let {x0 = Cabbage};
                                       let {x305 = True};
                                       let {x306 = False};
                                       let {x307 = False};
                                       _________safe_III x305 x306 x307;
                                       let {x308 = False};
                                       let {x309 = True};
                                       let {x310 = True};
                                       __________safe_III x308 x309 x310;
                                       (x4, x5) <- _______________stepEvalOO;
                                       let {x1 = Cons x4 x5};
                                       return (x0, x1)}]
__________________stepEvalOO = msum [do {(x0,
                                          x1) <- ________________step_EvalOO;
                                         return (x0, x1)}]
________________step_EvalOO = msum [do {let {x0 = Empty};
                                        let {x311 = True};
                                        let {x312 = True};
                                        let {x313 = False};
                                        _________safe_III x311 x312 x313;
                                        let {x314 = False};
                                        let {x315 = False};
                                        let {x316 = True};
                                        __________safe_III x314 x315 x316;
                                        (x2, x3) <- ___________________stepEvalOO;
                                        let {x1 = Cons x2 x3};
                                        return (x0, x1)},
                                    do {let {x0 = Goat};
                                        let {x317 = False};
                                        let {x318 = True};
                                        let {x319 = False};
                                        _________safe_III x317 x318 x319;
                                        let {x320 = True};
                                        let {x321 = False};
                                        let {x322 = True};
                                        __________safe_III x320 x321 x322;
                                        (x4, x5) <- _________________stepEvalOO;
                                        let {x1 = Cons x4 x5};
                                        return (x0, x1)},
                                    do {let {x0 = Wolf};
                                        let {x323 = True};
                                        let {x324 = False};
                                        let {x325 = False};
                                        _________safe_III x323 x324 x325;
                                        let {x326 = False};
                                        let {x327 = True};
                                        let {x328 = True};
                                        __________safe_III x326 x327 x328;
                                        (x4, x5) <- ___stepEvalOO;
                                        let {x1 = Cons x4 x5};
                                        return (x0, x1)}]
___________________stepEvalOO = msum [do {(x0,
                                           x1) <- _________________step_EvalOO;
                                          return (x0, x1)}]
_________________step_EvalOO = msum [do {let {x0 = Empty};
                                         let {x329 = False};
                                         let {x330 = False};
                                         let {x331 = True};
                                         _________safe_III x329 x330 x331;
                                         let {x332 = True};
                                         let {x333 = True};
                                         let {x334 = False};
                                         __________safe_III x332 x333 x334;
                                         (x2, x3) <- __________________stepEvalOO;
                                         let {x1 = Cons x2 x3};
                                         return (x0, x1)},
                                     do {let {x0 = Cabbage};
                                         let {x335 = False};
                                         let {x336 = False};
                                         let {x337 = False};
                                         _________safe_III x335 x336 x337;
                                         let {x338 = True};
                                         let {x339 = True};
                                         let {x340 = True};
                                         __________safe_III x338 x339 x340;
                                         let {x343 = False};
                                         let {x344 = False};
                                         let {x345 = False};
                                         let {x346 = False};
                                         let {x342 = Quad x343 x344 x345 x346};
                                         let {x348 = True};
                                         let {x349 = True};
                                         let {x350 = True};
                                         let {x351 = True};
                                         let {x347 = Quad x348 x349 x350 x351};
                                         let {x341 = Pair x342 x347};
                                         x1 <- _evalIO x341;
                                         return (x0, x1)}]
_______________stepEvalOO = msum [do {let {x274 = False};
                                      let {x275 = True};
                                      let {x276 = True};
                                      let {x277 = True};
                                      let {x278 = False};
                                      let {x279 = False};
                                      (x0, x273) <- _step_OOIIIIII x274 x275 x276 x277 x278 x279;
                                      (x2, x3) <- case x273 of
                                                  {Pair y2 y3 -> return (y2, y3); _ -> mzero};
                                      let {x280 = Pair x3 x2};
                                      x1 <- _evalIO x280;
                                      return (x0, x1)}]
_____________step_EvalOO = msum [do {let {x0 = Empty};
                                     let {x255 = True};
                                     let {x256 = True};
                                     let {x257 = False};
                                     _________safe_III x255 x256 x257;
                                     let {x258 = False};
                                     let {x259 = False};
                                     let {x260 = True};
                                     __________safe_III x258 x259 x260;
                                     (x2, x3) <- ____________________stepEvalOO;
                                     let {x1 = Cons x2 x3};
                                     return (x0, x1)},
                                 do {let {x0 = Goat};
                                     let {x261 = False};
                                     let {x262 = True};
                                     let {x263 = False};
                                     _________safe_III x261 x262 x263;
                                     let {x264 = True};
                                     let {x265 = False};
                                     let {x266 = True};
                                     __________safe_III x264 x265 x266;
                                     (x4, x5) <- ____________stepEvalOO;
                                     let {x1 = Cons x4 x5};
                                     return (x0, x1)},
                                 do {let {x0 = Wolf};
                                     let {x267 = True};
                                     let {x268 = False};
                                     let {x269 = False};
                                     _________safe_III x267 x268 x269;
                                     let {x270 = False};
                                     let {x271 = True};
                                     let {x272 = True};
                                     __________safe_III x270 x271 x272;
                                     (x4, x5) <- _______________stepEvalOO;
                                     let {x1 = Cons x4 x5};
                                     return (x0, x1)}]
____________stepEvalOO = msum [do {(x0,
                                    x1) <- ___________step_EvalOO;
                                   return (x0, x1)}]
___________step_EvalOO = msum [do {let {x0 = Empty};
                                   let {x225 = True};
                                   let {x226 = False};
                                   let {x227 = True};
                                   _________safe_III x225 x226 x227;
                                   let {x228 = False};
                                   let {x229 = True};
                                   let {x230 = False};
                                   __________safe_III x228 x229 x230;
                                   (x2, x3) <- _____________stepEvalOO;
                                   let {x1 = Cons x2 x3};
                                   return (x0, x1)},
                               do {let {x0 = Goat};
                                   let {x231 = False};
                                   let {x232 = False};
                                   let {x233 = True};
                                   _________safe_III x231 x232 x233;
                                   let {x234 = True};
                                   let {x235 = True};
                                   let {x236 = False};
                                   __________safe_III x234 x235 x236;
                                   (x4, x5) <- ______________stepEvalOO;
                                   let {x1 = Cons x4 x5};
                                   return (x0, x1)},
                               do {let {x0 = Cabbage};
                                   let {x237 = True};
                                   let {x238 = False};
                                   let {x239 = False};
                                   _________safe_III x237 x238 x239;
                                   let {x240 = False};
                                   let {x241 = True};
                                   let {x242 = True};
                                   __________safe_III x240 x241 x242;
                                   (x4, x5) <- ___stepEvalOO;
                                   let {x1 = Cons x4 x5};
                                   return (x0, x1)}]
_____________stepEvalOO = msum [do {(x0,
                                     x1) <- ____________step_EvalOO;
                                    return (x0, x1)}]
____________step_EvalOO = msum [do {let {x0 = Empty};
                                    let {x243 = False};
                                    let {x244 = True};
                                    let {x245 = False};
                                    _________safe_III x243 x244 x245;
                                    let {x246 = True};
                                    let {x247 = False};
                                    let {x248 = True};
                                    __________safe_III x246 x247 x248;
                                    (x2, x3) <- ____________stepEvalOO;
                                    let {x1 = Cons x2 x3};
                                    return (x0, x1)},
                                do {let {x0 = Wolf};
                                    let {x249 = False};
                                    let {x250 = False};
                                    let {x251 = False};
                                    _________safe_III x249 x250 x251;
                                    let {x252 = False};
                                    let {x253 = True};
                                    let {x254 = True};
                                    __________safe_III x252 x253 x254;
                                    (x4, x5) <- ______stepEvalOO;
                                    let {x1 = Cons x4 x5};
                                    return (x0, x1)}]
__________stepEvalOO = msum [do {(x0, x1) <- _________step_EvalOO;
                                 return (x0, x1)}]
_________step_EvalOO = msum [do {let {x0 = Empty};
                                 let {x201 = False};
                                 let {x202 = True};
                                 let {x203 = True};
                                 _________safe_III x201 x202 x203;
                                 let {x204 = False};
                                 let {x205 = False};
                                 let {x206 = False};
                                 __________safe_III x204 x205 x206;
                                 (x2, x3) <- ___________stepEvalOO;
                                 let {x1 = Cons x2 x3};
                                 return (x0, x1)},
                             do {let {x0 = Wolf};
                                 let {x207 = False};
                                 let {x208 = False};
                                 let {x209 = True};
                                 _________safe_III x207 x208 x209;
                                 let {x210 = False};
                                 let {x211 = True};
                                 let {x212 = False};
                                 __________safe_III x210 x211 x212;
                                 (x4, x5) <- ____stepEvalOO;
                                 let {x1 = Cons x4 x5};
                                 return (x0, x1)},
                             do {let {x0 = Cabbage};
                                 let {x213 = False};
                                 let {x214 = True};
                                 let {x215 = False};
                                 _________safe_III x213 x214 x215;
                                 let {x216 = False};
                                 let {x217 = False};
                                 let {x218 = True};
                                 __________safe_III x216 x217 x218;
                                 (x4, x5) <- _________stepEvalOO;
                                 let {x1 = Cons x4 x5};
                                 return (x0, x1)}]
___________stepEvalOO = msum [do {(x0,
                                   x1) <- __________step_EvalOO;
                                  return (x0, x1)}]
__________step_EvalOO = msum [do {let {x0 = Empty};
                                  let {x219 = False};
                                  let {x220 = False};
                                  let {x221 = False};
                                  _________safe_III x219 x220 x221;
                                  let {x222 = False};
                                  let {x223 = True};
                                  let {x224 = True};
                                  __________safe_III x222 x223 x224;
                                  (x2, x3) <- __________stepEvalOO;
                                  let {x1 = Cons x2 x3};
                                  return (x0, x1)}]
_________stepEvalOO = msum [do {let {x132 = False};
                                let {x133 = False};
                                let {x134 = True};
                                let {x135 = False};
                                let {x136 = True};
                                let {x137 = False};
                                (x0, x131) <- _step_OOIIIIII x132 x133 x134 x135 x136 x137;
                                (x2, x3) <- case x131 of
                                            {Pair y2 y3 -> return (y2, y3); _ -> mzero};
                                let {x138 = Pair x3 x2};
                                x1 <- _evalIO x138;
                                return (x0, x1)}]
______stepEvalOO = msum [do {(x0, x1) <- ______step_EvalOO;
                             return (x0, x1)}]
______step_EvalOO = msum [do {let {x0 = Empty};
                              let {x95 = False};
                              let {x96 = True};
                              let {x97 = True};
                              _________safe_III x95 x96 x97;
                              let {x98 = False};
                              let {x99 = False};
                              let {x100 = False};
                              __________safe_III x98 x99 x100;
                              (x2, x3) <- _______stepEvalOO;
                              let {x1 = Cons x2 x3};
                              return (x0, x1)},
                          do {let {x0 = Wolf};
                              let {x101 = False};
                              let {x102 = False};
                              let {x103 = True};
                              _________safe_III x101 x102 x103;
                              let {x104 = False};
                              let {x105 = True};
                              let {x106 = False};
                              __________safe_III x104 x105 x106;
                              (x4, x5) <- ________stepEvalOO;
                              let {x1 = Cons x4 x5};
                              return (x0, x1)},
                          do {let {x0 = Cabbage};
                              let {x107 = False};
                              let {x108 = True};
                              let {x109 = False};
                              _________safe_III x107 x108 x109;
                              let {x110 = False};
                              let {x111 = False};
                              let {x112 = True};
                              __________safe_III x110 x111 x112;
                              (x4, x5) <- _____stepEvalOO;
                              let {x1 = Cons x4 x5};
                              return (x0, x1)}]
________stepEvalOO = msum [do {(x0, x1) <- ________step_EvalOO;
                               return (x0, x1)}]
________step_EvalOO = msum [do {let {x0 = Empty};
                                let {x119 = False};
                                let {x120 = True};
                                let {x121 = False};
                                _________safe_III x119 x120 x121;
                                let {x122 = False};
                                let {x123 = False};
                                let {x124 = True};
                                __________safe_III x122 x123 x124;
                                (x2, x3) <- _________stepEvalOO;
                                let {x1 = Cons x2 x3};
                                return (x0, x1)},
                            do {let {x0 = Wolf};
                                let {x125 = False};
                                let {x126 = False};
                                let {x127 = False};
                                _________safe_III x125 x126 x127;
                                let {x128 = False};
                                let {x129 = True};
                                let {x130 = True};
                                __________safe_III x128 x129 x130;
                                (x4, x5) <- ______stepEvalOO;
                                let {x1 = Cons x4 x5};
                                return (x0, x1)}]
_______stepEvalOO = msum [do {(x0, x1) <- _______step_EvalOO;
                              return (x0, x1)}]
_______step_EvalOO = msum [do {let {x0 = Empty};
                               let {x113 = False};
                               let {x114 = False};
                               let {x115 = False};
                               _________safe_III x113 x114 x115;
                               let {x116 = False};
                               let {x117 = True};
                               let {x118 = True};
                               __________safe_III x116 x117 x118;
                               (x2, x3) <- ______stepEvalOO;
                               let {x1 = Cons x2 x3};
                               return (x0, x1)}]
_____stepEvalOO = msum [do {(x0, x1) <- _____step_EvalOO;
                            return (x0, x1)}]
_____step_EvalOO = msum [do {let {x0 = Empty};
                             let {x83 = False};
                             let {x84 = False};
                             let {x85 = True};
                             _________safe_III x83 x84 x85;
                             let {x86 = False};
                             let {x87 = True};
                             let {x88 = False};
                             __________safe_III x86 x87 x88;
                             (x2, x3) <- ____stepEvalOO;
                             let {x1 = Cons x2 x3};
                             return (x0, x1)},
                         do {let {x0 = Cabbage};
                             let {x89 = False};
                             let {x90 = False};
                             let {x91 = False};
                             _________safe_III x89 x90 x91;
                             let {x92 = False};
                             let {x93 = True};
                             let {x94 = True};
                             __________safe_III x92 x93 x94;
                             (x4, x5) <- ______stepEvalOO;
                             let {x1 = Cons x4 x5};
                             return (x0, x1)}]
____stepEvalOO = msum [do {(x0, x1) <- ____step_EvalOO;
                           return (x0, x1)}]
____step_EvalOO = msum [do {let {x0 = Empty};
                            let {x71 = False};
                            let {x72 = True};
                            let {x73 = False};
                            _________safe_III x71 x72 x73;
                            let {x74 = False};
                            let {x75 = False};
                            let {x76 = True};
                            __________safe_III x74 x75 x76;
                            (x2, x3) <- _____stepEvalOO;
                            let {x1 = Cons x2 x3};
                            return (x0, x1)},
                        do {let {x0 = Wolf};
                            let {x77 = False};
                            let {x78 = False};
                            let {x79 = False};
                            _________safe_III x77 x78 x79;
                            let {x80 = False};
                            let {x81 = True};
                            let {x82 = True};
                            __________safe_III x80 x81 x82;
                            (x4, x5) <- __________stepEvalOO;
                            let {x1 = Cons x4 x5};
                            return (x0, x1)}]
___stepEvalOO = msum [do {(x0, x1) <- ___step_EvalOO;
                          return (x0, x1)}]
___step_EvalOO = msum [do {let {x0 = Empty};
                           let {x53 = False};
                           let {x54 = True};
                           let {x55 = True};
                           _________safe_III x53 x54 x55;
                           let {x56 = True};
                           let {x57 = False};
                           let {x58 = False};
                           __________safe_III x56 x57 x58;
                           (x2, x3) <- __stepEvalOO;
                           let {x1 = Cons x2 x3};
                           return (x0, x1)},
                       do {let {x0 = Wolf};
                           let {x59 = False};
                           let {x60 = False};
                           let {x61 = True};
                           _________safe_III x59 x60 x61;
                           let {x62 = False};
                           let {x63 = True};
                           let {x64 = False};
                           __________safe_III x62 x63 x64;
                           (x4, x5) <- ____stepEvalOO;
                           let {x1 = Cons x4 x5};
                           return (x0, x1)},
                       do {let {x0 = Cabbage};
                           let {x65 = False};
                           let {x66 = True};
                           let {x67 = False};
                           _________safe_III x65 x66 x67;
                           let {x68 = True};
                           let {x69 = False};
                           let {x70 = True};
                           __________safe_III x68 x69 x70;
                           (x4, x5) <- ____________stepEvalOO;
                           let {x1 = Cons x4 x5};
                           return (x0, x1)}]
__stepEvalOO = msum [do {(x0, x1) <- __step_EvalOO;
                         return (x0, x1)}]
__step_EvalOO = msum [do {let {x0 = Empty};
                          let {x41 = True};
                          let {x42 = False};
                          let {x43 = False};
                          _________safe_III x41 x42 x43;
                          let {x44 = False};
                          let {x45 = True};
                          let {x46 = True};
                          __________safe_III x44 x45 x46;
                          (x2, x3) <- ___stepEvalOO;
                          let {x1 = Cons x2 x3};
                          return (x0, x1)},
                      do {let {x0 = Goat};
                          let {x47 = False};
                          let {x48 = False};
                          let {x49 = False};
                          _________safe_III x47 x48 x49;
                          let {x50 = True};
                          let {x51 = True};
                          let {x52 = True};
                          __________safe_III x50 x51 x52;
                          (x4, x5) <- _stepEvalOO;
                          let {x1 = Cons x4 x5};
                          return (x0, x1)}]
_evalIO x0 = msum [do {let {x1 = Nil};
                       let {x140 = False};
                       let {x141 = False};
                       let {x142 = False};
                       let {x143 = False};
                       let {x139 = Quad x140 x141 x142 x143};
                       let {x145 = True};
                       let {x146 = True};
                       let {x147 = True};
                       let {x148 = True};
                       let {x144 = Quad x145 x146 x147 x148};
                       (x149, x150) <- case x0 of
                                       {Pair y149 y150 -> return (y149, y150); _ -> mzero};
                       guard (x149 == x139);
                       guard (x150 == x144);
                       return x1},
                   do {(x2, x4) <- stepIOO x0;
                       x3 <- _evalIO x4;
                       let {x1 = Cons x2 x3};
                       return x1}]
_stepEvalOO = msum [do {(x0, x1) <- step_EvalOO; return (x0, x1)}]
_step_OOIIIIII x2 x3 x4 x5 x6 x7 = msum [do {_________safe_III x2 x3 x4;
                                             __________safe_III x5 x6 x7;
                                             let {x165 = False};
                                             let {x164 = Quad x2 x3 x4 x165};
                                             let {x167 = True};
                                             let {x166 = Quad x5 x6 x7 x167};
                                             let {x0 = Empty};
                                             let {x168 = x164};
                                             let {x169 = x166};
                                             let {x1 = Pair x168 x169};
                                             return (x0, x1)},
                                         do {let {x170 = False};
                                             _________safe_III x170 x3 x4;
                                             let {x0 = Goat};
                                             let {x172 = False};
                                             let {x173 = False};
                                             let {x171 = Quad x172 x3 x4 x173};
                                             let {x175 = True};
                                             let {x176 = True};
                                             let {x174 = Quad x175 x6 x7 x176};
                                             let {x179 = True};
                                             __________safe_III x179 x6 x7;
                                             guard (x2 == True);
                                             let {x177 = x171};
                                             let {x178 = x174};
                                             let {x1 = Pair x177 x178};
                                             return (x0, x1)},
                                         do {let {x180 = False};
                                             _________safe_III x2 x180 x4;
                                             let {x0 = Wolf};
                                             let {x182 = False};
                                             let {x183 = False};
                                             let {x181 = Quad x2 x182 x4 x183};
                                             let {x185 = True};
                                             let {x186 = True};
                                             let {x184 = Quad x6 x185 x7 x186};
                                             let {x189 = True};
                                             __________safe_III x6 x189 x7;
                                             guard (x3 == True);
                                             let {x187 = x181};
                                             let {x188 = x184};
                                             let {x1 = Pair x187 x188};
                                             return (x0, x1)},
                                         do {let {x190 = False};
                                             _________safe_III x2 x3 x190;
                                             let {x0 = Cabbage};
                                             let {x192 = False};
                                             let {x193 = False};
                                             let {x191 = Quad x2 x3 x192 x193};
                                             let {x195 = True};
                                             let {x196 = True};
                                             let {x194 = Quad x5 x6 x195 x196};
                                             let {x199 = True};
                                             __________safe_III x5 x6 x199;
                                             guard (x4 == True);
                                             let {x197 = x191};
                                             let {x198 = x194};
                                             let {x1 = Pair x197 x198};
                                             return (x0, x1)}]
stepIOO x0 = msum [do {let {x152 = True};
                       let {x154 = False};
                       (x155, x156) <- case x0 of
                                       {Pair y155 y156 -> return (y155, y156); _ -> mzero};
                       let {x151 = x155};
                       (x3, x4, x5) <- case x151 of
                                       {Quad y3 y4 y5 y152 -> do {guard (x152 == y152);
                                                                  return (y3, y4, y5)};
                                        _ -> mzero};
                       let {x153 = x156};
                       (x6, x7, x8) <- case x153 of
                                       {Quad y6 y7 y8 y154 -> do {guard (x154 == y154);
                                                                  return (y6, y7, y8)};
                                        _ -> mzero};
                       (x1, x2) <- _step_OOIIIIII x3 x4 x5 x6 x7 x8;
                       return (x1, x2)},
                   do {let {x158 = False};
                       let {x160 = True};
                       (x161, x162) <- case x0 of
                                       {Pair y161 y162 -> return (y161, y162); _ -> mzero};
                       let {x157 = x161};
                       (x6, x7, x8) <- case x157 of
                                       {Quad y6 y7 y8 y158 -> do {guard (x158 == y158);
                                                                  return (y6, y7, y8)};
                                        _ -> mzero};
                       let {x159 = x162};
                       (x3, x4, x5) <- case x159 of
                                       {Quad y3 y4 y5 y160 -> do {guard (x160 == y160);
                                                                  return (y3, y4, y5)};
                                        _ -> mzero};
                       (x1, x163) <- _step_OOIIIIII x3 x4 x5 x6 x7 x8;
                       (x10, x9) <- case x163 of
                                    {Pair y10 y9 -> return (y10, y9); _ -> mzero};
                       let {x2 = Pair x9 x10};
                       return (x1, x2)}]
stepEvalOO = msum [do {(x0, x1) <- _step_EvalOO; return (x0, x1)}]
_step_EvalOO = msum [do {let {x0 = Empty};
                         let {x35 = False};
                         let {x36 = False};
                         let {x37 = False};
                         _________safe_III x35 x36 x37;
                         let {x38 = True};
                         let {x39 = True};
                         let {x40 = True};
                         __________safe_III x38 x39 x40;
                         (x2, x3) <- _stepEvalOO;
                         let {x1 = Cons x2 x3};
                         return (x0, x1)}]