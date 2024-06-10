module Goat_online where

import Stream
import Control.Monad
import Term

evalI x0 = Immature $ msum [do {(x1, x2) <- case x0 of
                                 {Cons y1 y2 -> return (y1, y2); _ -> mzero};
                     step_EvalII x1 x2;
                     return ()}]
evalO = Immature $ msum [do {(x1, x2) <- step_EvalOO;
                  let {x0 = Cons x1 x2};
                  return x0}]
step_EvalII x0 x1 = Immature $ msum [do {let {x11 = Term.True};
                              let {x12 = Term.True};
                              let {x13 = Term.True};
                              _________safe_III x11 x12 x13;
                              let {x14 = Term.False};
                              let {x15 = Term.False};
                              let {x16 = Term.False};
                              __________safe_III x14 x15 x16;
                              (x2, x3) <- case x1 of
                                          {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                              stepEvalII x2 x3;
                              guard (x0 == Term.Empty);
                              return ()},
                          do {let {x17 = Term.False};
                              let {x18 = Term.True};
                              let {x19 = Term.True};
                              _________safe_III x17 x18 x19;
                              let {x20 = Term.True};
                              let {x21 = Term.False};
                              let {x22 = Term.False};
                              __________safe_III x20 x21 x22;
                              (x4, x5) <- case x1 of
                                          {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                              __stepEvalII x4 x5;
                              guard (x0 == Goat);
                              return ()},
                          do {let {x23 = Term.True};
                              let {x24 = Term.False};
                              let {x25 = Term.True};
                              _________safe_III x23 x24 x25;
                              let {x26 = Term.False};
                              let {x27 = Term.True};
                              let {x28 = Term.False};
                              __________safe_III x26 x27 x28;
                              (x4, x5) <- case x1 of
                                          {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                              ________________stepEvalII x4 x5;
                              guard (x0 == Wolf);
                              return ()},
                          do {let {x29 = Term.True};
                              let {x30 = Term.True};
                              let {x31 = Term.False};
                              _________safe_III x29 x30 x31;
                              let {x32 = Term.False};
                              let {x33 = Term.False};
                              let {x34 = Term.True};
                              __________safe_III x32 x33 x34;
                              (x4, x5) <- case x1 of
                                          {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                              ____________________stepEvalII x4 x5;
                              guard (x0 == Cabbage);
                              return ()}]
____________________stepEvalII x0 x1 = Immature $ msum [do {__________________step_EvalII x0 x1;
                                                 return ()}]
__________________step_EvalII x0 x1 = Immature $ msum [do {let {x352 = Term.False};
                                                let {x353 = Term.False};
                                                let {x354 = Term.True};
                                                _________safe_III x352 x353 x354;
                                                let {x355 = Term.True};
                                                let {x356 = Term.True};
                                                let {x357 = Term.False};
                                                __________safe_III x355 x356 x357;
                                                (x2, x3) <- case x1 of
                                                            {Cons y2 y3 -> return (y2, y3);
                                                             _ -> mzero};
                                                ______________stepEvalII x2 x3;
                                                guard (x0 == Term.Empty);
                                                return ()},
                                            do {let {x358 = Term.False};
                                                let {x359 = Term.False};
                                                let {x360 = Term.False};
                                                _________safe_III x358 x359 x360;
                                                let {x361 = Term.True};
                                                let {x362 = Term.True};
                                                let {x363 = Term.True};
                                                __________safe_III x361 x362 x363;
                                                (x4, x5) <- case x1 of
                                                            {Cons y4 y5 -> return (y4, y5);
                                                             _ -> mzero};
                                                _stepEvalII x4 x5;
                                                guard (x0 == Cabbage);
                                                return ()}]
________________stepEvalII x0 x1 = Immature $ msum [do {______________step_EvalII x0 x1;
                                             return ()}]
______________stepEvalII x0 x1 = Immature $ msum [do {_____________step_EvalII x0 x1;
                                           return ()}]
______________step_EvalII x0 x1 = Immature $ msum [do {let {x281 = Term.False};
                                            let {x282 = Term.True};
                                            let {x283 = Term.False};
                                            _________safe_III x281 x282 x283;
                                            let {x284 = Term.True};
                                            let {x285 = Term.False};
                                            let {x286 = Term.True};
                                            __________safe_III x284 x285 x286;
                                            (x2, x3) <- case x1 of
                                                        {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                            _________________stepEvalII x2 x3;
                                            guard (x0 == Term.Empty);
                                            return ()},
                                        do {let {x287 = Term.False};
                                            let {x288 = Term.False};
                                            let {x289 = Term.False};
                                            _________safe_III x287 x288 x289;
                                            let {x290 = Term.False};
                                            let {x291 = Term.True};
                                            let {x292 = Term.True};
                                            __________safe_III x290 x291 x292;
                                            (x4, x5) <- case x1 of
                                                        {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                            __________stepEvalII x4 x5;
                                            guard (x0 == Wolf);
                                            return ()}]
_________________stepEvalII x0 x1 = Immature $ msum [do {_______________step_EvalII x0 x1;
                                              return ()}]
_______________step_EvalII x0 x1 = Immature $ msum [do {let {x293 = Term.True};
                                             let {x294 = Term.False};
                                             let {x295 = Term.True};
                                             _________safe_III x293 x294 x295;
                                             let {x296 = Term.False};
                                             let {x297 = Term.True};
                                             let {x298 = Term.False};
                                             __________safe_III x296 x297 x298;
                                             (x2, x3) <- case x1 of
                                                         {Cons y2 y3 -> return (y2, y3);
                                                          _ -> mzero};
                                             ________________stepEvalII x2 x3;
                                             guard (x0 == Term.Empty);
                                             return ()},
                                         do {let {x299 = Term.False};
                                             let {x300 = Term.False};
                                             let {x301 = Term.True};
                                             _________safe_III x299 x300 x301;
                                             let {x302 = Term.True};
                                             let {x303 = Term.True};
                                             let {x304 = Term.False};
                                             __________safe_III x302 x303 x304;
                                             (x4, x5) <- case x1 of
                                                         {Cons y4 y5 -> return (y4, y5);
                                                          _ -> mzero};
                                             __________________stepEvalII x4 x5;
                                             guard (x0 == Goat);
                                             return ()},
                                         do {let {x305 = Term.True};
                                             let {x306 = Term.False};
                                             let {x307 = Term.False};
                                             _________safe_III x305 x306 x307;
                                             let {x308 = Term.False};
                                             let {x309 = Term.True};
                                             let {x310 = Term.True};
                                             __________safe_III x308 x309 x310;
                                             (x4, x5) <- case x1 of
                                                         {Cons y4 y5 -> return (y4, y5);
                                                          _ -> mzero};
                                             _______________stepEvalII x4 x5;
                                             guard (x0 == Cabbage);
                                             return ()}]
__________________stepEvalII x0 x1 = Immature $ msum [do {________________step_EvalII x0 x1;
                                               return ()}]
________________step_EvalII x0 x1 = Immature $ msum [do {let {x311 = Term.True};
                                              let {x312 = Term.True};
                                              let {x313 = Term.False};
                                              _________safe_III x311 x312 x313;
                                              let {x314 = Term.False};
                                              let {x315 = Term.False};
                                              let {x316 = Term.True};
                                              __________safe_III x314 x315 x316;
                                              (x2, x3) <- case x1 of
                                                          {Cons y2 y3 -> return (y2, y3);
                                                           _ -> mzero};
                                              ___________________stepEvalII x2 x3;
                                              guard (x0 == Term.Empty);
                                              return ()},
                                          do {let {x317 = Term.False};
                                              let {x318 = Term.True};
                                              let {x319 = Term.False};
                                              _________safe_III x317 x318 x319;
                                              let {x320 = Term.True};
                                              let {x321 = Term.False};
                                              let {x322 = Term.True};
                                              __________safe_III x320 x321 x322;
                                              (x4, x5) <- case x1 of
                                                          {Cons y4 y5 -> return (y4, y5);
                                                           _ -> mzero};
                                              _________________stepEvalII x4 x5;
                                              guard (x0 == Goat);
                                              return ()},
                                          do {let {x323 = Term.True};
                                              let {x324 = Term.False};
                                              let {x325 = Term.False};
                                              _________safe_III x323 x324 x325;
                                              let {x326 = Term.False};
                                              let {x327 = Term.True};
                                              let {x328 = Term.True};
                                              __________safe_III x326 x327 x328;
                                              (x4, x5) <- case x1 of
                                                          {Cons y4 y5 -> return (y4, y5);
                                                           _ -> mzero};
                                              ___stepEvalII x4 x5;
                                              guard (x0 == Wolf);
                                              return ()}]
___________________stepEvalII x0 x1 = Immature $ msum [do {_________________step_EvalII x0 x1;
                                                return ()}]
_________________step_EvalII x0 x1 = Immature $ msum [do {let {x329 = Term.False};
                                               let {x330 = Term.False};
                                               let {x331 = Term.True};
                                               _________safe_III x329 x330 x331;
                                               let {x332 = Term.True};
                                               let {x333 = Term.True};
                                               let {x334 = Term.False};
                                               __________safe_III x332 x333 x334;
                                               (x2, x3) <- case x1 of
                                                           {Cons y2 y3 -> return (y2, y3);
                                                            _ -> mzero};
                                               __________________stepEvalII x2 x3;
                                               guard (x0 == Term.Empty);
                                               return ()},
                                           do {let {x335 = Term.False};
                                               let {x336 = Term.False};
                                               let {x337 = Term.False};
                                               _________safe_III x335 x336 x337;
                                               let {x338 = Term.True};
                                               let {x339 = Term.True};
                                               let {x340 = Term.True};
                                               __________safe_III x338 x339 x340;
                                               let {x343 = Term.False};
                                               let {x344 = Term.False};
                                               let {x345 = Term.False};
                                               let {x346 = Term.False};
                                               let {x342 = Quad x343 x344 x345 x346};
                                               let {x348 = Term.True};
                                               let {x349 = Term.True};
                                               let {x350 = Term.True};
                                               let {x351 = Term.True};
                                               let {x347 = Quad x348 x349 x350 x351};
                                               let {x341 = Pair x342 x347};
                                               _evalII x341 x1;
                                               guard (x0 == Cabbage);
                                               return ()}]
_______________stepEvalII x0 x1 = Immature $ msum [do {let {x274 = Term.False};
                                            let {x275 = Term.True};
                                            let {x276 = Term.True};
                                            let {x277 = Term.True};
                                            let {x278 = Term.False};
                                            let {x279 = Term.False};
                                            x273 <- _step_IOIIIIII x0 x274 x275 x276 x277 x278 x279;
                                            (x2, x3) <- case x273 of
                                                        {Pair y2 y3 -> return (y2, y3); _ -> mzero};
                                            let {x280 = Pair x3 x2};
                                            _evalII x280 x1;
                                            return ()}]
_____________step_EvalII x0 x1 = Immature $ msum [do {let {x255 = Term.True};
                                           let {x256 = Term.True};
                                           let {x257 = Term.False};
                                           _________safe_III x255 x256 x257;
                                           let {x258 = Term.False};
                                           let {x259 = Term.False};
                                           let {x260 = Term.True};
                                           __________safe_III x258 x259 x260;
                                           (x2, x3) <- case x1 of
                                                       {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                           ____________________stepEvalII x2 x3;
                                           guard (x0 == Term.Empty);
                                           return ()},
                                       do {let {x261 = Term.False};
                                           let {x262 = Term.True};
                                           let {x263 = Term.False};
                                           _________safe_III x261 x262 x263;
                                           let {x264 = Term.True};
                                           let {x265 = Term.False};
                                           let {x266 = Term.True};
                                           __________safe_III x264 x265 x266;
                                           (x4, x5) <- case x1 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                           ____________stepEvalII x4 x5;
                                           guard (x0 == Goat);
                                           return ()},
                                       do {let {x267 = Term.True};
                                           let {x268 = Term.False};
                                           let {x269 = Term.False};
                                           _________safe_III x267 x268 x269;
                                           let {x270 = Term.False};
                                           let {x271 = Term.True};
                                           let {x272 = Term.True};
                                           __________safe_III x270 x271 x272;
                                           (x4, x5) <- case x1 of
                                                       {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                           _______________stepEvalII x4 x5;
                                           guard (x0 == Wolf);
                                           return ()}]
____________stepEvalII x0 x1 = Immature $ msum [do {___________step_EvalII x0 x1;
                                         return ()}]
___________step_EvalII x0 x1 = Immature $ msum [do {let {x225 = Term.True};
                                         let {x226 = Term.False};
                                         let {x227 = Term.True};
                                         _________safe_III x225 x226 x227;
                                         let {x228 = Term.False};
                                         let {x229 = Term.True};
                                         let {x230 = Term.False};
                                         __________safe_III x228 x229 x230;
                                         (x2, x3) <- case x1 of
                                                     {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                         _____________stepEvalII x2 x3;
                                         guard (x0 == Term.Empty);
                                         return ()},
                                     do {let {x231 = Term.False};
                                         let {x232 = Term.False};
                                         let {x233 = Term.True};
                                         _________safe_III x231 x232 x233;
                                         let {x234 = Term.True};
                                         let {x235 = Term.True};
                                         let {x236 = Term.False};
                                         __________safe_III x234 x235 x236;
                                         (x4, x5) <- case x1 of
                                                     {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                         ______________stepEvalII x4 x5;
                                         guard (x0 == Goat);
                                         return ()},
                                     do {let {x237 = Term.True};
                                         let {x238 = Term.False};
                                         let {x239 = Term.False};
                                         _________safe_III x237 x238 x239;
                                         let {x240 = Term.False};
                                         let {x241 = Term.True};
                                         let {x242 = Term.True};
                                         __________safe_III x240 x241 x242;
                                         (x4, x5) <- case x1 of
                                                     {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                         ___stepEvalII x4 x5;
                                         guard (x0 == Cabbage);
                                         return ()}]
_____________stepEvalII x0 x1 = Immature $ msum [do {____________step_EvalII x0 x1;
                                          return ()}]
____________step_EvalII x0 x1 = Immature $ msum [do {let {x243 = Term.False};
                                          let {x244 = Term.True};
                                          let {x245 = Term.False};
                                          _________safe_III x243 x244 x245;
                                          let {x246 = Term.True};
                                          let {x247 = Term.False};
                                          let {x248 = Term.True};
                                          __________safe_III x246 x247 x248;
                                          (x2, x3) <- case x1 of
                                                      {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                          ____________stepEvalII x2 x3;
                                          guard (x0 == Term.Empty);
                                          return ()},
                                      do {let {x249 = Term.False};
                                          let {x250 = Term.False};
                                          let {x251 = Term.False};
                                          _________safe_III x249 x250 x251;
                                          let {x252 = Term.False};
                                          let {x253 = Term.True};
                                          let {x254 = Term.True};
                                          __________safe_III x252 x253 x254;
                                          (x4, x5) <- case x1 of
                                                      {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                          ______stepEvalII x4 x5;
                                          guard (x0 == Wolf);
                                          return ()}]
__________safe_III x0 x1 x2 = Immature $ msum [do {let {x200 = x0};
                                        guard (x200 == x0);
                                        return ()}]
__________stepEvalII x0 x1 = Immature $ msum [do {_________step_EvalII x0 x1;
                                       return ()}]
_________safe_III x0 x1 x2 = Immature $ msum [do {guard (x0 == Term.False);
                                       return ()},
                                   do {guard (x2 == Term.True);
                                       guard (x1 == Term.True);
                                       guard (x0 == Term.True);
                                       return ()},
                                   do {guard (x2 == Term.False);
                                       guard (x1 == Term.False);
                                       guard (x0 == Term.True);
                                       return ()}]
_________step_EvalII x0 x1 = Immature $ msum [do {let {x201 = Term.False};
                                       let {x202 = Term.True};
                                       let {x203 = Term.True};
                                       _________safe_III x201 x202 x203;
                                       let {x204 = Term.False};
                                       let {x205 = Term.False};
                                       let {x206 = Term.False};
                                       __________safe_III x204 x205 x206;
                                       (x2, x3) <- case x1 of
                                                   {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                       ___________stepEvalII x2 x3;
                                       guard (x0 == Term.Empty);
                                       return ()},
                                   do {let {x207 = Term.False};
                                       let {x208 = Term.False};
                                       let {x209 = Term.True};
                                       _________safe_III x207 x208 x209;
                                       let {x210 = Term.False};
                                       let {x211 = Term.True};
                                       let {x212 = Term.False};
                                       __________safe_III x210 x211 x212;
                                       (x4, x5) <- case x1 of
                                                   {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                       ____stepEvalII x4 x5;
                                       guard (x0 == Wolf);
                                       return ()},
                                   do {let {x213 = Term.False};
                                       let {x214 = Term.True};
                                       let {x215 = Term.False};
                                       _________safe_III x213 x214 x215;
                                       let {x216 = Term.False};
                                       let {x217 = Term.False};
                                       let {x218 = Term.True};
                                       __________safe_III x216 x217 x218;
                                       (x4, x5) <- case x1 of
                                                   {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                       _________stepEvalII x4 x5;
                                       guard (x0 == Cabbage);
                                       return ()}]
___________stepEvalII x0 x1 = Immature $ msum [do {__________step_EvalII x0 x1;
                                        return ()}]
__________step_EvalII x0 x1 = Immature $ msum [do {let {x219 = Term.False};
                                        let {x220 = Term.False};
                                        let {x221 = Term.False};
                                        _________safe_III x219 x220 x221;
                                        let {x222 = Term.False};
                                        let {x223 = Term.True};
                                        let {x224 = Term.True};
                                        __________safe_III x222 x223 x224;
                                        (x2, x3) <- case x1 of
                                                    {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                        __________stepEvalII x2 x3;
                                        guard (x0 == Term.Empty);
                                        return ()}]
_________stepEvalII x0 x1 = Immature $ msum [do {let {x132 = Term.False};
                                      let {x133 = Term.False};
                                      let {x134 = Term.True};
                                      let {x135 = Term.False};
                                      let {x136 = Term.True};
                                      let {x137 = Term.False};
                                      x131 <- _step_IOIIIIII x0 x132 x133 x134 x135 x136 x137;
                                      (x2, x3) <- case x131 of
                                                  {Pair y2 y3 -> return (y2, y3); _ -> mzero};
                                      let {x138 = Pair x3 x2};
                                      _evalII x138 x1;
                                      return ()}]
______stepEvalII x0 x1 = Immature $ msum [do {______step_EvalII x0 x1;
                                   return ()}]
______step_EvalII x0 x1 = Immature $ msum [do {let {x95 = Term.False};
                                    let {x96 = Term.True};
                                    let {x97 = Term.True};
                                    _________safe_III x95 x96 x97;
                                    let {x98 = Term.False};
                                    let {x99 = Term.False};
                                    let {x100 = Term.False};
                                    __________safe_III x98 x99 x100;
                                    (x2, x3) <- case x1 of
                                                {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                    _______stepEvalII x2 x3;
                                    guard (x0 == Term.Empty);
                                    return ()},
                                do {let {x101 = Term.False};
                                    let {x102 = Term.False};
                                    let {x103 = Term.True};
                                    _________safe_III x101 x102 x103;
                                    let {x104 = Term.False};
                                    let {x105 = Term.True};
                                    let {x106 = Term.False};
                                    __________safe_III x104 x105 x106;
                                    (x4, x5) <- case x1 of
                                                {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                    ________stepEvalII x4 x5;
                                    guard (x0 == Wolf);
                                    return ()},
                                do {let {x107 = Term.False};
                                    let {x108 = Term.True};
                                    let {x109 = Term.False};
                                    _________safe_III x107 x108 x109;
                                    let {x110 = Term.False};
                                    let {x111 = Term.False};
                                    let {x112 = Term.True};
                                    __________safe_III x110 x111 x112;
                                    (x4, x5) <- case x1 of
                                                {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                    _____stepEvalII x4 x5;
                                    guard (x0 == Cabbage);
                                    return ()}]
________stepEvalII x0 x1 = Immature $ msum [do {________step_EvalII x0 x1;
                                     return ()}]
________step_EvalII x0 x1 = Immature $ msum [do {let {x119 = Term.False};
                                      let {x120 = Term.True};
                                      let {x121 = Term.False};
                                      _________safe_III x119 x120 x121;
                                      let {x122 = Term.False};
                                      let {x123 = Term.False};
                                      let {x124 = Term.True};
                                      __________safe_III x122 x123 x124;
                                      (x2, x3) <- case x1 of
                                                  {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                      _________stepEvalII x2 x3;
                                      guard (x0 == Term.Empty);
                                      return ()},
                                  do {let {x125 = Term.False};
                                      let {x126 = Term.False};
                                      let {x127 = Term.False};
                                      _________safe_III x125 x126 x127;
                                      let {x128 = Term.False};
                                      let {x129 = Term.True};
                                      let {x130 = Term.True};
                                      __________safe_III x128 x129 x130;
                                      (x4, x5) <- case x1 of
                                                  {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                      ______stepEvalII x4 x5;
                                      guard (x0 == Wolf);
                                      return ()}]
_______stepEvalII x0 x1 = Immature $ msum [do {_______step_EvalII x0 x1;
                                    return ()}]
_______step_EvalII x0 x1 = Immature $ msum [do {let {x113 = Term.False};
                                     let {x114 = Term.False};
                                     let {x115 = Term.False};
                                     _________safe_III x113 x114 x115;
                                     let {x116 = Term.False};
                                     let {x117 = Term.True};
                                     let {x118 = Term.True};
                                     __________safe_III x116 x117 x118;
                                     (x2, x3) <- case x1 of
                                                 {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                     ______stepEvalII x2 x3;
                                     guard (x0 == Term.Empty);
                                     return ()}]
_____stepEvalII x0 x1 = Immature $ msum [do {_____step_EvalII x0 x1;
                                  return ()}]
_____step_EvalII x0 x1 = Immature $ msum [do {let {x83 = Term.False};
                                   let {x84 = Term.False};
                                   let {x85 = Term.True};
                                   _________safe_III x83 x84 x85;
                                   let {x86 = Term.False};
                                   let {x87 = Term.True};
                                   let {x88 = Term.False};
                                   __________safe_III x86 x87 x88;
                                   (x2, x3) <- case x1 of
                                               {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                   ____stepEvalII x2 x3;
                                   guard (x0 == Term.Empty);
                                   return ()},
                               do {let {x89 = Term.False};
                                   let {x90 = Term.False};
                                   let {x91 = Term.False};
                                   _________safe_III x89 x90 x91;
                                   let {x92 = Term.False};
                                   let {x93 = Term.True};
                                   let {x94 = Term.True};
                                   __________safe_III x92 x93 x94;
                                   (x4, x5) <- case x1 of
                                               {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                   ______stepEvalII x4 x5;
                                   guard (x0 == Cabbage);
                                   return ()}]
____stepEvalII x0 x1 = Immature $ msum [do {____step_EvalII x0 x1; return ()}]
____step_EvalII x0 x1 = Immature $ msum [do {let {x71 = Term.False};
                                  let {x72 = Term.True};
                                  let {x73 = Term.False};
                                  _________safe_III x71 x72 x73;
                                  let {x74 = Term.False};
                                  let {x75 = Term.False};
                                  let {x76 = Term.True};
                                  __________safe_III x74 x75 x76;
                                  (x2, x3) <- case x1 of
                                              {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                  _____stepEvalII x2 x3;
                                  guard (x0 == Term.Empty);
                                  return ()},
                              do {let {x77 = Term.False};
                                  let {x78 = Term.False};
                                  let {x79 = Term.False};
                                  _________safe_III x77 x78 x79;
                                  let {x80 = Term.False};
                                  let {x81 = Term.True};
                                  let {x82 = Term.True};
                                  __________safe_III x80 x81 x82;
                                  (x4, x5) <- case x1 of
                                              {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                  __________stepEvalII x4 x5;
                                  guard (x0 == Wolf);
                                  return ()}]
___stepEvalII x0 x1 = Immature $ msum [do {___step_EvalII x0 x1; return ()}]
___step_EvalII x0 x1 = Immature $ msum [do {let {x53 = Term.False};
                                 let {x54 = Term.True};
                                 let {x55 = Term.True};
                                 _________safe_III x53 x54 x55;
                                 let {x56 = Term.True};
                                 let {x57 = Term.False};
                                 let {x58 = Term.False};
                                 __________safe_III x56 x57 x58;
                                 (x2, x3) <- case x1 of
                                             {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                 __stepEvalII x2 x3;
                                 guard (x0 == Term.Empty);
                                 return ()},
                             do {let {x59 = Term.False};
                                 let {x60 = Term.False};
                                 let {x61 = Term.True};
                                 _________safe_III x59 x60 x61;
                                 let {x62 = Term.False};
                                 let {x63 = Term.True};
                                 let {x64 = Term.False};
                                 __________safe_III x62 x63 x64;
                                 (x4, x5) <- case x1 of
                                             {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                 ____stepEvalII x4 x5;
                                 guard (x0 == Wolf);
                                 return ()},
                             do {let {x65 = Term.False};
                                 let {x66 = Term.True};
                                 let {x67 = Term.False};
                                 _________safe_III x65 x66 x67;
                                 let {x68 = Term.True};
                                 let {x69 = Term.False};
                                 let {x70 = Term.True};
                                 __________safe_III x68 x69 x70;
                                 (x4, x5) <- case x1 of
                                             {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                 ____________stepEvalII x4 x5;
                                 guard (x0 == Cabbage);
                                 return ()}]
__stepEvalII x0 x1 = Immature $ msum [do {__step_EvalII x0 x1; return ()}]
__step_EvalII x0 x1 = Immature $ msum [do {let {x41 = Term.True};
                                let {x42 = Term.False};
                                let {x43 = Term.False};
                                _________safe_III x41 x42 x43;
                                let {x44 = Term.False};
                                let {x45 = Term.True};
                                let {x46 = Term.True};
                                __________safe_III x44 x45 x46;
                                (x2, x3) <- case x1 of
                                            {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                ___stepEvalII x2 x3;
                                guard (x0 == Term.Empty);
                                return ()},
                            do {let {x47 = Term.False};
                                let {x48 = Term.False};
                                let {x49 = Term.False};
                                _________safe_III x47 x48 x49;
                                let {x50 = Term.True};
                                let {x51 = Term.True};
                                let {x52 = Term.True};
                                __________safe_III x50 x51 x52;
                                (x4, x5) <- case x1 of
                                            {Cons y4 y5 -> return (y4, y5); _ -> mzero};
                                _stepEvalII x4 x5;
                                guard (x0 == Goat);
                                return ()}]
_evalII x0 x1 = Immature $ msum [do {let {x140 = Term.False};
                          let {x141 = Term.False};
                          let {x142 = Term.False};
                          let {x143 = Term.False};
                          let {x139 = Quad x140 x141 x142 x143};
                          let {x145 = Term.True};
                          let {x146 = Term.True};
                          let {x147 = Term.True};
                          let {x148 = Term.True};
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
_stepEvalII x0 x1 = Immature $ msum [do {step_EvalII x0 x1; return ()}]
_step_IOIIIIII x0 x2 x3 x4 x5 x6 x7 = Immature $ msum [do {_________safe_III x2 x3 x4;
                                                __________safe_III x5 x6 x7;
                                                let {x165 = Term.False};
                                                let {x164 = Quad x2 x3 x4 x165};
                                                let {x167 = Term.True};
                                                let {x166 = Quad x5 x6 x7 x167};
                                                guard (x0 == Term.Empty);
                                                let {x168 = x164};
                                                let {x169 = x166};
                                                let {x1 = Pair x168 x169};
                                                return x1},
                                            do {let {x170 = Term.False};
                                                _________safe_III x170 x3 x4;
                                                let {x172 = Term.False};
                                                let {x173 = Term.False};
                                                let {x171 = Quad x172 x3 x4 x173};
                                                let {x175 = Term.True};
                                                let {x176 = Term.True};
                                                let {x174 = Quad x175 x6 x7 x176};
                                                let {x179 = Term.True};
                                                __________safe_III x179 x6 x7;
                                                guard (x2 == Term.True);
                                                guard (x0 == Goat);
                                                let {x177 = x171};
                                                let {x178 = x174};
                                                let {x1 = Pair x177 x178};
                                                return x1},
                                            do {let {x180 = Term.False};
                                                _________safe_III x2 x180 x4;
                                                let {x182 = Term.False};
                                                let {x183 = Term.False};
                                                let {x181 = Quad x2 x182 x4 x183};
                                                let {x185 = Term.True};
                                                let {x186 = Term.True};
                                                let {x184 = Quad x6 x185 x7 x186};
                                                let {x189 = Term.True};
                                                __________safe_III x6 x189 x7;
                                                guard (x3 == Term.True);
                                                guard (x0 == Wolf);
                                                let {x187 = x181};
                                                let {x188 = x184};
                                                let {x1 = Pair x187 x188};
                                                return x1},
                                            do {let {x190 = Term.False};
                                                _________safe_III x2 x3 x190;
                                                let {x192 = Term.False};
                                                let {x193 = Term.False};
                                                let {x191 = Quad x2 x3 x192 x193};
                                                let {x195 = Term.True};
                                                let {x196 = Term.True};
                                                let {x194 = Quad x5 x6 x195 x196};
                                                let {x199 = Term.True};
                                                __________safe_III x5 x6 x199;
                                                guard (x4 == Term.True);
                                                guard (x0 == Cabbage);
                                                let {x197 = x191};
                                                let {x198 = x194};
                                                let {x1 = Pair x197 x198};
                                                return x1}]
stepIIO x0 x1 = Immature $ msum [do {let {x152 = Term.True};
                          let {x154 = Term.False};
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
                      do {let {x158 = Term.False};
                          let {x160 = Term.True};
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
stepEvalII x0 x1 = Immature $ msum [do {_step_EvalII x0 x1; return ()}]
_step_EvalII x0 x1 = Immature $ msum [do {let {x35 = Term.False};
                               let {x36 = Term.False};
                               let {x37 = Term.False};
                               _________safe_III x35 x36 x37;
                               let {x38 = Term.True};
                               let {x39 = Term.True};
                               let {x40 = Term.True};
                               __________safe_III x38 x39 x40;
                               (x2, x3) <- case x1 of
                                           {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                               _stepEvalII x2 x3;
                               guard (x0 == Term.Empty);
                               return ()}]
step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                        let {x11 = Term.True};
                        let {x12 = Term.True};
                        let {x13 = Term.True};
                        _________safe_III x11 x12 x13;
                        let {x14 = Term.False};
                        let {x15 = Term.False};
                        let {x16 = Term.False};
                        __________safe_III x14 x15 x16;
                        (x2, x3) <- stepEvalOO;
                        let {x1 = Cons x2 x3};
                        return (x0, x1)},
                    do {let {x0 = Goat};
                        let {x17 = Term.False};
                        let {x18 = Term.True};
                        let {x19 = Term.True};
                        _________safe_III x17 x18 x19;
                        let {x20 = Term.True};
                        let {x21 = Term.False};
                        let {x22 = Term.False};
                        __________safe_III x20 x21 x22;
                        (x4, x5) <- __stepEvalOO;
                        let {x1 = Cons x4 x5};
                        return (x0, x1)},
                    do {let {x0 = Wolf};
                        let {x23 = Term.True};
                        let {x24 = Term.False};
                        let {x25 = Term.True};
                        _________safe_III x23 x24 x25;
                        let {x26 = Term.False};
                        let {x27 = Term.True};
                        let {x28 = Term.False};
                        __________safe_III x26 x27 x28;
                        (x4, x5) <- ________________stepEvalOO;
                        let {x1 = Cons x4 x5};
                        return (x0, x1)},
                    do {let {x0 = Cabbage};
                        let {x29 = Term.True};
                        let {x30 = Term.True};
                        let {x31 = Term.False};
                        _________safe_III x29 x30 x31;
                        let {x32 = Term.False};
                        let {x33 = Term.False};
                        let {x34 = Term.True};
                        __________safe_III x32 x33 x34;
                        (x4, x5) <- ____________________stepEvalOO;
                        let {x1 = Cons x4 x5};
                        return (x0, x1)}]
____________________stepEvalOO = Immature $ msum [do {(x0,
                                            x1) <- __________________step_EvalOO;
                                           return (x0, x1)}]
__________________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                          let {x352 = Term.False};
                                          let {x353 = Term.False};
                                          let {x354 = Term.True};
                                          _________safe_III x352 x353 x354;
                                          let {x355 = Term.True};
                                          let {x356 = Term.True};
                                          let {x357 = Term.False};
                                          __________safe_III x355 x356 x357;
                                          (x2, x3) <- ______________stepEvalOO;
                                          let {x1 = Cons x2 x3};
                                          return (x0, x1)},
                                      do {let {x0 = Cabbage};
                                          let {x358 = Term.False};
                                          let {x359 = Term.False};
                                          let {x360 = Term.False};
                                          _________safe_III x358 x359 x360;
                                          let {x361 = Term.True};
                                          let {x362 = Term.True};
                                          let {x363 = Term.True};
                                          __________safe_III x361 x362 x363;
                                          (x4, x5) <- _stepEvalOO;
                                          let {x1 = Cons x4 x5};
                                          return (x0, x1)}]
________________stepEvalOO = Immature $ msum [do {(x0,
                                        x1) <- ______________step_EvalOO;
                                       return (x0, x1)}]
______________stepEvalOO = Immature $ msum [do {(x0,
                                      x1) <- _____________step_EvalOO;
                                     return (x0, x1)}]
______________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                      let {x281 = Term.False};
                                      let {x282 = Term.True};
                                      let {x283 = Term.False};
                                      _________safe_III x281 x282 x283;
                                      let {x284 = Term.True};
                                      let {x285 = Term.False};
                                      let {x286 = Term.True};
                                      __________safe_III x284 x285 x286;
                                      (x2, x3) <- _________________stepEvalOO;
                                      let {x1 = Cons x2 x3};
                                      return (x0, x1)},
                                  do {let {x0 = Wolf};
                                      let {x287 = Term.False};
                                      let {x288 = Term.False};
                                      let {x289 = Term.False};
                                      _________safe_III x287 x288 x289;
                                      let {x290 = Term.False};
                                      let {x291 = Term.True};
                                      let {x292 = Term.True};
                                      __________safe_III x290 x291 x292;
                                      (x4, x5) <- __________stepEvalOO;
                                      let {x1 = Cons x4 x5};
                                      return (x0, x1)}]
_________________stepEvalOO = Immature $ msum [do {(x0,
                                         x1) <- _______________step_EvalOO;
                                        return (x0, x1)}]
_______________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                       let {x293 = Term.True};
                                       let {x294 = Term.False};
                                       let {x295 = Term.True};
                                       _________safe_III x293 x294 x295;
                                       let {x296 = Term.False};
                                       let {x297 = Term.True};
                                       let {x298 = Term.False};
                                       __________safe_III x296 x297 x298;
                                       (x2, x3) <- ________________stepEvalOO;
                                       let {x1 = Cons x2 x3};
                                       return (x0, x1)},
                                   do {let {x0 = Goat};
                                       let {x299 = Term.False};
                                       let {x300 = Term.False};
                                       let {x301 = Term.True};
                                       _________safe_III x299 x300 x301;
                                       let {x302 = Term.True};
                                       let {x303 = Term.True};
                                       let {x304 = Term.False};
                                       __________safe_III x302 x303 x304;
                                       (x4, x5) <- __________________stepEvalOO;
                                       let {x1 = Cons x4 x5};
                                       return (x0, x1)},
                                   do {let {x0 = Cabbage};
                                       let {x305 = Term.True};
                                       let {x306 = Term.False};
                                       let {x307 = Term.False};
                                       _________safe_III x305 x306 x307;
                                       let {x308 = Term.False};
                                       let {x309 = Term.True};
                                       let {x310 = Term.True};
                                       __________safe_III x308 x309 x310;
                                       (x4, x5) <- _______________stepEvalOO;
                                       let {x1 = Cons x4 x5};
                                       return (x0, x1)}]
__________________stepEvalOO = Immature $ msum [do {(x0,
                                          x1) <- ________________step_EvalOO;
                                         return (x0, x1)}]
________________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                        let {x311 = Term.True};
                                        let {x312 = Term.True};
                                        let {x313 = Term.False};
                                        _________safe_III x311 x312 x313;
                                        let {x314 = Term.False};
                                        let {x315 = Term.False};
                                        let {x316 = Term.True};
                                        __________safe_III x314 x315 x316;
                                        (x2, x3) <- ___________________stepEvalOO;
                                        let {x1 = Cons x2 x3};
                                        return (x0, x1)},
                                    do {let {x0 = Goat};
                                        let {x317 = Term.False};
                                        let {x318 = Term.True};
                                        let {x319 = Term.False};
                                        _________safe_III x317 x318 x319;
                                        let {x320 = Term.True};
                                        let {x321 = Term.False};
                                        let {x322 = Term.True};
                                        __________safe_III x320 x321 x322;
                                        (x4, x5) <- _________________stepEvalOO;
                                        let {x1 = Cons x4 x5};
                                        return (x0, x1)},
                                    do {let {x0 = Wolf};
                                        let {x323 = Term.True};
                                        let {x324 = Term.False};
                                        let {x325 = Term.False};
                                        _________safe_III x323 x324 x325;
                                        let {x326 = Term.False};
                                        let {x327 = Term.True};
                                        let {x328 = Term.True};
                                        __________safe_III x326 x327 x328;
                                        (x4, x5) <- ___stepEvalOO;
                                        let {x1 = Cons x4 x5};
                                        return (x0, x1)}]
___________________stepEvalOO = Immature $ msum [do {(x0,
                                           x1) <- _________________step_EvalOO;
                                          return (x0, x1)}]
_________________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                         let {x329 = Term.False};
                                         let {x330 = Term.False};
                                         let {x331 = Term.True};
                                         _________safe_III x329 x330 x331;
                                         let {x332 = Term.True};
                                         let {x333 = Term.True};
                                         let {x334 = Term.False};
                                         __________safe_III x332 x333 x334;
                                         (x2, x3) <- __________________stepEvalOO;
                                         let {x1 = Cons x2 x3};
                                         return (x0, x1)},
                                     do {let {x0 = Cabbage};
                                         let {x335 = Term.False};
                                         let {x336 = Term.False};
                                         let {x337 = Term.False};
                                         _________safe_III x335 x336 x337;
                                         let {x338 = Term.True};
                                         let {x339 = Term.True};
                                         let {x340 = Term.True};
                                         __________safe_III x338 x339 x340;
                                         let {x343 = Term.False};
                                         let {x344 = Term.False};
                                         let {x345 = Term.False};
                                         let {x346 = Term.False};
                                         let {x342 = Quad x343 x344 x345 x346};
                                         let {x348 = Term.True};
                                         let {x349 = Term.True};
                                         let {x350 = Term.True};
                                         let {x351 = Term.True};
                                         let {x347 = Quad x348 x349 x350 x351};
                                         let {x341 = Pair x342 x347};
                                         x1 <- _evalIO x341;
                                         return (x0, x1)}]
_______________stepEvalOO = Immature $ msum [do {let {x274 = Term.False};
                                      let {x275 = Term.True};
                                      let {x276 = Term.True};
                                      let {x277 = Term.True};
                                      let {x278 = Term.False};
                                      let {x279 = Term.False};
                                      (x0, x273) <- _step_OOIIIIII x274 x275 x276 x277 x278 x279;
                                      (x2, x3) <- case x273 of
                                                  {Pair y2 y3 -> return (y2, y3); _ -> mzero};
                                      let {x280 = Pair x3 x2};
                                      x1 <- _evalIO x280;
                                      return (x0, x1)}]
_____________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                     let {x255 = Term.True};
                                     let {x256 = Term.True};
                                     let {x257 = Term.False};
                                     _________safe_III x255 x256 x257;
                                     let {x258 = Term.False};
                                     let {x259 = Term.False};
                                     let {x260 = Term.True};
                                     __________safe_III x258 x259 x260;
                                     (x2, x3) <- ____________________stepEvalOO;
                                     let {x1 = Cons x2 x3};
                                     return (x0, x1)},
                                 do {let {x0 = Goat};
                                     let {x261 = Term.False};
                                     let {x262 = Term.True};
                                     let {x263 = Term.False};
                                     _________safe_III x261 x262 x263;
                                     let {x264 = Term.True};
                                     let {x265 = Term.False};
                                     let {x266 = Term.True};
                                     __________safe_III x264 x265 x266;
                                     (x4, x5) <- ____________stepEvalOO;
                                     let {x1 = Cons x4 x5};
                                     return (x0, x1)},
                                 do {let {x0 = Wolf};
                                     let {x267 = Term.True};
                                     let {x268 = Term.False};
                                     let {x269 = Term.False};
                                     _________safe_III x267 x268 x269;
                                     let {x270 = Term.False};
                                     let {x271 = Term.True};
                                     let {x272 = Term.True};
                                     __________safe_III x270 x271 x272;
                                     (x4, x5) <- _______________stepEvalOO;
                                     let {x1 = Cons x4 x5};
                                     return (x0, x1)}]
____________stepEvalOO = Immature $ msum [do {(x0,
                                    x1) <- ___________step_EvalOO;
                                   return (x0, x1)}]
___________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                   let {x225 = Term.True};
                                   let {x226 = Term.False};
                                   let {x227 = Term.True};
                                   _________safe_III x225 x226 x227;
                                   let {x228 = Term.False};
                                   let {x229 = Term.True};
                                   let {x230 = Term.False};
                                   __________safe_III x228 x229 x230;
                                   (x2, x3) <- _____________stepEvalOO;
                                   let {x1 = Cons x2 x3};
                                   return (x0, x1)},
                               do {let {x0 = Goat};
                                   let {x231 = Term.False};
                                   let {x232 = Term.False};
                                   let {x233 = Term.True};
                                   _________safe_III x231 x232 x233;
                                   let {x234 = Term.True};
                                   let {x235 = Term.True};
                                   let {x236 = Term.False};
                                   __________safe_III x234 x235 x236;
                                   (x4, x5) <- ______________stepEvalOO;
                                   let {x1 = Cons x4 x5};
                                   return (x0, x1)},
                               do {let {x0 = Cabbage};
                                   let {x237 = Term.True};
                                   let {x238 = Term.False};
                                   let {x239 = Term.False};
                                   _________safe_III x237 x238 x239;
                                   let {x240 = Term.False};
                                   let {x241 = Term.True};
                                   let {x242 = Term.True};
                                   __________safe_III x240 x241 x242;
                                   (x4, x5) <- ___stepEvalOO;
                                   let {x1 = Cons x4 x5};
                                   return (x0, x1)}]
_____________stepEvalOO = Immature $ msum [do {(x0,
                                     x1) <- ____________step_EvalOO;
                                    return (x0, x1)}]
____________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                    let {x243 = Term.False};
                                    let {x244 = Term.True};
                                    let {x245 = Term.False};
                                    _________safe_III x243 x244 x245;
                                    let {x246 = Term.True};
                                    let {x247 = Term.False};
                                    let {x248 = Term.True};
                                    __________safe_III x246 x247 x248;
                                    (x2, x3) <- ____________stepEvalOO;
                                    let {x1 = Cons x2 x3};
                                    return (x0, x1)},
                                do {let {x0 = Wolf};
                                    let {x249 = Term.False};
                                    let {x250 = Term.False};
                                    let {x251 = Term.False};
                                    _________safe_III x249 x250 x251;
                                    let {x252 = Term.False};
                                    let {x253 = Term.True};
                                    let {x254 = Term.True};
                                    __________safe_III x252 x253 x254;
                                    (x4, x5) <- ______stepEvalOO;
                                    let {x1 = Cons x4 x5};
                                    return (x0, x1)}]
__________stepEvalOO = Immature $ msum [do {(x0, x1) <- _________step_EvalOO;
                                 return (x0, x1)}]
_________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                 let {x201 = Term.False};
                                 let {x202 = Term.True};
                                 let {x203 = Term.True};
                                 _________safe_III x201 x202 x203;
                                 let {x204 = Term.False};
                                 let {x205 = Term.False};
                                 let {x206 = Term.False};
                                 __________safe_III x204 x205 x206;
                                 (x2, x3) <- ___________stepEvalOO;
                                 let {x1 = Cons x2 x3};
                                 return (x0, x1)},
                             do {let {x0 = Wolf};
                                 let {x207 = Term.False};
                                 let {x208 = Term.False};
                                 let {x209 = Term.True};
                                 _________safe_III x207 x208 x209;
                                 let {x210 = Term.False};
                                 let {x211 = Term.True};
                                 let {x212 = Term.False};
                                 __________safe_III x210 x211 x212;
                                 (x4, x5) <- ____stepEvalOO;
                                 let {x1 = Cons x4 x5};
                                 return (x0, x1)},
                             do {let {x0 = Cabbage};
                                 let {x213 = Term.False};
                                 let {x214 = Term.True};
                                 let {x215 = Term.False};
                                 _________safe_III x213 x214 x215;
                                 let {x216 = Term.False};
                                 let {x217 = Term.False};
                                 let {x218 = Term.True};
                                 __________safe_III x216 x217 x218;
                                 (x4, x5) <- _________stepEvalOO;
                                 let {x1 = Cons x4 x5};
                                 return (x0, x1)}]
___________stepEvalOO = Immature $ msum [do {(x0,
                                   x1) <- __________step_EvalOO;
                                  return (x0, x1)}]
__________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                  let {x219 = Term.False};
                                  let {x220 = Term.False};
                                  let {x221 = Term.False};
                                  _________safe_III x219 x220 x221;
                                  let {x222 = Term.False};
                                  let {x223 = Term.True};
                                  let {x224 = Term.True};
                                  __________safe_III x222 x223 x224;
                                  (x2, x3) <- __________stepEvalOO;
                                  let {x1 = Cons x2 x3};
                                  return (x0, x1)}]
_________stepEvalOO = Immature $ msum [do {let {x132 = Term.False};
                                let {x133 = Term.False};
                                let {x134 = Term.True};
                                let {x135 = Term.False};
                                let {x136 = Term.True};
                                let {x137 = Term.False};
                                (x0, x131) <- _step_OOIIIIII x132 x133 x134 x135 x136 x137;
                                (x2, x3) <- case x131 of
                                            {Pair y2 y3 -> return (y2, y3); _ -> mzero};
                                let {x138 = Pair x3 x2};
                                x1 <- _evalIO x138;
                                return (x0, x1)}]
______stepEvalOO = Immature $ msum [do {(x0, x1) <- ______step_EvalOO;
                             return (x0, x1)}]
______step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                              let {x95 = Term.False};
                              let {x96 = Term.True};
                              let {x97 = Term.True};
                              _________safe_III x95 x96 x97;
                              let {x98 = Term.False};
                              let {x99 = Term.False};
                              let {x100 = Term.False};
                              __________safe_III x98 x99 x100;
                              (x2, x3) <- _______stepEvalOO;
                              let {x1 = Cons x2 x3};
                              return (x0, x1)},
                          do {let {x0 = Wolf};
                              let {x101 = Term.False};
                              let {x102 = Term.False};
                              let {x103 = Term.True};
                              _________safe_III x101 x102 x103;
                              let {x104 = Term.False};
                              let {x105 = Term.True};
                              let {x106 = Term.False};
                              __________safe_III x104 x105 x106;
                              (x4, x5) <- ________stepEvalOO;
                              let {x1 = Cons x4 x5};
                              return (x0, x1)},
                          do {let {x0 = Cabbage};
                              let {x107 = Term.False};
                              let {x108 = Term.True};
                              let {x109 = Term.False};
                              _________safe_III x107 x108 x109;
                              let {x110 = Term.False};
                              let {x111 = Term.False};
                              let {x112 = Term.True};
                              __________safe_III x110 x111 x112;
                              (x4, x5) <- _____stepEvalOO;
                              let {x1 = Cons x4 x5};
                              return (x0, x1)}]
________stepEvalOO = Immature $ msum [do {(x0, x1) <- ________step_EvalOO;
                               return (x0, x1)}]
________step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                                let {x119 = Term.False};
                                let {x120 = Term.True};
                                let {x121 = Term.False};
                                _________safe_III x119 x120 x121;
                                let {x122 = Term.False};
                                let {x123 = Term.False};
                                let {x124 = Term.True};
                                __________safe_III x122 x123 x124;
                                (x2, x3) <- _________stepEvalOO;
                                let {x1 = Cons x2 x3};
                                return (x0, x1)},
                            do {let {x0 = Wolf};
                                let {x125 = Term.False};
                                let {x126 = Term.False};
                                let {x127 = Term.False};
                                _________safe_III x125 x126 x127;
                                let {x128 = Term.False};
                                let {x129 = Term.True};
                                let {x130 = Term.True};
                                __________safe_III x128 x129 x130;
                                (x4, x5) <- ______stepEvalOO;
                                let {x1 = Cons x4 x5};
                                return (x0, x1)}]
_______stepEvalOO = Immature $ msum [do {(x0, x1) <- _______step_EvalOO;
                              return (x0, x1)}]
_______step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                               let {x113 = Term.False};
                               let {x114 = Term.False};
                               let {x115 = Term.False};
                               _________safe_III x113 x114 x115;
                               let {x116 = Term.False};
                               let {x117 = Term.True};
                               let {x118 = Term.True};
                               __________safe_III x116 x117 x118;
                               (x2, x3) <- ______stepEvalOO;
                               let {x1 = Cons x2 x3};
                               return (x0, x1)}]
_____stepEvalOO = Immature $ msum [do {(x0, x1) <- _____step_EvalOO;
                            return (x0, x1)}]
_____step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                             let {x83 = Term.False};
                             let {x84 = Term.False};
                             let {x85 = Term.True};
                             _________safe_III x83 x84 x85;
                             let {x86 = Term.False};
                             let {x87 = Term.True};
                             let {x88 = Term.False};
                             __________safe_III x86 x87 x88;
                             (x2, x3) <- ____stepEvalOO;
                             let {x1 = Cons x2 x3};
                             return (x0, x1)},
                         do {let {x0 = Cabbage};
                             let {x89 = Term.False};
                             let {x90 = Term.False};
                             let {x91 = Term.False};
                             _________safe_III x89 x90 x91;
                             let {x92 = Term.False};
                             let {x93 = Term.True};
                             let {x94 = Term.True};
                             __________safe_III x92 x93 x94;
                             (x4, x5) <- ______stepEvalOO;
                             let {x1 = Cons x4 x5};
                             return (x0, x1)}]
____stepEvalOO = Immature $ msum [do {(x0, x1) <- ____step_EvalOO;
                           return (x0, x1)}]
____step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                            let {x71 = Term.False};
                            let {x72 = Term.True};
                            let {x73 = Term.False};
                            _________safe_III x71 x72 x73;
                            let {x74 = Term.False};
                            let {x75 = Term.False};
                            let {x76 = Term.True};
                            __________safe_III x74 x75 x76;
                            (x2, x3) <- _____stepEvalOO;
                            let {x1 = Cons x2 x3};
                            return (x0, x1)},
                        do {let {x0 = Wolf};
                            let {x77 = Term.False};
                            let {x78 = Term.False};
                            let {x79 = Term.False};
                            _________safe_III x77 x78 x79;
                            let {x80 = Term.False};
                            let {x81 = Term.True};
                            let {x82 = Term.True};
                            __________safe_III x80 x81 x82;
                            (x4, x5) <- __________stepEvalOO;
                            let {x1 = Cons x4 x5};
                            return (x0, x1)}]
___stepEvalOO = Immature $ msum [do {(x0, x1) <- ___step_EvalOO;
                          return (x0, x1)}]
___step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                           let {x53 = Term.False};
                           let {x54 = Term.True};
                           let {x55 = Term.True};
                           _________safe_III x53 x54 x55;
                           let {x56 = Term.True};
                           let {x57 = Term.False};
                           let {x58 = Term.False};
                           __________safe_III x56 x57 x58;
                           (x2, x3) <- __stepEvalOO;
                           let {x1 = Cons x2 x3};
                           return (x0, x1)},
                       do {let {x0 = Wolf};
                           let {x59 = Term.False};
                           let {x60 = Term.False};
                           let {x61 = Term.True};
                           _________safe_III x59 x60 x61;
                           let {x62 = Term.False};
                           let {x63 = Term.True};
                           let {x64 = Term.False};
                           __________safe_III x62 x63 x64;
                           (x4, x5) <- ____stepEvalOO;
                           let {x1 = Cons x4 x5};
                           return (x0, x1)},
                       do {let {x0 = Cabbage};
                           let {x65 = Term.False};
                           let {x66 = Term.True};
                           let {x67 = Term.False};
                           _________safe_III x65 x66 x67;
                           let {x68 = Term.True};
                           let {x69 = Term.False};
                           let {x70 = Term.True};
                           __________safe_III x68 x69 x70;
                           (x4, x5) <- ____________stepEvalOO;
                           let {x1 = Cons x4 x5};
                           return (x0, x1)}]
__stepEvalOO = Immature $ msum [do {(x0, x1) <- __step_EvalOO;
                         return (x0, x1)}]
__step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                          let {x41 = Term.True};
                          let {x42 = Term.False};
                          let {x43 = Term.False};
                          _________safe_III x41 x42 x43;
                          let {x44 = Term.False};
                          let {x45 = Term.True};
                          let {x46 = Term.True};
                          __________safe_III x44 x45 x46;
                          (x2, x3) <- ___stepEvalOO;
                          let {x1 = Cons x2 x3};
                          return (x0, x1)},
                      do {let {x0 = Goat};
                          let {x47 = Term.False};
                          let {x48 = Term.False};
                          let {x49 = Term.False};
                          _________safe_III x47 x48 x49;
                          let {x50 = Term.True};
                          let {x51 = Term.True};
                          let {x52 = Term.True};
                          __________safe_III x50 x51 x52;
                          (x4, x5) <- _stepEvalOO;
                          let {x1 = Cons x4 x5};
                          return (x0, x1)}]
_evalIO x0 = Immature $ msum [do {let {x1 = Nil};
                       let {x140 = Term.False};
                       let {x141 = Term.False};
                       let {x142 = Term.False};
                       let {x143 = Term.False};
                       let {x139 = Quad x140 x141 x142 x143};
                       let {x145 = Term.True};
                       let {x146 = Term.True};
                       let {x147 = Term.True};
                       let {x148 = Term.True};
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
_stepEvalOO = Immature $ msum [do {(x0, x1) <- step_EvalOO; return (x0, x1)}]
_step_OOIIIIII x2 x3 x4 x5 x6 x7 = Immature $ msum [do {_________safe_III x2 x3 x4;
                                             __________safe_III x5 x6 x7;
                                             let {x165 = Term.False};
                                             let {x164 = Quad x2 x3 x4 x165};
                                             let {x167 = Term.True};
                                             let {x166 = Quad x5 x6 x7 x167};
                                             let {x0 = Term.Empty};
                                             let {x168 = x164};
                                             let {x169 = x166};
                                             let {x1 = Pair x168 x169};
                                             return (x0, x1)},
                                         do {let {x170 = Term.False};
                                             _________safe_III x170 x3 x4;
                                             let {x0 = Goat};
                                             let {x172 = Term.False};
                                             let {x173 = Term.False};
                                             let {x171 = Quad x172 x3 x4 x173};
                                             let {x175 = Term.True};
                                             let {x176 = Term.True};
                                             let {x174 = Quad x175 x6 x7 x176};
                                             let {x179 = Term.True};
                                             __________safe_III x179 x6 x7;
                                             guard (x2 == Term.True);
                                             let {x177 = x171};
                                             let {x178 = x174};
                                             let {x1 = Pair x177 x178};
                                             return (x0, x1)},
                                         do {let {x180 = Term.False};
                                             _________safe_III x2 x180 x4;
                                             let {x0 = Wolf};
                                             let {x182 = Term.False};
                                             let {x183 = Term.False};
                                             let {x181 = Quad x2 x182 x4 x183};
                                             let {x185 = Term.True};
                                             let {x186 = Term.True};
                                             let {x184 = Quad x6 x185 x7 x186};
                                             let {x189 = Term.True};
                                             __________safe_III x6 x189 x7;
                                             guard (x3 == Term.True);
                                             let {x187 = x181};
                                             let {x188 = x184};
                                             let {x1 = Pair x187 x188};
                                             return (x0, x1)},
                                         do {let {x190 = Term.False};
                                             _________safe_III x2 x3 x190;
                                             let {x0 = Cabbage};
                                             let {x192 = Term.False};
                                             let {x193 = Term.False};
                                             let {x191 = Quad x2 x3 x192 x193};
                                             let {x195 = Term.True};
                                             let {x196 = Term.True};
                                             let {x194 = Quad x5 x6 x195 x196};
                                             let {x199 = Term.True};
                                             __________safe_III x5 x6 x199;
                                             guard (x4 == Term.True);
                                             let {x197 = x191};
                                             let {x198 = x194};
                                             let {x1 = Pair x197 x198};
                                             return (x0, x1)}]
stepIOO x0 = Immature $ msum [do {let {x152 = Term.True};
                       let {x154 = Term.False};
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
                   do {let {x158 = Term.False};
                       let {x160 = Term.True};
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
stepEvalOO = Immature $ msum [do {(x0, x1) <- _step_EvalOO; return (x0, x1)}]
_step_EvalOO = Immature $ msum [do {let {x0 = Term.Empty};
                         let {x35 = Term.False};
                         let {x36 = Term.False};
                         let {x37 = Term.False};
                         _________safe_III x35 x36 x37;
                         let {x38 = Term.True};
                         let {x39 = Term.True};
                         let {x40 = Term.True};
                         __________safe_III x38 x39 x40;
                         (x2, x3) <- _stepEvalOO;
                         let {x1 = Cons x2 x3};
                         return (x0, x1)}]