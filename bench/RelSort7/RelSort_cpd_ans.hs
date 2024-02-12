module RelSort_cpd_ans where

import Stream
import Control.Monad
import Term

sortoI x0 = Immature $ msum [do {let {x19 = Nil};
                      (x1, x20) <- case x0 of
                                   {Cons y1 y20 -> return (y1, y20); _ -> mzero};
                      let {x15 = x20};
                      (x2, x16) <- case x15 of
                                   {Cons y2 y16 -> return (y2, y16); _ -> mzero};
                      (x3, x17) <- case x16 of
                                   {Cons y3 y17 -> return (y3, y17); _ -> mzero};
                      (x4, x18) <- case x17 of
                                   {Cons y4 y18 -> return (y4, y18); _ -> mzero};
                      x5 <- case x18 of
                            {Cons y5 y19 -> do {guard (x19 == y19); return y5}; _ -> mzero};
                      minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x1 x2 x3 x4 x5;
                      return ()},
                  do {let {x25 = Nil};
                      (x1, x26) <- case x0 of
                                   {Cons y1 y26 -> return (y1, y26); _ -> mzero};
                      let {x21 = x26};
                      (x2, x22) <- case x21 of
                                   {Cons y2 y22 -> return (y2, y22); _ -> mzero};
                      (x3, x23) <- case x22 of
                                   {Cons y3 y23 -> return (y3, y23); _ -> mzero};
                      (x4, x24) <- case x23 of
                                   {Cons y4 y24 -> return (y4, y24); _ -> mzero};
                      x5 <- case x24 of
                            {Cons y5 y25 -> do {guard (x25 == y25); return y5}; _ -> mzero};
                      _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x1 x2 x3 x4 x5;
                      return ()}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x3 x4 = Immature $ msum [do {(x5,
                                                                                                  x9) <- ___minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x1 x2 x3 x4;
                                                                                                 _________________________minmaxoMinmaxoIII x5 x0 x9;
                                                                                                 return ()}]
_________________________minmaxoMinmaxoIII x0 x1 x2 = Immature $ msum [do {let {x602 = Zero};
                                                                let {x601 = Succ x602};
                                                                let {x600 = Succ x601};
                                                                let {x599 = Succ x600};
                                                                let {x604 = Zero};
                                                                let {x606 = Zero};
                                                                let {x605 = Succ x606};
                                                                minmaxoIIII x604 x1 x2 x605;
                                                                x603 <- case x0 of
                                                                        {Succ y603 -> return y603;
                                                                         _ -> mzero};
                                                                guard (x603 == x599);
                                                                return ()},
                                                            do {let {x607 = Zero};
                                                                let {x609 = Zero};
                                                                let {x614 = Zero};
                                                                let {x613 = Succ x614};
                                                                let {x612 = Succ x613};
                                                                let {x611 = Succ x612};
                                                                let {x610 = Succ x611};
                                                                minmaxoIIII x609 x1 x2 x610;
                                                                x608 <- case x0 of
                                                                        {Succ y608 -> return y608;
                                                                         _ -> mzero};
                                                                guard (x608 == x607);
                                                                return ()}]
___minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5 = Immature $ msum [do {(x0,
                                                                             x1) <- ____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5;
                                                                            return (x0, x1)},
                                                                        do {(x0,
                                                                             x1) <- _____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5;
                                                                            return (x0, x1)}]
_____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5 = Immature $ msum [do {let {x591 = Zero};
                                                                       let {x590 = Succ x591};
                                                                       let {x589 = Succ x590};
                                                                       let {x588 = Succ x589};
                                                                       let {x594 = Zero};
                                                                       let {x593 = Succ x594};
                                                                       let {x592 = Succ x593};
                                                                       (x1,
                                                                        x0,
                                                                        x6,
                                                                        x7) <- leoMinmaxoMinmaxoMinmaxoOIOIOIIO x2 x3 x4 x5;
                                                                       minmaxoIIII x588 x6 x7 x592;
                                                                       return (x0, x1)},
                                                                   do {let {x598 = Zero};
                                                                       let {x597 = Succ x598};
                                                                       let {x596 = Succ x597};
                                                                       let {x595 = Succ x596};
                                                                       (x0,
                                                                        x8) <- _______________gtoMinmaxoMinmaxoMinmaxoOOIII x3 x4 x5;
                                                                       x1 <- minmaxoOIII x2 x8 x595;
                                                                       return (x0, x1)}]
_______________gtoMinmaxoMinmaxoMinmaxoOOIII x2 x3 x4 = Immature $ msum [do {let {x0 = Zero};
                                                                  x1 <- ____________minmaxoMinmaxoMinmaxoOIII x2 x3 x4;
                                                                  return (x0, x1)},
                                                              do {(x1,
                                                                   x5) <- ________________gtoMinmaxoMinmaxoMinmaxoOIIIO x2 x3 x4;
                                                                  let {x0 = Succ x5};
                                                                  return (x0, x1)}]
________________gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                                   x0 <- _____________minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                                   return (x0, x4)},
                                                               do {(x0,
                                                                    x5) <- _________________gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3;
                                                                   let {x4 = Succ x5};
                                                                   return (x0, x4)}]
_________________gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                                    x0 <- ______________minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                                    return (x0, x4)}]
______________minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {x0 <- _______________________minmaxoMinmaxoOIII x1 x2 x3;
                                                             return x0}]
_______________________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x549 = Zero};
                                                               let {x548 = Succ x549};
                                                               let {x547 = Succ x548};
                                                               leoII x1 x547;
                                                               let {x552 = Zero};
                                                               let {x551 = Succ x552};
                                                               let {x550 = Succ x551};
                                                               let {x555 = Zero};
                                                               let {x554 = Succ x555};
                                                               let {x553 = Succ x554};
                                                               minmaxoIIII x550 x2 x3 x553;
                                                               let {x0 = x1};
                                                               return x0},
                                                           do {let {x557 = Zero};
                                                               let {x556 = Succ x557};
                                                               x558 <- case x1 of
                                                                       {Succ y558 -> return y558;
                                                                        _ -> mzero};
                                                               guard (x558 == x556);
                                                               x0 <- ___________________________________gtoMinmaxoOII x2 x3;
                                                               return x0}]
___________________________________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                                                   let {x559 = Zero};
                                                                   let {x562 = Zero};
                                                                   let {x561 = Succ x562};
                                                                   let {x560 = Succ x561};
                                                                   minmaxoIIII x559 x1 x2 x560;
                                                                   return x0},
                                                               do {x3 <- ____________________________________gtoMinmaxoIIO x1 x2;
                                                                   let {x0 = Succ x3};
                                                                   return x0}]
____________________________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                                    let {x564 = Zero};
                                                                    let {x563 = Succ x564};
                                                                    let {x567 = Zero};
                                                                    let {x566 = Succ x567};
                                                                    let {x565 = Succ x566};
                                                                    minmaxoIIII x563 x0 x1 x565;
                                                                    return x2}]
_____________minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {x0 <- _____________________minmaxoMinmaxoOIII x1 x2 x3;
                                                            return x0},
                                                        do {x0 <- ______________________minmaxoMinmaxoOIII x1 x2 x3;
                                                            return x0}]
______________________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x531 = Zero};
                                                              let {x530 = Succ x531};
                                                              let {x529 = Succ x530};
                                                              leoII x1 x529;
                                                              let {x534 = Zero};
                                                              let {x533 = Succ x534};
                                                              let {x532 = Succ x533};
                                                              let {x536 = Zero};
                                                              let {x535 = Succ x536};
                                                              minmaxoIIII x532 x2 x3 x535;
                                                              let {x0 = x1};
                                                              return x0},
                                                          do {let {x538 = Zero};
                                                              let {x537 = Succ x538};
                                                              x539 <- case x1 of
                                                                      {Succ y539 -> return y539;
                                                                       _ -> mzero};
                                                              guard (x539 == x537);
                                                              x0 <- _________________________________gtoMinmaxoOII x2 x3;
                                                              return x0}]
_________________________________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                                                 let {x540 = Zero};
                                                                 let {x542 = Zero};
                                                                 let {x541 = Succ x542};
                                                                 minmaxoIIII x540 x1 x2 x541;
                                                                 return x0},
                                                             do {x3 <- __________________________________gtoMinmaxoIIO x1 x2;
                                                                 let {x0 = Succ x3};
                                                                 return x0}]
__________________________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                                  let {x544 = Zero};
                                                                  let {x543 = Succ x544};
                                                                  let {x546 = Zero};
                                                                  let {x545 = Succ x546};
                                                                  minmaxoIIII x543 x0 x1 x545;
                                                                  return x2}]
_____________________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x517 = Zero};
                                                             let {x516 = Succ x517};
                                                             leoII x1 x516;
                                                             let {x519 = Zero};
                                                             let {x518 = Succ x519};
                                                             let {x522 = Zero};
                                                             let {x521 = Succ x522};
                                                             let {x520 = Succ x521};
                                                             minmaxoIIII x518 x2 x3 x520;
                                                             let {x0 = x1};
                                                             return x0},
                                                         do {let {x523 = Zero};
                                                             x524 <- case x1 of
                                                                     {Succ y524 -> return y524;
                                                                      _ -> mzero};
                                                             guard (x524 == x523);
                                                             x0 <- ________________________________gtoMinmaxoOII x2 x3;
                                                             return x0}]
________________________________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                                                let {x525 = Zero};
                                                                let {x528 = Zero};
                                                                let {x527 = Succ x528};
                                                                let {x526 = Succ x527};
                                                                minmaxoIIII x525 x1 x2 x526;
                                                                return x0}]
____________minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {x0 <- ___________________minmaxoMinmaxoOIII x1 x2 x3;
                                                           return x0},
                                                       do {x0 <- ____________________minmaxoMinmaxoOIII x1 x2 x3;
                                                           return x0}]
____________________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x503 = Zero};
                                                            let {x502 = Succ x503};
                                                            let {x501 = Succ x502};
                                                            leoII x1 x501;
                                                            let {x506 = Zero};
                                                            let {x505 = Succ x506};
                                                            let {x504 = Succ x505};
                                                            let {x507 = Zero};
                                                            minmaxoIIII x504 x2 x3 x507;
                                                            let {x0 = x1};
                                                            return x0},
                                                        do {let {x509 = Zero};
                                                            let {x508 = Succ x509};
                                                            x510 <- case x1 of
                                                                    {Succ y510 -> return y510;
                                                                     _ -> mzero};
                                                            guard (x510 == x508);
                                                            x0 <- ______________________________gtoMinmaxoOII x2 x3;
                                                            return x0}]
______________________________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                                              let {x511 = Zero};
                                                              let {x512 = Zero};
                                                              minmaxoIIII x511 x1 x2 x512;
                                                              return x0},
                                                          do {x3 <- _______________________________gtoMinmaxoIIO x1 x2;
                                                              let {x0 = Succ x3};
                                                              return x0}]
_______________________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                               let {x514 = Zero};
                                                               let {x513 = Succ x514};
                                                               let {x515 = Zero};
                                                               minmaxoIIII x513 x0 x1 x515;
                                                               return x2}]
___________________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x497 = Zero};
                                                           let {x500 = Zero};
                                                           let {x499 = Succ x500};
                                                           let {x498 = Succ x499};
                                                           minmaxoIIII x497 x2 x3 x498;
                                                           guard (x1 == Zero);
                                                           let {x0 = x1};
                                                           return x0}]
____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5 = Immature $ msum [do {let {x580 = Zero};
                                                                      let {x579 = Succ x580};
                                                                      let {x578 = Succ x579};
                                                                      let {x584 = Zero};
                                                                      let {x583 = Succ x584};
                                                                      let {x582 = Succ x583};
                                                                      let {x581 = Succ x582};
                                                                      (x1,
                                                                       x0,
                                                                       x6,
                                                                       x7) <- _______leoMinmaxoMinmaxoMinmaxoOIOIOIIO x2 x3 x4 x5;
                                                                      minmaxoIIII x578 x6 x7 x581;
                                                                      return (x0, x1)},
                                                                  do {let {x587 = Zero};
                                                                      let {x586 = Succ x587};
                                                                      let {x585 = Succ x586};
                                                                      (x0,
                                                                       x8) <- _____________gtoMinmaxoMinmaxoMinmaxoOOIII x3 x4 x5;
                                                                      x1 <- minmaxoOIII x2 x8 x585;
                                                                      return (x0, x1)}]
_____________gtoMinmaxoMinmaxoMinmaxoOOIII x2 x3 x4 = Immature $ msum [do {let {x0 = Zero};
                                                                x1 <- ________minmaxoMinmaxoMinmaxoOIII x2 x3 x4;
                                                                return (x0, x1)},
                                                            do {(x1,
                                                                 x5) <- ______________gtoMinmaxoMinmaxoMinmaxoOIIIO x2 x3 x4;
                                                                let {x0 = Succ x5};
                                                                return (x0, x1)}]
______________gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                                 x0 <- _________minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                                 return (x0, x4)}]
_________minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {x0 <- _____________minmaxoMinmaxoOIII x1 x2 x3;
                                                        return x0},
                                                    do {x0 <- ______________minmaxoMinmaxoOIII x1 x2 x3;
                                                        return x0}]
______________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x348 = Zero};
                                                      let {x347 = Succ x348};
                                                      let {x346 = Succ x347};
                                                      let {x345 = Succ x346};
                                                      leoII x1 x345;
                                                      let {x352 = Zero};
                                                      let {x351 = Succ x352};
                                                      let {x350 = Succ x351};
                                                      let {x349 = Succ x350};
                                                      let {x354 = Zero};
                                                      let {x353 = Succ x354};
                                                      minmaxoIIII x349 x2 x3 x353;
                                                      let {x0 = x1};
                                                      return x0},
                                                  do {let {x357 = Zero};
                                                      let {x356 = Succ x357};
                                                      let {x355 = Succ x356};
                                                      x358 <- case x1 of
                                                              {Succ y358 -> return y358;
                                                               _ -> mzero};
                                                      guard (x358 == x355);
                                                      x0 <- ___________________gtoMinmaxoOII x2 x3;
                                                      return x0}]
___________________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                                   let {x359 = Zero};
                                                   let {x361 = Zero};
                                                   let {x360 = Succ x361};
                                                   minmaxoIIII x359 x1 x2 x360;
                                                   return x0},
                                               do {x3 <- ____________________gtoMinmaxoIIO x1 x2;
                                                   let {x0 = Succ x3};
                                                   return x0}]
____________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                    let {x363 = Zero};
                                                    let {x362 = Succ x363};
                                                    let {x365 = Zero};
                                                    let {x364 = Succ x365};
                                                    minmaxoIIII x362 x0 x1 x364;
                                                    return x2},
                                                do {x3 <- _____________________gtoMinmaxoIIO x0 x1;
                                                    let {x2 = Succ x3};
                                                    return x2}]
_____________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                     let {x368 = Zero};
                                                     let {x367 = Succ x368};
                                                     let {x366 = Succ x367};
                                                     let {x370 = Zero};
                                                     let {x369 = Succ x370};
                                                     minmaxoIIII x366 x0 x1 x369;
                                                     return x2}]
_____________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x331 = Zero};
                                                     let {x330 = Succ x331};
                                                     leoII x1 x330;
                                                     let {x333 = Zero};
                                                     let {x332 = Succ x333};
                                                     let {x337 = Zero};
                                                     let {x336 = Succ x337};
                                                     let {x335 = Succ x336};
                                                     let {x334 = Succ x335};
                                                     minmaxoIIII x332 x2 x3 x334;
                                                     let {x0 = x1};
                                                     return x0},
                                                 do {let {x338 = Zero};
                                                     x339 <- case x1 of
                                                             {Succ y339 -> return y339; _ -> mzero};
                                                     guard (x339 == x338);
                                                     x0 <- __________________gtoMinmaxoOII x2 x3;
                                                     return x0}]
__________________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                                  let {x340 = Zero};
                                                  let {x344 = Zero};
                                                  let {x343 = Succ x344};
                                                  let {x342 = Succ x343};
                                                  let {x341 = Succ x342};
                                                  minmaxoIIII x340 x1 x2 x341;
                                                  return x0}]
________minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {x0 <- ___________minmaxoMinmaxoOIII x1 x2 x3;
                                                       return x0},
                                                   do {x0 <- ____________minmaxoMinmaxoOIII x1 x2 x3;
                                                       return x0}]
____________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x311 = Zero};
                                                    let {x310 = Succ x311};
                                                    let {x309 = Succ x310};
                                                    let {x308 = Succ x309};
                                                    leoII x1 x308;
                                                    let {x315 = Zero};
                                                    let {x314 = Succ x315};
                                                    let {x313 = Succ x314};
                                                    let {x312 = Succ x313};
                                                    let {x316 = Zero};
                                                    minmaxoIIII x312 x2 x3 x316;
                                                    let {x0 = x1};
                                                    return x0},
                                                do {let {x319 = Zero};
                                                    let {x318 = Succ x319};
                                                    let {x317 = Succ x318};
                                                    x320 <- case x1 of
                                                            {Succ y320 -> return y320; _ -> mzero};
                                                    guard (x320 == x317);
                                                    x0 <- _______________gtoMinmaxoOII x2 x3;
                                                    return x0}]
_______________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                               let {x321 = Zero};
                                               let {x322 = Zero};
                                               minmaxoIIII x321 x1 x2 x322;
                                               return x0},
                                           do {x3 <- ________________gtoMinmaxoIIO x1 x2;
                                               let {x0 = Succ x3};
                                               return x0}]
________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                let {x324 = Zero};
                                                let {x323 = Succ x324};
                                                let {x325 = Zero};
                                                minmaxoIIII x323 x0 x1 x325;
                                                return x2},
                                            do {x3 <- _________________gtoMinmaxoIIO x0 x1;
                                                let {x2 = Succ x3};
                                                return x2}]
_________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                 let {x328 = Zero};
                                                 let {x327 = Succ x328};
                                                 let {x326 = Succ x327};
                                                 let {x329 = Zero};
                                                 minmaxoIIII x326 x0 x1 x329;
                                                 return x2}]
___________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x303 = Zero};
                                                   let {x307 = Zero};
                                                   let {x306 = Succ x307};
                                                   let {x305 = Succ x306};
                                                   let {x304 = Succ x305};
                                                   minmaxoIIII x303 x2 x3 x304;
                                                   guard (x1 == Zero);
                                                   let {x0 = x1};
                                                   return x0}]
_______leoMinmaxoMinmaxoMinmaxoOIOIOIIO x1 x3 x5 x6 = Immature $ msum [do {let {x2 = Zero};
                                                                (x0,
                                                                 x4,
                                                                 x7) <- minmaxoMinmaxoMinmaxoOIIOIIO x1 x3 x5 x6;
                                                                return (x0, x2, x4, x7)},
                                                            do {(x0,
                                                                 x4,
                                                                 x7,
                                                                 x8) <- ________leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x3 x5 x6;
                                                                let {x2 = Succ x8};
                                                                return (x0, x2, x4, x7)}]
________leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x2 x4 x5 = Immature $ msum [do {let {x7 = Zero};
                                                                 (x0,
                                                                  x3,
                                                                  x6) <- _minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5;
                                                                 return (x0, x3, x6, x7)},
                                                             do {let {x482 = Zero};
                                                                 let {x483 = x482};
                                                                 let {x7 = Succ x483};
                                                                 (x0,
                                                                  x3,
                                                                  x6) <- __minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5;
                                                                 return (x0, x3, x6, x7)}]
__minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5 = Immature $ msum [do {let {x56 = Zero};
                                                       let {x55 = Succ x56};
                                                       let {x54 = Succ x55};
                                                       leoII x1 x54;
                                                       let {x0 = x1};
                                                       (x3, x6) <- __minmaxoMinmaxoIOIIO x2 x4 x5;
                                                       return (x0, x3, x6)},
                                                   do {let {x58 = Zero};
                                                       let {x57 = Succ x58};
                                                       x59 <- case x1 of
                                                              {Succ y59 -> return y59; _ -> mzero};
                                                       guard (x59 == x57);
                                                       (x0,
                                                        x3,
                                                        x6) <- _gtoMinmaxoMinmaxoOIOIIO x2 x4 x5;
                                                       return (x0, x3, x6)}]
__minmaxoMinmaxoIOIIO x0 x2 x3 = Immature $ msum [do {let {x63 = Zero};
                                           let {x62 = Succ x63};
                                           x64 <- case x0 of
                                                  {Succ y64 -> return y64; _ -> mzero};
                                           guard (x64 == x62);
                                           (x65, x4) <- minmaxoOIIO x2 x3;
                                           x66 <- case x65 of
                                                  {Succ y66 -> return y66; _ -> mzero};
                                           x5 <- case x66 of
                                                 {Succ y5 -> return y5; _ -> mzero};
                                           let {x60 = Succ x5};
                                           let {x61 = x60};
                                           let {x1 = Succ x61};
                                           return (x1, x4)},
                                       do {let {x72 = Zero};
                                           let {x71 = Succ x72};
                                           let {x70 = Succ x71};
                                           let {x1 = x0};
                                           x69 <- case x1 of
                                                  {Succ y69 -> return y69; _ -> mzero};
                                           let {x67 = x69};
                                           x68 <- case x67 of
                                                  {Succ y68 -> return y68; _ -> mzero};
                                           x6 <- case x68 of
                                                 {Succ y6 -> return y6; _ -> mzero};
                                           x4 <- minmaxoIIIO x70 x2 x3;
                                           return (x1, x4)}]
_gtoMinmaxoMinmaxoOIOIIO x1 x3 x4 = Immature $ msum [do {let {x0 = Zero};
                                              (x2, x5) <- minmaxoMinmaxoIOIIO x1 x3 x4;
                                              return (x0, x2, x5)},
                                          do {(x2, x5, x6) <- __gtoMinmaxoMinmaxoIOIIOO x1 x3 x4;
                                              let {x0 = Succ x6};
                                              return (x0, x2, x5)}]
__gtoMinmaxoMinmaxoIOIIOO x0 x2 x3 = Immature $ msum [do {let {x5 = Zero};
                                               (x1, x4) <- _minmaxoMinmaxoIOIIO x0 x2 x3;
                                               return (x1, x4, x5)}]
_minmaxoMinmaxoIOIIO x0 x2 x3 = Immature $ msum [do {let {x45 = Zero};
                                          x46 <- case x0 of
                                                 {Succ y46 -> return y46; _ -> mzero};
                                          guard (x46 == x45);
                                          (x47, x4) <- minmaxoOIIO x2 x3;
                                          x5 <- case x47 of
                                                {Succ y5 -> return y5; _ -> mzero};
                                          let {x1 = Succ x5};
                                          return (x1, x4)},
                                      do {let {x51 = Zero};
                                          let {x50 = Succ x51};
                                          let {x1 = x0};
                                          x49 <- case x1 of
                                                 {Succ y49 -> return y49; _ -> mzero};
                                          let {x48 = x49};
                                          x6 <- case x48 of
                                                {Succ y6 -> return y6; _ -> mzero};
                                          x4 <- minmaxoIIIO x50 x2 x3;
                                          return (x1, x4)}]
_minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5 = Immature $ msum [do {let {x42 = Zero};
                                                      let {x41 = Succ x42};
                                                      leoII x1 x41;
                                                      let {x0 = x1};
                                                      (x3, x6) <- _minmaxoMinmaxoIOIIO x2 x4 x5;
                                                      return (x0, x3, x6)},
                                                  do {let {x43 = Zero};
                                                      x44 <- case x1 of
                                                             {Succ y44 -> return y44; _ -> mzero};
                                                      guard (x44 == x43);
                                                      (x0,
                                                       x3,
                                                       x6) <- gtoMinmaxoMinmaxoOIOIIO x2 x4 x5;
                                                      return (x0, x3, x6)}]
gtoMinmaxoMinmaxoOIOIIO x1 x3 x4 = Immature $ msum [do {let {x0 = Zero};
                                             (x2, x5) <- minmaxoMinmaxoIOIIO x1 x3 x4;
                                             return (x0, x2, x5)}]
leoII x0 x1 = Immature $ msum [do {guard (x0 == Zero); return ()},
                    do {x2 <- case x1 of
                              {Succ y2 -> return y2; _ -> mzero};
                        x3 <- case x0 of
                              {Succ y3 -> return y3; _ -> mzero};
                        leoII x3 x2;
                        return ()}]
leoMinmaxoMinmaxoMinmaxoOIOIOIIO x1 x3 x5 x6 = Immature $ msum [do {let {x2 = Zero};
                                                         (x0,
                                                          x4,
                                                          x7) <- minmaxoMinmaxoMinmaxoOIIOIIO x1 x3 x5 x6;
                                                         return (x0, x2, x4, x7)},
                                                     do {(x0,
                                                          x4,
                                                          x7,
                                                          x8) <- _leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x3 x5 x6;
                                                         let {x2 = Succ x8};
                                                         return (x0, x2, x4, x7)}]
_leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x2 x4 x5 = Immature $ msum [do {let {x7 = Zero};
                                                          (x0,
                                                           x3,
                                                           x6) <- _minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5;
                                                          return (x0, x3, x6, x7)},
                                                      do {(x0,
                                                           x3,
                                                           x6,
                                                           x8) <- __leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x2 x4 x5;
                                                          let {x7 = Succ x8};
                                                          return (x0, x3, x6, x7)}]
__leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x2 x4 x5 = Immature $ msum [do {let {x7 = Zero};
                                                           (x0,
                                                            x3,
                                                            x6) <- __minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5;
                                                           return (x0, x3, x6, x7)},
                                                       do {let {x52 = Zero};
                                                           let {x53 = x52};
                                                           let {x7 = Succ x53};
                                                           (x0,
                                                            x3,
                                                            x6) <- ___minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5;
                                                           return (x0, x3, x6, x7)}]
___minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5 = Immature $ msum [do {let {x76 = Zero};
                                                        let {x75 = Succ x76};
                                                        let {x74 = Succ x75};
                                                        let {x73 = Succ x74};
                                                        leoII x1 x73;
                                                        let {x0 = x1};
                                                        (x3, x6) <- ___minmaxoMinmaxoIOIIO x2 x4 x5;
                                                        return (x0, x3, x6)},
                                                    do {let {x79 = Zero};
                                                        let {x78 = Succ x79};
                                                        let {x77 = Succ x78};
                                                        x80 <- case x1 of
                                                               {Succ y80 -> return y80; _ -> mzero};
                                                        guard (x80 == x77);
                                                        (x0,
                                                         x3,
                                                         x6) <- ___gtoMinmaxoMinmaxoOIOIIO x2 x4 x5;
                                                        return (x0, x3, x6)}]
___gtoMinmaxoMinmaxoOIOIIO x1 x3 x4 = Immature $ msum [do {let {x0 = Zero};
                                                (x2, x5) <- minmaxoMinmaxoIOIIO x1 x3 x4;
                                                return (x0, x2, x5)},
                                            do {(x2,
                                                 x5,
                                                 x6) <- ____gtoMinmaxoMinmaxoIOIIOO x1 x3 x4;
                                                let {x0 = Succ x6};
                                                return (x0, x2, x5)}]
____gtoMinmaxoMinmaxoIOIIOO x0 x2 x3 = Immature $ msum [do {let {x5 = Zero};
                                                 (x1, x4) <- _minmaxoMinmaxoIOIIO x0 x2 x3;
                                                 return (x1, x4, x5)},
                                             do {(x1,
                                                  x4,
                                                  x6) <- _____gtoMinmaxoMinmaxoIOIIOO x0 x2 x3;
                                                 let {x5 = Succ x6};
                                                 return (x1, x4, x5)}]
_____gtoMinmaxoMinmaxoIOIIOO x0 x2 x3 = Immature $ msum [do {let {x5 = Zero};
                                                  (x1, x4) <- __minmaxoMinmaxoIOIIO x0 x2 x3;
                                                  return (x1, x4, x5)}]
___minmaxoMinmaxoIOIIO x0 x2 x3 = Immature $ msum [do {let {x86 = Zero};
                                            let {x85 = Succ x86};
                                            let {x84 = Succ x85};
                                            x87 <- case x0 of
                                                   {Succ y87 -> return y87; _ -> mzero};
                                            guard (x87 == x84);
                                            (x88, x4) <- minmaxoOIIO x2 x3;
                                            x89 <- case x88 of
                                                   {Succ y89 -> return y89; _ -> mzero};
                                            x90 <- case x89 of
                                                   {Succ y90 -> return y90; _ -> mzero};
                                            x5 <- case x90 of
                                                  {Succ y5 -> return y5; _ -> mzero};
                                            let {x82 = Succ x5};
                                            let {x81 = Succ x82};
                                            let {x83 = x81};
                                            let {x1 = Succ x83};
                                            return (x1, x4)},
                                        do {let {x98 = Zero};
                                            let {x97 = Succ x98};
                                            let {x96 = Succ x97};
                                            let {x95 = Succ x96};
                                            let {x1 = x0};
                                            x94 <- case x1 of
                                                   {Succ y94 -> return y94; _ -> mzero};
                                            let {x91 = x94};
                                            x92 <- case x91 of
                                                   {Succ y92 -> return y92; _ -> mzero};
                                            x93 <- case x92 of
                                                   {Succ y93 -> return y93; _ -> mzero};
                                            x6 <- case x93 of
                                                  {Succ y6 -> return y6; _ -> mzero};
                                            x4 <- minmaxoIIIO x95 x2 x3;
                                            return (x1, x4)}]
minmaxoIIII x0 x1 x2 x3 = Immature $ msum [do {guard (x2 == x3);
                                    guard (x0 == x1);
                                    leoII x1 x3;
                                    return ()},
                                do {guard (x1 == x3); guard (x0 == x2); gtoII x2 x3; return ()}]
gtoII x0 x1 = Immature $ msum [do {x2 <- case x1 of
                              {Succ y2 -> return y2; _ -> mzero};
                        guard (x0 == Zero);
                        return ()},
                    do {x3 <- case x1 of
                              {Succ y3 -> return y3; _ -> mzero};
                        x4 <- case x0 of
                              {Succ y4 -> return y4; _ -> mzero};
                        gtoII x4 x3;
                        return ()}]
minmaxoIIIO x0 x1 x2 = Immature $ msum [do {guard (x0 == x1);
                                 let {x3 = x2};
                                 leoII x1 x3;
                                 return x3},
                             do {guard (x0 == x2); let {x3 = x1}; gtoII x2 x3; return x3}]
minmaxoOIII x1 x2 x3 = Immature $ msum [do {guard (x2 == x3);
                                 leoII x1 x3;
                                 let {x0 = x1};
                                 return x0},
                             do {guard (x1 == x3); gtoII x2 x3; let {x0 = x2}; return x0}]
minmaxoOIIO x1 x2 = Immature $ msum [do {let {x3 = x2};
                              leoII x1 x3;
                              let {x0 = x1};
                              return (x0, x3)},
                          do {let {x3 = x1}; gtoII x2 x3; let {x0 = x2}; return (x0, x3)}]
minmaxoMinmaxoIOIIO x0 x2 x3 = Immature $ msum [do {guard (x0 == Zero);
                                         (x1, x4) <- minmaxoOIIO x2 x3;
                                         return (x1, x4)},
                                     do {let {x40 = Zero};
                                         let {x1 = x0};
                                         x5 <- case x1 of
                                               {Succ y5 -> return y5; _ -> mzero};
                                         x4 <- minmaxoIIIO x40 x2 x3;
                                         return (x1, x4)}]
minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5 = Immature $ msum [do {guard (x1 == Zero);
                                                     let {x0 = x1};
                                                     (x3, x6) <- minmaxoMinmaxoIOIIO x2 x4 x5;
                                                     return (x0, x3, x6)}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x3 x4 = Immature $ msum [do {(x5,
                                                                                                 x6) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x1 x2 x3 x4;
                                                                                                __________________minmaxoMinmaxoIII x5 x0 x6;
                                                                                                return ()},
                                                                                            do {(x5,
                                                                                                 x6) <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x1 x2 x3 x4;
                                                                                                ________________________minmaxoMinmaxoIII x5 x0 x6;
                                                                                                return ()}]
________________________minmaxoMinmaxoIII x0 x1 x2 = Immature $ msum [do {let {x570 = Zero};
                                                               let {x569 = Succ x570};
                                                               let {x568 = Succ x569};
                                                               let {x572 = Zero};
                                                               let {x574 = Zero};
                                                               let {x573 = Succ x574};
                                                               minmaxoIIII x572 x1 x2 x573;
                                                               x571 <- case x0 of
                                                                       {Succ y571 -> return y571;
                                                                        _ -> mzero};
                                                               guard (x571 == x568);
                                                               return ()},
                                                           do {let {x575 = Zero};
                                                               let {x577 = Zero};
                                                               ______________________gtoMinmaxoIII x577 x1 x2;
                                                               x576 <- case x0 of
                                                                       {Succ y576 -> return y576;
                                                                        _ -> mzero};
                                                               guard (x576 == x575);
                                                               return ()}]
______________________gtoMinmaxoIII x0 x1 x2 = Immature $ msum [do {let {x384 = Zero};
                                                         let {x388 = Zero};
                                                         let {x387 = Succ x388};
                                                         let {x386 = Succ x387};
                                                         let {x385 = Succ x386};
                                                         minmaxoIIII x384 x1 x2 x385;
                                                         guard (x0 == Zero);
                                                         return ()},
                                                     do {x3 <- case x0 of
                                                               {Succ y3 -> return y3; _ -> mzero};
                                                         _______________________gtoMinmaxoIII x1 x2 x3;
                                                         return ()}]
_______________________gtoMinmaxoIII x0 x1 x2 = Immature $ msum [do {let {x390 = Zero};
                                                          let {x389 = Succ x390};
                                                          let {x394 = Zero};
                                                          let {x393 = Succ x394};
                                                          let {x392 = Succ x393};
                                                          let {x391 = Succ x392};
                                                          minmaxoIIII x389 x0 x1 x391;
                                                          guard (x2 == Zero);
                                                          return ()}]
__________________minmaxoMinmaxoIII x0 x1 x2 = Immature $ msum [do {let {x460 = Zero};
                                                         let {x459 = Succ x460};
                                                         let {x462 = Zero};
                                                         let {x464 = Zero};
                                                         let {x463 = Succ x464};
                                                         minmaxoIIII x462 x1 x2 x463;
                                                         x461 <- case x0 of
                                                                 {Succ y461 -> return y461;
                                                                  _ -> mzero};
                                                         guard (x461 == x459);
                                                         return ()},
                                                     do {let {x465 = Zero};
                                                         let {x467 = Zero};
                                                         let {x470 = Zero};
                                                         let {x469 = Succ x470};
                                                         let {x468 = Succ x469};
                                                         minmaxoIIII x467 x1 x2 x468;
                                                         x466 <- case x0 of
                                                                 {Succ y466 -> return y466;
                                                                  _ -> mzero};
                                                         guard (x466 == x465);
                                                         return ()}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5 = Immature $ msum [do {(x0,
                                                                           x1) <- __minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5;
                                                                          return (x0, x1)},
                                                                      do {(x0,
                                                                           x1) <- ___minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5;
                                                                          return (x0, x1)}]
___minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5 = Immature $ msum [do {let {x488 = Zero};
                                                                     let {x487 = Succ x488};
                                                                     let {x486 = Succ x487};
                                                                     let {x485 = Succ x486};
                                                                     let {x484 = Succ x485};
                                                                     let {x491 = Zero};
                                                                     let {x490 = Succ x491};
                                                                     let {x489 = Succ x490};
                                                                     (x1,
                                                                      x0,
                                                                      x6,
                                                                      x7) <- ___leoMinmaxoMinmaxoMinmaxoOIOIOIIO x2 x3 x4 x5;
                                                                     minmaxoIIII x484 x6 x7 x489;
                                                                     return (x0, x1)},
                                                                 do {let {x496 = Zero};
                                                                     let {x495 = Succ x496};
                                                                     let {x494 = Succ x495};
                                                                     let {x493 = Succ x494};
                                                                     let {x492 = Succ x493};
                                                                     (x0,
                                                                      x8) <- _________gtoMinmaxoMinmaxoMinmaxoOOIII x3 x4 x5;
                                                                     x1 <- minmaxoOIII x2 x8 x492;
                                                                     return (x0, x1)}]
_________gtoMinmaxoMinmaxoMinmaxoOOIII x2 x3 x4 = Immature $ msum [do {let {x0 = Zero};
                                                            x1 <- ____________minmaxoMinmaxoMinmaxoOIII x2 x3 x4;
                                                            return (x0, x1)},
                                                        do {(x1,
                                                             x5) <- __________gtoMinmaxoMinmaxoMinmaxoOIIIO x2 x3 x4;
                                                            let {x0 = Succ x5};
                                                            return (x0, x1)}]
__________gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                             x0 <- _____________minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                             return (x0, x4)},
                                                         do {(x0,
                                                              x5) <- ___________gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3;
                                                             let {x4 = Succ x5};
                                                             return (x0, x4)}]
___________gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                              x0 <- ______________minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                              return (x0, x4)}]
___leoMinmaxoMinmaxoMinmaxoOIOIOIIO x1 x3 x5 x6 = Immature $ msum [do {let {x2 = Zero};
                                                            (x0,
                                                             x4,
                                                             x7) <- minmaxoMinmaxoMinmaxoOIIOIIO x1 x3 x5 x6;
                                                            return (x0, x2, x4, x7)},
                                                        do {(x0,
                                                             x4,
                                                             x7,
                                                             x8) <- ____leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x3 x5 x6;
                                                            let {x2 = Succ x8};
                                                            return (x0, x2, x4, x7)}]
____leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x2 x4 x5 = Immature $ msum [do {let {x7 = Zero};
                                                             (x0,
                                                              x3,
                                                              x6) <- _minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5;
                                                             return (x0, x3, x6, x7)},
                                                         do {(x0,
                                                              x3,
                                                              x6,
                                                              x8) <- _____leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x2 x4 x5;
                                                             let {x7 = Succ x8};
                                                             return (x0, x3, x6, x7)}]
_____leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x2 x4 x5 = Immature $ msum [do {let {x7 = Zero};
                                                              (x0,
                                                               x3,
                                                               x6) <- __minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5;
                                                              return (x0, x3, x6, x7)},
                                                          do {(x0,
                                                               x3,
                                                               x6,
                                                               x8) <- ______leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x2 x4 x5;
                                                              let {x7 = Succ x8};
                                                              return (x0, x3, x6, x7)}]
______leoMinmaxoMinmaxoMinmaxoOIIOIIOO x1 x2 x4 x5 = Immature $ msum [do {let {x7 = Zero};
                                                               (x0,
                                                                x3,
                                                                x6) <- ___minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5;
                                                               return (x0, x3, x6, x7)},
                                                           do {let {x268 = Zero};
                                                               let {x269 = x268};
                                                               let {x7 = Succ x269};
                                                               (x0,
                                                                x3,
                                                                x6) <- _______minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5;
                                                               return (x0, x3, x6, x7)}]
_______minmaxoMinmaxoMinmaxoOIIOIIO x1 x2 x4 x5 = Immature $ msum [do {let {x274 = Zero};
                                                            let {x273 = Succ x274};
                                                            let {x272 = Succ x273};
                                                            let {x271 = Succ x272};
                                                            let {x270 = Succ x271};
                                                            leoII x1 x270;
                                                            let {x0 = x1};
                                                            (x3,
                                                             x6) <- __________minmaxoMinmaxoIOIIO x2 x4 x5;
                                                            return (x0, x3, x6)},
                                                        do {let {x278 = Zero};
                                                            let {x277 = Succ x278};
                                                            let {x276 = Succ x277};
                                                            let {x275 = Succ x276};
                                                            x279 <- case x1 of
                                                                    {Succ y279 -> return y279;
                                                                     _ -> mzero};
                                                            guard (x279 == x275);
                                                            (x0,
                                                             x3,
                                                             x6) <- ______gtoMinmaxoMinmaxoOIOIIO x2 x4 x5;
                                                            return (x0, x3, x6)}]
__________minmaxoMinmaxoIOIIO x0 x2 x3 = Immature $ msum [do {let {x287 = Zero};
                                                   let {x286 = Succ x287};
                                                   let {x285 = Succ x286};
                                                   let {x284 = Succ x285};
                                                   x288 <- case x0 of
                                                           {Succ y288 -> return y288; _ -> mzero};
                                                   guard (x288 == x284);
                                                   (x289, x4) <- minmaxoOIIO x2 x3;
                                                   x290 <- case x289 of
                                                           {Succ y290 -> return y290; _ -> mzero};
                                                   x291 <- case x290 of
                                                           {Succ y291 -> return y291; _ -> mzero};
                                                   x292 <- case x291 of
                                                           {Succ y292 -> return y292; _ -> mzero};
                                                   x5 <- case x292 of
                                                         {Succ y5 -> return y5; _ -> mzero};
                                                   let {x282 = Succ x5};
                                                   let {x281 = Succ x282};
                                                   let {x280 = Succ x281};
                                                   let {x283 = x280};
                                                   let {x1 = Succ x283};
                                                   return (x1, x4)},
                                               do {let {x302 = Zero};
                                                   let {x301 = Succ x302};
                                                   let {x300 = Succ x301};
                                                   let {x299 = Succ x300};
                                                   let {x298 = Succ x299};
                                                   let {x1 = x0};
                                                   x297 <- case x1 of
                                                           {Succ y297 -> return y297; _ -> mzero};
                                                   let {x293 = x297};
                                                   x294 <- case x293 of
                                                           {Succ y294 -> return y294; _ -> mzero};
                                                   x295 <- case x294 of
                                                           {Succ y295 -> return y295; _ -> mzero};
                                                   x296 <- case x295 of
                                                           {Succ y296 -> return y296; _ -> mzero};
                                                   x6 <- case x296 of
                                                         {Succ y6 -> return y6; _ -> mzero};
                                                   x4 <- minmaxoIIIO x298 x2 x3;
                                                   return (x1, x4)}]
______gtoMinmaxoMinmaxoOIOIIO x1 x3 x4 = Immature $ msum [do {let {x0 = Zero};
                                                   (x2, x5) <- minmaxoMinmaxoIOIIO x1 x3 x4;
                                                   return (x0, x2, x5)},
                                               do {(x2,
                                                    x5,
                                                    x6) <- _______gtoMinmaxoMinmaxoIOIIOO x1 x3 x4;
                                                   let {x0 = Succ x6};
                                                   return (x0, x2, x5)}]
_______gtoMinmaxoMinmaxoIOIIOO x0 x2 x3 = Immature $ msum [do {let {x5 = Zero};
                                                    (x1, x4) <- _minmaxoMinmaxoIOIIO x0 x2 x3;
                                                    return (x1, x4, x5)},
                                                do {(x1,
                                                     x4,
                                                     x6) <- ________gtoMinmaxoMinmaxoIOIIOO x0 x2 x3;
                                                    let {x5 = Succ x6};
                                                    return (x1, x4, x5)}]
________gtoMinmaxoMinmaxoIOIIOO x0 x2 x3 = Immature $ msum [do {let {x5 = Zero};
                                                     (x1, x4) <- __minmaxoMinmaxoIOIIO x0 x2 x3;
                                                     return (x1, x4, x5)},
                                                 do {(x1,
                                                      x4,
                                                      x6) <- _________gtoMinmaxoMinmaxoIOIIOO x0 x2 x3;
                                                     let {x5 = Succ x6};
                                                     return (x1, x4, x5)}]
_________gtoMinmaxoMinmaxoIOIIOO x0 x2 x3 = Immature $ msum [do {let {x5 = Zero};
                                                      (x1, x4) <- ___minmaxoMinmaxoIOIIO x0 x2 x3;
                                                      return (x1, x4, x5)}]
__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5 = Immature $ msum [do {let {x473 = Zero};
                                                                    let {x472 = Succ x473};
                                                                    let {x471 = Succ x472};
                                                                    let {x478 = Zero};
                                                                    let {x477 = Succ x478};
                                                                    let {x476 = Succ x477};
                                                                    let {x475 = Succ x476};
                                                                    let {x474 = Succ x475};
                                                                    (x1,
                                                                     x0,
                                                                     x6,
                                                                     x7) <- _______leoMinmaxoMinmaxoMinmaxoOIOIOIIO x2 x3 x4 x5;
                                                                    minmaxoIIII x471 x6 x7 x474;
                                                                    return (x0, x1)},
                                                                do {let {x481 = Zero};
                                                                    let {x480 = Succ x481};
                                                                    let {x479 = Succ x480};
                                                                    (x0,
                                                                     x8) <- _______gtoMinmaxoMinmaxoMinmaxoOOIII x3 x4 x5;
                                                                    x1 <- minmaxoOIII x2 x8 x479;
                                                                    return (x0, x1)}]
_______gtoMinmaxoMinmaxoMinmaxoOOIII x2 x3 x4 = Immature $ msum [do {let {x0 = Zero};
                                                          x1 <- ____minmaxoMinmaxoMinmaxoOIII x2 x3 x4;
                                                          return (x0, x1)},
                                                      do {(x1,
                                                           x5) <- ________gtoMinmaxoMinmaxoMinmaxoOIIIO x2 x3 x4;
                                                          let {x0 = Succ x5};
                                                          return (x0, x1)}]
________gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                           x0 <- _____minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                           return (x0, x4)}]
_____minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {x0 <- ______minmaxoMinmaxoOIII x1 x2 x3;
                                                    return x0},
                                                do {x0 <- _______minmaxoMinmaxoOIII x1 x2 x3;
                                                    return x0}]
_______minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x156 = Zero};
                                               let {x155 = Succ x156};
                                               let {x154 = Succ x155};
                                               let {x153 = Succ x154};
                                               let {x152 = Succ x153};
                                               leoII x1 x152;
                                               let {x161 = Zero};
                                               let {x160 = Succ x161};
                                               let {x159 = Succ x160};
                                               let {x158 = Succ x159};
                                               let {x157 = Succ x158};
                                               let {x163 = Zero};
                                               let {x162 = Succ x163};
                                               minmaxoIIII x157 x2 x3 x162;
                                               let {x0 = x1};
                                               return x0},
                                           do {let {x167 = Zero};
                                               let {x166 = Succ x167};
                                               let {x165 = Succ x166};
                                               let {x164 = Succ x165};
                                               x168 <- case x1 of
                                                       {Succ y168 -> return y168; _ -> mzero};
                                               guard (x168 == x164);
                                               x0 <- _____gtoMinmaxoOII x2 x3;
                                               return x0}]
______minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x136 = Zero};
                                              let {x135 = Succ x136};
                                              leoII x1 x135;
                                              let {x138 = Zero};
                                              let {x137 = Succ x138};
                                              let {x143 = Zero};
                                              let {x142 = Succ x143};
                                              let {x141 = Succ x142};
                                              let {x140 = Succ x141};
                                              let {x139 = Succ x140};
                                              minmaxoIIII x137 x2 x3 x139;
                                              let {x0 = x1};
                                              return x0},
                                          do {let {x144 = Zero};
                                              x145 <- case x1 of
                                                      {Succ y145 -> return y145; _ -> mzero};
                                              guard (x145 == x144);
                                              x0 <- ____gtoMinmaxoOII x2 x3;
                                              return x0}]
_____gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                     let {x169 = Zero};
                                     let {x171 = Zero};
                                     let {x170 = Succ x171};
                                     minmaxoIIII x169 x1 x2 x170;
                                     return x0},
                                 do {x3 <- ______gtoMinmaxoIIO x1 x2;
                                     let {x0 = Succ x3};
                                     return x0}]
______gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                      let {x173 = Zero};
                                      let {x172 = Succ x173};
                                      let {x175 = Zero};
                                      let {x174 = Succ x175};
                                      minmaxoIIII x172 x0 x1 x174;
                                      return x2},
                                  do {x3 <- _______gtoMinmaxoIIO x0 x1;
                                      let {x2 = Succ x3};
                                      return x2}]
_______gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                       let {x178 = Zero};
                                       let {x177 = Succ x178};
                                       let {x176 = Succ x177};
                                       let {x180 = Zero};
                                       let {x179 = Succ x180};
                                       minmaxoIIII x176 x0 x1 x179;
                                       return x2},
                                   do {x3 <- ________gtoMinmaxoIIO x0 x1;
                                       let {x2 = Succ x3};
                                       return x2}]
________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                        let {x184 = Zero};
                                        let {x183 = Succ x184};
                                        let {x182 = Succ x183};
                                        let {x181 = Succ x182};
                                        let {x186 = Zero};
                                        let {x185 = Succ x186};
                                        minmaxoIIII x181 x0 x1 x185;
                                        return x2}]
____gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                    let {x146 = Zero};
                                    let {x151 = Zero};
                                    let {x150 = Succ x151};
                                    let {x149 = Succ x150};
                                    let {x148 = Succ x149};
                                    let {x147 = Succ x148};
                                    minmaxoIIII x146 x1 x2 x147;
                                    return x0}]
____minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {x0 <- ____minmaxoMinmaxoOIII x1 x2 x3;
                                                   return x0},
                                               do {x0 <- _____minmaxoMinmaxoOIII x1 x2 x3;
                                                   return x0}]
_____minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x109 = Zero};
                                             let {x108 = Succ x109};
                                             let {x107 = Succ x108};
                                             let {x106 = Succ x107};
                                             let {x105 = Succ x106};
                                             leoII x1 x105;
                                             let {x114 = Zero};
                                             let {x113 = Succ x114};
                                             let {x112 = Succ x113};
                                             let {x111 = Succ x112};
                                             let {x110 = Succ x111};
                                             let {x115 = Zero};
                                             minmaxoIIII x110 x2 x3 x115;
                                             let {x0 = x1};
                                             return x0},
                                         do {let {x119 = Zero};
                                             let {x118 = Succ x119};
                                             let {x117 = Succ x118};
                                             let {x116 = Succ x117};
                                             x120 <- case x1 of
                                                     {Succ y120 -> return y120; _ -> mzero};
                                             guard (x120 == x116);
                                             x0 <- gtoMinmaxoOII x2 x3;
                                             return x0}]
____minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x99 = Zero};
                                            let {x104 = Zero};
                                            let {x103 = Succ x104};
                                            let {x102 = Succ x103};
                                            let {x101 = Succ x102};
                                            let {x100 = Succ x101};
                                            minmaxoIIII x99 x2 x3 x100;
                                            guard (x1 == Zero);
                                            let {x0 = x1};
                                            return x0}]
gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                let {x121 = Zero};
                                let {x122 = Zero};
                                minmaxoIIII x121 x1 x2 x122;
                                return x0},
                            do {x3 <- _gtoMinmaxoIIO x1 x2; let {x0 = Succ x3}; return x0}]
_gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                 let {x124 = Zero};
                                 let {x123 = Succ x124};
                                 let {x125 = Zero};
                                 minmaxoIIII x123 x0 x1 x125;
                                 return x2},
                             do {x3 <- __gtoMinmaxoIIO x0 x1; let {x2 = Succ x3}; return x2}]
__gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                  let {x128 = Zero};
                                  let {x127 = Succ x128};
                                  let {x126 = Succ x127};
                                  let {x129 = Zero};
                                  minmaxoIIII x126 x0 x1 x129;
                                  return x2},
                              do {x3 <- ___gtoMinmaxoIIO x0 x1; let {x2 = Succ x3}; return x2}]
___gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                   let {x133 = Zero};
                                   let {x132 = Succ x133};
                                   let {x131 = Succ x132};
                                   let {x130 = Succ x131};
                                   let {x134 = Zero};
                                   minmaxoIIII x130 x0 x1 x134;
                                   return x2}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5 = Immature $ msum [do {(x0,
                                                                          x1) <- Immature (minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5);
                                                                         return (x0, x1)},
                                                                     do {(x0,
                                                                          x1) <- Immature (_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5);
                                                                         return (x0, x1)}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5 = Immature $ msum [do {let {x258 = Zero};
                                                                   let {x257 = Succ x258};
                                                                   let {x256 = Succ x257};
                                                                   let {x255 = Succ x256};
                                                                   let {x254 = Succ x255};
                                                                   let {x262 = Zero};
                                                                   let {x261 = Succ x262};
                                                                   let {x260 = Succ x261};
                                                                   let {x259 = Succ x260};
                                                                   (x1,
                                                                    x0,
                                                                    x6,
                                                                    x7) <- ___leoMinmaxoMinmaxoMinmaxoOIOIOIIO x2 x3 x4 x5;
                                                                   minmaxoIIII x254 x6 x7 x259;
                                                                   return (x0, x1)},
                                                               do {let {x267 = Zero};
                                                                   let {x266 = Succ x267};
                                                                   let {x265 = Succ x266};
                                                                   let {x264 = Succ x265};
                                                                   let {x263 = Succ x264};
                                                                   (x0,
                                                                    x8) <- ___gtoMinmaxoMinmaxoMinmaxoOOIII x3 x4 x5;
                                                                   x1 <- minmaxoOIII x2 x8 x263;
                                                                   return (x0, x1)}]
___gtoMinmaxoMinmaxoMinmaxoOOIII x2 x3 x4 = Immature $ msum [do {let {x0 = Zero};
                                                      x1 <- ________minmaxoMinmaxoMinmaxoOIII x2 x3 x4;
                                                      return (x0, x1)},
                                                  do {(x1,
                                                       x5) <- ____gtoMinmaxoMinmaxoMinmaxoOIIIO x2 x3 x4;
                                                      let {x0 = Succ x5};
                                                      return (x0, x1)}]
____gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                       x0 <- _________minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                       return (x0, x4)},
                                                   do {(x0,
                                                        x5) <- _____gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3;
                                                       let {x4 = Succ x5};
                                                       return (x0, x4)}]
_____gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                        x0 <- __________minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                        return (x0, x4)},
                                                    do {(x0,
                                                         x5) <- ______gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3;
                                                        let {x4 = Succ x5};
                                                        return (x0, x4)}]
__________minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {x0 <- _______________minmaxoMinmaxoOIII x1 x2 x3;
                                                         return x0},
                                                     do {x0 <- ________________minmaxoMinmaxoOIII x1 x2 x3;
                                                         return x0}]
________________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x398 = Zero};
                                                        let {x397 = Succ x398};
                                                        let {x396 = Succ x397};
                                                        let {x395 = Succ x396};
                                                        leoII x1 x395;
                                                        let {x402 = Zero};
                                                        let {x401 = Succ x402};
                                                        let {x400 = Succ x401};
                                                        let {x399 = Succ x400};
                                                        let {x405 = Zero};
                                                        let {x404 = Succ x405};
                                                        let {x403 = Succ x404};
                                                        minmaxoIIII x399 x2 x3 x403;
                                                        let {x0 = x1};
                                                        return x0},
                                                    do {let {x408 = Zero};
                                                        let {x407 = Succ x408};
                                                        let {x406 = Succ x407};
                                                        x409 <- case x1 of
                                                                {Succ y409 -> return y409;
                                                                 _ -> mzero};
                                                        guard (x409 == x406);
                                                        x0 <- ________________________gtoMinmaxoOII x2 x3;
                                                        return x0}]
________________________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                                        let {x410 = Zero};
                                                        let {x413 = Zero};
                                                        let {x412 = Succ x413};
                                                        let {x411 = Succ x412};
                                                        minmaxoIIII x410 x1 x2 x411;
                                                        return x0},
                                                    do {x3 <- _________________________gtoMinmaxoIIO x1 x2;
                                                        let {x0 = Succ x3};
                                                        return x0}]
_________________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                         let {x415 = Zero};
                                                         let {x414 = Succ x415};
                                                         let {x418 = Zero};
                                                         let {x417 = Succ x418};
                                                         let {x416 = Succ x417};
                                                         minmaxoIIII x414 x0 x1 x416;
                                                         return x2},
                                                     do {x3 <- __________________________gtoMinmaxoIIO x0 x1;
                                                         let {x2 = Succ x3};
                                                         return x2}]
__________________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                          let {x421 = Zero};
                                                          let {x420 = Succ x421};
                                                          let {x419 = Succ x420};
                                                          let {x424 = Zero};
                                                          let {x423 = Succ x424};
                                                          let {x422 = Succ x423};
                                                          minmaxoIIII x419 x0 x1 x422;
                                                          return x2}]
_______________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x373 = Zero};
                                                       let {x372 = Succ x373};
                                                       let {x371 = Succ x372};
                                                       leoII x1 x371;
                                                       let {x376 = Zero};
                                                       let {x375 = Succ x376};
                                                       let {x374 = Succ x375};
                                                       let {x380 = Zero};
                                                       let {x379 = Succ x380};
                                                       let {x378 = Succ x379};
                                                       let {x377 = Succ x378};
                                                       minmaxoIIII x374 x2 x3 x377;
                                                       let {x0 = x1};
                                                       return x0},
                                                   do {let {x382 = Zero};
                                                       let {x381 = Succ x382};
                                                       x383 <- case x1 of
                                                               {Succ y383 -> return y383;
                                                                _ -> mzero};
                                                       guard (x383 == x381);
                                                       x0 <- ______________________gtoMinmaxoOII x2 x3;
                                                       return x0}]
______________________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                                      let {x384 = Zero};
                                                      let {x388 = Zero};
                                                      let {x387 = Succ x388};
                                                      let {x386 = Succ x387};
                                                      let {x385 = Succ x386};
                                                      minmaxoIIII x384 x1 x2 x385;
                                                      return x0},
                                                  do {x3 <- _______________________gtoMinmaxoIIO x1 x2;
                                                      let {x0 = Succ x3};
                                                      return x0}]
_______________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                       let {x390 = Zero};
                                                       let {x389 = Succ x390};
                                                       let {x394 = Zero};
                                                       let {x393 = Succ x394};
                                                       let {x392 = Succ x393};
                                                       let {x391 = Succ x392};
                                                       minmaxoIIII x389 x0 x1 x391;
                                                       return x2}]
______gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                         x0 <- ___________minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                         return (x0, x4)}]
___________minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {x0 <- _________________minmaxoMinmaxoOIII x1 x2 x3;
                                                          return x0}]
_________________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x428 = Zero};
                                                         let {x427 = Succ x428};
                                                         let {x426 = Succ x427};
                                                         let {x425 = Succ x426};
                                                         leoII x1 x425;
                                                         let {x432 = Zero};
                                                         let {x431 = Succ x432};
                                                         let {x430 = Succ x431};
                                                         let {x429 = Succ x430};
                                                         let {x436 = Zero};
                                                         let {x435 = Succ x436};
                                                         let {x434 = Succ x435};
                                                         let {x433 = Succ x434};
                                                         minmaxoIIII x429 x2 x3 x433;
                                                         let {x0 = x1};
                                                         return x0},
                                                     do {let {x439 = Zero};
                                                         let {x438 = Succ x439};
                                                         let {x437 = Succ x438};
                                                         x440 <- case x1 of
                                                                 {Succ y440 -> return y440;
                                                                  _ -> mzero};
                                                         guard (x440 == x437);
                                                         x0 <- ___________________________gtoMinmaxoOII x2 x3;
                                                         return x0}]
___________________________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                                           let {x441 = Zero};
                                                           let {x445 = Zero};
                                                           let {x444 = Succ x445};
                                                           let {x443 = Succ x444};
                                                           let {x442 = Succ x443};
                                                           minmaxoIIII x441 x1 x2 x442;
                                                           return x0},
                                                       do {x3 <- ____________________________gtoMinmaxoIIO x1 x2;
                                                           let {x0 = Succ x3};
                                                           return x0}]
____________________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                            let {x447 = Zero};
                                                            let {x446 = Succ x447};
                                                            let {x451 = Zero};
                                                            let {x450 = Succ x451};
                                                            let {x449 = Succ x450};
                                                            let {x448 = Succ x449};
                                                            minmaxoIIII x446 x0 x1 x448;
                                                            return x2},
                                                        do {x3 <- _____________________________gtoMinmaxoIIO x0 x1;
                                                            let {x2 = Succ x3};
                                                            return x2}]
_____________________________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                                             let {x454 = Zero};
                                                             let {x453 = Succ x454};
                                                             let {x452 = Succ x453};
                                                             let {x458 = Zero};
                                                             let {x457 = Succ x458};
                                                             let {x456 = Succ x457};
                                                             let {x455 = Succ x456};
                                                             minmaxoIIII x452 x0 x1 x455;
                                                             return x2}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x2 x3 x4 x5 = Immature $ msum [do {let {x30 = Zero};
                                                                  let {x29 = Succ x30};
                                                                  let {x28 = Succ x29};
                                                                  let {x27 = Succ x28};
                                                                  let {x35 = Zero};
                                                                  let {x34 = Succ x35};
                                                                  let {x33 = Succ x34};
                                                                  let {x32 = Succ x33};
                                                                  let {x31 = Succ x32};
                                                                  (x1,
                                                                   x0,
                                                                   x6,
                                                                   x7) <- leoMinmaxoMinmaxoMinmaxoOIOIOIIO x2 x3 x4 x5;
                                                                  minmaxoIIII x27 x6 x7 x31;
                                                                  return (x0, x1)},
                                                              do {let {x39 = Zero};
                                                                  let {x38 = Succ x39};
                                                                  let {x37 = Succ x38};
                                                                  let {x36 = Succ x37};
                                                                  (x0,
                                                                   x8) <- gtoMinmaxoMinmaxoMinmaxoOOIII x3 x4 x5;
                                                                  x1 <- minmaxoOIII x2 x8 x36;
                                                                  return (x0, x1)}]
gtoMinmaxoMinmaxoMinmaxoOOIII x2 x3 x4 = Immature $ msum [do {let {x0 = Zero};
                                                   x1 <- ____minmaxoMinmaxoMinmaxoOIII x2 x3 x4;
                                                   return (x0, x1)},
                                               do {(x1,
                                                    x5) <- _gtoMinmaxoMinmaxoMinmaxoOIIIO x2 x3 x4;
                                                   let {x0 = Succ x5};
                                                   return (x0, x1)}]
_gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                    x0 <- _____minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                    return (x0, x4)},
                                                do {(x0,
                                                     x5) <- __gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3;
                                                    let {x4 = Succ x5};
                                                    return (x0, x4)}]
__gtoMinmaxoMinmaxoMinmaxoOIIIO x1 x2 x3 = Immature $ msum [do {let {x4 = Zero};
                                                     x0 <- ______minmaxoMinmaxoMinmaxoOIII x1 x2 x3;
                                                     return (x0, x4)}]
______minmaxoMinmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {x0 <- ________minmaxoMinmaxoOIII x1 x2 x3;
                                                     return x0},
                                                 do {x0 <- _________minmaxoMinmaxoOIII x1 x2 x3;
                                                     return x0}]
_________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x218 = Zero};
                                                 let {x217 = Succ x218};
                                                 let {x216 = Succ x217};
                                                 let {x215 = Succ x216};
                                                 let {x214 = Succ x215};
                                                 leoII x1 x214;
                                                 let {x223 = Zero};
                                                 let {x222 = Succ x223};
                                                 let {x221 = Succ x222};
                                                 let {x220 = Succ x221};
                                                 let {x219 = Succ x220};
                                                 let {x226 = Zero};
                                                 let {x225 = Succ x226};
                                                 let {x224 = Succ x225};
                                                 minmaxoIIII x219 x2 x3 x224;
                                                 let {x0 = x1};
                                                 return x0},
                                             do {let {x230 = Zero};
                                                 let {x229 = Succ x230};
                                                 let {x228 = Succ x229};
                                                 let {x227 = Succ x228};
                                                 x231 <- case x1 of
                                                         {Succ y231 -> return y231; _ -> mzero};
                                                 guard (x231 == x227);
                                                 x0 <- ___________gtoMinmaxoOII x2 x3;
                                                 return x0}]
___________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                           let {x232 = Zero};
                                           let {x235 = Zero};
                                           let {x234 = Succ x235};
                                           let {x233 = Succ x234};
                                           minmaxoIIII x232 x1 x2 x233;
                                           return x0},
                                       do {x3 <- ____________gtoMinmaxoIIO x1 x2;
                                           let {x0 = Succ x3};
                                           return x0}]
____________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                            let {x237 = Zero};
                                            let {x236 = Succ x237};
                                            let {x240 = Zero};
                                            let {x239 = Succ x240};
                                            let {x238 = Succ x239};
                                            minmaxoIIII x236 x0 x1 x238;
                                            return x2},
                                        do {x3 <- _____________gtoMinmaxoIIO x0 x1;
                                            let {x2 = Succ x3};
                                            return x2}]
_____________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                             let {x243 = Zero};
                                             let {x242 = Succ x243};
                                             let {x241 = Succ x242};
                                             let {x246 = Zero};
                                             let {x245 = Succ x246};
                                             let {x244 = Succ x245};
                                             minmaxoIIII x241 x0 x1 x244;
                                             return x2},
                                         do {x3 <- ______________gtoMinmaxoIIO x0 x1;
                                             let {x2 = Succ x3};
                                             return x2}]
______________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                              let {x250 = Zero};
                                              let {x249 = Succ x250};
                                              let {x248 = Succ x249};
                                              let {x247 = Succ x248};
                                              let {x253 = Zero};
                                              let {x252 = Succ x253};
                                              let {x251 = Succ x252};
                                              minmaxoIIII x247 x0 x1 x251;
                                              return x2}]
________minmaxoMinmaxoOIII x1 x2 x3 = Immature $ msum [do {let {x189 = Zero};
                                                let {x188 = Succ x189};
                                                let {x187 = Succ x188};
                                                leoII x1 x187;
                                                let {x192 = Zero};
                                                let {x191 = Succ x192};
                                                let {x190 = Succ x191};
                                                let {x197 = Zero};
                                                let {x196 = Succ x197};
                                                let {x195 = Succ x196};
                                                let {x194 = Succ x195};
                                                let {x193 = Succ x194};
                                                minmaxoIIII x190 x2 x3 x193;
                                                let {x0 = x1};
                                                return x0},
                                            do {let {x199 = Zero};
                                                let {x198 = Succ x199};
                                                x200 <- case x1 of
                                                        {Succ y200 -> return y200; _ -> mzero};
                                                guard (x200 == x198);
                                                x0 <- _________gtoMinmaxoOII x2 x3;
                                                return x0}]
_________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x0 = Zero};
                                         let {x201 = Zero};
                                         let {x206 = Zero};
                                         let {x205 = Succ x206};
                                         let {x204 = Succ x205};
                                         let {x203 = Succ x204};
                                         let {x202 = Succ x203};
                                         minmaxoIIII x201 x1 x2 x202;
                                         return x0},
                                     do {x3 <- __________gtoMinmaxoIIO x1 x2;
                                         let {x0 = Succ x3};
                                         return x0}]
__________gtoMinmaxoIIO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                                          let {x208 = Zero};
                                          let {x207 = Succ x208};
                                          let {x213 = Zero};
                                          let {x212 = Succ x213};
                                          let {x211 = Succ x212};
                                          let {x210 = Succ x211};
                                          let {x209 = Succ x210};
                                          minmaxoIIII x207 x0 x1 x209;
                                          return x2}]
sortoO gen_sortoO_x15 gen_sortoO_x21 = Immature $ msum [do {let {x19 = Nil};
                                                 (x20, x15) <- do {x15 <- Immature gen_sortoO_x15;
                                                                   return (x15, x15)};
                                                 (x2, x16) <- case x15 of
                                                              {Cons y2 y16 -> return (y2, y16);
                                                               _ -> mzero};
                                                 (x3, x17) <- case x16 of
                                                              {Cons y3 y17 -> return (y3, y17);
                                                               _ -> mzero};
                                                 (x4, x18) <- case x17 of
                                                              {Cons y4 y18 -> return (y4, y18);
                                                               _ -> mzero};
                                                 x5 <- case x18 of
                                                       {Cons y5 y19 -> do {guard (x19 == y19);
                                                                           return y5};
                                                        _ -> mzero};
                                                 x1 <- Immature (minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x2 x3 x4 x5);
                                                 let {x0 = Cons x1 x20};
                                                 return x0},
                                             do {let {x25 = Nil};
                                                 (x26, x21) <- do {x21 <- Immature gen_sortoO_x21;
                                                                   return (x21, x21)};
                                                 (x2, x22) <- case x21 of
                                                              {Cons y2 y22 -> return (y2, y22);
                                                               _ -> mzero};
                                                 (x3, x23) <- case x22 of
                                                              {Cons y3 y23 -> return (y3, y23);
                                                               _ -> mzero};
                                                 (x4, x24) <- case x23 of
                                                              {Cons y4 y24 -> return (y4, y24);
                                                               _ -> mzero};
                                                 x5 <- case x24 of
                                                       {Cons y5 y25 -> do {guard (x25 == y25);
                                                                           return y5};
                                                        _ -> mzero};
                                                 x1 <- Immature (_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x2 x3 x4 x5);
                                                 let {x0 = Cons x1 x26};
                                                 return x0}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x3 x4 = Immature $ msum [do {(x5,
                                                                                               x9) <- ___minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x1 x2 x3 x4;
                                                                                              x0 <- _________________________minmaxoMinmaxoIOI x5 x9;
                                                                                              return x0}]
_________________________minmaxoMinmaxoIOI x0 x2 = Immature $ msum [do {let {x602 = Zero};
                                                             let {x601 = Succ x602};
                                                             let {x600 = Succ x601};
                                                             let {x599 = Succ x600};
                                                             let {x604 = Zero};
                                                             let {x606 = Zero};
                                                             let {x605 = Succ x606};
                                                             x603 <- case x0 of
                                                                     {Succ y603 -> return y603;
                                                                      _ -> mzero};
                                                             guard (x603 == x599);
                                                             x1 <- minmaxoIOII x604 x2 x605;
                                                             return x1},
                                                         do {let {x607 = Zero};
                                                             let {x609 = Zero};
                                                             let {x614 = Zero};
                                                             let {x613 = Succ x614};
                                                             let {x612 = Succ x613};
                                                             let {x611 = Succ x612};
                                                             let {x610 = Succ x611};
                                                             x608 <- case x0 of
                                                                     {Succ y608 -> return y608;
                                                                      _ -> mzero};
                                                             guard (x608 == x607);
                                                             x1 <- minmaxoIOII x609 x2 x610;
                                                             return x1}]
minmaxoIOII x0 x2 x3 = Immature $ msum [do {guard (x2 == x3);
                                 let {x1 = x0};
                                 leoII x1 x3;
                                 return x1},
                             do {guard (x0 == x2); gtoII x2 x3; let {x1 = x3}; return x1}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x3 x4 = Immature $ msum [do {(x5,
                                                                                              x6) <- Immature (minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x1 x2 x3 x4);
                                                                                             x0 <- __________________minmaxoMinmaxoIOI x5 x6;
                                                                                             return x0},
                                                                                         do {(x5,
                                                                                              x6) <- Immature (_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIIII x1 x2 x3 x4);
                                                                                             x0 <- ________________________minmaxoMinmaxoIOI x5 x6;
                                                                                             return x0}]
________________________minmaxoMinmaxoIOI x0 x2 = Immature $ msum [do {let {x570 = Zero};
                                                            let {x569 = Succ x570};
                                                            let {x568 = Succ x569};
                                                            let {x572 = Zero};
                                                            let {x574 = Zero};
                                                            let {x573 = Succ x574};
                                                            x571 <- case x0 of
                                                                    {Succ y571 -> return y571;
                                                                     _ -> mzero};
                                                            guard (x571 == x568);
                                                            x1 <- minmaxoIOII x572 x2 x573;
                                                            return x1},
                                                        do {let {x575 = Zero};
                                                            let {x577 = Zero};
                                                            x576 <- case x0 of
                                                                    {Succ y576 -> return y576;
                                                                     _ -> mzero};
                                                            guard (x576 == x575);
                                                            x1 <- ______________________gtoMinmaxoIOI x577 x2;
                                                            return x1}]
______________________gtoMinmaxoIOI x0 x2 = Immature $ msum [do {let {x384 = Zero};
                                                      let {x388 = Zero};
                                                      let {x387 = Succ x388};
                                                      let {x386 = Succ x387};
                                                      let {x385 = Succ x386};
                                                      guard (x0 == Zero);
                                                      x1 <- minmaxoIOII x384 x2 x385;
                                                      return x1},
                                                  do {x3 <- case x0 of
                                                            {Succ y3 -> return y3; _ -> mzero};
                                                      x1 <- _______________________gtoMinmaxoOII x2 x3;
                                                      return x1}]
_______________________gtoMinmaxoOII x1 x2 = Immature $ msum [do {let {x390 = Zero};
                                                       let {x389 = Succ x390};
                                                       let {x394 = Zero};
                                                       let {x393 = Succ x394};
                                                       let {x392 = Succ x393};
                                                       let {x391 = Succ x392};
                                                       guard (x2 == Zero);
                                                       x0 <- minmaxoIOII x389 x1 x391;
                                                       return x0}]
__________________minmaxoMinmaxoIOI x0 x2 = Immature $ msum [do {let {x460 = Zero};
                                                      let {x459 = Succ x460};
                                                      let {x462 = Zero};
                                                      let {x464 = Zero};
                                                      let {x463 = Succ x464};
                                                      x461 <- case x0 of
                                                              {Succ y461 -> return y461;
                                                               _ -> mzero};
                                                      guard (x461 == x459);
                                                      x1 <- minmaxoIOII x462 x2 x463;
                                                      return x1},
                                                  do {let {x465 = Zero};
                                                      let {x467 = Zero};
                                                      let {x470 = Zero};
                                                      let {x469 = Succ x470};
                                                      let {x468 = Succ x469};
                                                      x466 <- case x0 of
                                                              {Succ y466 -> return y466;
                                                               _ -> mzero};
                                                      guard (x466 == x465);
                                                      x1 <- minmaxoIOII x467 x2 x468;
                                                      return x1}]