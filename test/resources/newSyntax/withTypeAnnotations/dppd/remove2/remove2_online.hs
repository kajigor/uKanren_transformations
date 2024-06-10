module Remove2_clean where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | Succ Term
    | Zero
    deriving (Show, Eq)
rrI x0 = msum [do {(x1, x9) <- case x0 of
                               {Cons y1 y9 -> return (y1, y9); _ -> mzero};
                   let {x8 = x9};
                   (x2, x3) <- case x8 of
                               {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                   gGIII x1 x2 x3;
                   return ()}]
gGIII x0 x1 x2 = msum [do {guard (x0 == x1);
                           (x3, x4) <- case x2 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           hGIII x1 x3 x4;
                           return ()},
                       do {_neqHGIII x0 x1 x2; return ()}]
_neqHGIII x0 x1 x2 = msum [do {let {x80 = Zero};
                               x3 <- case x1 of
                                     {Succ y3 -> return y3; _ -> mzero};
                               let {x81 = Succ x3};
                               hGIII x80 x81 x2;
                               guard (x0 == Zero);
                               return ()},
                           do {let {x83 = Zero};
                               guard (x1 == Zero);
                               x3 <- case x0 of
                                     {Succ y3 -> return y3; _ -> mzero};
                               let {x82 = Succ x3};
                               hGIII x82 x83 x2;
                               return ()},
                           do {x4 <- case x1 of
                                     {Succ y4 -> return y4; _ -> mzero};
                               let {x85 = Succ x4};
                               x86 <- case x0 of
                                      {Succ y86 -> return y86; _ -> mzero};
                               let {x5 = x86};
                               neqII x5 x4;
                               let {x84 = Succ x5};
                               hGIII x84 x85 x2;
                               return ()}]
hGIII x0 x1 x2 = msum [do {let {x11 = Nil};
                           let {x12 = Nil};
                           guard (x2 == Nil);
                           let {x10 = x1};
                           _______gIIIII x0 x1 x10 x11 x12;
                           return ()},
                       do {(x3, x4) <- case x2 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           _gGIIII x0 x1 x3 x4;
                           return ()}]
_______gIIIII x0 x1 x2 x3 x4 = msum [do {guard (x1 == x0);
                                         let {x60 = Zero};
                                         let {x63 = Zero};
                                         let {x67 = Zero};
                                         let {x66 = Succ x67};
                                         let {x65 = Succ x66};
                                         let {x68 = Nil};
                                         let {x64 = Cons x65 x68};
                                         let {x62 = Cons x63 x64};
                                         fII x4 x62;
                                         x61 <- case x0 of
                                                {Succ y61 -> return y61; _ -> mzero};
                                         guard (x61 == x60);
                                         return ()},
                                     do {let {x70 = Zero};
                                         let {x69 = Succ x70};
                                         neqII x69 x1;
                                         let {x71 = Zero};
                                         let {x74 = Zero};
                                         let {x78 = Zero};
                                         let {x77 = Succ x78};
                                         let {x76 = Succ x77};
                                         let {x79 = Nil};
                                         let {x75 = Cons x76 x79};
                                         let {x73 = Cons x74 x75};
                                         (x5, x6) <- case x3 of
                                                     {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                         _____gIIII x2 x73 x5 x6;
                                         x72 <- case x0 of
                                                {Succ y72 -> return y72; _ -> mzero};
                                         guard (x72 == x71);
                                         return ()}]
_____gIIII x0 x1 x2 x3 = msum [do {guard (x0 == x2);
                                   (x28, x4) <- case x1 of
                                                {Cons y28 y4 -> return (y28, y4); _ -> mzero};
                                   guard (x28 == x2);
                                   fII x3 x4;
                                   return ()},
                               do {neqII x0 x2;
                                   (x29, x4) <- case x1 of
                                                {Cons y29 y4 -> return (y29, y4); _ -> mzero};
                                   guard (x29 == x0);
                                   hIII x2 x3 x4;
                                   return ()}]
_gGIIII x0 x1 x2 x3 = msum [do {guard (x1 == x2);
                                fGIII x0 x3 x2;
                                return ()},
                            do {neqHGIIII x0 x1 x2 x3; return ()}]
fII x0 x1 = msum [do {guard (x1 == Nil);
                      guard (x0 == Nil);
                      return ()},
                  do {(x2, x3) <- case x0 of
                                  {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                      hIII x2 x3 x1;
                      return ()}]
fGIII x0 x1 x2 = msum [do {let {x14 = Nil};
                           let {x15 = Nil};
                           guard (x1 == Nil);
                           let {x13 = x2};
                           _______gIIIII x0 x2 x13 x14 x15;
                           return ()},
                       do {(x3, x4) <- case x1 of
                                       {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                           let {x16 = x2};
                           __hGIIIII x0 x4 x3 x2 x16;
                           return ()}]
__hGIIIII x0 x1 x2 x3 x4 = msum [do {let {x43 = Nil};
                                     let {x42 = Cons x2 x43};
                                     let {x46 = Nil};
                                     guard (x1 == Nil);
                                     let {x45 = x2};
                                     let {x44 = Cons x45 x46};
                                     _______gIIIII x0 x3 x4 x42 x44;
                                     return ()},
                                 do {(x5, x6) <- case x1 of
                                                 {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                     ___gGIIIIII x0 x2 x3 x4 x5 x6;
                                     return ()}]
___gGIIIIII x0 x1 x2 x3 x4 x5 = msum [do {guard (x1 == x4);
                                          _fGIIIII x0 x2 x3 x5 x4;
                                          return ()},
                                      do {neqII x1 x4;
                                          let {x49 = x1};
                                          x6 <- hIIO x4 x5;
                                          let {x47 = Cons x1 x6};
                                          let {x50 = x6};
                                          let {x48 = Cons x49 x50};
                                          _______gIIIII x0 x2 x3 x47 x48;
                                          return ()}]
_fGIIIII x0 x1 x2 x3 x4 = msum [do {let {x52 = Nil};
                                    let {x51 = Cons x4 x52};
                                    let {x55 = Nil};
                                    guard (x3 == Nil);
                                    let {x54 = x4};
                                    let {x53 = Cons x54 x55};
                                    _______gIIIII x0 x1 x2 x51 x53;
                                    return ()},
                                do {(x5, x6) <- case x3 of
                                                {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                    let {x58 = x4};
                                    x7 <- hIIO x5 x6;
                                    let {x56 = Cons x4 x7};
                                    let {x59 = x7};
                                    let {x57 = Cons x58 x59};
                                    _______gIIIII x0 x1 x2 x56 x57;
                                    return ()}]
hIII x0 x1 x2 = msum [do {let {x25 = Nil};
                          (x26, x27) <- case x2 of
                                        {Cons y26 y27 -> return (y26, y27); _ -> mzero};
                          guard (x26 == x0);
                          guard (x27 == x25);
                          guard (x1 == Nil);
                          return ()},
                      do {(x3, x4) <- case x1 of
                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                          _____gIIII x0 x2 x3 x4;
                          return ()}]
hIIO x0 x1 = msum [do {let {x25 = Nil};
                       guard (x1 == Nil);
                       let {x26 = x0};
                       let {x27 = x25};
                       let {x2 = Cons x26 x27};
                       return x2},
                   do {(x3, x4) <- case x1 of
                                   {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                       x2 <- _____gIOII x0 x3 x4;
                       return x2}]
_____gIOII x0 x2 x3 = msum [do {guard (x0 == x2);
                                let {x28 = x2};
                                x4 <- fIO x3;
                                let {x1 = Cons x28 x4};
                                return x1},
                            do {neqII x0 x2;
                                let {x29 = x0};
                                x4 <- hIIO x2 x3;
                                let {x1 = Cons x29 x4};
                                return x1}]
fIO x0 = msum [do {let {x1 = Nil}; guard (x0 == Nil); return x1},
               do {(x2, x3) <- case x0 of
                               {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                   x1 <- hIIO x2 x3;
                   return x1}]
neqII x0 x1 = msum [do {x2 <- case x1 of
                              {Succ y2 -> return y2; _ -> mzero};
                        guard (x0 == Zero);
                        return ()},
                    do {guard (x1 == Zero);
                        x2 <- case x0 of
                              {Succ y2 -> return y2; _ -> mzero};
                        return ()},
                    do {x3 <- case x1 of
                              {Succ y3 -> return y3; _ -> mzero};
                        x4 <- case x0 of
                              {Succ y4 -> return y4; _ -> mzero};
                        neqII x4 x3;
                        return ()}]
neqHGIIII x0 x1 x2 x3 = msum [do {let {x31 = Zero};
                                  let {x32 = Zero};
                                  x4 <- case x2 of
                                        {Succ y4 -> return y4; _ -> mzero};
                                  let {x30 = Succ x4};
                                  __hGIIIII x0 x3 x30 x31 x32;
                                  guard (x1 == Zero);
                                  return ()},
                              do {let {x33 = Zero};
                                  guard (x2 == Zero);
                                  x4 <- case x1 of
                                        {Succ y4 -> return y4; _ -> mzero};
                                  let {x34 = Succ x4};
                                  let {x36 = x4};
                                  let {x35 = Succ x36};
                                  __hGIIIII x0 x3 x33 x34 x35;
                                  return ()},
                              do {x5 <- case x2 of
                                        {Succ y5 -> return y5; _ -> mzero};
                                  let {x37 = Succ x5};
                                  x41 <- case x1 of
                                         {Succ y41 -> return y41; _ -> mzero};
                                  let {x6 = x41};
                                  neqII x6 x5;
                                  let {x38 = Succ x6};
                                  let {x40 = x6};
                                  let {x39 = Succ x40};
                                  __hGIIIII x0 x3 x37 x38 x39;
                                  return ()}]
rrO gen_rrO_x8 = msum [do {(x9, x8) <- do {x8 <- gen_rrO_x8;
                                           return (x8, x8)};
                           (x2, x3) <- case x8 of
                                       {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                           x1 <- gGOII x2 x3;
                           let {x0 = Cons x1 x9};
                           return x0}]
gGOII x1 x2 = msum [do {(x3, x4) <- case x2 of
                                    {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                        hGIII x1 x3 x4;
                        let {x0 = x1};
                        return x0},
                    do {x0 <- _neqHGOII x1 x2; return x0}]
_neqHGOII x1 x2 = msum [do {let {x0 = Zero};
                            let {x80 = Zero};
                            x3 <- case x1 of
                                  {Succ y3 -> return y3; _ -> mzero};
                            let {x81 = Succ x3};
                            hGIII x80 x81 x2;
                            return x0},
                        do {let {x83 = Zero};
                            guard (x1 == Zero);
                            x82 <- hGOII x83 x2;
                            x3 <- case x82 of
                                  {Succ y3 -> return y3; _ -> mzero};
                            let {x0 = Succ x3};
                            return x0},
                        do {x4 <- case x1 of
                                  {Succ y4 -> return y4; _ -> mzero};
                            let {x85 = Succ x4};
                            x84 <- hGOII x85 x2;
                            x5 <- case x84 of
                                  {Succ y5 -> return y5; _ -> mzero};
                            neqII x5 x4;
                            let {x86 = x5};
                            let {x0 = Succ x86};
                            return x0}]
hGOII x1 x2 = msum [do {let {x11 = Nil};
                        let {x12 = Nil};
                        guard (x2 == Nil);
                        let {x10 = x1};
                        x0 <- _______gOIIII x1 x10 x11 x12;
                        return x0},
                    do {(x3, x4) <- case x2 of
                                    {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                        x0 <- _gGOIII x1 x3 x4;
                        return x0}]
_______gOIIII x1 x2 x3 x4 = msum [do {let {x60 = Zero};
                                      let {x63 = Zero};
                                      let {x67 = Zero};
                                      let {x66 = Succ x67};
                                      let {x65 = Succ x66};
                                      let {x68 = Nil};
                                      let {x64 = Cons x65 x68};
                                      let {x62 = Cons x63 x64};
                                      fII x4 x62;
                                      let {x0 = x1};
                                      x61 <- case x0 of
                                             {Succ y61 -> return y61; _ -> mzero};
                                      guard (x61 == x60);
                                      return x0},
                                  do {let {x70 = Zero};
                                      let {x69 = Succ x70};
                                      neqII x69 x1;
                                      let {x71 = Zero};
                                      let {x74 = Zero};
                                      let {x78 = Zero};
                                      let {x77 = Succ x78};
                                      let {x76 = Succ x77};
                                      let {x79 = Nil};
                                      let {x75 = Cons x76 x79};
                                      let {x73 = Cons x74 x75};
                                      (x5, x6) <- case x3 of
                                                  {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                      _____gIIII x2 x73 x5 x6;
                                      let {x72 = x71};
                                      let {x0 = Succ x72};
                                      return x0}]
_gGOIII x1 x2 x3 = msum [do {guard (x1 == x2);
                             x0 <- fGOII x3 x2;
                             return x0},
                         do {x0 <- neqHGOIII x1 x2 x3; return x0}]
fGOII x1 x2 = msum [do {let {x14 = Nil};
                        let {x15 = Nil};
                        guard (x1 == Nil);
                        let {x13 = x2};
                        x0 <- _______gOIIII x2 x13 x14 x15;
                        return x0},
                    do {(x3, x4) <- case x1 of
                                    {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                        let {x16 = x2};
                        x0 <- __hGOIIII x4 x3 x2 x16;
                        return x0}]
__hGOIIII x1 x2 x3 x4 = msum [do {let {x43 = Nil};
                                  let {x42 = Cons x2 x43};
                                  let {x46 = Nil};
                                  guard (x1 == Nil);
                                  let {x45 = x2};
                                  let {x44 = Cons x45 x46};
                                  x0 <- _______gOIIII x3 x4 x42 x44;
                                  return x0},
                              do {(x5, x6) <- case x1 of
                                              {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                  x0 <- ___gGOIIIII x2 x3 x4 x5 x6;
                                  return x0}]
___gGOIIIII x1 x2 x3 x4 x5 = msum [do {guard (x1 == x4);
                                       x0 <- _fGOIIII x2 x3 x5 x4;
                                       return x0},
                                   do {neqII x1 x4;
                                       let {x49 = x1};
                                       x6 <- hIIO x4 x5;
                                       let {x47 = Cons x1 x6};
                                       let {x50 = x6};
                                       let {x48 = Cons x49 x50};
                                       x0 <- _______gOIIII x2 x3 x47 x48;
                                       return x0}]
_fGOIIII x1 x2 x3 x4 = msum [do {let {x52 = Nil};
                                 let {x51 = Cons x4 x52};
                                 let {x55 = Nil};
                                 guard (x3 == Nil);
                                 let {x54 = x4};
                                 let {x53 = Cons x54 x55};
                                 x0 <- _______gOIIII x1 x2 x51 x53;
                                 return x0},
                             do {(x5, x6) <- case x3 of
                                             {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                                 let {x58 = x4};
                                 x7 <- hIIO x5 x6;
                                 let {x56 = Cons x4 x7};
                                 let {x59 = x7};
                                 let {x57 = Cons x58 x59};
                                 x0 <- _______gOIIII x1 x2 x56 x57;
                                 return x0}]
neqHGOIII x1 x2 x3 = msum [do {let {x31 = Zero};
                               let {x32 = Zero};
                               x4 <- case x2 of
                                     {Succ y4 -> return y4; _ -> mzero};
                               let {x30 = Succ x4};
                               guard (x1 == Zero);
                               x0 <- __hGOIIII x3 x30 x31 x32;
                               return x0},
                           do {let {x33 = Zero};
                               guard (x2 == Zero);
                               x4 <- case x1 of
                                     {Succ y4 -> return y4; _ -> mzero};
                               let {x34 = Succ x4};
                               let {x36 = x4};
                               let {x35 = Succ x36};
                               x0 <- __hGOIIII x3 x33 x34 x35;
                               return x0},
                           do {x5 <- case x2 of
                                     {Succ y5 -> return y5; _ -> mzero};
                               let {x37 = Succ x5};
                               x41 <- case x1 of
                                      {Succ y41 -> return y41; _ -> mzero};
                               let {x6 = x41};
                               neqII x6 x5;
                               let {x38 = Succ x6};
                               let {x40 = x6};
                               let {x39 = Succ x40};
                               x0 <- __hGOIIII x3 x37 x38 x39;
                               return x0}]