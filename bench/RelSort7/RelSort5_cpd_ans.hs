module RelSort5_cpd_ans where

import Stream
import Control.Monad
import Term

sortoI x0 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {(x1,
                                                                                                  x12) <- case x0 of
                                                                                                          {Cons y1
                                                                                                                y12 -> return (y1,
                                                                                                                               y12);
                                                                                                           _ -> mzero};
                                                                                                 let {x11 = x12};
                                                                                                 (x2,
                                                                                                  x3) <- case x11 of
                                                                                                         {Cons y2
                                                                                                               y3 -> return (y2,
                                                                                                                             y3);
                                                                                                          _ -> mzero};
                                                                                                 smallestoMinmaxoMinmaxoSmallestoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIII x1 x2 x3 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                 return ()}]
smallestoMinmaxoMinmaxoSmallestoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIII x0 x1 x2 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {(x3,
                                                                                                                                                                        x4) <- case x2 of
                                                                                                                                                                               {Cons y3
                                                                                                                                                                                     y4 -> return (y3,
                                                                                                                                                                                                   y4);
                                                                                                                                                                                _ -> mzero};
                                                                                                                                                                       smallestoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x0 x1 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                       return ()}]
smallestoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x0 x1 x2 x3 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x13 = Zero};
                                                                                                                                                                                let {x15 = Nil};
                                                                                                                                                                                (x16,
                                                                                                                                                                                 x17) <- case x3 of
                                                                                                                                                                                         {Cons y16
                                                                                                                                                                                               y17 -> return (y16,
                                                                                                                                                                                                              y17);
                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                guard (x16 == x13);
                                                                                                                                                                                let {x14 = x17};
                                                                                                                                                                                x4 <- case x14 of
                                                                                                                                                                                      {Cons y4
                                                                                                                                                                                            y15 -> do {guard (x15 == y15);
                                                                                                                                                                                                       return y4};
                                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                                minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x0 x1 x2 x4 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                                return ()},
                                                                                                                                                                            do {let {x21 = Nil};
                                                                                                                                                                                (x22,
                                                                                                                                                                                 x23) <- case x3 of
                                                                                                                                                                                         {Cons y22
                                                                                                                                                                                               y23 -> return (y22,
                                                                                                                                                                                                              y23);
                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                let {x18 = x22};
                                                                                                                                                                                x5 <- case x18 of
                                                                                                                                                                                      {Succ y5 -> return y5;
                                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                                let {x19 = x23};
                                                                                                                                                                                x20 <- case x19 of
                                                                                                                                                                                       {Cons y20
                                                                                                                                                                                             y21 -> do {guard (x21 == y21);
                                                                                                                                                                                                        return y20};
                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                x6 <- case x20 of
                                                                                                                                                                                      {Succ y6 -> return y6;
                                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                                leoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x5 x6 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                                return ()},
                                                                                                                                                                            do {let {x26 = Zero};
                                                                                                                                                                                let {x27 = Nil};
                                                                                                                                                                                let {x25 = Cons x26 x27};
                                                                                                                                                                                (x28,
                                                                                                                                                                                 x29) <- case x3 of
                                                                                                                                                                                         {Cons y28
                                                                                                                                                                                               y29 -> return (y28,
                                                                                                                                                                                                              y29);
                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                guard (x29 == x25);
                                                                                                                                                                                let {x24 = x28};
                                                                                                                                                                                x7 <- case x24 of
                                                                                                                                                                                      {Succ y7 -> return y7;
                                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                                let {x30 = Succ x7};
                                                                                                                                                                                minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x0 x1 x2 x30 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                                return ()},
                                                                                                                                                                            do {let {x34 = Nil};
                                                                                                                                                                                (x35,
                                                                                                                                                                                 x36) <- case x3 of
                                                                                                                                                                                         {Cons y35
                                                                                                                                                                                               y36 -> return (y35,
                                                                                                                                                                                                              y36);
                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                let {x31 = x35};
                                                                                                                                                                                x5 <- case x31 of
                                                                                                                                                                                      {Succ y5 -> return y5;
                                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                                let {x32 = x36};
                                                                                                                                                                                x33 <- case x32 of
                                                                                                                                                                                       {Cons y33
                                                                                                                                                                                             y34 -> do {guard (x34 == y34);
                                                                                                                                                                                                        return y33};
                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                x6 <- case x33 of
                                                                                                                                                                                      {Succ y6 -> return y6;
                                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                                gtoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x5 x6 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                                return ()}]
gtoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x314 = Zero};
                                                                                                                                                                              guard (x4 == Zero);
                                                                                                                                                                              x5 <- case x3 of
                                                                                                                                                                                    {Succ y5 -> return y5;
                                                                                                                                                                                     _ -> mzero};
                                                                                                                                                                              let {x315 = Succ x5};
                                                                                                                                                                              _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x314 x315 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                              return ()},
                                                                                                                                                                          do {x6 <- case x4 of
                                                                                                                                                                                    {Succ y6 -> return y6;
                                                                                                                                                                                     _ -> mzero};
                                                                                                                                                                              let {x316 = Succ x6};
                                                                                                                                                                              x318 <- case x3 of
                                                                                                                                                                                      {Succ y318 -> return y318;
                                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                              let {x7 = x318};
                                                                                                                                                                              gtoII x7 x6;
                                                                                                                                                                              let {x317 = Succ x7};
                                                                                                                                                                              _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x316 x317 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                              return ()}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x95 = Succ x3};
                                                                                                                                                                            let {x96 = Succ x4};
                                                                                                                                                                            guard (x2 == Zero);
                                                                                                                                                                            (x5,
                                                                                                                                                                             x6) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIO x0 x1 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                            minmaxoIIII x5 x6 x95 x96;
                                                                                                                                                                            return ()},
                                                                                                                                                                        do {let {x97 = Zero};
                                                                                                                                                                            let {x99 = Succ x3};
                                                                                                                                                                            let {x100 = Succ x4};
                                                                                                                                                                            x98 <- case x2 of
                                                                                                                                                                                   {Succ y98 -> return y98;
                                                                                                                                                                                    _ -> mzero};
                                                                                                                                                                            guard (x98 == x97);
                                                                                                                                                                            (x5,
                                                                                                                                                                             x6) <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIO x0 x1 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                            minmaxoIIII x5 x6 x99 x100;
                                                                                                                                                                            return ()},
                                                                                                                                                                        do {x7 <- case x3 of
                                                                                                                                                                                  {Succ y7 -> return y7;
                                                                                                                                                                                   _ -> mzero};
                                                                                                                                                                            x102 <- case x2 of
                                                                                                                                                                                    {Succ y102 -> return y102;
                                                                                                                                                                                     _ -> mzero};
                                                                                                                                                                            let {x101 = x102};
                                                                                                                                                                            x8 <- case x101 of
                                                                                                                                                                                  {Succ y8 -> return y8;
                                                                                                                                                                                   _ -> mzero};
                                                                                                                                                                            leoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x4 x8 x7 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                            return ()},
                                                                                                                                                                        do {let {x107 = Succ x4};
                                                                                                                                                                            guard (x3 == Zero);
                                                                                                                                                                            x104 <- case x2 of
                                                                                                                                                                                    {Succ y104 -> return y104;
                                                                                                                                                                                     _ -> mzero};
                                                                                                                                                                            let {x103 = x104};
                                                                                                                                                                            x9 <- case x103 of
                                                                                                                                                                                  {Succ y9 -> return y9;
                                                                                                                                                                                   _ -> mzero};
                                                                                                                                                                            let {x106 = Succ x9};
                                                                                                                                                                            let {x105 = Succ x106};
                                                                                                                                                                            (x5,
                                                                                                                                                                             x6) <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIO x0 x1 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                            minmaxoIIII x5 x6 x105 x107;
                                                                                                                                                                            return ()},
                                                                                                                                                                        do {x7 <- case x3 of
                                                                                                                                                                                  {Succ y7 -> return y7;
                                                                                                                                                                                   _ -> mzero};
                                                                                                                                                                            x109 <- case x2 of
                                                                                                                                                                                    {Succ y109 -> return y109;
                                                                                                                                                                                     _ -> mzero};
                                                                                                                                                                            let {x108 = x109};
                                                                                                                                                                            x8 <- case x108 of
                                                                                                                                                                                  {Succ y8 -> return y8;
                                                                                                                                                                                   _ -> mzero};
                                                                                                                                                                            gtoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x4 x8 x7 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                            return ()}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIO x0 x2 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x110 = Zero};
                                                                                                                                                    guard (x2 == Zero);
                                                                                                                                                    (x4,
                                                                                                                                                     x5,
                                                                                                                                                     x6) <- minmaxoMinmaxoMinmaxoMinmaxoIOOO x0 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                    (x1,
                                                                                                                                                     x3) <- _minmaxoMinmaxoIIIOOI x4 x5 x6 x110 gen_leoIO_x1;
                                                                                                                                                    return (x1,
                                                                                                                                                            x3)},
                                                                                                                                                do {let {x111 = Zero};
                                                                                                                                                    x112 <- case x2 of
                                                                                                                                                            {Succ y112 -> return y112;
                                                                                                                                                             _ -> mzero};
                                                                                                                                                    guard (x112 == x111);
                                                                                                                                                    guard (x0 == Zero);
                                                                                                                                                    (x1,
                                                                                                                                                     x3) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOO gen_leoIO_x1;
                                                                                                                                                    return (x1,
                                                                                                                                                            x3)},
                                                                                                                                                do {x114 <- case x2 of
                                                                                                                                                            {Succ y114 -> return y114;
                                                                                                                                                             _ -> mzero};
                                                                                                                                                    guard (x0 == Zero);
                                                                                                                                                    let {x113 = x114};
                                                                                                                                                    x7 <- case x113 of
                                                                                                                                                          {Succ y7 -> return y7;
                                                                                                                                                           _ -> mzero};
                                                                                                                                                    (x1,
                                                                                                                                                     x3) <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI x7 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_leoIO_x1;
                                                                                                                                                    return (x1,
                                                                                                                                                            x3)}]
_minmaxoMinmaxoIIIOOI x0 x1 x2 x5 gen_leoIO_x1 = Immature $ msum [do {guard (x5 == Zero);
                                                           x66 <- case x1 of
                                                                  {Succ y66 -> return y66;
                                                                   _ -> mzero};
                                                           guard (x66 == x5);
                                                           (x67, x4) <- minmaxoIIOO x0 x2;
                                                           x6 <- case x67 of
                                                                 {Succ y6 -> return y6; _ -> mzero};
                                                           let {x3 = Succ x6};
                                                           return (x3, x4)},
                                                       do {x7 <- case x5 of
                                                                 {Succ y7 -> return y7; _ -> mzero};
                                                           x70 <- case x1 of
                                                                  {Succ y70 -> return y70;
                                                                   _ -> mzero};
                                                           guard (x70 == x5);
                                                           (x4,
                                                            x8) <- leoMinmaxoIIOIO x0 x2 x7 gen_leoIO_x1;
                                                           let {x68 = Succ x8};
                                                           let {x69 = x68};
                                                           let {x3 = Succ x69};
                                                           return (x3, x4)},
                                                       do {let {x3 = Zero};
                                                           guard (x1 == x3);
                                                           let {x71 = Succ x5};
                                                           x4 <- minmaxoIIIO x0 x2 x71;
                                                           return (x3, x4)},
                                                       do {let {x72 = Zero};
                                                           x9 <- case x5 of
                                                                 {Succ y9 -> return y9; _ -> mzero};
                                                           let {x75 = Succ x9};
                                                           let {x74 = Succ x75};
                                                           let {x73 = x72};
                                                           let {x3 = Succ x73};
                                                           guard (x1 == x3);
                                                           x4 <- minmaxoIIIO x0 x2 x74;
                                                           return (x3, x4)},
                                                       do {x7 <- case x5 of
                                                                 {Succ y7 -> return y7; _ -> mzero};
                                                           let {x3 = x1};
                                                           x77 <- case x3 of
                                                                  {Succ y77 -> return y77;
                                                                   _ -> mzero};
                                                           let {x76 = x77};
                                                           x8 <- case x76 of
                                                                 {Succ y8 -> return y8; _ -> mzero};
                                                           x4 <- gtoMinmaxoIIOII x0 x2 x7 x8;
                                                           return (x3, x4)}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI x2 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_leoIO_x1 = Immature $ msum [do {guard (x2 == Zero);
                                                                                                                     (x120,
                                                                                                                      x119) <- do {x119 <- gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119;
                                                                                                                                   return (x119,
                                                                                                                                           x119)};
                                                                                                                     let {x0 = Succ x120};
                                                                                                                     x3 <- case x119 of
                                                                                                                           {Succ y3 -> return y3;
                                                                                                                            _ -> mzero};
                                                                                                                     x1 <- _minmaxoMinmaxoMinmaxoOI x3;
                                                                                                                     return (x0,
                                                                                                                             x1)},
                                                                                                                 do {let {x122 = Zero};
                                                                                                                     let {x121 = Succ x122};
                                                                                                                     x4 <- case x2 of
                                                                                                                           {Succ y4 -> return y4;
                                                                                                                            _ -> mzero};
                                                                                                                     let {x123 = x121};
                                                                                                                     let {x1 = Succ x123};
                                                                                                                     x5 <- _leoMinmaxoOI x4;
                                                                                                                     let {x125 = Succ x5};
                                                                                                                     let {x124 = Succ x125};
                                                                                                                     let {x126 = x124};
                                                                                                                     let {x0 = Succ x126};
                                                                                                                     return (x0,
                                                                                                                             x1)},
                                                                                                                 do {let {x127 = Zero};
                                                                                                                     let {x130 = Zero};
                                                                                                                     let {x129 = Succ x130};
                                                                                                                     let {x131 = Zero};
                                                                                                                     let {x133 = Succ x2};
                                                                                                                     let {x132 = Succ x133};
                                                                                                                     let {x128 = x127};
                                                                                                                     let {x0 = Succ x128};
                                                                                                                     (x6,
                                                                                                                      x7) <- minmaxoMinmaxoMinmaxoOIOI x129 x131 gen_leoIO_x1;
                                                                                                                     x1 <- minmaxoIIIO x6 x7 x132;
                                                                                                                     return (x0,
                                                                                                                             x1)},
                                                                                                                 do {let {x135 = Zero};
                                                                                                                     let {x134 = Succ x135};
                                                                                                                     x8 <- case x2 of
                                                                                                                           {Succ y8 -> return y8;
                                                                                                                            _ -> mzero};
                                                                                                                     let {x137 = Succ x8};
                                                                                                                     let {x136 = x134};
                                                                                                                     let {x0 = Succ x136};
                                                                                                                     x1 <- _minmaxoMinmaxoMinmaxoOI x137;
                                                                                                                     return (x0,
                                                                                                                             x1)},
                                                                                                                 do {let {x139 = Zero};
                                                                                                                     let {x138 = Succ x139};
                                                                                                                     x4 <- case x2 of
                                                                                                                           {Succ y4 -> return y4;
                                                                                                                            _ -> mzero};
                                                                                                                     let {x140 = x138};
                                                                                                                     let {x1 = Succ x140};
                                                                                                                     x5 <- _gtoMinmaxoIO x4;
                                                                                                                     let {x142 = Succ x5};
                                                                                                                     let {x141 = Succ x142};
                                                                                                                     let {x143 = x141};
                                                                                                                     let {x0 = Succ x143};
                                                                                                                     return (x0,
                                                                                                                             x1)}]
_gtoMinmaxoIO x0 = Immature $ msum [do {let {x1 = Zero};
                             let {x250 = Zero};
                             let {x249 = Succ x250};
                             let {x248 = Succ x249};
                             let {x247 = Succ x248};
                             let {x255 = Zero};
                             let {x254 = Succ x255};
                             let {x253 = Succ x254};
                             let {x252 = Succ x253};
                             let {x251 = Succ x252};
                             let {x259 = Zero};
                             let {x258 = Succ x259};
                             let {x257 = Succ x258};
                             let {x256 = Succ x257};
                             x2 <- case x0 of
                                   {Succ y2 -> return y2; _ -> mzero};
                             let {x263 = Succ x2};
                             let {x262 = Succ x263};
                             let {x261 = Succ x262};
                             let {x260 = Succ x261};
                             minmaxoIIII x247 x251 x256 x260;
                             return x1}]
_leoMinmaxoOI x1 = Immature $ msum [do {let {x234 = Zero};
                             let {x233 = Succ x234};
                             let {x232 = Succ x233};
                             let {x231 = Succ x232};
                             let {x239 = Zero};
                             let {x238 = Succ x239};
                             let {x237 = Succ x238};
                             let {x236 = Succ x237};
                             let {x235 = Succ x236};
                             let {x243 = Zero};
                             let {x242 = Succ x243};
                             let {x241 = Succ x242};
                             let {x240 = Succ x241};
                             guard (x1 == Zero);
                             x244 <- minmaxoIIIO x231 x235 x240;
                             x245 <- case x244 of
                                     {Succ y245 -> return y245; _ -> mzero};
                             x246 <- case x245 of
                                     {Succ y246 -> return y246; _ -> mzero};
                             x0 <- case x246 of
                                   {Succ y0 -> return y0; _ -> mzero};
                             return x0}]
_minmaxoMinmaxoMinmaxoOI x1 = Immature $ msum [do {let {x149 = Zero};
                                        let {x148 = Succ x149};
                                        let {x147 = Succ x148};
                                        let {x146 = Succ x147};
                                        let {x152 = Zero};
                                        let {x151 = Succ x152};
                                        let {x150 = Succ x151};
                                        let {x157 = Zero};
                                        let {x156 = Succ x157};
                                        let {x155 = Succ x156};
                                        let {x154 = Succ x155};
                                        let {x153 = Succ x154};
                                        let {x160 = Zero};
                                        let {x159 = Succ x160};
                                        let {x158 = Succ x159};
                                        let {x164 = Zero};
                                        let {x163 = Succ x164};
                                        guard (x1 == Zero);
                                        x161 <- _minmaxoMinmaxoIIIIOI x146 x150 x153 x158 x163;
                                        x162 <- case x161 of
                                                {Succ y162 -> return y162; _ -> mzero};
                                        x2 <- case x162 of
                                              {Succ y2 -> return y2; _ -> mzero};
                                        let {x144 = Succ x2};
                                        let {x145 = x144};
                                        let {x0 = Succ x145};
                                        return x0},
                                    do {x3 <- case x1 of
                                              {Succ y3 -> return y3; _ -> mzero};
                                        x4 <- _leoMinmaxoOI x3;
                                        let {x166 = Succ x4};
                                        let {x165 = Succ x166};
                                        let {x167 = x165};
                                        let {x0 = Succ x167};
                                        return x0},
                                    do {let {x0 = Zero};
                                        let {x171 = Zero};
                                        let {x170 = Succ x171};
                                        let {x169 = Succ x170};
                                        let {x168 = Succ x169};
                                        let {x174 = Zero};
                                        let {x173 = Succ x174};
                                        let {x172 = Succ x173};
                                        let {x179 = Zero};
                                        let {x178 = Succ x179};
                                        let {x177 = Succ x178};
                                        let {x176 = Succ x177};
                                        let {x175 = Succ x176};
                                        let {x180 = Zero};
                                        let {x182 = Succ x1};
                                        let {x181 = Succ x182};
                                        let {x184 = Zero};
                                        let {x183 = Succ x184};
                                        _minmaxoMinmaxoIIIIII x168 x172 x175 x180 x181 x183;
                                        return x0},
                                    do {let {x185 = Zero};
                                        let {x190 = Zero};
                                        let {x189 = Succ x190};
                                        let {x188 = Succ x189};
                                        let {x187 = Succ x188};
                                        let {x193 = Zero};
                                        let {x192 = Succ x193};
                                        let {x191 = Succ x192};
                                        let {x198 = Zero};
                                        let {x197 = Succ x198};
                                        let {x196 = Succ x197};
                                        let {x195 = Succ x196};
                                        let {x194 = Succ x195};
                                        let {x200 = Zero};
                                        let {x199 = Succ x200};
                                        let {x202 = Succ x1};
                                        let {x201 = Succ x202};
                                        let {x204 = Zero};
                                        let {x203 = Succ x204};
                                        _minmaxoMinmaxoIIIIII x187 x191 x194 x199 x201 x203;
                                        let {x186 = x185};
                                        let {x0 = Succ x186};
                                        return x0},
                                    do {let {x206 = Zero};
                                        let {x205 = Succ x206};
                                        let {x211 = Zero};
                                        let {x210 = Succ x211};
                                        let {x209 = Succ x210};
                                        let {x208 = Succ x209};
                                        let {x214 = Zero};
                                        let {x213 = Succ x214};
                                        let {x212 = Succ x213};
                                        let {x219 = Zero};
                                        let {x218 = Succ x219};
                                        let {x217 = Succ x218};
                                        let {x216 = Succ x217};
                                        let {x215 = Succ x216};
                                        let {x222 = Zero};
                                        let {x221 = Succ x222};
                                        let {x220 = Succ x221};
                                        let {x227 = Zero};
                                        let {x226 = Succ x227};
                                        x5 <- case x1 of
                                              {Succ y5 -> return y5; _ -> mzero};
                                        let {x225 = Succ x5};
                                        let {x224 = Succ x225};
                                        let {x223 = Succ x224};
                                        _minmaxoMinmaxoIIIIII x208 x212 x215 x220 x223 x226;
                                        let {x207 = x205};
                                        let {x0 = Succ x207};
                                        return x0},
                                    do {x3 <- case x1 of
                                              {Succ y3 -> return y3; _ -> mzero};
                                        x4 <- _gtoMinmaxoIO x3;
                                        let {x229 = Succ x4};
                                        let {x228 = Succ x229};
                                        let {x230 = x228};
                                        let {x0 = Succ x230};
                                        return x0}]
_minmaxoMinmaxoIIIIII x0 x1 x2 x3 x4 x5 = Immature $ msum [do {guard (x5 == Zero);
                                                    x6 <- case x3 of
                                                          {Succ y6 -> return y6; _ -> mzero};
                                                    let {x67 = Succ x6};
                                                    minmaxoIIII x0 x2 x67 x4;
                                                    x66 <- case x1 of
                                                           {Succ y66 -> return y66; _ -> mzero};
                                                    guard (x66 == x5);
                                                    return ()},
                                                do {x7 <- case x5 of
                                                          {Succ y7 -> return y7; _ -> mzero};
                                                    x69 <- case x3 of
                                                           {Succ y69 -> return y69; _ -> mzero};
                                                    x70 <- case x1 of
                                                           {Succ y70 -> return y70; _ -> mzero};
                                                    guard (x70 == x5);
                                                    let {x68 = x69};
                                                    x8 <- case x68 of
                                                          {Succ y8 -> return y8; _ -> mzero};
                                                    leoMinmaxoIIIII x0 x2 x4 x7 x8;
                                                    return ()},
                                                do {guard (x1 == x3);
                                                    let {x71 = Succ x5};
                                                    minmaxoIIII x0 x2 x71 x4;
                                                    guard (x3 == Zero);
                                                    return ()},
                                                do {guard (x1 == x3);
                                                    let {x72 = Zero};
                                                    x9 <- case x5 of
                                                          {Succ y9 -> return y9; _ -> mzero};
                                                    let {x75 = Succ x9};
                                                    let {x74 = Succ x75};
                                                    minmaxoIIII x0 x2 x74 x4;
                                                    x73 <- case x3 of
                                                           {Succ y73 -> return y73; _ -> mzero};
                                                    guard (x73 == x72);
                                                    return ()},
                                                do {guard (x1 == x3);
                                                    x7 <- case x5 of
                                                          {Succ y7 -> return y7; _ -> mzero};
                                                    x77 <- case x3 of
                                                           {Succ y77 -> return y77; _ -> mzero};
                                                    let {x76 = x77};
                                                    x8 <- case x76 of
                                                          {Succ y8 -> return y8; _ -> mzero};
                                                    gtoMinmaxoIIIII x0 x2 x4 x7 x8;
                                                    return ()}]
_minmaxoMinmaxoIIIIOI x0 x1 x2 x3 x5 = Immature $ msum [do {guard (x5 == Zero);
                                                 x6 <- case x3 of
                                                       {Succ y6 -> return y6; _ -> mzero};
                                                 let {x67 = Succ x6};
                                                 x66 <- case x1 of
                                                        {Succ y66 -> return y66; _ -> mzero};
                                                 guard (x66 == x5);
                                                 x4 <- minmaxoIIIO x0 x2 x67;
                                                 return x4},
                                             do {x7 <- case x5 of
                                                       {Succ y7 -> return y7; _ -> mzero};
                                                 x69 <- case x3 of
                                                        {Succ y69 -> return y69; _ -> mzero};
                                                 x70 <- case x1 of
                                                        {Succ y70 -> return y70; _ -> mzero};
                                                 guard (x70 == x5);
                                                 let {x68 = x69};
                                                 x8 <- case x68 of
                                                       {Succ y8 -> return y8; _ -> mzero};
                                                 x4 <- leoMinmaxoIIOII x0 x2 x7 x8;
                                                 return x4},
                                             do {guard (x1 == x3);
                                                 let {x71 = Succ x5};
                                                 guard (x3 == Zero);
                                                 x4 <- minmaxoIIIO x0 x2 x71;
                                                 return x4},
                                             do {guard (x1 == x3);
                                                 let {x72 = Zero};
                                                 x9 <- case x5 of
                                                       {Succ y9 -> return y9; _ -> mzero};
                                                 let {x75 = Succ x9};
                                                 let {x74 = Succ x75};
                                                 x73 <- case x3 of
                                                        {Succ y73 -> return y73; _ -> mzero};
                                                 guard (x73 == x72);
                                                 x4 <- minmaxoIIIO x0 x2 x74;
                                                 return x4},
                                             do {guard (x1 == x3);
                                                 x7 <- case x5 of
                                                       {Succ y7 -> return y7; _ -> mzero};
                                                 x77 <- case x3 of
                                                        {Succ y77 -> return y77; _ -> mzero};
                                                 let {x76 = x77};
                                                 x8 <- case x76 of
                                                       {Succ y8 -> return y8; _ -> mzero};
                                                 x4 <- gtoMinmaxoIIOII x0 x2 x7 x8;
                                                 return x4}]
gtoII x0 x1 = Immature $ msum [do {guard (x1 == Zero);
                        x2 <- case x0 of
                              {Succ y2 -> return y2; _ -> mzero};
                        return ()},
                    do {x3 <- case x1 of
                              {Succ y3 -> return y3; _ -> mzero};
                        x4 <- case x0 of
                              {Succ y4 -> return y4; _ -> mzero};
                        gtoII x4 x3;
                        return ()}]
gtoMinmaxoIIIII x0 x1 x2 x3 x4 = Immature $ msum [do {guard (x4 == Zero);
                                           x5 <- case x3 of
                                                 {Succ y5 -> return y5; _ -> mzero};
                                           let {x86 = Succ x5};
                                           let {x85 = Succ x86};
                                           let {x84 = Succ x85};
                                           minmaxoIIII x0 x1 x84 x2;
                                           return ()},
                                       do {x6 <- case x4 of
                                                 {Succ y6 -> return y6; _ -> mzero};
                                           x87 <- case x3 of
                                                  {Succ y87 -> return y87; _ -> mzero};
                                           let {x7 = x87};
                                           gtoII x7 x6;
                                           let {x90 = Succ x7};
                                           let {x89 = Succ x90};
                                           let {x88 = Succ x89};
                                           minmaxoIIII x0 x1 x88 x2;
                                           return ()}]
gtoMinmaxoIIOII x0 x1 x3 x4 = Immature $ msum [do {guard (x4 == Zero);
                                        x5 <- case x3 of
                                              {Succ y5 -> return y5; _ -> mzero};
                                        let {x86 = Succ x5};
                                        let {x85 = Succ x86};
                                        let {x84 = Succ x85};
                                        x2 <- minmaxoIIIO x0 x1 x84;
                                        return x2},
                                    do {x6 <- case x4 of
                                              {Succ y6 -> return y6; _ -> mzero};
                                        x87 <- case x3 of
                                               {Succ y87 -> return y87; _ -> mzero};
                                        let {x7 = x87};
                                        gtoII x7 x6;
                                        let {x90 = Succ x7};
                                        let {x89 = Succ x90};
                                        let {x88 = Succ x89};
                                        x2 <- minmaxoIIIO x0 x1 x88;
                                        return x2}]
gtoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x304 = Zero};
                                                                                                                                                                       let {x308 = Succ x2};
                                                                                                                                                                       guard (x4 == Zero);
                                                                                                                                                                       x7 <- case x3 of
                                                                                                                                                                             {Succ y7 -> return y7;
                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                       let {x307 = Succ x7};
                                                                                                                                                                       let {x306 = Succ x307};
                                                                                                                                                                       let {x305 = Succ x306};
                                                                                                                                                                       (x5,
                                                                                                                                                                        x6) <- __minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIOI x0 x1 x304 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                       minmaxoIIII x5 x6 x305 x308;
                                                                                                                                                                       return ()},
                                                                                                                                                                   do {let {x313 = Succ x2};
                                                                                                                                                                       x8 <- case x4 of
                                                                                                                                                                             {Succ y8 -> return y8;
                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                       let {x309 = Succ x8};
                                                                                                                                                                       x9 <- case x3 of
                                                                                                                                                                             {Succ y9 -> return y9;
                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                       gtoII x9 x8;
                                                                                                                                                                       let {x312 = Succ x9};
                                                                                                                                                                       let {x311 = Succ x312};
                                                                                                                                                                       let {x310 = Succ x311};
                                                                                                                                                                       (x5,
                                                                                                                                                                        x6) <- __minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIOI x0 x1 x309 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                       minmaxoIIII x5 x6 x310 x313;
                                                                                                                                                                       return ()}]
__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIOI x0 x2 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x274 = Succ x4};
                                                                                                                                                         guard (x2 == Zero);
                                                                                                                                                         (x5,
                                                                                                                                                          x6,
                                                                                                                                                          x7) <- minmaxoMinmaxoMinmaxoMinmaxoIOOO x0 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                         (x1,
                                                                                                                                                          x3) <- _minmaxoMinmaxoIIIOOI x5 x6 x7 x274 gen_leoIO_x1;
                                                                                                                                                         return (x1,
                                                                                                                                                                 x3)},
                                                                                                                                                     do {let {x275 = Zero};
                                                                                                                                                         x276 <- case x2 of
                                                                                                                                                                 {Succ y276 -> return y276;
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                         guard (x276 == x275);
                                                                                                                                                         guard (x0 == Zero);
                                                                                                                                                         (x1,
                                                                                                                                                          x3) <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_leoIO_x1;
                                                                                                                                                         return (x1,
                                                                                                                                                                 x3)},
                                                                                                                                                     do {let {x278 = Zero};
                                                                                                                                                         let {x277 = Succ x278};
                                                                                                                                                         let {x280 = Zero};
                                                                                                                                                         x279 <- case x2 of
                                                                                                                                                                 {Succ y279 -> return y279;
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                         guard (x279 == x277);
                                                                                                                                                         guard (x0 == Zero);
                                                                                                                                                         let {x281 = x280};
                                                                                                                                                         let {x1 = Succ x281};
                                                                                                                                                         x3 <- _minmaxoMinmaxoMinmaxoOI x4;
                                                                                                                                                         return (x1,
                                                                                                                                                                 x3)},
                                                                                                                                                     do {let {x283 = Zero};
                                                                                                                                                         let {x282 = Succ x283};
                                                                                                                                                         let {x288 = Zero};
                                                                                                                                                         x8 <- case x4 of
                                                                                                                                                               {Succ y8 -> return y8;
                                                                                                                                                                _ -> mzero};
                                                                                                                                                         x287 <- case x2 of
                                                                                                                                                                 {Succ y287 -> return y287;
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                         guard (x0 == Zero);
                                                                                                                                                         let {x284 = x282};
                                                                                                                                                         let {x3 = Succ x284};
                                                                                                                                                         let {x285 = x287};
                                                                                                                                                         x286 <- case x285 of
                                                                                                                                                                 {Succ y286 -> return y286;
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                         x9 <- case x286 of
                                                                                                                                                               {Succ y9 -> return y9;
                                                                                                                                                                _ -> mzero};
                                                                                                                                                         _leoMinmaxoII x8 x9;
                                                                                                                                                         let {x289 = x288};
                                                                                                                                                         let {x1 = Succ x289};
                                                                                                                                                         return (x1,
                                                                                                                                                                 x3)},
                                                                                                                                                     do {let {x293 = Zero};
                                                                                                                                                         guard (x4 == Zero);
                                                                                                                                                         x292 <- case x2 of
                                                                                                                                                                 {Succ y292 -> return y292;
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                         guard (x0 == Zero);
                                                                                                                                                         let {x290 = x292};
                                                                                                                                                         x291 <- case x290 of
                                                                                                                                                                 {Succ y291 -> return y291;
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                         x10 <- case x291 of
                                                                                                                                                                {Succ y10 -> return y10;
                                                                                                                                                                 _ -> mzero};
                                                                                                                                                         let {x295 = Succ x10};
                                                                                                                                                         let {x294 = x293};
                                                                                                                                                         let {x1 = Succ x294};
                                                                                                                                                         x3 <- _minmaxoMinmaxoMinmaxoOI x295;
                                                                                                                                                         return (x1,
                                                                                                                                                                 x3)},
                                                                                                                                                     do {let {x297 = Zero};
                                                                                                                                                         let {x296 = Succ x297};
                                                                                                                                                         let {x302 = Zero};
                                                                                                                                                         x8 <- case x4 of
                                                                                                                                                               {Succ y8 -> return y8;
                                                                                                                                                                _ -> mzero};
                                                                                                                                                         x301 <- case x2 of
                                                                                                                                                                 {Succ y301 -> return y301;
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                         guard (x0 == Zero);
                                                                                                                                                         let {x298 = x296};
                                                                                                                                                         let {x3 = Succ x298};
                                                                                                                                                         let {x299 = x301};
                                                                                                                                                         x300 <- case x299 of
                                                                                                                                                                 {Succ y300 -> return y300;
                                                                                                                                                                  _ -> mzero};
                                                                                                                                                         x9 <- case x300 of
                                                                                                                                                               {Succ y9 -> return y9;
                                                                                                                                                                _ -> mzero};
                                                                                                                                                         _gtoMinmaxoII x9 x8;
                                                                                                                                                         let {x303 = x302};
                                                                                                                                                         let {x1 = Succ x303};
                                                                                                                                                         return (x1,
                                                                                                                                                                 x3)}]
_gtoMinmaxoII x0 x1 = Immature $ msum [do {let {x250 = Zero};
                                let {x249 = Succ x250};
                                let {x248 = Succ x249};
                                let {x247 = Succ x248};
                                let {x255 = Zero};
                                let {x254 = Succ x255};
                                let {x253 = Succ x254};
                                let {x252 = Succ x253};
                                let {x251 = Succ x252};
                                let {x259 = Zero};
                                let {x258 = Succ x259};
                                let {x257 = Succ x258};
                                let {x256 = Succ x257};
                                guard (x1 == Zero);
                                x2 <- case x0 of
                                      {Succ y2 -> return y2; _ -> mzero};
                                let {x263 = Succ x2};
                                let {x262 = Succ x263};
                                let {x261 = Succ x262};
                                let {x260 = Succ x261};
                                minmaxoIIII x247 x251 x256 x260;
                                return ()}]
_leoMinmaxoII x0 x1 = Immature $ msum [do {let {x234 = Zero};
                                let {x233 = Succ x234};
                                let {x232 = Succ x233};
                                let {x231 = Succ x232};
                                let {x239 = Zero};
                                let {x238 = Succ x239};
                                let {x237 = Succ x238};
                                let {x236 = Succ x237};
                                let {x235 = Succ x236};
                                let {x243 = Zero};
                                let {x242 = Succ x243};
                                let {x241 = Succ x242};
                                let {x240 = Succ x241};
                                let {x246 = Succ x0};
                                let {x245 = Succ x246};
                                let {x244 = Succ x245};
                                minmaxoIIII x231 x235 x240 x244;
                                guard (x1 == Zero);
                                return ()}]
leoMinmaxoIIIII x0 x1 x2 x3 x4 = Immature $ msum [do {let {x79 = Succ x4};
                                           let {x78 = Succ x79};
                                           minmaxoIIII x0 x1 x78 x2;
                                           guard (x3 == Zero);
                                           return ()},
                                       do {x5 <- case x4 of
                                                 {Succ y5 -> return y5; _ -> mzero};
                                           let {x83 = Succ x5};
                                           let {x82 = Succ x83};
                                           let {x81 = Succ x82};
                                           minmaxoIIII x0 x1 x81 x2;
                                           x80 <- case x3 of
                                                  {Succ y80 -> return y80; _ -> mzero};
                                           let {x6 = x80};
                                           leoII x6 x5;
                                           return ()}]
leoII x0 x1 = Immature $ msum [do {guard (x0 == Zero); return ()},
                    do {x2 <- case x1 of
                              {Succ y2 -> return y2; _ -> mzero};
                        x3 <- case x0 of
                              {Succ y3 -> return y3; _ -> mzero};
                        leoII x3 x2;
                        return ()}]
leoMinmaxoIIOII x0 x1 x3 x4 = Immature $ msum [do {let {x79 = Succ x4};
                                        let {x78 = Succ x79};
                                        guard (x3 == Zero);
                                        x2 <- minmaxoIIIO x0 x1 x78;
                                        return x2},
                                    do {x5 <- case x4 of
                                              {Succ y5 -> return y5; _ -> mzero};
                                        let {x83 = Succ x5};
                                        let {x82 = Succ x83};
                                        let {x81 = Succ x82};
                                        x80 <- case x3 of
                                               {Succ y80 -> return y80; _ -> mzero};
                                        let {x6 = x80};
                                        leoII x6 x5;
                                        x2 <- minmaxoIIIO x0 x1 x81;
                                        return x2}]
leoMinmaxoIIOIO x0 x1 x3 gen_leoIO_x1 = Immature $ msum [do {guard (x3 == Zero);
                                                  (x78, x2) <- minmaxoIIOO x0 x1;
                                                  x79 <- case x78 of
                                                         {Succ y79 -> return y79; _ -> mzero};
                                                  x4 <- case x79 of
                                                        {Succ y4 -> return y4; _ -> mzero};
                                                  return (x2, x4)},
                                              do {x80 <- case x3 of
                                                         {Succ y80 -> return y80; _ -> mzero};
                                                  let {x6 = x80};
                                                  x5 <- leoIO x6 gen_leoIO_x1;
                                                  let {x4 = Succ x5};
                                                  let {x83 = Succ x5};
                                                  let {x82 = Succ x83};
                                                  let {x81 = Succ x82};
                                                  x2 <- minmaxoIIIO x0 x1 x81;
                                                  return (x2, x4)}]
leoIO x0 gen_leoIO_x1 = Immature $ msum [do {guard (x0 == Zero);
                                  x1 <- gen_leoIO_x1;
                                  return x1},
                              do {x3 <- case x0 of
                                        {Succ y3 -> return y3; _ -> mzero};
                                  x2 <- leoIO x3 gen_leoIO_x1;
                                  let {x1 = Succ x2};
                                  return x1}]
leoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x264 = Zero};
                                                                                                                                                                       let {x266 = Succ x4};
                                                                                                                                                                       let {x265 = Succ x266};
                                                                                                                                                                       let {x267 = Succ x2};
                                                                                                                                                                       guard (x3 == Zero);
                                                                                                                                                                       (x5,
                                                                                                                                                                        x6) <- __minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIOI x0 x1 x264 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                       minmaxoIIII x5 x6 x265 x267;
                                                                                                                                                                       return ()},
                                                                                                                                                                   do {let {x273 = Succ x2};
                                                                                                                                                                       x7 <- case x4 of
                                                                                                                                                                             {Succ y7 -> return y7;
                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                       let {x272 = Succ x7};
                                                                                                                                                                       let {x271 = Succ x272};
                                                                                                                                                                       let {x270 = Succ x271};
                                                                                                                                                                       x269 <- case x3 of
                                                                                                                                                                               {Succ y269 -> return y269;
                                                                                                                                                                                _ -> mzero};
                                                                                                                                                                       let {x8 = x269};
                                                                                                                                                                       leoII x8 x7;
                                                                                                                                                                       let {x268 = Succ x8};
                                                                                                                                                                       (x5,
                                                                                                                                                                        x6) <- __minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIOI x0 x1 x268 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                       minmaxoIIII x5 x6 x270 x273;
                                                                                                                                                                       return ()}]
leoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x91 = Zero};
                                                                                                                                                                              _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x91 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                              guard (x3 == Zero);
                                                                                                                                                                              return ()},
                                                                                                                                                                          do {x5 <- case x4 of
                                                                                                                                                                                    {Succ y5 -> return y5;
                                                                                                                                                                                     _ -> mzero};
                                                                                                                                                                              let {x93 = Succ x5};
                                                                                                                                                                              x94 <- case x3 of
                                                                                                                                                                                     {Succ y94 -> return y94;
                                                                                                                                                                                      _ -> mzero};
                                                                                                                                                                              let {x6 = x94};
                                                                                                                                                                              leoII x6 x5;
                                                                                                                                                                              let {x92 = Succ x6};
                                                                                                                                                                              _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIIII x0 x1 x2 x92 x93 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                                              return ()}]
minmaxoIIII x0 x1 x2 x3 = Immature $ msum [do {guard (x1 == x3);
                                    guard (x0 == x2);
                                    guard (x2 == Zero);
                                    return ()},
                                do {guard (x1 == x3);
                                    guard (x0 == x2);
                                    x4 <- case x3 of
                                          {Succ y4 -> return y4; _ -> mzero};
                                    x5 <- case x2 of
                                          {Succ y5 -> return y5; _ -> mzero};
                                    leoII x5 x4;
                                    return ()},
                                do {guard (x1 == x2);
                                    guard (x0 == x3);
                                    guard (x3 == Zero);
                                    x6 <- case x2 of
                                          {Succ y6 -> return y6; _ -> mzero};
                                    return ()},
                                do {guard (x1 == x2);
                                    guard (x0 == x3);
                                    x4 <- case x3 of
                                          {Succ y4 -> return y4; _ -> mzero};
                                    x5 <- case x2 of
                                          {Succ y5 -> return y5; _ -> mzero};
                                    gtoII x5 x4;
                                    return ()}]
minmaxoIIIO x0 x1 x2 = Immature $ msum [do {guard (x0 == x2);
                                 guard (x2 == Zero);
                                 let {x3 = x1};
                                 return x3},
                             do {guard (x0 == x2);
                                 x5 <- case x2 of
                                       {Succ y5 -> return y5; _ -> mzero};
                                 let {x3 = x1};
                                 x4 <- case x3 of
                                       {Succ y4 -> return y4; _ -> mzero};
                                 leoII x5 x4;
                                 return x3},
                             do {guard (x1 == x2);
                                 let {x3 = Zero};
                                 guard (x0 == x3);
                                 x6 <- case x2 of
                                       {Succ y6 -> return y6; _ -> mzero};
                                 return x3},
                             do {guard (x1 == x2);
                                 x5 <- case x2 of
                                       {Succ y5 -> return y5; _ -> mzero};
                                 let {x3 = x0};
                                 x4 <- case x3 of
                                       {Succ y4 -> return y4; _ -> mzero};
                                 gtoII x5 x4;
                                 return x3}]
minmaxoIIOO x0 x1 = Immature $ msum [do {let {x2 = Zero};
                              guard (x0 == x2);
                              let {x3 = x1};
                              return (x2, x3)},
                          do {let {x3 = x1};
                              x4 <- case x3 of
                                    {Succ y4 -> return y4; _ -> mzero};
                              let {x2 = x0};
                              x5 <- case x2 of
                                    {Succ y5 -> return y5; _ -> mzero};
                              leoII x5 x4;
                              return (x2, x3)},
                          do {let {x3 = Zero};
                              guard (x0 == x3);
                              let {x2 = x1};
                              x6 <- case x2 of
                                    {Succ y6 -> return y6; _ -> mzero};
                              return (x2, x3)},
                          do {let {x2 = x1};
                              x5 <- case x2 of
                                    {Succ y5 -> return y5; _ -> mzero};
                              let {x3 = x0};
                              x4 <- case x3 of
                                    {Succ y4 -> return y4; _ -> mzero};
                              gtoII x5 x4;
                              return (x2, x3)}]
minmaxoMinmaxoMinmaxoOIOI x1 x3 gen_leoIO_x1 = Immature $ msum [do {let {x42 = Zero};
                                                         let {x41 = Succ x42};
                                                         let {x40 = Succ x41};
                                                         let {x39 = Succ x40};
                                                         let {x45 = Zero};
                                                         let {x44 = Succ x45};
                                                         let {x43 = Succ x44};
                                                         let {x50 = Zero};
                                                         let {x49 = Succ x50};
                                                         let {x48 = Succ x49};
                                                         let {x47 = Succ x48};
                                                         let {x46 = Succ x47};
                                                         guard (x3 == Zero);
                                                         x4 <- case x1 of
                                                               {Succ y4 -> return y4; _ -> mzero};
                                                         (x0,
                                                          x2) <- _minmaxoMinmaxoIIIOOI x39 x43 x46 x4 gen_leoIO_x1;
                                                         return (x0, x2)},
                                                     do {let {x51 = Zero};
                                                         let {x56 = Zero};
                                                         let {x55 = Succ x56};
                                                         let {x54 = Succ x55};
                                                         let {x53 = Succ x54};
                                                         let {x59 = Zero};
                                                         let {x58 = Succ x59};
                                                         let {x57 = Succ x58};
                                                         let {x64 = Zero};
                                                         let {x63 = Succ x64};
                                                         let {x62 = Succ x63};
                                                         let {x61 = Succ x62};
                                                         let {x60 = Succ x61};
                                                         x5 <- case x3 of
                                                               {Succ y5 -> return y5; _ -> mzero};
                                                         let {x65 = Succ x5};
                                                         x52 <- case x1 of
                                                                {Succ y52 -> return y52;
                                                                 _ -> mzero};
                                                         guard (x52 == x51);
                                                         (x0,
                                                          x2) <- _minmaxoMinmaxoIIIOOI x53 x57 x60 x65 gen_leoIO_x1;
                                                         return (x0, x2)}]
minmaxoMinmaxoMinmaxoMinmaxoIOOO x0 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {x4 <- case x0 of
                                                                                {Succ y4 -> return y4;
                                                                                 _ -> mzero};
                                                                          (x1,
                                                                           x2,
                                                                           x3) <- minmaxoMinmaxoMinmaxoOOOI x4 gen_gtoOI_x2 gen_leoIO_x1;
                                                                          return (x1, x2, x3)}]
minmaxoMinmaxoMinmaxoOOOI x3 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x42 = Zero};
                                                                   let {x41 = Succ x42};
                                                                   let {x40 = Succ x41};
                                                                   let {x39 = Succ x40};
                                                                   let {x45 = Zero};
                                                                   let {x44 = Succ x45};
                                                                   let {x43 = Succ x44};
                                                                   let {x50 = Zero};
                                                                   let {x49 = Succ x50};
                                                                   let {x48 = Succ x49};
                                                                   let {x47 = Succ x48};
                                                                   let {x46 = Succ x47};
                                                                   guard (x3 == Zero);
                                                                   (x0,
                                                                    x2,
                                                                    x4) <- _minmaxoMinmaxoIIIOOO x39 x43 x46 gen_gtoOI_x2 gen_leoIO_x1;
                                                                   let {x1 = Succ x4};
                                                                   return (x0, x1, x2)},
                                                               do {let {x51 = Zero};
                                                                   let {x56 = Zero};
                                                                   let {x55 = Succ x56};
                                                                   let {x54 = Succ x55};
                                                                   let {x53 = Succ x54};
                                                                   let {x59 = Zero};
                                                                   let {x58 = Succ x59};
                                                                   let {x57 = Succ x58};
                                                                   let {x64 = Zero};
                                                                   let {x63 = Succ x64};
                                                                   let {x62 = Succ x63};
                                                                   let {x61 = Succ x62};
                                                                   let {x60 = Succ x61};
                                                                   x5 <- case x3 of
                                                                         {Succ y5 -> return y5;
                                                                          _ -> mzero};
                                                                   let {x65 = Succ x5};
                                                                   let {x52 = x51};
                                                                   let {x1 = Succ x52};
                                                                   (x0,
                                                                    x2) <- _minmaxoMinmaxoIIIOOI x53 x57 x60 x65 gen_leoIO_x1;
                                                                   return (x0, x1, x2)}]
_minmaxoMinmaxoIIIOOO x0 x1 x2 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x5 = Zero};
                                                                     x66 <- case x1 of
                                                                            {Succ y66 -> return y66;
                                                                             _ -> mzero};
                                                                     guard (x66 == x5);
                                                                     (x67, x4) <- minmaxoIIOO x0 x2;
                                                                     x6 <- case x67 of
                                                                           {Succ y6 -> return y6;
                                                                            _ -> mzero};
                                                                     let {x3 = Succ x6};
                                                                     return (x3, x4, x5)},
                                                                 do {x70 <- case x1 of
                                                                            {Succ y70 -> return y70;
                                                                             _ -> mzero};
                                                                     let {x5 = x70};
                                                                     x7 <- case x5 of
                                                                           {Succ y7 -> return y7;
                                                                            _ -> mzero};
                                                                     (x4,
                                                                      x8) <- leoMinmaxoIIOIO x0 x2 x7 gen_leoIO_x1;
                                                                     let {x68 = Succ x8};
                                                                     let {x69 = x68};
                                                                     let {x3 = Succ x69};
                                                                     return (x3, x4, x5)},
                                                                 do {let {x3 = Zero};
                                                                     guard (x1 == x3);
                                                                     (x71, x4) <- minmaxoIIOO x0 x2;
                                                                     x5 <- case x71 of
                                                                           {Succ y5 -> return y5;
                                                                            _ -> mzero};
                                                                     return (x3, x4, x5)},
                                                                 do {let {x72 = Zero};
                                                                     let {x73 = x72};
                                                                     let {x3 = Succ x73};
                                                                     guard (x1 == x3);
                                                                     (x74, x4) <- minmaxoIIOO x0 x2;
                                                                     x75 <- case x74 of
                                                                            {Succ y75 -> return y75;
                                                                             _ -> mzero};
                                                                     x9 <- case x75 of
                                                                           {Succ y9 -> return y9;
                                                                            _ -> mzero};
                                                                     let {x5 = Succ x9};
                                                                     return (x3, x4, x5)},
                                                                 do {let {x3 = x1};
                                                                     x77 <- case x3 of
                                                                            {Succ y77 -> return y77;
                                                                             _ -> mzero};
                                                                     let {x76 = x77};
                                                                     x8 <- case x76 of
                                                                           {Succ y8 -> return y8;
                                                                            _ -> mzero};
                                                                     (x4,
                                                                      x7) <- gtoMinmaxoIIOOI x0 x2 x8 gen_gtoOI_x2;
                                                                     let {x5 = Succ x7};
                                                                     return (x3, x4, x5)}]
gtoMinmaxoIIOOI x0 x1 x4 gen_gtoOI_x2 = Immature $ msum [do {guard (x4 == Zero);
                                                  (x84, x2) <- minmaxoIIOO x0 x1;
                                                  x85 <- case x84 of
                                                         {Succ y85 -> return y85; _ -> mzero};
                                                  x86 <- case x85 of
                                                         {Succ y86 -> return y86; _ -> mzero};
                                                  x5 <- case x86 of
                                                        {Succ y5 -> return y5; _ -> mzero};
                                                  let {x3 = Succ x5};
                                                  return (x2, x3)},
                                              do {x6 <- case x4 of
                                                        {Succ y6 -> return y6; _ -> mzero};
                                                  x7 <- gtoOI x6 gen_gtoOI_x2;
                                                  let {x90 = Succ x7};
                                                  let {x89 = Succ x90};
                                                  let {x88 = Succ x89};
                                                  let {x87 = x7};
                                                  let {x3 = Succ x87};
                                                  x2 <- minmaxoIIIO x0 x1 x88;
                                                  return (x2, x3)}]
gtoOI x1 gen_gtoOI_x2 = Immature $ msum [do {guard (x1 == Zero);
                                  (x0, x2) <- do {x2 <- gen_gtoOI_x2;
                                                  let {x0 = Succ x2};
                                                  return (x0, x2)};
                                  return x0},
                              do {x3 <- case x1 of
                                        {Succ y3 -> return y3; _ -> mzero};
                                  x4 <- gtoOI x3 gen_gtoOI_x2;
                                  let {x0 = Succ x4};
                                  return x0}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOO gen_leoIO_x1 = Immature $ msum [do {let {x116 = Zero};
                                                               let {x115 = Succ x116};
                                                               let {x117 = Zero};
                                                               (x3,
                                                                x4) <- minmaxoMinmaxoMinmaxoOIOI x115 x117 gen_leoIO_x1;
                                                               (x118, x1) <- minmaxoIIOO x3 x4;
                                                               x2 <- case x118 of
                                                                     {Succ y2 -> return y2;
                                                                      _ -> mzero};
                                                               let {x0 = Succ x2};
                                                               return (x0, x1)}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIO x0 x2 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {guard (x2 == Zero);
                                                                                                  (x4,
                                                                                                   x5,
                                                                                                   x6) <- minmaxoMinmaxoMinmaxoMinmaxoIOOO x0 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                  (x1,
                                                                                                   x3) <- minmaxoMinmaxoIIIOO x4 x5 x6;
                                                                                                  return (x1,
                                                                                                          x3)},
                                                                                              do {x7 <- case x2 of
                                                                                                        {Succ y7 -> return y7;
                                                                                                         _ -> mzero};
                                                                                                  (x4,
                                                                                                   x5,
                                                                                                   x6) <- minmaxoMinmaxoMinmaxoMinmaxoIOOO x0 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                  (x1,
                                                                                                   x3) <- _minmaxoMinmaxoIIIOOI x4 x5 x6 x7 gen_leoIO_x1;
                                                                                                  return (x1,
                                                                                                          x3)}]
minmaxoMinmaxoIIIOO x0 x1 x2 = Immature $ msum [do {guard (x1 == Zero);
                                         (x3, x4) <- minmaxoIIOO x0 x2;
                                         return (x3, x4)}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIIII x0 x1 x2 x3 gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x37 = Zero};
                                                                                                                      guard (x2 == Zero);
                                                                                                                      (x4,
                                                                                                                       x5) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIO x0 x1 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                      minmaxoIIII x4 x5 x37 x3;
                                                                                                                      return ()},
                                                                                                                  do {x6 <- case x2 of
                                                                                                                            {Succ y6 -> return y6;
                                                                                                                             _ -> mzero};
                                                                                                                      let {x38 = Succ x6};
                                                                                                                      (x4,
                                                                                                                       x5) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoIOIO x0 x1 gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                      minmaxoIIII x4 x5 x38 x3;
                                                                                                                      return ()}]
sortoO gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 gen_sortoO_x11 = Immature $ msum [do {(x12,
                                                                                                                                                                       x11) <- do {x11 <- gen_sortoO_x11;
                                                                                                                                                                                   return (x11,
                                                                                                                                                                                           x11)};
                                                                                                                                                                      (x2,
                                                                                                                                                                       x3) <- case x11 of
                                                                                                                                                                              {Cons y2
                                                                                                                                                                                    y3 -> return (y2,
                                                                                                                                                                                                  y3);
                                                                                                                                                                               _ -> mzero};
                                                                                                                                                                      x1 <- smallestoMinmaxoMinmaxoSmallestoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOII x2 x3 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                      let {x0 = Cons x1 x12};
                                                                                                                                                                      return x0}]
smallestoMinmaxoMinmaxoSmallestoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOII x1 x2 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {(x3,
                                                                                                                                                                                                                              x4) <- case x2 of
                                                                                                                                                                                                                                     {Cons y3
                                                                                                                                                                                                                                           y4 -> return (y3,
                                                                                                                                                                                                                                                         y4);
                                                                                                                                                                                                                                      _ -> mzero};
                                                                                                                                                                                                                             x0 <- smallestoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                             return x0}]
smallestoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {let {x13 = Zero};
                                                                                                                                                                                                                                      let {x15 = Nil};
                                                                                                                                                                                                                                      (x16,
                                                                                                                                                                                                                                       x17) <- case x3 of
                                                                                                                                                                                                                                               {Cons y16
                                                                                                                                                                                                                                                     y17 -> return (y16,
                                                                                                                                                                                                                                                                    y17);
                                                                                                                                                                                                                                                _ -> mzero};
                                                                                                                                                                                                                                      guard (x16 == x13);
                                                                                                                                                                                                                                      let {x14 = x17};
                                                                                                                                                                                                                                      x4 <- case x14 of
                                                                                                                                                                                                                                            {Cons y4
                                                                                                                                                                                                                                                  y15 -> do {guard (x15 == y15);
                                                                                                                                                                                                                                                             return y4};
                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                      x0 <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x4 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                      return x0},
                                                                                                                                                                                                                                  do {let {x21 = Nil};
                                                                                                                                                                                                                                      (x22,
                                                                                                                                                                                                                                       x23) <- case x3 of
                                                                                                                                                                                                                                               {Cons y22
                                                                                                                                                                                                                                                     y23 -> return (y22,
                                                                                                                                                                                                                                                                    y23);
                                                                                                                                                                                                                                                _ -> mzero};
                                                                                                                                                                                                                                      let {x18 = x22};
                                                                                                                                                                                                                                      x5 <- case x18 of
                                                                                                                                                                                                                                            {Succ y5 -> return y5;
                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                      let {x19 = x23};
                                                                                                                                                                                                                                      x20 <- case x19 of
                                                                                                                                                                                                                                             {Cons y20
                                                                                                                                                                                                                                                   y21 -> do {guard (x21 == y21);
                                                                                                                                                                                                                                                              return y20};
                                                                                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                                                                                      x6 <- case x20 of
                                                                                                                                                                                                                                            {Succ y6 -> return y6;
                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                      x0 <- leoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x5 x6 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                      return x0},
                                                                                                                                                                                                                                  do {let {x26 = Zero};
                                                                                                                                                                                                                                      let {x27 = Nil};
                                                                                                                                                                                                                                      let {x25 = Cons x26 x27};
                                                                                                                                                                                                                                      (x28,
                                                                                                                                                                                                                                       x29) <- case x3 of
                                                                                                                                                                                                                                               {Cons y28
                                                                                                                                                                                                                                                     y29 -> return (y28,
                                                                                                                                                                                                                                                                    y29);
                                                                                                                                                                                                                                                _ -> mzero};
                                                                                                                                                                                                                                      guard (x29 == x25);
                                                                                                                                                                                                                                      let {x24 = x28};
                                                                                                                                                                                                                                      x7 <- case x24 of
                                                                                                                                                                                                                                            {Succ y7 -> return y7;
                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                      let {x30 = Succ x7};
                                                                                                                                                                                                                                      x0 <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x30 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                      return x0},
                                                                                                                                                                                                                                  do {let {x34 = Nil};
                                                                                                                                                                                                                                      (x35,
                                                                                                                                                                                                                                       x36) <- case x3 of
                                                                                                                                                                                                                                               {Cons y35
                                                                                                                                                                                                                                                     y36 -> return (y35,
                                                                                                                                                                                                                                                                    y36);
                                                                                                                                                                                                                                                _ -> mzero};
                                                                                                                                                                                                                                      let {x31 = x35};
                                                                                                                                                                                                                                      x5 <- case x31 of
                                                                                                                                                                                                                                            {Succ y5 -> return y5;
                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                      let {x32 = x36};
                                                                                                                                                                                                                                      x33 <- case x32 of
                                                                                                                                                                                                                                             {Cons y33
                                                                                                                                                                                                                                                   y34 -> do {guard (x34 == y34);
                                                                                                                                                                                                                                                              return y33};
                                                                                                                                                                                                                                              _ -> mzero};
                                                                                                                                                                                                                                      x6 <- case x33 of
                                                                                                                                                                                                                                            {Succ y6 -> return y6;
                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                      x0 <- gtoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x5 x6 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                      return x0}]
gtoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {let {x314 = Zero};
                                                                                                                                                                                                                                    guard (x4 == Zero);
                                                                                                                                                                                                                                    x5 <- case x3 of
                                                                                                                                                                                                                                          {Succ y5 -> return y5;
                                                                                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                                                                                    let {x315 = Succ x5};
                                                                                                                                                                                                                                    x0 <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x314 x315 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                    return x0},
                                                                                                                                                                                                                                do {x6 <- case x4 of
                                                                                                                                                                                                                                          {Succ y6 -> return y6;
                                                                                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                                                                                    let {x316 = Succ x6};
                                                                                                                                                                                                                                    x318 <- case x3 of
                                                                                                                                                                                                                                            {Succ y318 -> return y318;
                                                                                                                                                                                                                                             _ -> mzero};
                                                                                                                                                                                                                                    let {x7 = x318};
                                                                                                                                                                                                                                    gtoII x7 x6;
                                                                                                                                                                                                                                    let {x317 = Succ x7};
                                                                                                                                                                                                                                    x0 <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x316 x317 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                    return x0}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {let {x95 = Succ x3};
                                                                                                                                                                                                                                  let {x96 = Succ x4};
                                                                                                                                                                                                                                  guard (x2 == Zero);
                                                                                                                                                                                                                                  (x0,
                                                                                                                                                                                                                                   x5,
                                                                                                                                                                                                                                   x6) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIO x1 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                  minmaxoIIII x5 x6 x95 x96;
                                                                                                                                                                                                                                  return x0},
                                                                                                                                                                                                                              do {let {x97 = Zero};
                                                                                                                                                                                                                                  let {x99 = Succ x3};
                                                                                                                                                                                                                                  let {x100 = Succ x4};
                                                                                                                                                                                                                                  x98 <- case x2 of
                                                                                                                                                                                                                                         {Succ y98 -> return y98;
                                                                                                                                                                                                                                          _ -> mzero};
                                                                                                                                                                                                                                  guard (x98 == x97);
                                                                                                                                                                                                                                  (x0,
                                                                                                                                                                                                                                   x5,
                                                                                                                                                                                                                                   x6) <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIO x1 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                  minmaxoIIII x5 x6 x99 x100;
                                                                                                                                                                                                                                  return x0},
                                                                                                                                                                                                                              do {x7 <- case x3 of
                                                                                                                                                                                                                                        {Succ y7 -> return y7;
                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                  x102 <- case x2 of
                                                                                                                                                                                                                                          {Succ y102 -> return y102;
                                                                                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                                                                                  let {x101 = x102};
                                                                                                                                                                                                                                  x8 <- case x101 of
                                                                                                                                                                                                                                        {Succ y8 -> return y8;
                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                  x0 <- leoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x4 x8 x7 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                  return x0},
                                                                                                                                                                                                                              do {let {x107 = Succ x4};
                                                                                                                                                                                                                                  guard (x3 == Zero);
                                                                                                                                                                                                                                  x104 <- case x2 of
                                                                                                                                                                                                                                          {Succ y104 -> return y104;
                                                                                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                                                                                  let {x103 = x104};
                                                                                                                                                                                                                                  x9 <- case x103 of
                                                                                                                                                                                                                                        {Succ y9 -> return y9;
                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                  let {x106 = Succ x9};
                                                                                                                                                                                                                                  let {x105 = Succ x106};
                                                                                                                                                                                                                                  (x0,
                                                                                                                                                                                                                                   x5,
                                                                                                                                                                                                                                   x6) <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIO x1 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                  minmaxoIIII x5 x6 x105 x107;
                                                                                                                                                                                                                                  return x0},
                                                                                                                                                                                                                              do {x7 <- case x3 of
                                                                                                                                                                                                                                        {Succ y7 -> return y7;
                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                  x109 <- case x2 of
                                                                                                                                                                                                                                          {Succ y109 -> return y109;
                                                                                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                                                                                  let {x108 = x109};
                                                                                                                                                                                                                                  x8 <- case x108 of
                                                                                                                                                                                                                                        {Succ y8 -> return y8;
                                                                                                                                                                                                                                         _ -> mzero};
                                                                                                                                                                                                                                  x0 <- gtoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x4 x8 x7 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                  return x0}]
_minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIO x2 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {let {x110 = Zero};
                                                                                                                                                                                                          guard (x2 == Zero);
                                                                                                                                                                                                          (x4,
                                                                                                                                                                                                           x5,
                                                                                                                                                                                                           x6,
                                                                                                                                                                                                           x1,
                                                                                                                                                                                                           x3) <- _minmaxoMinmaxoOOOOOI x110 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                          x0 <- minmaxoMinmaxoMinmaxoMinmaxoOIII x4 x5 x6 gen_gtoOI_x2;
                                                                                                                                                                                                          return (x0,
                                                                                                                                                                                                                  x1,
                                                                                                                                                                                                                  x3)},
                                                                                                                                                                                                      do {let {x111 = Zero};
                                                                                                                                                                                                          let {x0 = Zero};
                                                                                                                                                                                                          x112 <- case x2 of
                                                                                                                                                                                                                  {Succ y112 -> return y112;
                                                                                                                                                                                                                   _ -> mzero};
                                                                                                                                                                                                          guard (x112 == x111);
                                                                                                                                                                                                          (x1,
                                                                                                                                                                                                           x3) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOO gen_leoIO_x1;
                                                                                                                                                                                                          return (x0,
                                                                                                                                                                                                                  x1,
                                                                                                                                                                                                                  x3)},
                                                                                                                                                                                                      do {let {x0 = Zero};
                                                                                                                                                                                                          x114 <- case x2 of
                                                                                                                                                                                                                  {Succ y114 -> return y114;
                                                                                                                                                                                                                   _ -> mzero};
                                                                                                                                                                                                          let {x113 = x114};
                                                                                                                                                                                                          x7 <- case x113 of
                                                                                                                                                                                                                {Succ y7 -> return y7;
                                                                                                                                                                                                                 _ -> mzero};
                                                                                                                                                                                                          (x1,
                                                                                                                                                                                                           x3) <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI x7 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_leoIO_x1;
                                                                                                                                                                                                          return (x0,
                                                                                                                                                                                                                  x1,
                                                                                                                                                                                                                  x3)}]
_minmaxoMinmaxoOOOOOI x5 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {guard (x5 == Zero);
                                                                                                           let {x66 = x5};
                                                                                                           let {x1 = Succ x66};
                                                                                                           (x0,
                                                                                                            x2,
                                                                                                            x67,
                                                                                                            x4) <- minmaxoOOOO gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                           x6 <- case x67 of
                                                                                                                 {Succ y6 -> return y6;
                                                                                                                  _ -> mzero};
                                                                                                           let {x3 = Succ x6};
                                                                                                           return (x0,
                                                                                                                   x1,
                                                                                                                   x2,
                                                                                                                   x3,
                                                                                                                   x4)},
                                                                                                       do {x7 <- case x5 of
                                                                                                                 {Succ y7 -> return y7;
                                                                                                                  _ -> mzero};
                                                                                                           let {x70 = x5};
                                                                                                           let {x1 = Succ x70};
                                                                                                           (x0,
                                                                                                            x2,
                                                                                                            x4,
                                                                                                            x8) <- leoMinmaxoOOOIO x7 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                           let {x68 = Succ x8};
                                                                                                           let {x69 = x68};
                                                                                                           let {x3 = Succ x69};
                                                                                                           return (x0,
                                                                                                                   x1,
                                                                                                                   x2,
                                                                                                                   x3,
                                                                                                                   x4)},
                                                                                                       do {let {x3 = Zero};
                                                                                                           let {x71 = Succ x5};
                                                                                                           let {x1 = x3};
                                                                                                           (x0,
                                                                                                            x2,
                                                                                                            x4) <- minmaxoOOIO x71 gen_leoIO_x1 gen_minmaxoOOIO_x3;
                                                                                                           return (x0,
                                                                                                                   x1,
                                                                                                                   x2,
                                                                                                                   x3,
                                                                                                                   x4)},
                                                                                                       do {let {x72 = Zero};
                                                                                                           x9 <- case x5 of
                                                                                                                 {Succ y9 -> return y9;
                                                                                                                  _ -> mzero};
                                                                                                           let {x75 = Succ x9};
                                                                                                           let {x74 = Succ x75};
                                                                                                           let {x73 = x72};
                                                                                                           let {x3 = Succ x73};
                                                                                                           let {x1 = x3};
                                                                                                           (x0,
                                                                                                            x2,
                                                                                                            x4) <- minmaxoOOIO x74 gen_leoIO_x1 gen_minmaxoOOIO_x3;
                                                                                                           return (x0,
                                                                                                                   x1,
                                                                                                                   x2,
                                                                                                                   x3,
                                                                                                                   x4)},
                                                                                                       do {x7 <- case x5 of
                                                                                                                 {Succ y7 -> return y7;
                                                                                                                  _ -> mzero};
                                                                                                           (x0,
                                                                                                            x2,
                                                                                                            x4,
                                                                                                            x8) <- gtoMinmaxoOOOIO x7 gen_leoIO_x1 gen_minmaxoOOIO_x3;
                                                                                                           let {x76 = Succ x8};
                                                                                                           let {x77 = x76};
                                                                                                           let {x3 = Succ x77};
                                                                                                           let {x1 = x3};
                                                                                                           return (x0,
                                                                                                                   x1,
                                                                                                                   x2,
                                                                                                                   x3,
                                                                                                                   x4)}]
gtoMinmaxoOOOIO x3 gen_leoIO_x1 gen_minmaxoOOIO_x3 = Immature $ msum [do {let {x4 = Zero};
                                                               x5 <- case x3 of
                                                                     {Succ y5 -> return y5;
                                                                      _ -> mzero};
                                                               let {x86 = Succ x5};
                                                               let {x85 = Succ x86};
                                                               let {x84 = Succ x85};
                                                               (x0,
                                                                x1,
                                                                x2) <- minmaxoOOIO x84 gen_leoIO_x1 gen_minmaxoOOIO_x3;
                                                               return (x0, x1, x2, x4)},
                                                           do {x87 <- case x3 of
                                                                      {Succ y87 -> return y87;
                                                                       _ -> mzero};
                                                               let {x7 = x87};
                                                               let {x90 = Succ x7};
                                                               let {x89 = Succ x90};
                                                               let {x88 = Succ x89};
                                                               x6 <- gtoIO x7;
                                                               let {x4 = Succ x6};
                                                               (x0,
                                                                x1,
                                                                x2) <- minmaxoOOIO x88 gen_leoIO_x1 gen_minmaxoOOIO_x3;
                                                               return (x0, x1, x2, x4)}]
gtoIO x0 = Immature $ msum [do {let {x1 = Zero};
                     x2 <- case x0 of
                           {Succ y2 -> return y2; _ -> mzero};
                     return x1},
                 do {x4 <- case x0 of
                           {Succ y4 -> return y4; _ -> mzero};
                     x3 <- gtoIO x4;
                     let {x1 = Succ x3};
                     return x1}]
gtoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {let {x304 = Zero};
                                                                                                                                                                                                                             let {x308 = Succ x2};
                                                                                                                                                                                                                             guard (x4 == Zero);
                                                                                                                                                                                                                             x7 <- case x3 of
                                                                                                                                                                                                                                   {Succ y7 -> return y7;
                                                                                                                                                                                                                                    _ -> mzero};
                                                                                                                                                                                                                             let {x307 = Succ x7};
                                                                                                                                                                                                                             let {x306 = Succ x307};
                                                                                                                                                                                                                             let {x305 = Succ x306};
                                                                                                                                                                                                                             (x0,
                                                                                                                                                                                                                              x5,
                                                                                                                                                                                                                              x6) <- __minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIOI x1 x304 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                             minmaxoIIII x5 x6 x305 x308;
                                                                                                                                                                                                                             return x0},
                                                                                                                                                                                                                         do {let {x313 = Succ x2};
                                                                                                                                                                                                                             x8 <- case x4 of
                                                                                                                                                                                                                                   {Succ y8 -> return y8;
                                                                                                                                                                                                                                    _ -> mzero};
                                                                                                                                                                                                                             let {x309 = Succ x8};
                                                                                                                                                                                                                             x9 <- case x3 of
                                                                                                                                                                                                                                   {Succ y9 -> return y9;
                                                                                                                                                                                                                                    _ -> mzero};
                                                                                                                                                                                                                             gtoII x9 x8;
                                                                                                                                                                                                                             let {x312 = Succ x9};
                                                                                                                                                                                                                             let {x311 = Succ x312};
                                                                                                                                                                                                                             let {x310 = Succ x311};
                                                                                                                                                                                                                             (x0,
                                                                                                                                                                                                                              x5,
                                                                                                                                                                                                                              x6) <- __minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIOI x1 x309 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                             minmaxoIIII x5 x6 x310 x313;
                                                                                                                                                                                                                             return x0}]
__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIOI x2 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {let {x274 = Succ x4};
                                                                                                                                                                                                               guard (x2 == Zero);
                                                                                                                                                                                                               (x5,
                                                                                                                                                                                                                x6,
                                                                                                                                                                                                                x7,
                                                                                                                                                                                                                x1,
                                                                                                                                                                                                                x3) <- _minmaxoMinmaxoOOOOOI x274 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                               x0 <- minmaxoMinmaxoMinmaxoMinmaxoOIII x5 x6 x7 gen_gtoOI_x2;
                                                                                                                                                                                                               return (x0,
                                                                                                                                                                                                                       x1,
                                                                                                                                                                                                                       x3)},
                                                                                                                                                                                                           do {let {x275 = Zero};
                                                                                                                                                                                                               let {x0 = Zero};
                                                                                                                                                                                                               x276 <- case x2 of
                                                                                                                                                                                                                       {Succ y276 -> return y276;
                                                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                                               guard (x276 == x275);
                                                                                                                                                                                                               (x1,
                                                                                                                                                                                                                x3) <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_leoIO_x1;
                                                                                                                                                                                                               return (x0,
                                                                                                                                                                                                                       x1,
                                                                                                                                                                                                                       x3)},
                                                                                                                                                                                                           do {let {x278 = Zero};
                                                                                                                                                                                                               let {x277 = Succ x278};
                                                                                                                                                                                                               let {x280 = Zero};
                                                                                                                                                                                                               let {x0 = Zero};
                                                                                                                                                                                                               x279 <- case x2 of
                                                                                                                                                                                                                       {Succ y279 -> return y279;
                                                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                                               guard (x279 == x277);
                                                                                                                                                                                                               let {x281 = x280};
                                                                                                                                                                                                               let {x1 = Succ x281};
                                                                                                                                                                                                               x3 <- _minmaxoMinmaxoMinmaxoOI x4;
                                                                                                                                                                                                               return (x0,
                                                                                                                                                                                                                       x1,
                                                                                                                                                                                                                       x3)},
                                                                                                                                                                                                           do {let {x283 = Zero};
                                                                                                                                                                                                               let {x282 = Succ x283};
                                                                                                                                                                                                               let {x288 = Zero};
                                                                                                                                                                                                               let {x0 = Zero};
                                                                                                                                                                                                               x8 <- case x4 of
                                                                                                                                                                                                                     {Succ y8 -> return y8;
                                                                                                                                                                                                                      _ -> mzero};
                                                                                                                                                                                                               x287 <- case x2 of
                                                                                                                                                                                                                       {Succ y287 -> return y287;
                                                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                                               let {x284 = x282};
                                                                                                                                                                                                               let {x3 = Succ x284};
                                                                                                                                                                                                               let {x285 = x287};
                                                                                                                                                                                                               x286 <- case x285 of
                                                                                                                                                                                                                       {Succ y286 -> return y286;
                                                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                                               x9 <- case x286 of
                                                                                                                                                                                                                     {Succ y9 -> return y9;
                                                                                                                                                                                                                      _ -> mzero};
                                                                                                                                                                                                               _leoMinmaxoII x8 x9;
                                                                                                                                                                                                               let {x289 = x288};
                                                                                                                                                                                                               let {x1 = Succ x289};
                                                                                                                                                                                                               return (x0,
                                                                                                                                                                                                                       x1,
                                                                                                                                                                                                                       x3)},
                                                                                                                                                                                                           do {let {x293 = Zero};
                                                                                                                                                                                                               let {x0 = Zero};
                                                                                                                                                                                                               guard (x4 == Zero);
                                                                                                                                                                                                               x292 <- case x2 of
                                                                                                                                                                                                                       {Succ y292 -> return y292;
                                                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                                               let {x290 = x292};
                                                                                                                                                                                                               x291 <- case x290 of
                                                                                                                                                                                                                       {Succ y291 -> return y291;
                                                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                                               x10 <- case x291 of
                                                                                                                                                                                                                      {Succ y10 -> return y10;
                                                                                                                                                                                                                       _ -> mzero};
                                                                                                                                                                                                               let {x295 = Succ x10};
                                                                                                                                                                                                               let {x294 = x293};
                                                                                                                                                                                                               let {x1 = Succ x294};
                                                                                                                                                                                                               x3 <- _minmaxoMinmaxoMinmaxoOI x295;
                                                                                                                                                                                                               return (x0,
                                                                                                                                                                                                                       x1,
                                                                                                                                                                                                                       x3)},
                                                                                                                                                                                                           do {let {x297 = Zero};
                                                                                                                                                                                                               let {x296 = Succ x297};
                                                                                                                                                                                                               let {x302 = Zero};
                                                                                                                                                                                                               let {x0 = Zero};
                                                                                                                                                                                                               x8 <- case x4 of
                                                                                                                                                                                                                     {Succ y8 -> return y8;
                                                                                                                                                                                                                      _ -> mzero};
                                                                                                                                                                                                               x301 <- case x2 of
                                                                                                                                                                                                                       {Succ y301 -> return y301;
                                                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                                               let {x298 = x296};
                                                                                                                                                                                                               let {x3 = Succ x298};
                                                                                                                                                                                                               let {x299 = x301};
                                                                                                                                                                                                               x300 <- case x299 of
                                                                                                                                                                                                                       {Succ y300 -> return y300;
                                                                                                                                                                                                                        _ -> mzero};
                                                                                                                                                                                                               x9 <- case x300 of
                                                                                                                                                                                                                     {Succ y9 -> return y9;
                                                                                                                                                                                                                      _ -> mzero};
                                                                                                                                                                                                               _gtoMinmaxoII x9 x8;
                                                                                                                                                                                                               let {x303 = x302};
                                                                                                                                                                                                               let {x1 = Succ x303};
                                                                                                                                                                                                               return (x0,
                                                                                                                                                                                                                       x1,
                                                                                                                                                                                                                       x3)}]
leoMinmaxoOOOIO x3 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {guard (x3 == Zero);
                                                                                                     (x0,
                                                                                                      x1,
                                                                                                      x78,
                                                                                                      x2) <- minmaxoOOOO gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                     x79 <- case x78 of
                                                                                                            {Succ y79 -> return y79;
                                                                                                             _ -> mzero};
                                                                                                     x4 <- case x79 of
                                                                                                           {Succ y4 -> return y4;
                                                                                                            _ -> mzero};
                                                                                                     return (x0,
                                                                                                             x1,
                                                                                                             x2,
                                                                                                             x4)},
                                                                                                 do {x80 <- case x3 of
                                                                                                            {Succ y80 -> return y80;
                                                                                                             _ -> mzero};
                                                                                                     let {x6 = x80};
                                                                                                     x5 <- leoIO x6 gen_leoIO_x1;
                                                                                                     let {x4 = Succ x5};
                                                                                                     let {x83 = Succ x5};
                                                                                                     let {x82 = Succ x83};
                                                                                                     let {x81 = Succ x82};
                                                                                                     (x0,
                                                                                                      x1,
                                                                                                      x2) <- minmaxoOOIO x81 gen_leoIO_x1 gen_minmaxoOOIO_x3;
                                                                                                     return (x0,
                                                                                                             x1,
                                                                                                             x2,
                                                                                                             x4)}]
leoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {let {x264 = Zero};
                                                                                                                                                                                                                             let {x266 = Succ x4};
                                                                                                                                                                                                                             let {x265 = Succ x266};
                                                                                                                                                                                                                             let {x267 = Succ x2};
                                                                                                                                                                                                                             guard (x3 == Zero);
                                                                                                                                                                                                                             (x0,
                                                                                                                                                                                                                              x5,
                                                                                                                                                                                                                              x6) <- __minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIOI x1 x264 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                             minmaxoIIII x5 x6 x265 x267;
                                                                                                                                                                                                                             return x0},
                                                                                                                                                                                                                         do {let {x273 = Succ x2};
                                                                                                                                                                                                                             x7 <- case x4 of
                                                                                                                                                                                                                                   {Succ y7 -> return y7;
                                                                                                                                                                                                                                    _ -> mzero};
                                                                                                                                                                                                                             let {x272 = Succ x7};
                                                                                                                                                                                                                             let {x271 = Succ x272};
                                                                                                                                                                                                                             let {x270 = Succ x271};
                                                                                                                                                                                                                             x269 <- case x3 of
                                                                                                                                                                                                                                     {Succ y269 -> return y269;
                                                                                                                                                                                                                                      _ -> mzero};
                                                                                                                                                                                                                             let {x8 = x269};
                                                                                                                                                                                                                             leoII x8 x7;
                                                                                                                                                                                                                             let {x268 = Succ x8};
                                                                                                                                                                                                                             (x0,
                                                                                                                                                                                                                              x5,
                                                                                                                                                                                                                              x6) <- __minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIOI x1 x268 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                             minmaxoIIII x5 x6 x270 x273;
                                                                                                                                                                                                                             return x0}]
leoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x3 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {let {x91 = Zero};
                                                                                                                                                                                                                                    guard (x3 == Zero);
                                                                                                                                                                                                                                    x0 <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x91 x4 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                    return x0},
                                                                                                                                                                                                                                do {x5 <- case x4 of
                                                                                                                                                                                                                                          {Succ y5 -> return y5;
                                                                                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                                                                                    let {x93 = Succ x5};
                                                                                                                                                                                                                                    x94 <- case x3 of
                                                                                                                                                                                                                                           {Succ y94 -> return y94;
                                                                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                                                                    let {x6 = x94};
                                                                                                                                                                                                                                    leoII x6 x5;
                                                                                                                                                                                                                                    let {x92 = Succ x6};
                                                                                                                                                                                                                                    x0 <- _minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIIII x1 x2 x92 x93 gen__minmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOI_x119 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                                                                                    return x0}]
minmaxoOOIO x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 = Immature $ msum [do {guard (x2 == Zero);
                                                           let {x0 = x2};
                                                           (x1, x3) <- do {x3 <- gen_minmaxoOOIO_x3;
                                                                           return (x3, x3)};
                                                           return (x0, x1, x3)},
                                                       do {x5 <- case x2 of
                                                                 {Succ y5 -> return y5; _ -> mzero};
                                                           let {x0 = x2};
                                                           x4 <- leoIO x5 gen_leoIO_x1;
                                                           let {x3 = Succ x4};
                                                           let {x1 = x3};
                                                           return (x0, x1, x3)},
                                                       do {let {x3 = Zero};
                                                           x6 <- case x2 of
                                                                 {Succ y6 -> return y6; _ -> mzero};
                                                           let {x1 = x2};
                                                           let {x0 = x3};
                                                           return (x0, x1, x3)},
                                                       do {x5 <- case x2 of
                                                                 {Succ y5 -> return y5; _ -> mzero};
                                                           let {x1 = x2};
                                                           x4 <- gtoIO x5;
                                                           let {x3 = Succ x4};
                                                           let {x0 = x3};
                                                           return (x0, x1, x3)}]
minmaxoOOOO gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {let {x2 = Zero};
                                                              let {x0 = x2};
                                                              (x1,
                                                               x3) <- do {x3 <- gen_minmaxoOOOO_x3;
                                                                          return (x3, x3)};
                                                              return (x0, x1, x2, x3)},
                                                          do {(x1,
                                                               x3) <- do {x3 <- gen_minmaxoOOOO_x3;
                                                                          return (x3, x3)};
                                                              x4 <- case x3 of
                                                                    {Succ y4 -> return y4;
                                                                     _ -> mzero};
                                                              x5 <- leoOI x4;
                                                              let {x2 = Succ x5};
                                                              let {x0 = x2};
                                                              return (x0, x1, x2, x3)},
                                                          do {let {x3 = Zero};
                                                              let {x0 = x3};
                                                              (x1,
                                                               x2) <- do {x2 <- gen_minmaxoOOOO_x2;
                                                                          return (x2, x2)};
                                                              x6 <- case x2 of
                                                                    {Succ y6 -> return y6;
                                                                     _ -> mzero};
                                                              return (x0, x1, x2, x3)},
                                                          do {(x1,
                                                               x2) <- do {x2 <- gen_minmaxoOOOO_x2;
                                                                          return (x2, x2)};
                                                              x5 <- case x2 of
                                                                    {Succ y5 -> return y5;
                                                                     _ -> mzero};
                                                              x4 <- gtoIO x5;
                                                              let {x3 = Succ x4};
                                                              let {x0 = x3};
                                                              return (x0, x1, x2, x3)}]
leoOI x1 = Immature $ msum [do {let {x0 = Zero}; return x0},
                 do {x2 <- case x1 of
                           {Succ y2 -> return y2; _ -> mzero};
                     x3 <- leoOI x2;
                     let {x0 = Succ x3};
                     return x0}]
minmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 gen_gtoOI_x2 = Immature $ msum [do {x4 <- minmaxoMinmaxoMinmaxoIIIO x1 x2 x3 gen_gtoOI_x2;
                                                                   let {x0 = Succ x4};
                                                                   return x0}]
minmaxoMinmaxoMinmaxoIIIO x0 x1 x2 gen_gtoOI_x2 = Immature $ msum [do {let {x3 = Zero};
                                                            let {x42 = Zero};
                                                            let {x41 = Succ x42};
                                                            let {x40 = Succ x41};
                                                            let {x39 = Succ x40};
                                                            let {x45 = Zero};
                                                            let {x44 = Succ x45};
                                                            let {x43 = Succ x44};
                                                            let {x50 = Zero};
                                                            let {x49 = Succ x50};
                                                            let {x48 = Succ x49};
                                                            let {x47 = Succ x48};
                                                            let {x46 = Succ x47};
                                                            x4 <- case x1 of
                                                                  {Succ y4 -> return y4;
                                                                   _ -> mzero};
                                                            _minmaxoMinmaxoIIIIII x39 x43 x46 x0 x2 x4;
                                                            return x3},
                                                        do {let {x51 = Zero};
                                                            let {x56 = Zero};
                                                            let {x55 = Succ x56};
                                                            let {x54 = Succ x55};
                                                            let {x53 = Succ x54};
                                                            let {x59 = Zero};
                                                            let {x58 = Succ x59};
                                                            let {x57 = Succ x58};
                                                            let {x64 = Zero};
                                                            let {x63 = Succ x64};
                                                            let {x62 = Succ x63};
                                                            let {x61 = Succ x62};
                                                            let {x60 = Succ x61};
                                                            x52 <- case x1 of
                                                                   {Succ y52 -> return y52;
                                                                    _ -> mzero};
                                                            guard (x52 == x51);
                                                            x65 <- _minmaxoMinmaxoIIIIIO x53 x57 x60 x0 x2 gen_gtoOI_x2;
                                                            x5 <- case x65 of
                                                                  {Succ y5 -> return y5;
                                                                   _ -> mzero};
                                                            let {x3 = Succ x5};
                                                            return x3}]
_minmaxoMinmaxoIIIIIO x0 x1 x2 x3 x4 gen_gtoOI_x2 = Immature $ msum [do {let {x5 = Zero};
                                                              x6 <- case x3 of
                                                                    {Succ y6 -> return y6;
                                                                     _ -> mzero};
                                                              let {x67 = Succ x6};
                                                              minmaxoIIII x0 x2 x67 x4;
                                                              x66 <- case x1 of
                                                                     {Succ y66 -> return y66;
                                                                      _ -> mzero};
                                                              guard (x66 == x5);
                                                              return x5},
                                                          do {x69 <- case x3 of
                                                                     {Succ y69 -> return y69;
                                                                      _ -> mzero};
                                                              x70 <- case x1 of
                                                                     {Succ y70 -> return y70;
                                                                      _ -> mzero};
                                                              let {x68 = x69};
                                                              x8 <- case x68 of
                                                                    {Succ y8 -> return y8;
                                                                     _ -> mzero};
                                                              let {x5 = x70};
                                                              x7 <- case x5 of
                                                                    {Succ y7 -> return y7;
                                                                     _ -> mzero};
                                                              leoMinmaxoIIIII x0 x2 x4 x7 x8;
                                                              return x5},
                                                          do {guard (x1 == x3);
                                                              guard (x3 == Zero);
                                                              x71 <- minmaxoIIOI x0 x2 x4;
                                                              x5 <- case x71 of
                                                                    {Succ y5 -> return y5;
                                                                     _ -> mzero};
                                                              return x5},
                                                          do {guard (x1 == x3);
                                                              let {x72 = Zero};
                                                              x73 <- case x3 of
                                                                     {Succ y73 -> return y73;
                                                                      _ -> mzero};
                                                              guard (x73 == x72);
                                                              x74 <- minmaxoIIOI x0 x2 x4;
                                                              x75 <- case x74 of
                                                                     {Succ y75 -> return y75;
                                                                      _ -> mzero};
                                                              x9 <- case x75 of
                                                                    {Succ y9 -> return y9;
                                                                     _ -> mzero};
                                                              let {x5 = Succ x9};
                                                              return x5},
                                                          do {guard (x1 == x3);
                                                              x77 <- case x3 of
                                                                     {Succ y77 -> return y77;
                                                                      _ -> mzero};
                                                              let {x76 = x77};
                                                              x8 <- case x76 of
                                                                    {Succ y8 -> return y8;
                                                                     _ -> mzero};
                                                              x7 <- gtoMinmaxoIIIOI x0 x2 x4 x8 gen_gtoOI_x2;
                                                              let {x5 = Succ x7};
                                                              return x5}]
gtoMinmaxoIIIOI x0 x1 x2 x4 gen_gtoOI_x2 = Immature $ msum [do {guard (x4 == Zero);
                                                     x84 <- minmaxoIIOI x0 x1 x2;
                                                     x85 <- case x84 of
                                                            {Succ y85 -> return y85; _ -> mzero};
                                                     x86 <- case x85 of
                                                            {Succ y86 -> return y86; _ -> mzero};
                                                     x5 <- case x86 of
                                                           {Succ y5 -> return y5; _ -> mzero};
                                                     let {x3 = Succ x5};
                                                     return x3},
                                                 do {x6 <- case x4 of
                                                           {Succ y6 -> return y6; _ -> mzero};
                                                     x7 <- gtoOI x6 gen_gtoOI_x2;
                                                     let {x90 = Succ x7};
                                                     let {x89 = Succ x90};
                                                     let {x88 = Succ x89};
                                                     minmaxoIIII x0 x1 x88 x2;
                                                     let {x87 = x7};
                                                     let {x3 = Succ x87};
                                                     return x3}]
minmaxoIIOI x0 x1 x3 = Immature $ msum [do {guard (x1 == x3);
                                 let {x2 = Zero};
                                 guard (x0 == x2);
                                 return x2},
                             do {guard (x1 == x3);
                                 x4 <- case x3 of
                                       {Succ y4 -> return y4; _ -> mzero};
                                 let {x2 = x0};
                                 x5 <- case x2 of
                                       {Succ y5 -> return y5; _ -> mzero};
                                 leoII x5 x4;
                                 return x2},
                             do {guard (x0 == x3);
                                 guard (x3 == Zero);
                                 let {x2 = x1};
                                 x6 <- case x2 of
                                       {Succ y6 -> return y6; _ -> mzero};
                                 return x2},
                             do {guard (x0 == x3);
                                 x4 <- case x3 of
                                       {Succ y4 -> return y4; _ -> mzero};
                                 let {x2 = x1};
                                 x5 <- case x2 of
                                       {Succ y5 -> return y5; _ -> mzero};
                                 gtoII x5 x4;
                                 return x2}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIO x2 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {guard (x2 == Zero);
                                                                                                                                                        (x0,
                                                                                                                                                         x4,
                                                                                                                                                         x5,
                                                                                                                                                         x6) <- minmaxoMinmaxoMinmaxoMinmaxoOOOO gen_gtoOI_x2 gen_leoIO_x1;
                                                                                                                                                        (x1,
                                                                                                                                                         x3) <- minmaxoMinmaxoIIIOO x4 x5 x6;
                                                                                                                                                        return (x0,
                                                                                                                                                                x1,
                                                                                                                                                                x3)},
                                                                                                                                                    do {x7 <- case x2 of
                                                                                                                                                              {Succ y7 -> return y7;
                                                                                                                                                               _ -> mzero};
                                                                                                                                                        (x4,
                                                                                                                                                         x5,
                                                                                                                                                         x6,
                                                                                                                                                         x1,
                                                                                                                                                         x3) <- _minmaxoMinmaxoOOOOOI x7 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                        x0 <- minmaxoMinmaxoMinmaxoMinmaxoOIII x4 x5 x6 gen_gtoOI_x2;
                                                                                                                                                        return (x0,
                                                                                                                                                                x1,
                                                                                                                                                                x3)}]
minmaxoMinmaxoMinmaxoMinmaxoOOOO gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {(x1,
                                                                        x2,
                                                                        x3,
                                                                        x4) <- minmaxoMinmaxoMinmaxoOOOO gen_gtoOI_x2 gen_leoIO_x1;
                                                                       let {x0 = Succ x4};
                                                                       return (x0, x1, x2, x3)}]
minmaxoMinmaxoMinmaxoOOOO gen_gtoOI_x2 gen_leoIO_x1 = Immature $ msum [do {let {x3 = Zero};
                                                                let {x42 = Zero};
                                                                let {x41 = Succ x42};
                                                                let {x40 = Succ x41};
                                                                let {x39 = Succ x40};
                                                                let {x45 = Zero};
                                                                let {x44 = Succ x45};
                                                                let {x43 = Succ x44};
                                                                let {x50 = Zero};
                                                                let {x49 = Succ x50};
                                                                let {x48 = Succ x49};
                                                                let {x47 = Succ x48};
                                                                let {x46 = Succ x47};
                                                                (x0,
                                                                 x2,
                                                                 x4) <- _minmaxoMinmaxoIIIOOO x39 x43 x46 gen_gtoOI_x2 gen_leoIO_x1;
                                                                let {x1 = Succ x4};
                                                                return (x0, x1, x2, x3)},
                                                            do {let {x51 = Zero};
                                                                let {x56 = Zero};
                                                                let {x55 = Succ x56};
                                                                let {x54 = Succ x55};
                                                                let {x53 = Succ x54};
                                                                let {x59 = Zero};
                                                                let {x58 = Succ x59};
                                                                let {x57 = Succ x58};
                                                                let {x64 = Zero};
                                                                let {x63 = Succ x64};
                                                                let {x62 = Succ x63};
                                                                let {x61 = Succ x62};
                                                                let {x60 = Succ x61};
                                                                let {x52 = x51};
                                                                let {x1 = Succ x52};
                                                                (x0,
                                                                 x2,
                                                                 x65) <- _minmaxoMinmaxoIIIOOO x53 x57 x60 gen_gtoOI_x2 gen_leoIO_x1;
                                                                x5 <- case x65 of
                                                                      {Succ y5 -> return y5;
                                                                       _ -> mzero};
                                                                let {x3 = Succ x5};
                                                                return (x0, x1, x2, x3)}]
minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOIII x1 x2 x3 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3 = Immature $ msum [do {let {x37 = Zero};
                                                                                                                                                                            guard (x2 == Zero);
                                                                                                                                                                            (x0,
                                                                                                                                                                             x4,
                                                                                                                                                                             x5) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIO x1 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                            minmaxoIIII x4 x5 x37 x3;
                                                                                                                                                                            return x0},
                                                                                                                                                                        do {x6 <- case x2 of
                                                                                                                                                                                  {Succ y6 -> return y6;
                                                                                                                                                                                   _ -> mzero};
                                                                                                                                                                            let {x38 = Succ x6};
                                                                                                                                                                            (x0,
                                                                                                                                                                             x4,
                                                                                                                                                                             x5) <- minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoOOIO x1 gen_gtoOI_x2 gen_leoIO_x1 gen_minmaxoOOIO_x3 gen_minmaxoOOOO_x2 gen_minmaxoOOOO_x3;
                                                                                                                                                                            minmaxoIIII x4 x5 x38 x3;
                                                                                                                                                                            return x0}]