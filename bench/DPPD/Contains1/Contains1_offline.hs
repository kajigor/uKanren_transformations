module Contains1_offline where

import Stream
import Control.Monad
import Term

containsosdI x0 = Immature $ msum [do {let {x8 = O};
                            let {x7 = S x8};
                            let {x10 = O};
                            let {x14 = O};
                            let {x13 = S x14};
                            let {x12 = S x13};
                            (x15, x16) <- case x0 of
                                          {Cons y15 y16 -> return (y15, y16); _ -> mzero};
                            guard (x15 == x7);
                            let {x9 = x16};
                            x11 <- case x9 of
                                   {Cons y10 y11 -> do {guard (x10 == y10); return y11};
                                    _ -> mzero};
                            x1 <- case x11 of
                                  {Cons y12 y1 -> do {guard (x12 == y12); return y1}; _ -> mzero};
                            return ()},
                        do {let {x18 = O};
                            let {x17 = S x18};
                            let {x20 = O};
                            (x22, x23) <- case x0 of
                                          {Cons y22 y23 -> return (y22, y23); _ -> mzero};
                            guard (x22 == x17);
                            let {x19 = x23};
                            x21 <- case x19 of
                                   {Cons y20 y21 -> do {guard (x20 == y20); return y21};
                                    _ -> mzero};
                            (x2, x1) <- case x21 of
                                        {Cons y2 y1 -> return (y2, y1); _ -> mzero};
                            _conodssI x1;
                            return ()},
                        do {let {x25 = O};
                            let {x24 = S x25};
                            let {x27 = O};
                            let {x30 = O};
                            let {x29 = S x30};
                            (x31, x32) <- case x0 of
                                          {Cons y31 y32 -> return (y31, y32); _ -> mzero};
                            guard (x31 == x24);
                            let {x26 = x32};
                            x28 <- case x26 of
                                   {Cons y27 y28 -> do {guard (x27 == y27); return y28};
                                    _ -> mzero};
                            x1 <- case x28 of
                                  {Cons y29 y1 -> do {guard (x29 == y29); return y1}; _ -> mzero};
                            __conodssI x1;
                            return ()},
                        do {let {x34 = O};
                            let {x33 = S x34};
                            (x36, x37) <- case x0 of
                                          {Cons y36 y37 -> return (y36, y37); _ -> mzero};
                            guard (x36 == x33);
                            let {x35 = x37};
                            (x3, x4) <- case x35 of
                                        {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                            _conodssI x4;
                            return ()},
                        do {let {x39 = O};
                            let {x38 = S x39};
                            let {x42 = O};
                            let {x41 = S x42};
                            (x43, x44) <- case x0 of
                                          {Cons y43 y44 -> return (y43, y44); _ -> mzero};
                            guard (x43 == x38);
                            let {x40 = x44};
                            x4 <- case x40 of
                                  {Cons y41 y4 -> do {guard (x41 == y41); return y4}; _ -> mzero};
                            __conodssI x4;
                            return ()},
                        do {(x5, x6) <- case x0 of
                                        {Cons y5 y6 -> return (y5, y6); _ -> mzero};
                            _conodssI x6;
                            return ()}]
__conodssI x0 = Immature $ msum [do {let {x48 = O};
                          let {x52 = O};
                          let {x51 = S x52};
                          let {x50 = S x51};
                          (x53, x54) <- case x0 of
                                        {Cons y53 y54 -> return (y53, y54); _ -> mzero};
                          guard (x53 == x48);
                          let {x49 = x54};
                          x1 <- case x49 of
                                {Cons y50 y1 -> do {guard (x50 == y50); return y1}; _ -> mzero};
                          return ()},
                      do {let {x55 = O};
                          (x57, x58) <- case x0 of
                                        {Cons y57 y58 -> return (y57, y58); _ -> mzero};
                          guard (x57 == x55);
                          let {x56 = x58};
                          (x2, x1) <- case x56 of
                                      {Cons y2 y1 -> return (y2, y1); _ -> mzero};
                          _conodssI x1;
                          return ()},
                      do {let {x59 = O};
                          let {x62 = O};
                          let {x61 = S x62};
                          (x63, x64) <- case x0 of
                                        {Cons y63 y64 -> return (y63, y64); _ -> mzero};
                          guard (x63 == x59);
                          let {x60 = x64};
                          x1 <- case x60 of
                                {Cons y61 y1 -> do {guard (x61 == y61); return y1}; _ -> mzero};
                          __conodssI x1;
                          return ()},
                      do {(x3, x4) <- case x0 of
                                      {Cons y3 y4 -> return (y3, y4); _ -> mzero};
                          _conodssI x4;
                          return ()},
                      do {let {x66 = O};
                          let {x65 = S x66};
                          (x67, x4) <- case x0 of
                                       {Cons y67 y4 -> return (y67, y4); _ -> mzero};
                          guard (x67 == x65);
                          __conodssI x4;
                          return ()}]
_conodssI x0 = Immature $ msum [do {let {x46 = O};
                         let {x45 = S x46};
                         (x47, x1) <- case x0 of
                                      {Cons y47 y1 -> return (y47, y1); _ -> mzero};
                         guard (x47 == x45);
                         __conodssI x1;
                         return ()},
                     do {(x2, x1) <- case x0 of
                                     {Cons y2 y1 -> return (y2, y1); _ -> mzero};
                         _conodssI x1;
                         return ()}]
containsosdO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2 gen_containsosdO_x19 gen_containsosdO_x35 gen_containsosdO_x5 gen_containsosdO_x9 = Immature $ msum [do {let {x8 = O};
                                                                                                                                                                                    let {x7 = S x8};
                                                                                                                                                                                    let {x10 = O};
                                                                                                                                                                                    let {x14 = O};
                                                                                                                                                                                    let {x13 = S x14};
                                                                                                                                                                                    let {x12 = S x13};
                                                                                                                                                                                    let {x15 = x7};
                                                                                                                                                                                    (x16,
                                                                                                                                                                                     x9) <- do {x9 <- gen_containsosdO_x9;
                                                                                                                                                                                                return (x9,
                                                                                                                                                                                                        x9)};
                                                                                                                                                                                    let {x0 = Cons x15 x16};
                                                                                                                                                                                    x11 <- case x9 of
                                                                                                                                                                                           {Cons y10
                                                                                                                                                                                                 y11 -> do {guard (x10 == y10);
                                                                                                                                                                                                            return y11};
                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                    x1 <- case x11 of
                                                                                                                                                                                          {Cons y12
                                                                                                                                                                                                y1 -> do {guard (x12 == y12);
                                                                                                                                                                                                          return y1};
                                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                                    return x0},
                                                                                                                                                                                do {let {x18 = O};
                                                                                                                                                                                    let {x17 = S x18};
                                                                                                                                                                                    let {x20 = O};
                                                                                                                                                                                    let {x22 = x17};
                                                                                                                                                                                    x1 <- _conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                                                                                                                    (x23,
                                                                                                                                                                                     x19) <- do {x19 <- gen_containsosdO_x19;
                                                                                                                                                                                                 return (x19,
                                                                                                                                                                                                         x19)};
                                                                                                                                                                                    let {x0 = Cons x22 x23};
                                                                                                                                                                                    x21 <- case x19 of
                                                                                                                                                                                           {Cons y20
                                                                                                                                                                                                 y21 -> do {guard (x20 == y20);
                                                                                                                                                                                                            return y21};
                                                                                                                                                                                            _ -> mzero};
                                                                                                                                                                                    x2 <- case x21 of
                                                                                                                                                                                          {Cons y2
                                                                                                                                                                                                y1 -> do {guard (x1 == y1);
                                                                                                                                                                                                          return y2};
                                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                                    return x0},
                                                                                                                                                                                do {let {x25 = O};
                                                                                                                                                                                    let {x24 = S x25};
                                                                                                                                                                                    let {x27 = O};
                                                                                                                                                                                    let {x30 = O};
                                                                                                                                                                                    let {x29 = S x30};
                                                                                                                                                                                    let {x31 = x24};
                                                                                                                                                                                    x1 <- __conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                                                                                                                    let {x28 = Cons x29 x1};
                                                                                                                                                                                    let {x26 = Cons x27 x28};
                                                                                                                                                                                    let {x32 = x26};
                                                                                                                                                                                    let {x0 = Cons x31 x32};
                                                                                                                                                                                    return x0},
                                                                                                                                                                                do {let {x34 = O};
                                                                                                                                                                                    let {x33 = S x34};
                                                                                                                                                                                    let {x36 = x33};
                                                                                                                                                                                    x4 <- _conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                                                                                                                    (x37,
                                                                                                                                                                                     x35) <- do {x35 <- gen_containsosdO_x35;
                                                                                                                                                                                                 return (x35,
                                                                                                                                                                                                         x35)};
                                                                                                                                                                                    let {x0 = Cons x36 x37};
                                                                                                                                                                                    x3 <- case x35 of
                                                                                                                                                                                          {Cons y3
                                                                                                                                                                                                y4 -> do {guard (x4 == y4);
                                                                                                                                                                                                          return y3};
                                                                                                                                                                                           _ -> mzero};
                                                                                                                                                                                    return x0},
                                                                                                                                                                                do {let {x39 = O};
                                                                                                                                                                                    let {x38 = S x39};
                                                                                                                                                                                    let {x42 = O};
                                                                                                                                                                                    let {x41 = S x42};
                                                                                                                                                                                    let {x43 = x38};
                                                                                                                                                                                    x4 <- __conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                                                                                                                    let {x40 = Cons x41 x4};
                                                                                                                                                                                    let {x44 = x40};
                                                                                                                                                                                    let {x0 = Cons x43 x44};
                                                                                                                                                                                    return x0},
                                                                                                                                                                                do {x6 <- _conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                                                                                                                    (x0,
                                                                                                                                                                                     x5) <- do {x5 <- gen_containsosdO_x5;
                                                                                                                                                                                                let {x0 = Cons x5 x6};
                                                                                                                                                                                                return (x0,
                                                                                                                                                                                                        x5)};
                                                                                                                                                                                    return x0}]
__conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2 = Immature $ msum [do {let {x48 = O};
                                                                                                let {x52 = O};
                                                                                                let {x51 = S x52};
                                                                                                let {x50 = S x51};
                                                                                                let {x53 = x48};
                                                                                                (x54,
                                                                                                 x49) <- do {x49 <- gen___conodssO_x49;
                                                                                                             return (x49,
                                                                                                                     x49)};
                                                                                                let {x0 = Cons x53 x54};
                                                                                                x1 <- case x49 of
                                                                                                      {Cons y50
                                                                                                            y1 -> do {guard (x50 == y50);
                                                                                                                      return y1};
                                                                                                       _ -> mzero};
                                                                                                return x0},
                                                                                            do {let {x55 = O};
                                                                                                let {x57 = x55};
                                                                                                x1 <- _conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                                (x58,
                                                                                                 x56) <- do {x56 <- gen___conodssO_x56;
                                                                                                             return (x56,
                                                                                                                     x56)};
                                                                                                let {x0 = Cons x57 x58};
                                                                                                x2 <- case x56 of
                                                                                                      {Cons y2
                                                                                                            y1 -> do {guard (x1 == y1);
                                                                                                                      return y2};
                                                                                                       _ -> mzero};
                                                                                                return x0},
                                                                                            do {let {x59 = O};
                                                                                                let {x62 = O};
                                                                                                let {x61 = S x62};
                                                                                                let {x63 = x59};
                                                                                                x1 <- __conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                                let {x60 = Cons x61 x1};
                                                                                                let {x64 = x60};
                                                                                                let {x0 = Cons x63 x64};
                                                                                                return x0},
                                                                                            do {x4 <- _conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                                (x0,
                                                                                                 x3) <- do {x3 <- gen___conodssO_x3;
                                                                                                            let {x0 = Cons x3 x4};
                                                                                                            return (x0,
                                                                                                                    x3)};
                                                                                                return x0},
                                                                                            do {let {x66 = O};
                                                                                                let {x65 = S x66};
                                                                                                let {x67 = x65};
                                                                                                x4 <- __conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                                let {x0 = Cons x67 x4};
                                                                                                return x0}]
_conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2 = Immature $ msum [do {let {x46 = O};
                                                                                               let {x45 = S x46};
                                                                                               let {x47 = x45};
                                                                                               x1 <- __conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                               let {x0 = Cons x47 x1};
                                                                                               return x0},
                                                                                           do {x1 <- _conodssO gen___conodssO_x3 gen___conodssO_x49 gen___conodssO_x56 gen__conodssO_x2;
                                                                                               (x0,
                                                                                                x2) <- do {x2 <- gen__conodssO_x2;
                                                                                                           let {x0 = Cons x2 x1};
                                                                                                           return (x0,
                                                                                                                   x2)};
                                                                                               return x0}]