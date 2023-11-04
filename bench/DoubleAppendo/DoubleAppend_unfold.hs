module DoubleAppend_unfold where

import Stream
import Control.Monad (msum, guard, MonadPlus)
import Term

double_appendoIIIOffline :: MonadPlus m => Term -> Term -> Term -> m ()
double_appendoIIIOffline x0 x1 x2 = msum [do {appendoII x2 x1;
                                       guard (x0 == Nil);
                                       return ()},
                                   do {let {x5 = O};
                                       let {x4 = S x5};
                                       (x6, x3) <- case x0 of
                                                   {Cons y6 y3 -> return (y6, y3); _ -> mzero};
                                       guard (x6 == x4);
                                       appendoAppendoIII x1 x2 x3;
                                       return ()}]

appendoII :: MonadPlus m => Term -> Term -> m ()                          
appendoII x0 x1 = msum [do {let {x8 = O};
                            let {x7 = S x8};
                            let {x12 = O};
                            let {x11 = S x12};
                            let {x10 = S x11};
                            let {x17 = O};
                            let {x16 = S x17};
                            let {x15 = S x16};
                            let {x14 = S x15};
                            let {x19 = O};
                            let {x21 = O};
                            let {x24 = O};
                            let {x23 = S x24};
                            let {x28 = O};
                            let {x27 = S x28};
                            let {x26 = S x27};
                            let {x29 = Nil};
                            let {x25 = Cons x26 x29};
                            let {x22 = Cons x23 x25};
                            let {x20 = Cons x21 x22};
                            let {x18 = Cons x19 x20};
                            let {x13 = Cons x14 x18};
                            let {x9 = Cons x10 x13};
                            guard (x1 == Nil);
                            (x30, x31) <- case x0 of
                                          {Cons y30 y31 -> return (y30, y31); _ -> mzero};
                            guard (x30 == x7);
                            guard (x31 == x9);
                            return ()},
                        do {let {x33 = O};
                            let {x32 = S x33};
                            (x34, x2) <- case x1 of
                                         {Cons y34 y2 -> return (y34, y2); _ -> mzero};
                            guard (x34 == x32);
                            _appendoII x0 x2;
                            return ()}]
_appendoII :: MonadPlus m => Term -> Term -> m ()
_appendoII x0 x1 = msum [do {let {x37 = O};
                             let {x36 = S x37};
                             let {x35 = S x36};
                             let {x42 = O};
                             let {x41 = S x42};
                             let {x40 = S x41};
                             let {x39 = S x40};
                             let {x44 = O};
                             let {x46 = O};
                             let {x49 = O};
                             let {x48 = S x49};
                             let {x53 = O};
                             let {x52 = S x53};
                             let {x51 = S x52};
                             let {x54 = Nil};
                             let {x50 = Cons x51 x54};
                             let {x47 = Cons x48 x50};
                             let {x45 = Cons x46 x47};
                             let {x43 = Cons x44 x45};
                             let {x38 = Cons x39 x43};
                             guard (x1 == Nil);
                             (x55, x56) <- case x0 of
                                           {Cons y55 y56 -> return (y55, y56); _ -> mzero};
                             guard (x55 == x35);
                             guard (x56 == x38);
                             return ()},
                         do {let {x59 = O};
                             let {x58 = S x59};
                             let {x57 = S x58};
                             (x60, x2) <- case x1 of
                                          {Cons y60 y2 -> return (y60, y2); _ -> mzero};
                             guard (x60 == x57);
                             __appendoII x0 x2;
                             return ()}]
__appendoII :: MonadPlus m => Term -> Term -> m ()
__appendoII x0 x1 = msum [do {let {x64 = O};
                              let {x63 = S x64};
                              let {x62 = S x63};
                              let {x61 = S x62};
                              let {x66 = O};
                              let {x68 = O};
                              let {x71 = O};
                              let {x70 = S x71};
                              let {x75 = O};
                              let {x74 = S x75};
                              let {x73 = S x74};
                              let {x76 = Nil};
                              let {x72 = Cons x73 x76};
                              let {x69 = Cons x70 x72};
                              let {x67 = Cons x68 x69};
                              let {x65 = Cons x66 x67};
                              guard (x1 == Nil);
                              (x77, x78) <- case x0 of
                                            {Cons y77 y78 -> return (y77, y78); _ -> mzero};
                              guard (x77 == x61);
                              guard (x78 == x65);
                              return ()},
                          do {let {x82 = O};
                              let {x81 = S x82};
                              let {x80 = S x81};
                              let {x79 = S x80};
                              (x83, x2) <- case x1 of
                                           {Cons y83 y2 -> return (y83, y2); _ -> mzero};
                              guard (x83 == x79);
                              ___appendoII x0 x2;
                              return ()}]
___appendoII :: MonadPlus m => Term -> Term -> m ()
___appendoII x0 x1 = msum [do {let {x84 = O};
                               let {x86 = O};
                               let {x89 = O};
                               let {x88 = S x89};
                               let {x93 = O};
                               let {x92 = S x93};
                               let {x91 = S x92};
                               let {x94 = Nil};
                               let {x90 = Cons x91 x94};
                               let {x87 = Cons x88 x90};
                               let {x85 = Cons x86 x87};
                               guard (x1 == Nil);
                               (x95, x96) <- case x0 of
                                             {Cons y95 y96 -> return (y95, y96); _ -> mzero};
                               guard (x95 == x84);
                               guard (x96 == x85);
                               return ()},
                           do {let {x97 = O};
                               (x98, x2) <- case x1 of
                                            {Cons y98 y2 -> return (y98, y2); _ -> mzero};
                               guard (x98 == x97);
                               ____appendoII x0 x2;
                               return ()}]
____appendoII :: MonadPlus m => Term -> Term -> m ()
____appendoII x0 x1 = msum [do {let {x99 = O};
                                let {x102 = O};
                                let {x101 = S x102};
                                let {x106 = O};
                                let {x105 = S x106};
                                let {x104 = S x105};
                                let {x107 = Nil};
                                let {x103 = Cons x104 x107};
                                let {x100 = Cons x101 x103};
                                guard (x1 == Nil);
                                (x108, x109) <- case x0 of
                                                {Cons y108 y109 -> return (y108, y109); _ -> mzero};
                                guard (x108 == x99);
                                guard (x109 == x100);
                                return ()},
                            do {let {x110 = O};
                                (x111, x2) <- case x1 of
                                              {Cons y111 y2 -> return (y111, y2); _ -> mzero};
                                guard (x111 == x110);
                                _____appendoII x0 x2;
                                return ()}]
_____appendoII :: MonadPlus m => Term -> Term -> m ()
_____appendoII x0 x1 = msum [do {let {x113 = O};
                                 let {x112 = S x113};
                                 let {x117 = O};
                                 let {x116 = S x117};
                                 let {x115 = S x116};
                                 let {x118 = Nil};
                                 let {x114 = Cons x115 x118};
                                 guard (x1 == Nil);
                                 (x119, x120) <- case x0 of
                                                 {Cons y119 y120 -> return (y119, y120);
                                                  _ -> mzero};
                                 guard (x119 == x112);
                                 guard (x120 == x114);
                                 return ()},
                             do {let {x122 = O};
                                 let {x121 = S x122};
                                 (x123, x2) <- case x1 of
                                               {Cons y123 y2 -> return (y123, y2); _ -> mzero};
                                 guard (x123 == x121);
                                 ______appendoII x0 x2;
                                 return ()}]
______appendoII :: MonadPlus m => Term -> Term -> m ()
______appendoII x0 x1 = msum [do {let {x126 = O};
                                  let {x125 = S x126};
                                  let {x124 = S x125};
                                  let {x127 = Nil};
                                  guard (x1 == Nil);
                                  (x128, x129) <- case x0 of
                                                  {Cons y128 y129 -> return (y128, y129);
                                                   _ -> mzero};
                                  guard (x128 == x124);
                                  guard (x129 == x127);
                                  return ()},
                              do {let {x132 = O};
                                  let {x131 = S x132};
                                  let {x130 = S x131};
                                  let {x133 = Nil};
                                  (x134, x135) <- case x1 of
                                                  {Cons y134 y135 -> return (y134, y135);
                                                   _ -> mzero};
                                  guard (x134 == x130);
                                  guard (x135 == x133);
                                  guard (x0 == Nil);
                                  return ()}]
appendoAppendoIII :: MonadPlus m => Term -> Term -> Term -> m ()
appendoAppendoIII x0 x1 x2 = msum [do {_appendoII x1 x0;
                                       guard (x2 == Nil);
                                       return ()},
                                   do {let {x138 = O};
                                       let {x137 = S x138};
                                       let {x136 = S x137};
                                       (x139, x3) <- case x2 of
                                                     {Cons y139 y3 -> return (y139, y3);
                                                      _ -> mzero};
                                       guard (x139 == x136);
                                       _appendoAppendoIII x0 x1 x3;
                                       return ()}]
_appendoAppendoIII :: MonadPlus m => Term -> Term -> Term -> m ()
_appendoAppendoIII x0 x1 x2 = msum [do {__appendoII x1 x0;
                                        guard (x2 == Nil);
                                        return ()},
                                    do {let {x143 = O};
                                        let {x142 = S x143};
                                        let {x141 = S x142};
                                        let {x140 = S x141};
                                        (x144, x3) <- case x2 of
                                                      {Cons y144 y3 -> return (y144, y3);
                                                       _ -> mzero};
                                        guard (x144 == x140);
                                        __appendoAppendoIII x0 x1 x3;
                                        return ()}]
__appendoAppendoIII :: MonadPlus m => Term -> Term -> Term -> m ()
__appendoAppendoIII x0 x1 x2 = msum [do {___appendoII x1 x0;
                                         guard (x2 == Nil);
                                         return ()},
                                     do {let {x145 = O};
                                         (x146, x3) <- case x2 of
                                                       {Cons y146 y3 -> return (y146, y3);
                                                        _ -> mzero};
                                         guard (x146 == x145);
                                         ___appendoAppendoIII x0 x1 x3;
                                         return ()}]
___appendoAppendoIII :: MonadPlus m => Term -> Term -> Term -> m ()
___appendoAppendoIII x0 x1 x2 = msum [do {____appendoII x1 x0;
                                          guard (x2 == Nil);
                                          return ()},
                                      do {let {x147 = O};
                                          (x148, x3) <- case x2 of
                                                        {Cons y148 y3 -> return (y148, y3);
                                                         _ -> mzero};
                                          guard (x148 == x147);
                                          ____appendoAppendoIII x0 x1 x3;
                                          return ()}]
____appendoAppendoIII :: MonadPlus m => Term -> Term -> Term -> m ()
____appendoAppendoIII x0 x1 x2 = msum [do {_____appendoII x1 x0;
                                           guard (x2 == Nil);
                                           return ()},
                                       do {let {x150 = O};
                                           let {x149 = S x150};
                                           (x151, x3) <- case x2 of
                                                         {Cons y151 y3 -> return (y151, y3);
                                                          _ -> mzero};
                                           guard (x151 == x149);
                                           _____appendoAppendoIII x0 x1 x3;
                                           return ()}]
_____appendoAppendoIII :: MonadPlus m => Term -> Term -> Term -> m ()
_____appendoAppendoIII x0 x1 x2 = msum [do {______appendoII x1 x0;
                                            guard (x2 == Nil);
                                            return ()},
                                        do {let {x154 = O};
                                            let {x153 = S x154};
                                            let {x152 = S x153};
                                            (x155, x3) <- case x2 of
                                                          {Cons y155 y3 -> return (y155, y3);
                                                           _ -> mzero};
                                            guard (x155 == x152);
                                            _______appendoII x0 x3;
                                            guard (x1 == Nil);
                                            return ()}]
_______appendoII :: MonadPlus m => Term -> Term -> m ()
_______appendoII x0 x1 = msum [do {guard (x1 == Nil);
                                   guard (x0 == Nil);
                                   return ()}]
double_appendoIIOOffline :: MonadPlus m => Term -> Term -> m (Term)
double_appendoIIOOffline x0 x1 = msum [do {guard (x0 == Nil);
                                    x2 <- appendoOI x1;
                                    return x2},
                                do {let {x5 = O};
                                    let {x4 = S x5};
                                    (x6, x3) <- case x0 of
                                                {Cons y6 y3 -> return (y6, y3); _ -> mzero};
                                    guard (x6 == x4);
                                    x2 <- appendoAppendoIOI x1 x3;
                                    return x2}]
appendoOI :: MonadPlus m => Term -> m (Term)
appendoOI x1 = msum [do {let {x8 = O};
                         let {x7 = S x8};
                         let {x12 = O};
                         let {x11 = S x12};
                         let {x10 = S x11};
                         let {x17 = O};
                         let {x16 = S x17};
                         let {x15 = S x16};
                         let {x14 = S x15};
                         let {x19 = O};
                         let {x21 = O};
                         let {x24 = O};
                         let {x23 = S x24};
                         let {x28 = O};
                         let {x27 = S x28};
                         let {x26 = S x27};
                         let {x29 = Nil};
                         let {x25 = Cons x26 x29};
                         let {x22 = Cons x23 x25};
                         let {x20 = Cons x21 x22};
                         let {x18 = Cons x19 x20};
                         let {x13 = Cons x14 x18};
                         let {x9 = Cons x10 x13};
                         guard (x1 == Nil);
                         let {x30 = x7};
                         let {x31 = x9};
                         let {x0 = Cons x30 x31};
                         return x0},
                     do {let {x33 = O};
                         let {x32 = S x33};
                         (x34, x2) <- case x1 of
                                      {Cons y34 y2 -> return (y34, y2); _ -> mzero};
                         guard (x34 == x32);
                         x0 <- _appendoOI x2;
                         return x0}]
_appendoOI :: MonadPlus m => Term -> m (Term)
_appendoOI x1 = msum [do {let {x37 = O};
                          let {x36 = S x37};
                          let {x35 = S x36};
                          let {x42 = O};
                          let {x41 = S x42};
                          let {x40 = S x41};
                          let {x39 = S x40};
                          let {x44 = O};
                          let {x46 = O};
                          let {x49 = O};
                          let {x48 = S x49};
                          let {x53 = O};
                          let {x52 = S x53};
                          let {x51 = S x52};
                          let {x54 = Nil};
                          let {x50 = Cons x51 x54};
                          let {x47 = Cons x48 x50};
                          let {x45 = Cons x46 x47};
                          let {x43 = Cons x44 x45};
                          let {x38 = Cons x39 x43};
                          guard (x1 == Nil);
                          let {x55 = x35};
                          let {x56 = x38};
                          let {x0 = Cons x55 x56};
                          return x0},
                      do {let {x59 = O};
                          let {x58 = S x59};
                          let {x57 = S x58};
                          (x60, x2) <- case x1 of
                                       {Cons y60 y2 -> return (y60, y2); _ -> mzero};
                          guard (x60 == x57);
                          x0 <- __appendoOI x2;
                          return x0}]
__appendoOI :: MonadPlus m => Term -> m (Term)
__appendoOI x1 = msum [do {let {x64 = O};
                           let {x63 = S x64};
                           let {x62 = S x63};
                           let {x61 = S x62};
                           let {x66 = O};
                           let {x68 = O};
                           let {x71 = O};
                           let {x70 = S x71};
                           let {x75 = O};
                           let {x74 = S x75};
                           let {x73 = S x74};
                           let {x76 = Nil};
                           let {x72 = Cons x73 x76};
                           let {x69 = Cons x70 x72};
                           let {x67 = Cons x68 x69};
                           let {x65 = Cons x66 x67};
                           guard (x1 == Nil);
                           let {x77 = x61};
                           let {x78 = x65};
                           let {x0 = Cons x77 x78};
                           return x0},
                       do {let {x82 = O};
                           let {x81 = S x82};
                           let {x80 = S x81};
                           let {x79 = S x80};
                           (x83, x2) <- case x1 of
                                        {Cons y83 y2 -> return (y83, y2); _ -> mzero};
                           guard (x83 == x79);
                           x0 <- ___appendoOI x2;
                           return x0}]
___appendoOI :: MonadPlus m => Term -> m (Term)
___appendoOI x1 = msum [do {let {x84 = O};
                            let {x86 = O};
                            let {x89 = O};
                            let {x88 = S x89};
                            let {x93 = O};
                            let {x92 = S x93};
                            let {x91 = S x92};
                            let {x94 = Nil};
                            let {x90 = Cons x91 x94};
                            let {x87 = Cons x88 x90};
                            let {x85 = Cons x86 x87};
                            guard (x1 == Nil);
                            let {x95 = x84};
                            let {x96 = x85};
                            let {x0 = Cons x95 x96};
                            return x0},
                        do {let {x97 = O};
                            (x98, x2) <- case x1 of
                                         {Cons y98 y2 -> return (y98, y2); _ -> mzero};
                            guard (x98 == x97);
                            x0 <- ____appendoOI x2;
                            return x0}]
____appendoOI :: MonadPlus m => Term -> m (Term)
____appendoOI x1 = msum [do {let {x99 = O};
                             let {x102 = O};
                             let {x101 = S x102};
                             let {x106 = O};
                             let {x105 = S x106};
                             let {x104 = S x105};
                             let {x107 = Nil};
                             let {x103 = Cons x104 x107};
                             let {x100 = Cons x101 x103};
                             guard (x1 == Nil);
                             let {x108 = x99};
                             let {x109 = x100};
                             let {x0 = Cons x108 x109};
                             return x0},
                         do {let {x110 = O};
                             (x111, x2) <- case x1 of
                                           {Cons y111 y2 -> return (y111, y2); _ -> mzero};
                             guard (x111 == x110);
                             x0 <- _____appendoOI x2;
                             return x0}]
_____appendoOI :: MonadPlus m => Term -> m (Term)
_____appendoOI x1 = msum [do {let {x113 = O};
                              let {x112 = S x113};
                              let {x117 = O};
                              let {x116 = S x117};
                              let {x115 = S x116};
                              let {x118 = Nil};
                              let {x114 = Cons x115 x118};
                              guard (x1 == Nil);
                              let {x119 = x112};
                              let {x120 = x114};
                              let {x0 = Cons x119 x120};
                              return x0},
                          do {let {x122 = O};
                              let {x121 = S x122};
                              (x123, x2) <- case x1 of
                                            {Cons y123 y2 -> return (y123, y2); _ -> mzero};
                              guard (x123 == x121);
                              x0 <- ______appendoOI x2;
                              return x0}]
______appendoOI :: MonadPlus m => Term -> m (Term)
______appendoOI x1 = msum [do {let {x126 = O};
                               let {x125 = S x126};
                               let {x124 = S x125};
                               let {x127 = Nil};
                               guard (x1 == Nil);
                               let {x128 = x124};
                               let {x129 = x127};
                               let {x0 = Cons x128 x129};
                               return x0},
                           do {let {x132 = O};
                               let {x131 = S x132};
                               let {x130 = S x131};
                               let {x133 = Nil};
                               let {x0 = Nil};
                               (x134, x135) <- case x1 of
                                               {Cons y134 y135 -> return (y134, y135); _ -> mzero};
                               guard (x134 == x130);
                               guard (x135 == x133);
                               return x0}]
appendoAppendoIOI :: MonadPlus m => Term -> Term -> m (Term)
appendoAppendoIOI x0 x2 = msum [do {guard (x2 == Nil);
                                    x1 <- _appendoOI x0;
                                    return x1},
                                do {let {x138 = O};
                                    let {x137 = S x138};
                                    let {x136 = S x137};
                                    (x139, x3) <- case x2 of
                                                  {Cons y139 y3 -> return (y139, y3); _ -> mzero};
                                    guard (x139 == x136);
                                    x1 <- _appendoAppendoIOI x0 x3;
                                    return x1}]
_appendoAppendoIOI :: MonadPlus m => Term -> Term -> m (Term)
_appendoAppendoIOI x0 x2 = msum [do {guard (x2 == Nil);
                                     x1 <- __appendoOI x0;
                                     return x1},
                                 do {let {x143 = O};
                                     let {x142 = S x143};
                                     let {x141 = S x142};
                                     let {x140 = S x141};
                                     (x144, x3) <- case x2 of
                                                   {Cons y144 y3 -> return (y144, y3); _ -> mzero};
                                     guard (x144 == x140);
                                     x1 <- __appendoAppendoIOI x0 x3;
                                     return x1}]
__appendoAppendoIOI :: MonadPlus m => Term -> Term -> m (Term)
__appendoAppendoIOI x0 x2 = msum [do {guard (x2 == Nil);
                                      x1 <- ___appendoOI x0;
                                      return x1},
                                  do {let {x145 = O};
                                      (x146, x3) <- case x2 of
                                                    {Cons y146 y3 -> return (y146, y3); _ -> mzero};
                                      guard (x146 == x145);
                                      x1 <- ___appendoAppendoIOI x0 x3;
                                      return x1}]
___appendoAppendoIOI :: MonadPlus m => Term -> Term -> m (Term)
___appendoAppendoIOI x0 x2 = msum [do {guard (x2 == Nil);
                                       x1 <- ____appendoOI x0;
                                       return x1},
                                   do {let {x147 = O};
                                       (x148, x3) <- case x2 of
                                                     {Cons y148 y3 -> return (y148, y3);
                                                      _ -> mzero};
                                       guard (x148 == x147);
                                       x1 <- ____appendoAppendoIOI x0 x3;
                                       return x1}]
____appendoAppendoIOI :: MonadPlus m => Term -> Term -> m (Term)
____appendoAppendoIOI x0 x2 = msum [do {guard (x2 == Nil);
                                        x1 <- _____appendoOI x0;
                                        return x1},
                                    do {let {x150 = O};
                                        let {x149 = S x150};
                                        (x151, x3) <- case x2 of
                                                      {Cons y151 y3 -> return (y151, y3);
                                                       _ -> mzero};
                                        guard (x151 == x149);
                                        x1 <- _____appendoAppendoIOI x0 x3;
                                        return x1}]
_____appendoAppendoIOI :: MonadPlus m => Term -> Term -> m (Term)
_____appendoAppendoIOI x0 x2 = msum [do {guard (x2 == Nil);
                                         x1 <- ______appendoOI x0;
                                         return x1},
                                     do {let {x154 = O};
                                         let {x153 = S x154};
                                         let {x152 = S x153};
                                         let {x1 = Nil};
                                         (x155, x3) <- case x2 of
                                                       {Cons y155 y3 -> return (y155, y3);
                                                        _ -> mzero};
                                         guard (x155 == x152);
                                         _______appendoII x0 x3;
                                         return x1}]
double_appendoIOIOffline :: MonadPlus m => Term -> Term -> m (Term)
double_appendoIOIOffline x0 x2 = msum [do {guard (x0 == Nil);
                                    x1 <- appendoIO x2;
                                    return x1},
                                do {let {x5 = O};
                                    let {x4 = S x5};
                                    (x6, x3) <- case x0 of
                                                {Cons y6 y3 -> return (y6, y3); _ -> mzero};
                                    guard (x6 == x4);
                                    x1 <- appendoAppendoOII x2 x3;
                                    return x1}]
appendoIO :: MonadPlus m => Term -> m (Term)
appendoIO x0 = msum [do {let {x1 = Nil};
                         let {x8 = O};
                         let {x7 = S x8};
                         let {x12 = O};
                         let {x11 = S x12};
                         let {x10 = S x11};
                         let {x17 = O};
                         let {x16 = S x17};
                         let {x15 = S x16};
                         let {x14 = S x15};
                         let {x19 = O};
                         let {x21 = O};
                         let {x24 = O};
                         let {x23 = S x24};
                         let {x28 = O};
                         let {x27 = S x28};
                         let {x26 = S x27};
                         let {x29 = Nil};
                         let {x25 = Cons x26 x29};
                         let {x22 = Cons x23 x25};
                         let {x20 = Cons x21 x22};
                         let {x18 = Cons x19 x20};
                         let {x13 = Cons x14 x18};
                         let {x9 = Cons x10 x13};
                         (x30, x31) <- case x0 of
                                       {Cons y30 y31 -> return (y30, y31); _ -> mzero};
                         guard (x30 == x7);
                         guard (x31 == x9);
                         return x1},
                     do {let {x33 = O};
                         let {x32 = S x33};
                         let {x34 = x32};
                         x2 <- _appendoIO x0;
                         let {x1 = Cons x34 x2};
                         return x1}]
_appendoIO :: MonadPlus m => Term -> m (Term)
_appendoIO x0 = msum [do {let {x1 = Nil};
                          let {x37 = O};
                          let {x36 = S x37};
                          let {x35 = S x36};
                          let {x42 = O};
                          let {x41 = S x42};
                          let {x40 = S x41};
                          let {x39 = S x40};
                          let {x44 = O};
                          let {x46 = O};
                          let {x49 = O};
                          let {x48 = S x49};
                          let {x53 = O};
                          let {x52 = S x53};
                          let {x51 = S x52};
                          let {x54 = Nil};
                          let {x50 = Cons x51 x54};
                          let {x47 = Cons x48 x50};
                          let {x45 = Cons x46 x47};
                          let {x43 = Cons x44 x45};
                          let {x38 = Cons x39 x43};
                          (x55, x56) <- case x0 of
                                        {Cons y55 y56 -> return (y55, y56); _ -> mzero};
                          guard (x55 == x35);
                          guard (x56 == x38);
                          return x1},
                      do {let {x59 = O};
                          let {x58 = S x59};
                          let {x57 = S x58};
                          let {x60 = x57};
                          x2 <- __appendoIO x0;
                          let {x1 = Cons x60 x2};
                          return x1}]
__appendoIO :: MonadPlus m => Term -> m (Term)
__appendoIO x0 = msum [do {let {x1 = Nil};
                           let {x64 = O};
                           let {x63 = S x64};
                           let {x62 = S x63};
                           let {x61 = S x62};
                           let {x66 = O};
                           let {x68 = O};
                           let {x71 = O};
                           let {x70 = S x71};
                           let {x75 = O};
                           let {x74 = S x75};
                           let {x73 = S x74};
                           let {x76 = Nil};
                           let {x72 = Cons x73 x76};
                           let {x69 = Cons x70 x72};
                           let {x67 = Cons x68 x69};
                           let {x65 = Cons x66 x67};
                           (x77, x78) <- case x0 of
                                         {Cons y77 y78 -> return (y77, y78); _ -> mzero};
                           guard (x77 == x61);
                           guard (x78 == x65);
                           return x1},
                       do {let {x82 = O};
                           let {x81 = S x82};
                           let {x80 = S x81};
                           let {x79 = S x80};
                           let {x83 = x79};
                           x2 <- ___appendoIO x0;
                           let {x1 = Cons x83 x2};
                           return x1}]
___appendoIO :: MonadPlus m => Term -> m (Term)
___appendoIO x0 = msum [do {let {x1 = Nil};
                            let {x84 = O};
                            let {x86 = O};
                            let {x89 = O};
                            let {x88 = S x89};
                            let {x93 = O};
                            let {x92 = S x93};
                            let {x91 = S x92};
                            let {x94 = Nil};
                            let {x90 = Cons x91 x94};
                            let {x87 = Cons x88 x90};
                            let {x85 = Cons x86 x87};
                            (x95, x96) <- case x0 of
                                          {Cons y95 y96 -> return (y95, y96); _ -> mzero};
                            guard (x95 == x84);
                            guard (x96 == x85);
                            return x1},
                        do {let {x97 = O};
                            let {x98 = x97};
                            x2 <- ____appendoIO x0;
                            let {x1 = Cons x98 x2};
                            return x1}]
____appendoIO :: MonadPlus m => Term -> m (Term)
____appendoIO x0 = msum [do {let {x1 = Nil};
                             let {x99 = O};
                             let {x102 = O};
                             let {x101 = S x102};
                             let {x106 = O};
                             let {x105 = S x106};
                             let {x104 = S x105};
                             let {x107 = Nil};
                             let {x103 = Cons x104 x107};
                             let {x100 = Cons x101 x103};
                             (x108, x109) <- case x0 of
                                             {Cons y108 y109 -> return (y108, y109); _ -> mzero};
                             guard (x108 == x99);
                             guard (x109 == x100);
                             return x1},
                         do {let {x110 = O};
                             let {x111 = x110};
                             x2 <- _____appendoIO x0;
                             let {x1 = Cons x111 x2};
                             return x1}]
_____appendoIO :: MonadPlus m => Term -> m (Term)
_____appendoIO x0 = msum [do {let {x1 = Nil};
                              let {x113 = O};
                              let {x112 = S x113};
                              let {x117 = O};
                              let {x116 = S x117};
                              let {x115 = S x116};
                              let {x118 = Nil};
                              let {x114 = Cons x115 x118};
                              (x119, x120) <- case x0 of
                                              {Cons y119 y120 -> return (y119, y120); _ -> mzero};
                              guard (x119 == x112);
                              guard (x120 == x114);
                              return x1},
                          do {let {x122 = O};
                              let {x121 = S x122};
                              let {x123 = x121};
                              x2 <- ______appendoIO x0;
                              let {x1 = Cons x123 x2};
                              return x1}]
______appendoIO :: MonadPlus m => Term -> m (Term)
______appendoIO x0 = msum [do {let {x1 = Nil};
                               let {x126 = O};
                               let {x125 = S x126};
                               let {x124 = S x125};
                               let {x127 = Nil};
                               (x128, x129) <- case x0 of
                                               {Cons y128 y129 -> return (y128, y129); _ -> mzero};
                               guard (x128 == x124);
                               guard (x129 == x127);
                               return x1},
                           do {let {x132 = O};
                               let {x131 = S x132};
                               let {x130 = S x131};
                               let {x133 = Nil};
                               guard (x0 == Nil);
                               let {x134 = x130};
                               let {x135 = x133};
                               let {x1 = Cons x134 x135};
                               return x1}]
appendoAppendoOII :: MonadPlus m => Term -> Term -> m (Term)
appendoAppendoOII x1 x2 = msum [do {guard (x2 == Nil);
                                    x0 <- _appendoIO x1;
                                    return x0},
                                do {let {x138 = O};
                                    let {x137 = S x138};
                                    let {x136 = S x137};
                                    (x139, x3) <- case x2 of
                                                  {Cons y139 y3 -> return (y139, y3); _ -> mzero};
                                    guard (x139 == x136);
                                    x0 <- _appendoAppendoOII x1 x3;
                                    return x0}]
_appendoAppendoOII :: MonadPlus m => Term -> Term -> m (Term)
_appendoAppendoOII x1 x2 = msum [do {guard (x2 == Nil);
                                     x0 <- __appendoIO x1;
                                     return x0},
                                 do {let {x143 = O};
                                     let {x142 = S x143};
                                     let {x141 = S x142};
                                     let {x140 = S x141};
                                     (x144, x3) <- case x2 of
                                                   {Cons y144 y3 -> return (y144, y3); _ -> mzero};
                                     guard (x144 == x140);
                                     x0 <- __appendoAppendoOII x1 x3;
                                     return x0}]
__appendoAppendoOII :: MonadPlus m => Term -> Term -> m (Term)
__appendoAppendoOII x1 x2 = msum [do {guard (x2 == Nil);
                                      x0 <- ___appendoIO x1;
                                      return x0},
                                  do {let {x145 = O};
                                      (x146, x3) <- case x2 of
                                                    {Cons y146 y3 -> return (y146, y3); _ -> mzero};
                                      guard (x146 == x145);
                                      x0 <- ___appendoAppendoOII x1 x3;
                                      return x0}]
___appendoAppendoOII :: MonadPlus m => Term -> Term -> m (Term)
___appendoAppendoOII x1 x2 = msum [do {guard (x2 == Nil);
                                       x0 <- ____appendoIO x1;
                                       return x0},
                                   do {let {x147 = O};
                                       (x148, x3) <- case x2 of
                                                     {Cons y148 y3 -> return (y148, y3);
                                                      _ -> mzero};
                                       guard (x148 == x147);
                                       x0 <- ____appendoAppendoOII x1 x3;
                                       return x0}]
____appendoAppendoOII :: MonadPlus m => Term -> Term -> m (Term)
____appendoAppendoOII x1 x2 = msum [do {guard (x2 == Nil);
                                        x0 <- _____appendoIO x1;
                                        return x0},
                                    do {let {x150 = O};
                                        let {x149 = S x150};
                                        (x151, x3) <- case x2 of
                                                      {Cons y151 y3 -> return (y151, y3);
                                                       _ -> mzero};
                                        guard (x151 == x149);
                                        x0 <- _____appendoAppendoOII x1 x3;
                                        return x0}]
_____appendoAppendoOII :: MonadPlus m => Term -> Term -> m (Term)
_____appendoAppendoOII x1 x2 = msum [do {guard (x2 == Nil);
                                         x0 <- ______appendoIO x1;
                                         return x0},
                                     do {let {x154 = O};
                                         let {x153 = S x154};
                                         let {x152 = S x153};
                                         (x155, x3) <- case x2 of
                                                       {Cons y155 y3 -> return (y155, y3);
                                                        _ -> mzero};
                                         guard (x155 == x152);
                                         guard (x1 == Nil);
                                         x0 <- _______appendoOI x3;
                                         return x0}]
_______appendoOI :: MonadPlus m => Term -> m (Term)
_______appendoOI x1 = msum [do {let {x0 = Nil};
                                guard (x1 == Nil);
                                return x0}]
double_appendoIOOOffline :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term) 
double_appendoIOOOffline x0 gen______appendoOO_x2 gen_____appendoOO_x2 gen____appendoOO_x2 gen___appendoOO_x2 gen__appendoOO_x2 gen_appendoOO_x2 = msum [do {guard (x0 == Nil);
                                                                                                                                                      (x2,
                                                                                                                                                       x1) <- appendoOO gen_appendoOO_x2;
                                                                                                                                                      return (x1,
                                                                                                                                                              x2)},
                                                                                                                                                  do {let {x5 = O};
                                                                                                                                                      let {x4 = S x5};
                                                                                                                                                      (x6,
                                                                                                                                                       x3) <- case x0 of
                                                                                                                                                              {Cons y6
                                                                                                                                                                    y3 -> return (y6,
                                                                                                                                                                                  y3);
                                                                                                                                                               _ -> mzero};
                                                                                                                                                      guard (x6 == x4);
                                                                                                                                                      (x1,
                                                                                                                                                       x2) <- appendoAppendoOOI x3 gen______appendoOO_x2 gen_____appendoOO_x2 gen____appendoOO_x2 gen___appendoOO_x2 gen__appendoOO_x2;
                                                                                                                                                      return (x1,
                                                                                                                                                              x2)}]
appendoOO :: MonadPlus m => m Term -> m (Term, Term) 
appendoOO gen_appendoOO_x2 = msum [do {let {x1 = Nil};
                                       let {x8 = O};
                                       let {x7 = S x8};
                                       let {x12 = O};
                                       let {x11 = S x12};
                                       let {x10 = S x11};
                                       let {x17 = O};
                                       let {x16 = S x17};
                                       let {x15 = S x16};
                                       let {x14 = S x15};
                                       let {x19 = O};
                                       let {x21 = O};
                                       let {x24 = O};
                                       let {x23 = S x24};
                                       let {x28 = O};
                                       let {x27 = S x28};
                                       let {x26 = S x27};
                                       let {x29 = Nil};
                                       let {x25 = Cons x26 x29};
                                       let {x22 = Cons x23 x25};
                                       let {x20 = Cons x21 x22};
                                       let {x18 = Cons x19 x20};
                                       let {x13 = Cons x14 x18};
                                       let {x9 = Cons x10 x13};
                                       let {x30 = x7};
                                       let {x31 = x9};
                                       let {x0 = Cons x30 x31};
                                       return (x0, x1)},
                                   do {let {x33 = O};
                                       let {x32 = S x33};
                                       let {x34 = x32};
                                       (x1, x2) <- do {x2 <- gen_appendoOO_x2;
                                                       let {x1 = Cons x34 x2};
                                                       return (x1, x2)};
                                       x0 <- _appendoOI x2;
                                       return (x0, x1)}]
appendoAppendoOOI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term) 
appendoAppendoOOI x2 gen______appendoOO_x2 gen_____appendoOO_x2 gen____appendoOO_x2 gen___appendoOO_x2 gen__appendoOO_x2 = msum [do {guard (x2 == Nil);
                                                                                                                                     (x1,
                                                                                                                                      x0) <- _appendoOO gen__appendoOO_x2;
                                                                                                                                     return (x0,
                                                                                                                                             x1)},
                                                                                                                                 do {let {x138 = O};
                                                                                                                                     let {x137 = S x138};
                                                                                                                                     let {x136 = S x137};
                                                                                                                                     (x139,
                                                                                                                                      x3) <- case x2 of
                                                                                                                                             {Cons y139
                                                                                                                                                   y3 -> return (y139,
                                                                                                                                                                 y3);
                                                                                                                                              _ -> mzero};
                                                                                                                                     guard (x139 == x136);
                                                                                                                                     (x0,
                                                                                                                                      x1) <- _appendoAppendoOOI x3 gen______appendoOO_x2 gen_____appendoOO_x2 gen____appendoOO_x2 gen___appendoOO_x2;
                                                                                                                                     return (x0,
                                                                                                                                             x1)}]
_appendoOO :: MonadPlus m => m Term -> m (Term, Term) 
_appendoOO gen__appendoOO_x2 = msum [do {let {x1 = Nil};
                                         let {x37 = O};
                                         let {x36 = S x37};
                                         let {x35 = S x36};
                                         let {x42 = O};
                                         let {x41 = S x42};
                                         let {x40 = S x41};
                                         let {x39 = S x40};
                                         let {x44 = O};
                                         let {x46 = O};
                                         let {x49 = O};
                                         let {x48 = S x49};
                                         let {x53 = O};
                                         let {x52 = S x53};
                                         let {x51 = S x52};
                                         let {x54 = Nil};
                                         let {x50 = Cons x51 x54};
                                         let {x47 = Cons x48 x50};
                                         let {x45 = Cons x46 x47};
                                         let {x43 = Cons x44 x45};
                                         let {x38 = Cons x39 x43};
                                         let {x55 = x35};
                                         let {x56 = x38};
                                         let {x0 = Cons x55 x56};
                                         return (x0, x1)},
                                     do {let {x59 = O};
                                         let {x58 = S x59};
                                         let {x57 = S x58};
                                         let {x60 = x57};
                                         (x1, x2) <- do {x2 <- gen__appendoOO_x2;
                                                         let {x1 = Cons x60 x2};
                                                         return (x1, x2)};
                                         x0 <- __appendoOI x2;
                                         return (x0, x1)}]
_appendoAppendoOOI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term) 
_appendoAppendoOOI x2 gen______appendoOO_x2 gen_____appendoOO_x2 gen____appendoOO_x2 gen___appendoOO_x2 = msum [do {guard (x2 == Nil);
                                                                                                                    (x1,
                                                                                                                     x0) <- __appendoOO gen___appendoOO_x2;
                                                                                                                    return (x0,
                                                                                                                            x1)},
                                                                                                                do {let {x143 = O};
                                                                                                                    let {x142 = S x143};
                                                                                                                    let {x141 = S x142};
                                                                                                                    let {x140 = S x141};
                                                                                                                    (x144,
                                                                                                                     x3) <- case x2 of
                                                                                                                            {Cons y144
                                                                                                                                  y3 -> return (y144,
                                                                                                                                                y3);
                                                                                                                             _ -> mzero};
                                                                                                                    guard (x144 == x140);
                                                                                                                    (x0,
                                                                                                                     x1) <- __appendoAppendoOOI x3 gen______appendoOO_x2 gen_____appendoOO_x2 gen____appendoOO_x2;
                                                                                                                    return (x0,
                                                                                                                            x1)}]
__appendoOO :: MonadPlus m => m Term -> m (Term, Term) 
__appendoOO gen___appendoOO_x2 = msum [do {let {x1 = Nil};
                                           let {x64 = O};
                                           let {x63 = S x64};
                                           let {x62 = S x63};
                                           let {x61 = S x62};
                                           let {x66 = O};
                                           let {x68 = O};
                                           let {x71 = O};
                                           let {x70 = S x71};
                                           let {x75 = O};
                                           let {x74 = S x75};
                                           let {x73 = S x74};
                                           let {x76 = Nil};
                                           let {x72 = Cons x73 x76};
                                           let {x69 = Cons x70 x72};
                                           let {x67 = Cons x68 x69};
                                           let {x65 = Cons x66 x67};
                                           let {x77 = x61};
                                           let {x78 = x65};
                                           let {x0 = Cons x77 x78};
                                           return (x0, x1)},
                                       do {let {x82 = O};
                                           let {x81 = S x82};
                                           let {x80 = S x81};
                                           let {x79 = S x80};
                                           let {x83 = x79};
                                           (x1, x2) <- do {x2 <- gen___appendoOO_x2;
                                                           let {x1 = Cons x83 x2};
                                                           return (x1, x2)};
                                           x0 <- ___appendoOI x2;
                                           return (x0, x1)}]
__appendoAppendoOOI :: MonadPlus m => Term -> m Term -> m Term -> m Term -> m (Term, Term) 
__appendoAppendoOOI x2 gen______appendoOO_x2 gen_____appendoOO_x2 gen____appendoOO_x2 = msum [do {guard (x2 == Nil);
                                                                                                  (x1,
                                                                                                   x0) <- ___appendoOO gen____appendoOO_x2;
                                                                                                  return (x0,
                                                                                                          x1)},
                                                                                              do {let {x145 = O};
                                                                                                  (x146,
                                                                                                   x3) <- case x2 of
                                                                                                          {Cons y146
                                                                                                                y3 -> return (y146,
                                                                                                                              y3);
                                                                                                           _ -> mzero};
                                                                                                  guard (x146 == x145);
                                                                                                  (x0,
                                                                                                   x1) <- ___appendoAppendoOOI x3 gen______appendoOO_x2 gen_____appendoOO_x2;
                                                                                                  return (x0,
                                                                                                          x1)}]
___appendoOO :: MonadPlus m => m Term -> m (Term, Term) 
___appendoOO gen____appendoOO_x2 = msum [do {let {x1 = Nil};
                                             let {x84 = O};
                                             let {x86 = O};
                                             let {x89 = O};
                                             let {x88 = S x89};
                                             let {x93 = O};
                                             let {x92 = S x93};
                                             let {x91 = S x92};
                                             let {x94 = Nil};
                                             let {x90 = Cons x91 x94};
                                             let {x87 = Cons x88 x90};
                                             let {x85 = Cons x86 x87};
                                             let {x95 = x84};
                                             let {x96 = x85};
                                             let {x0 = Cons x95 x96};
                                             return (x0, x1)},
                                         do {let {x97 = O};
                                             let {x98 = x97};
                                             (x1, x2) <- do {x2 <- gen____appendoOO_x2;
                                                             let {x1 = Cons x98 x2};
                                                             return (x1, x2)};
                                             x0 <- ____appendoOI x2;
                                             return (x0, x1)}]
___appendoAppendoOOI :: MonadPlus m => Term -> m Term -> m Term -> m (Term, Term) 
___appendoAppendoOOI x2 gen______appendoOO_x2 gen_____appendoOO_x2 = msum [do {guard (x2 == Nil);
                                                                               (x1,
                                                                                x0) <- ____appendoOO gen_____appendoOO_x2;
                                                                               return (x0, x1)},
                                                                           do {let {x147 = O};
                                                                               (x148,
                                                                                x3) <- case x2 of
                                                                                       {Cons y148
                                                                                             y3 -> return (y148,
                                                                                                           y3);
                                                                                        _ -> mzero};
                                                                               guard (x148 == x147);
                                                                               (x0,
                                                                                x1) <- ____appendoAppendoOOI x3 gen______appendoOO_x2;
                                                                               return (x0, x1)}]
____appendoOO :: MonadPlus m => m Term -> m (Term, Term) 
____appendoOO gen_____appendoOO_x2 = msum [do {let {x1 = Nil};
                                               let {x99 = O};
                                               let {x102 = O};
                                               let {x101 = S x102};
                                               let {x106 = O};
                                               let {x105 = S x106};
                                               let {x104 = S x105};
                                               let {x107 = Nil};
                                               let {x103 = Cons x104 x107};
                                               let {x100 = Cons x101 x103};
                                               let {x108 = x99};
                                               let {x109 = x100};
                                               let {x0 = Cons x108 x109};
                                               return (x0, x1)},
                                           do {let {x110 = O};
                                               let {x111 = x110};
                                               (x1, x2) <- do {x2 <- gen_____appendoOO_x2;
                                                               let {x1 = Cons x111 x2};
                                                               return (x1, x2)};
                                               x0 <- _____appendoOI x2;
                                               return (x0, x1)}]
____appendoAppendoOOI :: MonadPlus m => Term -> m Term -> m (Term, Term) 
____appendoAppendoOOI x2 gen______appendoOO_x2 = msum [do {guard (x2 == Nil);
                                                           (x1,
                                                            x0) <- _____appendoOO gen______appendoOO_x2;
                                                           return (x0, x1)},
                                                       do {let {x150 = O};
                                                           let {x149 = S x150};
                                                           (x151, x3) <- case x2 of
                                                                         {Cons y151
                                                                               y3 -> return (y151,
                                                                                             y3);
                                                                          _ -> mzero};
                                                           guard (x151 == x149);
                                                           (x0, x1) <- _____appendoAppendoOOI x3;
                                                           return (x0, x1)}]
_____appendoOO :: MonadPlus m => m Term -> m (Term, Term) 
_____appendoOO gen______appendoOO_x2 = msum [do {let {x1 = Nil};
                                                 let {x113 = O};
                                                 let {x112 = S x113};
                                                 let {x117 = O};
                                                 let {x116 = S x117};
                                                 let {x115 = S x116};
                                                 let {x118 = Nil};
                                                 let {x114 = Cons x115 x118};
                                                 let {x119 = x112};
                                                 let {x120 = x114};
                                                 let {x0 = Cons x119 x120};
                                                 return (x0, x1)},
                                             do {let {x122 = O};
                                                 let {x121 = S x122};
                                                 let {x123 = x121};
                                                 (x1, x2) <- do {x2 <- gen______appendoOO_x2;
                                                                 let {x1 = Cons x123 x2};
                                                                 return (x1, x2)};
                                                 x0 <- ______appendoOI x2;
                                                 return (x0, x1)}]
                                                 
_____appendoAppendoOOI :: MonadPlus m => Term -> m (Term, Term) 
_____appendoAppendoOOI x2 = msum [do {guard (x2 == Nil);
                                      (x1, x0) <- ______appendoOO;
                                      return (x0, x1)},
                                  do {let {x154 = O};
                                      let {x153 = S x154};
                                      let {x152 = S x153};
                                      let {x1 = Nil};
                                      (x155, x3) <- case x2 of
                                                    {Cons y155 y3 -> return (y155, y3); _ -> mzero};
                                      guard (x155 == x152);
                                      x0 <- _______appendoOI x3;
                                      return (x0, x1)}]
______appendoOO :: MonadPlus m => m (Term, Term) 
______appendoOO = msum [do {let {x1 = Nil};
                            let {x126 = O};
                            let {x125 = S x126};
                            let {x124 = S x125};
                            let {x127 = Nil};
                            let {x128 = x124};
                            let {x129 = x127};
                            let {x0 = Cons x128 x129};
                            return (x0, x1)},
                        do {let {x132 = O};
                            let {x131 = S x132};
                            let {x130 = S x131};
                            let {x133 = Nil};
                            let {x0 = Nil};
                            let {x134 = x130};
                            let {x135 = x133};
                            let {x1 = Cons x134 x135};
                            return (x0, x1)}]
double_appendoOIIOffline :: MonadPlus m => Term -> Term -> m (Term) 
double_appendoOIIOffline x1 x2 = msum [do {appendoII x2 x1;
                                    let {x0 = Nil};
                                    return x0},
                                do {let {x5 = O};
                                    let {x4 = S x5};
                                    let {x6 = x4};
                                    x3 <- appendoAppendoIIO x1 x2;
                                    let {x0 = Cons x6 x3};
                                    return x0}]
appendoAppendoIIO :: MonadPlus m => Term -> Term -> m (Term)
appendoAppendoIIO x0 x1 = msum [do {_appendoII x1 x0;
                                    let {x2 = Nil};
                                    return x2},
                                do {let {x138 = O};
                                    let {x137 = S x138};
                                    let {x136 = S x137};
                                    let {x139 = x136};
                                    x3 <- _appendoAppendoIIO x0 x1;
                                    let {x2 = Cons x139 x3};
                                    return x2}]
_appendoAppendoIIO :: MonadPlus m => Term -> Term -> m (Term)
_appendoAppendoIIO x0 x1 = msum [do {__appendoII x1 x0;
                                     let {x2 = Nil};
                                     return x2},
                                 do {let {x143 = O};
                                     let {x142 = S x143};
                                     let {x141 = S x142};
                                     let {x140 = S x141};
                                     let {x144 = x140};
                                     x3 <- __appendoAppendoIIO x0 x1;
                                     let {x2 = Cons x144 x3};
                                     return x2}]
__appendoAppendoIIO :: MonadPlus m => Term -> Term -> m (Term)
__appendoAppendoIIO x0 x1 = msum [do {___appendoII x1 x0;
                                      let {x2 = Nil};
                                      return x2},
                                  do {let {x145 = O};
                                      let {x146 = x145};
                                      x3 <- ___appendoAppendoIIO x0 x1;
                                      let {x2 = Cons x146 x3};
                                      return x2}]
___appendoAppendoIIO :: MonadPlus m => Term -> Term -> m (Term)
___appendoAppendoIIO x0 x1 = msum [do {____appendoII x1 x0;
                                       let {x2 = Nil};
                                       return x2},
                                   do {let {x147 = O};
                                       let {x148 = x147};
                                       x3 <- ____appendoAppendoIIO x0 x1;
                                       let {x2 = Cons x148 x3};
                                       return x2}]
____appendoAppendoIIO :: MonadPlus m => Term -> Term -> m (Term)
____appendoAppendoIIO x0 x1 = msum [do {_____appendoII x1 x0;
                                        let {x2 = Nil};
                                        return x2},
                                    do {let {x150 = O};
                                        let {x149 = S x150};
                                        let {x151 = x149};
                                        x3 <- _____appendoAppendoIIO x0 x1;
                                        let {x2 = Cons x151 x3};
                                        return x2}]
_____appendoAppendoIIO :: MonadPlus m => Term -> Term -> m (Term)
_____appendoAppendoIIO x0 x1 = msum [do {______appendoII x1 x0;
                                         let {x2 = Nil};
                                         return x2},
                                     do {let {x154 = O};
                                         let {x153 = S x154};
                                         let {x152 = S x153};
                                         guard (x1 == Nil);
                                         let {x155 = x152};
                                         x3 <- _______appendoIO x0;
                                         let {x2 = Cons x155 x3};
                                         return x2}]
_______appendoIO :: MonadPlus m => Term -> m (Term)
_______appendoIO x0 = msum [do {let {x1 = Nil};
                                guard (x0 == Nil);
                                return x1}]
double_appendoOIOOffline :: MonadPlus m => Term -> m (Term, Term)
double_appendoOIOOffline x1 = msum [do {let {x0 = Nil};
                                 x2 <- appendoOI x1;
                                 return (x0, x2)},
                             do {let {x5 = O};
                                 let {x4 = S x5};
                                 let {x6 = x4};
                                 (x2, x3) <- appendoAppendoIOO x1;
                                 let {x0 = Cons x6 x3};
                                 return (x0, x2)}]
appendoAppendoIOO :: MonadPlus m => Term -> m (Term, Term)
appendoAppendoIOO x0 = msum [do {let {x2 = Nil};
                                 x1 <- _appendoOI x0;
                                 return (x1, x2)},
                             do {let {x138 = O};
                                 let {x137 = S x138};
                                 let {x136 = S x137};
                                 let {x139 = x136};
                                 (x1, x3) <- _appendoAppendoIOO x0;
                                 let {x2 = Cons x139 x3};
                                 return (x1, x2)}]
_appendoAppendoIOO :: MonadPlus m => Term -> m (Term, Term)
_appendoAppendoIOO x0 = msum [do {let {x2 = Nil};
                                  x1 <- __appendoOI x0;
                                  return (x1, x2)},
                              do {let {x143 = O};
                                  let {x142 = S x143};
                                  let {x141 = S x142};
                                  let {x140 = S x141};
                                  let {x144 = x140};
                                  (x1, x3) <- __appendoAppendoIOO x0;
                                  let {x2 = Cons x144 x3};
                                  return (x1, x2)}]
__appendoAppendoIOO :: MonadPlus m => Term -> m (Term, Term)
__appendoAppendoIOO x0 = msum [do {let {x2 = Nil};
                                   x1 <- ___appendoOI x0;
                                   return (x1, x2)},
                               do {let {x145 = O};
                                   let {x146 = x145};
                                   (x1, x3) <- ___appendoAppendoIOO x0;
                                   let {x2 = Cons x146 x3};
                                   return (x1, x2)}]
___appendoAppendoIOO :: MonadPlus m => Term -> m (Term, Term)
___appendoAppendoIOO x0 = msum [do {let {x2 = Nil};
                                    x1 <- ____appendoOI x0;
                                    return (x1, x2)},
                                do {let {x147 = O};
                                    let {x148 = x147};
                                    (x1, x3) <- ____appendoAppendoIOO x0;
                                    let {x2 = Cons x148 x3};
                                    return (x1, x2)}]
____appendoAppendoIOO :: MonadPlus m => Term -> m (Term, Term)
____appendoAppendoIOO x0 = msum [do {let {x2 = Nil};
                                     x1 <- _____appendoOI x0;
                                     return (x1, x2)},
                                 do {let {x150 = O};
                                     let {x149 = S x150};
                                     let {x151 = x149};
                                     (x1, x3) <- _____appendoAppendoIOO x0;
                                     let {x2 = Cons x151 x3};
                                     return (x1, x2)}]
_____appendoAppendoIOO :: MonadPlus m => Term -> m (Term, Term)
_____appendoAppendoIOO x0 = msum [do {let {x2 = Nil};
                                      x1 <- ______appendoOI x0;
                                      return (x1, x2)},
                                  do {let {x154 = O};
                                      let {x153 = S x154};
                                      let {x152 = S x153};
                                      let {x1 = Nil};
                                      let {x155 = x152};
                                      x3 <- _______appendoIO x0;
                                      let {x2 = Cons x155 x3};
                                      return (x1, x2)}]
double_appendoOOIOffline :: MonadPlus m => Term -> m Term -> m (Term, Term)
double_appendoOOIOffline x2 gen______appendoAppendoOIO_x3 = msum [do {let {x0 = Nil};
                                                               x1 <- appendoIO x2;
                                                               return (x0, x1)},
                                                           do {let {x5 = O};
                                                               let {x4 = S x5};
                                                               let {x6 = x4};
                                                               (x1,
                                                                x3) <- appendoAppendoOIO x2 gen______appendoAppendoOIO_x3;
                                                               let {x0 = Cons x6 x3};
                                                               return (x0, x1)}]
appendoAppendoOIO :: MonadPlus m => Term -> m Term -> m (Term, Term)
appendoAppendoOIO x1 gen______appendoAppendoOIO_x3 = msum [do {let {x2 = Nil};
                                                               x0 <- _appendoIO x1;
                                                               return (x0, x2)},
                                                           do {let {x138 = O};
                                                               let {x137 = S x138};
                                                               let {x136 = S x137};
                                                               let {x139 = x136};
                                                               (x0,
                                                                x3) <- _appendoAppendoOIO x1 gen______appendoAppendoOIO_x3;
                                                               let {x2 = Cons x139 x3};
                                                               return (x0, x2)}]
_appendoAppendoOIO :: MonadPlus m => Term -> m Term -> m (Term, Term)
_appendoAppendoOIO x1 gen______appendoAppendoOIO_x3 = msum [do {let {x2 = Nil};
                                                                x0 <- __appendoIO x1;
                                                                return (x0, x2)},
                                                            do {let {x143 = O};
                                                                let {x142 = S x143};
                                                                let {x141 = S x142};
                                                                let {x140 = S x141};
                                                                let {x144 = x140};
                                                                (x0,
                                                                 x3) <- __appendoAppendoOIO x1 gen______appendoAppendoOIO_x3;
                                                                let {x2 = Cons x144 x3};
                                                                return (x0, x2)}]
__appendoAppendoOIO :: MonadPlus m => Term -> m Term -> m (Term, Term)
__appendoAppendoOIO x1 gen______appendoAppendoOIO_x3 = msum [do {let {x2 = Nil};
                                                                 x0 <- ___appendoIO x1;
                                                                 return (x0, x2)},
                                                             do {let {x145 = O};
                                                                 let {x146 = x145};
                                                                 (x0,
                                                                  x3) <- ___appendoAppendoOIO x1 gen______appendoAppendoOIO_x3;
                                                                 let {x2 = Cons x146 x3};
                                                                 return (x0, x2)}]
___appendoAppendoOIO :: MonadPlus m => Term -> m Term -> m (Term, Term)
___appendoAppendoOIO x1 gen______appendoAppendoOIO_x3 = msum [do {let {x2 = Nil};
                                                                  x0 <- ____appendoIO x1;
                                                                  return (x0, x2)},
                                                              do {let {x147 = O};
                                                                  let {x148 = x147};
                                                                  (x0,
                                                                   x3) <- ____appendoAppendoOIO x1 gen______appendoAppendoOIO_x3;
                                                                  let {x2 = Cons x148 x3};
                                                                  return (x0, x2)}]
____appendoAppendoOIO :: MonadPlus m => Term -> m Term -> m (Term, Term)
____appendoAppendoOIO x1 gen______appendoAppendoOIO_x3 = msum [do {let {x2 = Nil};
                                                                   x0 <- _____appendoIO x1;
                                                                   return (x0, x2)},
                                                               do {let {x150 = O};
                                                                   let {x149 = S x150};
                                                                   let {x151 = x149};
                                                                   (x0,
                                                                    x3) <- _____appendoAppendoOIO x1 gen______appendoAppendoOIO_x3;
                                                                   let {x2 = Cons x151 x3};
                                                                   return (x0, x2)}]
_____appendoAppendoOIO :: MonadPlus m => Term -> m Term -> m (Term, Term)
_____appendoAppendoOIO x1 gen______appendoAppendoOIO_x3 = msum [do {let {x2 = Nil};
                                                                    x0 <- ______appendoIO x1;
                                                                    return (x0, x2)},
                                                                do {let {x154 = O};
                                                                    let {x153 = S x154};
                                                                    let {x152 = S x153};
                                                                    guard (x1 == Nil);
                                                                    let {x155 = x152};
                                                                    (x2,
                                                                     x3) <- do {x3 <- gen______appendoAppendoOIO_x3;
                                                                                let {x2 = Cons x155 x3};
                                                                                return (x2, x3)};
                                                                    x0 <- _______appendoOI x3;
                                                                    return (x0, x2)}]
double_appendoOOOOffline :: MonadPlus m => m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m Term -> m (Term, Term, Term)
double_appendoOOOOffline gen______appendoOO_x2 gen_____appendoOO_x2 gen____appendoOO_x2 gen___appendoOO_x2 gen__appendoOO_x2 gen_appendoOO_x2 gen_double_appendoOOO_x3 = msum [do {let {x0 = Nil};
                                                                                                                                                                            (x2,
                                                                                                                                                                             x1) <- appendoOO gen_appendoOO_x2;
                                                                                                                                                                            return (x0,
                                                                                                                                                                                    x1,
                                                                                                                                                                                    x2)},
                                                                                                                                                                        do {let {x5 = O};
                                                                                                                                                                            let {x4 = S x5};
                                                                                                                                                                            let {x6 = x4};
                                                                                                                                                                            (x0,
                                                                                                                                                                             x3) <- do {x3 <- gen_double_appendoOOO_x3;
                                                                                                                                                                                        let {x0 = Cons x6 x3};
                                                                                                                                                                                        return (x0,
                                                                                                                                                                                                x3)};
                                                                                                                                                                            (x1,
                                                                                                                                                                             x2) <- appendoAppendoOOI x3 gen______appendoOO_x2 gen_____appendoOO_x2 gen____appendoOO_x2 gen___appendoOO_x2 gen__appendoOO_x2;
                                                                                                                                                                            return (x0,
                                                                                                                                                                                    x1,
                                                                                                                                                                                    x2)}]