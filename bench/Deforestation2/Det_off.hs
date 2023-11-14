module Det_off where

import Stream
import Control.Monad
import Simple

rrI x0 = msum [do {rRI x0; return ()}, do {__neqRRI x0; return ()}]
__neqRRI x0 = msum [do {___________rRI x0; return ()}]
___________rRI x0 = msum [do {____________rRI x0; return ()},
                          do {_________________rRI x0; return ()}]
_________________rRI x0 = msum [do {__________________rRI x0;
                                    return ()},
                                do {____neqRRI x0; return ()}]
__________________rRI x0 = msum [do {___________________rRI x0;
                                     return ()}]
___________________rRI x0 = msum [do {_______________rI x0;
                                      return ()}]
_______________rI x0 = msum [do {let {x191 = O};
                                 let {x190 = S x191};
                                 let {x194 = O};
                                 let {x193 = S x194};
                                 (x195, x196) <- case x0 of
                                                 {Cons y195 y196 -> return (y195, y196);
                                                  _ -> mzero};
                                 guard (x195 == x190);
                                 let {x192 = x196};
                                 x1 <- case x192 of
                                       {Cons y193 y1 -> do {guard (x193 == y193); return y1};
                                        _ -> mzero};
                                 ___rI x1;
                                 return ()},
                             do {let {x198 = O};
                                 let {x197 = S x198};
                                 (x200, x201) <- case x0 of
                                                 {Cons y200 y201 -> return (y200, y201);
                                                  _ -> mzero};
                                 guard (x200 == x197);
                                 let {x199 = x201};
                                 (x2, x3) <- case x199 of
                                             {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                 _______neqRII x2 x3;
                                 return ()}]
____________rRI x0 = msum [do {_____________rRI x0; return ()},
                           do {___neqRRI x0; return ()}]
_____________rRI x0 = msum [do {______________rRI x0; return ()}]
______________rRI x0 = msum [do {_rI x0; return ()}]
_______neqRII x0 x1 = msum [do {let {x203 = O};
                                let {x202 = Cons x203 x1};
                                ___rI x202;
                                guard (x0 == O);
                                return ()},
                            do {x205 <- case x0 of
                                        {S y205 -> return y205; _ -> mzero};
                                let {x204 = x205};
                                x2 <- case x204 of
                                      {S y2 -> return y2; _ -> mzero};
                                let {x208 = S x2};
                                let {x207 = S x208};
                                let {x206 = Cons x207 x1};
                                ___rI x206;
                                return ()}]
____neqRRI x0 = msum [do {____________________rRI x0; return ()}]
____________________rRI x0 = msum [do {_____________________rRI x0;
                                       return ()}]
_____________________rRI x0 = msum [do {_________________rI x0;
                                        return ()}]
_________________rI x0 = msum [do {let {x229 = O};
                                   let {x228 = S x229};
                                   let {x232 = O};
                                   let {x231 = S x232};
                                   (x233, x234) <- case x0 of
                                                   {Cons y233 y234 -> return (y233, y234);
                                                    _ -> mzero};
                                   guard (x233 == x228);
                                   let {x230 = x234};
                                   x1 <- case x230 of
                                         {Cons y231 y1 -> do {guard (x231 == y231); return y1};
                                          _ -> mzero};
                                   _____________rI x1;
                                   return ()},
                               do {let {x236 = O};
                                   let {x235 = S x236};
                                   (x238, x239) <- case x0 of
                                                   {Cons y238 y239 -> return (y238, y239);
                                                    _ -> mzero};
                                   guard (x238 == x235);
                                   let {x237 = x239};
                                   (x2, x3) <- case x237 of
                                               {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                   _________neqRII x2 x3;
                                   return ()}]
_____________rI x0 = msum [do {let {x152 = O};
                               let {x154 = O};
                               (x155, x156) <- case x0 of
                                               {Cons y155 y156 -> return (y155, y156); _ -> mzero};
                               guard (x155 == x152);
                               let {x153 = x156};
                               x1 <- case x153 of
                                     {Cons y154 y1 -> do {guard (x154 == y154); return y1};
                                      _ -> mzero};
                               _____rI x1;
                               return ()},
                           do {let {x157 = O};
                               (x160, x161) <- case x0 of
                                               {Cons y160 y161 -> return (y160, y161); _ -> mzero};
                               guard (x160 == x157);
                               let {x158 = x161};
                               (x159, x3) <- case x158 of
                                             {Cons y159 y3 -> return (y159, y3); _ -> mzero};
                               x2 <- case x159 of
                                     {S y2 -> return y2; _ -> mzero};
                               let {x163 = S x2};
                               let {x162 = Cons x163 x3};
                               _____rI x162;
                               return ()}]
_________neqRII x0 x1 = msum [do {let {x241 = O};
                                  let {x240 = Cons x241 x1};
                                  _____________rI x240;
                                  guard (x0 == O);
                                  return ()},
                              do {x243 <- case x0 of
                                          {S y243 -> return y243; _ -> mzero};
                                  let {x242 = x243};
                                  x2 <- case x242 of
                                        {S y2 -> return y2; _ -> mzero};
                                  let {x246 = S x2};
                                  let {x245 = S x246};
                                  let {x244 = Cons x245 x1};
                                  _____________rI x244;
                                  return ()}]
_____rI x0 = msum [do {let {x62 = O};
                       let {x61 = S x62};
                       let {x60 = S x61};
                       let {x66 = O};
                       let {x65 = S x66};
                       let {x64 = S x65};
                       (x67, x68) <- case x0 of
                                     {Cons y67 y68 -> return (y67, y68); _ -> mzero};
                       guard (x67 == x60);
                       let {x63 = x68};
                       x1 <- case x63 of
                             {Cons y64 y1 -> do {guard (x64 == y64); return y1}; _ -> mzero};
                       ______rI x1;
                       return ()},
                   do {let {x71 = O};
                       let {x70 = S x71};
                       let {x69 = S x70};
                       (x73, x74) <- case x0 of
                                     {Cons y73 y74 -> return (y73, y74); _ -> mzero};
                       guard (x73 == x69);
                       let {x72 = x74};
                       (x2, x3) <- case x72 of
                                   {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                       neqRII x2 x3;
                       return ()}]
______rI x0 = msum [do {let {x75 = O};
                        let {x77 = O};
                        (x78, x79) <- case x0 of
                                      {Cons y78 y79 -> return (y78, y79); _ -> mzero};
                        guard (x78 == x75);
                        let {x76 = x79};
                        x1 <- case x76 of
                              {Cons y77 y1 -> do {guard (x77 == y77); return y1}; _ -> mzero};
                        _______rI x1;
                        return ()}]
_______rI x0 = msum [do {let {x80 = O};
                         let {x82 = O};
                         (x83, x84) <- case x0 of
                                       {Cons y83 y84 -> return (y83, y84); _ -> mzero};
                         guard (x83 == x80);
                         let {x81 = x84};
                         x1 <- case x81 of
                               {Cons y82 y1 -> do {guard (x82 == y82); return y1}; _ -> mzero};
                         ________rI x1;
                         return ()}]
________rI x0 = msum [do {let {x85 = O};
                          let {x87 = O};
                          (x88, x89) <- case x0 of
                                        {Cons y88 y89 -> return (y88, y89); _ -> mzero};
                          guard (x88 == x85);
                          let {x86 = x89};
                          x1 <- case x86 of
                                {Cons y87 y1 -> do {guard (x87 == y87); return y1}; _ -> mzero};
                          _________rI x1;
                          return ()}]
_________rI x0 = msum [do {let {x90 = O};
                           let {x92 = O};
                           let {x93 = Nil};
                           let {x91 = Cons x92 x93};
                           (x94, x95) <- case x0 of
                                         {Cons y94 y95 -> return (y94, y95); _ -> mzero};
                           guard (x94 == x90);
                           guard (x95 == x91);
                           return ()}]
___neqRRI x0 = msum [do {_______________rRI x0; return ()}]
_______________rRI x0 = msum [do {________________rRI x0;
                                  return ()}]
________________rRI x0 = msum [do {___________rI x0; return ()}]
___________rI x0 = msum [do {let {x136 = O};
                             let {x135 = S x136};
                             let {x139 = O};
                             let {x138 = S x139};
                             (x140, x141) <- case x0 of
                                             {Cons y140 y141 -> return (y140, y141); _ -> mzero};
                             guard (x140 == x135);
                             let {x137 = x141};
                             x1 <- case x137 of
                                   {Cons y138 y1 -> do {guard (x138 == y138); return y1};
                                    _ -> mzero};
                             ____________rI x1;
                             return ()},
                         do {let {x143 = O};
                             let {x142 = S x143};
                             (x145, x146) <- case x0 of
                                             {Cons y145 y146 -> return (y145, y146); _ -> mzero};
                             guard (x145 == x142);
                             let {x144 = x146};
                             (x2, x3) <- case x144 of
                                         {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                             _____neqRII x2 x3;
                             return ()}]
____________rI x0 = msum [do {let {x147 = O};
                              let {x149 = O};
                              (x150, x151) <- case x0 of
                                              {Cons y150 y151 -> return (y150, y151); _ -> mzero};
                              guard (x150 == x147);
                              let {x148 = x151};
                              x1 <- case x148 of
                                    {Cons y149 y1 -> do {guard (x149 == y149); return y1};
                                     _ -> mzero};
                              _____________rI x1;
                              return ()}]
_____neqRII x0 x1 = msum [do {let {x165 = O};
                              let {x164 = Cons x165 x1};
                              ____________rI x164;
                              guard (x0 == O);
                              return ()},
                          do {x167 <- case x0 of
                                      {S y167 -> return y167; _ -> mzero};
                              let {x166 = x167};
                              x2 <- case x166 of
                                    {S y2 -> return y2; _ -> mzero};
                              let {x170 = S x2};
                              let {x169 = S x170};
                              let {x168 = Cons x169 x1};
                              ____________rI x168;
                              return ()}]
___rI x0 = msum [do {let {x33 = O};
                     let {x35 = O};
                     (x36, x37) <- case x0 of
                                   {Cons y36 y37 -> return (y36, y37); _ -> mzero};
                     guard (x36 == x33);
                     let {x34 = x37};
                     x1 <- case x34 of
                           {Cons y35 y1 -> do {guard (x35 == y35); return y1}; _ -> mzero};
                     ____rI x1;
                     return ()},
                 do {let {x38 = O};
                     (x41, x42) <- case x0 of
                                   {Cons y41 y42 -> return (y41, y42); _ -> mzero};
                     guard (x41 == x38);
                     let {x39 = x42};
                     (x40, x3) <- case x39 of
                                  {Cons y40 y3 -> return (y40, y3); _ -> mzero};
                     x2 <- case x40 of
                           {S y2 -> return y2; _ -> mzero};
                     let {x44 = S x2};
                     let {x43 = Cons x44 x3};
                     ____rI x43;
                     return ()}]
____rI x0 = msum [do {let {x47 = O};
                      let {x46 = S x47};
                      let {x45 = S x46};
                      let {x51 = O};
                      let {x50 = S x51};
                      let {x49 = S x50};
                      (x52, x53) <- case x0 of
                                    {Cons y52 y53 -> return (y52, y53); _ -> mzero};
                      guard (x52 == x45);
                      let {x48 = x53};
                      x1 <- case x48 of
                            {Cons y49 y1 -> do {guard (x49 == y49); return y1}; _ -> mzero};
                      _____rI x1;
                      return ()},
                  do {let {x56 = O};
                      let {x55 = S x56};
                      let {x54 = S x55};
                      (x58, x59) <- case x0 of
                                    {Cons y58 y59 -> return (y58, y59); _ -> mzero};
                      guard (x58 == x54);
                      let {x57 = x59};
                      (x2, x3) <- case x57 of
                                  {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                      _neqRII x2 x3;
                      return ()}]
_neqRII x0 x1 = msum [do {let {x99 = O};
                          let {x98 = Cons x99 x1};
                          _____rI x98;
                          guard (x0 == O);
                          return ()},
                      do {x2 <- case x0 of
                                {S y2 -> return y2; _ -> mzero};
                          __neqRII x1 x2;
                          return ()}]
__neqRII x0 x1 = msum [do {let {x102 = O};
                           let {x101 = S x102};
                           let {x100 = Cons x101 x0};
                           _____rI x100;
                           guard (x1 == O);
                           return ()},
                       do {x104 <- case x1 of
                                   {S y104 -> return y104; _ -> mzero};
                           let {x103 = x104};
                           x2 <- case x103 of
                                 {S y2 -> return y2; _ -> mzero};
                           let {x108 = S x2};
                           let {x107 = S x108};
                           let {x106 = S x107};
                           let {x105 = Cons x106 x0};
                           _____rI x105;
                           return ()}]
_rI x0 = msum [do {let {x17 = O};
                   let {x16 = S x17};
                   let {x20 = O};
                   let {x19 = S x20};
                   (x21, x22) <- case x0 of
                                 {Cons y21 y22 -> return (y21, y22); _ -> mzero};
                   guard (x21 == x16);
                   let {x18 = x22};
                   x1 <- case x18 of
                         {Cons y19 y1 -> do {guard (x19 == y19); return y1}; _ -> mzero};
                   __rI x1;
                   return ()},
               do {let {x24 = O};
                   let {x23 = S x24};
                   (x26, x27) <- case x0 of
                                 {Cons y26 y27 -> return (y26, y27); _ -> mzero};
                   guard (x26 == x23);
                   let {x25 = x27};
                   (x2, x3) <- case x25 of
                               {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                   ___neqRII x2 x3;
                   return ()}]
___neqRII x0 x1 = msum [do {let {x110 = O};
                            let {x109 = Cons x110 x1};
                            __rI x109;
                            guard (x0 == O);
                            return ()},
                        do {x112 <- case x0 of
                                    {S y112 -> return y112; _ -> mzero};
                            let {x111 = x112};
                            x2 <- case x111 of
                                  {S y2 -> return y2; _ -> mzero};
                            let {x115 = S x2};
                            let {x114 = S x115};
                            let {x113 = Cons x114 x1};
                            __rI x113;
                            return ()}]
__rI x0 = msum [do {let {x28 = O};
                    let {x30 = O};
                    (x31, x32) <- case x0 of
                                  {Cons y31 y32 -> return (y31, y32); _ -> mzero};
                    guard (x31 == x28);
                    let {x29 = x32};
                    x1 <- case x29 of
                          {Cons y30 y1 -> do {guard (x30 == y30); return y1}; _ -> mzero};
                    ___rI x1;
                    return ()}]
neqRII x0 x1 = msum [do {let {x97 = O};
                         let {x96 = Cons x97 x1};
                         ______rI x96;
                         guard (x0 == O);
                         return ()}]
rRI x0 = msum [do {_rRI x0; return ()},
               do {______rRI x0; return ()}]
______rRI x0 = msum [do {_______rRI x0; return ()},
                     do {_neqRRI x0; return ()}]
_______rRI x0 = msum [do {________rRI x0; return ()}]
________rRI x0 = msum [do {______________rI x0; return ()}]
______________rI x0 = msum [do {let {x179 = O};
                                let {x178 = S x179};
                                let {x182 = O};
                                let {x181 = S x182};
                                (x183, x184) <- case x0 of
                                                {Cons y183 y184 -> return (y183, y184); _ -> mzero};
                                guard (x183 == x178);
                                let {x180 = x184};
                                x1 <- case x180 of
                                      {Cons y181 y1 -> do {guard (x181 == y181); return y1};
                                       _ -> mzero};
                                _______________rI x1;
                                return ()},
                            do {let {x186 = O};
                                let {x185 = S x186};
                                (x188, x189) <- case x0 of
                                                {Cons y188 y189 -> return (y188, y189); _ -> mzero};
                                guard (x188 == x185);
                                let {x187 = x189};
                                (x2, x3) <- case x187 of
                                            {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                ________neqRII x2 x3;
                                return ()}]
________neqRII x0 x1 = msum [do {let {x210 = O};
                                 let {x209 = Cons x210 x1};
                                 _______________rI x209;
                                 guard (x0 == O);
                                 return ()},
                             do {x212 <- case x0 of
                                         {S y212 -> return y212; _ -> mzero};
                                 let {x211 = x212};
                                 x2 <- case x211 of
                                       {S y2 -> return y2; _ -> mzero};
                                 let {x215 = S x2};
                                 let {x214 = S x215};
                                 let {x213 = Cons x214 x1};
                                 _______________rI x213;
                                 return ()}]
_neqRRI x0 = msum [do {_________rRI x0; return ()}]
_________rRI x0 = msum [do {__________rRI x0; return ()}]
__________rRI x0 = msum [do {________________rI x0; return ()}]
________________rI x0 = msum [do {let {x217 = O};
                                  let {x216 = S x217};
                                  let {x220 = O};
                                  let {x219 = S x220};
                                  (x221, x222) <- case x0 of
                                                  {Cons y221 y222 -> return (y221, y222);
                                                   _ -> mzero};
                                  guard (x221 == x216);
                                  let {x218 = x222};
                                  x1 <- case x218 of
                                        {Cons y219 y1 -> do {guard (x219 == y219); return y1};
                                         _ -> mzero};
                                  _________________rI x1;
                                  return ()},
                              do {let {x224 = O};
                                  let {x223 = S x224};
                                  (x226, x227) <- case x0 of
                                                  {Cons y226 y227 -> return (y226, y227);
                                                   _ -> mzero};
                                  guard (x226 == x223);
                                  let {x225 = x227};
                                  (x2, x3) <- case x225 of
                                              {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                  __________neqRII x2 x3;
                                  return ()}]
__________neqRII x0 x1 = msum [do {let {x248 = O};
                                   let {x247 = Cons x248 x1};
                                   _________________rI x247;
                                   guard (x0 == O);
                                   return ()},
                               do {x250 <- case x0 of
                                           {S y250 -> return y250; _ -> mzero};
                                   let {x249 = x250};
                                   x2 <- case x249 of
                                         {S y2 -> return y2; _ -> mzero};
                                   let {x253 = S x2};
                                   let {x252 = S x253};
                                   let {x251 = Cons x252 x1};
                                   _________________rI x251;
                                   return ()}]
_rRI x0 = msum [do {__rRI x0; return ()},
                do {neqRRI x0; return ()}]
__rRI x0 = msum [do {___rRI x0; return ()}]
___rRI x0 = msum [do {rI x0; return ()}]
neqRRI x0 = msum [do {____rRI x0; return ()}]
____rRI x0 = msum [do {_____rRI x0; return ()}]
_____rRI x0 = msum [do {__________rI x0; return ()}]
__________rI x0 = msum [do {let {x124 = O};
                            let {x123 = S x124};
                            let {x127 = O};
                            let {x126 = S x127};
                            (x128, x129) <- case x0 of
                                            {Cons y128 y129 -> return (y128, y129); _ -> mzero};
                            guard (x128 == x123);
                            let {x125 = x129};
                            x1 <- case x125 of
                                  {Cons y126 y1 -> do {guard (x126 == y126); return y1};
                                   _ -> mzero};
                            ___________rI x1;
                            return ()},
                        do {let {x131 = O};
                            let {x130 = S x131};
                            (x133, x134) <- case x0 of
                                            {Cons y133 y134 -> return (y133, y134); _ -> mzero};
                            guard (x133 == x130);
                            let {x132 = x134};
                            (x2, x3) <- case x132 of
                                        {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                            ______neqRII x2 x3;
                            return ()}]
______neqRII x0 x1 = msum [do {let {x172 = O};
                               let {x171 = Cons x172 x1};
                               ___________rI x171;
                               guard (x0 == O);
                               return ()},
                           do {x174 <- case x0 of
                                       {S y174 -> return y174; _ -> mzero};
                               let {x173 = x174};
                               x2 <- case x173 of
                                     {S y2 -> return y2; _ -> mzero};
                               let {x177 = S x2};
                               let {x176 = S x177};
                               let {x175 = Cons x176 x1};
                               ___________rI x175;
                               return ()}]
rI x0 = msum [do {let {x5 = O};
                  let {x4 = S x5};
                  let {x8 = O};
                  let {x7 = S x8};
                  (x9, x10) <- case x0 of
                               {Cons y9 y10 -> return (y9, y10); _ -> mzero};
                  guard (x9 == x4);
                  let {x6 = x10};
                  x1 <- case x6 of
                        {Cons y7 y1 -> do {guard (x7 == y7); return y1}; _ -> mzero};
                  _rI x1;
                  return ()},
              do {let {x12 = O};
                  let {x11 = S x12};
                  (x14, x15) <- case x0 of
                                {Cons y14 y15 -> return (y14, y15); _ -> mzero};
                  guard (x14 == x11);
                  let {x13 = x15};
                  (x2, x3) <- case x13 of
                              {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                  ____neqRII x2 x3;
                  return ()}]
____neqRII x0 x1 = msum [do {let {x117 = O};
                             let {x116 = Cons x117 x1};
                             _rI x116;
                             guard (x0 == O);
                             return ()},
                         do {x119 <- case x0 of
                                     {S y119 -> return y119; _ -> mzero};
                             let {x118 = x119};
                             x2 <- case x118 of
                                   {S y2 -> return y2; _ -> mzero};
                             let {x122 = S x2};
                             let {x121 = S x122};
                             let {x120 = Cons x121 x1};
                             _rI x120;
                             return ()}]
--rrO gen__________________rO_x237 gen_________________rO_x225 gen________________rO_x199 gen_______________rO_x187 gen____________rO_x144 gen___________rO_x132 gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13 = msum [do {x0 <- rRO gen__________________rO_x237 gen_________________rO_x225 gen________________rO_x199 gen_______________rO_x187 gen____________rO_x144 gen___________rO_x132 gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13;
--                                                                                                                                                                                                                                 return x0},
--                                                                                                                                                                                                                             do {x0 <- __neqRRO gen__________________rO_x237 gen________________rO_x199 gen____________rO_x144 gen______rO_x72 gen_____rO_x57 gen__rO_x25;
--                                                                                                                                                                                                                                 return x0}]
--__neqRRO gen__________________rO_x237 gen________________rO_x199 gen____________rO_x144 gen______rO_x72 gen_____rO_x57 gen__rO_x25 = msum [do {x0 <- ___________rRO gen__________________rO_x237 gen________________rO_x199 gen____________rO_x144 gen______rO_x72 gen_____rO_x57 gen__rO_x25;
--                                                                                                                                               return x0}]
--___________rRO gen__________________rO_x237 gen________________rO_x199 gen____________rO_x144 gen______rO_x72 gen_____rO_x57 gen__rO_x25 = msum [do {x0 <- ____________rRO gen____________rO_x144 gen______rO_x72 gen_____rO_x57 gen__rO_x25;
--                                                                                                                                                     return x0},
--                                                                                                                                                 do {x0 <- _________________rRO gen__________________rO_x237 gen________________rO_x199 gen______rO_x72 gen_____rO_x57;
--                                                                                                                                                     return x0}]
--_________________rRO gen__________________rO_x237 gen________________rO_x199 gen______rO_x72 gen_____rO_x57 = msum [do {x0 <- __________________rRO gen________________rO_x199 gen______rO_x72 gen_____rO_x57;
--                                                                                                                        return x0},
--                                                                                                                    do {x0 <- ____neqRRO gen__________________rO_x237 gen______rO_x72;
--                                                                                                                        return x0}]
--__________________rRO gen________________rO_x199 gen______rO_x72 gen_____rO_x57 = msum [do {x0 <- ___________________rRO gen________________rO_x199 gen______rO_x72 gen_____rO_x57;
--                                                                                            return x0}]
--___________________rRO gen________________rO_x199 gen______rO_x72 gen_____rO_x57 = msum [do {x0 <- _______________rO gen________________rO_x199 gen______rO_x72 gen_____rO_x57;
--                                                                                             return x0}]
--_______________rO gen________________rO_x199 gen______rO_x72 gen_____rO_x57 = msum [do {let {x191 = O};
--                                                                                        let {x190 = S x191};
--                                                                                        let {x194 = O};
--                                                                                        let {x193 = S x194};
--                                                                                        let {x195 = x190};
--                                                                                        x1 <- ___rO gen______rO_x72 gen_____rO_x57;
--                                                                                        let {x192 = Cons x193 x1};
--                                                                                        let {x196 = x192};
--                                                                                        let {x0 = Cons x195 x196};
--                                                                                        return x0},
--                                                                                    do {let {x198 = O};
--                                                                                        let {x197 = S x198};
--                                                                                        let {x200 = x197};
--                                                                                        (x201,
--                                                                                         x199) <- do {x199 <- gen________________rO_x199;
--                                                                                                      return (x199,
--                                                                                                              x199)};
--                                                                                        let {x0 = Cons x200 x201};
--                                                                                        (x2,
--                                                                                         x3) <- case x199 of
--                                                                                                {Cons y2
--                                                                                                      y3 -> return (y2,
--                                                                                                                    y3);
--                                                                                                 _ -> mzero};
--                                                                                        _______neqRII x2 x3;
--                                                                                        return x0}]
--____________rRO gen____________rO_x144 gen______rO_x72 gen_____rO_x57 gen__rO_x25 = msum [do {x0 <- _____________rRO gen______rO_x72 gen_____rO_x57 gen__rO_x25;
--                                                                                              return x0},
--                                                                                          do {x0 <- ___neqRRO gen____________rO_x144 gen______rO_x72;
--                                                                                              return x0}]
--_____________rRO gen______rO_x72 gen_____rO_x57 gen__rO_x25 = msum [do {x0 <- ______________rRO gen______rO_x72 gen_____rO_x57 gen__rO_x25;
--                                                                        return x0}]
--______________rRO gen______rO_x72 gen_____rO_x57 gen__rO_x25 = msum [do {x0 <- _rO gen______rO_x72 gen_____rO_x57 gen__rO_x25;
--                                                                         return x0}]
--____neqRRO gen__________________rO_x237 gen______rO_x72 = msum [do {x0 <- ____________________rRO gen__________________rO_x237 gen______rO_x72;
--                                                                    return x0}]
--____________________rRO gen__________________rO_x237 gen______rO_x72 = msum [do {x0 <- _____________________rRO gen__________________rO_x237 gen______rO_x72;
--                                                                                 return x0}]
--_____________________rRO gen__________________rO_x237 gen______rO_x72 = msum [do {x0 <- _________________rO gen__________________rO_x237 gen______rO_x72;
--                                                                                  return x0}]
--_________________rO gen__________________rO_x237 gen______rO_x72 = msum [do {let {x229 = O};
--                                                                             let {x228 = S x229};
--                                                                             let {x232 = O};
--                                                                             let {x231 = S x232};
--                                                                             let {x233 = x228};
--                                                                             x1 <- _____________rO gen______rO_x72;
--                                                                             let {x230 = Cons x231 x1};
--                                                                             let {x234 = x230};
--                                                                             let {x0 = Cons x233 x234};
--                                                                             return x0},
--                                                                         do {let {x236 = O};
--                                                                             let {x235 = S x236};
--                                                                             let {x238 = x235};
--                                                                             (x239,
--                                                                              x237) <- do {x237 <- gen__________________rO_x237;
--                                                                                           return (x237,
--                                                                                                   x237)};
--                                                                             let {x0 = Cons x238 x239};
--                                                                             (x2,
--                                                                              x3) <- case x237 of
--                                                                                     {Cons y2
--                                                                                           y3 -> return (y2,
--                                                                                                         y3);
--                                                                                      _ -> mzero};
--                                                                             _________neqRII x2 x3;
--                                                                             return x0}]
--_____________rO gen______rO_x72 = msum [do {let {x152 = O};
--                                            let {x154 = O};
--                                            let {x155 = x152};
--                                            x1 <- _____rO gen______rO_x72;
--                                            let {x153 = Cons x154 x1};
--                                            let {x156 = x153};
--                                            let {x0 = Cons x155 x156};
--                                            return x0},
--                                        do {let {x157 = O};
--                                            let {x160 = x157};
--                                            x162 <- _____rO gen______rO_x72;
--                                            (x163, x3) <- case x162 of
--                                                          {Cons y163 y3 -> return (y163, y3);
--                                                           _ -> mzero};
--                                            x2 <- case x163 of
--                                                  {S y2 -> return y2; _ -> mzero};
--                                            let {x159 = S x2};
--                                            let {x158 = Cons x159 x3};
--                                            let {x161 = x158};
--                                            let {x0 = Cons x160 x161};
--                                            return x0}]
--_____rO gen______rO_x72 = msum [do {let {x62 = O};
--                                    let {x61 = S x62};
--                                    let {x60 = S x61};
--                                    let {x66 = O};
--                                    let {x65 = S x66};
--                                    let {x64 = S x65};
--                                    let {x67 = x60};
--                                    x1 <- ______rO;
--                                    let {x63 = Cons x64 x1};
--                                    let {x68 = x63};
--                                    let {x0 = Cons x67 x68};
--                                    return x0},
--                                do {let {x71 = O};
--                                    let {x70 = S x71};
--                                    let {x69 = S x70};
--                                    let {x73 = x69};
--                                    (x74, x72) <- do {x72 <- gen______rO_x72; return (x72, x72)};
--                                    let {x0 = Cons x73 x74};
--                                    (x2, x3) <- case x72 of
--                                                {Cons y2 y3 -> return (y2, y3); _ -> mzero};
--                                    neqRII x2 x3;
--                                    return x0}]
--______rO = msum [do {let {x75 = O};
--                     let {x77 = O};
--                     let {x78 = x75};
--                     x1 <- _______rO;
--                     let {x76 = Cons x77 x1};
--                     let {x79 = x76};
--                     let {x0 = Cons x78 x79};
--                     return x0}]
--_______rO = msum [do {let {x80 = O};
--                      let {x82 = O};
--                      let {x83 = x80};
--                      x1 <- ________rO;
--                      let {x81 = Cons x82 x1};
--                      let {x84 = x81};
--                      let {x0 = Cons x83 x84};
--                      return x0}]
--________rO = msum [do {let {x85 = O};
--                       let {x87 = O};
--                       let {x88 = x85};
--                       x1 <- _________rO;
--                       let {x86 = Cons x87 x1};
--                       let {x89 = x86};
--                       let {x0 = Cons x88 x89};
--                       return x0}]
--_________rO = msum [do {let {x90 = O};
--                        let {x92 = O};
--                        let {x93 = Nil};
--                        let {x91 = Cons x92 x93};
--                        let {x94 = x90};
--                        let {x95 = x91};
--                        let {x0 = Cons x94 x95};
--                        return x0}]
--___neqRRO gen____________rO_x144 gen______rO_x72 = msum [do {x0 <- _______________rRO gen____________rO_x144 gen______rO_x72;
--                                                             return x0}]
--_______________rRO gen____________rO_x144 gen______rO_x72 = msum [do {x0 <- ________________rRO gen____________rO_x144 gen______rO_x72;
--                                                                      return x0}]
--________________rRO gen____________rO_x144 gen______rO_x72 = msum [do {x0 <- ___________rO gen____________rO_x144 gen______rO_x72;
--                                                                       return x0}]
--___________rO gen____________rO_x144 gen______rO_x72 = msum [do {let {x136 = O};
--                                                                 let {x135 = S x136};
--                                                                 let {x139 = O};
--                                                                 let {x138 = S x139};
--                                                                 let {x140 = x135};
--                                                                 x1 <- ____________rO gen______rO_x72;
--                                                                 let {x137 = Cons x138 x1};
--                                                                 let {x141 = x137};
--                                                                 let {x0 = Cons x140 x141};
--                                                                 return x0},
--                                                             do {let {x143 = O};
--                                                                 let {x142 = S x143};
--                                                                 let {x145 = x142};
--                                                                 (x146,
--                                                                  x144) <- do {x144 <- gen____________rO_x144;
--                                                                               return (x144, x144)};
--                                                                 let {x0 = Cons x145 x146};
--                                                                 (x2, x3) <- case x144 of
--                                                                             {Cons y2
--                                                                                   y3 -> return (y2,
--                                                                                                 y3);
--                                                                              _ -> mzero};
--                                                                 _____neqRII x2 x3;
--                                                                 return x0}]
--____________rO gen______rO_x72 = msum [do {let {x147 = O};
--                                           let {x149 = O};
--                                           let {x150 = x147};
--                                           x1 <- _____________rO gen______rO_x72;
--                                           let {x148 = Cons x149 x1};
--                                           let {x151 = x148};
--                                           let {x0 = Cons x150 x151};
--                                           return x0}]
--___rO gen______rO_x72 gen_____rO_x57 = msum [do {let {x33 = O};
--                                                 let {x35 = O};
--                                                 let {x36 = x33};
--                                                 x1 <- ____rO gen______rO_x72 gen_____rO_x57;
--                                                 let {x34 = Cons x35 x1};
--                                                 let {x37 = x34};
--                                                 let {x0 = Cons x36 x37};
--                                                 return x0},
--                                             do {let {x38 = O};
--                                                 let {x41 = x38};
--                                                 x43 <- ____rO gen______rO_x72 gen_____rO_x57;
--                                                 (x44, x3) <- case x43 of
--                                                              {Cons y44 y3 -> return (y44, y3);
--                                                               _ -> mzero};
--                                                 x2 <- case x44 of
--                                                       {S y2 -> return y2; _ -> mzero};
--                                                 let {x40 = S x2};
--                                                 let {x39 = Cons x40 x3};
--                                                 let {x42 = x39};
--                                                 let {x0 = Cons x41 x42};
--                                                 return x0}]
--____rO gen______rO_x72 gen_____rO_x57 = msum [do {let {x47 = O};
--                                                  let {x46 = S x47};
--                                                  let {x45 = S x46};
--                                                  let {x51 = O};
--                                                  let {x50 = S x51};
--                                                  let {x49 = S x50};
--                                                  let {x52 = x45};
--                                                  x1 <- _____rO gen______rO_x72;
--                                                  let {x48 = Cons x49 x1};
--                                                  let {x53 = x48};
--                                                  let {x0 = Cons x52 x53};
--                                                  return x0},
--                                              do {let {x56 = O};
--                                                  let {x55 = S x56};
--                                                  let {x54 = S x55};
--                                                  let {x58 = x54};
--                                                  (x59, x57) <- do {x57 <- gen_____rO_x57;
--                                                                    return (x57, x57)};
--                                                  let {x0 = Cons x58 x59};
--                                                  (x2, x3) <- case x57 of
--                                                              {Cons y2 y3 -> return (y2, y3);
--                                                               _ -> mzero};
--                                                  _neqRII x2 x3;
--                                                  return x0}]
--_rO gen______rO_x72 gen_____rO_x57 gen__rO_x25 = msum [do {let {x17 = O};
--                                                           let {x16 = S x17};
--                                                           let {x20 = O};
--                                                           let {x19 = S x20};
--                                                           let {x21 = x16};
--                                                           x1 <- __rO gen______rO_x72 gen_____rO_x57;
--                                                           let {x18 = Cons x19 x1};
--                                                           let {x22 = x18};
--                                                           let {x0 = Cons x21 x22};
--                                                           return x0},
--                                                       do {let {x24 = O};
--                                                           let {x23 = S x24};
--                                                           let {x26 = x23};
--                                                           (x27, x25) <- do {x25 <- gen__rO_x25;
--                                                                             return (x25, x25)};
--                                                           let {x0 = Cons x26 x27};
--                                                           (x2, x3) <- case x25 of
--                                                                       {Cons y2 y3 -> return (y2,
--                                                                                              y3);
--                                                                        _ -> mzero};
--                                                           ___neqRII x2 x3;
--                                                           return x0}]
--__rO gen______rO_x72 gen_____rO_x57 = msum [do {let {x28 = O};
--                                                let {x30 = O};
--                                                let {x31 = x28};
--                                                x1 <- ___rO gen______rO_x72 gen_____rO_x57;
--                                                let {x29 = Cons x30 x1};
--                                                let {x32 = x29};
--                                                let {x0 = Cons x31 x32};
--                                                return x0}]
--rRO gen__________________rO_x237 gen_________________rO_x225 gen________________rO_x199 gen_______________rO_x187 gen____________rO_x144 gen___________rO_x132 gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13 = msum [do {x0 <- _rRO gen____________rO_x144 gen___________rO_x132 gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13;
--                                                                                                                                                                                                                                 return x0},
--                                                                                                                                                                                                                             do {x0 <- ______rRO gen__________________rO_x237 gen_________________rO_x225 gen________________rO_x199 gen_______________rO_x187 gen______rO_x72 gen_____rO_x57;
--                                                                                                                                                                                                                                 return x0}]
--______rRO gen__________________rO_x237 gen_________________rO_x225 gen________________rO_x199 gen_______________rO_x187 gen______rO_x72 gen_____rO_x57 = msum [do {x0 <- _______rRO gen________________rO_x199 gen_______________rO_x187 gen______rO_x72 gen_____rO_x57;
--                                                                                                                                                                   return x0},
--                                                                                                                                                               do {x0 <- _neqRRO gen__________________rO_x237 gen_________________rO_x225 gen______rO_x72;
--                                                                                                                                                                   return x0}]
--_______rRO gen________________rO_x199 gen_______________rO_x187 gen______rO_x72 gen_____rO_x57 = msum [do {x0 <- ________rRO gen________________rO_x199 gen_______________rO_x187 gen______rO_x72 gen_____rO_x57;
--                                                                                                           return x0}]
--________rRO gen________________rO_x199 gen_______________rO_x187 gen______rO_x72 gen_____rO_x57 = msum [do {x0 <- ______________rO gen________________rO_x199 gen_______________rO_x187 gen______rO_x72 gen_____rO_x57;
--                                                                                                            return x0}]
--______________rO gen________________rO_x199 gen_______________rO_x187 gen______rO_x72 gen_____rO_x57 = msum [do {let {x179 = O};
--                                                                                                                 let {x178 = S x179};
--                                                                                                                 let {x182 = O};
--                                                                                                                 let {x181 = S x182};
--                                                                                                                 let {x183 = x178};
--                                                                                                                 x1 <- _______________rO gen________________rO_x199 gen______rO_x72 gen_____rO_x57;
--                                                                                                                 let {x180 = Cons x181 x1};
--                                                                                                                 let {x184 = x180};
--                                                                                                                 let {x0 = Cons x183 x184};
--                                                                                                                 return x0},
--                                                                                                             do {let {x186 = O};
--                                                                                                                 let {x185 = S x186};
--                                                                                                                 let {x188 = x185};
--                                                                                                                 (x189,
--                                                                                                                  x187) <- do {x187 <- gen_______________rO_x187;
--                                                                                                                               return (x187,
--                                                                                                                                       x187)};
--                                                                                                                 let {x0 = Cons x188 x189};
--                                                                                                                 (x2,
--                                                                                                                  x3) <- case x187 of
--                                                                                                                         {Cons y2
--                                                                                                                               y3 -> return (y2,
--                                                                                                                                             y3);
--                                                                                                                          _ -> mzero};
--                                                                                                                 ________neqRII x2 x3;
--                                                                                                                 return x0}]
--_neqRRO gen__________________rO_x237 gen_________________rO_x225 gen______rO_x72 = msum [do {x0 <- _________rRO gen__________________rO_x237 gen_________________rO_x225 gen______rO_x72;
--                                                                                             return x0}]
--_________rRO gen__________________rO_x237 gen_________________rO_x225 gen______rO_x72 = msum [do {x0 <- __________rRO gen__________________rO_x237 gen_________________rO_x225 gen______rO_x72;
--                                                                                                  return x0}]
--__________rRO gen__________________rO_x237 gen_________________rO_x225 gen______rO_x72 = msum [do {x0 <- ________________rO gen__________________rO_x237 gen_________________rO_x225 gen______rO_x72;
--                                                                                                   return x0}]
--________________rO gen__________________rO_x237 gen_________________rO_x225 gen______rO_x72 = msum [do {let {x217 = O};
--                                                                                                        let {x216 = S x217};
--                                                                                                        let {x220 = O};
--                                                                                                        let {x219 = S x220};
--                                                                                                        let {x221 = x216};
--                                                                                                        x1 <- _________________rO gen__________________rO_x237 gen______rO_x72;
--                                                                                                        let {x218 = Cons x219 x1};
--                                                                                                        let {x222 = x218};
--                                                                                                        let {x0 = Cons x221 x222};
--                                                                                                        return x0},
--                                                                                                    do {let {x224 = O};
--                                                                                                        let {x223 = S x224};
--                                                                                                        let {x226 = x223};
--                                                                                                        (x227,
--                                                                                                         x225) <- do {x225 <- gen_________________rO_x225;
--                                                                                                                      return (x225,
--                                                                                                                              x225)};
--                                                                                                        let {x0 = Cons x226 x227};
--                                                                                                        (x2,
--                                                                                                         x3) <- case x225 of
--                                                                                                                {Cons y2
--                                                                                                                      y3 -> return (y2,
--                                                                                                                                    y3);
--                                                                                                                 _ -> mzero};
--                                                                                                        __________neqRII x2 x3;
--                                                                                                        return x0}]
--_rRO gen____________rO_x144 gen___________rO_x132 gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13 = msum [do {x0 <- __rRO gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13;
--                                                                                                                    return x0},
--                                                                                                                do {x0 <- neqRRO gen____________rO_x144 gen___________rO_x132 gen______rO_x72;
--                                                                                                                    return x0}]
--__rRO gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13 = msum [do {x0 <- ___rRO gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13;
--                                                                        return x0}]
--___rRO gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13 = msum [do {x0 <- rO gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13;
--                                                                         return x0}]
--neqRRO gen____________rO_x144 gen___________rO_x132 gen______rO_x72 = msum [do {x0 <- ____rRO gen____________rO_x144 gen___________rO_x132 gen______rO_x72;
--                                                                                return x0}]
--____rRO gen____________rO_x144 gen___________rO_x132 gen______rO_x72 = msum [do {x0 <- _____rRO gen____________rO_x144 gen___________rO_x132 gen______rO_x72;
--                                                                                 return x0}]
--_____rRO gen____________rO_x144 gen___________rO_x132 gen______rO_x72 = msum [do {x0 <- __________rO gen____________rO_x144 gen___________rO_x132 gen______rO_x72;
--                                                                                  return x0}]
--__________rO gen____________rO_x144 gen___________rO_x132 gen______rO_x72 = msum [do {let {x124 = O};
--                                                                                      let {x123 = S x124};
--                                                                                      let {x127 = O};
--                                                                                      let {x126 = S x127};
--                                                                                      let {x128 = x123};
--                                                                                      x1 <- ___________rO gen____________rO_x144 gen______rO_x72;
--                                                                                      let {x125 = Cons x126 x1};
--                                                                                      let {x129 = x125};
--                                                                                      let {x0 = Cons x128 x129};
--                                                                                      return x0},
--                                                                                  do {let {x131 = O};
--                                                                                      let {x130 = S x131};
--                                                                                      let {x133 = x130};
--                                                                                      (x134,
--                                                                                       x132) <- do {x132 <- gen___________rO_x132;
--                                                                                                    return (x132,
--                                                                                                            x132)};
--                                                                                      let {x0 = Cons x133 x134};
--                                                                                      (x2,
--                                                                                       x3) <- case x132 of
--                                                                                              {Cons y2
--                                                                                                    y3 -> return (y2,
--                                                                                                                  y3);
--                                                                                               _ -> mzero};
--                                                                                      ______neqRII x2 x3;
--                                                                                      return x0}]
--rO gen______rO_x72 gen_____rO_x57 gen__rO_x25 gen_rO_x13 = msum [do {let {x5 = O};
--                                                                     let {x4 = S x5};
--                                                                     let {x8 = O};
--                                                                     let {x7 = S x8};
--                                                                     let {x9 = x4};
--                                                                     x1 <- _rO gen______rO_x72 gen_____rO_x57 gen__rO_x25;
--                                                                     let {x6 = Cons x7 x1};
--                                                                     let {x10 = x6};
--                                                                     let {x0 = Cons x9 x10};
--                                                                     return x0},
--                                                                 do {let {x12 = O};
--                                                                     let {x11 = S x12};
--                                                                     let {x14 = x11};
--                                                                     (x15,
--                                                                      x13) <- do {x13 <- gen_rO_x13;
--                                                                                  return (x13,
--                                                                                          x13)};
--                                                                     let {x0 = Cons x14 x15};
--                                                                     (x2, x3) <- case x13 of
--                                                                                 {Cons y2
--                                                                                       y3 -> return (y2,
--                                                                                                     y3);
--                                                                                  _ -> mzero};
--                                                                     ____neqRII x2 x3;
--                                                                     return x0}]