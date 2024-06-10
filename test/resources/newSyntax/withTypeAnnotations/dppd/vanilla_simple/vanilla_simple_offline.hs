module Vanilla_clean1 where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
solvesIII x0 x1 x2 = msum [do {let {x6 = O};
                               let {x5 = S x6};
                               let {x4 = S x5};
                               let {x9 = O};
                               let {x8 = S x9};
                               let {x13 = O};
                               let {x12 = S x13};
                               let {x11 = S x12};
                               let {x16 = O};
                               let {x15 = S x16};
                               let {x17 = Nil};
                               let {x14 = Cons x15 x17};
                               let {x10 = Cons x11 x14};
                               let {x7 = Cons x8 x10};
                               (x18, x19) <- case x2 of
                                             {Cons y18 y19 -> return (y18, y19); _ -> mzero};
                               guard (x18 == x4);
                               guard (x19 == x7);
                               guard (x1 == Nil);
                               guard (x0 == Nil);
                               return ()},
                           do {let {x21 = O};
                               let {x20 = S x21};
                               let {x25 = O};
                               let {x24 = S x25};
                               let {x23 = S x24};
                               let {x28 = O};
                               let {x27 = S x28};
                               let {x29 = Nil};
                               let {x26 = Cons x27 x29};
                               let {x22 = Cons x23 x26};
                               let {x34 = O};
                               let {x33 = S x34};
                               let {x32 = S x33};
                               let {x35 = Nil};
                               (x30, x31) <- case x2 of
                                             {Cons y30 y31 -> return (y30, y31); _ -> mzero};
                               guard (x30 == x20);
                               guard (x31 == x22);
                               (x36, x37) <- case x1 of
                                             {Cons y36 y37 -> return (y36, y37); _ -> mzero};
                               guard (x36 == x32);
                               guard (x37 == x35);
                               guard (x0 == Nil);
                               return ()},
                           do {let {x40 = O};
                               let {x39 = S x40};
                               let {x38 = S x39};
                               let {x43 = O};
                               let {x42 = S x43};
                               let {x44 = Nil};
                               let {x41 = Cons x42 x44};
                               let {x49 = O};
                               let {x48 = S x49};
                               let {x47 = S x48};
                               let {x52 = O};
                               let {x51 = S x52};
                               let {x53 = Nil};
                               let {x50 = Cons x51 x53};
                               (x45, x46) <- case x2 of
                                             {Cons y45 y46 -> return (y45, y46); _ -> mzero};
                               guard (x45 == x38);
                               guard (x46 == x41);
                               (x54, x55) <- case x1 of
                                             {Cons y54 y55 -> return (y54, y55); _ -> mzero};
                               guard (x54 == x47);
                               guard (x55 == x50);
                               guard (x0 == Nil);
                               return ()},
                           do {let {x57 = O};
                               let {x56 = S x57};
                               let {x58 = Nil};
                               let {x63 = O};
                               let {x62 = S x63};
                               let {x61 = S x62};
                               let {x66 = O};
                               let {x65 = S x66};
                               let {x70 = O};
                               let {x69 = S x70};
                               let {x68 = S x69};
                               let {x71 = Nil};
                               let {x67 = Cons x68 x71};
                               let {x64 = Cons x65 x67};
                               (x59, x60) <- case x2 of
                                             {Cons y59 y60 -> return (y59, y60); _ -> mzero};
                               guard (x59 == x56);
                               guard (x60 == x58);
                               (x72, x73) <- case x1 of
                                             {Cons y72 y73 -> return (y72, y73); _ -> mzero};
                               guard (x72 == x61);
                               guard (x73 == x64);
                               guard (x0 == Nil);
                               return ()},
                           do {let {x76 = O};
                               let {x75 = S x76};
                               let {x74 = S x75};
                               let {x79 = O};
                               let {x78 = S x79};
                               let {x83 = O};
                               let {x82 = S x83};
                               let {x81 = S x82};
                               let {x86 = O};
                               let {x85 = S x86};
                               let {x87 = Nil};
                               let {x84 = Cons x85 x87};
                               let {x80 = Cons x81 x84};
                               let {x77 = Cons x78 x80};
                               guard (x2 == Nil);
                               (x88, x89) <- case x1 of
                                             {Cons y88 y89 -> return (y88, y89); _ -> mzero};
                               guard (x88 == x74);
                               guard (x89 == x77);
                               guard (x0 == Nil);
                               return ()},
                           do {let {x91 = O};
                               let {x90 = S x91};
                               let {x95 = O};
                               let {x94 = S x95};
                               let {x93 = S x94};
                               let {x98 = O};
                               let {x97 = S x98};
                               let {x99 = Nil};
                               let {x96 = Cons x97 x99};
                               let {x92 = Cons x93 x96};
                               let {x104 = O};
                               let {x103 = S x104};
                               let {x102 = S x103};
                               let {x105 = Nil};
                               (x100, x101) <- case x2 of
                                               {Cons y100 y101 -> return (y100, y101); _ -> mzero};
                               guard (x100 == x90);
                               guard (x101 == x92);
                               guard (x1 == Nil);
                               (x106, x107) <- case x0 of
                                               {Cons y106 y107 -> return (y106, y107); _ -> mzero};
                               guard (x106 == x102);
                               guard (x107 == x105);
                               return ()},
                           do {let {x110 = O};
                               let {x109 = S x110};
                               let {x108 = S x109};
                               let {x113 = O};
                               let {x112 = S x113};
                               let {x114 = Nil};
                               let {x111 = Cons x112 x114};
                               let {x118 = O};
                               let {x117 = S x118};
                               let {x119 = Nil};
                               let {x124 = O};
                               let {x123 = S x124};
                               let {x122 = S x123};
                               let {x125 = Nil};
                               (x115, x116) <- case x2 of
                                               {Cons y115 y116 -> return (y115, y116); _ -> mzero};
                               guard (x115 == x108);
                               guard (x116 == x111);
                               (x120, x121) <- case x1 of
                                               {Cons y120 y121 -> return (y120, y121); _ -> mzero};
                               guard (x120 == x117);
                               guard (x121 == x119);
                               (x126, x127) <- case x0 of
                                               {Cons y126 y127 -> return (y126, y127); _ -> mzero};
                               guard (x126 == x122);
                               guard (x127 == x125);
                               return ()},
                           do {let {x129 = O};
                               let {x128 = S x129};
                               let {x130 = Nil};
                               let {x134 = O};
                               let {x133 = S x134};
                               let {x138 = O};
                               let {x137 = S x138};
                               let {x136 = S x137};
                               let {x139 = Nil};
                               let {x135 = Cons x136 x139};
                               let {x144 = O};
                               let {x143 = S x144};
                               let {x142 = S x143};
                               let {x145 = Nil};
                               (x131, x132) <- case x2 of
                                               {Cons y131 y132 -> return (y131, y132); _ -> mzero};
                               guard (x131 == x128);
                               guard (x132 == x130);
                               (x140, x141) <- case x1 of
                                               {Cons y140 y141 -> return (y140, y141); _ -> mzero};
                               guard (x140 == x133);
                               guard (x141 == x135);
                               (x146, x147) <- case x0 of
                                               {Cons y146 y147 -> return (y146, y147); _ -> mzero};
                               guard (x146 == x142);
                               guard (x147 == x145);
                               return ()},
                           do {let {x149 = O};
                               let {x148 = S x149};
                               let {x153 = O};
                               let {x152 = S x153};
                               let {x151 = S x152};
                               let {x156 = O};
                               let {x155 = S x156};
                               let {x157 = Nil};
                               let {x154 = Cons x155 x157};
                               let {x150 = Cons x151 x154};
                               let {x162 = O};
                               let {x161 = S x162};
                               let {x160 = S x161};
                               let {x163 = Nil};
                               guard (x2 == Nil);
                               (x158, x159) <- case x1 of
                                               {Cons y158 y159 -> return (y158, y159); _ -> mzero};
                               guard (x158 == x148);
                               guard (x159 == x150);
                               (x164, x165) <- case x0 of
                                               {Cons y164 y165 -> return (y164, y165); _ -> mzero};
                               guard (x164 == x160);
                               guard (x165 == x163);
                               return ()},
                           do {let {x168 = O};
                               let {x167 = S x168};
                               let {x166 = S x167};
                               let {x171 = O};
                               let {x170 = S x171};
                               let {x172 = Nil};
                               let {x169 = Cons x170 x172};
                               let {x177 = O};
                               let {x176 = S x177};
                               let {x175 = S x176};
                               let {x180 = O};
                               let {x179 = S x180};
                               (x173, x174) <- case x2 of
                                               {Cons y173 y174 -> return (y173, y174); _ -> mzero};
                               guard (x173 == x166);
                               guard (x174 == x169);
                               (x181, x182) <- case x0 of
                                               {Cons y181 y182 -> return (y181, y182); _ -> mzero};
                               guard (x181 == x175);
                               let {x178 = x182};
                               x3 <- case x178 of
                                     {Cons y179 y3 -> do {guard (x179 == y179); return y3};
                                      _ -> mzero};
                               _solvesII x1 x3;
                               return ()},
                           do {let {x184 = O};
                               let {x183 = S x184};
                               let {x185 = Nil};
                               let {x190 = O};
                               let {x189 = S x190};
                               let {x188 = S x189};
                               let {x193 = O};
                               let {x192 = S x193};
                               (x186, x187) <- case x2 of
                                               {Cons y186 y187 -> return (y186, y187); _ -> mzero};
                               guard (x186 == x183);
                               guard (x187 == x185);
                               (x194, x195) <- case x0 of
                                               {Cons y194 y195 -> return (y194, y195); _ -> mzero};
                               guard (x194 == x188);
                               let {x191 = x195};
                               x3 <- case x191 of
                                     {Cons y192 y3 -> do {guard (x192 == y192); return y3};
                                      _ -> mzero};
                               __solvesII x1 x3;
                               return ()},
                           do {let {x198 = O};
                               let {x197 = S x198};
                               let {x196 = S x197};
                               let {x201 = O};
                               let {x200 = S x201};
                               guard (x2 == Nil);
                               (x202, x203) <- case x0 of
                                               {Cons y202 y203 -> return (y202, y203); _ -> mzero};
                               guard (x202 == x196);
                               let {x199 = x203};
                               x3 <- case x199 of
                                     {Cons y200 y3 -> do {guard (x200 == y200); return y3};
                                      _ -> mzero};
                               ___solvesII x1 x3;
                               return ()}]
___solvesII x0 x1 = msum [do {let {x218 = O};
                              let {x217 = S x218};
                              let {x216 = S x217};
                              let {x221 = O};
                              let {x220 = S x221};
                              let {x222 = Nil};
                              let {x219 = Cons x220 x222};
                              guard (x1 == Nil);
                              (x223, x224) <- case x0 of
                                              {Cons y223 y224 -> return (y223, y224); _ -> mzero};
                              guard (x223 == x216);
                              guard (x224 == x219);
                              return ()},
                          do {let {x227 = O};
                              let {x226 = S x227};
                              let {x225 = S x226};
                              let {x228 = Nil};
                              let {x232 = O};
                              let {x231 = S x232};
                              let {x233 = Nil};
                              (x229, x230) <- case x1 of
                                              {Cons y229 y230 -> return (y229, y230); _ -> mzero};
                              guard (x229 == x225);
                              guard (x230 == x228);
                              (x234, x235) <- case x0 of
                                              {Cons y234 y235 -> return (y234, y235); _ -> mzero};
                              guard (x234 == x231);
                              guard (x235 == x233);
                              return ()},
                          do {let {x238 = O};
                              let {x237 = S x238};
                              let {x236 = S x237};
                              let {x241 = O};
                              let {x240 = S x241};
                              let {x242 = Nil};
                              let {x239 = Cons x240 x242};
                              (x243, x244) <- case x1 of
                                              {Cons y243 y244 -> return (y243, y244); _ -> mzero};
                              guard (x243 == x236);
                              guard (x244 == x239);
                              guard (x0 == Nil);
                              return ()}]
__solvesII x0 x1 = msum [do {let {x206 = O};
                             let {x205 = S x206};
                             let {x204 = S x205};
                             let {x207 = Nil};
                             guard (x1 == Nil);
                             (x208, x209) <- case x0 of
                                             {Cons y208 y209 -> return (y208, y209); _ -> mzero};
                             guard (x208 == x204);
                             guard (x209 == x207);
                             return ()},
                         do {let {x212 = O};
                             let {x211 = S x212};
                             let {x210 = S x211};
                             let {x213 = Nil};
                             (x214, x215) <- case x1 of
                                             {Cons y214 y215 -> return (y214, y215); _ -> mzero};
                             guard (x214 == x210);
                             guard (x215 == x213);
                             guard (x0 == Nil);
                             return ()}]
_solvesII x0 x1 = msum [do {guard (x1 == Nil);
                            guard (x0 == Nil);
                            return ()}]
solvesIIO x0 x1 = msum [do {let {x6 = O};
                            let {x5 = S x6};
                            let {x4 = S x5};
                            let {x9 = O};
                            let {x8 = S x9};
                            let {x13 = O};
                            let {x12 = S x13};
                            let {x11 = S x12};
                            let {x16 = O};
                            let {x15 = S x16};
                            let {x17 = Nil};
                            let {x14 = Cons x15 x17};
                            let {x10 = Cons x11 x14};
                            let {x7 = Cons x8 x10};
                            guard (x1 == Nil);
                            guard (x0 == Nil);
                            let {x18 = x4};
                            let {x19 = x7};
                            let {x2 = Cons x18 x19};
                            return x2},
                        do {let {x21 = O};
                            let {x20 = S x21};
                            let {x25 = O};
                            let {x24 = S x25};
                            let {x23 = S x24};
                            let {x28 = O};
                            let {x27 = S x28};
                            let {x29 = Nil};
                            let {x26 = Cons x27 x29};
                            let {x22 = Cons x23 x26};
                            let {x34 = O};
                            let {x33 = S x34};
                            let {x32 = S x33};
                            let {x35 = Nil};
                            (x36, x37) <- case x1 of
                                          {Cons y36 y37 -> return (y36, y37); _ -> mzero};
                            guard (x36 == x32);
                            guard (x37 == x35);
                            guard (x0 == Nil);
                            let {x30 = x20};
                            let {x31 = x22};
                            let {x2 = Cons x30 x31};
                            return x2},
                        do {let {x40 = O};
                            let {x39 = S x40};
                            let {x38 = S x39};
                            let {x43 = O};
                            let {x42 = S x43};
                            let {x44 = Nil};
                            let {x41 = Cons x42 x44};
                            let {x49 = O};
                            let {x48 = S x49};
                            let {x47 = S x48};
                            let {x52 = O};
                            let {x51 = S x52};
                            let {x53 = Nil};
                            let {x50 = Cons x51 x53};
                            (x54, x55) <- case x1 of
                                          {Cons y54 y55 -> return (y54, y55); _ -> mzero};
                            guard (x54 == x47);
                            guard (x55 == x50);
                            guard (x0 == Nil);
                            let {x45 = x38};
                            let {x46 = x41};
                            let {x2 = Cons x45 x46};
                            return x2},
                        do {let {x57 = O};
                            let {x56 = S x57};
                            let {x58 = Nil};
                            let {x63 = O};
                            let {x62 = S x63};
                            let {x61 = S x62};
                            let {x66 = O};
                            let {x65 = S x66};
                            let {x70 = O};
                            let {x69 = S x70};
                            let {x68 = S x69};
                            let {x71 = Nil};
                            let {x67 = Cons x68 x71};
                            let {x64 = Cons x65 x67};
                            (x72, x73) <- case x1 of
                                          {Cons y72 y73 -> return (y72, y73); _ -> mzero};
                            guard (x72 == x61);
                            guard (x73 == x64);
                            guard (x0 == Nil);
                            let {x59 = x56};
                            let {x60 = x58};
                            let {x2 = Cons x59 x60};
                            return x2},
                        do {let {x2 = Nil};
                            let {x76 = O};
                            let {x75 = S x76};
                            let {x74 = S x75};
                            let {x79 = O};
                            let {x78 = S x79};
                            let {x83 = O};
                            let {x82 = S x83};
                            let {x81 = S x82};
                            let {x86 = O};
                            let {x85 = S x86};
                            let {x87 = Nil};
                            let {x84 = Cons x85 x87};
                            let {x80 = Cons x81 x84};
                            let {x77 = Cons x78 x80};
                            (x88, x89) <- case x1 of
                                          {Cons y88 y89 -> return (y88, y89); _ -> mzero};
                            guard (x88 == x74);
                            guard (x89 == x77);
                            guard (x0 == Nil);
                            return x2},
                        do {let {x91 = O};
                            let {x90 = S x91};
                            let {x95 = O};
                            let {x94 = S x95};
                            let {x93 = S x94};
                            let {x98 = O};
                            let {x97 = S x98};
                            let {x99 = Nil};
                            let {x96 = Cons x97 x99};
                            let {x92 = Cons x93 x96};
                            let {x104 = O};
                            let {x103 = S x104};
                            let {x102 = S x103};
                            let {x105 = Nil};
                            guard (x1 == Nil);
                            (x106, x107) <- case x0 of
                                            {Cons y106 y107 -> return (y106, y107); _ -> mzero};
                            guard (x106 == x102);
                            guard (x107 == x105);
                            let {x100 = x90};
                            let {x101 = x92};
                            let {x2 = Cons x100 x101};
                            return x2},
                        do {let {x110 = O};
                            let {x109 = S x110};
                            let {x108 = S x109};
                            let {x113 = O};
                            let {x112 = S x113};
                            let {x114 = Nil};
                            let {x111 = Cons x112 x114};
                            let {x118 = O};
                            let {x117 = S x118};
                            let {x119 = Nil};
                            let {x124 = O};
                            let {x123 = S x124};
                            let {x122 = S x123};
                            let {x125 = Nil};
                            (x120, x121) <- case x1 of
                                            {Cons y120 y121 -> return (y120, y121); _ -> mzero};
                            guard (x120 == x117);
                            guard (x121 == x119);
                            (x126, x127) <- case x0 of
                                            {Cons y126 y127 -> return (y126, y127); _ -> mzero};
                            guard (x126 == x122);
                            guard (x127 == x125);
                            let {x115 = x108};
                            let {x116 = x111};
                            let {x2 = Cons x115 x116};
                            return x2},
                        do {let {x129 = O};
                            let {x128 = S x129};
                            let {x130 = Nil};
                            let {x134 = O};
                            let {x133 = S x134};
                            let {x138 = O};
                            let {x137 = S x138};
                            let {x136 = S x137};
                            let {x139 = Nil};
                            let {x135 = Cons x136 x139};
                            let {x144 = O};
                            let {x143 = S x144};
                            let {x142 = S x143};
                            let {x145 = Nil};
                            (x140, x141) <- case x1 of
                                            {Cons y140 y141 -> return (y140, y141); _ -> mzero};
                            guard (x140 == x133);
                            guard (x141 == x135);
                            (x146, x147) <- case x0 of
                                            {Cons y146 y147 -> return (y146, y147); _ -> mzero};
                            guard (x146 == x142);
                            guard (x147 == x145);
                            let {x131 = x128};
                            let {x132 = x130};
                            let {x2 = Cons x131 x132};
                            return x2},
                        do {let {x2 = Nil};
                            let {x149 = O};
                            let {x148 = S x149};
                            let {x153 = O};
                            let {x152 = S x153};
                            let {x151 = S x152};
                            let {x156 = O};
                            let {x155 = S x156};
                            let {x157 = Nil};
                            let {x154 = Cons x155 x157};
                            let {x150 = Cons x151 x154};
                            let {x162 = O};
                            let {x161 = S x162};
                            let {x160 = S x161};
                            let {x163 = Nil};
                            (x158, x159) <- case x1 of
                                            {Cons y158 y159 -> return (y158, y159); _ -> mzero};
                            guard (x158 == x148);
                            guard (x159 == x150);
                            (x164, x165) <- case x0 of
                                            {Cons y164 y165 -> return (y164, y165); _ -> mzero};
                            guard (x164 == x160);
                            guard (x165 == x163);
                            return x2},
                        do {let {x168 = O};
                            let {x167 = S x168};
                            let {x166 = S x167};
                            let {x171 = O};
                            let {x170 = S x171};
                            let {x172 = Nil};
                            let {x169 = Cons x170 x172};
                            let {x177 = O};
                            let {x176 = S x177};
                            let {x175 = S x176};
                            let {x180 = O};
                            let {x179 = S x180};
                            (x181, x182) <- case x0 of
                                            {Cons y181 y182 -> return (y181, y182); _ -> mzero};
                            guard (x181 == x175);
                            let {x173 = x166};
                            let {x174 = x169};
                            let {x2 = Cons x173 x174};
                            let {x178 = x182};
                            x3 <- case x178 of
                                  {Cons y179 y3 -> do {guard (x179 == y179); return y3};
                                   _ -> mzero};
                            _solvesII x1 x3;
                            return x2},
                        do {let {x184 = O};
                            let {x183 = S x184};
                            let {x185 = Nil};
                            let {x190 = O};
                            let {x189 = S x190};
                            let {x188 = S x189};
                            let {x193 = O};
                            let {x192 = S x193};
                            (x194, x195) <- case x0 of
                                            {Cons y194 y195 -> return (y194, y195); _ -> mzero};
                            guard (x194 == x188);
                            let {x186 = x183};
                            let {x187 = x185};
                            let {x2 = Cons x186 x187};
                            let {x191 = x195};
                            x3 <- case x191 of
                                  {Cons y192 y3 -> do {guard (x192 == y192); return y3};
                                   _ -> mzero};
                            __solvesII x1 x3;
                            return x2},
                        do {let {x2 = Nil};
                            let {x198 = O};
                            let {x197 = S x198};
                            let {x196 = S x197};
                            let {x201 = O};
                            let {x200 = S x201};
                            (x202, x203) <- case x0 of
                                            {Cons y202 y203 -> return (y202, y203); _ -> mzero};
                            guard (x202 == x196);
                            let {x199 = x203};
                            x3 <- case x199 of
                                  {Cons y200 y3 -> do {guard (x200 == y200); return y3};
                                   _ -> mzero};
                            ___solvesII x1 x3;
                            return x2}]
solvesIOI x0 x2 = msum [do {let {x6 = O};
                            let {x5 = S x6};
                            let {x4 = S x5};
                            let {x9 = O};
                            let {x8 = S x9};
                            let {x13 = O};
                            let {x12 = S x13};
                            let {x11 = S x12};
                            let {x16 = O};
                            let {x15 = S x16};
                            let {x17 = Nil};
                            let {x14 = Cons x15 x17};
                            let {x10 = Cons x11 x14};
                            let {x7 = Cons x8 x10};
                            let {x1 = Nil};
                            (x18, x19) <- case x2 of
                                          {Cons y18 y19 -> return (y18, y19); _ -> mzero};
                            guard (x18 == x4);
                            guard (x19 == x7);
                            guard (x0 == Nil);
                            return x1},
                        do {let {x21 = O};
                            let {x20 = S x21};
                            let {x25 = O};
                            let {x24 = S x25};
                            let {x23 = S x24};
                            let {x28 = O};
                            let {x27 = S x28};
                            let {x29 = Nil};
                            let {x26 = Cons x27 x29};
                            let {x22 = Cons x23 x26};
                            let {x34 = O};
                            let {x33 = S x34};
                            let {x32 = S x33};
                            let {x35 = Nil};
                            (x30, x31) <- case x2 of
                                          {Cons y30 y31 -> return (y30, y31); _ -> mzero};
                            guard (x30 == x20);
                            guard (x31 == x22);
                            guard (x0 == Nil);
                            let {x36 = x32};
                            let {x37 = x35};
                            let {x1 = Cons x36 x37};
                            return x1},
                        do {let {x40 = O};
                            let {x39 = S x40};
                            let {x38 = S x39};
                            let {x43 = O};
                            let {x42 = S x43};
                            let {x44 = Nil};
                            let {x41 = Cons x42 x44};
                            let {x49 = O};
                            let {x48 = S x49};
                            let {x47 = S x48};
                            let {x52 = O};
                            let {x51 = S x52};
                            let {x53 = Nil};
                            let {x50 = Cons x51 x53};
                            (x45, x46) <- case x2 of
                                          {Cons y45 y46 -> return (y45, y46); _ -> mzero};
                            guard (x45 == x38);
                            guard (x46 == x41);
                            guard (x0 == Nil);
                            let {x54 = x47};
                            let {x55 = x50};
                            let {x1 = Cons x54 x55};
                            return x1},
                        do {let {x57 = O};
                            let {x56 = S x57};
                            let {x58 = Nil};
                            let {x63 = O};
                            let {x62 = S x63};
                            let {x61 = S x62};
                            let {x66 = O};
                            let {x65 = S x66};
                            let {x70 = O};
                            let {x69 = S x70};
                            let {x68 = S x69};
                            let {x71 = Nil};
                            let {x67 = Cons x68 x71};
                            let {x64 = Cons x65 x67};
                            (x59, x60) <- case x2 of
                                          {Cons y59 y60 -> return (y59, y60); _ -> mzero};
                            guard (x59 == x56);
                            guard (x60 == x58);
                            guard (x0 == Nil);
                            let {x72 = x61};
                            let {x73 = x64};
                            let {x1 = Cons x72 x73};
                            return x1},
                        do {let {x76 = O};
                            let {x75 = S x76};
                            let {x74 = S x75};
                            let {x79 = O};
                            let {x78 = S x79};
                            let {x83 = O};
                            let {x82 = S x83};
                            let {x81 = S x82};
                            let {x86 = O};
                            let {x85 = S x86};
                            let {x87 = Nil};
                            let {x84 = Cons x85 x87};
                            let {x80 = Cons x81 x84};
                            let {x77 = Cons x78 x80};
                            guard (x2 == Nil);
                            guard (x0 == Nil);
                            let {x88 = x74};
                            let {x89 = x77};
                            let {x1 = Cons x88 x89};
                            return x1},
                        do {let {x91 = O};
                            let {x90 = S x91};
                            let {x95 = O};
                            let {x94 = S x95};
                            let {x93 = S x94};
                            let {x98 = O};
                            let {x97 = S x98};
                            let {x99 = Nil};
                            let {x96 = Cons x97 x99};
                            let {x92 = Cons x93 x96};
                            let {x1 = Nil};
                            let {x104 = O};
                            let {x103 = S x104};
                            let {x102 = S x103};
                            let {x105 = Nil};
                            (x100, x101) <- case x2 of
                                            {Cons y100 y101 -> return (y100, y101); _ -> mzero};
                            guard (x100 == x90);
                            guard (x101 == x92);
                            (x106, x107) <- case x0 of
                                            {Cons y106 y107 -> return (y106, y107); _ -> mzero};
                            guard (x106 == x102);
                            guard (x107 == x105);
                            return x1},
                        do {let {x110 = O};
                            let {x109 = S x110};
                            let {x108 = S x109};
                            let {x113 = O};
                            let {x112 = S x113};
                            let {x114 = Nil};
                            let {x111 = Cons x112 x114};
                            let {x118 = O};
                            let {x117 = S x118};
                            let {x119 = Nil};
                            let {x124 = O};
                            let {x123 = S x124};
                            let {x122 = S x123};
                            let {x125 = Nil};
                            (x115, x116) <- case x2 of
                                            {Cons y115 y116 -> return (y115, y116); _ -> mzero};
                            guard (x115 == x108);
                            guard (x116 == x111);
                            (x126, x127) <- case x0 of
                                            {Cons y126 y127 -> return (y126, y127); _ -> mzero};
                            guard (x126 == x122);
                            guard (x127 == x125);
                            let {x120 = x117};
                            let {x121 = x119};
                            let {x1 = Cons x120 x121};
                            return x1},
                        do {let {x129 = O};
                            let {x128 = S x129};
                            let {x130 = Nil};
                            let {x134 = O};
                            let {x133 = S x134};
                            let {x138 = O};
                            let {x137 = S x138};
                            let {x136 = S x137};
                            let {x139 = Nil};
                            let {x135 = Cons x136 x139};
                            let {x144 = O};
                            let {x143 = S x144};
                            let {x142 = S x143};
                            let {x145 = Nil};
                            (x131, x132) <- case x2 of
                                            {Cons y131 y132 -> return (y131, y132); _ -> mzero};
                            guard (x131 == x128);
                            guard (x132 == x130);
                            (x146, x147) <- case x0 of
                                            {Cons y146 y147 -> return (y146, y147); _ -> mzero};
                            guard (x146 == x142);
                            guard (x147 == x145);
                            let {x140 = x133};
                            let {x141 = x135};
                            let {x1 = Cons x140 x141};
                            return x1},
                        do {let {x149 = O};
                            let {x148 = S x149};
                            let {x153 = O};
                            let {x152 = S x153};
                            let {x151 = S x152};
                            let {x156 = O};
                            let {x155 = S x156};
                            let {x157 = Nil};
                            let {x154 = Cons x155 x157};
                            let {x150 = Cons x151 x154};
                            let {x162 = O};
                            let {x161 = S x162};
                            let {x160 = S x161};
                            let {x163 = Nil};
                            guard (x2 == Nil);
                            (x164, x165) <- case x0 of
                                            {Cons y164 y165 -> return (y164, y165); _ -> mzero};
                            guard (x164 == x160);
                            guard (x165 == x163);
                            let {x158 = x148};
                            let {x159 = x150};
                            let {x1 = Cons x158 x159};
                            return x1},
                        do {let {x168 = O};
                            let {x167 = S x168};
                            let {x166 = S x167};
                            let {x171 = O};
                            let {x170 = S x171};
                            let {x172 = Nil};
                            let {x169 = Cons x170 x172};
                            let {x177 = O};
                            let {x176 = S x177};
                            let {x175 = S x176};
                            let {x180 = O};
                            let {x179 = S x180};
                            (x173, x174) <- case x2 of
                                            {Cons y173 y174 -> return (y173, y174); _ -> mzero};
                            guard (x173 == x166);
                            guard (x174 == x169);
                            (x181, x182) <- case x0 of
                                            {Cons y181 y182 -> return (y181, y182); _ -> mzero};
                            guard (x181 == x175);
                            let {x178 = x182};
                            x3 <- case x178 of
                                  {Cons y179 y3 -> do {guard (x179 == y179); return y3};
                                   _ -> mzero};
                            x1 <- _solvesOI x3;
                            return x1},
                        do {let {x184 = O};
                            let {x183 = S x184};
                            let {x185 = Nil};
                            let {x190 = O};
                            let {x189 = S x190};
                            let {x188 = S x189};
                            let {x193 = O};
                            let {x192 = S x193};
                            (x186, x187) <- case x2 of
                                            {Cons y186 y187 -> return (y186, y187); _ -> mzero};
                            guard (x186 == x183);
                            guard (x187 == x185);
                            (x194, x195) <- case x0 of
                                            {Cons y194 y195 -> return (y194, y195); _ -> mzero};
                            guard (x194 == x188);
                            let {x191 = x195};
                            x3 <- case x191 of
                                  {Cons y192 y3 -> do {guard (x192 == y192); return y3};
                                   _ -> mzero};
                            x1 <- __solvesOI x3;
                            return x1},
                        do {let {x198 = O};
                            let {x197 = S x198};
                            let {x196 = S x197};
                            let {x201 = O};
                            let {x200 = S x201};
                            guard (x2 == Nil);
                            (x202, x203) <- case x0 of
                                            {Cons y202 y203 -> return (y202, y203); _ -> mzero};
                            guard (x202 == x196);
                            let {x199 = x203};
                            x3 <- case x199 of
                                  {Cons y200 y3 -> do {guard (x200 == y200); return y3};
                                   _ -> mzero};
                            x1 <- ___solvesOI x3;
                            return x1}]
___solvesOI x1 = msum [do {let {x218 = O};
                           let {x217 = S x218};
                           let {x216 = S x217};
                           let {x221 = O};
                           let {x220 = S x221};
                           let {x222 = Nil};
                           let {x219 = Cons x220 x222};
                           guard (x1 == Nil);
                           let {x223 = x216};
                           let {x224 = x219};
                           let {x0 = Cons x223 x224};
                           return x0},
                       do {let {x227 = O};
                           let {x226 = S x227};
                           let {x225 = S x226};
                           let {x228 = Nil};
                           let {x232 = O};
                           let {x231 = S x232};
                           let {x233 = Nil};
                           (x229, x230) <- case x1 of
                                           {Cons y229 y230 -> return (y229, y230); _ -> mzero};
                           guard (x229 == x225);
                           guard (x230 == x228);
                           let {x234 = x231};
                           let {x235 = x233};
                           let {x0 = Cons x234 x235};
                           return x0},
                       do {let {x238 = O};
                           let {x237 = S x238};
                           let {x236 = S x237};
                           let {x241 = O};
                           let {x240 = S x241};
                           let {x242 = Nil};
                           let {x239 = Cons x240 x242};
                           let {x0 = Nil};
                           (x243, x244) <- case x1 of
                                           {Cons y243 y244 -> return (y243, y244); _ -> mzero};
                           guard (x243 == x236);
                           guard (x244 == x239);
                           return x0}]
__solvesOI x1 = msum [do {let {x206 = O};
                          let {x205 = S x206};
                          let {x204 = S x205};
                          let {x207 = Nil};
                          guard (x1 == Nil);
                          let {x208 = x204};
                          let {x209 = x207};
                          let {x0 = Cons x208 x209};
                          return x0},
                      do {let {x212 = O};
                          let {x211 = S x212};
                          let {x210 = S x211};
                          let {x213 = Nil};
                          let {x0 = Nil};
                          (x214, x215) <- case x1 of
                                          {Cons y214 y215 -> return (y214, y215); _ -> mzero};
                          guard (x214 == x210);
                          guard (x215 == x213);
                          return x0}]
_solvesOI x1 = msum [do {let {x0 = Nil};
                         guard (x1 == Nil);
                         return x0}]
solvesIOO x0 = msum [do {let {x6 = O};
                         let {x5 = S x6};
                         let {x4 = S x5};
                         let {x9 = O};
                         let {x8 = S x9};
                         let {x13 = O};
                         let {x12 = S x13};
                         let {x11 = S x12};
                         let {x16 = O};
                         let {x15 = S x16};
                         let {x17 = Nil};
                         let {x14 = Cons x15 x17};
                         let {x10 = Cons x11 x14};
                         let {x7 = Cons x8 x10};
                         let {x1 = Nil};
                         guard (x0 == Nil);
                         let {x18 = x4};
                         let {x19 = x7};
                         let {x2 = Cons x18 x19};
                         return (x1, x2)},
                     do {let {x21 = O};
                         let {x20 = S x21};
                         let {x25 = O};
                         let {x24 = S x25};
                         let {x23 = S x24};
                         let {x28 = O};
                         let {x27 = S x28};
                         let {x29 = Nil};
                         let {x26 = Cons x27 x29};
                         let {x22 = Cons x23 x26};
                         let {x34 = O};
                         let {x33 = S x34};
                         let {x32 = S x33};
                         let {x35 = Nil};
                         guard (x0 == Nil);
                         let {x30 = x20};
                         let {x31 = x22};
                         let {x2 = Cons x30 x31};
                         let {x36 = x32};
                         let {x37 = x35};
                         let {x1 = Cons x36 x37};
                         return (x1, x2)},
                     do {let {x40 = O};
                         let {x39 = S x40};
                         let {x38 = S x39};
                         let {x43 = O};
                         let {x42 = S x43};
                         let {x44 = Nil};
                         let {x41 = Cons x42 x44};
                         let {x49 = O};
                         let {x48 = S x49};
                         let {x47 = S x48};
                         let {x52 = O};
                         let {x51 = S x52};
                         let {x53 = Nil};
                         let {x50 = Cons x51 x53};
                         guard (x0 == Nil);
                         let {x45 = x38};
                         let {x46 = x41};
                         let {x2 = Cons x45 x46};
                         let {x54 = x47};
                         let {x55 = x50};
                         let {x1 = Cons x54 x55};
                         return (x1, x2)},
                     do {let {x57 = O};
                         let {x56 = S x57};
                         let {x58 = Nil};
                         let {x63 = O};
                         let {x62 = S x63};
                         let {x61 = S x62};
                         let {x66 = O};
                         let {x65 = S x66};
                         let {x70 = O};
                         let {x69 = S x70};
                         let {x68 = S x69};
                         let {x71 = Nil};
                         let {x67 = Cons x68 x71};
                         let {x64 = Cons x65 x67};
                         guard (x0 == Nil);
                         let {x59 = x56};
                         let {x60 = x58};
                         let {x2 = Cons x59 x60};
                         let {x72 = x61};
                         let {x73 = x64};
                         let {x1 = Cons x72 x73};
                         return (x1, x2)},
                     do {let {x2 = Nil};
                         let {x76 = O};
                         let {x75 = S x76};
                         let {x74 = S x75};
                         let {x79 = O};
                         let {x78 = S x79};
                         let {x83 = O};
                         let {x82 = S x83};
                         let {x81 = S x82};
                         let {x86 = O};
                         let {x85 = S x86};
                         let {x87 = Nil};
                         let {x84 = Cons x85 x87};
                         let {x80 = Cons x81 x84};
                         let {x77 = Cons x78 x80};
                         guard (x0 == Nil);
                         let {x88 = x74};
                         let {x89 = x77};
                         let {x1 = Cons x88 x89};
                         return (x1, x2)},
                     do {let {x91 = O};
                         let {x90 = S x91};
                         let {x95 = O};
                         let {x94 = S x95};
                         let {x93 = S x94};
                         let {x98 = O};
                         let {x97 = S x98};
                         let {x99 = Nil};
                         let {x96 = Cons x97 x99};
                         let {x92 = Cons x93 x96};
                         let {x1 = Nil};
                         let {x104 = O};
                         let {x103 = S x104};
                         let {x102 = S x103};
                         let {x105 = Nil};
                         (x106, x107) <- case x0 of
                                         {Cons y106 y107 -> return (y106, y107); _ -> mzero};
                         guard (x106 == x102);
                         guard (x107 == x105);
                         let {x100 = x90};
                         let {x101 = x92};
                         let {x2 = Cons x100 x101};
                         return (x1, x2)},
                     do {let {x110 = O};
                         let {x109 = S x110};
                         let {x108 = S x109};
                         let {x113 = O};
                         let {x112 = S x113};
                         let {x114 = Nil};
                         let {x111 = Cons x112 x114};
                         let {x118 = O};
                         let {x117 = S x118};
                         let {x119 = Nil};
                         let {x124 = O};
                         let {x123 = S x124};
                         let {x122 = S x123};
                         let {x125 = Nil};
                         (x126, x127) <- case x0 of
                                         {Cons y126 y127 -> return (y126, y127); _ -> mzero};
                         guard (x126 == x122);
                         guard (x127 == x125);
                         let {x115 = x108};
                         let {x116 = x111};
                         let {x2 = Cons x115 x116};
                         let {x120 = x117};
                         let {x121 = x119};
                         let {x1 = Cons x120 x121};
                         return (x1, x2)},
                     do {let {x129 = O};
                         let {x128 = S x129};
                         let {x130 = Nil};
                         let {x134 = O};
                         let {x133 = S x134};
                         let {x138 = O};
                         let {x137 = S x138};
                         let {x136 = S x137};
                         let {x139 = Nil};
                         let {x135 = Cons x136 x139};
                         let {x144 = O};
                         let {x143 = S x144};
                         let {x142 = S x143};
                         let {x145 = Nil};
                         (x146, x147) <- case x0 of
                                         {Cons y146 y147 -> return (y146, y147); _ -> mzero};
                         guard (x146 == x142);
                         guard (x147 == x145);
                         let {x131 = x128};
                         let {x132 = x130};
                         let {x2 = Cons x131 x132};
                         let {x140 = x133};
                         let {x141 = x135};
                         let {x1 = Cons x140 x141};
                         return (x1, x2)},
                     do {let {x2 = Nil};
                         let {x149 = O};
                         let {x148 = S x149};
                         let {x153 = O};
                         let {x152 = S x153};
                         let {x151 = S x152};
                         let {x156 = O};
                         let {x155 = S x156};
                         let {x157 = Nil};
                         let {x154 = Cons x155 x157};
                         let {x150 = Cons x151 x154};
                         let {x162 = O};
                         let {x161 = S x162};
                         let {x160 = S x161};
                         let {x163 = Nil};
                         (x164, x165) <- case x0 of
                                         {Cons y164 y165 -> return (y164, y165); _ -> mzero};
                         guard (x164 == x160);
                         guard (x165 == x163);
                         let {x158 = x148};
                         let {x159 = x150};
                         let {x1 = Cons x158 x159};
                         return (x1, x2)},
                     do {let {x168 = O};
                         let {x167 = S x168};
                         let {x166 = S x167};
                         let {x171 = O};
                         let {x170 = S x171};
                         let {x172 = Nil};
                         let {x169 = Cons x170 x172};
                         let {x177 = O};
                         let {x176 = S x177};
                         let {x175 = S x176};
                         let {x180 = O};
                         let {x179 = S x180};
                         (x181, x182) <- case x0 of
                                         {Cons y181 y182 -> return (y181, y182); _ -> mzero};
                         guard (x181 == x175);
                         let {x173 = x166};
                         let {x174 = x169};
                         let {x2 = Cons x173 x174};
                         let {x178 = x182};
                         x3 <- case x178 of
                               {Cons y179 y3 -> do {guard (x179 == y179); return y3}; _ -> mzero};
                         x1 <- _solvesOI x3;
                         return (x1, x2)},
                     do {let {x184 = O};
                         let {x183 = S x184};
                         let {x185 = Nil};
                         let {x190 = O};
                         let {x189 = S x190};
                         let {x188 = S x189};
                         let {x193 = O};
                         let {x192 = S x193};
                         (x194, x195) <- case x0 of
                                         {Cons y194 y195 -> return (y194, y195); _ -> mzero};
                         guard (x194 == x188);
                         let {x186 = x183};
                         let {x187 = x185};
                         let {x2 = Cons x186 x187};
                         let {x191 = x195};
                         x3 <- case x191 of
                               {Cons y192 y3 -> do {guard (x192 == y192); return y3}; _ -> mzero};
                         x1 <- __solvesOI x3;
                         return (x1, x2)},
                     do {let {x2 = Nil};
                         let {x198 = O};
                         let {x197 = S x198};
                         let {x196 = S x197};
                         let {x201 = O};
                         let {x200 = S x201};
                         (x202, x203) <- case x0 of
                                         {Cons y202 y203 -> return (y202, y203); _ -> mzero};
                         guard (x202 == x196);
                         let {x199 = x203};
                         x3 <- case x199 of
                               {Cons y200 y3 -> do {guard (x200 == y200); return y3}; _ -> mzero};
                         x1 <- ___solvesOI x3;
                         return (x1, x2)}]
solvesOII x1 x2 = msum [do {let {x6 = O};
                            let {x5 = S x6};
                            let {x4 = S x5};
                            let {x9 = O};
                            let {x8 = S x9};
                            let {x13 = O};
                            let {x12 = S x13};
                            let {x11 = S x12};
                            let {x16 = O};
                            let {x15 = S x16};
                            let {x17 = Nil};
                            let {x14 = Cons x15 x17};
                            let {x10 = Cons x11 x14};
                            let {x7 = Cons x8 x10};
                            let {x0 = Nil};
                            (x18, x19) <- case x2 of
                                          {Cons y18 y19 -> return (y18, y19); _ -> mzero};
                            guard (x18 == x4);
                            guard (x19 == x7);
                            guard (x1 == Nil);
                            return x0},
                        do {let {x21 = O};
                            let {x20 = S x21};
                            let {x25 = O};
                            let {x24 = S x25};
                            let {x23 = S x24};
                            let {x28 = O};
                            let {x27 = S x28};
                            let {x29 = Nil};
                            let {x26 = Cons x27 x29};
                            let {x22 = Cons x23 x26};
                            let {x34 = O};
                            let {x33 = S x34};
                            let {x32 = S x33};
                            let {x35 = Nil};
                            let {x0 = Nil};
                            (x30, x31) <- case x2 of
                                          {Cons y30 y31 -> return (y30, y31); _ -> mzero};
                            guard (x30 == x20);
                            guard (x31 == x22);
                            (x36, x37) <- case x1 of
                                          {Cons y36 y37 -> return (y36, y37); _ -> mzero};
                            guard (x36 == x32);
                            guard (x37 == x35);
                            return x0},
                        do {let {x40 = O};
                            let {x39 = S x40};
                            let {x38 = S x39};
                            let {x43 = O};
                            let {x42 = S x43};
                            let {x44 = Nil};
                            let {x41 = Cons x42 x44};
                            let {x49 = O};
                            let {x48 = S x49};
                            let {x47 = S x48};
                            let {x52 = O};
                            let {x51 = S x52};
                            let {x53 = Nil};
                            let {x50 = Cons x51 x53};
                            let {x0 = Nil};
                            (x45, x46) <- case x2 of
                                          {Cons y45 y46 -> return (y45, y46); _ -> mzero};
                            guard (x45 == x38);
                            guard (x46 == x41);
                            (x54, x55) <- case x1 of
                                          {Cons y54 y55 -> return (y54, y55); _ -> mzero};
                            guard (x54 == x47);
                            guard (x55 == x50);
                            return x0},
                        do {let {x57 = O};
                            let {x56 = S x57};
                            let {x58 = Nil};
                            let {x63 = O};
                            let {x62 = S x63};
                            let {x61 = S x62};
                            let {x66 = O};
                            let {x65 = S x66};
                            let {x70 = O};
                            let {x69 = S x70};
                            let {x68 = S x69};
                            let {x71 = Nil};
                            let {x67 = Cons x68 x71};
                            let {x64 = Cons x65 x67};
                            let {x0 = Nil};
                            (x59, x60) <- case x2 of
                                          {Cons y59 y60 -> return (y59, y60); _ -> mzero};
                            guard (x59 == x56);
                            guard (x60 == x58);
                            (x72, x73) <- case x1 of
                                          {Cons y72 y73 -> return (y72, y73); _ -> mzero};
                            guard (x72 == x61);
                            guard (x73 == x64);
                            return x0},
                        do {let {x76 = O};
                            let {x75 = S x76};
                            let {x74 = S x75};
                            let {x79 = O};
                            let {x78 = S x79};
                            let {x83 = O};
                            let {x82 = S x83};
                            let {x81 = S x82};
                            let {x86 = O};
                            let {x85 = S x86};
                            let {x87 = Nil};
                            let {x84 = Cons x85 x87};
                            let {x80 = Cons x81 x84};
                            let {x77 = Cons x78 x80};
                            let {x0 = Nil};
                            guard (x2 == Nil);
                            (x88, x89) <- case x1 of
                                          {Cons y88 y89 -> return (y88, y89); _ -> mzero};
                            guard (x88 == x74);
                            guard (x89 == x77);
                            return x0},
                        do {let {x91 = O};
                            let {x90 = S x91};
                            let {x95 = O};
                            let {x94 = S x95};
                            let {x93 = S x94};
                            let {x98 = O};
                            let {x97 = S x98};
                            let {x99 = Nil};
                            let {x96 = Cons x97 x99};
                            let {x92 = Cons x93 x96};
                            let {x104 = O};
                            let {x103 = S x104};
                            let {x102 = S x103};
                            let {x105 = Nil};
                            (x100, x101) <- case x2 of
                                            {Cons y100 y101 -> return (y100, y101); _ -> mzero};
                            guard (x100 == x90);
                            guard (x101 == x92);
                            guard (x1 == Nil);
                            let {x106 = x102};
                            let {x107 = x105};
                            let {x0 = Cons x106 x107};
                            return x0},
                        do {let {x110 = O};
                            let {x109 = S x110};
                            let {x108 = S x109};
                            let {x113 = O};
                            let {x112 = S x113};
                            let {x114 = Nil};
                            let {x111 = Cons x112 x114};
                            let {x118 = O};
                            let {x117 = S x118};
                            let {x119 = Nil};
                            let {x124 = O};
                            let {x123 = S x124};
                            let {x122 = S x123};
                            let {x125 = Nil};
                            (x115, x116) <- case x2 of
                                            {Cons y115 y116 -> return (y115, y116); _ -> mzero};
                            guard (x115 == x108);
                            guard (x116 == x111);
                            (x120, x121) <- case x1 of
                                            {Cons y120 y121 -> return (y120, y121); _ -> mzero};
                            guard (x120 == x117);
                            guard (x121 == x119);
                            let {x126 = x122};
                            let {x127 = x125};
                            let {x0 = Cons x126 x127};
                            return x0},
                        do {let {x129 = O};
                            let {x128 = S x129};
                            let {x130 = Nil};
                            let {x134 = O};
                            let {x133 = S x134};
                            let {x138 = O};
                            let {x137 = S x138};
                            let {x136 = S x137};
                            let {x139 = Nil};
                            let {x135 = Cons x136 x139};
                            let {x144 = O};
                            let {x143 = S x144};
                            let {x142 = S x143};
                            let {x145 = Nil};
                            (x131, x132) <- case x2 of
                                            {Cons y131 y132 -> return (y131, y132); _ -> mzero};
                            guard (x131 == x128);
                            guard (x132 == x130);
                            (x140, x141) <- case x1 of
                                            {Cons y140 y141 -> return (y140, y141); _ -> mzero};
                            guard (x140 == x133);
                            guard (x141 == x135);
                            let {x146 = x142};
                            let {x147 = x145};
                            let {x0 = Cons x146 x147};
                            return x0},
                        do {let {x149 = O};
                            let {x148 = S x149};
                            let {x153 = O};
                            let {x152 = S x153};
                            let {x151 = S x152};
                            let {x156 = O};
                            let {x155 = S x156};
                            let {x157 = Nil};
                            let {x154 = Cons x155 x157};
                            let {x150 = Cons x151 x154};
                            let {x162 = O};
                            let {x161 = S x162};
                            let {x160 = S x161};
                            let {x163 = Nil};
                            guard (x2 == Nil);
                            (x158, x159) <- case x1 of
                                            {Cons y158 y159 -> return (y158, y159); _ -> mzero};
                            guard (x158 == x148);
                            guard (x159 == x150);
                            let {x164 = x160};
                            let {x165 = x163};
                            let {x0 = Cons x164 x165};
                            return x0},
                        do {let {x168 = O};
                            let {x167 = S x168};
                            let {x166 = S x167};
                            let {x171 = O};
                            let {x170 = S x171};
                            let {x172 = Nil};
                            let {x169 = Cons x170 x172};
                            let {x177 = O};
                            let {x176 = S x177};
                            let {x175 = S x176};
                            let {x180 = O};
                            let {x179 = S x180};
                            (x173, x174) <- case x2 of
                                            {Cons y173 y174 -> return (y173, y174); _ -> mzero};
                            guard (x173 == x166);
                            guard (x174 == x169);
                            let {x181 = x175};
                            x3 <- _solvesIO x1;
                            let {x178 = Cons x179 x3};
                            let {x182 = x178};
                            let {x0 = Cons x181 x182};
                            return x0},
                        do {let {x184 = O};
                            let {x183 = S x184};
                            let {x185 = Nil};
                            let {x190 = O};
                            let {x189 = S x190};
                            let {x188 = S x189};
                            let {x193 = O};
                            let {x192 = S x193};
                            (x186, x187) <- case x2 of
                                            {Cons y186 y187 -> return (y186, y187); _ -> mzero};
                            guard (x186 == x183);
                            guard (x187 == x185);
                            let {x194 = x188};
                            x3 <- __solvesIO x1;
                            let {x191 = Cons x192 x3};
                            let {x195 = x191};
                            let {x0 = Cons x194 x195};
                            return x0},
                        do {let {x198 = O};
                            let {x197 = S x198};
                            let {x196 = S x197};
                            let {x201 = O};
                            let {x200 = S x201};
                            guard (x2 == Nil);
                            let {x202 = x196};
                            x3 <- ___solvesIO x1;
                            let {x199 = Cons x200 x3};
                            let {x203 = x199};
                            let {x0 = Cons x202 x203};
                            return x0}]
___solvesIO x0 = msum [do {let {x1 = Nil};
                           let {x218 = O};
                           let {x217 = S x218};
                           let {x216 = S x217};
                           let {x221 = O};
                           let {x220 = S x221};
                           let {x222 = Nil};
                           let {x219 = Cons x220 x222};
                           (x223, x224) <- case x0 of
                                           {Cons y223 y224 -> return (y223, y224); _ -> mzero};
                           guard (x223 == x216);
                           guard (x224 == x219);
                           return x1},
                       do {let {x227 = O};
                           let {x226 = S x227};
                           let {x225 = S x226};
                           let {x228 = Nil};
                           let {x232 = O};
                           let {x231 = S x232};
                           let {x233 = Nil};
                           (x234, x235) <- case x0 of
                                           {Cons y234 y235 -> return (y234, y235); _ -> mzero};
                           guard (x234 == x231);
                           guard (x235 == x233);
                           let {x229 = x225};
                           let {x230 = x228};
                           let {x1 = Cons x229 x230};
                           return x1},
                       do {let {x238 = O};
                           let {x237 = S x238};
                           let {x236 = S x237};
                           let {x241 = O};
                           let {x240 = S x241};
                           let {x242 = Nil};
                           let {x239 = Cons x240 x242};
                           guard (x0 == Nil);
                           let {x243 = x236};
                           let {x244 = x239};
                           let {x1 = Cons x243 x244};
                           return x1}]
__solvesIO x0 = msum [do {let {x1 = Nil};
                          let {x206 = O};
                          let {x205 = S x206};
                          let {x204 = S x205};
                          let {x207 = Nil};
                          (x208, x209) <- case x0 of
                                          {Cons y208 y209 -> return (y208, y209); _ -> mzero};
                          guard (x208 == x204);
                          guard (x209 == x207);
                          return x1},
                      do {let {x212 = O};
                          let {x211 = S x212};
                          let {x210 = S x211};
                          let {x213 = Nil};
                          guard (x0 == Nil);
                          let {x214 = x210};
                          let {x215 = x213};
                          let {x1 = Cons x214 x215};
                          return x1}]
_solvesIO x0 = msum [do {let {x1 = Nil};
                         guard (x0 == Nil);
                         return x1}]
solvesOIO x1 = msum [do {let {x6 = O};
                         let {x5 = S x6};
                         let {x4 = S x5};
                         let {x9 = O};
                         let {x8 = S x9};
                         let {x13 = O};
                         let {x12 = S x13};
                         let {x11 = S x12};
                         let {x16 = O};
                         let {x15 = S x16};
                         let {x17 = Nil};
                         let {x14 = Cons x15 x17};
                         let {x10 = Cons x11 x14};
                         let {x7 = Cons x8 x10};
                         let {x0 = Nil};
                         guard (x1 == Nil);
                         let {x18 = x4};
                         let {x19 = x7};
                         let {x2 = Cons x18 x19};
                         return (x0, x2)},
                     do {let {x21 = O};
                         let {x20 = S x21};
                         let {x25 = O};
                         let {x24 = S x25};
                         let {x23 = S x24};
                         let {x28 = O};
                         let {x27 = S x28};
                         let {x29 = Nil};
                         let {x26 = Cons x27 x29};
                         let {x22 = Cons x23 x26};
                         let {x34 = O};
                         let {x33 = S x34};
                         let {x32 = S x33};
                         let {x35 = Nil};
                         let {x0 = Nil};
                         (x36, x37) <- case x1 of
                                       {Cons y36 y37 -> return (y36, y37); _ -> mzero};
                         guard (x36 == x32);
                         guard (x37 == x35);
                         let {x30 = x20};
                         let {x31 = x22};
                         let {x2 = Cons x30 x31};
                         return (x0, x2)},
                     do {let {x40 = O};
                         let {x39 = S x40};
                         let {x38 = S x39};
                         let {x43 = O};
                         let {x42 = S x43};
                         let {x44 = Nil};
                         let {x41 = Cons x42 x44};
                         let {x49 = O};
                         let {x48 = S x49};
                         let {x47 = S x48};
                         let {x52 = O};
                         let {x51 = S x52};
                         let {x53 = Nil};
                         let {x50 = Cons x51 x53};
                         let {x0 = Nil};
                         (x54, x55) <- case x1 of
                                       {Cons y54 y55 -> return (y54, y55); _ -> mzero};
                         guard (x54 == x47);
                         guard (x55 == x50);
                         let {x45 = x38};
                         let {x46 = x41};
                         let {x2 = Cons x45 x46};
                         return (x0, x2)},
                     do {let {x57 = O};
                         let {x56 = S x57};
                         let {x58 = Nil};
                         let {x63 = O};
                         let {x62 = S x63};
                         let {x61 = S x62};
                         let {x66 = O};
                         let {x65 = S x66};
                         let {x70 = O};
                         let {x69 = S x70};
                         let {x68 = S x69};
                         let {x71 = Nil};
                         let {x67 = Cons x68 x71};
                         let {x64 = Cons x65 x67};
                         let {x0 = Nil};
                         (x72, x73) <- case x1 of
                                       {Cons y72 y73 -> return (y72, y73); _ -> mzero};
                         guard (x72 == x61);
                         guard (x73 == x64);
                         let {x59 = x56};
                         let {x60 = x58};
                         let {x2 = Cons x59 x60};
                         return (x0, x2)},
                     do {let {x2 = Nil};
                         let {x76 = O};
                         let {x75 = S x76};
                         let {x74 = S x75};
                         let {x79 = O};
                         let {x78 = S x79};
                         let {x83 = O};
                         let {x82 = S x83};
                         let {x81 = S x82};
                         let {x86 = O};
                         let {x85 = S x86};
                         let {x87 = Nil};
                         let {x84 = Cons x85 x87};
                         let {x80 = Cons x81 x84};
                         let {x77 = Cons x78 x80};
                         let {x0 = Nil};
                         (x88, x89) <- case x1 of
                                       {Cons y88 y89 -> return (y88, y89); _ -> mzero};
                         guard (x88 == x74);
                         guard (x89 == x77);
                         return (x0, x2)},
                     do {let {x91 = O};
                         let {x90 = S x91};
                         let {x95 = O};
                         let {x94 = S x95};
                         let {x93 = S x94};
                         let {x98 = O};
                         let {x97 = S x98};
                         let {x99 = Nil};
                         let {x96 = Cons x97 x99};
                         let {x92 = Cons x93 x96};
                         let {x104 = O};
                         let {x103 = S x104};
                         let {x102 = S x103};
                         let {x105 = Nil};
                         guard (x1 == Nil);
                         let {x100 = x90};
                         let {x101 = x92};
                         let {x2 = Cons x100 x101};
                         let {x106 = x102};
                         let {x107 = x105};
                         let {x0 = Cons x106 x107};
                         return (x0, x2)},
                     do {let {x110 = O};
                         let {x109 = S x110};
                         let {x108 = S x109};
                         let {x113 = O};
                         let {x112 = S x113};
                         let {x114 = Nil};
                         let {x111 = Cons x112 x114};
                         let {x118 = O};
                         let {x117 = S x118};
                         let {x119 = Nil};
                         let {x124 = O};
                         let {x123 = S x124};
                         let {x122 = S x123};
                         let {x125 = Nil};
                         (x120, x121) <- case x1 of
                                         {Cons y120 y121 -> return (y120, y121); _ -> mzero};
                         guard (x120 == x117);
                         guard (x121 == x119);
                         let {x115 = x108};
                         let {x116 = x111};
                         let {x2 = Cons x115 x116};
                         let {x126 = x122};
                         let {x127 = x125};
                         let {x0 = Cons x126 x127};
                         return (x0, x2)},
                     do {let {x129 = O};
                         let {x128 = S x129};
                         let {x130 = Nil};
                         let {x134 = O};
                         let {x133 = S x134};
                         let {x138 = O};
                         let {x137 = S x138};
                         let {x136 = S x137};
                         let {x139 = Nil};
                         let {x135 = Cons x136 x139};
                         let {x144 = O};
                         let {x143 = S x144};
                         let {x142 = S x143};
                         let {x145 = Nil};
                         (x140, x141) <- case x1 of
                                         {Cons y140 y141 -> return (y140, y141); _ -> mzero};
                         guard (x140 == x133);
                         guard (x141 == x135);
                         let {x131 = x128};
                         let {x132 = x130};
                         let {x2 = Cons x131 x132};
                         let {x146 = x142};
                         let {x147 = x145};
                         let {x0 = Cons x146 x147};
                         return (x0, x2)},
                     do {let {x2 = Nil};
                         let {x149 = O};
                         let {x148 = S x149};
                         let {x153 = O};
                         let {x152 = S x153};
                         let {x151 = S x152};
                         let {x156 = O};
                         let {x155 = S x156};
                         let {x157 = Nil};
                         let {x154 = Cons x155 x157};
                         let {x150 = Cons x151 x154};
                         let {x162 = O};
                         let {x161 = S x162};
                         let {x160 = S x161};
                         let {x163 = Nil};
                         (x158, x159) <- case x1 of
                                         {Cons y158 y159 -> return (y158, y159); _ -> mzero};
                         guard (x158 == x148);
                         guard (x159 == x150);
                         let {x164 = x160};
                         let {x165 = x163};
                         let {x0 = Cons x164 x165};
                         return (x0, x2)},
                     do {let {x168 = O};
                         let {x167 = S x168};
                         let {x166 = S x167};
                         let {x171 = O};
                         let {x170 = S x171};
                         let {x172 = Nil};
                         let {x169 = Cons x170 x172};
                         let {x177 = O};
                         let {x176 = S x177};
                         let {x175 = S x176};
                         let {x180 = O};
                         let {x179 = S x180};
                         let {x173 = x166};
                         let {x174 = x169};
                         let {x2 = Cons x173 x174};
                         let {x181 = x175};
                         x3 <- _solvesIO x1;
                         let {x178 = Cons x179 x3};
                         let {x182 = x178};
                         let {x0 = Cons x181 x182};
                         return (x0, x2)},
                     do {let {x184 = O};
                         let {x183 = S x184};
                         let {x185 = Nil};
                         let {x190 = O};
                         let {x189 = S x190};
                         let {x188 = S x189};
                         let {x193 = O};
                         let {x192 = S x193};
                         let {x186 = x183};
                         let {x187 = x185};
                         let {x2 = Cons x186 x187};
                         let {x194 = x188};
                         x3 <- __solvesIO x1;
                         let {x191 = Cons x192 x3};
                         let {x195 = x191};
                         let {x0 = Cons x194 x195};
                         return (x0, x2)},
                     do {let {x2 = Nil};
                         let {x198 = O};
                         let {x197 = S x198};
                         let {x196 = S x197};
                         let {x201 = O};
                         let {x200 = S x201};
                         let {x202 = x196};
                         x3 <- ___solvesIO x1;
                         let {x199 = Cons x200 x3};
                         let {x203 = x199};
                         let {x0 = Cons x202 x203};
                         return (x0, x2)}]
solvesOOI x2 = msum [do {let {x6 = O};
                         let {x5 = S x6};
                         let {x4 = S x5};
                         let {x9 = O};
                         let {x8 = S x9};
                         let {x13 = O};
                         let {x12 = S x13};
                         let {x11 = S x12};
                         let {x16 = O};
                         let {x15 = S x16};
                         let {x17 = Nil};
                         let {x14 = Cons x15 x17};
                         let {x10 = Cons x11 x14};
                         let {x7 = Cons x8 x10};
                         let {x1 = Nil};
                         let {x0 = Nil};
                         (x18, x19) <- case x2 of
                                       {Cons y18 y19 -> return (y18, y19); _ -> mzero};
                         guard (x18 == x4);
                         guard (x19 == x7);
                         return (x0, x1)},
                     do {let {x21 = O};
                         let {x20 = S x21};
                         let {x25 = O};
                         let {x24 = S x25};
                         let {x23 = S x24};
                         let {x28 = O};
                         let {x27 = S x28};
                         let {x29 = Nil};
                         let {x26 = Cons x27 x29};
                         let {x22 = Cons x23 x26};
                         let {x34 = O};
                         let {x33 = S x34};
                         let {x32 = S x33};
                         let {x35 = Nil};
                         let {x0 = Nil};
                         (x30, x31) <- case x2 of
                                       {Cons y30 y31 -> return (y30, y31); _ -> mzero};
                         guard (x30 == x20);
                         guard (x31 == x22);
                         let {x36 = x32};
                         let {x37 = x35};
                         let {x1 = Cons x36 x37};
                         return (x0, x1)},
                     do {let {x40 = O};
                         let {x39 = S x40};
                         let {x38 = S x39};
                         let {x43 = O};
                         let {x42 = S x43};
                         let {x44 = Nil};
                         let {x41 = Cons x42 x44};
                         let {x49 = O};
                         let {x48 = S x49};
                         let {x47 = S x48};
                         let {x52 = O};
                         let {x51 = S x52};
                         let {x53 = Nil};
                         let {x50 = Cons x51 x53};
                         let {x0 = Nil};
                         (x45, x46) <- case x2 of
                                       {Cons y45 y46 -> return (y45, y46); _ -> mzero};
                         guard (x45 == x38);
                         guard (x46 == x41);
                         let {x54 = x47};
                         let {x55 = x50};
                         let {x1 = Cons x54 x55};
                         return (x0, x1)},
                     do {let {x57 = O};
                         let {x56 = S x57};
                         let {x58 = Nil};
                         let {x63 = O};
                         let {x62 = S x63};
                         let {x61 = S x62};
                         let {x66 = O};
                         let {x65 = S x66};
                         let {x70 = O};
                         let {x69 = S x70};
                         let {x68 = S x69};
                         let {x71 = Nil};
                         let {x67 = Cons x68 x71};
                         let {x64 = Cons x65 x67};
                         let {x0 = Nil};
                         (x59, x60) <- case x2 of
                                       {Cons y59 y60 -> return (y59, y60); _ -> mzero};
                         guard (x59 == x56);
                         guard (x60 == x58);
                         let {x72 = x61};
                         let {x73 = x64};
                         let {x1 = Cons x72 x73};
                         return (x0, x1)},
                     do {let {x76 = O};
                         let {x75 = S x76};
                         let {x74 = S x75};
                         let {x79 = O};
                         let {x78 = S x79};
                         let {x83 = O};
                         let {x82 = S x83};
                         let {x81 = S x82};
                         let {x86 = O};
                         let {x85 = S x86};
                         let {x87 = Nil};
                         let {x84 = Cons x85 x87};
                         let {x80 = Cons x81 x84};
                         let {x77 = Cons x78 x80};
                         let {x0 = Nil};
                         guard (x2 == Nil);
                         let {x88 = x74};
                         let {x89 = x77};
                         let {x1 = Cons x88 x89};
                         return (x0, x1)},
                     do {let {x91 = O};
                         let {x90 = S x91};
                         let {x95 = O};
                         let {x94 = S x95};
                         let {x93 = S x94};
                         let {x98 = O};
                         let {x97 = S x98};
                         let {x99 = Nil};
                         let {x96 = Cons x97 x99};
                         let {x92 = Cons x93 x96};
                         let {x1 = Nil};
                         let {x104 = O};
                         let {x103 = S x104};
                         let {x102 = S x103};
                         let {x105 = Nil};
                         (x100, x101) <- case x2 of
                                         {Cons y100 y101 -> return (y100, y101); _ -> mzero};
                         guard (x100 == x90);
                         guard (x101 == x92);
                         let {x106 = x102};
                         let {x107 = x105};
                         let {x0 = Cons x106 x107};
                         return (x0, x1)},
                     do {let {x110 = O};
                         let {x109 = S x110};
                         let {x108 = S x109};
                         let {x113 = O};
                         let {x112 = S x113};
                         let {x114 = Nil};
                         let {x111 = Cons x112 x114};
                         let {x118 = O};
                         let {x117 = S x118};
                         let {x119 = Nil};
                         let {x124 = O};
                         let {x123 = S x124};
                         let {x122 = S x123};
                         let {x125 = Nil};
                         (x115, x116) <- case x2 of
                                         {Cons y115 y116 -> return (y115, y116); _ -> mzero};
                         guard (x115 == x108);
                         guard (x116 == x111);
                         let {x120 = x117};
                         let {x121 = x119};
                         let {x1 = Cons x120 x121};
                         let {x126 = x122};
                         let {x127 = x125};
                         let {x0 = Cons x126 x127};
                         return (x0, x1)},
                     do {let {x129 = O};
                         let {x128 = S x129};
                         let {x130 = Nil};
                         let {x134 = O};
                         let {x133 = S x134};
                         let {x138 = O};
                         let {x137 = S x138};
                         let {x136 = S x137};
                         let {x139 = Nil};
                         let {x135 = Cons x136 x139};
                         let {x144 = O};
                         let {x143 = S x144};
                         let {x142 = S x143};
                         let {x145 = Nil};
                         (x131, x132) <- case x2 of
                                         {Cons y131 y132 -> return (y131, y132); _ -> mzero};
                         guard (x131 == x128);
                         guard (x132 == x130);
                         let {x140 = x133};
                         let {x141 = x135};
                         let {x1 = Cons x140 x141};
                         let {x146 = x142};
                         let {x147 = x145};
                         let {x0 = Cons x146 x147};
                         return (x0, x1)},
                     do {let {x149 = O};
                         let {x148 = S x149};
                         let {x153 = O};
                         let {x152 = S x153};
                         let {x151 = S x152};
                         let {x156 = O};
                         let {x155 = S x156};
                         let {x157 = Nil};
                         let {x154 = Cons x155 x157};
                         let {x150 = Cons x151 x154};
                         let {x162 = O};
                         let {x161 = S x162};
                         let {x160 = S x161};
                         let {x163 = Nil};
                         guard (x2 == Nil);
                         let {x158 = x148};
                         let {x159 = x150};
                         let {x1 = Cons x158 x159};
                         let {x164 = x160};
                         let {x165 = x163};
                         let {x0 = Cons x164 x165};
                         return (x0, x1)},
                     do {let {x168 = O};
                         let {x167 = S x168};
                         let {x166 = S x167};
                         let {x171 = O};
                         let {x170 = S x171};
                         let {x172 = Nil};
                         let {x169 = Cons x170 x172};
                         let {x177 = O};
                         let {x176 = S x177};
                         let {x175 = S x176};
                         let {x180 = O};
                         let {x179 = S x180};
                         (x173, x174) <- case x2 of
                                         {Cons y173 y174 -> return (y173, y174); _ -> mzero};
                         guard (x173 == x166);
                         guard (x174 == x169);
                         let {x181 = x175};
                         (x1, x3) <- _solvesOO;
                         let {x178 = Cons x179 x3};
                         let {x182 = x178};
                         let {x0 = Cons x181 x182};
                         return (x0, x1)},
                     do {let {x184 = O};
                         let {x183 = S x184};
                         let {x185 = Nil};
                         let {x190 = O};
                         let {x189 = S x190};
                         let {x188 = S x189};
                         let {x193 = O};
                         let {x192 = S x193};
                         (x186, x187) <- case x2 of
                                         {Cons y186 y187 -> return (y186, y187); _ -> mzero};
                         guard (x186 == x183);
                         guard (x187 == x185);
                         let {x194 = x188};
                         (x1, x3) <- __solvesOO;
                         let {x191 = Cons x192 x3};
                         let {x195 = x191};
                         let {x0 = Cons x194 x195};
                         return (x0, x1)},
                     do {let {x198 = O};
                         let {x197 = S x198};
                         let {x196 = S x197};
                         let {x201 = O};
                         let {x200 = S x201};
                         guard (x2 == Nil);
                         let {x202 = x196};
                         (x1, x3) <- ___solvesOO;
                         let {x199 = Cons x200 x3};
                         let {x203 = x199};
                         let {x0 = Cons x202 x203};
                         return (x0, x1)}]
___solvesOO = msum [do {let {x1 = Nil};
                        let {x218 = O};
                        let {x217 = S x218};
                        let {x216 = S x217};
                        let {x221 = O};
                        let {x220 = S x221};
                        let {x222 = Nil};
                        let {x219 = Cons x220 x222};
                        let {x223 = x216};
                        let {x224 = x219};
                        let {x0 = Cons x223 x224};
                        return (x0, x1)},
                    do {let {x227 = O};
                        let {x226 = S x227};
                        let {x225 = S x226};
                        let {x228 = Nil};
                        let {x232 = O};
                        let {x231 = S x232};
                        let {x233 = Nil};
                        let {x229 = x225};
                        let {x230 = x228};
                        let {x1 = Cons x229 x230};
                        let {x234 = x231};
                        let {x235 = x233};
                        let {x0 = Cons x234 x235};
                        return (x0, x1)},
                    do {let {x238 = O};
                        let {x237 = S x238};
                        let {x236 = S x237};
                        let {x241 = O};
                        let {x240 = S x241};
                        let {x242 = Nil};
                        let {x239 = Cons x240 x242};
                        let {x0 = Nil};
                        let {x243 = x236};
                        let {x244 = x239};
                        let {x1 = Cons x243 x244};
                        return (x0, x1)}]
__solvesOO = msum [do {let {x1 = Nil};
                       let {x206 = O};
                       let {x205 = S x206};
                       let {x204 = S x205};
                       let {x207 = Nil};
                       let {x208 = x204};
                       let {x209 = x207};
                       let {x0 = Cons x208 x209};
                       return (x0, x1)},
                   do {let {x212 = O};
                       let {x211 = S x212};
                       let {x210 = S x211};
                       let {x213 = Nil};
                       let {x0 = Nil};
                       let {x214 = x210};
                       let {x215 = x213};
                       let {x1 = Cons x214 x215};
                       return (x0, x1)}]
_solvesOO = msum [do {let {x1 = Nil};
                      let {x0 = Nil};
                      return (x0, x1)}]
solvesOOO = msum [do {let {x6 = O};
                      let {x5 = S x6};
                      let {x4 = S x5};
                      let {x9 = O};
                      let {x8 = S x9};
                      let {x13 = O};
                      let {x12 = S x13};
                      let {x11 = S x12};
                      let {x16 = O};
                      let {x15 = S x16};
                      let {x17 = Nil};
                      let {x14 = Cons x15 x17};
                      let {x10 = Cons x11 x14};
                      let {x7 = Cons x8 x10};
                      let {x1 = Nil};
                      let {x0 = Nil};
                      let {x18 = x4};
                      let {x19 = x7};
                      let {x2 = Cons x18 x19};
                      return (x0, x1, x2)},
                  do {let {x21 = O};
                      let {x20 = S x21};
                      let {x25 = O};
                      let {x24 = S x25};
                      let {x23 = S x24};
                      let {x28 = O};
                      let {x27 = S x28};
                      let {x29 = Nil};
                      let {x26 = Cons x27 x29};
                      let {x22 = Cons x23 x26};
                      let {x34 = O};
                      let {x33 = S x34};
                      let {x32 = S x33};
                      let {x35 = Nil};
                      let {x0 = Nil};
                      let {x30 = x20};
                      let {x31 = x22};
                      let {x2 = Cons x30 x31};
                      let {x36 = x32};
                      let {x37 = x35};
                      let {x1 = Cons x36 x37};
                      return (x0, x1, x2)},
                  do {let {x40 = O};
                      let {x39 = S x40};
                      let {x38 = S x39};
                      let {x43 = O};
                      let {x42 = S x43};
                      let {x44 = Nil};
                      let {x41 = Cons x42 x44};
                      let {x49 = O};
                      let {x48 = S x49};
                      let {x47 = S x48};
                      let {x52 = O};
                      let {x51 = S x52};
                      let {x53 = Nil};
                      let {x50 = Cons x51 x53};
                      let {x0 = Nil};
                      let {x45 = x38};
                      let {x46 = x41};
                      let {x2 = Cons x45 x46};
                      let {x54 = x47};
                      let {x55 = x50};
                      let {x1 = Cons x54 x55};
                      return (x0, x1, x2)},
                  do {let {x57 = O};
                      let {x56 = S x57};
                      let {x58 = Nil};
                      let {x63 = O};
                      let {x62 = S x63};
                      let {x61 = S x62};
                      let {x66 = O};
                      let {x65 = S x66};
                      let {x70 = O};
                      let {x69 = S x70};
                      let {x68 = S x69};
                      let {x71 = Nil};
                      let {x67 = Cons x68 x71};
                      let {x64 = Cons x65 x67};
                      let {x0 = Nil};
                      let {x59 = x56};
                      let {x60 = x58};
                      let {x2 = Cons x59 x60};
                      let {x72 = x61};
                      let {x73 = x64};
                      let {x1 = Cons x72 x73};
                      return (x0, x1, x2)},
                  do {let {x2 = Nil};
                      let {x76 = O};
                      let {x75 = S x76};
                      let {x74 = S x75};
                      let {x79 = O};
                      let {x78 = S x79};
                      let {x83 = O};
                      let {x82 = S x83};
                      let {x81 = S x82};
                      let {x86 = O};
                      let {x85 = S x86};
                      let {x87 = Nil};
                      let {x84 = Cons x85 x87};
                      let {x80 = Cons x81 x84};
                      let {x77 = Cons x78 x80};
                      let {x0 = Nil};
                      let {x88 = x74};
                      let {x89 = x77};
                      let {x1 = Cons x88 x89};
                      return (x0, x1, x2)},
                  do {let {x91 = O};
                      let {x90 = S x91};
                      let {x95 = O};
                      let {x94 = S x95};
                      let {x93 = S x94};
                      let {x98 = O};
                      let {x97 = S x98};
                      let {x99 = Nil};
                      let {x96 = Cons x97 x99};
                      let {x92 = Cons x93 x96};
                      let {x1 = Nil};
                      let {x104 = O};
                      let {x103 = S x104};
                      let {x102 = S x103};
                      let {x105 = Nil};
                      let {x100 = x90};
                      let {x101 = x92};
                      let {x2 = Cons x100 x101};
                      let {x106 = x102};
                      let {x107 = x105};
                      let {x0 = Cons x106 x107};
                      return (x0, x1, x2)},
                  do {let {x110 = O};
                      let {x109 = S x110};
                      let {x108 = S x109};
                      let {x113 = O};
                      let {x112 = S x113};
                      let {x114 = Nil};
                      let {x111 = Cons x112 x114};
                      let {x118 = O};
                      let {x117 = S x118};
                      let {x119 = Nil};
                      let {x124 = O};
                      let {x123 = S x124};
                      let {x122 = S x123};
                      let {x125 = Nil};
                      let {x115 = x108};
                      let {x116 = x111};
                      let {x2 = Cons x115 x116};
                      let {x120 = x117};
                      let {x121 = x119};
                      let {x1 = Cons x120 x121};
                      let {x126 = x122};
                      let {x127 = x125};
                      let {x0 = Cons x126 x127};
                      return (x0, x1, x2)},
                  do {let {x129 = O};
                      let {x128 = S x129};
                      let {x130 = Nil};
                      let {x134 = O};
                      let {x133 = S x134};
                      let {x138 = O};
                      let {x137 = S x138};
                      let {x136 = S x137};
                      let {x139 = Nil};
                      let {x135 = Cons x136 x139};
                      let {x144 = O};
                      let {x143 = S x144};
                      let {x142 = S x143};
                      let {x145 = Nil};
                      let {x131 = x128};
                      let {x132 = x130};
                      let {x2 = Cons x131 x132};
                      let {x140 = x133};
                      let {x141 = x135};
                      let {x1 = Cons x140 x141};
                      let {x146 = x142};
                      let {x147 = x145};
                      let {x0 = Cons x146 x147};
                      return (x0, x1, x2)},
                  do {let {x2 = Nil};
                      let {x149 = O};
                      let {x148 = S x149};
                      let {x153 = O};
                      let {x152 = S x153};
                      let {x151 = S x152};
                      let {x156 = O};
                      let {x155 = S x156};
                      let {x157 = Nil};
                      let {x154 = Cons x155 x157};
                      let {x150 = Cons x151 x154};
                      let {x162 = O};
                      let {x161 = S x162};
                      let {x160 = S x161};
                      let {x163 = Nil};
                      let {x158 = x148};
                      let {x159 = x150};
                      let {x1 = Cons x158 x159};
                      let {x164 = x160};
                      let {x165 = x163};
                      let {x0 = Cons x164 x165};
                      return (x0, x1, x2)},
                  do {let {x168 = O};
                      let {x167 = S x168};
                      let {x166 = S x167};
                      let {x171 = O};
                      let {x170 = S x171};
                      let {x172 = Nil};
                      let {x169 = Cons x170 x172};
                      let {x177 = O};
                      let {x176 = S x177};
                      let {x175 = S x176};
                      let {x180 = O};
                      let {x179 = S x180};
                      let {x173 = x166};
                      let {x174 = x169};
                      let {x2 = Cons x173 x174};
                      let {x181 = x175};
                      (x1, x3) <- _solvesOO;
                      let {x178 = Cons x179 x3};
                      let {x182 = x178};
                      let {x0 = Cons x181 x182};
                      return (x0, x1, x2)},
                  do {let {x184 = O};
                      let {x183 = S x184};
                      let {x185 = Nil};
                      let {x190 = O};
                      let {x189 = S x190};
                      let {x188 = S x189};
                      let {x193 = O};
                      let {x192 = S x193};
                      let {x186 = x183};
                      let {x187 = x185};
                      let {x2 = Cons x186 x187};
                      let {x194 = x188};
                      (x1, x3) <- __solvesOO;
                      let {x191 = Cons x192 x3};
                      let {x195 = x191};
                      let {x0 = Cons x194 x195};
                      return (x0, x1, x2)},
                  do {let {x2 = Nil};
                      let {x198 = O};
                      let {x197 = S x198};
                      let {x196 = S x197};
                      let {x201 = O};
                      let {x200 = S x201};
                      let {x202 = x196};
                      (x1, x3) <- ___solvesOO;
                      let {x199 = Cons x200 x3};
                      let {x203 = x199};
                      let {x0 = Cons x202 x203};
                      return (x0, x1, x2)}]