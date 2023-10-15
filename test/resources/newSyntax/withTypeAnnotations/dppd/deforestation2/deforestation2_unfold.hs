module Deforestation2_unfold where

import Stream
import Control.Monad

data Term
    = Cons Term Term
    | Nil
    | O
    | S Term
    deriving (Show, Eq)
rrI x0 = msum [do {(x1, x3) <- case x0 of
                               {Cons y1 y3 -> return (y1, y3); _ -> mzero};
                   x2 <- case x1 of
                         {S y2 -> return y2; _ -> mzero};
                   guard (x2 == O);
                   (x4, x6) <- case x3 of
                               {Cons y4 y6 -> return (y4, y6); _ -> mzero};
                   x5 <- case x4 of
                         {S y5 -> return y5; _ -> mzero};
                   guard (x5 == O);
                   (x7, x9) <- case x6 of
                               {Cons y7 y9 -> return (y7, y9); _ -> mzero};
                   x8 <- case x7 of
                         {S y8 -> return y8; _ -> mzero};
                   guard (x8 == O);
                   (x10, x12) <- case x9 of
                                 {Cons y10 y12 -> return (y10, y12); _ -> mzero};
                   x11 <- case x10 of
                          {S y11 -> return y11; _ -> mzero};
                   guard (x11 == O);
                   (x13, x14) <- case x12 of
                                 {Cons y13 y14 -> return (y13, y14); _ -> mzero};
                   guard (x13 == O);
                   (x15, x16) <- case x14 of
                                 {Cons y15 y16 -> return (y15, y16); _ -> mzero};
                   guard (x15 == O);
                   (x17, x18) <- case x16 of
                                 {Cons y17 y18 -> return (y17, y18); _ -> mzero};
                   guard (x17 == O);
                   (x19, x20) <- case x18 of
                                 {Cons y19 y20 -> return (y19, y20); _ -> mzero};
                   guard (x19 == O);
                   (x21, x24) <- case x20 of
                                 {Cons y21 y24 -> return (y21, y24); _ -> mzero};
                   x22 <- case x21 of
                          {S y22 -> return y22; _ -> mzero};
                   x23 <- case x22 of
                          {S y23 -> return y23; _ -> mzero};
                   guard (x23 == O);
                   (x25, x28) <- case x24 of
                                 {Cons y25 y28 -> return (y25, y28); _ -> mzero};
                   x26 <- case x25 of
                          {S y26 -> return y26; _ -> mzero};
                   x27 <- case x26 of
                          {S y27 -> return y27; _ -> mzero};
                   guard (x27 == O);
                   (x29, x32) <- case x28 of
                                 {Cons y29 y32 -> return (y29, y32); _ -> mzero};
                   x30 <- case x29 of
                          {S y30 -> return y30; _ -> mzero};
                   x31 <- case x30 of
                          {S y31 -> return y31; _ -> mzero};
                   guard (x31 == O);
                   (x33, x36) <- case x32 of
                                 {Cons y33 y36 -> return (y33, y36); _ -> mzero};
                   x34 <- case x33 of
                          {S y34 -> return y34; _ -> mzero};
                   x35 <- case x34 of
                          {S y35 -> return y35; _ -> mzero};
                   guard (x35 == O);
                   (x37, x38) <- case x36 of
                                 {Cons y37 y38 -> return (y37, y38); _ -> mzero};
                   guard (x37 == O);
                   (x39, x40) <- case x38 of
                                 {Cons y39 y40 -> return (y39, y40); _ -> mzero};
                   guard (x39 == O);
                   (x41, x42) <- case x40 of
                                 {Cons y41 y42 -> return (y41, y42); _ -> mzero};
                   guard (x41 == O);
                   (x43, x44) <- case x42 of
                                 {Cons y43 y44 -> return (y43, y44); _ -> mzero};
                   guard (x43 == O);
                   (x45, x46) <- case x44 of
                                 {Cons y45 y46 -> return (y45, y46); _ -> mzero};
                   guard (x45 == O);
                   (x47, x48) <- case x46 of
                                 {Cons y47 y48 -> return (y47, y48); _ -> mzero};
                   guard (x47 == O);
                   (x49, x50) <- case x48 of
                                 {Cons y49 y50 -> return (y49, y50); _ -> mzero};
                   guard (x49 == O);
                   (x51, x52) <- case x50 of
                                 {Cons y51 y52 -> return (y51, y52); _ -> mzero};
                   guard (x51 == O);
                   guard (x52 == Nil);
                   return ()},
               do {(x53, x55) <- case x0 of
                                 {Cons y53 y55 -> return (y53, y55); _ -> mzero};
                   x54 <- case x53 of
                          {S y54 -> return y54; _ -> mzero};
                   guard (x54 == O);
                   (x56, x58) <- case x55 of
                                 {Cons y56 y58 -> return (y56, y58); _ -> mzero};
                   x57 <- case x56 of
                          {S y57 -> return y57; _ -> mzero};
                   guard (x57 == O);
                   (x59, x61) <- case x58 of
                                 {Cons y59 y61 -> return (y59, y61); _ -> mzero};
                   x60 <- case x59 of
                          {S y60 -> return y60; _ -> mzero};
                   guard (x60 == O);
                   (x62, x63) <- case x61 of
                                 {Cons y62 y63 -> return (y62, y63); _ -> mzero};
                   guard (x62 == O);
                   (x64, x65) <- case x63 of
                                 {Cons y64 y65 -> return (y64, y65); _ -> mzero};
                   guard (x64 == O);
                   (x66, x67) <- case x65 of
                                 {Cons y66 y67 -> return (y66, y67); _ -> mzero};
                   guard (x66 == O);
                   (x68, x69) <- case x67 of
                                 {Cons y68 y69 -> return (y68, y69); _ -> mzero};
                   guard (x68 == O);
                   (x70, x73) <- case x69 of
                                 {Cons y70 y73 -> return (y70, y73); _ -> mzero};
                   x71 <- case x70 of
                          {S y71 -> return y71; _ -> mzero};
                   x72 <- case x71 of
                          {S y72 -> return y72; _ -> mzero};
                   guard (x72 == O);
                   (x74, x77) <- case x73 of
                                 {Cons y74 y77 -> return (y74, y77); _ -> mzero};
                   x75 <- case x74 of
                          {S y75 -> return y75; _ -> mzero};
                   x76 <- case x75 of
                          {S y76 -> return y76; _ -> mzero};
                   guard (x76 == O);
                   (x78, x81) <- case x77 of
                                 {Cons y78 y81 -> return (y78, y81); _ -> mzero};
                   x79 <- case x78 of
                          {S y79 -> return y79; _ -> mzero};
                   x80 <- case x79 of
                          {S y80 -> return y80; _ -> mzero};
                   guard (x80 == O);
                   (x82, x85) <- case x81 of
                                 {Cons y82 y85 -> return (y82, y85); _ -> mzero};
                   x83 <- case x82 of
                          {S y83 -> return y83; _ -> mzero};
                   x84 <- case x83 of
                          {S y84 -> return y84; _ -> mzero};
                   guard (x84 == O);
                   (x86, x87) <- case x85 of
                                 {Cons y86 y87 -> return (y86, y87); _ -> mzero};
                   guard (x86 == O);
                   (x88, x89) <- case x87 of
                                 {Cons y88 y89 -> return (y88, y89); _ -> mzero};
                   guard (x88 == O);
                   (x90, x91) <- case x89 of
                                 {Cons y90 y91 -> return (y90, y91); _ -> mzero};
                   guard (x90 == O);
                   (x92, x93) <- case x91 of
                                 {Cons y92 y93 -> return (y92, y93); _ -> mzero};
                   guard (x92 == O);
                   (x94, x95) <- case x93 of
                                 {Cons y94 y95 -> return (y94, y95); _ -> mzero};
                   guard (x94 == O);
                   (x96, x97) <- case x95 of
                                 {Cons y96 y97 -> return (y96, y97); _ -> mzero};
                   guard (x96 == O);
                   (x98, x99) <- case x97 of
                                 {Cons y98 y99 -> return (y98, y99); _ -> mzero};
                   guard (x98 == O);
                   (x100, x101) <- case x99 of
                                   {Cons y100 y101 -> return (y100, y101); _ -> mzero};
                   guard (x100 == O);
                   guard (x101 == Nil);
                   return ()},
               do {(x102, x104) <- case x0 of
                                   {Cons y102 y104 -> return (y102, y104); _ -> mzero};
                   x103 <- case x102 of
                           {S y103 -> return y103; _ -> mzero};
                   guard (x103 == O);
                   (x105, x107) <- case x104 of
                                   {Cons y105 y107 -> return (y105, y107); _ -> mzero};
                   x106 <- case x105 of
                           {S y106 -> return y106; _ -> mzero};
                   guard (x106 == O);
                   (x108, x109) <- case x107 of
                                   {Cons y108 y109 -> return (y108, y109); _ -> mzero};
                   guard (x108 == O);
                   (x110, x111) <- case x109 of
                                   {Cons y110 y111 -> return (y110, y111); _ -> mzero};
                   guard (x110 == O);
                   (x112, x113) <- case x111 of
                                   {Cons y112 y113 -> return (y112, y113); _ -> mzero};
                   guard (x112 == O);
                   (x114, x115) <- case x113 of
                                   {Cons y114 y115 -> return (y114, y115); _ -> mzero};
                   guard (x114 == O);
                   (x116, x119) <- case x115 of
                                   {Cons y116 y119 -> return (y116, y119); _ -> mzero};
                   x117 <- case x116 of
                           {S y117 -> return y117; _ -> mzero};
                   x118 <- case x117 of
                           {S y118 -> return y118; _ -> mzero};
                   guard (x118 == O);
                   (x120, x123) <- case x119 of
                                   {Cons y120 y123 -> return (y120, y123); _ -> mzero};
                   x121 <- case x120 of
                           {S y121 -> return y121; _ -> mzero};
                   x122 <- case x121 of
                           {S y122 -> return y122; _ -> mzero};
                   guard (x122 == O);
                   (x124, x127) <- case x123 of
                                   {Cons y124 y127 -> return (y124, y127); _ -> mzero};
                   x125 <- case x124 of
                           {S y125 -> return y125; _ -> mzero};
                   x126 <- case x125 of
                           {S y126 -> return y126; _ -> mzero};
                   guard (x126 == O);
                   (x128, x131) <- case x127 of
                                   {Cons y128 y131 -> return (y128, y131); _ -> mzero};
                   x129 <- case x128 of
                           {S y129 -> return y129; _ -> mzero};
                   x130 <- case x129 of
                           {S y130 -> return y130; _ -> mzero};
                   guard (x130 == O);
                   (x132, x133) <- case x131 of
                                   {Cons y132 y133 -> return (y132, y133); _ -> mzero};
                   guard (x132 == O);
                   (x134, x135) <- case x133 of
                                   {Cons y134 y135 -> return (y134, y135); _ -> mzero};
                   guard (x134 == O);
                   (x136, x137) <- case x135 of
                                   {Cons y136 y137 -> return (y136, y137); _ -> mzero};
                   guard (x136 == O);
                   (x138, x139) <- case x137 of
                                   {Cons y138 y139 -> return (y138, y139); _ -> mzero};
                   guard (x138 == O);
                   (x140, x141) <- case x139 of
                                   {Cons y140 y141 -> return (y140, y141); _ -> mzero};
                   guard (x140 == O);
                   (x142, x143) <- case x141 of
                                   {Cons y142 y143 -> return (y142, y143); _ -> mzero};
                   guard (x142 == O);
                   (x144, x145) <- case x143 of
                                   {Cons y144 y145 -> return (y144, y145); _ -> mzero};
                   guard (x144 == O);
                   (x146, x147) <- case x145 of
                                   {Cons y146 y147 -> return (y146, y147); _ -> mzero};
                   guard (x146 == O);
                   guard (x147 == Nil);
                   return ()},
               do {(x148, x150) <- case x0 of
                                   {Cons y148 y150 -> return (y148, y150); _ -> mzero};
                   x149 <- case x148 of
                           {S y149 -> return y149; _ -> mzero};
                   guard (x149 == O);
                   (x151, x152) <- case x150 of
                                   {Cons y151 y152 -> return (y151, y152); _ -> mzero};
                   guard (x151 == O);
                   (x153, x154) <- case x152 of
                                   {Cons y153 y154 -> return (y153, y154); _ -> mzero};
                   guard (x153 == O);
                   (x155, x156) <- case x154 of
                                   {Cons y155 y156 -> return (y155, y156); _ -> mzero};
                   guard (x155 == O);
                   (x157, x158) <- case x156 of
                                   {Cons y157 y158 -> return (y157, y158); _ -> mzero};
                   guard (x157 == O);
                   (x159, x162) <- case x158 of
                                   {Cons y159 y162 -> return (y159, y162); _ -> mzero};
                   x160 <- case x159 of
                           {S y160 -> return y160; _ -> mzero};
                   x161 <- case x160 of
                           {S y161 -> return y161; _ -> mzero};
                   guard (x161 == O);
                   (x163, x166) <- case x162 of
                                   {Cons y163 y166 -> return (y163, y166); _ -> mzero};
                   x164 <- case x163 of
                           {S y164 -> return y164; _ -> mzero};
                   x165 <- case x164 of
                           {S y165 -> return y165; _ -> mzero};
                   guard (x165 == O);
                   (x167, x170) <- case x166 of
                                   {Cons y167 y170 -> return (y167, y170); _ -> mzero};
                   x168 <- case x167 of
                           {S y168 -> return y168; _ -> mzero};
                   x169 <- case x168 of
                           {S y169 -> return y169; _ -> mzero};
                   guard (x169 == O);
                   (x171, x174) <- case x170 of
                                   {Cons y171 y174 -> return (y171, y174); _ -> mzero};
                   x172 <- case x171 of
                           {S y172 -> return y172; _ -> mzero};
                   x173 <- case x172 of
                           {S y173 -> return y173; _ -> mzero};
                   guard (x173 == O);
                   (x175, x176) <- case x174 of
                                   {Cons y175 y176 -> return (y175, y176); _ -> mzero};
                   guard (x175 == O);
                   (x177, x178) <- case x176 of
                                   {Cons y177 y178 -> return (y177, y178); _ -> mzero};
                   guard (x177 == O);
                   (x179, x180) <- case x178 of
                                   {Cons y179 y180 -> return (y179, y180); _ -> mzero};
                   guard (x179 == O);
                   (x181, x182) <- case x180 of
                                   {Cons y181 y182 -> return (y181, y182); _ -> mzero};
                   guard (x181 == O);
                   (x183, x184) <- case x182 of
                                   {Cons y183 y184 -> return (y183, y184); _ -> mzero};
                   guard (x183 == O);
                   (x185, x186) <- case x184 of
                                   {Cons y185 y186 -> return (y185, y186); _ -> mzero};
                   guard (x185 == O);
                   (x187, x188) <- case x186 of
                                   {Cons y187 y188 -> return (y187, y188); _ -> mzero};
                   guard (x187 == O);
                   (x189, x190) <- case x188 of
                                   {Cons y189 y190 -> return (y189, y190); _ -> mzero};
                   guard (x189 == O);
                   guard (x190 == Nil);
                   return ()}]
rrO = msum [do {let {x2 = O};
                let {x1 = S x2};
                let {x5 = O};
                let {x4 = S x5};
                let {x8 = O};
                let {x7 = S x8};
                let {x11 = O};
                let {x10 = S x11};
                let {x13 = O};
                let {x15 = O};
                let {x17 = O};
                let {x19 = O};
                let {x23 = O};
                let {x22 = S x23};
                let {x21 = S x22};
                let {x27 = O};
                let {x26 = S x27};
                let {x25 = S x26};
                let {x31 = O};
                let {x30 = S x31};
                let {x29 = S x30};
                let {x35 = O};
                let {x34 = S x35};
                let {x33 = S x34};
                let {x37 = O};
                let {x39 = O};
                let {x41 = O};
                let {x43 = O};
                let {x45 = O};
                let {x47 = O};
                let {x49 = O};
                let {x51 = O};
                let {x52 = Nil};
                let {x50 = Cons x51 x52};
                let {x48 = Cons x49 x50};
                let {x46 = Cons x47 x48};
                let {x44 = Cons x45 x46};
                let {x42 = Cons x43 x44};
                let {x40 = Cons x41 x42};
                let {x38 = Cons x39 x40};
                let {x36 = Cons x37 x38};
                let {x32 = Cons x33 x36};
                let {x28 = Cons x29 x32};
                let {x24 = Cons x25 x28};
                let {x20 = Cons x21 x24};
                let {x18 = Cons x19 x20};
                let {x16 = Cons x17 x18};
                let {x14 = Cons x15 x16};
                let {x12 = Cons x13 x14};
                let {x9 = Cons x10 x12};
                let {x6 = Cons x7 x9};
                let {x3 = Cons x4 x6};
                let {x0 = Cons x1 x3};
                return x0},
            do {let {x54 = O};
                let {x53 = S x54};
                let {x57 = O};
                let {x56 = S x57};
                let {x60 = O};
                let {x59 = S x60};
                let {x62 = O};
                let {x64 = O};
                let {x66 = O};
                let {x68 = O};
                let {x72 = O};
                let {x71 = S x72};
                let {x70 = S x71};
                let {x76 = O};
                let {x75 = S x76};
                let {x74 = S x75};
                let {x80 = O};
                let {x79 = S x80};
                let {x78 = S x79};
                let {x84 = O};
                let {x83 = S x84};
                let {x82 = S x83};
                let {x86 = O};
                let {x88 = O};
                let {x90 = O};
                let {x92 = O};
                let {x94 = O};
                let {x96 = O};
                let {x98 = O};
                let {x100 = O};
                let {x101 = Nil};
                let {x99 = Cons x100 x101};
                let {x97 = Cons x98 x99};
                let {x95 = Cons x96 x97};
                let {x93 = Cons x94 x95};
                let {x91 = Cons x92 x93};
                let {x89 = Cons x90 x91};
                let {x87 = Cons x88 x89};
                let {x85 = Cons x86 x87};
                let {x81 = Cons x82 x85};
                let {x77 = Cons x78 x81};
                let {x73 = Cons x74 x77};
                let {x69 = Cons x70 x73};
                let {x67 = Cons x68 x69};
                let {x65 = Cons x66 x67};
                let {x63 = Cons x64 x65};
                let {x61 = Cons x62 x63};
                let {x58 = Cons x59 x61};
                let {x55 = Cons x56 x58};
                let {x0 = Cons x53 x55};
                return x0},
            do {let {x103 = O};
                let {x102 = S x103};
                let {x106 = O};
                let {x105 = S x106};
                let {x108 = O};
                let {x110 = O};
                let {x112 = O};
                let {x114 = O};
                let {x118 = O};
                let {x117 = S x118};
                let {x116 = S x117};
                let {x122 = O};
                let {x121 = S x122};
                let {x120 = S x121};
                let {x126 = O};
                let {x125 = S x126};
                let {x124 = S x125};
                let {x130 = O};
                let {x129 = S x130};
                let {x128 = S x129};
                let {x132 = O};
                let {x134 = O};
                let {x136 = O};
                let {x138 = O};
                let {x140 = O};
                let {x142 = O};
                let {x144 = O};
                let {x146 = O};
                let {x147 = Nil};
                let {x145 = Cons x146 x147};
                let {x143 = Cons x144 x145};
                let {x141 = Cons x142 x143};
                let {x139 = Cons x140 x141};
                let {x137 = Cons x138 x139};
                let {x135 = Cons x136 x137};
                let {x133 = Cons x134 x135};
                let {x131 = Cons x132 x133};
                let {x127 = Cons x128 x131};
                let {x123 = Cons x124 x127};
                let {x119 = Cons x120 x123};
                let {x115 = Cons x116 x119};
                let {x113 = Cons x114 x115};
                let {x111 = Cons x112 x113};
                let {x109 = Cons x110 x111};
                let {x107 = Cons x108 x109};
                let {x104 = Cons x105 x107};
                let {x0 = Cons x102 x104};
                return x0},
            do {let {x149 = O};
                let {x148 = S x149};
                let {x151 = O};
                let {x153 = O};
                let {x155 = O};
                let {x157 = O};
                let {x161 = O};
                let {x160 = S x161};
                let {x159 = S x160};
                let {x165 = O};
                let {x164 = S x165};
                let {x163 = S x164};
                let {x169 = O};
                let {x168 = S x169};
                let {x167 = S x168};
                let {x173 = O};
                let {x172 = S x173};
                let {x171 = S x172};
                let {x175 = O};
                let {x177 = O};
                let {x179 = O};
                let {x181 = O};
                let {x183 = O};
                let {x185 = O};
                let {x187 = O};
                let {x189 = O};
                let {x190 = Nil};
                let {x188 = Cons x189 x190};
                let {x186 = Cons x187 x188};
                let {x184 = Cons x185 x186};
                let {x182 = Cons x183 x184};
                let {x180 = Cons x181 x182};
                let {x178 = Cons x179 x180};
                let {x176 = Cons x177 x178};
                let {x174 = Cons x175 x176};
                let {x170 = Cons x171 x174};
                let {x166 = Cons x167 x170};
                let {x162 = Cons x163 x166};
                let {x158 = Cons x159 x162};
                let {x156 = Cons x157 x158};
                let {x154 = Cons x155 x156};
                let {x152 = Cons x153 x154};
                let {x150 = Cons x151 x152};
                let {x0 = Cons x148 x150};
                return x0}]