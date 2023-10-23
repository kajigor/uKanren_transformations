{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}

import Test.Tasty.Bench
import Stream
import GHC.Generics (Generic)
import Control.Monad (msum, guard, MonadPlus)

import qualified Control.DeepSeq as DS
import Control.Applicative (Alternative)
import Debug.Trace (traceShow)

import Simple (Term (Cons, O, S, Nil) , rrIISimple)

rrOffline x0 = msum [do {(x1, x3) <- case x0 of
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

rrOnline x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rRI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                      return ()},
                                                                                                                                                  do {neqRRI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                      return ()}]
neqRRI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR4I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                           return ()}]
rRI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR14I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                      return ()}]
rR14I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR15I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                        return ()}]
rR15I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR16I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                        return ()}]
rR16I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                        return ()}]
rI x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {r0I x0;
                                                                                                                                                     x1 <- rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                     return ()},
                                                                                                                                                 do {r1I x0;
                                                                                                                                                     (x2,
                                                                                                                                                      x3) <- neqR1OO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                     return ()}]
neqR1OO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {let {x0 = O};
                                                                                                                                                       x58 <- rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                       (x59,
                                                                                                                                                        x1) <- case x58 of
                                                                                                                                                               {Cons y59
                                                                                                                                                                     y1 -> return (y59,
                                                                                                                                                                                   y1);
                                                                                                                                                                _ -> mzero};
                                                                                                                                                       guard (x59 == O);
                                                                                                                                                       return (x0,
                                                                                                                                                               x1)},
                                                                                                                                                   do {x61 <- rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                       (x62,
                                                                                                                                                        x1) <- case x61 of
                                                                                                                                                               {Cons y62
                                                                                                                                                                     y1 -> return (y62,
                                                                                                                                                                                   y1);
                                                                                                                                                                _ -> mzero};
                                                                                                                                                       x63 <- case x62 of
                                                                                                                                                              {S y63 -> return y63;
                                                                                                                                                               _ -> mzero};
                                                                                                                                                       x2 <- case x63 of
                                                                                                                                                             {S y2 -> return y2;
                                                                                                                                                              _ -> mzero};
                                                                                                                                                       let {x60 = S x2};
                                                                                                                                                       let {x0 = S x60};
                                                                                                                                                       return (x0,
                                                                                                                                                               x1)}]
r0I x0 = msum [do {(x3, x5) <- case x0 of
                               {Cons y3 y5 -> return (y3, y5); _ -> mzero};
                   x4 <- case x3 of
                         {S y4 -> return y4; _ -> mzero};
                   guard (x4 == O);
                   (x6, x1) <- case x5 of
                               {Cons y6 y1 -> return (y6, y1); _ -> mzero};
                   x7 <- case x6 of
                         {S y7 -> return y7; _ -> mzero};
                   guard (x7 == O);
                   return ()}]
r1I x0 = msum [do {(x8, x10) <- case x0 of
                                {Cons y8 y10 -> return (y8, y10); _ -> mzero};
                   x9 <- case x8 of
                         {S y9 -> return y9; _ -> mzero};
                   guard (x9 == O);
                   (x2, x3) <- case x10 of
                               {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                   return ()}]
rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR131O gen_rR131O_x13;
                                                                                                                                                     x1 <- rR12O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                     return x0},
                                                                                                                                                 do {x0 <- rR132O gen_rR132O_x18;
                                                                                                                                                     (x2,
                                                                                                                                                      x3) <- neqROO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                     return x0}]
neqROO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {let {x0 = O};
                                                                                                                        x52 <- rR12O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                        (x53,
                                                                                                                         x1) <- case x52 of
                                                                                                                                {Cons y53
                                                                                                                                      y1 -> return (y53,
                                                                                                                                                    y1);
                                                                                                                                 _ -> mzero};
                                                                                                                        guard (x53 == O);
                                                                                                                        return (x0,
                                                                                                                                x1)},
                                                                                                                    do {x55 <- rR12O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                        (x56,
                                                                                                                         x1) <- case x55 of
                                                                                                                                {Cons y56
                                                                                                                                      y1 -> return (y56,
                                                                                                                                                    y1);
                                                                                                                                 _ -> mzero};
                                                                                                                        x57 <- case x56 of
                                                                                                                               {S y57 -> return y57;
                                                                                                                                _ -> mzero};
                                                                                                                        x2 <- case x57 of
                                                                                                                              {S y2 -> return y2;
                                                                                                                               _ -> mzero};
                                                                                                                        let {x54 = S x2};
                                                                                                                        let {x0 = S x54};
                                                                                                                        return (x0,
                                                                                                                                x1)}]
rR12O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR122O gen_rR122O_x20;
                                                                                                                       x1 <- rR11O gen_rR102O_x28 gen_rR112O_x23 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                       return x0}]
rR11O gen_rR102O_x28 gen_rR112O_x23 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR112O gen_rR112O_x23;
                                                                                                        x1 <- rR10O gen_rR102O_x28 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                        return x0}]
rR10O gen_rR102O_x28 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR102O gen_rR102O_x28;
                                                                                         x1 <- rR9O gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                         return x0}]
rR102O gen_rR102O_x28 = msum [do {let {x27 = O};
                                  let {x26 = S x27};
                                  let {x25 = S x26};
                                  let {x31 = O};
                                  let {x30 = S x31};
                                  let {x29 = S x30};
                                  (x0, x28) <- do {x28 <- gen_rR102O_x28;
                                                   let {x0 = Cons x25 x28};
                                                   return (x0, x28)};
                                  x1 <- case x28 of
                                        {Cons y29 y1 -> do {guard (x29 == y29); return y1};
                                         _ -> mzero};
                                  return x0}]
rR112O gen_rR112O_x23 = msum [do {let {x22 = O};
                                  let {x24 = O};
                                  (x0, x23) <- do {x23 <- gen_rR112O_x23;
                                                   let {x0 = Cons x22 x23};
                                                   return (x0, x23)};
                                  x1 <- case x23 of
                                        {Cons y24 y1 -> do {guard (x24 == y24); return y1};
                                         _ -> mzero};
                                  return x0}]
rR122O gen_rR122O_x20 = msum [do {let {x19 = O};
                                  let {x21 = O};
                                  (x0, x20) <- do {x20 <- gen_rR122O_x20;
                                                   let {x0 = Cons x19 x20};
                                                   return (x0, x20)};
                                  x1 <- case x20 of
                                        {Cons y21 y1 -> do {guard (x21 == y21); return y1};
                                         _ -> mzero};
                                  return x0}]
rR131O gen_rR131O_x13 = msum [do {let {x12 = O};
                                  let {x11 = S x12};
                                  let {x15 = O};
                                  let {x14 = S x15};
                                  (x0, x13) <- do {x13 <- gen_rR131O_x13;
                                                   let {x0 = Cons x11 x13};
                                                   return (x0, x13)};
                                  x1 <- case x13 of
                                        {Cons y14 y1 -> do {guard (x14 == y14); return y1};
                                         _ -> mzero};
                                  return x0}]
rR132O gen_rR132O_x18 = msum [do {let {x17 = O};
                                  let {x16 = S x17};
                                  (x0, x18) <- do {x18 <- gen_rR132O_x18;
                                                   let {x0 = Cons x16 x18};
                                                   return (x0, x18)};
                                  (x2, x3) <- case x18 of
                                              {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                                  return x0}]
rR4I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR3I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                         return ()}]
rR3I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR2I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                         return ()}]
rR2I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR1I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                         return ()}]
rR1I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR13I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                         return ()}]
rR13I x0 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {rR131I x0;
                                                                                                                          x1 <- rR12O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                          return ()},
                                                                                                                      do {rR132I x0;
                                                                                                                          (x2,
                                                                                                                           x3) <- neqROO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                          return ()}]
rR131I x0 = msum [do {(x11, x13) <- case x0 of
                                    {Cons y11 y13 -> return (y11, y13); _ -> mzero};
                      x12 <- case x11 of
                             {S y12 -> return y12; _ -> mzero};
                      guard (x12 == O);
                      (x14, x1) <- case x13 of
                                   {Cons y14 y1 -> return (y14, y1); _ -> mzero};
                      x15 <- case x14 of
                             {S y15 -> return y15; _ -> mzero};
                      guard (x15 == O);
                      return ()}]
rR132I x0 = msum [do {(x16, x18) <- case x0 of
                                    {Cons y16 y18 -> return (y16, y18); _ -> mzero};
                      x17 <- case x16 of
                             {S y17 -> return y17; _ -> mzero};
                      guard (x17 == O);
                      (x2, x3) <- case x18 of
                                  {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                      return ()}]
rR9O gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR92O gen_rR92O_x35;
                                                                         x1 <- rR8O gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40;
                                                                         return x0}]
rR8O gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 = msum [do {x0 <- rR82O gen_rR82O_x40;
                                                           x1 <- rR7O gen_rR62O_x46 gen_rR72O_x43;
                                                           return x0}]
rR7O gen_rR62O_x46 gen_rR72O_x43 = msum [do {x0 <- rR72O gen_rR72O_x43;
                                             x1 <- rR6O gen_rR62O_x46;
                                             return x0}]
rR6O gen_rR62O_x46 = msum [do {x0 <- rR62O gen_rR62O_x46;
                               x1 <- rR5O;
                               return x0}]
rR5O = msum [do {let {x48 = O};
                 let {x50 = O};
                 let {x51 = Nil};
                 let {x49 = Cons x50 x51};
                 let {x0 = Cons x48 x49};
                 return x0}]
rR62O gen_rR62O_x46 = msum [do {let {x45 = O};
                                let {x47 = O};
                                (x0, x46) <- do {x46 <- gen_rR62O_x46;
                                                 let {x0 = Cons x45 x46};
                                                 return (x0, x46)};
                                x1 <- case x46 of
                                      {Cons y47 y1 -> do {guard (x47 == y47); return y1};
                                       _ -> mzero};
                                return x0}]
rR72O gen_rR72O_x43 = msum [do {let {x42 = O};
                                let {x44 = O};
                                (x0, x43) <- do {x43 <- gen_rR72O_x43;
                                                 let {x0 = Cons x42 x43};
                                                 return (x0, x43)};
                                x1 <- case x43 of
                                      {Cons y44 y1 -> do {guard (x44 == y44); return y1};
                                       _ -> mzero};
                                return x0}]
rR82O gen_rR82O_x40 = msum [do {let {x39 = O};
                                let {x41 = O};
                                (x0, x40) <- do {x40 <- gen_rR82O_x40;
                                                 let {x0 = Cons x39 x40};
                                                 return (x0, x40)};
                                x1 <- case x40 of
                                      {Cons y41 y1 -> do {guard (x41 == y41); return y1};
                                       _ -> mzero};
                                return x0}]
rR92O gen_rR92O_x35 = msum [do {let {x34 = O};
                                let {x33 = S x34};
                                let {x32 = S x33};
                                let {x38 = O};
                                let {x37 = S x38};
                                let {x36 = S x37};
                                (x0, x35) <- do {x35 <- gen_rR92O_x35;
                                                 let {x0 = Cons x32 x35};
                                                 return (x0, x35)};
                                x1 <- case x35 of
                                      {Cons y36 y1 -> do {guard (x36 == y36); return y1};
                                       _ -> mzero};
                                return x0}]
rrO gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rRO gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                          return x0},
                                                                                                                                                                      do {x0 <- neqRRO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                          return x0}]
neqRRO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR4O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                      return x0}]
rRO gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR14O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                          return x0}]
rR14O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR15O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                            return x0}]
rR15O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR16O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                            return x0}]
rR16O gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rO gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                            return x0}]
rO gen_r0O_x5 gen_r1O_x10 gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- r0O gen_r0O_x5;
                                                                                                                                                                         x1 <- rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                         return x0},
                                                                                                                                                                     do {x0 <- r1O gen_r1O_x10;
                                                                                                                                                                         (x2,
                                                                                                                                                                          x3) <- neqR1OO gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                                         return x0}]
r0O gen_r0O_x5 = msum [do {let {x4 = O};
                           let {x3 = S x4};
                           let {x7 = O};
                           let {x6 = S x7};
                           (x0, x5) <- do {x5 <- gen_r0O_x5;
                                           let {x0 = Cons x3 x5};
                                           return (x0, x5)};
                           x1 <- case x5 of
                                 {Cons y6 y1 -> do {guard (x6 == y6); return y1}; _ -> mzero};
                           return x0}]
r1O gen_r1O_x10 = msum [do {let {x9 = O};
                            let {x8 = S x9};
                            (x0, x10) <- do {x10 <- gen_r1O_x10;
                                             let {x0 = Cons x8 x10};
                                             return (x0, x10)};
                            (x2, x3) <- case x10 of
                                        {Cons y2 y3 -> return (y2, y3); _ -> mzero};
                            return x0}]
rR4O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR3O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                    return x0}]
rR3O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR2O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                    return x0}]
rR2O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR1O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                    return x0}]
rR1O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35 = msum [do {x0 <- rR13O gen_rR102O_x28 gen_rR112O_x23 gen_rR122O_x20 gen_rR131O_x13 gen_rR132O_x18 gen_rR62O_x46 gen_rR72O_x43 gen_rR82O_x40 gen_rR92O_x35;
                                                                                                                                                    return x0}]


natGen :: (MonadPlus m) => m Term
natGen = return O <|> (S <$> natGen)

eval :: Show r => (m r -> [r]) -> (a -> m r) -> a -> [r]
eval listify f = listify . f

eval9 :: Show r => (m r -> [r]) -> (a -> x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> m r) -> (a, x1, x2, x3, x4, x5, x6, x7, x8, x9) -> [r]
eval9 listify f = eval listify  $ \(b, y1, y2, y3, y4, y5, y6, y7, y8, y9) -> f b y1 y2 y3 y4 y5 y6 y7 y8 y9


eval14 :: Show r => (m r -> [r]) -> (x1 -> x2 -> x3 -> x4 -> x5 -> x6 -> x7 -> x8 -> x9 -> x10 -> x11 -> x12 -> x13 -> x14 -> m r) -> (x1, x2, x3, x4, x5, x6, x7, x8, x9, x10, x11, x12, x13, x14) -> [r]
eval14 listify f = eval listify $ \(y1, y2, y3, y4, y5, y6, y7, y8, y9, y10, y11, y12, y13, y14) -> f y1 y2 y3 y4 y5 y6 y7 y8 y9 y10 y11 y12 y13 y14

put14 x y = 
  (x, y, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen)

rightTerm :: Term
rightTerm = Cons (S O) (Cons (S O) (Cons (S O) (Cons (S O) (Cons O (Cons O (Cons O (Cons O (Cons (S (S O)) (Cons (S (S O)) (Cons (S (S O)) (Cons (S (S O)) (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O Nil)))))))))))))))))))

wrongGen :: (Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term)
wrongGen = (Cons (S O) Nil, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen)
rightGen :: (Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term, Stream Term)
rightGen = (rightTerm, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen, natGen)

--[S O, O, S (S O), O, O]
secondTerm = Cons (S O) (Cons O (Cons (S (S O)) (Cons O (Cons O Nil))))

main :: IO ()
main = defaultMain
  [
    bgroup "Deforestation2"
     [ bench "offline1"    $ nf (eval (takeS 1) rrOffline) (Cons (S O) Nil)
     , bench "online1"     $ nf (eval9 (takeS 1) rrOnline) wrongGen
--     , bench "simple1"     $ nf (eval14 (takeS 1) rrIISimple) $ put14 rightTerm secondTerm  -- failing
     , bench "offline2"    $ nf (eval (takeS 1) rrOffline) rightTerm
     --, bench "online2"     $ nf (eval9 (takeS 1) rrOnline) rightGen --failing
     ]
  ]

--  [S O, S O, S O, S O, O, O, O, O, S (S O), S (S O), S (S O), S (S O), O, O, O, O, O, O, O, O]

--  Cons (S O) (Cons (S O) (Cons (S O) (Cons (S O) (Cons O (Cons O (Cons O (Cons O (Cons (S (S O)) (Cons (S (S O)) (Cons (S (S O)) (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O (Cons O Nil))))))))))))))))))