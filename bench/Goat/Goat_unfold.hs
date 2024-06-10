module Goat_unfold where

import Stream
import Control.Monad
import Term

evalI x0 = Immature $ msum [do {let {x29 = Term.Empty};
                     let {x31 = Term.Empty};
                     (x32, x33) <- case x0 of
                                   {Cons y32 y33 -> return (y32, y33); _ -> mzero};
                     guard (x32 == x29);
                     let {x30 = x33};
                     x1 <- case x30 of
                           {Cons y31 y1 -> do {guard (x31 == y31); return y1}; _ -> mzero};
                     evalI x1;
                     return ()},
                 do {let {x34 = Goat};
                     let {x36 = Term.Empty};
                     let {x38 = Term.Empty};
                     (x39, x40) <- case x0 of
                                   {Cons y39 y40 -> return (y39, y40); _ -> mzero};
                     guard (x39 == x34);
                     let {x35 = x40};
                     x37 <- case x35 of
                            {Cons y36 y37 -> do {guard (x36 == y36); return y37}; _ -> mzero};
                     x2 <- case x37 of
                           {Cons y38 y2 -> do {guard (x38 == y38); return y2}; _ -> mzero};
                     _evalI x2;
                     return ()},
                 do {let {x41 = Goat};
                     let {x43 = Term.Empty};
                     let {x45 = Wolf};
                     let {x47 = Term.Empty};
                     let {x49 = Term.Empty};
                     (x50, x51) <- case x0 of
                                   {Cons y50 y51 -> return (y50, y51); _ -> mzero};
                     guard (x50 == x41);
                     let {x42 = x51};
                     x44 <- case x42 of
                            {Cons y43 y44 -> do {guard (x43 == y43); return y44}; _ -> mzero};
                     x46 <- case x44 of
                            {Cons y45 y46 -> do {guard (x45 == y45); return y46}; _ -> mzero};
                     x48 <- case x46 of
                            {Cons y47 y48 -> do {guard (x47 == y47); return y48}; _ -> mzero};
                     x3 <- case x48 of
                           {Cons y49 y3 -> do {guard (x49 == y49); return y3}; _ -> mzero};
                     __evalI x3;
                     return ()},
                 do {let {x52 = Goat};
                     let {x54 = Term.Empty};
                     let {x56 = Wolf};
                     let {x58 = Term.Empty};
                     let {x60 = Cabbage};
                     let {x62 = Term.Empty};
                     let {x64 = Term.Empty};
                     (x65, x66) <- case x0 of
                                   {Cons y65 y66 -> return (y65, y66); _ -> mzero};
                     guard (x65 == x52);
                     let {x53 = x66};
                     x55 <- case x53 of
                            {Cons y54 y55 -> do {guard (x54 == y54); return y55}; _ -> mzero};
                     x57 <- case x55 of
                            {Cons y56 y57 -> do {guard (x56 == y56); return y57}; _ -> mzero};
                     x59 <- case x57 of
                            {Cons y58 y59 -> do {guard (x58 == y58); return y59}; _ -> mzero};
                     x61 <- case x59 of
                            {Cons y60 y61 -> do {guard (x60 == y60); return y61}; _ -> mzero};
                     x63 <- case x61 of
                            {Cons y62 y63 -> do {guard (x62 == y62); return y63}; _ -> mzero};
                     x4 <- case x63 of
                           {Cons y64 y4 -> do {guard (x64 == y64); return y4}; _ -> mzero};
                     ___evalI x4;
                     return ()},
                 do {let {x67 = Goat};
                     let {x69 = Term.Empty};
                     let {x71 = Wolf};
                     let {x73 = Term.Empty};
                     let {x75 = Cabbage};
                     let {x77 = Wolf};
                     let {x79 = Term.Empty};
                     let {x81 = Term.Empty};
                     (x82, x83) <- case x0 of
                                   {Cons y82 y83 -> return (y82, y83); _ -> mzero};
                     guard (x82 == x67);
                     let {x68 = x83};
                     x70 <- case x68 of
                            {Cons y69 y70 -> do {guard (x69 == y69); return y70}; _ -> mzero};
                     x72 <- case x70 of
                            {Cons y71 y72 -> do {guard (x71 == y71); return y72}; _ -> mzero};
                     x74 <- case x72 of
                            {Cons y73 y74 -> do {guard (x73 == y73); return y74}; _ -> mzero};
                     x76 <- case x74 of
                            {Cons y75 y76 -> do {guard (x75 == y75); return y76}; _ -> mzero};
                     x78 <- case x76 of
                            {Cons y77 y78 -> do {guard (x77 == y77); return y78}; _ -> mzero};
                     x80 <- case x78 of
                            {Cons y79 y80 -> do {guard (x79 == y79); return y80}; _ -> mzero};
                     x5 <- case x80 of
                           {Cons y81 y5 -> do {guard (x81 == y81); return y5}; _ -> mzero};
                     swapEvalI x5;
                     return ()},
                 do {let {x84 = Goat};
                     let {x86 = Term.Empty};
                     let {x88 = Wolf};
                     let {x90 = Term.Empty};
                     let {x92 = Cabbage};
                     let {x94 = Wolf};
                     let {x96 = Term.Empty};
                     let {x98 = Cabbage};
                     let {x100 = Term.Empty};
                     let {x102 = Term.Empty};
                     (x103, x104) <- case x0 of
                                     {Cons y103 y104 -> return (y103, y104); _ -> mzero};
                     guard (x103 == x84);
                     let {x85 = x104};
                     x87 <- case x85 of
                            {Cons y86 y87 -> do {guard (x86 == y86); return y87}; _ -> mzero};
                     x89 <- case x87 of
                            {Cons y88 y89 -> do {guard (x88 == y88); return y89}; _ -> mzero};
                     x91 <- case x89 of
                            {Cons y90 y91 -> do {guard (x90 == y90); return y91}; _ -> mzero};
                     x93 <- case x91 of
                            {Cons y92 y93 -> do {guard (x92 == y92); return y93}; _ -> mzero};
                     x95 <- case x93 of
                            {Cons y94 y95 -> do {guard (x94 == y94); return y95}; _ -> mzero};
                     x97 <- case x95 of
                            {Cons y96 y97 -> do {guard (x96 == y96); return y97}; _ -> mzero};
                     x99 <- case x97 of
                            {Cons y98 y99 -> do {guard (x98 == y98); return y99}; _ -> mzero};
                     x101 <- case x99 of
                             {Cons y100 y101 -> do {guard (x100 == y100); return y101};
                              _ -> mzero};
                     x6 <- case x101 of
                           {Cons y102 y6 -> do {guard (x102 == y102); return y6}; _ -> mzero};
                     _swapEvalI x6;
                     return ()},
                 do {let {x105 = Goat};
                     let {x107 = Term.Empty};
                     let {x109 = Wolf};
                     let {x111 = Term.Empty};
                     let {x113 = Cabbage};
                     let {x115 = Wolf};
                     let {x117 = Term.Empty};
                     let {x119 = Cabbage};
                     let {x121 = Wolf};
                     (x122, x123) <- case x0 of
                                     {Cons y122 y123 -> return (y122, y123); _ -> mzero};
                     guard (x122 == x105);
                     let {x106 = x123};
                     x108 <- case x106 of
                             {Cons y107 y108 -> do {guard (x107 == y107); return y108};
                              _ -> mzero};
                     x110 <- case x108 of
                             {Cons y109 y110 -> do {guard (x109 == y109); return y110};
                              _ -> mzero};
                     x112 <- case x110 of
                             {Cons y111 y112 -> do {guard (x111 == y111); return y112};
                              _ -> mzero};
                     x114 <- case x112 of
                             {Cons y113 y114 -> do {guard (x113 == y113); return y114};
                              _ -> mzero};
                     x116 <- case x114 of
                             {Cons y115 y116 -> do {guard (x115 == y115); return y116};
                              _ -> mzero};
                     x118 <- case x116 of
                             {Cons y117 y118 -> do {guard (x117 == y117); return y118};
                              _ -> mzero};
                     x120 <- case x118 of
                             {Cons y119 y120 -> do {guard (x119 == y119); return y120};
                              _ -> mzero};
                     x7 <- case x120 of
                           {Cons y121 y7 -> do {guard (x121 == y121); return y7}; _ -> mzero};
                     __evalI x7;
                     return ()},
                 do {let {x124 = Goat};
                     let {x126 = Term.Empty};
                     let {x128 = Wolf};
                     let {x130 = Term.Empty};
                     let {x132 = Cabbage};
                     let {x134 = Wolf};
                     let {x136 = Term.Empty};
                     let {x138 = Cabbage};
                     let {x140 = Cabbage};
                     (x141, x142) <- case x0 of
                                     {Cons y141 y142 -> return (y141, y142); _ -> mzero};
                     guard (x141 == x124);
                     let {x125 = x142};
                     x127 <- case x125 of
                             {Cons y126 y127 -> do {guard (x126 == y126); return y127};
                              _ -> mzero};
                     x129 <- case x127 of
                             {Cons y128 y129 -> do {guard (x128 == y128); return y129};
                              _ -> mzero};
                     x131 <- case x129 of
                             {Cons y130 y131 -> do {guard (x130 == y130); return y131};
                              _ -> mzero};
                     x133 <- case x131 of
                             {Cons y132 y133 -> do {guard (x132 == y132); return y133};
                              _ -> mzero};
                     x135 <- case x133 of
                             {Cons y134 y135 -> do {guard (x134 == y134); return y135};
                              _ -> mzero};
                     x137 <- case x135 of
                             {Cons y136 y137 -> do {guard (x136 == y136); return y137};
                              _ -> mzero};
                     x139 <- case x137 of
                             {Cons y138 y139 -> do {guard (x138 == y138); return y139};
                              _ -> mzero};
                     x7 <- case x139 of
                           {Cons y140 y7 -> do {guard (x140 == y140); return y7}; _ -> mzero};
                     ____evalI x7;
                     return ()},
                 do {let {x143 = Goat};
                     let {x145 = Term.Empty};
                     let {x147 = Wolf};
                     let {x149 = Term.Empty};
                     let {x151 = Cabbage};
                     let {x153 = Wolf};
                     let {x155 = Wolf};
                     (x156, x157) <- case x0 of
                                     {Cons y156 y157 -> return (y156, y157); _ -> mzero};
                     guard (x156 == x143);
                     let {x144 = x157};
                     x146 <- case x144 of
                             {Cons y145 y146 -> do {guard (x145 == y145); return y146};
                              _ -> mzero};
                     x148 <- case x146 of
                             {Cons y147 y148 -> do {guard (x147 == y147); return y148};
                              _ -> mzero};
                     x150 <- case x148 of
                             {Cons y149 y150 -> do {guard (x149 == y149); return y150};
                              _ -> mzero};
                     x152 <- case x150 of
                             {Cons y151 y152 -> do {guard (x151 == y151); return y152};
                              _ -> mzero};
                     x154 <- case x152 of
                             {Cons y153 y154 -> do {guard (x153 == y153); return y154};
                              _ -> mzero};
                     x8 <- case x154 of
                           {Cons y155 y8 -> do {guard (x155 == y155); return y8}; _ -> mzero};
                     ___evalI x8;
                     return ()},
                 do {let {x158 = Goat};
                     let {x160 = Term.Empty};
                     let {x162 = Wolf};
                     let {x164 = Term.Empty};
                     let {x166 = Cabbage};
                     let {x168 = Cabbage};
                     (x169, x170) <- case x0 of
                                     {Cons y169 y170 -> return (y169, y170); _ -> mzero};
                     guard (x169 == x158);
                     let {x159 = x170};
                     x161 <- case x159 of
                             {Cons y160 y161 -> do {guard (x160 == y160); return y161};
                              _ -> mzero};
                     x163 <- case x161 of
                             {Cons y162 y163 -> do {guard (x162 == y162); return y163};
                              _ -> mzero};
                     x165 <- case x163 of
                             {Cons y164 y165 -> do {guard (x164 == y164); return y165};
                              _ -> mzero};
                     x167 <- case x165 of
                             {Cons y166 y167 -> do {guard (x166 == y166); return y167};
                              _ -> mzero};
                     x9 <- case x167 of
                           {Cons y168 y9 -> do {guard (x168 == y168); return y9}; _ -> mzero};
                     __swapEvalI x9;
                     return ()},
                 do {let {x171 = Goat};
                     let {x173 = Term.Empty};
                     let {x175 = Wolf};
                     let {x177 = Wolf};
                     let {x179 = Term.Empty};
                     let {x181 = Term.Empty};
                     (x182, x183) <- case x0 of
                                     {Cons y182 y183 -> return (y182, y183); _ -> mzero};
                     guard (x182 == x171);
                     let {x172 = x183};
                     x174 <- case x172 of
                             {Cons y173 y174 -> do {guard (x173 == y173); return y174};
                              _ -> mzero};
                     x176 <- case x174 of
                             {Cons y175 y176 -> do {guard (x175 == y175); return y176};
                              _ -> mzero};
                     x178 <- case x176 of
                             {Cons y177 y178 -> do {guard (x177 == y177); return y178};
                              _ -> mzero};
                     x180 <- case x178 of
                             {Cons y179 y180 -> do {guard (x179 == y179); return y180};
                              _ -> mzero};
                     x9 <- case x180 of
                           {Cons y181 y9 -> do {guard (x181 == y181); return y9}; _ -> mzero};
                     _swapEvalI x9;
                     return ()},
                 do {let {x184 = Goat};
                     let {x186 = Term.Empty};
                     let {x188 = Wolf};
                     let {x190 = Wolf};
                     let {x192 = Wolf};
                     (x193, x194) <- case x0 of
                                     {Cons y193 y194 -> return (y193, y194); _ -> mzero};
                     guard (x193 == x184);
                     let {x185 = x194};
                     x187 <- case x185 of
                             {Cons y186 y187 -> do {guard (x186 == y186); return y187};
                              _ -> mzero};
                     x189 <- case x187 of
                             {Cons y188 y189 -> do {guard (x188 == y188); return y189};
                              _ -> mzero};
                     x191 <- case x189 of
                             {Cons y190 y191 -> do {guard (x190 == y190); return y191};
                              _ -> mzero};
                     x10 <- case x191 of
                            {Cons y192 y10 -> do {guard (x192 == y192); return y10};
                             _ -> mzero};
                     __evalI x10;
                     return ()},
                 do {let {x195 = Goat};
                     let {x197 = Term.Empty};
                     let {x199 = Wolf};
                     let {x201 = Wolf};
                     let {x203 = Cabbage};
                     let {x205 = Term.Empty};
                     let {x207 = Term.Empty};
                     (x208, x209) <- case x0 of
                                     {Cons y208 y209 -> return (y208, y209); _ -> mzero};
                     guard (x208 == x195);
                     let {x196 = x209};
                     x198 <- case x196 of
                             {Cons y197 y198 -> do {guard (x197 == y197); return y198};
                              _ -> mzero};
                     x200 <- case x198 of
                             {Cons y199 y200 -> do {guard (x199 == y199); return y200};
                              _ -> mzero};
                     x202 <- case x200 of
                             {Cons y201 y202 -> do {guard (x201 == y201); return y202};
                              _ -> mzero};
                     x204 <- case x202 of
                             {Cons y203 y204 -> do {guard (x203 == y203); return y204};
                              _ -> mzero};
                     x206 <- case x204 of
                             {Cons y205 y206 -> do {guard (x205 == y205); return y206};
                              _ -> mzero};
                     x8 <- case x206 of
                           {Cons y207 y8 -> do {guard (x207 == y207); return y8}; _ -> mzero};
                     ____evalI x8;
                     return ()},
                 do {let {x210 = Goat};
                     let {x212 = Term.Empty};
                     let {x214 = Wolf};
                     let {x216 = Wolf};
                     let {x218 = Cabbage};
                     let {x220 = Term.Empty};
                     let {x222 = Wolf};
                     let {x224 = Term.Empty};
                     let {x226 = Term.Empty};
                     (x227, x228) <- case x0 of
                                     {Cons y227 y228 -> return (y227, y228); _ -> mzero};
                     guard (x227 == x210);
                     let {x211 = x228};
                     x213 <- case x211 of
                             {Cons y212 y213 -> do {guard (x212 == y212); return y213};
                              _ -> mzero};
                     x215 <- case x213 of
                             {Cons y214 y215 -> do {guard (x214 == y214); return y215};
                              _ -> mzero};
                     x217 <- case x215 of
                             {Cons y216 y217 -> do {guard (x216 == y216); return y217};
                              _ -> mzero};
                     x219 <- case x217 of
                             {Cons y218 y219 -> do {guard (x218 == y218); return y219};
                              _ -> mzero};
                     x221 <- case x219 of
                             {Cons y220 y221 -> do {guard (x220 == y220); return y221};
                              _ -> mzero};
                     x223 <- case x221 of
                             {Cons y222 y223 -> do {guard (x222 == y222); return y223};
                              _ -> mzero};
                     x225 <- case x223 of
                             {Cons y224 y225 -> do {guard (x224 == y224); return y225};
                              _ -> mzero};
                     x7 <- case x225 of
                           {Cons y226 y7 -> do {guard (x226 == y226); return y7}; _ -> mzero};
                     ___evalI x7;
                     return ()},
                 do {let {x229 = Goat};
                     let {x231 = Term.Empty};
                     let {x233 = Wolf};
                     let {x235 = Wolf};
                     let {x237 = Cabbage};
                     let {x239 = Term.Empty};
                     let {x241 = Wolf};
                     let {x243 = Wolf};
                     (x244, x245) <- case x0 of
                                     {Cons y244 y245 -> return (y244, y245); _ -> mzero};
                     guard (x244 == x229);
                     let {x230 = x245};
                     x232 <- case x230 of
                             {Cons y231 y232 -> do {guard (x231 == y231); return y232};
                              _ -> mzero};
                     x234 <- case x232 of
                             {Cons y233 y234 -> do {guard (x233 == y233); return y234};
                              _ -> mzero};
                     x236 <- case x234 of
                             {Cons y235 y236 -> do {guard (x235 == y235); return y236};
                              _ -> mzero};
                     x238 <- case x236 of
                             {Cons y237 y238 -> do {guard (x237 == y237); return y238};
                              _ -> mzero};
                     x240 <- case x238 of
                             {Cons y239 y240 -> do {guard (x239 == y239); return y240};
                              _ -> mzero};
                     x242 <- case x240 of
                             {Cons y241 y242 -> do {guard (x241 == y241); return y242};
                              _ -> mzero};
                     x11 <- case x242 of
                            {Cons y243 y11 -> do {guard (x243 == y243); return y11};
                             _ -> mzero};
                     swapEvalI x11;
                     return ()},
                 do {let {x246 = Goat};
                     let {x248 = Term.Empty};
                     let {x250 = Wolf};
                     let {x252 = Wolf};
                     let {x254 = Cabbage};
                     let {x256 = Term.Empty};
                     let {x258 = Wolf};
                     let {x260 = Cabbage};
                     let {x262 = Term.Empty};
                     (x263, x264) <- case x0 of
                                     {Cons y263 y264 -> return (y263, y264); _ -> mzero};
                     guard (x263 == x246);
                     let {x247 = x264};
                     x249 <- case x247 of
                             {Cons y248 y249 -> do {guard (x248 == y248); return y249};
                              _ -> mzero};
                     x251 <- case x249 of
                             {Cons y250 y251 -> do {guard (x250 == y250); return y251};
                              _ -> mzero};
                     x253 <- case x251 of
                             {Cons y252 y253 -> do {guard (x252 == y252); return y253};
                              _ -> mzero};
                     x255 <- case x253 of
                             {Cons y254 y255 -> do {guard (x254 == y254); return y255};
                              _ -> mzero};
                     x257 <- case x255 of
                             {Cons y256 y257 -> do {guard (x256 == y256); return y257};
                              _ -> mzero};
                     x259 <- case x257 of
                             {Cons y258 y259 -> do {guard (x258 == y258); return y259};
                              _ -> mzero};
                     x261 <- case x259 of
                             {Cons y260 y261 -> do {guard (x260 == y260); return y261};
                              _ -> mzero};
                     x12 <- case x261 of
                            {Cons y262 y12 -> do {guard (x262 == y262); return y12};
                             _ -> mzero};
                     __evalI x12;
                     return ()},
                 do {let {x265 = Goat};
                     let {x267 = Term.Empty};
                     let {x269 = Wolf};
                     let {x271 = Wolf};
                     let {x273 = Cabbage};
                     let {x275 = Term.Empty};
                     let {x277 = Wolf};
                     let {x279 = Cabbage};
                     let {x281 = Cabbage};
                     (x282, x283) <- case x0 of
                                     {Cons y282 y283 -> return (y282, y283); _ -> mzero};
                     guard (x282 == x265);
                     let {x266 = x283};
                     x268 <- case x266 of
                             {Cons y267 y268 -> do {guard (x267 == y267); return y268};
                              _ -> mzero};
                     x270 <- case x268 of
                             {Cons y269 y270 -> do {guard (x269 == y269); return y270};
                              _ -> mzero};
                     x272 <- case x270 of
                             {Cons y271 y272 -> do {guard (x271 == y271); return y272};
                              _ -> mzero};
                     x274 <- case x272 of
                             {Cons y273 y274 -> do {guard (x273 == y273); return y274};
                              _ -> mzero};
                     x276 <- case x274 of
                             {Cons y275 y276 -> do {guard (x275 == y275); return y276};
                              _ -> mzero};
                     x278 <- case x276 of
                             {Cons y277 y278 -> do {guard (x277 == y277); return y278};
                              _ -> mzero};
                     x280 <- case x278 of
                             {Cons y279 y280 -> do {guard (x279 == y279); return y280};
                              _ -> mzero};
                     x12 <- case x280 of
                            {Cons y281 y12 -> do {guard (x281 == y281); return y12};
                             _ -> mzero};
                     ___evalI x12;
                     return ()},
                 do {let {x284 = Goat};
                     let {x286 = Term.Empty};
                     let {x288 = Wolf};
                     let {x290 = Wolf};
                     let {x292 = Cabbage};
                     let {x294 = Cabbage};
                     (x295, x296) <- case x0 of
                                     {Cons y295 y296 -> return (y295, y296); _ -> mzero};
                     guard (x295 == x284);
                     let {x285 = x296};
                     x287 <- case x285 of
                             {Cons y286 y287 -> do {guard (x286 == y286); return y287};
                              _ -> mzero};
                     x289 <- case x287 of
                             {Cons y288 y289 -> do {guard (x288 == y288); return y289};
                              _ -> mzero};
                     x291 <- case x289 of
                             {Cons y290 y291 -> do {guard (x290 == y290); return y291};
                              _ -> mzero};
                     x293 <- case x291 of
                             {Cons y292 y293 -> do {guard (x292 == y292); return y293};
                              _ -> mzero};
                     x13 <- case x293 of
                            {Cons y294 y13 -> do {guard (x294 == y294); return y13};
                             _ -> mzero};
                     _swapEvalI x13;
                     return ()},
                 do {let {x297 = Goat};
                     let {x299 = Term.Empty};
                     let {x301 = Cabbage};
                     let {x303 = Goat};
                     let {x305 = Goat};
                     (x306, x307) <- case x0 of
                                     {Cons y306 y307 -> return (y306, y307); _ -> mzero};
                     guard (x306 == x297);
                     let {x298 = x307};
                     x300 <- case x298 of
                             {Cons y299 y300 -> do {guard (x299 == y299); return y300};
                              _ -> mzero};
                     x302 <- case x300 of
                             {Cons y301 y302 -> do {guard (x301 == y301); return y302};
                              _ -> mzero};
                     x304 <- case x302 of
                             {Cons y303 y304 -> do {guard (x303 == y303); return y304};
                              _ -> mzero};
                     x10 <- case x304 of
                            {Cons y305 y10 -> do {guard (x305 == y305); return y10};
                             _ -> mzero};
                     _____evalI x10;
                     return ()},
                 do {let {x308 = Goat};
                     let {x310 = Term.Empty};
                     let {x312 = Cabbage};
                     let {x314 = Goat};
                     let {x316 = Wolf};
                     let {x318 = Term.Empty};
                     let {x320 = Term.Empty};
                     (x321, x322) <- case x0 of
                                     {Cons y321 y322 -> return (y321, y322); _ -> mzero};
                     guard (x321 == x308);
                     let {x309 = x322};
                     x311 <- case x309 of
                             {Cons y310 y311 -> do {guard (x310 == y310); return y311};
                              _ -> mzero};
                     x313 <- case x311 of
                             {Cons y312 y313 -> do {guard (x312 == y312); return y313};
                              _ -> mzero};
                     x315 <- case x313 of
                             {Cons y314 y315 -> do {guard (x314 == y314); return y315};
                              _ -> mzero};
                     x317 <- case x315 of
                             {Cons y316 y317 -> do {guard (x316 == y316); return y317};
                              _ -> mzero};
                     x319 <- case x317 of
                             {Cons y318 y319 -> do {guard (x318 == y318); return y319};
                              _ -> mzero};
                     x14 <- case x319 of
                            {Cons y320 y14 -> do {guard (x320 == y320); return y14};
                             _ -> mzero};
                     ______evalI x14;
                     return ()},
                 do {let {x323 = Goat};
                     let {x325 = Term.Empty};
                     let {x327 = Cabbage};
                     let {x329 = Goat};
                     let {x331 = Wolf};
                     let {x333 = Term.Empty};
                     let {x335 = Goat};
                     (x336, x337) <- case x0 of
                                     {Cons y336 y337 -> return (y336, y337); _ -> mzero};
                     guard (x336 == x323);
                     let {x324 = x337};
                     x326 <- case x324 of
                             {Cons y325 y326 -> do {guard (x325 == y325); return y326};
                              _ -> mzero};
                     x328 <- case x326 of
                             {Cons y327 y328 -> do {guard (x327 == y327); return y328};
                              _ -> mzero};
                     x330 <- case x328 of
                             {Cons y329 y330 -> do {guard (x329 == y329); return y330};
                              _ -> mzero};
                     x332 <- case x330 of
                             {Cons y331 y332 -> do {guard (x331 == y331); return y332};
                              _ -> mzero};
                     x334 <- case x332 of
                             {Cons y333 y334 -> do {guard (x333 == y333); return y334};
                              _ -> mzero};
                     x14 <- case x334 of
                            {Cons y335 y14 -> do {guard (x335 == y335); return y14};
                             _ -> mzero};
                     _______evalI x14;
                     return ()},
                 do {let {x338 = Goat};
                     let {x340 = Term.Empty};
                     let {x342 = Cabbage};
                     let {x344 = Goat};
                     let {x346 = Wolf};
                     let {x348 = Wolf};
                     let {x350 = Term.Empty};
                     let {x352 = Term.Empty};
                     (x353, x354) <- case x0 of
                                     {Cons y353 y354 -> return (y353, y354); _ -> mzero};
                     guard (x353 == x338);
                     let {x339 = x354};
                     x341 <- case x339 of
                             {Cons y340 y341 -> do {guard (x340 == y340); return y341};
                              _ -> mzero};
                     x343 <- case x341 of
                             {Cons y342 y343 -> do {guard (x342 == y342); return y343};
                              _ -> mzero};
                     x345 <- case x343 of
                             {Cons y344 y345 -> do {guard (x344 == y344); return y345};
                              _ -> mzero};
                     x347 <- case x345 of
                             {Cons y346 y347 -> do {guard (x346 == y346); return y347};
                              _ -> mzero};
                     x349 <- case x347 of
                             {Cons y348 y349 -> do {guard (x348 == y348); return y349};
                              _ -> mzero};
                     x351 <- case x349 of
                             {Cons y350 y351 -> do {guard (x350 == y350); return y351};
                              _ -> mzero};
                     x15 <- case x351 of
                            {Cons y352 y15 -> do {guard (x352 == y352); return y15};
                             _ -> mzero};
                     swapEvalI x15;
                     return ()},
                 do {let {x355 = Goat};
                     let {x357 = Term.Empty};
                     let {x359 = Cabbage};
                     let {x361 = Goat};
                     let {x363 = Wolf};
                     let {x365 = Wolf};
                     let {x367 = Term.Empty};
                     let {x369 = Cabbage};
                     let {x371 = Term.Empty};
                     let {x373 = Term.Empty};
                     (x374, x375) <- case x0 of
                                     {Cons y374 y375 -> return (y374, y375); _ -> mzero};
                     guard (x374 == x355);
                     let {x356 = x375};
                     x358 <- case x356 of
                             {Cons y357 y358 -> do {guard (x357 == y357); return y358};
                              _ -> mzero};
                     x360 <- case x358 of
                             {Cons y359 y360 -> do {guard (x359 == y359); return y360};
                              _ -> mzero};
                     x362 <- case x360 of
                             {Cons y361 y362 -> do {guard (x361 == y361); return y362};
                              _ -> mzero};
                     x364 <- case x362 of
                             {Cons y363 y364 -> do {guard (x363 == y363); return y364};
                              _ -> mzero};
                     x366 <- case x364 of
                             {Cons y365 y366 -> do {guard (x365 == y365); return y366};
                              _ -> mzero};
                     x368 <- case x366 of
                             {Cons y367 y368 -> do {guard (x367 == y367); return y368};
                              _ -> mzero};
                     x370 <- case x368 of
                             {Cons y369 y370 -> do {guard (x369 == y369); return y370};
                              _ -> mzero};
                     x372 <- case x370 of
                             {Cons y371 y372 -> do {guard (x371 == y371); return y372};
                              _ -> mzero};
                     x16 <- case x372 of
                            {Cons y373 y16 -> do {guard (x373 == y373); return y16};
                             _ -> mzero};
                     _swapEvalI x16;
                     return ()},
                 do {let {x376 = Goat};
                     let {x378 = Term.Empty};
                     let {x380 = Cabbage};
                     let {x382 = Goat};
                     let {x384 = Wolf};
                     let {x386 = Wolf};
                     let {x388 = Term.Empty};
                     let {x390 = Cabbage};
                     let {x392 = Wolf};
                     let {x394 = Term.Empty};
                     let {x396 = Term.Empty};
                     (x397, x398) <- case x0 of
                                     {Cons y397 y398 -> return (y397, y398); _ -> mzero};
                     guard (x397 == x376);
                     let {x377 = x398};
                     x379 <- case x377 of
                             {Cons y378 y379 -> do {guard (x378 == y378); return y379};
                              _ -> mzero};
                     x381 <- case x379 of
                             {Cons y380 y381 -> do {guard (x380 == y380); return y381};
                              _ -> mzero};
                     x383 <- case x381 of
                             {Cons y382 y383 -> do {guard (x382 == y382); return y383};
                              _ -> mzero};
                     x385 <- case x383 of
                             {Cons y384 y385 -> do {guard (x384 == y384); return y385};
                              _ -> mzero};
                     x387 <- case x385 of
                             {Cons y386 y387 -> do {guard (x386 == y386); return y387};
                              _ -> mzero};
                     x389 <- case x387 of
                             {Cons y388 y389 -> do {guard (x388 == y388); return y389};
                              _ -> mzero};
                     x391 <- case x389 of
                             {Cons y390 y391 -> do {guard (x390 == y390); return y391};
                              _ -> mzero};
                     x393 <- case x391 of
                             {Cons y392 y393 -> do {guard (x392 == y392); return y393};
                              _ -> mzero};
                     x395 <- case x393 of
                             {Cons y394 y395 -> do {guard (x394 == y394); return y395};
                              _ -> mzero};
                     x17 <- case x395 of
                            {Cons y396 y17 -> do {guard (x396 == y396); return y17};
                             _ -> mzero};
                     __evalI x17;
                     return ()},
                 do {let {x399 = Goat};
                     let {x401 = Term.Empty};
                     let {x403 = Cabbage};
                     let {x405 = Goat};
                     let {x407 = Wolf};
                     let {x409 = Wolf};
                     let {x411 = Term.Empty};
                     let {x413 = Cabbage};
                     let {x415 = Wolf};
                     let {x417 = Term.Empty};
                     let {x419 = Cabbage};
                     let {x421 = Term.Empty};
                     let {x423 = Term.Empty};
                     (x424, x425) <- case x0 of
                                     {Cons y424 y425 -> return (y424, y425); _ -> mzero};
                     guard (x424 == x399);
                     let {x400 = x425};
                     x402 <- case x400 of
                             {Cons y401 y402 -> do {guard (x401 == y401); return y402};
                              _ -> mzero};
                     x404 <- case x402 of
                             {Cons y403 y404 -> do {guard (x403 == y403); return y404};
                              _ -> mzero};
                     x406 <- case x404 of
                             {Cons y405 y406 -> do {guard (x405 == y405); return y406};
                              _ -> mzero};
                     x408 <- case x406 of
                             {Cons y407 y408 -> do {guard (x407 == y407); return y408};
                              _ -> mzero};
                     x410 <- case x408 of
                             {Cons y409 y410 -> do {guard (x409 == y409); return y410};
                              _ -> mzero};
                     x412 <- case x410 of
                             {Cons y411 y412 -> do {guard (x411 == y411); return y412};
                              _ -> mzero};
                     x414 <- case x412 of
                             {Cons y413 y414 -> do {guard (x413 == y413); return y414};
                              _ -> mzero};
                     x416 <- case x414 of
                             {Cons y415 y416 -> do {guard (x415 == y415); return y416};
                              _ -> mzero};
                     x418 <- case x416 of
                             {Cons y417 y418 -> do {guard (x417 == y417); return y418};
                              _ -> mzero};
                     x420 <- case x418 of
                             {Cons y419 y420 -> do {guard (x419 == y419); return y420};
                              _ -> mzero};
                     x422 <- case x420 of
                             {Cons y421 y422 -> do {guard (x421 == y421); return y422};
                              _ -> mzero};
                     x18 <- case x422 of
                            {Cons y423 y18 -> do {guard (x423 == y423); return y18};
                             _ -> mzero};
                     ___evalI x18;
                     return ()},
                 do {let {x426 = Goat};
                     let {x428 = Term.Empty};
                     let {x430 = Cabbage};
                     let {x432 = Goat};
                     let {x434 = Wolf};
                     let {x436 = Wolf};
                     let {x438 = Term.Empty};
                     let {x440 = Cabbage};
                     let {x442 = Wolf};
                     let {x444 = Term.Empty};
                     let {x446 = Cabbage};
                     let {x448 = Wolf};
                     (x449, x450) <- case x0 of
                                     {Cons y449 y450 -> return (y449, y450); _ -> mzero};
                     guard (x449 == x426);
                     let {x427 = x450};
                     x429 <- case x427 of
                             {Cons y428 y429 -> do {guard (x428 == y428); return y429};
                              _ -> mzero};
                     x431 <- case x429 of
                             {Cons y430 y431 -> do {guard (x430 == y430); return y431};
                              _ -> mzero};
                     x433 <- case x431 of
                             {Cons y432 y433 -> do {guard (x432 == y432); return y433};
                              _ -> mzero};
                     x435 <- case x433 of
                             {Cons y434 y435 -> do {guard (x434 == y434); return y435};
                              _ -> mzero};
                     x437 <- case x435 of
                             {Cons y436 y437 -> do {guard (x436 == y436); return y437};
                              _ -> mzero};
                     x439 <- case x437 of
                             {Cons y438 y439 -> do {guard (x438 == y438); return y439};
                              _ -> mzero};
                     x441 <- case x439 of
                             {Cons y440 y441 -> do {guard (x440 == y440); return y441};
                              _ -> mzero};
                     x443 <- case x441 of
                             {Cons y442 y443 -> do {guard (x442 == y442); return y443};
                              _ -> mzero};
                     x445 <- case x443 of
                             {Cons y444 y445 -> do {guard (x444 == y444); return y445};
                              _ -> mzero};
                     x447 <- case x445 of
                             {Cons y446 y447 -> do {guard (x446 == y446); return y447};
                              _ -> mzero};
                     x19 <- case x447 of
                            {Cons y448 y19 -> do {guard (x448 == y448); return y19};
                             _ -> mzero};
                     swapEvalI x19;
                     return ()},
                 do {let {x451 = Goat};
                     let {x453 = Term.Empty};
                     let {x455 = Cabbage};
                     let {x457 = Goat};
                     let {x459 = Wolf};
                     let {x461 = Wolf};
                     let {x463 = Term.Empty};
                     let {x465 = Cabbage};
                     let {x467 = Wolf};
                     let {x469 = Term.Empty};
                     let {x471 = Cabbage};
                     let {x473 = Cabbage};
                     (x474, x475) <- case x0 of
                                     {Cons y474 y475 -> return (y474, y475); _ -> mzero};
                     guard (x474 == x451);
                     let {x452 = x475};
                     x454 <- case x452 of
                             {Cons y453 y454 -> do {guard (x453 == y453); return y454};
                              _ -> mzero};
                     x456 <- case x454 of
                             {Cons y455 y456 -> do {guard (x455 == y455); return y456};
                              _ -> mzero};
                     x458 <- case x456 of
                             {Cons y457 y458 -> do {guard (x457 == y457); return y458};
                              _ -> mzero};
                     x460 <- case x458 of
                             {Cons y459 y460 -> do {guard (x459 == y459); return y460};
                              _ -> mzero};
                     x462 <- case x460 of
                             {Cons y461 y462 -> do {guard (x461 == y461); return y462};
                              _ -> mzero};
                     x464 <- case x462 of
                             {Cons y463 y464 -> do {guard (x463 == y463); return y464};
                              _ -> mzero};
                     x466 <- case x464 of
                             {Cons y465 y466 -> do {guard (x465 == y465); return y466};
                              _ -> mzero};
                     x468 <- case x466 of
                             {Cons y467 y468 -> do {guard (x467 == y467); return y468};
                              _ -> mzero};
                     x470 <- case x468 of
                             {Cons y469 y470 -> do {guard (x469 == y469); return y470};
                              _ -> mzero};
                     x472 <- case x470 of
                             {Cons y471 y472 -> do {guard (x471 == y471); return y472};
                              _ -> mzero};
                     x19 <- case x472 of
                            {Cons y473 y19 -> do {guard (x473 == y473); return y19};
                             _ -> mzero};
                     __swapEvalI x19;
                     return ()},
                 do {let {x476 = Goat};
                     let {x478 = Term.Empty};
                     let {x480 = Cabbage};
                     let {x482 = Goat};
                     let {x484 = Wolf};
                     let {x486 = Wolf};
                     let {x488 = Term.Empty};
                     let {x490 = Cabbage};
                     let {x492 = Wolf};
                     let {x494 = Wolf};
                     (x495, x496) <- case x0 of
                                     {Cons y495 y496 -> return (y495, y496); _ -> mzero};
                     guard (x495 == x476);
                     let {x477 = x496};
                     x479 <- case x477 of
                             {Cons y478 y479 -> do {guard (x478 == y478); return y479};
                              _ -> mzero};
                     x481 <- case x479 of
                             {Cons y480 y481 -> do {guard (x480 == y480); return y481};
                              _ -> mzero};
                     x483 <- case x481 of
                             {Cons y482 y483 -> do {guard (x482 == y482); return y483};
                              _ -> mzero};
                     x485 <- case x483 of
                             {Cons y484 y485 -> do {guard (x484 == y484); return y485};
                              _ -> mzero};
                     x487 <- case x485 of
                             {Cons y486 y487 -> do {guard (x486 == y486); return y487};
                              _ -> mzero};
                     x489 <- case x487 of
                             {Cons y488 y489 -> do {guard (x488 == y488); return y489};
                              _ -> mzero};
                     x491 <- case x489 of
                             {Cons y490 y491 -> do {guard (x490 == y490); return y491};
                              _ -> mzero};
                     x493 <- case x491 of
                             {Cons y492 y493 -> do {guard (x492 == y492); return y493};
                              _ -> mzero};
                     x20 <- case x493 of
                            {Cons y494 y20 -> do {guard (x494 == y494); return y20};
                             _ -> mzero};
                     _swapEvalI x20;
                     return ()},
                 do {let {x497 = Goat};
                     let {x499 = Term.Empty};
                     let {x501 = Cabbage};
                     let {x503 = Goat};
                     let {x505 = Wolf};
                     let {x507 = Wolf};
                     let {x509 = Term.Empty};
                     let {x511 = Cabbage};
                     let {x513 = Cabbage};
                     (x514, x515) <- case x0 of
                                     {Cons y514 y515 -> return (y514, y515); _ -> mzero};
                     guard (x514 == x497);
                     let {x498 = x515};
                     x500 <- case x498 of
                             {Cons y499 y500 -> do {guard (x499 == y499); return y500};
                              _ -> mzero};
                     x502 <- case x500 of
                             {Cons y501 y502 -> do {guard (x501 == y501); return y502};
                              _ -> mzero};
                     x504 <- case x502 of
                             {Cons y503 y504 -> do {guard (x503 == y503); return y504};
                              _ -> mzero};
                     x506 <- case x504 of
                             {Cons y505 y506 -> do {guard (x505 == y505); return y506};
                              _ -> mzero};
                     x508 <- case x506 of
                             {Cons y507 y508 -> do {guard (x507 == y507); return y508};
                              _ -> mzero};
                     x510 <- case x508 of
                             {Cons y509 y510 -> do {guard (x509 == y509); return y510};
                              _ -> mzero};
                     x512 <- case x510 of
                             {Cons y511 y512 -> do {guard (x511 == y511); return y512};
                              _ -> mzero};
                     x21 <- case x512 of
                            {Cons y513 y21 -> do {guard (x513 == y513); return y21};
                             _ -> mzero};
                     ____evalI x21;
                     return ()},
                 do {let {x516 = Goat};
                     let {x518 = Term.Empty};
                     let {x520 = Cabbage};
                     let {x522 = Goat};
                     let {x524 = Wolf};
                     let {x526 = Wolf};
                     let {x528 = Wolf};
                     let {x530 = Term.Empty};
                     let {x532 = Term.Empty};
                     (x533, x534) <- case x0 of
                                     {Cons y533 y534 -> return (y533, y534); _ -> mzero};
                     guard (x533 == x516);
                     let {x517 = x534};
                     x519 <- case x517 of
                             {Cons y518 y519 -> do {guard (x518 == y518); return y519};
                              _ -> mzero};
                     x521 <- case x519 of
                             {Cons y520 y521 -> do {guard (x520 == y520); return y521};
                              _ -> mzero};
                     x523 <- case x521 of
                             {Cons y522 y523 -> do {guard (x522 == y522); return y523};
                              _ -> mzero};
                     x525 <- case x523 of
                             {Cons y524 y525 -> do {guard (x524 == y524); return y525};
                              _ -> mzero};
                     x527 <- case x525 of
                             {Cons y526 y527 -> do {guard (x526 == y526); return y527};
                              _ -> mzero};
                     x529 <- case x527 of
                             {Cons y528 y529 -> do {guard (x528 == y528); return y529};
                              _ -> mzero};
                     x531 <- case x529 of
                             {Cons y530 y531 -> do {guard (x530 == y530); return y531};
                              _ -> mzero};
                     x21 <- case x531 of
                            {Cons y532 y21 -> do {guard (x532 == y532); return y21};
                             _ -> mzero};
                     ___evalI x21;
                     return ()},
                 do {let {x535 = Goat};
                     let {x537 = Term.Empty};
                     let {x539 = Cabbage};
                     let {x541 = Goat};
                     let {x543 = Wolf};
                     let {x545 = Wolf};
                     let {x547 = Wolf};
                     let {x549 = Wolf};
                     (x550, x551) <- case x0 of
                                     {Cons y550 y551 -> return (y550, y551); _ -> mzero};
                     guard (x550 == x535);
                     let {x536 = x551};
                     x538 <- case x536 of
                             {Cons y537 y538 -> do {guard (x537 == y537); return y538};
                              _ -> mzero};
                     x540 <- case x538 of
                             {Cons y539 y540 -> do {guard (x539 == y539); return y540};
                              _ -> mzero};
                     x542 <- case x540 of
                             {Cons y541 y542 -> do {guard (x541 == y541); return y542};
                              _ -> mzero};
                     x544 <- case x542 of
                             {Cons y543 y544 -> do {guard (x543 == y543); return y544};
                              _ -> mzero};
                     x546 <- case x544 of
                             {Cons y545 y546 -> do {guard (x545 == y545); return y546};
                              _ -> mzero};
                     x548 <- case x546 of
                             {Cons y547 y548 -> do {guard (x547 == y547); return y548};
                              _ -> mzero};
                     x22 <- case x548 of
                            {Cons y549 y22 -> do {guard (x549 == y549); return y22};
                             _ -> mzero};
                     swapEvalI x22;
                     return ()},
                 do {let {x552 = Goat};
                     let {x554 = Term.Empty};
                     let {x556 = Cabbage};
                     let {x558 = Goat};
                     let {x560 = Wolf};
                     let {x562 = Wolf};
                     let {x564 = Wolf};
                     let {x566 = Cabbage};
                     let {x568 = Term.Empty};
                     let {x570 = Term.Empty};
                     (x571, x572) <- case x0 of
                                     {Cons y571 y572 -> return (y571, y572); _ -> mzero};
                     guard (x571 == x552);
                     let {x553 = x572};
                     x555 <- case x553 of
                             {Cons y554 y555 -> do {guard (x554 == y554); return y555};
                              _ -> mzero};
                     x557 <- case x555 of
                             {Cons y556 y557 -> do {guard (x556 == y556); return y557};
                              _ -> mzero};
                     x559 <- case x557 of
                             {Cons y558 y559 -> do {guard (x558 == y558); return y559};
                              _ -> mzero};
                     x561 <- case x559 of
                             {Cons y560 y561 -> do {guard (x560 == y560); return y561};
                              _ -> mzero};
                     x563 <- case x561 of
                             {Cons y562 y563 -> do {guard (x562 == y562); return y563};
                              _ -> mzero};
                     x565 <- case x563 of
                             {Cons y564 y565 -> do {guard (x564 == y564); return y565};
                              _ -> mzero};
                     x567 <- case x565 of
                             {Cons y566 y567 -> do {guard (x566 == y566); return y567};
                              _ -> mzero};
                     x569 <- case x567 of
                             {Cons y568 y569 -> do {guard (x568 == y568); return y569};
                              _ -> mzero};
                     x20 <- case x569 of
                            {Cons y570 y20 -> do {guard (x570 == y570); return y20};
                             _ -> mzero};
                     __swapEvalI x20;
                     return ()},
                 do {let {x573 = Goat};
                     let {x575 = Term.Empty};
                     let {x577 = Cabbage};
                     let {x579 = Goat};
                     let {x581 = Wolf};
                     let {x583 = Wolf};
                     let {x585 = Wolf};
                     let {x587 = Cabbage};
                     let {x589 = Term.Empty};
                     let {x591 = Wolf};
                     let {x593 = Term.Empty};
                     let {x595 = Term.Empty};
                     (x596, x597) <- case x0 of
                                     {Cons y596 y597 -> return (y596, y597); _ -> mzero};
                     guard (x596 == x573);
                     let {x574 = x597};
                     x576 <- case x574 of
                             {Cons y575 y576 -> do {guard (x575 == y575); return y576};
                              _ -> mzero};
                     x578 <- case x576 of
                             {Cons y577 y578 -> do {guard (x577 == y577); return y578};
                              _ -> mzero};
                     x580 <- case x578 of
                             {Cons y579 y580 -> do {guard (x579 == y579); return y580};
                              _ -> mzero};
                     x582 <- case x580 of
                             {Cons y581 y582 -> do {guard (x581 == y581); return y582};
                              _ -> mzero};
                     x584 <- case x582 of
                             {Cons y583 y584 -> do {guard (x583 == y583); return y584};
                              _ -> mzero};
                     x586 <- case x584 of
                             {Cons y585 y586 -> do {guard (x585 == y585); return y586};
                              _ -> mzero};
                     x588 <- case x586 of
                             {Cons y587 y588 -> do {guard (x587 == y587); return y588};
                              _ -> mzero};
                     x590 <- case x588 of
                             {Cons y589 y590 -> do {guard (x589 == y589); return y590};
                              _ -> mzero};
                     x592 <- case x590 of
                             {Cons y591 y592 -> do {guard (x591 == y591); return y592};
                              _ -> mzero};
                     x594 <- case x592 of
                             {Cons y593 y594 -> do {guard (x593 == y593); return y594};
                              _ -> mzero};
                     x19 <- case x594 of
                            {Cons y595 y19 -> do {guard (x595 == y595); return y19};
                             _ -> mzero};
                     _swapEvalI x19;
                     return ()},
                 do {let {x598 = Goat};
                     let {x600 = Term.Empty};
                     let {x602 = Cabbage};
                     let {x604 = Goat};
                     let {x606 = Wolf};
                     let {x608 = Wolf};
                     let {x610 = Wolf};
                     let {x612 = Cabbage};
                     let {x614 = Term.Empty};
                     let {x616 = Wolf};
                     let {x618 = Wolf};
                     (x619, x620) <- case x0 of
                                     {Cons y619 y620 -> return (y619, y620); _ -> mzero};
                     guard (x619 == x598);
                     let {x599 = x620};
                     x601 <- case x599 of
                             {Cons y600 y601 -> do {guard (x600 == y600); return y601};
                              _ -> mzero};
                     x603 <- case x601 of
                             {Cons y602 y603 -> do {guard (x602 == y602); return y603};
                              _ -> mzero};
                     x605 <- case x603 of
                             {Cons y604 y605 -> do {guard (x604 == y604); return y605};
                              _ -> mzero};
                     x607 <- case x605 of
                             {Cons y606 y607 -> do {guard (x606 == y606); return y607};
                              _ -> mzero};
                     x609 <- case x607 of
                             {Cons y608 y609 -> do {guard (x608 == y608); return y609};
                              _ -> mzero};
                     x611 <- case x609 of
                             {Cons y610 y611 -> do {guard (x610 == y610); return y611};
                              _ -> mzero};
                     x613 <- case x611 of
                             {Cons y612 y613 -> do {guard (x612 == y612); return y613};
                              _ -> mzero};
                     x615 <- case x613 of
                             {Cons y614 y615 -> do {guard (x614 == y614); return y615};
                              _ -> mzero};
                     x617 <- case x615 of
                             {Cons y616 y617 -> do {guard (x616 == y616); return y617};
                              _ -> mzero};
                     x23 <- case x617 of
                            {Cons y618 y23 -> do {guard (x618 == y618); return y23};
                             _ -> mzero};
                     __evalI x23;
                     return ()},
                 do {let {x621 = Goat};
                     let {x623 = Term.Empty};
                     let {x625 = Cabbage};
                     let {x627 = Goat};
                     let {x629 = Wolf};
                     let {x631 = Wolf};
                     let {x633 = Wolf};
                     let {x635 = Cabbage};
                     let {x637 = Term.Empty};
                     let {x639 = Wolf};
                     let {x641 = Cabbage};
                     let {x643 = Term.Empty};
                     (x644, x645) <- case x0 of
                                     {Cons y644 y645 -> return (y644, y645); _ -> mzero};
                     guard (x644 == x621);
                     let {x622 = x645};
                     x624 <- case x622 of
                             {Cons y623 y624 -> do {guard (x623 == y623); return y624};
                              _ -> mzero};
                     x626 <- case x624 of
                             {Cons y625 y626 -> do {guard (x625 == y625); return y626};
                              _ -> mzero};
                     x628 <- case x626 of
                             {Cons y627 y628 -> do {guard (x627 == y627); return y628};
                              _ -> mzero};
                     x630 <- case x628 of
                             {Cons y629 y630 -> do {guard (x629 == y629); return y630};
                              _ -> mzero};
                     x632 <- case x630 of
                             {Cons y631 y632 -> do {guard (x631 == y631); return y632};
                              _ -> mzero};
                     x634 <- case x632 of
                             {Cons y633 y634 -> do {guard (x633 == y633); return y634};
                              _ -> mzero};
                     x636 <- case x634 of
                             {Cons y635 y636 -> do {guard (x635 == y635); return y636};
                              _ -> mzero};
                     x638 <- case x636 of
                             {Cons y637 y638 -> do {guard (x637 == y637); return y638};
                              _ -> mzero};
                     x640 <- case x638 of
                             {Cons y639 y640 -> do {guard (x639 == y639); return y640};
                              _ -> mzero};
                     x642 <- case x640 of
                             {Cons y641 y642 -> do {guard (x641 == y641); return y642};
                              _ -> mzero};
                     x24 <- case x642 of
                            {Cons y643 y24 -> do {guard (x643 == y643); return y24};
                             _ -> mzero};
                     swapEvalI x24;
                     return ()},
                 do {let {x646 = Goat};
                     let {x648 = Term.Empty};
                     let {x650 = Cabbage};
                     let {x652 = Goat};
                     let {x654 = Wolf};
                     let {x656 = Wolf};
                     let {x658 = Wolf};
                     let {x660 = Cabbage};
                     let {x662 = Term.Empty};
                     let {x664 = Wolf};
                     let {x666 = Cabbage};
                     let {x668 = Cabbage};
                     (x669, x670) <- case x0 of
                                     {Cons y669 y670 -> return (y669, y670); _ -> mzero};
                     guard (x669 == x646);
                     let {x647 = x670};
                     x649 <- case x647 of
                             {Cons y648 y649 -> do {guard (x648 == y648); return y649};
                              _ -> mzero};
                     x651 <- case x649 of
                             {Cons y650 y651 -> do {guard (x650 == y650); return y651};
                              _ -> mzero};
                     x653 <- case x651 of
                             {Cons y652 y653 -> do {guard (x652 == y652); return y653};
                              _ -> mzero};
                     x655 <- case x653 of
                             {Cons y654 y655 -> do {guard (x654 == y654); return y655};
                              _ -> mzero};
                     x657 <- case x655 of
                             {Cons y656 y657 -> do {guard (x656 == y656); return y657};
                              _ -> mzero};
                     x659 <- case x657 of
                             {Cons y658 y659 -> do {guard (x658 == y658); return y659};
                              _ -> mzero};
                     x661 <- case x659 of
                             {Cons y660 y661 -> do {guard (x660 == y660); return y661};
                              _ -> mzero};
                     x663 <- case x661 of
                             {Cons y662 y663 -> do {guard (x662 == y662); return y663};
                              _ -> mzero};
                     x665 <- case x663 of
                             {Cons y664 y665 -> do {guard (x664 == y664); return y665};
                              _ -> mzero};
                     x667 <- case x665 of
                             {Cons y666 y667 -> do {guard (x666 == y666); return y667};
                              _ -> mzero};
                     x24 <- case x667 of
                            {Cons y668 y24 -> do {guard (x668 == y668); return y24};
                             _ -> mzero};
                     _swapEvalI x24;
                     return ()},
                 do {let {x671 = Goat};
                     let {x673 = Term.Empty};
                     let {x675 = Cabbage};
                     let {x677 = Goat};
                     let {x679 = Wolf};
                     let {x681 = Wolf};
                     let {x683 = Wolf};
                     let {x685 = Cabbage};
                     let {x687 = Cabbage};
                     (x688, x689) <- case x0 of
                                     {Cons y688 y689 -> return (y688, y689); _ -> mzero};
                     guard (x688 == x671);
                     let {x672 = x689};
                     x674 <- case x672 of
                             {Cons y673 y674 -> do {guard (x673 == y673); return y674};
                              _ -> mzero};
                     x676 <- case x674 of
                             {Cons y675 y676 -> do {guard (x675 == y675); return y676};
                              _ -> mzero};
                     x678 <- case x676 of
                             {Cons y677 y678 -> do {guard (x677 == y677); return y678};
                              _ -> mzero};
                     x680 <- case x678 of
                             {Cons y679 y680 -> do {guard (x679 == y679); return y680};
                              _ -> mzero};
                     x682 <- case x680 of
                             {Cons y681 y682 -> do {guard (x681 == y681); return y682};
                              _ -> mzero};
                     x684 <- case x682 of
                             {Cons y683 y684 -> do {guard (x683 == y683); return y684};
                              _ -> mzero};
                     x686 <- case x684 of
                             {Cons y685 y686 -> do {guard (x685 == y685); return y686};
                              _ -> mzero};
                     x25 <- case x686 of
                            {Cons y687 y25 -> do {guard (x687 == y687); return y25};
                             _ -> mzero};
                     ___evalI x25;
                     return ()},
                 do {let {x690 = Goat};
                     let {x692 = Term.Empty};
                     let {x694 = Cabbage};
                     let {x696 = Goat};
                     let {x698 = Wolf};
                     let {x700 = Cabbage};
                     let {x702 = Goat};
                     let {x704 = Goat};
                     (x705, x706) <- case x0 of
                                     {Cons y705 y706 -> return (y705, y706); _ -> mzero};
                     guard (x705 == x690);
                     let {x691 = x706};
                     x693 <- case x691 of
                             {Cons y692 y693 -> do {guard (x692 == y692); return y693};
                              _ -> mzero};
                     x695 <- case x693 of
                             {Cons y694 y695 -> do {guard (x694 == y694); return y695};
                              _ -> mzero};
                     x697 <- case x695 of
                             {Cons y696 y697 -> do {guard (x696 == y696); return y697};
                              _ -> mzero};
                     x699 <- case x697 of
                             {Cons y698 y699 -> do {guard (x698 == y698); return y699};
                              _ -> mzero};
                     x701 <- case x699 of
                             {Cons y700 y701 -> do {guard (x700 == y700); return y701};
                              _ -> mzero};
                     x703 <- case x701 of
                             {Cons y702 y703 -> do {guard (x702 == y702); return y703};
                              _ -> mzero};
                     x22 <- case x703 of
                            {Cons y704 y22 -> do {guard (x704 == y704); return y22};
                             _ -> mzero};
                     ___swapEvalI x22;
                     return ()},
                 do {let {x707 = Goat};
                     let {x709 = Term.Empty};
                     let {x711 = Cabbage};
                     let {x713 = Goat};
                     let {x715 = Wolf};
                     let {x717 = Cabbage};
                     let {x719 = Goat};
                     let {x721 = Wolf};
                     (x722, x723) <- case x0 of
                                     {Cons y722 y723 -> return (y722, y723); _ -> mzero};
                     guard (x722 == x707);
                     let {x708 = x723};
                     x710 <- case x708 of
                             {Cons y709 y710 -> do {guard (x709 == y709); return y710};
                              _ -> mzero};
                     x712 <- case x710 of
                             {Cons y711 y712 -> do {guard (x711 == y711); return y712};
                              _ -> mzero};
                     x714 <- case x712 of
                             {Cons y713 y714 -> do {guard (x713 == y713); return y714};
                              _ -> mzero};
                     x716 <- case x714 of
                             {Cons y715 y716 -> do {guard (x715 == y715); return y716};
                              _ -> mzero};
                     x718 <- case x716 of
                             {Cons y717 y718 -> do {guard (x717 == y717); return y718};
                              _ -> mzero};
                     x720 <- case x718 of
                             {Cons y719 y720 -> do {guard (x719 == y719); return y720};
                              _ -> mzero};
                     x22 <- case x720 of
                            {Cons y721 y22 -> do {guard (x721 == y721); return y22};
                             _ -> mzero};
                     ____swapEvalI x22;
                     return ()},
                 do {let {x724 = Goat};
                     let {x726 = Term.Empty};
                     let {x728 = Cabbage};
                     let {x730 = Goat};
                     let {x732 = Wolf};
                     let {x734 = Cabbage};
                     let {x736 = Cabbage};
                     (x737, x738) <- case x0 of
                                     {Cons y737 y738 -> return (y737, y738); _ -> mzero};
                     guard (x737 == x724);
                     let {x725 = x738};
                     x727 <- case x725 of
                             {Cons y726 y727 -> do {guard (x726 == y726); return y727};
                              _ -> mzero};
                     x729 <- case x727 of
                             {Cons y728 y729 -> do {guard (x728 == y728); return y729};
                              _ -> mzero};
                     x731 <- case x729 of
                             {Cons y730 y731 -> do {guard (x730 == y730); return y731};
                              _ -> mzero};
                     x733 <- case x731 of
                             {Cons y732 y733 -> do {guard (x732 == y732); return y733};
                              _ -> mzero};
                     x735 <- case x733 of
                             {Cons y734 y735 -> do {guard (x734 == y734); return y735};
                              _ -> mzero};
                     x26 <- case x735 of
                            {Cons y736 y26 -> do {guard (x736 == y736); return y26};
                             _ -> mzero};
                     ______evalI x26;
                     return ()},
                 do {let {x739 = Goat};
                     let {x741 = Term.Empty};
                     let {x743 = Cabbage};
                     let {x745 = Cabbage};
                     (x746, x747) <- case x0 of
                                     {Cons y746 y747 -> return (y746, y747); _ -> mzero};
                     guard (x746 == x739);
                     let {x740 = x747};
                     x742 <- case x740 of
                             {Cons y741 y742 -> do {guard (x741 == y741); return y742};
                              _ -> mzero};
                     x744 <- case x742 of
                             {Cons y743 y744 -> do {guard (x743 == y743); return y744};
                              _ -> mzero};
                     x27 <- case x744 of
                            {Cons y745 y27 -> do {guard (x745 == y745); return y27};
                             _ -> mzero};
                     ____swapEvalI x27;
                     return ()},
                 do {let {x748 = Goat};
                     let {x750 = Goat};
                     (x751, x752) <- case x0 of
                                     {Cons y751 y752 -> return (y751, y752); _ -> mzero};
                     guard (x751 == x748);
                     let {x749 = x752};
                     x28 <- case x749 of
                            {Cons y750 y28 -> do {guard (x750 == y750); return y28};
                             _ -> mzero};
                     evalI x28;
                     return ()}]
_______evalI x0 = Immature $ msum [do {guard (x0 == Nil); return ()},
                        do {let {x817 = Term.Empty};
                            let {x819 = Term.Empty};
                            (x820, x821) <- case x0 of
                                            {Cons y820 y821 -> return (y820, y821); _ -> mzero};
                            guard (x820 == x817);
                            let {x818 = x821};
                            x1 <- case x818 of
                                  {Cons y819 y1 -> do {guard (x819 == y819); return y1};
                                   _ -> mzero};
                            _______evalI x1;
                            return ()},
                        do {let {x822 = Goat};
                            let {x824 = Term.Empty};
                            (x825, x826) <- case x0 of
                                            {Cons y825 y826 -> return (y825, y826); _ -> mzero};
                            guard (x825 == x822);
                            let {x823 = x826};
                            x2 <- case x823 of
                                  {Cons y824 y2 -> do {guard (x824 == y824); return y2};
                                   _ -> mzero};
                            ______evalI x2;
                            return ()},
                        do {let {x827 = Goat};
                            let {x829 = Goat};
                            (x830, x831) <- case x0 of
                                            {Cons y830 y831 -> return (y830, y831); _ -> mzero};
                            guard (x830 == x827);
                            let {x828 = x831};
                            x2 <- case x828 of
                                  {Cons y829 y2 -> do {guard (x829 == y829); return y2};
                                   _ -> mzero};
                            _______evalI x2;
                            return ()}]
______evalI x0 = Immature $ msum [do {let {x803 = Term.Empty};
                           let {x805 = Term.Empty};
                           (x806, x807) <- case x0 of
                                           {Cons y806 y807 -> return (y806, y807); _ -> mzero};
                           guard (x806 == x803);
                           let {x804 = x807};
                           x1 <- case x804 of
                                 {Cons y805 y1 -> do {guard (x805 == y805); return y1}; _ -> mzero};
                           ______evalI x1;
                           return ()},
                       do {let {x808 = Term.Empty};
                           let {x810 = Goat};
                           (x811, x812) <- case x0 of
                                           {Cons y811 y812 -> return (y811, y812); _ -> mzero};
                           guard (x811 == x808);
                           let {x809 = x812};
                           x1 <- case x809 of
                                 {Cons y810 y1 -> do {guard (x810 == y810); return y1}; _ -> mzero};
                           _______evalI x1;
                           return ()},
                       do {let {x813 = Wolf};
                           (x814, x2) <- case x0 of
                                         {Cons y814 y2 -> return (y814, y2); _ -> mzero};
                           guard (x814 == x813);
                           swapEvalI x2;
                           return ()},
                       do {let {x815 = Cabbage};
                           (x816, x2) <- case x0 of
                                         {Cons y816 y2 -> return (y816, y2); _ -> mzero};
                           guard (x816 == x815);
                           ___swapEvalI x2;
                           return ()}]
_____evalI x0 = Immature $ msum [do {let {x791 = Goat};
                          let {x793 = Goat};
                          (x794, x795) <- case x0 of
                                          {Cons y794 y795 -> return (y794, y795); _ -> mzero};
                          guard (x794 == x791);
                          let {x792 = x795};
                          x1 <- case x792 of
                                {Cons y793 y1 -> do {guard (x793 == y793); return y1}; _ -> mzero};
                          _____evalI x1;
                          return ()},
                      do {let {x796 = Goat};
                          let {x798 = Wolf};
                          (x799, x800) <- case x0 of
                                          {Cons y799 y800 -> return (y799, y800); _ -> mzero};
                          guard (x799 == x796);
                          let {x797 = x800};
                          x1 <- case x797 of
                                {Cons y798 y1 -> do {guard (x798 == y798); return y1}; _ -> mzero};
                          ______evalI x1;
                          return ()},
                      do {let {x801 = Cabbage};
                          (x802, x2) <- case x0 of
                                        {Cons y802 y2 -> return (y802, y2); _ -> mzero};
                          guard (x802 == x801);
                          ____swapEvalI x2;
                          return ()}]
____evalI x0 = Immature $ msum [do {let {x783 = Term.Empty};
                         (x784, x1) <- case x0 of
                                       {Cons y784 y1 -> return (y784, y1); _ -> mzero};
                         guard (x784 == x783);
                         swapEvalI x1;
                         return ()},
                     do {let {x785 = Cabbage};
                         (x786, x1) <- case x0 of
                                       {Cons y786 y1 -> return (y786, y1); _ -> mzero};
                         guard (x786 == x785);
                         _swapEvalI x1;
                         return ()}]
____swapEvalI x0 = Immature $ msum [do {let {x844 = Term.Empty};
                             (x845, x1) <- case x0 of
                                           {Cons y845 y1 -> return (y845, y1); _ -> mzero};
                             guard (x845 == x844);
                             _evalI x1;
                             return ()},
                         do {let {x846 = Wolf};
                             (x847, x1) <- case x0 of
                                           {Cons y847 y1 -> return (y847, y1); _ -> mzero};
                             guard (x847 == x846);
                             __evalI x1;
                             return ()},
                         do {let {x848 = Cabbage};
                             (x849, x1) <- case x0 of
                                           {Cons y849 y1 -> return (y849, y1); _ -> mzero};
                             guard (x849 == x848);
                             _____evalI x1;
                             return ()}]
___evalI x0 = Immature $ msum [do {let {x761 = Term.Empty};
                        let {x763 = Term.Empty};
                        (x764, x765) <- case x0 of
                                        {Cons y764 y765 -> return (y764, y765); _ -> mzero};
                        guard (x764 == x761);
                        let {x762 = x765};
                        x1 <- case x762 of
                              {Cons y763 y1 -> do {guard (x763 == y763); return y1}; _ -> mzero};
                        ___evalI x1;
                        return ()},
                    do {let {x766 = Wolf};
                        (x767, x2) <- case x0 of
                                      {Cons y767 y2 -> return (y767, y2); _ -> mzero};
                        guard (x767 == x766);
                        swapEvalI x2;
                        return ()},
                    do {let {x768 = Cabbage};
                        (x769, x2) <- case x0 of
                                      {Cons y769 y2 -> return (y769, y2); _ -> mzero};
                        guard (x769 == x768);
                        __swapEvalI x2;
                        return ()}]
___swapEvalI x0 = Immature $ msum [do {let {x832 = Goat};
                            let {x834 = Goat};
                            (x835, x836) <- case x0 of
                                            {Cons y835 y836 -> return (y835, y836); _ -> mzero};
                            guard (x835 == x832);
                            let {x833 = x836};
                            x1 <- case x833 of
                                  {Cons y834 y1 -> do {guard (x834 == y834); return y1};
                                   _ -> mzero};
                            ___swapEvalI x1;
                            return ()},
                        do {let {x837 = Goat};
                            let {x839 = Wolf};
                            (x840, x841) <- case x0 of
                                            {Cons y840 y841 -> return (y840, y841); _ -> mzero};
                            guard (x840 == x837);
                            let {x838 = x841};
                            x1 <- case x838 of
                                  {Cons y839 y1 -> do {guard (x839 == y839); return y1};
                                   _ -> mzero};
                            ____swapEvalI x1;
                            return ()},
                        do {let {x842 = Cabbage};
                            (x843, x2) <- case x0 of
                                          {Cons y843 y2 -> return (y843, y2); _ -> mzero};
                            guard (x843 == x842);
                            ______evalI x2;
                            return ()}]
__evalI x0 = Immature $ msum [do {let {x757 = Term.Empty};
                       (x758, x1) <- case x0 of
                                     {Cons y758 y1 -> return (y758, y1); _ -> mzero};
                       guard (x758 == x757);
                       __swapEvalI x1;
                       return ()},
                   do {let {x759 = Wolf};
                       (x760, x1) <- case x0 of
                                     {Cons y760 y1 -> return (y760, y1); _ -> mzero};
                       guard (x760 == x759);
                       _swapEvalI x1;
                       return ()}]
__swapEvalI x0 = Immature $ msum [do {let {x787 = Term.Empty};
                           (x788, x1) <- case x0 of
                                         {Cons y788 y1 -> return (y788, y1); _ -> mzero};
                           guard (x788 == x787);
                           __evalI x1;
                           return ()},
                       do {let {x789 = Cabbage};
                           (x790, x1) <- case x0 of
                                         {Cons y790 y1 -> return (y790, y1); _ -> mzero};
                           guard (x790 == x789);
                           ___evalI x1;
                           return ()}]
_evalI x0 = Immature $ msum [do {let {x753 = Term.Empty};
                      (x754, x1) <- case x0 of
                                    {Cons y754 y1 -> return (y754, y1); _ -> mzero};
                      guard (x754 == x753);
                      ____swapEvalI x1;
                      return ()},
                  do {let {x755 = Goat};
                      (x756, x1) <- case x0 of
                                    {Cons y756 y1 -> return (y756, y1); _ -> mzero};
                      guard (x756 == x755);
                      evalI x1;
                      return ()}]
_swapEvalI x0 = Immature $ msum [do {let {x774 = Term.Empty};
                          let {x776 = Term.Empty};
                          (x777, x778) <- case x0 of
                                          {Cons y777 y778 -> return (y777, y778); _ -> mzero};
                          guard (x777 == x774);
                          let {x775 = x778};
                          x1 <- case x775 of
                                {Cons y776 y1 -> do {guard (x776 == y776); return y1}; _ -> mzero};
                          _swapEvalI x1;
                          return ()},
                      do {let {x779 = Wolf};
                          (x780, x2) <- case x0 of
                                        {Cons y780 y2 -> return (y780, y2); _ -> mzero};
                          guard (x780 == x779);
                          __evalI x2;
                          return ()},
                      do {let {x781 = Cabbage};
                          (x782, x2) <- case x0 of
                                        {Cons y782 y2 -> return (y782, y2); _ -> mzero};
                          guard (x782 == x781);
                          ____evalI x2;
                          return ()}]
evalO = Immature $ msum [do {let {x29 = Term.Empty};
                  let {x31 = Term.Empty};
                  let {x32 = x29};
                  x1 <- evalO;
                  let {x30 = Cons x31 x1};
                  let {x33 = x30};
                  let {x0 = Cons x32 x33};
                  return x0},
              do {let {x34 = Goat};
                  let {x36 = Term.Empty};
                  let {x38 = Term.Empty};
                  let {x39 = x34};
                  x2 <- _evalO;
                  let {x37 = Cons x38 x2};
                  let {x35 = Cons x36 x37};
                  let {x40 = x35};
                  let {x0 = Cons x39 x40};
                  return x0},
              do {let {x41 = Goat};
                  let {x43 = Term.Empty};
                  let {x45 = Wolf};
                  let {x47 = Term.Empty};
                  let {x49 = Term.Empty};
                  let {x50 = x41};
                  x3 <- __evalO;
                  let {x48 = Cons x49 x3};
                  let {x46 = Cons x47 x48};
                  let {x44 = Cons x45 x46};
                  let {x42 = Cons x43 x44};
                  let {x51 = x42};
                  let {x0 = Cons x50 x51};
                  return x0},
              do {let {x52 = Goat};
                  let {x54 = Term.Empty};
                  let {x56 = Wolf};
                  let {x58 = Term.Empty};
                  let {x60 = Cabbage};
                  let {x62 = Term.Empty};
                  let {x64 = Term.Empty};
                  let {x65 = x52};
                  x4 <- ___evalO;
                  let {x63 = Cons x64 x4};
                  let {x61 = Cons x62 x63};
                  let {x59 = Cons x60 x61};
                  let {x57 = Cons x58 x59};
                  let {x55 = Cons x56 x57};
                  let {x53 = Cons x54 x55};
                  let {x66 = x53};
                  let {x0 = Cons x65 x66};
                  return x0},
              do {let {x67 = Goat};
                  let {x69 = Term.Empty};
                  let {x71 = Wolf};
                  let {x73 = Term.Empty};
                  let {x75 = Cabbage};
                  let {x77 = Wolf};
                  let {x79 = Term.Empty};
                  let {x81 = Term.Empty};
                  let {x82 = x67};
                  x5 <- swapEvalO;
                  let {x80 = Cons x81 x5};
                  let {x78 = Cons x79 x80};
                  let {x76 = Cons x77 x78};
                  let {x74 = Cons x75 x76};
                  let {x72 = Cons x73 x74};
                  let {x70 = Cons x71 x72};
                  let {x68 = Cons x69 x70};
                  let {x83 = x68};
                  let {x0 = Cons x82 x83};
                  return x0},
              do {let {x84 = Goat};
                  let {x86 = Term.Empty};
                  let {x88 = Wolf};
                  let {x90 = Term.Empty};
                  let {x92 = Cabbage};
                  let {x94 = Wolf};
                  let {x96 = Term.Empty};
                  let {x98 = Cabbage};
                  let {x100 = Term.Empty};
                  let {x102 = Term.Empty};
                  let {x103 = x84};
                  x6 <- _swapEvalO;
                  let {x101 = Cons x102 x6};
                  let {x99 = Cons x100 x101};
                  let {x97 = Cons x98 x99};
                  let {x95 = Cons x96 x97};
                  let {x93 = Cons x94 x95};
                  let {x91 = Cons x92 x93};
                  let {x89 = Cons x90 x91};
                  let {x87 = Cons x88 x89};
                  let {x85 = Cons x86 x87};
                  let {x104 = x85};
                  let {x0 = Cons x103 x104};
                  return x0},
              do {let {x105 = Goat};
                  let {x107 = Term.Empty};
                  let {x109 = Wolf};
                  let {x111 = Term.Empty};
                  let {x113 = Cabbage};
                  let {x115 = Wolf};
                  let {x117 = Term.Empty};
                  let {x119 = Cabbage};
                  let {x121 = Wolf};
                  let {x122 = x105};
                  x7 <- __evalO;
                  let {x120 = Cons x121 x7};
                  let {x118 = Cons x119 x120};
                  let {x116 = Cons x117 x118};
                  let {x114 = Cons x115 x116};
                  let {x112 = Cons x113 x114};
                  let {x110 = Cons x111 x112};
                  let {x108 = Cons x109 x110};
                  let {x106 = Cons x107 x108};
                  let {x123 = x106};
                  let {x0 = Cons x122 x123};
                  return x0},
              do {let {x124 = Goat};
                  let {x126 = Term.Empty};
                  let {x128 = Wolf};
                  let {x130 = Term.Empty};
                  let {x132 = Cabbage};
                  let {x134 = Wolf};
                  let {x136 = Term.Empty};
                  let {x138 = Cabbage};
                  let {x140 = Cabbage};
                  let {x141 = x124};
                  x7 <- ____evalO;
                  let {x139 = Cons x140 x7};
                  let {x137 = Cons x138 x139};
                  let {x135 = Cons x136 x137};
                  let {x133 = Cons x134 x135};
                  let {x131 = Cons x132 x133};
                  let {x129 = Cons x130 x131};
                  let {x127 = Cons x128 x129};
                  let {x125 = Cons x126 x127};
                  let {x142 = x125};
                  let {x0 = Cons x141 x142};
                  return x0},
              do {let {x143 = Goat};
                  let {x145 = Term.Empty};
                  let {x147 = Wolf};
                  let {x149 = Term.Empty};
                  let {x151 = Cabbage};
                  let {x153 = Wolf};
                  let {x155 = Wolf};
                  let {x156 = x143};
                  x8 <- ___evalO;
                  let {x154 = Cons x155 x8};
                  let {x152 = Cons x153 x154};
                  let {x150 = Cons x151 x152};
                  let {x148 = Cons x149 x150};
                  let {x146 = Cons x147 x148};
                  let {x144 = Cons x145 x146};
                  let {x157 = x144};
                  let {x0 = Cons x156 x157};
                  return x0},
              do {let {x158 = Goat};
                  let {x160 = Term.Empty};
                  let {x162 = Wolf};
                  let {x164 = Term.Empty};
                  let {x166 = Cabbage};
                  let {x168 = Cabbage};
                  let {x169 = x158};
                  x9 <- __swapEvalO;
                  let {x167 = Cons x168 x9};
                  let {x165 = Cons x166 x167};
                  let {x163 = Cons x164 x165};
                  let {x161 = Cons x162 x163};
                  let {x159 = Cons x160 x161};
                  let {x170 = x159};
                  let {x0 = Cons x169 x170};
                  return x0},
              do {let {x171 = Goat};
                  let {x173 = Term.Empty};
                  let {x175 = Wolf};
                  let {x177 = Wolf};
                  let {x179 = Term.Empty};
                  let {x181 = Term.Empty};
                  let {x182 = x171};
                  x9 <- _swapEvalO;
                  let {x180 = Cons x181 x9};
                  let {x178 = Cons x179 x180};
                  let {x176 = Cons x177 x178};
                  let {x174 = Cons x175 x176};
                  let {x172 = Cons x173 x174};
                  let {x183 = x172};
                  let {x0 = Cons x182 x183};
                  return x0},
              do {let {x184 = Goat};
                  let {x186 = Term.Empty};
                  let {x188 = Wolf};
                  let {x190 = Wolf};
                  let {x192 = Wolf};
                  let {x193 = x184};
                  x10 <- __evalO;
                  let {x191 = Cons x192 x10};
                  let {x189 = Cons x190 x191};
                  let {x187 = Cons x188 x189};
                  let {x185 = Cons x186 x187};
                  let {x194 = x185};
                  let {x0 = Cons x193 x194};
                  return x0},
              do {let {x195 = Goat};
                  let {x197 = Term.Empty};
                  let {x199 = Wolf};
                  let {x201 = Wolf};
                  let {x203 = Cabbage};
                  let {x205 = Term.Empty};
                  let {x207 = Term.Empty};
                  let {x208 = x195};
                  x8 <- ____evalO;
                  let {x206 = Cons x207 x8};
                  let {x204 = Cons x205 x206};
                  let {x202 = Cons x203 x204};
                  let {x200 = Cons x201 x202};
                  let {x198 = Cons x199 x200};
                  let {x196 = Cons x197 x198};
                  let {x209 = x196};
                  let {x0 = Cons x208 x209};
                  return x0},
              do {let {x210 = Goat};
                  let {x212 = Term.Empty};
                  let {x214 = Wolf};
                  let {x216 = Wolf};
                  let {x218 = Cabbage};
                  let {x220 = Term.Empty};
                  let {x222 = Wolf};
                  let {x224 = Term.Empty};
                  let {x226 = Term.Empty};
                  let {x227 = x210};
                  x7 <- ___evalO;
                  let {x225 = Cons x226 x7};
                  let {x223 = Cons x224 x225};
                  let {x221 = Cons x222 x223};
                  let {x219 = Cons x220 x221};
                  let {x217 = Cons x218 x219};
                  let {x215 = Cons x216 x217};
                  let {x213 = Cons x214 x215};
                  let {x211 = Cons x212 x213};
                  let {x228 = x211};
                  let {x0 = Cons x227 x228};
                  return x0},
              do {let {x229 = Goat};
                  let {x231 = Term.Empty};
                  let {x233 = Wolf};
                  let {x235 = Wolf};
                  let {x237 = Cabbage};
                  let {x239 = Term.Empty};
                  let {x241 = Wolf};
                  let {x243 = Wolf};
                  let {x244 = x229};
                  x11 <- swapEvalO;
                  let {x242 = Cons x243 x11};
                  let {x240 = Cons x241 x242};
                  let {x238 = Cons x239 x240};
                  let {x236 = Cons x237 x238};
                  let {x234 = Cons x235 x236};
                  let {x232 = Cons x233 x234};
                  let {x230 = Cons x231 x232};
                  let {x245 = x230};
                  let {x0 = Cons x244 x245};
                  return x0},
              do {let {x246 = Goat};
                  let {x248 = Term.Empty};
                  let {x250 = Wolf};
                  let {x252 = Wolf};
                  let {x254 = Cabbage};
                  let {x256 = Term.Empty};
                  let {x258 = Wolf};
                  let {x260 = Cabbage};
                  let {x262 = Term.Empty};
                  let {x263 = x246};
                  x12 <- __evalO;
                  let {x261 = Cons x262 x12};
                  let {x259 = Cons x260 x261};
                  let {x257 = Cons x258 x259};
                  let {x255 = Cons x256 x257};
                  let {x253 = Cons x254 x255};
                  let {x251 = Cons x252 x253};
                  let {x249 = Cons x250 x251};
                  let {x247 = Cons x248 x249};
                  let {x264 = x247};
                  let {x0 = Cons x263 x264};
                  return x0},
              do {let {x265 = Goat};
                  let {x267 = Term.Empty};
                  let {x269 = Wolf};
                  let {x271 = Wolf};
                  let {x273 = Cabbage};
                  let {x275 = Term.Empty};
                  let {x277 = Wolf};
                  let {x279 = Cabbage};
                  let {x281 = Cabbage};
                  let {x282 = x265};
                  x12 <- ___evalO;
                  let {x280 = Cons x281 x12};
                  let {x278 = Cons x279 x280};
                  let {x276 = Cons x277 x278};
                  let {x274 = Cons x275 x276};
                  let {x272 = Cons x273 x274};
                  let {x270 = Cons x271 x272};
                  let {x268 = Cons x269 x270};
                  let {x266 = Cons x267 x268};
                  let {x283 = x266};
                  let {x0 = Cons x282 x283};
                  return x0},
              do {let {x284 = Goat};
                  let {x286 = Term.Empty};
                  let {x288 = Wolf};
                  let {x290 = Wolf};
                  let {x292 = Cabbage};
                  let {x294 = Cabbage};
                  let {x295 = x284};
                  x13 <- _swapEvalO;
                  let {x293 = Cons x294 x13};
                  let {x291 = Cons x292 x293};
                  let {x289 = Cons x290 x291};
                  let {x287 = Cons x288 x289};
                  let {x285 = Cons x286 x287};
                  let {x296 = x285};
                  let {x0 = Cons x295 x296};
                  return x0},
              do {let {x297 = Goat};
                  let {x299 = Term.Empty};
                  let {x301 = Cabbage};
                  let {x303 = Goat};
                  let {x305 = Goat};
                  let {x306 = x297};
                  x10 <- _____evalO;
                  let {x304 = Cons x305 x10};
                  let {x302 = Cons x303 x304};
                  let {x300 = Cons x301 x302};
                  let {x298 = Cons x299 x300};
                  let {x307 = x298};
                  let {x0 = Cons x306 x307};
                  return x0},
              do {let {x308 = Goat};
                  let {x310 = Term.Empty};
                  let {x312 = Cabbage};
                  let {x314 = Goat};
                  let {x316 = Wolf};
                  let {x318 = Term.Empty};
                  let {x320 = Term.Empty};
                  let {x321 = x308};
                  x14 <- ______evalO;
                  let {x319 = Cons x320 x14};
                  let {x317 = Cons x318 x319};
                  let {x315 = Cons x316 x317};
                  let {x313 = Cons x314 x315};
                  let {x311 = Cons x312 x313};
                  let {x309 = Cons x310 x311};
                  let {x322 = x309};
                  let {x0 = Cons x321 x322};
                  return x0},
              do {let {x323 = Goat};
                  let {x325 = Term.Empty};
                  let {x327 = Cabbage};
                  let {x329 = Goat};
                  let {x331 = Wolf};
                  let {x333 = Term.Empty};
                  let {x335 = Goat};
                  let {x336 = x323};
                  x14 <- _______evalO;
                  let {x334 = Cons x335 x14};
                  let {x332 = Cons x333 x334};
                  let {x330 = Cons x331 x332};
                  let {x328 = Cons x329 x330};
                  let {x326 = Cons x327 x328};
                  let {x324 = Cons x325 x326};
                  let {x337 = x324};
                  let {x0 = Cons x336 x337};
                  return x0},
              do {let {x338 = Goat};
                  let {x340 = Term.Empty};
                  let {x342 = Cabbage};
                  let {x344 = Goat};
                  let {x346 = Wolf};
                  let {x348 = Wolf};
                  let {x350 = Term.Empty};
                  let {x352 = Term.Empty};
                  let {x353 = x338};
                  x15 <- swapEvalO;
                  let {x351 = Cons x352 x15};
                  let {x349 = Cons x350 x351};
                  let {x347 = Cons x348 x349};
                  let {x345 = Cons x346 x347};
                  let {x343 = Cons x344 x345};
                  let {x341 = Cons x342 x343};
                  let {x339 = Cons x340 x341};
                  let {x354 = x339};
                  let {x0 = Cons x353 x354};
                  return x0},
              do {let {x355 = Goat};
                  let {x357 = Term.Empty};
                  let {x359 = Cabbage};
                  let {x361 = Goat};
                  let {x363 = Wolf};
                  let {x365 = Wolf};
                  let {x367 = Term.Empty};
                  let {x369 = Cabbage};
                  let {x371 = Term.Empty};
                  let {x373 = Term.Empty};
                  let {x374 = x355};
                  x16 <- _swapEvalO;
                  let {x372 = Cons x373 x16};
                  let {x370 = Cons x371 x372};
                  let {x368 = Cons x369 x370};
                  let {x366 = Cons x367 x368};
                  let {x364 = Cons x365 x366};
                  let {x362 = Cons x363 x364};
                  let {x360 = Cons x361 x362};
                  let {x358 = Cons x359 x360};
                  let {x356 = Cons x357 x358};
                  let {x375 = x356};
                  let {x0 = Cons x374 x375};
                  return x0},
              do {let {x376 = Goat};
                  let {x378 = Term.Empty};
                  let {x380 = Cabbage};
                  let {x382 = Goat};
                  let {x384 = Wolf};
                  let {x386 = Wolf};
                  let {x388 = Term.Empty};
                  let {x390 = Cabbage};
                  let {x392 = Wolf};
                  let {x394 = Term.Empty};
                  let {x396 = Term.Empty};
                  let {x397 = x376};
                  x17 <- __evalO;
                  let {x395 = Cons x396 x17};
                  let {x393 = Cons x394 x395};
                  let {x391 = Cons x392 x393};
                  let {x389 = Cons x390 x391};
                  let {x387 = Cons x388 x389};
                  let {x385 = Cons x386 x387};
                  let {x383 = Cons x384 x385};
                  let {x381 = Cons x382 x383};
                  let {x379 = Cons x380 x381};
                  let {x377 = Cons x378 x379};
                  let {x398 = x377};
                  let {x0 = Cons x397 x398};
                  return x0},
              do {let {x399 = Goat};
                  let {x401 = Term.Empty};
                  let {x403 = Cabbage};
                  let {x405 = Goat};
                  let {x407 = Wolf};
                  let {x409 = Wolf};
                  let {x411 = Term.Empty};
                  let {x413 = Cabbage};
                  let {x415 = Wolf};
                  let {x417 = Term.Empty};
                  let {x419 = Cabbage};
                  let {x421 = Term.Empty};
                  let {x423 = Term.Empty};
                  let {x424 = x399};
                  x18 <- ___evalO;
                  let {x422 = Cons x423 x18};
                  let {x420 = Cons x421 x422};
                  let {x418 = Cons x419 x420};
                  let {x416 = Cons x417 x418};
                  let {x414 = Cons x415 x416};
                  let {x412 = Cons x413 x414};
                  let {x410 = Cons x411 x412};
                  let {x408 = Cons x409 x410};
                  let {x406 = Cons x407 x408};
                  let {x404 = Cons x405 x406};
                  let {x402 = Cons x403 x404};
                  let {x400 = Cons x401 x402};
                  let {x425 = x400};
                  let {x0 = Cons x424 x425};
                  return x0},
              do {let {x426 = Goat};
                  let {x428 = Term.Empty};
                  let {x430 = Cabbage};
                  let {x432 = Goat};
                  let {x434 = Wolf};
                  let {x436 = Wolf};
                  let {x438 = Term.Empty};
                  let {x440 = Cabbage};
                  let {x442 = Wolf};
                  let {x444 = Term.Empty};
                  let {x446 = Cabbage};
                  let {x448 = Wolf};
                  let {x449 = x426};
                  x19 <- swapEvalO;
                  let {x447 = Cons x448 x19};
                  let {x445 = Cons x446 x447};
                  let {x443 = Cons x444 x445};
                  let {x441 = Cons x442 x443};
                  let {x439 = Cons x440 x441};
                  let {x437 = Cons x438 x439};
                  let {x435 = Cons x436 x437};
                  let {x433 = Cons x434 x435};
                  let {x431 = Cons x432 x433};
                  let {x429 = Cons x430 x431};
                  let {x427 = Cons x428 x429};
                  let {x450 = x427};
                  let {x0 = Cons x449 x450};
                  return x0},
              do {let {x451 = Goat};
                  let {x453 = Term.Empty};
                  let {x455 = Cabbage};
                  let {x457 = Goat};
                  let {x459 = Wolf};
                  let {x461 = Wolf};
                  let {x463 = Term.Empty};
                  let {x465 = Cabbage};
                  let {x467 = Wolf};
                  let {x469 = Term.Empty};
                  let {x471 = Cabbage};
                  let {x473 = Cabbage};
                  let {x474 = x451};
                  x19 <- __swapEvalO;
                  let {x472 = Cons x473 x19};
                  let {x470 = Cons x471 x472};
                  let {x468 = Cons x469 x470};
                  let {x466 = Cons x467 x468};
                  let {x464 = Cons x465 x466};
                  let {x462 = Cons x463 x464};
                  let {x460 = Cons x461 x462};
                  let {x458 = Cons x459 x460};
                  let {x456 = Cons x457 x458};
                  let {x454 = Cons x455 x456};
                  let {x452 = Cons x453 x454};
                  let {x475 = x452};
                  let {x0 = Cons x474 x475};
                  return x0},
              do {let {x476 = Goat};
                  let {x478 = Term.Empty};
                  let {x480 = Cabbage};
                  let {x482 = Goat};
                  let {x484 = Wolf};
                  let {x486 = Wolf};
                  let {x488 = Term.Empty};
                  let {x490 = Cabbage};
                  let {x492 = Wolf};
                  let {x494 = Wolf};
                  let {x495 = x476};
                  x20 <- _swapEvalO;
                  let {x493 = Cons x494 x20};
                  let {x491 = Cons x492 x493};
                  let {x489 = Cons x490 x491};
                  let {x487 = Cons x488 x489};
                  let {x485 = Cons x486 x487};
                  let {x483 = Cons x484 x485};
                  let {x481 = Cons x482 x483};
                  let {x479 = Cons x480 x481};
                  let {x477 = Cons x478 x479};
                  let {x496 = x477};
                  let {x0 = Cons x495 x496};
                  return x0},
              do {let {x497 = Goat};
                  let {x499 = Term.Empty};
                  let {x501 = Cabbage};
                  let {x503 = Goat};
                  let {x505 = Wolf};
                  let {x507 = Wolf};
                  let {x509 = Term.Empty};
                  let {x511 = Cabbage};
                  let {x513 = Cabbage};
                  let {x514 = x497};
                  x21 <- ____evalO;
                  let {x512 = Cons x513 x21};
                  let {x510 = Cons x511 x512};
                  let {x508 = Cons x509 x510};
                  let {x506 = Cons x507 x508};
                  let {x504 = Cons x505 x506};
                  let {x502 = Cons x503 x504};
                  let {x500 = Cons x501 x502};
                  let {x498 = Cons x499 x500};
                  let {x515 = x498};
                  let {x0 = Cons x514 x515};
                  return x0},
              do {let {x516 = Goat};
                  let {x518 = Term.Empty};
                  let {x520 = Cabbage};
                  let {x522 = Goat};
                  let {x524 = Wolf};
                  let {x526 = Wolf};
                  let {x528 = Wolf};
                  let {x530 = Term.Empty};
                  let {x532 = Term.Empty};
                  let {x533 = x516};
                  x21 <- ___evalO;
                  let {x531 = Cons x532 x21};
                  let {x529 = Cons x530 x531};
                  let {x527 = Cons x528 x529};
                  let {x525 = Cons x526 x527};
                  let {x523 = Cons x524 x525};
                  let {x521 = Cons x522 x523};
                  let {x519 = Cons x520 x521};
                  let {x517 = Cons x518 x519};
                  let {x534 = x517};
                  let {x0 = Cons x533 x534};
                  return x0},
              do {let {x535 = Goat};
                  let {x537 = Term.Empty};
                  let {x539 = Cabbage};
                  let {x541 = Goat};
                  let {x543 = Wolf};
                  let {x545 = Wolf};
                  let {x547 = Wolf};
                  let {x549 = Wolf};
                  let {x550 = x535};
                  x22 <- swapEvalO;
                  let {x548 = Cons x549 x22};
                  let {x546 = Cons x547 x548};
                  let {x544 = Cons x545 x546};
                  let {x542 = Cons x543 x544};
                  let {x540 = Cons x541 x542};
                  let {x538 = Cons x539 x540};
                  let {x536 = Cons x537 x538};
                  let {x551 = x536};
                  let {x0 = Cons x550 x551};
                  return x0},
              do {let {x552 = Goat};
                  let {x554 = Term.Empty};
                  let {x556 = Cabbage};
                  let {x558 = Goat};
                  let {x560 = Wolf};
                  let {x562 = Wolf};
                  let {x564 = Wolf};
                  let {x566 = Cabbage};
                  let {x568 = Term.Empty};
                  let {x570 = Term.Empty};
                  let {x571 = x552};
                  x20 <- __swapEvalO;
                  let {x569 = Cons x570 x20};
                  let {x567 = Cons x568 x569};
                  let {x565 = Cons x566 x567};
                  let {x563 = Cons x564 x565};
                  let {x561 = Cons x562 x563};
                  let {x559 = Cons x560 x561};
                  let {x557 = Cons x558 x559};
                  let {x555 = Cons x556 x557};
                  let {x553 = Cons x554 x555};
                  let {x572 = x553};
                  let {x0 = Cons x571 x572};
                  return x0},
              do {let {x573 = Goat};
                  let {x575 = Term.Empty};
                  let {x577 = Cabbage};
                  let {x579 = Goat};
                  let {x581 = Wolf};
                  let {x583 = Wolf};
                  let {x585 = Wolf};
                  let {x587 = Cabbage};
                  let {x589 = Term.Empty};
                  let {x591 = Wolf};
                  let {x593 = Term.Empty};
                  let {x595 = Term.Empty};
                  let {x596 = x573};
                  x19 <- _swapEvalO;
                  let {x594 = Cons x595 x19};
                  let {x592 = Cons x593 x594};
                  let {x590 = Cons x591 x592};
                  let {x588 = Cons x589 x590};
                  let {x586 = Cons x587 x588};
                  let {x584 = Cons x585 x586};
                  let {x582 = Cons x583 x584};
                  let {x580 = Cons x581 x582};
                  let {x578 = Cons x579 x580};
                  let {x576 = Cons x577 x578};
                  let {x574 = Cons x575 x576};
                  let {x597 = x574};
                  let {x0 = Cons x596 x597};
                  return x0},
              do {let {x598 = Goat};
                  let {x600 = Term.Empty};
                  let {x602 = Cabbage};
                  let {x604 = Goat};
                  let {x606 = Wolf};
                  let {x608 = Wolf};
                  let {x610 = Wolf};
                  let {x612 = Cabbage};
                  let {x614 = Term.Empty};
                  let {x616 = Wolf};
                  let {x618 = Wolf};
                  let {x619 = x598};
                  x23 <- __evalO;
                  let {x617 = Cons x618 x23};
                  let {x615 = Cons x616 x617};
                  let {x613 = Cons x614 x615};
                  let {x611 = Cons x612 x613};
                  let {x609 = Cons x610 x611};
                  let {x607 = Cons x608 x609};
                  let {x605 = Cons x606 x607};
                  let {x603 = Cons x604 x605};
                  let {x601 = Cons x602 x603};
                  let {x599 = Cons x600 x601};
                  let {x620 = x599};
                  let {x0 = Cons x619 x620};
                  return x0},
              do {let {x621 = Goat};
                  let {x623 = Term.Empty};
                  let {x625 = Cabbage};
                  let {x627 = Goat};
                  let {x629 = Wolf};
                  let {x631 = Wolf};
                  let {x633 = Wolf};
                  let {x635 = Cabbage};
                  let {x637 = Term.Empty};
                  let {x639 = Wolf};
                  let {x641 = Cabbage};
                  let {x643 = Term.Empty};
                  let {x644 = x621};
                  x24 <- swapEvalO;
                  let {x642 = Cons x643 x24};
                  let {x640 = Cons x641 x642};
                  let {x638 = Cons x639 x640};
                  let {x636 = Cons x637 x638};
                  let {x634 = Cons x635 x636};
                  let {x632 = Cons x633 x634};
                  let {x630 = Cons x631 x632};
                  let {x628 = Cons x629 x630};
                  let {x626 = Cons x627 x628};
                  let {x624 = Cons x625 x626};
                  let {x622 = Cons x623 x624};
                  let {x645 = x622};
                  let {x0 = Cons x644 x645};
                  return x0},
              do {let {x646 = Goat};
                  let {x648 = Term.Empty};
                  let {x650 = Cabbage};
                  let {x652 = Goat};
                  let {x654 = Wolf};
                  let {x656 = Wolf};
                  let {x658 = Wolf};
                  let {x660 = Cabbage};
                  let {x662 = Term.Empty};
                  let {x664 = Wolf};
                  let {x666 = Cabbage};
                  let {x668 = Cabbage};
                  let {x669 = x646};
                  x24 <- _swapEvalO;
                  let {x667 = Cons x668 x24};
                  let {x665 = Cons x666 x667};
                  let {x663 = Cons x664 x665};
                  let {x661 = Cons x662 x663};
                  let {x659 = Cons x660 x661};
                  let {x657 = Cons x658 x659};
                  let {x655 = Cons x656 x657};
                  let {x653 = Cons x654 x655};
                  let {x651 = Cons x652 x653};
                  let {x649 = Cons x650 x651};
                  let {x647 = Cons x648 x649};
                  let {x670 = x647};
                  let {x0 = Cons x669 x670};
                  return x0},
              do {let {x671 = Goat};
                  let {x673 = Term.Empty};
                  let {x675 = Cabbage};
                  let {x677 = Goat};
                  let {x679 = Wolf};
                  let {x681 = Wolf};
                  let {x683 = Wolf};
                  let {x685 = Cabbage};
                  let {x687 = Cabbage};
                  let {x688 = x671};
                  x25 <- ___evalO;
                  let {x686 = Cons x687 x25};
                  let {x684 = Cons x685 x686};
                  let {x682 = Cons x683 x684};
                  let {x680 = Cons x681 x682};
                  let {x678 = Cons x679 x680};
                  let {x676 = Cons x677 x678};
                  let {x674 = Cons x675 x676};
                  let {x672 = Cons x673 x674};
                  let {x689 = x672};
                  let {x0 = Cons x688 x689};
                  return x0},
              do {let {x690 = Goat};
                  let {x692 = Term.Empty};
                  let {x694 = Cabbage};
                  let {x696 = Goat};
                  let {x698 = Wolf};
                  let {x700 = Cabbage};
                  let {x702 = Goat};
                  let {x704 = Goat};
                  let {x705 = x690};
                  x22 <- ___swapEvalO;
                  let {x703 = Cons x704 x22};
                  let {x701 = Cons x702 x703};
                  let {x699 = Cons x700 x701};
                  let {x697 = Cons x698 x699};
                  let {x695 = Cons x696 x697};
                  let {x693 = Cons x694 x695};
                  let {x691 = Cons x692 x693};
                  let {x706 = x691};
                  let {x0 = Cons x705 x706};
                  return x0},
              do {let {x707 = Goat};
                  let {x709 = Term.Empty};
                  let {x711 = Cabbage};
                  let {x713 = Goat};
                  let {x715 = Wolf};
                  let {x717 = Cabbage};
                  let {x719 = Goat};
                  let {x721 = Wolf};
                  let {x722 = x707};
                  x22 <- ____swapEvalO;
                  let {x720 = Cons x721 x22};
                  let {x718 = Cons x719 x720};
                  let {x716 = Cons x717 x718};
                  let {x714 = Cons x715 x716};
                  let {x712 = Cons x713 x714};
                  let {x710 = Cons x711 x712};
                  let {x708 = Cons x709 x710};
                  let {x723 = x708};
                  let {x0 = Cons x722 x723};
                  return x0},
              do {let {x724 = Goat};
                  let {x726 = Term.Empty};
                  let {x728 = Cabbage};
                  let {x730 = Goat};
                  let {x732 = Wolf};
                  let {x734 = Cabbage};
                  let {x736 = Cabbage};
                  let {x737 = x724};
                  x26 <- ______evalO;
                  let {x735 = Cons x736 x26};
                  let {x733 = Cons x734 x735};
                  let {x731 = Cons x732 x733};
                  let {x729 = Cons x730 x731};
                  let {x727 = Cons x728 x729};
                  let {x725 = Cons x726 x727};
                  let {x738 = x725};
                  let {x0 = Cons x737 x738};
                  return x0},
              do {let {x739 = Goat};
                  let {x741 = Term.Empty};
                  let {x743 = Cabbage};
                  let {x745 = Cabbage};
                  let {x746 = x739};
                  x27 <- ____swapEvalO;
                  let {x744 = Cons x745 x27};
                  let {x742 = Cons x743 x744};
                  let {x740 = Cons x741 x742};
                  let {x747 = x740};
                  let {x0 = Cons x746 x747};
                  return x0},
              do {let {x748 = Goat};
                  let {x750 = Goat};
                  let {x751 = x748};
                  x28 <- evalO;
                  let {x749 = Cons x750 x28};
                  let {x752 = x749};
                  let {x0 = Cons x751 x752};
                  return x0}]
_______evalO = Immature $ msum [do {let {x0 = Nil}; return x0},
                     do {let {x817 = Term.Empty};
                         let {x819 = Term.Empty};
                         let {x820 = x817};
                         x1 <- _______evalO;
                         let {x818 = Cons x819 x1};
                         let {x821 = x818};
                         let {x0 = Cons x820 x821};
                         return x0},
                     do {let {x822 = Goat};
                         let {x824 = Term.Empty};
                         let {x825 = x822};
                         x2 <- ______evalO;
                         let {x823 = Cons x824 x2};
                         let {x826 = x823};
                         let {x0 = Cons x825 x826};
                         return x0},
                     do {let {x827 = Goat};
                         let {x829 = Goat};
                         let {x830 = x827};
                         x2 <- _______evalO;
                         let {x828 = Cons x829 x2};
                         let {x831 = x828};
                         let {x0 = Cons x830 x831};
                         return x0}]
______evalO = Immature $ msum [do {let {x803 = Term.Empty};
                        let {x805 = Term.Empty};
                        let {x806 = x803};
                        x1 <- ______evalO;
                        let {x804 = Cons x805 x1};
                        let {x807 = x804};
                        let {x0 = Cons x806 x807};
                        return x0},
                    do {let {x808 = Term.Empty};
                        let {x810 = Goat};
                        let {x811 = x808};
                        x1 <- _______evalO;
                        let {x809 = Cons x810 x1};
                        let {x812 = x809};
                        let {x0 = Cons x811 x812};
                        return x0},
                    do {let {x813 = Wolf};
                        let {x814 = x813};
                        x2 <- swapEvalO;
                        let {x0 = Cons x814 x2};
                        return x0},
                    do {let {x815 = Cabbage};
                        let {x816 = x815};
                        x2 <- ___swapEvalO;
                        let {x0 = Cons x816 x2};
                        return x0}]
_____evalO = Immature $ msum [do {let {x791 = Goat};
                       let {x793 = Goat};
                       let {x794 = x791};
                       x1 <- _____evalO;
                       let {x792 = Cons x793 x1};
                       let {x795 = x792};
                       let {x0 = Cons x794 x795};
                       return x0},
                   do {let {x796 = Goat};
                       let {x798 = Wolf};
                       let {x799 = x796};
                       x1 <- ______evalO;
                       let {x797 = Cons x798 x1};
                       let {x800 = x797};
                       let {x0 = Cons x799 x800};
                       return x0},
                   do {let {x801 = Cabbage};
                       let {x802 = x801};
                       x2 <- ____swapEvalO;
                       let {x0 = Cons x802 x2};
                       return x0}]
____evalO = Immature $ msum [do {let {x783 = Term.Empty};
                      let {x784 = x783};
                      x1 <- swapEvalO;
                      let {x0 = Cons x784 x1};
                      return x0},
                  do {let {x785 = Cabbage};
                      let {x786 = x785};
                      x1 <- _swapEvalO;
                      let {x0 = Cons x786 x1};
                      return x0}]
____swapEvalO = Immature $ msum [do {let {x844 = Term.Empty};
                          let {x845 = x844};
                          x1 <- _evalO;
                          let {x0 = Cons x845 x1};
                          return x0},
                      do {let {x846 = Wolf};
                          let {x847 = x846};
                          x1 <- __evalO;
                          let {x0 = Cons x847 x1};
                          return x0},
                      do {let {x848 = Cabbage};
                          let {x849 = x848};
                          x1 <- _____evalO;
                          let {x0 = Cons x849 x1};
                          return x0}]
___evalO = Immature $ msum [do {let {x761 = Term.Empty};
                     let {x763 = Term.Empty};
                     let {x764 = x761};
                     x1 <- ___evalO;
                     let {x762 = Cons x763 x1};
                     let {x765 = x762};
                     let {x0 = Cons x764 x765};
                     return x0},
                 do {let {x766 = Wolf};
                     let {x767 = x766};
                     x2 <- swapEvalO;
                     let {x0 = Cons x767 x2};
                     return x0},
                 do {let {x768 = Cabbage};
                     let {x769 = x768};
                     x2 <- __swapEvalO;
                     let {x0 = Cons x769 x2};
                     return x0}]
___swapEvalO = Immature $ msum [do {let {x832 = Goat};
                         let {x834 = Goat};
                         let {x835 = x832};
                         x1 <- ___swapEvalO;
                         let {x833 = Cons x834 x1};
                         let {x836 = x833};
                         let {x0 = Cons x835 x836};
                         return x0},
                     do {let {x837 = Goat};
                         let {x839 = Wolf};
                         let {x840 = x837};
                         x1 <- ____swapEvalO;
                         let {x838 = Cons x839 x1};
                         let {x841 = x838};
                         let {x0 = Cons x840 x841};
                         return x0},
                     do {let {x842 = Cabbage};
                         let {x843 = x842};
                         x2 <- ______evalO;
                         let {x0 = Cons x843 x2};
                         return x0}]
__evalO = Immature $ msum [do {let {x757 = Term.Empty};
                    let {x758 = x757};
                    x1 <- __swapEvalO;
                    let {x0 = Cons x758 x1};
                    return x0},
                do {let {x759 = Wolf};
                    let {x760 = x759};
                    x1 <- _swapEvalO;
                    let {x0 = Cons x760 x1};
                    return x0}]
__swapEvalO = Immature $ msum [do {let {x787 = Term.Empty};
                        let {x788 = x787};
                        x1 <- __evalO;
                        let {x0 = Cons x788 x1};
                        return x0},
                    do {let {x789 = Cabbage};
                        let {x790 = x789};
                        x1 <- ___evalO;
                        let {x0 = Cons x790 x1};
                        return x0}]
_evalO = Immature $ msum [do {let {x753 = Term.Empty};
                   let {x754 = x753};
                   x1 <- ____swapEvalO;
                   let {x0 = Cons x754 x1};
                   return x0},
               do {let {x755 = Goat};
                   let {x756 = x755};
                   x1 <- evalO;
                   let {x0 = Cons x756 x1};
                   return x0}]
_swapEvalO = Immature $ msum [do {let {x774 = Term.Empty};
                       let {x776 = Term.Empty};
                       let {x777 = x774};
                       x1 <- _swapEvalO;
                       let {x775 = Cons x776 x1};
                       let {x778 = x775};
                       let {x0 = Cons x777 x778};
                       return x0},
                   do {let {x779 = Wolf};
                       let {x780 = x779};
                       x2 <- __evalO;
                       let {x0 = Cons x780 x2};
                       return x0},
                   do {let {x781 = Cabbage};
                       let {x782 = x781};
                       x2 <- ____evalO;
                       let {x0 = Cons x782 x2};
                       return x0}]
swapEvalI x0 = Immature $ msum [do {let {x770 = Term.Empty};
                         (x771, x1) <- case x0 of
                                       {Cons y771 y1 -> return (y771, y1); _ -> mzero};
                         guard (x771 == x770);
                         ____evalI x1;
                         return ()},
                     do {let {x772 = Wolf};
                         (x773, x1) <- case x0 of
                                       {Cons y773 y1 -> return (y773, y1); _ -> mzero};
                         guard (x773 == x772);
                         ___evalI x1;
                         return ()}]
swapEvalO = Immature $ msum [do {let {x770 = Term.Empty};
                      let {x771 = x770};
                      x1 <- ____evalO;
                      let {x0 = Cons x771 x1};
                      return x0},
                  do {let {x772 = Wolf};
                      let {x773 = x772};
                      x1 <- ___evalO;
                      let {x0 = Cons x773 x1};
                      return x0}]