open GT
open OCanren
open OCanren.Stream

let (let*) = OCanren.Stream.bind
let return = OCanren.Stream.single
let mzero = OCanren.Stream.nil
let mplus = OCanren.Stream.mplus
let msum xs = List.fold_right mplus xs mzero
let guard p = if p then return () else mzero
let make_lazy f = OCanren.Stream.from_fun f
type term =
  | Cons of (term * term)
  | Nil
  | One
  | Pair of (term * term)
  | Thr
  | Two

let rec checkI x0  =
  msum
    [(let* x3 = return One in
    let* x4 = return Two in
    let* x2 = return (Pair (x3, x4)) in
    let* (x5, x1) = match x0 with
      | Cons (y5, y1) ->  return (y5, y1)
      | _ -> mzero in
    let* _ = guard (x5 == x2) in
    let* _ = _checkI x1  in return ());
    (let* x7 = return One in
    let* x8 = return Thr in
    let* x6 = return (Pair (x7, x8)) in
    let* (x9, x1) = match x0 with
      | Cons (y9, y1) ->  return (y9, y1)
      | _ -> mzero in
    let* _ = guard (x9 == x6) in
    let* _ = __________________________checkI x1  in return ())]
and __________________________checkI x0  =
  msum
    [(let* x303 = return One in
    let* x304 = return Two in
    let* x302 = return (Pair (x303, x304)) in
    let* (x305, x1) = match x0 with
      | Cons (y305, y1) ->  return (y305, y1)
      | _ -> mzero in
    let* _ = guard (x305 == x302) in
    let* _ = _____checkI x1  in return ());
    (let* x307 = return Thr in
    let* x308 = return Two in
    let* x306 = return (Pair (x307, x308)) in
    let* (x309, x1) = match x0 with
      | Cons (y309, y1) ->  return (y309, y1)
      | _ -> mzero in
    let* _ = guard (x309 == x306) in
    let* _ = _checkI x1  in return ());
    (let* x311 = return Thr in
    let* x312 = return One in
    let* x310 = return (Pair (x311, x312)) in
    let* (x313, x1) = match x0 with
      | Cons (y313, y1) ->  return (y313, y1)
      | _ -> mzero in
    let* _ = guard (x313 == x310) in
    let* _ = checkI x1  in return ())]
and _____checkI x0  =
  msum
    [(let* x59 = return Two in
    let* x60 = return One in
    let* x58 = return (Pair (x59, x60)) in
    let* (x61, x1) = match x0 with
      | Cons (y61, y1) ->  return (y61, y1)
      | _ -> mzero in
    let* _ = guard (x61 == x58) in
    let* _ = __________________________checkI x1  in return ());
    (let* x63 = return Thr in
    let* x64 = return One in
    let* x62 = return (Pair (x63, x64)) in
    let* (x65, x1) = match x0 with
      | Cons (y65, y1) ->  return (y65, y1)
      | _ -> mzero in
    let* _ = guard (x65 == x62) in
    let* _ = ____checkI x1  in return ());
    (let* x67 = return Thr in
    let* x68 = return Two in
    let* x66 = return (Pair (x67, x68)) in
    let* (x69, x1) = match x0 with
      | Cons (y69, y1) ->  return (y69, y1)
      | _ -> mzero in
    let* _ = guard (x69 == x66) in
    let* _ = ______checkI x1  in return ())]
and ______checkI x0  =
  msum
    [(let* x71 = return One in
    let* x72 = return Thr in
    let* x70 = return (Pair (x71, x72)) in
    let* (x73, x1) = match x0 with
      | Cons (y73, y1) ->  return (y73, y1)
      | _ -> mzero in
    let* _ = guard (x73 == x70) in
    let* _ = _______checkI x1  in return ());
    (let* x75 = return Two in
    let* x76 = return Thr in
    let* x74 = return (Pair (x75, x76)) in
    let* (x77, x1) = match x0 with
      | Cons (y77, y1) ->  return (y77, y1)
      | _ -> mzero in
    let* _ = guard (x77 == x74) in
    let* _ = _____checkI x1  in return ());
    (let* x79 = return Two in
    let* x80 = return One in
    let* x78 = return (Pair (x79, x80)) in
    let* (x81, x1) = match x0 with
      | Cons (y81, y1) ->  return (y81, y1)
      | _ -> mzero in
    let* _ = guard (x81 == x78) in
    let* _ = ____checkI x1  in return ())]
and _______checkI x0  =
  msum
    [(let* x83 = return Two in
    let* x84 = return One in
    let* x82 = return (Pair (x83, x84)) in
    let* (x85, x1) = match x0 with
      | Cons (y85, y1) ->  return (y85, y1)
      | _ -> mzero in
    let* _ = guard (x85 == x82) in
    let* _ = ________checkI x1  in return ());
    (let* x87 = return Thr in
    let* x88 = return One in
    let* x86 = return (Pair (x87, x88)) in
    let* (x89, x1) = match x0 with
      | Cons (y89, y1) ->  return (y89, y1)
      | _ -> mzero in
    let* _ = guard (x89 == x86) in
    let* _ = ______checkI x1  in return ());
    (let* x91 = return Two in
    let* x92 = return Thr in
    let* x90 = return (Pair (x91, x92)) in
    let* (x93, x1) = match x0 with
      | Cons (y93, y1) ->  return (y93, y1)
      | _ -> mzero in
    let* _ = guard (x93 == x90) in
    let* _ = ________________________checkI x1  in return ())]
and ________________________checkI x0  =
  msum
    [(let* x279 = return Two in
    let* x280 = return One in
    let* x278 = return (Pair (x279, x280)) in
    let* (x281, x1) = match x0 with
      | Cons (y281, y1) ->  return (y281, y1)
      | _ -> mzero in
    let* _ = guard (x281 == x278) in
    let* _ = ______________________checkI x1  in return ());
    (let* x283 = return Thr in
    let* x284 = return One in
    let* x282 = return (Pair (x283, x284)) in
    let* (x285, x1) = match x0 with
      | Cons (y285, y1) ->  return (y285, y1)
      | _ -> mzero in
    let* _ = guard (x285 == x282) in
    let* _ = ________checkI x1  in return ());
    (let* x287 = return Thr in
    let* x288 = return Two in
    let* x286 = return (Pair (x287, x288)) in
    let* (x289, x1) = match x0 with
      | Cons (y289, y1) ->  return (y289, y1)
      | _ -> mzero in
    let* _ = guard (x289 == x286) in
    let* _ = _______checkI x1  in return ())]
and ______________________checkI x0  =
  msum
    [(let* x259 = return One in
    let* x260 = return Two in
    let* x258 = return (Pair (x259, x260)) in
    let* (x261, x1) = match x0 with
      | Cons (y261, y1) ->  return (y261, y1)
      | _ -> mzero in
    let* _ = guard (x261 == x258) in
    let* _ = ________________________checkI x1  in return ());
    (let* x263 = return Thr in
    let* x264 = return Two in
    let* x262 = return (Pair (x263, x264)) in
    let* (x265, x1) = match x0 with
      | Cons (y265, y1) ->  return (y265, y1)
      | _ -> mzero in
    let* _ = guard (x265 == x262) in
    let* _ = ___________checkI x1  in return ());
    (let* x267 = return Thr in
    let* x268 = return One in
    let* x266 = return (Pair (x267, x268)) in
    let* (x269, x1) = match x0 with
      | Cons (y269, y1) ->  return (y269, y1)
      | _ -> mzero in
    let* _ = guard (x269 == x266) in
    let* _ = ____________checkI x1  in return ())]
and ____________checkI x0  =
  msum
    [(let* x143 = return One in
    let* x144 = return Two in
    let* x142 = return (Pair (x143, x144)) in
    let* (x145, x1) = match x0 with
      | Cons (y145, y1) ->  return (y145, y1)
      | _ -> mzero in
    let* _ = guard (x145 == x142) in
    let* _ = ___________checkI x1  in return ());
    (let* x147 = return Thr in
    let* x148 = return Two in
    let* x146 = return (Pair (x147, x148)) in
    let* (x149, x1) = match x0 with
      | Cons (y149, y1) ->  return (y149, y1)
      | _ -> mzero in
    let* _ = guard (x149 == x146) in
    let* _ = _____________checkI x1  in return ());
    (let* x151 = return One in
    let* x152 = return Thr in
    let* x150 = return (Pair (x151, x152)) in
    let* (x153, x1) = match x0 with
      | Cons (y153, y1) ->  return (y153, y1)
      | _ -> mzero in
    let* _ = guard (x153 == x150) in
    let* _ = ______________________checkI x1  in return ())]
and _____________checkI x0  =
  msum
    [(let* x155 = return One in
    let* x156 = return Thr in
    let* x154 = return (Pair (x155, x156)) in
    let* (x157, x1) = match x0 with
      | Cons (y157, y1) ->  return (y157, y1)
      | _ -> mzero in
    let* _ = guard (x157 == x154) in
    let* _ = ______________checkI x1  in return ());
    (let* x159 = return Two in
    let* x160 = return Thr in
    let* x158 = return (Pair (x159, x160)) in
    let* (x161, x1) = match x0 with
      | Cons (y161, y1) ->  return (y161, y1)
      | _ -> mzero in
    let* _ = guard (x161 == x158) in
    let* _ = ____________checkI x1  in return ());
    (let* x163 = return One in
    let* x164 = return Two in
    let* x162 = return (Pair (x163, x164)) in
    let* (x165, x1) = match x0 with
      | Cons (y165, y1) ->  return (y165, y1)
      | _ -> mzero in
    let* _ = guard (x165 == x162) in
    let* _ = _____________________checkI x1  in return ())]
and _____________________checkI x0  =
  msum
    [(let* x247 = return One in
    let* x248 = return Thr in
    let* x246 = return (Pair (x247, x248)) in
    let* (x249, x1) = match x0 with
      | Cons (y249, y1) ->  return (y249, y1)
      | _ -> mzero in
    let* _ = guard (x249 == x246) in
    let* _ = __________________checkI x1  in return ());
    (let* x251 = return Two in
    let* x252 = return Thr in
    let* x250 = return (Pair (x251, x252)) in
    let* (x253, x1) = match x0 with
      | Cons (y253, y1) ->  return (y253, y1)
      | _ -> mzero in
    let* _ = guard (x253 == x250) in
    let* _ = ______________checkI x1  in return ());
    (let* x255 = return Two in
    let* x256 = return One in
    let* x254 = return (Pair (x255, x256)) in
    let* (x257, x1) = match x0 with
      | Cons (y257, y1) ->  return (y257, y1)
      | _ -> mzero in
    let* _ = guard (x257 == x254) in
    let* _ = _____________checkI x1  in return ())]
and __________________checkI x0  =
  msum
    [(let* x215 = return Two in
    let* x216 = return One in
    let* x214 = return (Pair (x215, x216)) in
    let* (x217, x1) = match x0 with
      | Cons (y217, y1) ->  return (y217, y1)
      | _ -> mzero in
    let* _ = guard (x217 == x214) in
    let* _ = _________________checkI x1  in return ());
    (let* x219 = return Thr in
    let* x220 = return One in
    let* x218 = return (Pair (x219, x220)) in
    let* (x221, x1) = match x0 with
      | Cons (y221, y1) ->  return (y221, y1)
      | _ -> mzero in
    let* _ = guard (x221 == x218) in
    let* _ = _____________________checkI x1  in return ());
    (let* x223 = return Two in
    let* x224 = return Thr in
    let* x222 = return (Pair (x223, x224)) in
    let* (x225, x1) = match x0 with
      | Cons (y225, y1) ->  return (y225, y1)
      | _ -> mzero in
    let* _ = guard (x225 == x222) in
    let* _ = ___________________checkI x1  in return ())]
and ___________________checkI x0  =
  msum
    [(let* x227 = return Two in
    let* x228 = return One in
    let* x226 = return (Pair (x227, x228)) in
    let* (x229, x1) = match x0 with
      | Cons (y229, y1) ->  return (y229, y1)
      | _ -> mzero in
    let* _ = guard (x229 == x226) in
    let* _ = _________________________checkI x1  in return ());
    (let* x231 = return Thr in
    let* x232 = return One in
    let* x230 = return (Pair (x231, x232)) in
    let* (x233, x1) = match x0 with
      | Cons (y233, y1) ->  return (y233, y1)
      | _ -> mzero in
    let* _ = guard (x233 == x230) in
    let* _ = _________________checkI x1  in return ());
    (let* x235 = return Thr in
    let* x236 = return Two in
    let* x234 = return (Pair (x235, x236)) in
    let* (x237, x1) = match x0 with
      | Cons (y237, y1) ->  return (y237, y1)
      | _ -> mzero in
    let* _ = guard (x237 == x234) in
    let* _ = __________________checkI x1  in return ())]
and _________________________checkI x0  =
  msum
    [(let* x291 = return One in
    let* x292 = return Two in
    let* x290 = return (Pair (x291, x292)) in
    let* (x293, x1) = match x0 with
      | Cons (y293, y1) ->  return (y293, y1)
      | _ -> mzero in
    let* _ = guard (x293 == x290) in
    let* _ = ___________________checkI x1  in return ());
    (let* x295 = return Thr in
    let* x296 = return Two in
    let* x294 = return (Pair (x295, x296)) in
    let* (x297, x1) = match x0 with
      | Cons (y297, y1) ->  return (y297, y1)
      | _ -> mzero in
    let* _ = guard (x297 == x294) in
    let* _ = __checkI x1  in return ());
    (let* x299 = return Thr in
    let* x300 = return One in
    let* x298 = return (Pair (x299, x300)) in
    let* (x301, x1) = match x0 with
      | Cons (y301, y1) ->  return (y301, y1)
      | _ -> mzero in
    let* _ = guard (x301 == x298) in
    let* _ = ___checkI x1  in return ())]
and _________________checkI x0  =
  msum
    [(let* x203 = return One in
    let* x204 = return Two in
    let* x202 = return (Pair (x203, x204)) in
    let* (x205, x1) = match x0 with
      | Cons (y205, y1) ->  return (y205, y1)
      | _ -> mzero in
    let* _ = guard (x205 == x202) in
    let* _ = __________________checkI x1  in return ());
    (let* x207 = return One in
    let* x208 = return Thr in
    let* x206 = return (Pair (x207, x208)) in
    let* (x209, x1) = match x0 with
      | Cons (y209, y1) ->  return (y209, y1)
      | _ -> mzero in
    let* _ = guard (x209 == x206) in
    let* _ = ___________________checkI x1  in return ());
    (let* x211 = return Thr in
    let* x212 = return Two in
    let* x210 = return (Pair (x211, x212)) in
    let* (x213, x1) = match x0 with
      | Cons (y213, y1) ->  return (y213, y1)
      | _ -> mzero in
    let* _ = guard (x213 == x210) in
    let* _ = ________________checkI x1  in return ())]
and ________________checkI x0  =
  msum
    [(let* x191 = return One in
    let* x192 = return Thr in
    let* x190 = return (Pair (x191, x192)) in
    let* (x193, x1) = match x0 with
      | Cons (y193, y1) ->  return (y193, y1)
      | _ -> mzero in
    let* _ = guard (x193 == x190) in
    let* _ = _______________checkI x1  in return ());
    (let* x195 = return Two in
    let* x196 = return Thr in
    let* x194 = return (Pair (x195, x196)) in
    let* (x197, x1) = match x0 with
      | Cons (y197, y1) ->  return (y197, y1)
      | _ -> mzero in
    let* _ = guard (x197 == x194) in
    let* _ = _________________checkI x1  in return ());
    (let* x199 = return One in
    let* x200 = return Two in
    let* x198 = return (Pair (x199, x200)) in
    let* (x201, x1) = match x0 with
      | Cons (y201, y1) ->  return (y201, y1)
      | _ -> mzero in
    let* _ = guard (x201 == x198) in
    let* _ = ____________________checkI x1  in return ())]
and ____________________checkI x0  =
  msum
    [(let* x239 = return Two in
    let* x240 = return One in
    let* x238 = return (Pair (x239, x240)) in
    let* (x241, x1) = match x0 with
      | Cons (y241, y1) ->  return (y241, y1)
      | _ -> mzero in
    let* _ = guard (x241 == x238) in
    let* _ = ________________checkI x1  in return ());
    (let* x243 = return Two in
    let* x244 = return Thr in
    let* x242 = return (Pair (x243, x244)) in
    let* (x245, x1) = match x0 with
      | Cons (y245, y1) ->  return (y245, y1)
      | _ -> mzero in
    let* _ = guard (x245 == x242) in
    let* _ = _______________checkI x1  in return ())]
and _______________checkI x0  =
  msum
    [(let* x179 = return Two in
    let* x180 = return One in
    let* x178 = return (Pair (x179, x180)) in
    let* (x181, x1) = match x0 with
      | Cons (y181, y1) ->  return (y181, y1)
      | _ -> mzero in
    let* _ = guard (x181 == x178) in
    let* _ = ______________checkI x1  in return ());
    (let* x183 = return Thr in
    let* x184 = return One in
    let* x182 = return (Pair (x183, x184)) in
    let* (x185, x1) = match x0 with
      | Cons (y185, y1) ->  return (y185, y1)
      | _ -> mzero in
    let* _ = guard (x185 == x182) in
    let* _ = ________________checkI x1  in return ());
    (let* x187 = return Thr in
    let* x188 = return Two in
    let* x186 = return (Pair (x187, x188)) in
    let* (x189, x1) = match x0 with
      | Cons (y189, y1) ->  return (y189, y1)
      | _ -> mzero in
    let* _ = guard (x189 == x186) in
    let* _ = ____________________checkI x1  in return ())]
and ______________checkI x0  =
  msum
    [(let* x167 = return One in
    let* x168 = return Two in
    let* x166 = return (Pair (x167, x168)) in
    let* (x169, x1) = match x0 with
      | Cons (y169, y1) ->  return (y169, y1)
      | _ -> mzero in
    let* _ = guard (x169 == x166) in
    let* _ = _______________checkI x1  in return ());
    (let* x171 = return Thr in
    let* x172 = return One in
    let* x170 = return (Pair (x171, x172)) in
    let* (x173, x1) = match x0 with
      | Cons (y173, y1) ->  return (y173, y1)
      | _ -> mzero in
    let* _ = guard (x173 == x170) in
    let* _ = _____________checkI x1  in return ());
    (let* x175 = return Thr in
    let* x176 = return Two in
    let* x174 = return (Pair (x175, x176)) in
    let* (x177, x1) = match x0 with
      | Cons (y177, y1) ->  return (y177, y1)
      | _ -> mzero in
    let* _ = guard (x177 == x174) in
    let* _ = _____________________checkI x1  in return ())]
and ___________checkI x0  =
  msum
    [(let* x131 = return One in
    let* x132 = return Thr in
    let* x130 = return (Pair (x131, x132)) in
    let* (x133, x1) = match x0 with
      | Cons (y133, y1) ->  return (y133, y1)
      | _ -> mzero in
    let* _ = guard (x133 == x130) in
    let* _ = __________checkI x1  in return ());
    (let* x135 = return Two in
    let* x136 = return One in
    let* x134 = return (Pair (x135, x136)) in
    let* (x137, x1) = match x0 with
      | Cons (y137, y1) ->  return (y137, y1)
      | _ -> mzero in
    let* _ = guard (x137 == x134) in
    let* _ = ____________checkI x1  in return ());
    (let* x139 = return Two in
    let* x140 = return Thr in
    let* x138 = return (Pair (x139, x140)) in
    let* (x141, x1) = match x0 with
      | Cons (y141, y1) ->  return (y141, y1)
      | _ -> mzero in
    let* _ = guard (x141 == x138) in
    let* _ = ______________________checkI x1  in return ())]
and __________checkI x0  =
  msum
    [(let* x119 = return Two in
    let* x120 = return One in
    let* x118 = return (Pair (x119, x120)) in
    let* (x121, x1) = match x0 with
      | Cons (y121, y1) ->  return (y121, y1)
      | _ -> mzero in
    let* _ = guard (x121 == x118) in
    let* _ = _________checkI x1  in return ());
    (let* x123 = return Thr in
    let* x124 = return One in
    let* x122 = return (Pair (x123, x124)) in
    let* (x125, x1) = match x0 with
      | Cons (y125, y1) ->  return (y125, y1)
      | _ -> mzero in
    let* _ = guard (x125 == x122) in
    let* _ = ___________checkI x1  in return ());
    (let* x127 = return Two in
    let* x128 = return Thr in
    let* x126 = return (Pair (x127, x128)) in
    let* (x129, x1) = match x0 with
      | Cons (y129, y1) ->  return (y129, y1)
      | _ -> mzero in
    let* _ = guard (x129 == x126) in
    let* _ = _______________________checkI x1  in return ())]
and _______________________checkI x0  =
  msum
    [(let* _ = guard (x0 == Nil) in return ());
    (let* x271 = return Thr in
    let* x272 = return One in
    let* x270 = return (Pair (x271, x272)) in
    let* (x273, x1) = match x0 with
      | Cons (y273, y1) ->  return (y273, y1)
      | _ -> mzero in
    let* _ = guard (x273 == x270) in
    let* _ = _________checkI x1  in return ());
    (let* x275 = return Thr in
    let* x276 = return Two in
    let* x274 = return (Pair (x275, x276)) in
    let* (x277, x1) = match x0 with
      | Cons (y277, y1) ->  return (y277, y1)
      | _ -> mzero in
    let* _ = guard (x277 == x274) in
    let* _ = __________checkI x1  in return ())]
and _________checkI x0  =
  msum
    [(let* x107 = return One in
    let* x108 = return Two in
    let* x106 = return (Pair (x107, x108)) in
    let* (x109, x1) = match x0 with
      | Cons (y109, y1) ->  return (y109, y1)
      | _ -> mzero in
    let* _ = guard (x109 == x106) in
    let* _ = __________checkI x1  in return ());
    (let* x111 = return Thr in
    let* x112 = return Two in
    let* x110 = return (Pair (x111, x112)) in
    let* (x113, x1) = match x0 with
      | Cons (y113, y1) ->  return (y113, y1)
      | _ -> mzero in
    let* _ = guard (x113 == x110) in
    let* _ = ________checkI x1  in return ());
    (let* x115 = return One in
    let* x116 = return Thr in
    let* x114 = return (Pair (x115, x116)) in
    let* (x117, x1) = match x0 with
      | Cons (y117, y1) ->  return (y117, y1)
      | _ -> mzero in
    let* _ = guard (x117 == x114) in
    let* _ = _______________________checkI x1  in return ())]
and ________checkI x0  =
  msum
    [(let* x95 = return One in
    let* x96 = return Two in
    let* x94 = return (Pair (x95, x96)) in
    let* (x97, x1) = match x0 with
      | Cons (y97, y1) ->  return (y97, y1)
      | _ -> mzero in
    let* _ = guard (x97 == x94) in
    let* _ = _______checkI x1  in return ());
    (let* x99 = return One in
    let* x100 = return Thr in
    let* x98 = return (Pair (x99, x100)) in
    let* (x101, x1) = match x0 with
      | Cons (y101, y1) ->  return (y101, y1)
      | _ -> mzero in
    let* _ = guard (x101 == x98) in
    let* _ = ________________________checkI x1  in return ());
    (let* x103 = return Two in
    let* x104 = return Thr in
    let* x102 = return (Pair (x103, x104)) in
    let* (x105, x1) = match x0 with
      | Cons (y105, y1) ->  return (y105, y1)
      | _ -> mzero in
    let* _ = guard (x105 == x102) in
    let* _ = _________checkI x1  in return ())]
and ____checkI x0  =
  msum
    [(let* x47 = return One in
    let* x48 = return Thr in
    let* x46 = return (Pair (x47, x48)) in
    let* (x49, x1) = match x0 with
      | Cons (y49, y1) ->  return (y49, y1)
      | _ -> mzero in
    let* _ = guard (x49 == x46) in
    let* _ = _____checkI x1  in return ());
    (let* x51 = return Two in
    let* x52 = return Thr in
    let* x50 = return (Pair (x51, x52)) in
    let* (x53, x1) = match x0 with
      | Cons (y53, y1) ->  return (y53, y1)
      | _ -> mzero in
    let* _ = guard (x53 == x50) in
    let* _ = ___checkI x1  in return ());
    (let* x55 = return One in
    let* x56 = return Two in
    let* x54 = return (Pair (x55, x56)) in
    let* (x57, x1) = match x0 with
      | Cons (y57, y1) ->  return (y57, y1)
      | _ -> mzero in
    let* _ = guard (x57 == x54) in
    let* _ = ______checkI x1  in return ())]
and ___checkI x0  =
  msum
    [(let* x35 = return One in
    let* x36 = return Two in
    let* x34 = return (Pair (x35, x36)) in
    let* (x37, x1) = match x0 with
      | Cons (y37, y1) ->  return (y37, y1)
      | _ -> mzero in
    let* _ = guard (x37 == x34) in
    let* _ = __checkI x1  in return ());
    (let* x39 = return Thr in
    let* x40 = return Two in
    let* x38 = return (Pair (x39, x40)) in
    let* (x41, x1) = match x0 with
      | Cons (y41, y1) ->  return (y41, y1)
      | _ -> mzero in
    let* _ = guard (x41 == x38) in
    let* _ = ____checkI x1  in return ());
    (let* x43 = return One in
    let* x44 = return Thr in
    let* x42 = return (Pair (x43, x44)) in
    let* (x45, x1) = match x0 with
      | Cons (y45, y1) ->  return (y45, y1)
      | _ -> mzero in
    let* _ = guard (x45 == x42) in
    let* _ = _________________________checkI x1  in return ())]
and __checkI x0  =
  msum
    [(let* x23 = return Two in
    let* x24 = return One in
    let* x22 = return (Pair (x23, x24)) in
    let* (x25, x1) = match x0 with
      | Cons (y25, y1) ->  return (y25, y1)
      | _ -> mzero in
    let* _ = guard (x25 == x22) in
    let* _ = ___checkI x1  in return ());
    (let* x27 = return Two in
    let* x28 = return Thr in
    let* x26 = return (Pair (x27, x28)) in
    let* (x29, x1) = match x0 with
      | Cons (y29, y1) ->  return (y29, y1)
      | _ -> mzero in
    let* _ = guard (x29 == x26) in
    let* _ = _________________________checkI x1  in return ());
    (let* x31 = return Thr in
    let* x32 = return One in
    let* x30 = return (Pair (x31, x32)) in
    let* (x33, x1) = match x0 with
      | Cons (y33, y1) ->  return (y33, y1)
      | _ -> mzero in
    let* _ = guard (x33 == x30) in
    let* _ = _checkI x1  in return ())]
and _checkI x0  =
  msum
    [(let* x11 = return One in
    let* x12 = return Thr in
    let* x10 = return (Pair (x11, x12)) in
    let* (x13, x1) = match x0 with
      | Cons (y13, y1) ->  return (y13, y1)
      | _ -> mzero in
    let* _ = guard (x13 == x10) in
    let* _ = __checkI x1  in return ());
    (let* x15 = return Two in
    let* x16 = return Thr in
    let* x14 = return (Pair (x15, x16)) in
    let* (x17, x1) = match x0 with
      | Cons (y17, y1) ->  return (y17, y1)
      | _ -> mzero in
    let* _ = guard (x17 == x14) in
    let* _ = __________________________checkI x1  in return ());
    (let* x19 = return Two in
    let* x20 = return One in
    let* x18 = return (Pair (x19, x20)) in
    let* (x21, x1) = match x0 with
      | Cons (y21, y1) ->  return (y21, y1)
      | _ -> mzero in
    let* _ = guard (x21 == x18) in
    let* _ = checkI x1  in return ())]
and checkO   =
  msum
    [(let* x3 = return One in
    let* x4 = return Two in
    let* x2 = return (Pair (x3, x4)) in
    let* x5 = return x2 in
    let* x1 = _checkO   in
    let* x0 = return (Cons (x5, x1)) in return x0);
    (let* x7 = return One in
    let* x8 = return Thr in
    let* x6 = return (Pair (x7, x8)) in
    let* x9 = return x6 in
    let* x1 = __________________________checkO   in
    let* x0 = return (Cons (x9, x1)) in return x0)]
and __________________________checkO   =
  msum
    [(let* x303 = return One in
    let* x304 = return Two in
    let* x302 = return (Pair (x303, x304)) in
    let* x305 = return x302 in
    let* x1 = _____checkO   in
    let* x0 = return (Cons (x305, x1)) in return x0);
    (let* x307 = return Thr in
    let* x308 = return Two in
    let* x306 = return (Pair (x307, x308)) in
    let* x309 = return x306 in
    let* x1 = _checkO   in
    let* x0 = return (Cons (x309, x1)) in return x0);
    (let* x311 = return Thr in
    let* x312 = return One in
    let* x310 = return (Pair (x311, x312)) in
    let* x313 = return x310 in
    let* x1 = checkO   in
    let* x0 = return (Cons (x313, x1)) in return x0)]
and _____checkO   =
  msum
    [(let* x59 = return Two in
    let* x60 = return One in
    let* x58 = return (Pair (x59, x60)) in
    let* x61 = return x58 in
    let* x1 = __________________________checkO   in
    let* x0 = return (Cons (x61, x1)) in return x0);
    (let* x63 = return Thr in
    let* x64 = return One in
    let* x62 = return (Pair (x63, x64)) in
    let* x65 = return x62 in
    let* x1 = ____checkO   in
    let* x0 = return (Cons (x65, x1)) in return x0);
    (let* x67 = return Thr in
    let* x68 = return Two in
    let* x66 = return (Pair (x67, x68)) in
    let* x69 = return x66 in
    let* x1 = ______checkO   in
    let* x0 = return (Cons (x69, x1)) in return x0)]
and ______checkO   =
  msum
    [(let* x71 = return One in
    let* x72 = return Thr in
    let* x70 = return (Pair (x71, x72)) in
    let* x73 = return x70 in
    let* x1 = _______checkO   in
    let* x0 = return (Cons (x73, x1)) in return x0);
    (let* x75 = return Two in
    let* x76 = return Thr in
    let* x74 = return (Pair (x75, x76)) in
    let* x77 = return x74 in
    let* x1 = _____checkO   in
    let* x0 = return (Cons (x77, x1)) in return x0);
    (let* x79 = return Two in
    let* x80 = return One in
    let* x78 = return (Pair (x79, x80)) in
    let* x81 = return x78 in
    let* x1 = ____checkO   in
    let* x0 = return (Cons (x81, x1)) in return x0)]
and _______checkO   =
  msum
    [(let* x83 = return Two in
    let* x84 = return One in
    let* x82 = return (Pair (x83, x84)) in
    let* x85 = return x82 in
    let* x1 = ________checkO   in
    let* x0 = return (Cons (x85, x1)) in return x0);
    (let* x87 = return Thr in
    let* x88 = return One in
    let* x86 = return (Pair (x87, x88)) in
    let* x89 = return x86 in
    let* x1 = ______checkO   in
    let* x0 = return (Cons (x89, x1)) in return x0);
    (let* x91 = return Two in
    let* x92 = return Thr in
    let* x90 = return (Pair (x91, x92)) in
    let* x93 = return x90 in
    let* x1 = ________________________checkO   in
    let* x0 = return (Cons (x93, x1)) in return x0)]
and ________________________checkO   =
  msum
    [(let* x279 = return Two in
    let* x280 = return One in
    let* x278 = return (Pair (x279, x280)) in
    let* x281 = return x278 in
    let* x1 = ______________________checkO   in
    let* x0 = return (Cons (x281, x1)) in return x0);
    (let* x283 = return Thr in
    let* x284 = return One in
    let* x282 = return (Pair (x283, x284)) in
    let* x285 = return x282 in
    let* x1 = ________checkO   in
    let* x0 = return (Cons (x285, x1)) in return x0);
    (let* x287 = return Thr in
    let* x288 = return Two in
    let* x286 = return (Pair (x287, x288)) in
    let* x289 = return x286 in
    let* x1 = _______checkO   in
    let* x0 = return (Cons (x289, x1)) in return x0)]
and ______________________checkO   =
  msum
    [(let* x259 = return One in
    let* x260 = return Two in
    let* x258 = return (Pair (x259, x260)) in
    let* x261 = return x258 in
    let* x1 = ________________________checkO   in
    let* x0 = return (Cons (x261, x1)) in return x0);
    (let* x263 = return Thr in
    let* x264 = return Two in
    let* x262 = return (Pair (x263, x264)) in
    let* x265 = return x262 in
    let* x1 = ___________checkO   in
    let* x0 = return (Cons (x265, x1)) in return x0);
    (let* x267 = return Thr in
    let* x268 = return One in
    let* x266 = return (Pair (x267, x268)) in
    let* x269 = return x266 in
    let* x1 = ____________checkO   in
    let* x0 = return (Cons (x269, x1)) in return x0)]
and ____________checkO   =
  msum
    [(let* x143 = return One in
    let* x144 = return Two in
    let* x142 = return (Pair (x143, x144)) in
    let* x145 = return x142 in
    let* x1 = ___________checkO   in
    let* x0 = return (Cons (x145, x1)) in return x0);
    (let* x147 = return Thr in
    let* x148 = return Two in
    let* x146 = return (Pair (x147, x148)) in
    let* x149 = return x146 in
    let* x1 = _____________checkO   in
    let* x0 = return (Cons (x149, x1)) in return x0);
    (let* x151 = return One in
    let* x152 = return Thr in
    let* x150 = return (Pair (x151, x152)) in
    let* x153 = return x150 in
    let* x1 = ______________________checkO   in
    let* x0 = return (Cons (x153, x1)) in return x0)]
and _____________checkO   =
  msum
    [(let* x155 = return One in
    let* x156 = return Thr in
    let* x154 = return (Pair (x155, x156)) in
    let* x157 = return x154 in
    let* x1 = ______________checkO   in
    let* x0 = return (Cons (x157, x1)) in return x0);
    (let* x159 = return Two in
    let* x160 = return Thr in
    let* x158 = return (Pair (x159, x160)) in
    let* x161 = return x158 in
    let* x1 = ____________checkO   in
    let* x0 = return (Cons (x161, x1)) in return x0);
    (let* x163 = return One in
    let* x164 = return Two in
    let* x162 = return (Pair (x163, x164)) in
    let* x165 = return x162 in
    let* x1 = _____________________checkO   in
    let* x0 = return (Cons (x165, x1)) in return x0)]
and _____________________checkO   =
  msum
    [(let* x247 = return One in
    let* x248 = return Thr in
    let* x246 = return (Pair (x247, x248)) in
    let* x249 = return x246 in
    let* x1 = __________________checkO   in
    let* x0 = return (Cons (x249, x1)) in return x0);
    (let* x251 = return Two in
    let* x252 = return Thr in
    let* x250 = return (Pair (x251, x252)) in
    let* x253 = return x250 in
    let* x1 = ______________checkO   in
    let* x0 = return (Cons (x253, x1)) in return x0);
    (let* x255 = return Two in
    let* x256 = return One in
    let* x254 = return (Pair (x255, x256)) in
    let* x257 = return x254 in
    let* x1 = _____________checkO   in
    let* x0 = return (Cons (x257, x1)) in return x0)]
and __________________checkO   =
  msum
    [(let* x215 = return Two in
    let* x216 = return One in
    let* x214 = return (Pair (x215, x216)) in
    let* x217 = return x214 in
    let* x1 = _________________checkO   in
    let* x0 = return (Cons (x217, x1)) in return x0);
    (let* x219 = return Thr in
    let* x220 = return One in
    let* x218 = return (Pair (x219, x220)) in
    let* x221 = return x218 in
    let* x1 = _____________________checkO   in
    let* x0 = return (Cons (x221, x1)) in return x0);
    (let* x223 = return Two in
    let* x224 = return Thr in
    let* x222 = return (Pair (x223, x224)) in
    let* x225 = return x222 in
    let* x1 = ___________________checkO   in
    let* x0 = return (Cons (x225, x1)) in return x0)]
and ___________________checkO   =
  msum
    [(let* x227 = return Two in
    let* x228 = return One in
    let* x226 = return (Pair (x227, x228)) in
    let* x229 = return x226 in
    let* x1 = _________________________checkO   in
    let* x0 = return (Cons (x229, x1)) in return x0);
    (let* x231 = return Thr in
    let* x232 = return One in
    let* x230 = return (Pair (x231, x232)) in
    let* x233 = return x230 in
    let* x1 = _________________checkO   in
    let* x0 = return (Cons (x233, x1)) in return x0);
    (let* x235 = return Thr in
    let* x236 = return Two in
    let* x234 = return (Pair (x235, x236)) in
    let* x237 = return x234 in
    let* x1 = __________________checkO   in
    let* x0 = return (Cons (x237, x1)) in return x0)]
and _________________________checkO   =
  msum
    [(let* x291 = return One in
    let* x292 = return Two in
    let* x290 = return (Pair (x291, x292)) in
    let* x293 = return x290 in
    let* x1 = ___________________checkO   in
    let* x0 = return (Cons (x293, x1)) in return x0);
    (let* x295 = return Thr in
    let* x296 = return Two in
    let* x294 = return (Pair (x295, x296)) in
    let* x297 = return x294 in
    let* x1 = __checkO   in
    let* x0 = return (Cons (x297, x1)) in return x0);
    (let* x299 = return Thr in
    let* x300 = return One in
    let* x298 = return (Pair (x299, x300)) in
    let* x301 = return x298 in
    let* x1 = ___checkO   in
    let* x0 = return (Cons (x301, x1)) in return x0)]
and _________________checkO   =
  msum
    [(let* x203 = return One in
    let* x204 = return Two in
    let* x202 = return (Pair (x203, x204)) in
    let* x205 = return x202 in
    let* x1 = __________________checkO   in
    let* x0 = return (Cons (x205, x1)) in return x0);
    (let* x207 = return One in
    let* x208 = return Thr in
    let* x206 = return (Pair (x207, x208)) in
    let* x209 = return x206 in
    let* x1 = ___________________checkO   in
    let* x0 = return (Cons (x209, x1)) in return x0);
    (let* x211 = return Thr in
    let* x212 = return Two in
    let* x210 = return (Pair (x211, x212)) in
    let* x213 = return x210 in
    let* x1 = ________________checkO   in
    let* x0 = return (Cons (x213, x1)) in return x0)]
and ________________checkO   =
  msum
    [(let* x191 = return One in
    let* x192 = return Thr in
    let* x190 = return (Pair (x191, x192)) in
    let* x193 = return x190 in
    let* x1 = _______________checkO   in
    let* x0 = return (Cons (x193, x1)) in return x0);
    (let* x195 = return Two in
    let* x196 = return Thr in
    let* x194 = return (Pair (x195, x196)) in
    let* x197 = return x194 in
    let* x1 = _________________checkO   in
    let* x0 = return (Cons (x197, x1)) in return x0);
    (let* x199 = return One in
    let* x200 = return Two in
    let* x198 = return (Pair (x199, x200)) in
    let* x201 = return x198 in
    let* x1 = ____________________checkO   in
    let* x0 = return (Cons (x201, x1)) in return x0)]
and ____________________checkO   =
  msum
    [(let* x239 = return Two in
    let* x240 = return One in
    let* x238 = return (Pair (x239, x240)) in
    let* x241 = return x238 in
    let* x1 = ________________checkO   in
    let* x0 = return (Cons (x241, x1)) in return x0);
    (let* x243 = return Two in
    let* x244 = return Thr in
    let* x242 = return (Pair (x243, x244)) in
    let* x245 = return x242 in
    let* x1 = _______________checkO   in
    let* x0 = return (Cons (x245, x1)) in return x0)]
and _______________checkO   =
  msum
    [(let* x179 = return Two in
    let* x180 = return One in
    let* x178 = return (Pair (x179, x180)) in
    let* x181 = return x178 in
    let* x1 = ______________checkO   in
    let* x0 = return (Cons (x181, x1)) in return x0);
    (let* x183 = return Thr in
    let* x184 = return One in
    let* x182 = return (Pair (x183, x184)) in
    let* x185 = return x182 in
    let* x1 = ________________checkO   in
    let* x0 = return (Cons (x185, x1)) in return x0);
    (let* x187 = return Thr in
    let* x188 = return Two in
    let* x186 = return (Pair (x187, x188)) in
    let* x189 = return x186 in
    let* x1 = ____________________checkO   in
    let* x0 = return (Cons (x189, x1)) in return x0)]
and ______________checkO   =
  msum
    [(let* x167 = return One in
    let* x168 = return Two in
    let* x166 = return (Pair (x167, x168)) in
    let* x169 = return x166 in
    let* x1 = _______________checkO   in
    let* x0 = return (Cons (x169, x1)) in return x0);
    (let* x171 = return Thr in
    let* x172 = return One in
    let* x170 = return (Pair (x171, x172)) in
    let* x173 = return x170 in
    let* x1 = _____________checkO   in
    let* x0 = return (Cons (x173, x1)) in return x0);
    (let* x175 = return Thr in
    let* x176 = return Two in
    let* x174 = return (Pair (x175, x176)) in
    let* x177 = return x174 in
    let* x1 = _____________________checkO   in
    let* x0 = return (Cons (x177, x1)) in return x0)]
and ___________checkO   =
  msum
    [(let* x131 = return One in
    let* x132 = return Thr in
    let* x130 = return (Pair (x131, x132)) in
    let* x133 = return x130 in
    let* x1 = __________checkO   in
    let* x0 = return (Cons (x133, x1)) in return x0);
    (let* x135 = return Two in
    let* x136 = return One in
    let* x134 = return (Pair (x135, x136)) in
    let* x137 = return x134 in
    let* x1 = ____________checkO   in
    let* x0 = return (Cons (x137, x1)) in return x0);
    (let* x139 = return Two in
    let* x140 = return Thr in
    let* x138 = return (Pair (x139, x140)) in
    let* x141 = return x138 in
    let* x1 = ______________________checkO   in
    let* x0 = return (Cons (x141, x1)) in return x0)]
and __________checkO   =
  msum
    [(let* x119 = return Two in
    let* x120 = return One in
    let* x118 = return (Pair (x119, x120)) in
    let* x121 = return x118 in
    let* x1 = _________checkO   in
    let* x0 = return (Cons (x121, x1)) in return x0);
    (let* x123 = return Thr in
    let* x124 = return One in
    let* x122 = return (Pair (x123, x124)) in
    let* x125 = return x122 in
    let* x1 = ___________checkO   in
    let* x0 = return (Cons (x125, x1)) in return x0);
    (let* x127 = return Two in
    let* x128 = return Thr in
    let* x126 = return (Pair (x127, x128)) in
    let* x129 = return x126 in
    let* x1 = _______________________checkO   in
    let* x0 = return (Cons (x129, x1)) in return x0)]
and _______________________checkO   =
  msum
    [(let* x0 = return Nil in return x0);
    (let* x271 = return Thr in
    let* x272 = return One in
    let* x270 = return (Pair (x271, x272)) in
    let* x273 = return x270 in
    let* x1 = _________checkO   in
    let* x0 = return (Cons (x273, x1)) in return x0);
    (let* x275 = return Thr in
    let* x276 = return Two in
    let* x274 = return (Pair (x275, x276)) in
    let* x277 = return x274 in
    let* x1 = __________checkO   in
    let* x0 = return (Cons (x277, x1)) in return x0)]
and _________checkO   =
  msum
    [(let* x107 = return One in
    let* x108 = return Two in
    let* x106 = return (Pair (x107, x108)) in
    let* x109 = return x106 in
    let* x1 = __________checkO   in
    let* x0 = return (Cons (x109, x1)) in return x0);
    (let* x111 = return Thr in
    let* x112 = return Two in
    let* x110 = return (Pair (x111, x112)) in
    let* x113 = return x110 in
    let* x1 = ________checkO   in
    let* x0 = return (Cons (x113, x1)) in return x0);
    (let* x115 = return One in
    let* x116 = return Thr in
    let* x114 = return (Pair (x115, x116)) in
    let* x117 = return x114 in
    let* x1 = _______________________checkO   in
    let* x0 = return (Cons (x117, x1)) in return x0)]
and ________checkO   =
  msum
    [(let* x95 = return One in
    let* x96 = return Two in
    let* x94 = return (Pair (x95, x96)) in
    let* x97 = return x94 in
    let* x1 = _______checkO   in
    let* x0 = return (Cons (x97, x1)) in return x0);
    (let* x99 = return One in
    let* x100 = return Thr in
    let* x98 = return (Pair (x99, x100)) in
    let* x101 = return x98 in
    let* x1 = ________________________checkO   in
    let* x0 = return (Cons (x101, x1)) in return x0);
    (let* x103 = return Two in
    let* x104 = return Thr in
    let* x102 = return (Pair (x103, x104)) in
    let* x105 = return x102 in
    let* x1 = _________checkO   in
    let* x0 = return (Cons (x105, x1)) in return x0)]
and ____checkO   =
  msum
    [(let* x47 = return One in
    let* x48 = return Thr in
    let* x46 = return (Pair (x47, x48)) in
    let* x49 = return x46 in
    let* x1 = _____checkO   in
    let* x0 = return (Cons (x49, x1)) in return x0);
    (let* x51 = return Two in
    let* x52 = return Thr in
    let* x50 = return (Pair (x51, x52)) in
    let* x53 = return x50 in
    let* x1 = ___checkO   in
    let* x0 = return (Cons (x53, x1)) in return x0);
    (let* x55 = return One in
    let* x56 = return Two in
    let* x54 = return (Pair (x55, x56)) in
    let* x57 = return x54 in
    let* x1 = ______checkO   in
    let* x0 = return (Cons (x57, x1)) in return x0)]
and ___checkO   =
  msum
    [(let* x35 = return One in
    let* x36 = return Two in
    let* x34 = return (Pair (x35, x36)) in
    let* x37 = return x34 in
    let* x1 = __checkO   in
    let* x0 = return (Cons (x37, x1)) in return x0);
    (let* x39 = return Thr in
    let* x40 = return Two in
    let* x38 = return (Pair (x39, x40)) in
    let* x41 = return x38 in
    let* x1 = ____checkO   in
    let* x0 = return (Cons (x41, x1)) in return x0);
    (let* x43 = return One in
    let* x44 = return Thr in
    let* x42 = return (Pair (x43, x44)) in
    let* x45 = return x42 in
    let* x1 = _________________________checkO   in
    let* x0 = return (Cons (x45, x1)) in return x0)]
and __checkO   =
  msum
    [(let* x23 = return Two in
    let* x24 = return One in
    let* x22 = return (Pair (x23, x24)) in
    let* x25 = return x22 in
    let* x1 = ___checkO   in
    let* x0 = return (Cons (x25, x1)) in return x0);
    (let* x27 = return Two in
    let* x28 = return Thr in
    let* x26 = return (Pair (x27, x28)) in
    let* x29 = return x26 in
    let* x1 = _________________________checkO   in
    let* x0 = return (Cons (x29, x1)) in return x0);
    (let* x31 = return Thr in
    let* x32 = return One in
    let* x30 = return (Pair (x31, x32)) in
    let* x33 = return x30 in
    let* x1 = _checkO   in
    let* x0 = return (Cons (x33, x1)) in return x0)]
and _checkO   =
  msum
    [(let* x11 = return One in
    let* x12 = return Thr in
    let* x10 = return (Pair (x11, x12)) in
    let* x13 = return x10 in
    let* x1 = __checkO   in
    let* x0 = return (Cons (x13, x1)) in return x0);
    (let* x15 = return Two in
    let* x16 = return Thr in
    let* x14 = return (Pair (x15, x16)) in
    let* x17 = return x14 in
    let* x1 = __________________________checkO   in
    let* x0 = return (Cons (x17, x1)) in return x0);
    (let* x19 = return Two in
    let* x20 = return One in
    let* x18 = return (Pair (x19, x20)) in
    let* x21 = return x18 in
    let* x1 = checkO   in
    let* x0 = return (Cons (x21, x1)) in return x0)]