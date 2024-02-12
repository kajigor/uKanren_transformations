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
  | Cabbage
  | Cons of (term * term)
  | Empty
  | False
  | Goat
  | Nil
  | Pair of (term * term)
  | Quad of (term * term * term * term)
  | True
  | Wolf

let rec evalI x0  =
  msum
    [(let* (x1, x2) = match x0 with
      | Cons (y1, y2) ->  return (y1, y2)
      | _ -> mzero in
    let* _ = step_EvalII x1 x2  in return ())]
and evalO   =
  msum
    [(let* (x1, x2) = step_EvalOO   in
    let* x0 = return (Cons (x1, x2)) in return x0)]
and step_EvalII x0 x1  =
  msum
    [(let* x11 = return True in
    let* x12 = return True in
    let* x13 = return True in
    let* _ = _________safe_III x11 x12 x13  in
    let* x14 = return False in
    let* x15 = return False in
    let* x16 = return False in
    let* _ = __________safe_III x14 x15 x16  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x17 = return False in
    let* x18 = return True in
    let* x19 = return True in
    let* _ = _________safe_III x17 x18 x19  in
    let* x20 = return True in
    let* x21 = return False in
    let* x22 = return False in
    let* _ = __________safe_III x20 x21 x22  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = __stepEvalII x4 x5  in
    let* _ = guard (x0 == Goat) in return ());
    (let* x23 = return True in
    let* x24 = return False in
    let* x25 = return True in
    let* _ = _________safe_III x23 x24 x25  in
    let* x26 = return False in
    let* x27 = return True in
    let* x28 = return False in
    let* _ = __________safe_III x26 x27 x28  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ________________stepEvalII x4 x5  in
    let* _ = guard (x0 == Wolf) in return ());
    (let* x29 = return True in
    let* x30 = return True in
    let* x31 = return False in
    let* _ = _________safe_III x29 x30 x31  in
    let* x32 = return False in
    let* x33 = return False in
    let* x34 = return True in
    let* _ = __________safe_III x32 x33 x34  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ____________________stepEvalII x4 x5  in
    let* _ = guard (x0 == Cabbage) in return ())]
and ____________________stepEvalII x0 x1  =
  msum
    [(let* _ = __________________step_EvalII x0 x1  in return ())]
and __________________step_EvalII x0 x1  =
  msum
    [(let* x352 = return False in
    let* x353 = return False in
    let* x354 = return True in
    let* _ = _________safe_III x352 x353 x354  in
    let* x355 = return True in
    let* x356 = return True in
    let* x357 = return False in
    let* _ = __________safe_III x355 x356 x357  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = ______________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x358 = return False in
    let* x359 = return False in
    let* x360 = return False in
    let* _ = _________safe_III x358 x359 x360  in
    let* x361 = return True in
    let* x362 = return True in
    let* x363 = return True in
    let* _ = __________safe_III x361 x362 x363  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = _stepEvalII x4 x5  in
    let* _ = guard (x0 == Cabbage) in return ())]
and ________________stepEvalII x0 x1  =
  msum
    [(let* _ = ______________step_EvalII x0 x1  in return ())]
and ______________stepEvalII x0 x1  =
  msum
    [(let* _ = _____________step_EvalII x0 x1  in return ())]
and ______________step_EvalII x0 x1  =
  msum
    [(let* x281 = return False in
    let* x282 = return True in
    let* x283 = return False in
    let* _ = _________safe_III x281 x282 x283  in
    let* x284 = return True in
    let* x285 = return False in
    let* x286 = return True in
    let* _ = __________safe_III x284 x285 x286  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = _________________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x287 = return False in
    let* x288 = return False in
    let* x289 = return False in
    let* _ = _________safe_III x287 x288 x289  in
    let* x290 = return False in
    let* x291 = return True in
    let* x292 = return True in
    let* _ = __________safe_III x290 x291 x292  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = __________stepEvalII x4 x5  in
    let* _ = guard (x0 == Wolf) in return ())]
and _________________stepEvalII x0 x1  =
  msum
    [(let* _ = _______________step_EvalII x0 x1  in return ())]
and _______________step_EvalII x0 x1  =
  msum
    [(let* x293 = return True in
    let* x294 = return False in
    let* x295 = return True in
    let* _ = _________safe_III x293 x294 x295  in
    let* x296 = return False in
    let* x297 = return True in
    let* x298 = return False in
    let* _ = __________safe_III x296 x297 x298  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = ________________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x299 = return False in
    let* x300 = return False in
    let* x301 = return True in
    let* _ = _________safe_III x299 x300 x301  in
    let* x302 = return True in
    let* x303 = return True in
    let* x304 = return False in
    let* _ = __________safe_III x302 x303 x304  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = __________________stepEvalII x4 x5  in
    let* _ = guard (x0 == Goat) in return ());
    (let* x305 = return True in
    let* x306 = return False in
    let* x307 = return False in
    let* _ = _________safe_III x305 x306 x307  in
    let* x308 = return False in
    let* x309 = return True in
    let* x310 = return True in
    let* _ = __________safe_III x308 x309 x310  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = _______________stepEvalII x4 x5  in
    let* _ = guard (x0 == Cabbage) in return ())]
and __________________stepEvalII x0 x1  =
  msum
    [(let* _ = ________________step_EvalII x0 x1  in return ())]
and ________________step_EvalII x0 x1  =
  msum
    [(let* x311 = return True in
    let* x312 = return True in
    let* x313 = return False in
    let* _ = _________safe_III x311 x312 x313  in
    let* x314 = return False in
    let* x315 = return False in
    let* x316 = return True in
    let* _ = __________safe_III x314 x315 x316  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = ___________________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x317 = return False in
    let* x318 = return True in
    let* x319 = return False in
    let* _ = _________safe_III x317 x318 x319  in
    let* x320 = return True in
    let* x321 = return False in
    let* x322 = return True in
    let* _ = __________safe_III x320 x321 x322  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = _________________stepEvalII x4 x5  in
    let* _ = guard (x0 == Goat) in return ());
    (let* x323 = return True in
    let* x324 = return False in
    let* x325 = return False in
    let* _ = _________safe_III x323 x324 x325  in
    let* x326 = return False in
    let* x327 = return True in
    let* x328 = return True in
    let* _ = __________safe_III x326 x327 x328  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ___stepEvalII x4 x5  in
    let* _ = guard (x0 == Wolf) in return ())]
and ___________________stepEvalII x0 x1  =
  msum
    [(let* _ = _________________step_EvalII x0 x1  in return ())]
and _________________step_EvalII x0 x1  =
  msum
    [(let* x329 = return False in
    let* x330 = return False in
    let* x331 = return True in
    let* _ = _________safe_III x329 x330 x331  in
    let* x332 = return True in
    let* x333 = return True in
    let* x334 = return False in
    let* _ = __________safe_III x332 x333 x334  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = __________________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x335 = return False in
    let* x336 = return False in
    let* x337 = return False in
    let* _ = _________safe_III x335 x336 x337  in
    let* x338 = return True in
    let* x339 = return True in
    let* x340 = return True in
    let* _ = __________safe_III x338 x339 x340  in
    let* x343 = return False in
    let* x344 = return False in
    let* x345 = return False in
    let* x346 = return False in
    let* x342 = return (Quad (x343, x344, x345, x346)) in
    let* x348 = return True in
    let* x349 = return True in
    let* x350 = return True in
    let* x351 = return True in
    let* x347 = return (Quad (x348, x349, x350, x351)) in
    let* x341 = return (Pair (x342, x347)) in
    let* _ = _evalII x341 x1  in
    let* _ = guard (x0 == Cabbage) in return ())]
and _______________stepEvalII x0 x1  =
  msum
    [(let* x274 = return False in
    let* x275 = return True in
    let* x276 = return True in
    let* x277 = return True in
    let* x278 = return False in
    let* x279 = return False in
    let* x273 = _step_IOIIIIII x0 x274 x275 x276 x277 x278 x279  in
    let* (x2, x3) = match x273 with
      | Pair (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* x280 = return (Pair (x3, x2)) in
    let* _ = _evalII x280 x1  in return ())]
and _____________step_EvalII x0 x1  =
  msum
    [(let* x255 = return True in
    let* x256 = return True in
    let* x257 = return False in
    let* _ = _________safe_III x255 x256 x257  in
    let* x258 = return False in
    let* x259 = return False in
    let* x260 = return True in
    let* _ = __________safe_III x258 x259 x260  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = ____________________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x261 = return False in
    let* x262 = return True in
    let* x263 = return False in
    let* _ = _________safe_III x261 x262 x263  in
    let* x264 = return True in
    let* x265 = return False in
    let* x266 = return True in
    let* _ = __________safe_III x264 x265 x266  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ____________stepEvalII x4 x5  in
    let* _ = guard (x0 == Goat) in return ());
    (let* x267 = return True in
    let* x268 = return False in
    let* x269 = return False in
    let* _ = _________safe_III x267 x268 x269  in
    let* x270 = return False in
    let* x271 = return True in
    let* x272 = return True in
    let* _ = __________safe_III x270 x271 x272  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = _______________stepEvalII x4 x5  in
    let* _ = guard (x0 == Wolf) in return ())]
and ____________stepEvalII x0 x1  =
  msum
    [(let* _ = ___________step_EvalII x0 x1  in return ())]
and ___________step_EvalII x0 x1  =
  msum
    [(let* x225 = return True in
    let* x226 = return False in
    let* x227 = return True in
    let* _ = _________safe_III x225 x226 x227  in
    let* x228 = return False in
    let* x229 = return True in
    let* x230 = return False in
    let* _ = __________safe_III x228 x229 x230  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = _____________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x231 = return False in
    let* x232 = return False in
    let* x233 = return True in
    let* _ = _________safe_III x231 x232 x233  in
    let* x234 = return True in
    let* x235 = return True in
    let* x236 = return False in
    let* _ = __________safe_III x234 x235 x236  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ______________stepEvalII x4 x5  in
    let* _ = guard (x0 == Goat) in return ());
    (let* x237 = return True in
    let* x238 = return False in
    let* x239 = return False in
    let* _ = _________safe_III x237 x238 x239  in
    let* x240 = return False in
    let* x241 = return True in
    let* x242 = return True in
    let* _ = __________safe_III x240 x241 x242  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ___stepEvalII x4 x5  in
    let* _ = guard (x0 == Cabbage) in return ())]
and _____________stepEvalII x0 x1  =
  msum
    [(let* _ = ____________step_EvalII x0 x1  in return ())]
and ____________step_EvalII x0 x1  =
  msum
    [(let* x243 = return False in
    let* x244 = return True in
    let* x245 = return False in
    let* _ = _________safe_III x243 x244 x245  in
    let* x246 = return True in
    let* x247 = return False in
    let* x248 = return True in
    let* _ = __________safe_III x246 x247 x248  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = ____________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x249 = return False in
    let* x250 = return False in
    let* x251 = return False in
    let* _ = _________safe_III x249 x250 x251  in
    let* x252 = return False in
    let* x253 = return True in
    let* x254 = return True in
    let* _ = __________safe_III x252 x253 x254  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ______stepEvalII x4 x5  in
    let* _ = guard (x0 == Wolf) in return ())]
and __________safe_III x0 x1 x2  =
  msum
    [(let* x200 = return x0 in
    let* _ = guard (x200 == x0) in return ())]
and __________stepEvalII x0 x1  =
  msum
    [(let* _ = _________step_EvalII x0 x1  in return ())]
and _________safe_III x0 x1 x2  =
  msum
    [(let* _ = guard (x0 == False) in return ());
    (let* _ = guard (x2 == True) in
    let* _ = guard (x1 == True) in
    let* _ = guard (x0 == True) in return ());
    (let* _ = guard (x2 == False) in
    let* _ = guard (x1 == False) in
    let* _ = guard (x0 == True) in return ())]
and _________step_EvalII x0 x1  =
  msum
    [(let* x201 = return False in
    let* x202 = return True in
    let* x203 = return True in
    let* _ = _________safe_III x201 x202 x203  in
    let* x204 = return False in
    let* x205 = return False in
    let* x206 = return False in
    let* _ = __________safe_III x204 x205 x206  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = ___________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x207 = return False in
    let* x208 = return False in
    let* x209 = return True in
    let* _ = _________safe_III x207 x208 x209  in
    let* x210 = return False in
    let* x211 = return True in
    let* x212 = return False in
    let* _ = __________safe_III x210 x211 x212  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ____stepEvalII x4 x5  in
    let* _ = guard (x0 == Wolf) in return ());
    (let* x213 = return False in
    let* x214 = return True in
    let* x215 = return False in
    let* _ = _________safe_III x213 x214 x215  in
    let* x216 = return False in
    let* x217 = return False in
    let* x218 = return True in
    let* _ = __________safe_III x216 x217 x218  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = _________stepEvalII x4 x5  in
    let* _ = guard (x0 == Cabbage) in return ())]
and ___________stepEvalII x0 x1  =
  msum
    [(let* _ = __________step_EvalII x0 x1  in return ())]
and __________step_EvalII x0 x1  =
  msum
    [(let* x219 = return False in
    let* x220 = return False in
    let* x221 = return False in
    let* _ = _________safe_III x219 x220 x221  in
    let* x222 = return False in
    let* x223 = return True in
    let* x224 = return True in
    let* _ = __________safe_III x222 x223 x224  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = __________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ())]
and _________stepEvalII x0 x1  =
  msum
    [(let* x132 = return False in
    let* x133 = return False in
    let* x134 = return True in
    let* x135 = return False in
    let* x136 = return True in
    let* x137 = return False in
    let* x131 = _step_IOIIIIII x0 x132 x133 x134 x135 x136 x137  in
    let* (x2, x3) = match x131 with
      | Pair (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* x138 = return (Pair (x3, x2)) in
    let* _ = _evalII x138 x1  in return ())]
and ______stepEvalII x0 x1  =
  msum
    [(let* _ = ______step_EvalII x0 x1  in return ())]
and ______step_EvalII x0 x1  =
  msum
    [(let* x95 = return False in
    let* x96 = return True in
    let* x97 = return True in
    let* _ = _________safe_III x95 x96 x97  in
    let* x98 = return False in
    let* x99 = return False in
    let* x100 = return False in
    let* _ = __________safe_III x98 x99 x100  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = _______stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x101 = return False in
    let* x102 = return False in
    let* x103 = return True in
    let* _ = _________safe_III x101 x102 x103  in
    let* x104 = return False in
    let* x105 = return True in
    let* x106 = return False in
    let* _ = __________safe_III x104 x105 x106  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ________stepEvalII x4 x5  in
    let* _ = guard (x0 == Wolf) in return ());
    (let* x107 = return False in
    let* x108 = return True in
    let* x109 = return False in
    let* _ = _________safe_III x107 x108 x109  in
    let* x110 = return False in
    let* x111 = return False in
    let* x112 = return True in
    let* _ = __________safe_III x110 x111 x112  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = _____stepEvalII x4 x5  in
    let* _ = guard (x0 == Cabbage) in return ())]
and ________stepEvalII x0 x1  =
  msum
    [(let* _ = ________step_EvalII x0 x1  in return ())]
and ________step_EvalII x0 x1  =
  msum
    [(let* x119 = return False in
    let* x120 = return True in
    let* x121 = return False in
    let* _ = _________safe_III x119 x120 x121  in
    let* x122 = return False in
    let* x123 = return False in
    let* x124 = return True in
    let* _ = __________safe_III x122 x123 x124  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = _________stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x125 = return False in
    let* x126 = return False in
    let* x127 = return False in
    let* _ = _________safe_III x125 x126 x127  in
    let* x128 = return False in
    let* x129 = return True in
    let* x130 = return True in
    let* _ = __________safe_III x128 x129 x130  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ______stepEvalII x4 x5  in
    let* _ = guard (x0 == Wolf) in return ())]
and _______stepEvalII x0 x1  =
  msum
    [(let* _ = _______step_EvalII x0 x1  in return ())]
and _______step_EvalII x0 x1  =
  msum
    [(let* x113 = return False in
    let* x114 = return False in
    let* x115 = return False in
    let* _ = _________safe_III x113 x114 x115  in
    let* x116 = return False in
    let* x117 = return True in
    let* x118 = return True in
    let* _ = __________safe_III x116 x117 x118  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = ______stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ())]
and _____stepEvalII x0 x1  =
  msum
    [(let* _ = _____step_EvalII x0 x1  in return ())]
and _____step_EvalII x0 x1  =
  msum
    [(let* x83 = return False in
    let* x84 = return False in
    let* x85 = return True in
    let* _ = _________safe_III x83 x84 x85  in
    let* x86 = return False in
    let* x87 = return True in
    let* x88 = return False in
    let* _ = __________safe_III x86 x87 x88  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = ____stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x89 = return False in
    let* x90 = return False in
    let* x91 = return False in
    let* _ = _________safe_III x89 x90 x91  in
    let* x92 = return False in
    let* x93 = return True in
    let* x94 = return True in
    let* _ = __________safe_III x92 x93 x94  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ______stepEvalII x4 x5  in
    let* _ = guard (x0 == Cabbage) in return ())]
and ____stepEvalII x0 x1  =
  msum
    [(let* _ = ____step_EvalII x0 x1  in return ())]
and ____step_EvalII x0 x1  =
  msum
    [(let* x71 = return False in
    let* x72 = return True in
    let* x73 = return False in
    let* _ = _________safe_III x71 x72 x73  in
    let* x74 = return False in
    let* x75 = return False in
    let* x76 = return True in
    let* _ = __________safe_III x74 x75 x76  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = _____stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x77 = return False in
    let* x78 = return False in
    let* x79 = return False in
    let* _ = _________safe_III x77 x78 x79  in
    let* x80 = return False in
    let* x81 = return True in
    let* x82 = return True in
    let* _ = __________safe_III x80 x81 x82  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = __________stepEvalII x4 x5  in
    let* _ = guard (x0 == Wolf) in return ())]
and ___stepEvalII x0 x1  =
  msum
    [(let* _ = ___step_EvalII x0 x1  in return ())]
and ___step_EvalII x0 x1  =
  msum
    [(let* x53 = return False in
    let* x54 = return True in
    let* x55 = return True in
    let* _ = _________safe_III x53 x54 x55  in
    let* x56 = return True in
    let* x57 = return False in
    let* x58 = return False in
    let* _ = __________safe_III x56 x57 x58  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = __stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x59 = return False in
    let* x60 = return False in
    let* x61 = return True in
    let* _ = _________safe_III x59 x60 x61  in
    let* x62 = return False in
    let* x63 = return True in
    let* x64 = return False in
    let* _ = __________safe_III x62 x63 x64  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ____stepEvalII x4 x5  in
    let* _ = guard (x0 == Wolf) in return ());
    (let* x65 = return False in
    let* x66 = return True in
    let* x67 = return False in
    let* _ = _________safe_III x65 x66 x67  in
    let* x68 = return True in
    let* x69 = return False in
    let* x70 = return True in
    let* _ = __________safe_III x68 x69 x70  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = ____________stepEvalII x4 x5  in
    let* _ = guard (x0 == Cabbage) in return ())]
and __stepEvalII x0 x1  =
  msum
    [(let* _ = __step_EvalII x0 x1  in return ())]
and __step_EvalII x0 x1  =
  msum
    [(let* x41 = return True in
    let* x42 = return False in
    let* x43 = return False in
    let* _ = _________safe_III x41 x42 x43  in
    let* x44 = return False in
    let* x45 = return True in
    let* x46 = return True in
    let* _ = __________safe_III x44 x45 x46  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = ___stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ());
    (let* x47 = return False in
    let* x48 = return False in
    let* x49 = return False in
    let* _ = _________safe_III x47 x48 x49  in
    let* x50 = return True in
    let* x51 = return True in
    let* x52 = return True in
    let* _ = __________safe_III x50 x51 x52  in
    let* (x4, x5) = match x1 with
      | Cons (y4, y5) ->  return (y4, y5)
      | _ -> mzero in
    let* _ = _stepEvalII x4 x5  in
    let* _ = guard (x0 == Goat) in return ())]
and _evalII x0 x1  =
  msum
    [(let* x140 = return False in
    let* x141 = return False in
    let* x142 = return False in
    let* x143 = return False in
    let* x139 = return (Quad (x140, x141, x142, x143)) in
    let* x145 = return True in
    let* x146 = return True in
    let* x147 = return True in
    let* x148 = return True in
    let* x144 = return (Quad (x145, x146, x147, x148)) in
    let* _ = guard (x1 == Nil) in
    let* (x149, x150) = match x0 with
      | Pair (y149, y150) ->  return (y149, y150)
      | _ -> mzero in
    let* _ = guard (x149 == x139) in
    let* _ = guard (x150 == x144) in return ());
    (let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* x4 = stepIIO x0 x2  in
    let* _ = _evalII x4 x3  in return ())]
and _stepEvalII x0 x1  =
  msum
    [(let* _ = step_EvalII x0 x1  in return ())]
and _step_IOIIIIII x0 x2 x3 x4 x5 x6 x7  =
  msum
    [(let* _ = _________safe_III x2 x3 x4  in
    let* _ = __________safe_III x5 x6 x7  in
    let* x165 = return False in
    let* x164 = return (Quad (x2, x3, x4, x165)) in
    let* x167 = return True in
    let* x166 = return (Quad (x5, x6, x7, x167)) in
    let* _ = guard (x0 == Empty) in
    let* x168 = return x164 in
    let* x169 = return x166 in
    let* x1 = return (Pair (x168, x169)) in return x1);
    (let* x170 = return False in
    let* _ = _________safe_III x170 x3 x4  in
    let* x172 = return False in
    let* x173 = return False in
    let* x171 = return (Quad (x172, x3, x4, x173)) in
    let* x175 = return True in
    let* x176 = return True in
    let* x174 = return (Quad (x175, x6, x7, x176)) in
    let* x179 = return True in
    let* _ = __________safe_III x179 x6 x7  in
    let* _ = guard (x2 == True) in
    let* _ = guard (x0 == Goat) in
    let* x177 = return x171 in
    let* x178 = return x174 in
    let* x1 = return (Pair (x177, x178)) in return x1);
    (let* x180 = return False in
    let* _ = _________safe_III x2 x180 x4  in
    let* x182 = return False in
    let* x183 = return False in
    let* x181 = return (Quad (x2, x182, x4, x183)) in
    let* x185 = return True in
    let* x186 = return True in
    let* x184 = return (Quad (x6, x185, x7, x186)) in
    let* x189 = return True in
    let* _ = __________safe_III x6 x189 x7  in
    let* _ = guard (x3 == True) in
    let* _ = guard (x0 == Wolf) in
    let* x187 = return x181 in
    let* x188 = return x184 in
    let* x1 = return (Pair (x187, x188)) in return x1);
    (let* x190 = return False in
    let* _ = _________safe_III x2 x3 x190  in
    let* x192 = return False in
    let* x193 = return False in
    let* x191 = return (Quad (x2, x3, x192, x193)) in
    let* x195 = return True in
    let* x196 = return True in
    let* x194 = return (Quad (x5, x6, x195, x196)) in
    let* x199 = return True in
    let* _ = __________safe_III x5 x6 x199  in
    let* _ = guard (x4 == True) in
    let* _ = guard (x0 == Cabbage) in
    let* x197 = return x191 in
    let* x198 = return x194 in
    let* x1 = return (Pair (x197, x198)) in return x1)]
and stepIIO x0 x1  =
  msum
    [(let* x152 = return True in
    let* x154 = return False in
    let* (x155, x156) = match x0 with
      | Pair (y155, y156) ->  return (y155, y156)
      | _ -> mzero in
    let* x151 = return x155 in
    let* (x3, x4, x5) = match x151 with
      | Quad (y3, y4, y5, y152) -> let* _ = guard (x152 == y152) in return (y3, y4, y5)
      | _ -> mzero in
    let* x153 = return x156 in
    let* (x6, x7, x8) = match x153 with
      | Quad (y6, y7, y8, y154) -> let* _ = guard (x154 == y154) in return (y6, y7, y8)
      | _ -> mzero in
    let* x2 = _step_IOIIIIII x1 x3 x4 x5 x6 x7 x8  in return x2);
    (let* x158 = return False in
    let* x160 = return True in
    let* (x161, x162) = match x0 with
      | Pair (y161, y162) ->  return (y161, y162)
      | _ -> mzero in
    let* x157 = return x161 in
    let* (x6, x7, x8) = match x157 with
      | Quad (y6, y7, y8, y158) -> let* _ = guard (x158 == y158) in return (y6, y7, y8)
      | _ -> mzero in
    let* x159 = return x162 in
    let* (x3, x4, x5) = match x159 with
      | Quad (y3, y4, y5, y160) -> let* _ = guard (x160 == y160) in return (y3, y4, y5)
      | _ -> mzero in
    let* x163 = _step_IOIIIIII x1 x3 x4 x5 x6 x7 x8  in
    let* (x10, x9) = match x163 with
      | Pair (y10, y9) ->  return (y10, y9)
      | _ -> mzero in
    let* x2 = return (Pair (x9, x10)) in return x2)]
and stepEvalII x0 x1  =
  msum
    [(let* _ = _step_EvalII x0 x1  in return ())]
and _step_EvalII x0 x1  =
  msum
    [(let* x35 = return False in
    let* x36 = return False in
    let* x37 = return False in
    let* _ = _________safe_III x35 x36 x37  in
    let* x38 = return True in
    let* x39 = return True in
    let* x40 = return True in
    let* _ = __________safe_III x38 x39 x40  in
    let* (x2, x3) = match x1 with
      | Cons (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* _ = _stepEvalII x2 x3  in
    let* _ = guard (x0 == Empty) in return ())]
and step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x11 = return True in
    let* x12 = return True in
    let* x13 = return True in
    let* _ = _________safe_III x11 x12 x13  in
    let* x14 = return False in
    let* x15 = return False in
    let* x16 = return False in
    let* _ = __________safe_III x14 x15 x16  in
    let* (x2, x3) = stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Goat in
    let* x17 = return False in
    let* x18 = return True in
    let* x19 = return True in
    let* _ = _________safe_III x17 x18 x19  in
    let* x20 = return True in
    let* x21 = return False in
    let* x22 = return False in
    let* _ = __________safe_III x20 x21 x22  in
    let* (x4, x5) = __stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1));
    (let* x0 = return Wolf in
    let* x23 = return True in
    let* x24 = return False in
    let* x25 = return True in
    let* _ = _________safe_III x23 x24 x25  in
    let* x26 = return False in
    let* x27 = return True in
    let* x28 = return False in
    let* _ = __________safe_III x26 x27 x28  in
    let* (x4, x5) = ________________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1));
    (let* x0 = return Cabbage in
    let* x29 = return True in
    let* x30 = return True in
    let* x31 = return False in
    let* _ = _________safe_III x29 x30 x31  in
    let* x32 = return False in
    let* x33 = return False in
    let* x34 = return True in
    let* _ = __________safe_III x32 x33 x34  in
    let* (x4, x5) = ____________________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and ____________________stepEvalOO   =
  msum
    [(let* (x0, x1) = __________________step_EvalOO   in return (x0, x1))]
and __________________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x352 = return False in
    let* x353 = return False in
    let* x354 = return True in
    let* _ = _________safe_III x352 x353 x354  in
    let* x355 = return True in
    let* x356 = return True in
    let* x357 = return False in
    let* _ = __________safe_III x355 x356 x357  in
    let* (x2, x3) = ______________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Cabbage in
    let* x358 = return False in
    let* x359 = return False in
    let* x360 = return False in
    let* _ = _________safe_III x358 x359 x360  in
    let* x361 = return True in
    let* x362 = return True in
    let* x363 = return True in
    let* _ = __________safe_III x361 x362 x363  in
    let* (x4, x5) = _stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and ________________stepEvalOO   =
  msum
    [(let* (x0, x1) = ______________step_EvalOO   in return (x0, x1))]
and ______________stepEvalOO   =
  msum
    [(let* (x0, x1) = _____________step_EvalOO   in return (x0, x1))]
and ______________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x281 = return False in
    let* x282 = return True in
    let* x283 = return False in
    let* _ = _________safe_III x281 x282 x283  in
    let* x284 = return True in
    let* x285 = return False in
    let* x286 = return True in
    let* _ = __________safe_III x284 x285 x286  in
    let* (x2, x3) = _________________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Wolf in
    let* x287 = return False in
    let* x288 = return False in
    let* x289 = return False in
    let* _ = _________safe_III x287 x288 x289  in
    let* x290 = return False in
    let* x291 = return True in
    let* x292 = return True in
    let* _ = __________safe_III x290 x291 x292  in
    let* (x4, x5) = __________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and _________________stepEvalOO   =
  msum
    [(let* (x0, x1) = _______________step_EvalOO   in return (x0, x1))]
and _______________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x293 = return True in
    let* x294 = return False in
    let* x295 = return True in
    let* _ = _________safe_III x293 x294 x295  in
    let* x296 = return False in
    let* x297 = return True in
    let* x298 = return False in
    let* _ = __________safe_III x296 x297 x298  in
    let* (x2, x3) = ________________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Goat in
    let* x299 = return False in
    let* x300 = return False in
    let* x301 = return True in
    let* _ = _________safe_III x299 x300 x301  in
    let* x302 = return True in
    let* x303 = return True in
    let* x304 = return False in
    let* _ = __________safe_III x302 x303 x304  in
    let* (x4, x5) = __________________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1));
    (let* x0 = return Cabbage in
    let* x305 = return True in
    let* x306 = return False in
    let* x307 = return False in
    let* _ = _________safe_III x305 x306 x307  in
    let* x308 = return False in
    let* x309 = return True in
    let* x310 = return True in
    let* _ = __________safe_III x308 x309 x310  in
    let* (x4, x5) = _______________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and __________________stepEvalOO   =
  msum
    [(let* (x0, x1) = ________________step_EvalOO   in return (x0, x1))]
and ________________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x311 = return True in
    let* x312 = return True in
    let* x313 = return False in
    let* _ = _________safe_III x311 x312 x313  in
    let* x314 = return False in
    let* x315 = return False in
    let* x316 = return True in
    let* _ = __________safe_III x314 x315 x316  in
    let* (x2, x3) = ___________________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Goat in
    let* x317 = return False in
    let* x318 = return True in
    let* x319 = return False in
    let* _ = _________safe_III x317 x318 x319  in
    let* x320 = return True in
    let* x321 = return False in
    let* x322 = return True in
    let* _ = __________safe_III x320 x321 x322  in
    let* (x4, x5) = _________________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1));
    (let* x0 = return Wolf in
    let* x323 = return True in
    let* x324 = return False in
    let* x325 = return False in
    let* _ = _________safe_III x323 x324 x325  in
    let* x326 = return False in
    let* x327 = return True in
    let* x328 = return True in
    let* _ = __________safe_III x326 x327 x328  in
    let* (x4, x5) = ___stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and ___________________stepEvalOO   =
  msum
    [(let* (x0, x1) = _________________step_EvalOO   in return (x0, x1))]
and _________________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x329 = return False in
    let* x330 = return False in
    let* x331 = return True in
    let* _ = _________safe_III x329 x330 x331  in
    let* x332 = return True in
    let* x333 = return True in
    let* x334 = return False in
    let* _ = __________safe_III x332 x333 x334  in
    let* (x2, x3) = __________________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Cabbage in
    let* x335 = return False in
    let* x336 = return False in
    let* x337 = return False in
    let* _ = _________safe_III x335 x336 x337  in
    let* x338 = return True in
    let* x339 = return True in
    let* x340 = return True in
    let* _ = __________safe_III x338 x339 x340  in
    let* x343 = return False in
    let* x344 = return False in
    let* x345 = return False in
    let* x346 = return False in
    let* x342 = return (Quad (x343, x344, x345, x346)) in
    let* x348 = return True in
    let* x349 = return True in
    let* x350 = return True in
    let* x351 = return True in
    let* x347 = return (Quad (x348, x349, x350, x351)) in
    let* x341 = return (Pair (x342, x347)) in
    let* x1 = _evalIO x341  in return (x0, x1))]
and _______________stepEvalOO   =
  msum
    [(let* x274 = return False in
    let* x275 = return True in
    let* x276 = return True in
    let* x277 = return True in
    let* x278 = return False in
    let* x279 = return False in
    let* (x0, x273) = _step_OOIIIIII x274 x275 x276 x277 x278 x279  in
    let* (x2, x3) = match x273 with
      | Pair (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* x280 = return (Pair (x3, x2)) in
    let* x1 = _evalIO x280  in return (x0, x1))]
and _____________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x255 = return True in
    let* x256 = return True in
    let* x257 = return False in
    let* _ = _________safe_III x255 x256 x257  in
    let* x258 = return False in
    let* x259 = return False in
    let* x260 = return True in
    let* _ = __________safe_III x258 x259 x260  in
    let* (x2, x3) = ____________________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Goat in
    let* x261 = return False in
    let* x262 = return True in
    let* x263 = return False in
    let* _ = _________safe_III x261 x262 x263  in
    let* x264 = return True in
    let* x265 = return False in
    let* x266 = return True in
    let* _ = __________safe_III x264 x265 x266  in
    let* (x4, x5) = ____________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1));
    (let* x0 = return Wolf in
    let* x267 = return True in
    let* x268 = return False in
    let* x269 = return False in
    let* _ = _________safe_III x267 x268 x269  in
    let* x270 = return False in
    let* x271 = return True in
    let* x272 = return True in
    let* _ = __________safe_III x270 x271 x272  in
    let* (x4, x5) = _______________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and ____________stepEvalOO   =
  msum
    [(let* (x0, x1) = ___________step_EvalOO   in return (x0, x1))]
and ___________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x225 = return True in
    let* x226 = return False in
    let* x227 = return True in
    let* _ = _________safe_III x225 x226 x227  in
    let* x228 = return False in
    let* x229 = return True in
    let* x230 = return False in
    let* _ = __________safe_III x228 x229 x230  in
    let* (x2, x3) = _____________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Goat in
    let* x231 = return False in
    let* x232 = return False in
    let* x233 = return True in
    let* _ = _________safe_III x231 x232 x233  in
    let* x234 = return True in
    let* x235 = return True in
    let* x236 = return False in
    let* _ = __________safe_III x234 x235 x236  in
    let* (x4, x5) = ______________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1));
    (let* x0 = return Cabbage in
    let* x237 = return True in
    let* x238 = return False in
    let* x239 = return False in
    let* _ = _________safe_III x237 x238 x239  in
    let* x240 = return False in
    let* x241 = return True in
    let* x242 = return True in
    let* _ = __________safe_III x240 x241 x242  in
    let* (x4, x5) = ___stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and _____________stepEvalOO   =
  msum
    [(let* (x0, x1) = ____________step_EvalOO   in return (x0, x1))]
and ____________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x243 = return False in
    let* x244 = return True in
    let* x245 = return False in
    let* _ = _________safe_III x243 x244 x245  in
    let* x246 = return True in
    let* x247 = return False in
    let* x248 = return True in
    let* _ = __________safe_III x246 x247 x248  in
    let* (x2, x3) = ____________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Wolf in
    let* x249 = return False in
    let* x250 = return False in
    let* x251 = return False in
    let* _ = _________safe_III x249 x250 x251  in
    let* x252 = return False in
    let* x253 = return True in
    let* x254 = return True in
    let* _ = __________safe_III x252 x253 x254  in
    let* (x4, x5) = ______stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and __________stepEvalOO   =
  msum
    [(let* (x0, x1) = _________step_EvalOO   in return (x0, x1))]
and _________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x201 = return False in
    let* x202 = return True in
    let* x203 = return True in
    let* _ = _________safe_III x201 x202 x203  in
    let* x204 = return False in
    let* x205 = return False in
    let* x206 = return False in
    let* _ = __________safe_III x204 x205 x206  in
    let* (x2, x3) = ___________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Wolf in
    let* x207 = return False in
    let* x208 = return False in
    let* x209 = return True in
    let* _ = _________safe_III x207 x208 x209  in
    let* x210 = return False in
    let* x211 = return True in
    let* x212 = return False in
    let* _ = __________safe_III x210 x211 x212  in
    let* (x4, x5) = ____stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1));
    (let* x0 = return Cabbage in
    let* x213 = return False in
    let* x214 = return True in
    let* x215 = return False in
    let* _ = _________safe_III x213 x214 x215  in
    let* x216 = return False in
    let* x217 = return False in
    let* x218 = return True in
    let* _ = __________safe_III x216 x217 x218  in
    let* (x4, x5) = _________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and ___________stepEvalOO   =
  msum
    [(let* (x0, x1) = __________step_EvalOO   in return (x0, x1))]
and __________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x219 = return False in
    let* x220 = return False in
    let* x221 = return False in
    let* _ = _________safe_III x219 x220 x221  in
    let* x222 = return False in
    let* x223 = return True in
    let* x224 = return True in
    let* _ = __________safe_III x222 x223 x224  in
    let* (x2, x3) = __________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1))]
and _________stepEvalOO   =
  msum
    [(let* x132 = return False in
    let* x133 = return False in
    let* x134 = return True in
    let* x135 = return False in
    let* x136 = return True in
    let* x137 = return False in
    let* (x0, x131) = _step_OOIIIIII x132 x133 x134 x135 x136 x137  in
    let* (x2, x3) = match x131 with
      | Pair (y2, y3) ->  return (y2, y3)
      | _ -> mzero in
    let* x138 = return (Pair (x3, x2)) in
    let* x1 = _evalIO x138  in return (x0, x1))]
and ______stepEvalOO   =
  msum
    [(let* (x0, x1) = ______step_EvalOO   in return (x0, x1))]
and ______step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x95 = return False in
    let* x96 = return True in
    let* x97 = return True in
    let* _ = _________safe_III x95 x96 x97  in
    let* x98 = return False in
    let* x99 = return False in
    let* x100 = return False in
    let* _ = __________safe_III x98 x99 x100  in
    let* (x2, x3) = _______stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Wolf in
    let* x101 = return False in
    let* x102 = return False in
    let* x103 = return True in
    let* _ = _________safe_III x101 x102 x103  in
    let* x104 = return False in
    let* x105 = return True in
    let* x106 = return False in
    let* _ = __________safe_III x104 x105 x106  in
    let* (x4, x5) = ________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1));
    (let* x0 = return Cabbage in
    let* x107 = return False in
    let* x108 = return True in
    let* x109 = return False in
    let* _ = _________safe_III x107 x108 x109  in
    let* x110 = return False in
    let* x111 = return False in
    let* x112 = return True in
    let* _ = __________safe_III x110 x111 x112  in
    let* (x4, x5) = _____stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and ________stepEvalOO   =
  msum
    [(let* (x0, x1) = ________step_EvalOO   in return (x0, x1))]
and ________step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x119 = return False in
    let* x120 = return True in
    let* x121 = return False in
    let* _ = _________safe_III x119 x120 x121  in
    let* x122 = return False in
    let* x123 = return False in
    let* x124 = return True in
    let* _ = __________safe_III x122 x123 x124  in
    let* (x2, x3) = _________stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Wolf in
    let* x125 = return False in
    let* x126 = return False in
    let* x127 = return False in
    let* _ = _________safe_III x125 x126 x127  in
    let* x128 = return False in
    let* x129 = return True in
    let* x130 = return True in
    let* _ = __________safe_III x128 x129 x130  in
    let* (x4, x5) = ______stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and _______stepEvalOO   =
  msum
    [(let* (x0, x1) = _______step_EvalOO   in return (x0, x1))]
and _______step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x113 = return False in
    let* x114 = return False in
    let* x115 = return False in
    let* _ = _________safe_III x113 x114 x115  in
    let* x116 = return False in
    let* x117 = return True in
    let* x118 = return True in
    let* _ = __________safe_III x116 x117 x118  in
    let* (x2, x3) = ______stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1))]
and _____stepEvalOO   =
  msum
    [(let* (x0, x1) = _____step_EvalOO   in return (x0, x1))]
and _____step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x83 = return False in
    let* x84 = return False in
    let* x85 = return True in
    let* _ = _________safe_III x83 x84 x85  in
    let* x86 = return False in
    let* x87 = return True in
    let* x88 = return False in
    let* _ = __________safe_III x86 x87 x88  in
    let* (x2, x3) = ____stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Cabbage in
    let* x89 = return False in
    let* x90 = return False in
    let* x91 = return False in
    let* _ = _________safe_III x89 x90 x91  in
    let* x92 = return False in
    let* x93 = return True in
    let* x94 = return True in
    let* _ = __________safe_III x92 x93 x94  in
    let* (x4, x5) = ______stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and ____stepEvalOO   =
  msum
    [(let* (x0, x1) = ____step_EvalOO   in return (x0, x1))]
and ____step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x71 = return False in
    let* x72 = return True in
    let* x73 = return False in
    let* _ = _________safe_III x71 x72 x73  in
    let* x74 = return False in
    let* x75 = return False in
    let* x76 = return True in
    let* _ = __________safe_III x74 x75 x76  in
    let* (x2, x3) = _____stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Wolf in
    let* x77 = return False in
    let* x78 = return False in
    let* x79 = return False in
    let* _ = _________safe_III x77 x78 x79  in
    let* x80 = return False in
    let* x81 = return True in
    let* x82 = return True in
    let* _ = __________safe_III x80 x81 x82  in
    let* (x4, x5) = __________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and ___stepEvalOO   =
  msum
    [(let* (x0, x1) = ___step_EvalOO   in return (x0, x1))]
and ___step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x53 = return False in
    let* x54 = return True in
    let* x55 = return True in
    let* _ = _________safe_III x53 x54 x55  in
    let* x56 = return True in
    let* x57 = return False in
    let* x58 = return False in
    let* _ = __________safe_III x56 x57 x58  in
    let* (x2, x3) = __stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Wolf in
    let* x59 = return False in
    let* x60 = return False in
    let* x61 = return True in
    let* _ = _________safe_III x59 x60 x61  in
    let* x62 = return False in
    let* x63 = return True in
    let* x64 = return False in
    let* _ = __________safe_III x62 x63 x64  in
    let* (x4, x5) = ____stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1));
    (let* x0 = return Cabbage in
    let* x65 = return False in
    let* x66 = return True in
    let* x67 = return False in
    let* _ = _________safe_III x65 x66 x67  in
    let* x68 = return True in
    let* x69 = return False in
    let* x70 = return True in
    let* _ = __________safe_III x68 x69 x70  in
    let* (x4, x5) = ____________stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and __stepEvalOO   =
  msum
    [(let* (x0, x1) = __step_EvalOO   in return (x0, x1))]
and __step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x41 = return True in
    let* x42 = return False in
    let* x43 = return False in
    let* _ = _________safe_III x41 x42 x43  in
    let* x44 = return False in
    let* x45 = return True in
    let* x46 = return True in
    let* _ = __________safe_III x44 x45 x46  in
    let* (x2, x3) = ___stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1));
    (let* x0 = return Goat in
    let* x47 = return False in
    let* x48 = return False in
    let* x49 = return False in
    let* _ = _________safe_III x47 x48 x49  in
    let* x50 = return True in
    let* x51 = return True in
    let* x52 = return True in
    let* _ = __________safe_III x50 x51 x52  in
    let* (x4, x5) = _stepEvalOO   in
    let* x1 = return (Cons (x4, x5)) in return (x0, x1))]
and _evalIO x0  =
  msum
    [(let* x1 = return Nil in
    let* x140 = return False in
    let* x141 = return False in
    let* x142 = return False in
    let* x143 = return False in
    let* x139 = return (Quad (x140, x141, x142, x143)) in
    let* x145 = return True in
    let* x146 = return True in
    let* x147 = return True in
    let* x148 = return True in
    let* x144 = return (Quad (x145, x146, x147, x148)) in
    let* (x149, x150) = match x0 with
      | Pair (y149, y150) ->  return (y149, y150)
      | _ -> mzero in
    let* _ = guard (x149 == x139) in
    let* _ = guard (x150 == x144) in return x1);
    (let* (x2, x4) = stepIOO x0  in
    let* x3 = _evalIO x4  in
    let* x1 = return (Cons (x2, x3)) in return x1)]
and _stepEvalOO   =
  msum
    [(let* (x0, x1) = step_EvalOO   in return (x0, x1))]
and _step_OOIIIIII x2 x3 x4 x5 x6 x7  =
  msum
    [(let* _ = _________safe_III x2 x3 x4  in
    let* _ = __________safe_III x5 x6 x7  in
    let* x165 = return False in
    let* x164 = return (Quad (x2, x3, x4, x165)) in
    let* x167 = return True in
    let* x166 = return (Quad (x5, x6, x7, x167)) in
    let* x0 = return Empty in
    let* x168 = return x164 in
    let* x169 = return x166 in
    let* x1 = return (Pair (x168, x169)) in return (x0, x1));
    (let* x170 = return False in
    let* _ = _________safe_III x170 x3 x4  in
    let* x0 = return Goat in
    let* x172 = return False in
    let* x173 = return False in
    let* x171 = return (Quad (x172, x3, x4, x173)) in
    let* x175 = return True in
    let* x176 = return True in
    let* x174 = return (Quad (x175, x6, x7, x176)) in
    let* x179 = return True in
    let* _ = __________safe_III x179 x6 x7  in
    let* _ = guard (x2 == True) in
    let* x177 = return x171 in
    let* x178 = return x174 in
    let* x1 = return (Pair (x177, x178)) in return (x0, x1));
    (let* x180 = return False in
    let* _ = _________safe_III x2 x180 x4  in
    let* x0 = return Wolf in
    let* x182 = return False in
    let* x183 = return False in
    let* x181 = return (Quad (x2, x182, x4, x183)) in
    let* x185 = return True in
    let* x186 = return True in
    let* x184 = return (Quad (x6, x185, x7, x186)) in
    let* x189 = return True in
    let* _ = __________safe_III x6 x189 x7  in
    let* _ = guard (x3 == True) in
    let* x187 = return x181 in
    let* x188 = return x184 in
    let* x1 = return (Pair (x187, x188)) in return (x0, x1));
    (let* x190 = return False in
    let* _ = _________safe_III x2 x3 x190  in
    let* x0 = return Cabbage in
    let* x192 = return False in
    let* x193 = return False in
    let* x191 = return (Quad (x2, x3, x192, x193)) in
    let* x195 = return True in
    let* x196 = return True in
    let* x194 = return (Quad (x5, x6, x195, x196)) in
    let* x199 = return True in
    let* _ = __________safe_III x5 x6 x199  in
    let* _ = guard (x4 == True) in
    let* x197 = return x191 in
    let* x198 = return x194 in
    let* x1 = return (Pair (x197, x198)) in return (x0, x1))]
and stepIOO x0  =
  msum
    [(let* x152 = return True in
    let* x154 = return False in
    let* (x155, x156) = match x0 with
      | Pair (y155, y156) ->  return (y155, y156)
      | _ -> mzero in
    let* x151 = return x155 in
    let* (x3, x4, x5) = match x151 with
      | Quad (y3, y4, y5, y152) -> let* _ = guard (x152 == y152) in return (y3, y4, y5)
      | _ -> mzero in
    let* x153 = return x156 in
    let* (x6, x7, x8) = match x153 with
      | Quad (y6, y7, y8, y154) -> let* _ = guard (x154 == y154) in return (y6, y7, y8)
      | _ -> mzero in
    let* (x1, x2) = _step_OOIIIIII x3 x4 x5 x6 x7 x8  in return (x1, x2));
    (let* x158 = return False in
    let* x160 = return True in
    let* (x161, x162) = match x0 with
      | Pair (y161, y162) ->  return (y161, y162)
      | _ -> mzero in
    let* x157 = return x161 in
    let* (x6, x7, x8) = match x157 with
      | Quad (y6, y7, y8, y158) -> let* _ = guard (x158 == y158) in return (y6, y7, y8)
      | _ -> mzero in
    let* x159 = return x162 in
    let* (x3, x4, x5) = match x159 with
      | Quad (y3, y4, y5, y160) -> let* _ = guard (x160 == y160) in return (y3, y4, y5)
      | _ -> mzero in
    let* (x1, x163) = _step_OOIIIIII x3 x4 x5 x6 x7 x8  in
    let* (x10, x9) = match x163 with
      | Pair (y10, y9) ->  return (y10, y9)
      | _ -> mzero in
    let* x2 = return (Pair (x9, x10)) in return (x1, x2))]
and stepEvalOO   =
  msum
    [(let* (x0, x1) = _step_EvalOO   in return (x0, x1))]
and _step_EvalOO   =
  msum
    [(let* x0 = return Empty in
    let* x35 = return False in
    let* x36 = return False in
    let* x37 = return False in
    let* _ = _________safe_III x35 x36 x37  in
    let* x38 = return True in
    let* x39 = return True in
    let* x40 = return True in
    let* _ = __________safe_III x38 x39 x40  in
    let* (x2, x3) = _stepEvalOO   in
    let* x1 = return (Cons (x2, x3)) in return (x0, x1))]