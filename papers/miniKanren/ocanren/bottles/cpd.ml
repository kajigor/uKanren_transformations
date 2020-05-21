open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 = ()
  (* let rec checkAnswer y0 y1 y2 =
    fresh (q1 q2) (y0 === List.nil () &&& __isFinishState y2 (Nat.zero) (Nat.zero) ||| (y0 === ( % ) q1 q2 &&& checkStepDoStepCheckAnswer_ y1 y2 q1 q2))
  and checkStepDoStepCheckAnswer_ y5 y6 y7 y8 =
    fresh (q1 q2 q3)
      ( y7
      === Pair.pair (fill ()) (fst_ ())
      &&& doStepCheckAnswer_ y5 y6 y8
      ||| (y7 === Pair.pair (fill ()) (snd_ ()) &&& _____________doStepCheckAnswer_ y5 y6 y8)
      ||| (y7 === Pair.pair (empty ()) (fst_ ()) &&& (y5 === Pair.pair q1 q2) &&& ____fancyEqDoStepCheckAnswer_ y6 y8 (Nat.zero) (Nat.zero) q1 q2)
      ||| (y7 === Pair.pair (empty ()) (snd_ ()) &&& (y5 === Pair.pair q3 q1) &&& _____fancyEqDoStepCheckAnswer_ y6 y8 (Nat.zero) (Nat.zero) q3 q1) )
  and doStepCheckAnswer_ y10 y11 y12 = fresh (q1 q2) (y10 === Pair.pair q1 q2 &&& _______createStateCheckAnswer_ y11 y12 q1 q2 (Nat.zero) q1)
  and __isFinishState y53 y54 y55 = _fancyEqFancyEq y53 y54 y55 ||| __fancyEqFancyEq y53 y54 y55
  and _fancyEqFancyEq y56 y57 y58 =
    fresh (q1 q2 q3)
      ( y57 === Nat.zero
      &&& (y56 === Nat.zero)
      &&& _____fancyEq y58 q1 (Nat.zero)
      ||| (y57 === Nat.succ q2 &&& (y56 === Nat.succ q3) &&& (_____fancyEq q2 !!true q3 &&& _____fancyEq y58 q1 (Nat.succ q3))) )
  and _____fancyEq y60 y61 y62 =
    fresh (q1 q2 q3)
      ( y60 === Nat.zero
      &&& (y62 === Nat.zero)
      &&& (y61 === !!true)
      ||| (y60 === Nat.zero &&& (y62 === Nat.succ q1) &&& (y61 === !!false))
      ||| (y60 === Nat.succ q2 &&& (y62 === Nat.zero) &&& (y61 === !!false))
      ||| (y60 === Nat.succ q2 &&& (y62 === Nat.succ q3) &&& _____fancyEq q2 y61 q3) )
  and __fancyEqFancyEq y63 y64 y65 =
    fresh (q1 q2 q3)
      ( y64 === Nat.zero
      &&& (y63 === Nat.succ q1)
      &&& _____fancyEq y65 !!true (Nat.succ q1)
      ||| (y64 === Nat.succ q2 &&& (y63 === Nat.zero) &&& _____fancyEq y65 !!true (Nat.zero))
      ||| (y64 === Nat.succ q2 &&& (y63 === Nat.succ q3) &&& (_____fancyEq q2 !!false q3 &&& _____fancyEq y65 !!true (Nat.succ q3))) )
  and ____createStateCheckAnswer_ y156 y157 y158 y160 y161 y162 = ____checkAnswer_ y156 y157 y158 y160 y161 y162
  and ____checkAnswer_ y163 y164 y165 y166 y167 y168 =
    fresh (q1 q2)
      ( y166 === List.nil () &&& __isFinishState y163 y165 y164
      ||| (y166 === ( % ) q1 q2 &&& ____checkStepDoStepCheckAnswer_ y163 y164 y165 y167 y168 q1 q2) )
  and ____checkStepDoStepCheckAnswer_ y169 y170 y171 y172 y173 y174 y175 =
    fresh (q1)
      ( y174
      === Pair.pair (fill ()) (fst_ ())
      &&& (y171 === Nat.zero)
      &&& _______doStepCheckAnswer_ y169 y172 y173 y175 y170
      ||| (y174 === Pair.pair (fill ()) (snd_ ()) &&& (y170 === Nat.zero) &&& ________doStepCheckAnswer_ y169 y172 y173 y175 y171)
      ||| (y174 === Pair.pair (empty ()) (fst_ ()) &&& ____fancyEqDoStepCheckAnswer_ y169 y175 y170 y171 y172 y173)
      ||| (y174 === Pair.pair (empty ()) (snd_ ()) &&& _____fancyEqDoStepCheckAnswer_ y169 y175 y171 y170 y172 y173)
      ||| (y174 === Pair.pair (pour ()) (fst_ ()) &&& (y171 === Nat.succ q1) &&& _anotherBottleGet_capacityFancyEqDoStepCheckAnswer_ y169 y172 y173 y175 y170 q1)
      ||| (y174 === Pair.pair (pour ()) (snd_ ()) &&& (y170 === Nat.succ q1) &&& __anotherBottleGet_capacityFancyEqDoStepCheckAnswer_ y169 y172 y173 y175 y171 q1)
      )
  and _______doStepCheckAnswer_ y177 y178 y179 y180 y182 = _______createStateCheckAnswer_ y177 y180 y178 y179 y182 y178
  and ________doStepCheckAnswer_ y189 y190 y191 y192 y194 = ____createStateCheckAnswer_ y189 y191 y194 y192 y190 y191
  and ____fancyEqDoStepCheckAnswer_ y195 y196 y198 y199 y200 y201 =
    fresh (q1 q2)
      ( y199 === Nat.zero
      &&& (y200 === Nat.zero)
      &&& _________doStepCheckAnswer_ y195 y196 y198 y201 (Nat.zero) (Nat.zero) (Nat.zero)
      ||| (y199 === Nat.succ q1 &&& (y200 === Nat.succ q2) &&& (_________doStepCheckAnswer_ y195 y196 y198 y201 (Nat.succ q1) (Nat.succ q2) (Nat.succ q2) &&& _____fancyEq q1 !!true q2)) )
  and _________doStepCheckAnswer_ y202 y203 y205 y206 y207 y208 y209 = ____checkAnswer_ y202 y205 (Nat.zero) y203 y209 y206
  and _____fancyEqDoStepCheckAnswer_ y210 y211 y213 y214 y215 y216 =
    fresh (q1 q2)
      ( y214 === Nat.zero
      &&& (y216 === Nat.zero)
      &&& __________doStepCheckAnswer_ y210 y211 y213 y215 (Nat.zero) (Nat.zero) (Nat.zero)
      ||| (y214 === Nat.succ q1 &&& (y216 === Nat.succ q2) &&& (__________doStepCheckAnswer_ y210 y211 y213 y215 (Nat.succ q1) (Nat.succ q2) (Nat.succ q2) &&& _____fancyEq q1 !!true q2)) )
  and __________doStepCheckAnswer_ y217 y218 y220 y221 y222 y223 y224 = ____createStateCheckAnswer_ y217 (Nat.zero) y220 y218 y221 y224
  and _anotherBottleGet_capacityFancyEqDoStepCheckAnswer_ y225 y226 y227 y228 y230 y233 = _get_capacityFancyEqDoStepCheckAnswer_ y225 y226 y227 y228 y230 y233
  and _get_capacityFancyEqDoStepCheckAnswer_ y234 y235 y236 y237 y239 y241 = ______fancyEqDoStepCheckAnswer_ y234 y237 y239 y241 y235 y236
  and ______fancyEqDoStepCheckAnswer_ y242 y243 y245 y246 y247 y248 =
    fresh (q1 q2 q3)
      ( y245 === Nat.zero &&& (y248 === Nat.succ q1) &&& ___________doStepCheckAnswer_ y242 y243 y246 y247 (Nat.zero) (Nat.succ q1) (Nat.succ q1)
      ||| (y245 === Nat.succ q2 &&& (y248 === Nat.zero) &&& ___________doStepCheckAnswer_ y242 y243 y246 y247 (Nat.succ q2) (Nat.zero) (Nat.zero))
      ||| (y245 === Nat.succ q2 &&& (y248 === Nat.succ q3) &&& (___________doStepCheckAnswer_ y242 y243 y246 y247 (Nat.succ q2) (Nat.succ q3) (Nat.succ q3) &&& _____fancyEq q2 !!false q3)) )
  and ___________doStepCheckAnswer_ y249 y250 y252 y253 y254 y255 y256 =
    fresh (q1 q2 q3 q4 q5)
      ( addGreaterAddSubCreateStateCheckAnswer_ y249 y250 y253 y256 y254 q1 q2 q3 y252
      &&& anotherBottleGet_capacityGet_capacityGet_capacity y253 y255 q1 q2 q3 q4 q5
      &&& _anotherBottle q4 &&& _anotherBottle q5
      ||| (addGreaterAddCreateStateCheckAnswer_ y249 y250 y253 y256 y254 q1 y252 &&& anotherBottleGet_capacity y253 y255 q1) )
  and addGreaterAddSubCreateStateCheckAnswer_ y257 y258 y260 y261 y262 y263 y265 y267 y268 =
    fresh (q1 q2)
      ( y268 === Nat.zero
      &&& greaterAddSubCreateStateCheckAnswer_ y257 y258 y260 y261 y263 y265 y267 y262
      ||| (y268 === Nat.succ q1 &&& (addAddSubCreateStateCheckAnswer_ y257 y258 y260 y261 y262 y265 y267 q1 q2 (Nat.succ q1) &&& greater (Nat.succ (Nat.succ q2)) y263)) )
  and greaterAddSubCreateStateCheckAnswer_ y270 y271 y273 y274 y275 y277 y279 y280 =
    fresh (q1)
      ( y275 === Nat.zero
      &&& _addSubCreateStateCheckAnswer_ y270 y271 y273 y274 y277 y279 y280 (Nat.zero)
      ||| (y275 === Nat.succ q1 &&& _greaterAddSubCreateStateCheckAnswer_ y270 y271 y273 y274 y277 y279 y280 q1) )
  and _subCreateStateCheckAnswer_ y307 y308 y310 y311 y313 y314 y315 =
    fresh (q1 q2)
      ( y314 === Nat.zero
      &&& _______createStateCheckAnswer_ y307 y308 y310 y311 y313 y315
      ||| (y314 === Nat.succ q1 &&& (y315 === Nat.zero) &&& _______createStateCheckAnswer_ y307 y308 y310 y311 y313 (Nat.zero))
      ||| (y314 === Nat.succ q1 &&& (y315 === Nat.succ q2) &&& _subCreateStateCheckAnswer_ y307 y308 y310 y311 y313 q1 q2) )
  and _______createStateCheckAnswer_ y316 y317 y319 y320 y321 y322 = ____checkAnswer_ y316 y321 y322 y317 y319 y320
  and _greaterAddSubCreateStateCheckAnswer_ y323 y324 y326 y327 y329 y331 y332 y333 =
    fresh (q1 q2)
      ( y332 === Nat.succ q1
      &&& (y333 === Nat.zero)
      &&& _addSubCreateStateCheckAnswer_ y323 y324 y326 y327 y329 y331 (Nat.succ q1) (Nat.zero)
      ||| (y332 === Nat.succ q1 &&& (y333 === Nat.succ q2) &&& (_addSubCreateStateCheckAnswer_ y323 y324 y326 y327 y329 y331 (Nat.succ q1) (Nat.zero) &&& greater q1 q2)) )
  and greater y334 y335 = fresh (q1 q2) (y334 === Nat.succ q1 &&& (y335 === Nat.zero) ||| (y334 === Nat.succ q1 &&& (y335 === Nat.succ q2) &&& greater q1 q2))
  and addAddSubCreateStateCheckAnswer_ y336 y337 y339 y340 y341 y343 y345 y346 y347 y348 =
    fresh (q1 q2)
      ( y346 === Nat.zero &&& (y341 === y347)
      &&& _addSubCreateStateCheckAnswer_ y336 y337 y339 y340 y343 y345 y347 y348
      ||| (y346 === Nat.succ q1 &&& (y347 === Nat.succ q2) &&& addAddSubCreateStateCheckAnswer_ y336 y337 y339 y340 y341 y343 y345 q1 q2 y348) )
  and _addSubCreateStateCheckAnswer_ y349 y350 y352 y353 y355 y357 y358 y359 =
    fresh (q1)
      ( y359 === Nat.zero
      &&& _subCreateStateCheckAnswer_ y349 y350 y352 y353 y355 y357 (Nat.succ y358)
      ||| (y359 === Nat.succ q1 &&& __addSubCreateStateCheckAnswer_ y349 y350 y352 y353 y355 y357 y358 q1) )
  and __addSubCreateStateCheckAnswer_ y360 y361 y363 y364 y366 y367 y368 y369 =
    fresh (q1 q2)
      ( y369 === Nat.zero
      &&& _subCreateStateCheckAnswer_ y360 y361 y363 y364 y366 y367 (Nat.succ (Nat.succ y368))
      ||| (y369 === Nat.succ q1 &&& (_subCreateStateCheckAnswer_ y360 y361 y363 y364 y366 y367 (Nat.succ (Nat.succ (Nat.succ q2))) &&& add y368 q1 q2)) )
  and add y371 y372 y373 = fresh (q1 q2) (y372 === Nat.zero &&& (y371 === y373) ||| (y372 === Nat.succ q1 &&& (y373 === Nat.succ q2) &&& add y371 q1 q2))
  and anotherBottleGet_capacityGet_capacityGet_capacity y374 y375 y376 y378 y379 y380 y381 =
    get_capacityGet_capacityGet_capacity y374 y375 y376 y378 y379 y380 y381
  and get_capacityGet_capacityGet_capacity y382 y383 y384 y385 y386 y387 y388 = y384 === y383 &&& get_capacityGet_capacity y385 y386 y387 y388 y382 y383
  and get_capacityGet_capacity y389 y390 y391 y392 y393 y394 =
    y391 === fst_ () &&& (y390 === y393) &&& get_capacity y389 y392 y393 y394 ||| (y391 === snd_ () &&& (y390 === y394) &&& get_capacity y389 y392 y393 y394)
  and get_capacity y395 y396 y397 y398 = y396 === fst_ () &&& (y395 === y397) ||| (y396 === snd_ () &&& (y395 === y398))
  and _anotherBottle y400 = y400 === snd_ ()
  and addGreaterAddCreateStateCheckAnswer_ y401 y402 y404 y405 y406 y407 y409 =
    fresh (q1 q2)
      ( y409 === Nat.zero
      &&& greaterAddCreateStateCheckAnswer_ y401 y402 y404 y405 y407 y406
      ||| (y409 === Nat.succ q1 &&& (addAddCreateStateCheckAnswer_ y401 y402 y404 y405 y406 q1 q2 (Nat.succ q1) &&& _greater (Nat.succ (Nat.succ q2)) y407)) )
  and greaterAddCreateStateCheckAnswer_ y411 y412 y414 y415 y416 y418 =
    fresh (q1 q2 q3)
      ( y416 === Nat.succ q1
      &&& (y418 === Nat.zero)
      &&& __addCreateStateCheckAnswer_ y411 y412 y414 y415 (Nat.zero) (Nat.zero)
      ||| (y416 === Nat.succ (Nat.succ q2) &&& (y418 === Nat.succ q3) &&& _greaterAddCreateStateCheckAnswer_ y411 y412 y414 y415 q3 q2) )
  and _greaterAddCreateStateCheckAnswer_ y425 y426 y428 y429 y431 y432 =
    fresh (q1 q2)
      ( y431 === Nat.zero
      &&& __addCreateStateCheckAnswer_ y425 y426 y428 y429 (Nat.succ (Nat.zero)) (Nat.zero)
      ||| (y431 === Nat.succ q1 &&& (y432 === Nat.succ q2) &&& (__addCreateStateCheckAnswer_ y425 y426 y428 y429 (Nat.succ (Nat.succ q1)) (Nat.zero) &&& _greater q1 q2)) )
  and _greater y433 y434 = fresh (q1 q2) (y433 === Nat.zero ||| (y433 === Nat.succ q1 &&& (y434 === Nat.succ q2) &&& _greater q1 q2))
  and addAddCreateStateCheckAnswer_ y442 y443 y445 y446 y447 y449 y450 y451 =
    fresh (q1 q2)
      ( y449 === Nat.zero &&& (y447 === y450)
      &&& __addCreateStateCheckAnswer_ y442 y443 y445 y446 y450 y451
      ||| (y449 === Nat.succ q1 &&& (y450 === Nat.succ q2) &&& addAddCreateStateCheckAnswer_ y442 y443 y445 y446 y447 q1 q2 y451) )
  and __addCreateStateCheckAnswer_ y452 y453 y455 y456 y458 y459 =
    fresh (q1)
      ( y459 === Nat.zero
      &&& _______createStateCheckAnswer_ y452 y453 y455 y456 (Nat.succ y458) (Nat.zero)
      ||| (y459 === Nat.succ q1 &&& ___addCreateStateCheckAnswer_ y452 y453 y455 y456 y458 q1) )
  and ___addCreateStateCheckAnswer_ y460 y461 y463 y464 y465 y466 =
    fresh (q1 q2)
      ( y466 === Nat.zero
      &&& _______createStateCheckAnswer_ y460 y461 y463 y464 (Nat.succ (Nat.succ y465)) (Nat.zero)
      ||| (y466 === Nat.succ q1 &&& (_______createStateCheckAnswer_ y460 y461 y463 y464 (Nat.succ (Nat.succ (Nat.succ q2))) (Nat.zero) &&& add y465 q1 q2)) )
  and anotherBottleGet_capacity y468 y469 y470 = get_capacity y470 (snd_ ()) y468 y469
  and __anotherBottleGet_capacityFancyEqDoStepCheckAnswer_ y472 y473 y474 y475 y477 y480 =
    __get_capacityFancyEqDoStepCheckAnswer_ y472 y473 y474 y475 y477 y480
  and __get_capacityFancyEqDoStepCheckAnswer_ y481 y482 y483 y484 y486 y488 = _______fancyEqDoStepCheckAnswer_ y481 y484 y486 y488 y482 y483
  and _______fancyEqDoStepCheckAnswer_ y489 y490 y492 y493 y494 y495 =
    fresh (q1 q2 q3)
      ( y492 === Nat.zero
      &&& (y494 === Nat.succ q1)
      &&& ____________doStepCheckAnswer_ y489 y490 y493 y495 (Nat.zero) (Nat.succ q1) (Nat.succ q1)
      ||| (y492 === Nat.succ q2 &&& (y494 === Nat.zero) &&& ____________doStepCheckAnswer_ y489 y490 y493 y495 (Nat.succ q2) (Nat.zero) (Nat.zero))
      ||| (y492 === Nat.succ q2 &&& (y494 === Nat.succ q3) &&& (____________doStepCheckAnswer_ y489 y490 y493 y495 (Nat.succ q2) (Nat.succ q3) (Nat.succ q3) &&& _____fancyEq q2 !!false q3)) )
  and ____________doStepCheckAnswer_ y496 y497 y499 y500 y501 y502 y503 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( addGreaterAddSub y499 y501 q1 q2 q3
      &&& _get_capacityGet_capacityGet_capacity y500 y502 q1 q4 q3 q5 q6
      &&& ____createStateCheckAnswer_ y496 q2 q4 y497 y503 y500 &&& ___anotherBottle q5 &&& ___anotherBottle q6
      ||| (_addGreaterAddCreateStateCheckAnswer_ y496 y497 y499 y500 y503 y501 q1 &&& get_capacity q1 (fst_ ()) y502 y500) )
  and addGreaterAddSub y504 y505 y507 y508 y510 =
    fresh (q1 q2) (y505 === Nat.zero &&& greaterAddSub y504 y507 y508 y510 ||| (y505 === Nat.succ q1 &&& (addAddSub y504 y508 y510 q1 q2 (Nat.succ q1) &&& greater (Nat.succ q2) y507)))
  and greaterAddSub y511 y512 y513 y515 = fresh (q1) (y512 === Nat.zero &&& _addSub y511 y513 y515 (Nat.zero) ||| (y512 === Nat.succ q1 &&& _greaterAddSub y513 y515 y511 q1))
  and _sub y523 y524 y525 =
    fresh (q1 q2)
      ( y524 === Nat.zero &&& (y523 === y525)
      ||| (y524 === Nat.succ q1 &&& (y525 === Nat.zero) &&& (y523 === Nat.zero))
      ||| (y524 === Nat.succ q1 &&& (y525 === Nat.succ q2) &&& _sub y523 q1 q2) )
  and _greaterAddSub y526 y528 y529 y530 =
    fresh (q1 q2)
      ( y529 === Nat.succ q1
      &&& (y530 === Nat.zero)
      &&& _addSub (Nat.succ q1) y526 y528 (Nat.zero)
      ||| (y529 === Nat.succ q1 &&& (y530 === Nat.succ q2) &&& (_addSub (Nat.succ q1) y526 y528 (Nat.zero) &&& greater q1 q2)) )
  and addAddSub y531 y532 y534 y535 y536 y537 =
    fresh (q1 q2)
      (y535 === Nat.zero &&& (y536 === Nat.succ y531) &&& _addSub y531 y532 y534 y537 ||| (y535 === Nat.succ q1 &&& (y536 === Nat.succ q2) &&& addAddSub y531 y532 y534 q1 q2 y537))
  and _addSub y538 y539 y541 y542 =
    fresh (q1 q2) (y542 === Nat.zero &&& _sub y539 y541 (Nat.succ y538) ||| (y542 === Nat.succ q1 &&& (add (Nat.succ y538) q1 q2 &&& _sub y539 y541 (Nat.succ q2))))
  and _get_capacityGet_capacityGet_capacity y543 y544 y545 y546 y547 y548 y549 = y545 === y544 &&& get_capacityGet_capacity y546 y547 y548 y549 y544 y543
  and _addGreaterAddCreateStateCheckAnswer_ y550 y551 y553 y554 y555 y556 y558 =
    fresh (q1 q2)
      ( y556 === Nat.zero
      &&& __greaterAddCreateStateCheckAnswer_ y550 y551 y553 y554 y555 y558
      ||| (y556 === Nat.succ q1 &&& (_addAddCreateStateCheckAnswer_ y550 y551 y553 y554 y555 q1 q2 (Nat.succ q1) &&& _greater (Nat.succ q2) y558)) )
  and __greaterAddCreateStateCheckAnswer_ y560 y561 y563 y564 y565 y566 =
    fresh (q1 q2 q3)
      ( y566 === Nat.succ q1
      &&& (y563 === Nat.zero)
      &&& ______addCreateStateCheckAnswer_ y560 y561 (Nat.zero) y564 y565 (Nat.zero)
      ||| (y566 === Nat.succ (Nat.succ q2) &&& (y563 === Nat.succ q3) &&& ___greaterAddCreateStateCheckAnswer_ y560 y561 y564 y565 q3 q2) )
  and ___greaterAddCreateStateCheckAnswer_ y574 y575 y577 y578 y580 y581 =
    fresh (q1 q2)
      ( y580 === Nat.zero
      &&& ______addCreateStateCheckAnswer_ y574 y575 (Nat.succ (Nat.zero)) y577 y578 (Nat.zero)
      ||| (y580 === Nat.succ q1 &&& (y581 === Nat.succ q2) &&& (______addCreateStateCheckAnswer_ y574 y575 (Nat.succ (Nat.succ q1)) y577 y578 (Nat.zero) &&& _greater q1 q2)) )
  and _addAddCreateStateCheckAnswer_ y589 y590 y592 y593 y594 y596 y597 y598 =
    fresh (q1 q2)
      ( y596 === Nat.zero
      &&& (y597 === Nat.succ y592)
      &&& ______addCreateStateCheckAnswer_ y589 y590 y592 y593 y594 y598
      ||| (y596 === Nat.succ q1 &&& (y597 === Nat.succ q2) &&& _addAddCreateStateCheckAnswer_ y589 y590 y592 y593 y594 q1 q2 y598) )
  and ______addCreateStateCheckAnswer_ y599 y600 y602 y603 y604 y606 =
    fresh (q1 q2)
      ( y606 === Nat.zero
      &&& ____createStateCheckAnswer_ y599 (Nat.zero) (Nat.succ y602) y600 y604 y603
      ||| (y606 === Nat.succ q1 &&& (____createStateCheckAnswer_ y599 (Nat.zero) (Nat.succ q2) y600 y604 y603 &&& add (Nat.succ y602) q1 q2)) )
  and anotherBottleGet_capacityGreaterAddGet_capacitySubGet_capacity y607 y608 y611 y612 y615 y616 =
    get_capacityGreaterAddGet_capacitySubGet_capacity y607 y608 y611 y612 y615 y616
  and get_capacityGreaterAddGet_capacitySubGet_capacity y617 y618 y620 y621 y624 y625 = greaterAddGet_capacitySubGet_capacity y617 y618 y620 y621 y624 y625
  and greaterAddGet_capacitySubGet_capacity y626 y627 y628 y629 y632 y633 =
    fresh (q1 q2)
      ( y626 === Nat.succ q1
      &&& (y627 === Nat.zero)
      &&& addGet_capacitySubGet_capacity y628 y629 y632 y633 q1
      ||| (y626 === Nat.succ q1 &&& (y627 === Nat.succ q2) &&& _greaterAddGet_capacitySubGet_capacity y628 y629 y632 y633 q1 q2) )
  and addGet_capacitySubGet_capacity y634 y635 y638 y639 y640 = get_capacitySubGet_capacity y634 y635 y638 y639 y640
  and get_capacitySubGet_capacity y641 y642 y644 y645 y646 =
    y644 === fst_ () &&& subGet_capacity y641 y642 y645 y646 ||| (y644 === snd_ () &&& _subGet_capacity y641 y642 y645 y646)
  and subGet_capacity y647 y648 y649 y650 = y647 === Nat.succ y650 &&& get_capacity y648 y649 (Nat.succ (Nat.zero)) (Nat.succ (Nat.succ y650))
  and _subGet_capacity y651 y652 y653 y654 =
    fresh (q1)
      (y654 === Nat.zero &&& (y651 === y654) &&& get_capacity y652 y653 (Nat.succ (Nat.zero)) (Nat.succ (Nat.succ (Nat.zero))) ||| (y654 === Nat.succ q1 &&& __subGet_capacity y651 y652 y653 q1))
  and __subGet_capacity y655 y656 y657 y658 =
    fresh (q1)
      ( y658 === Nat.zero &&& (y655 === y658)
      &&& get_capacity y656 y657 (Nat.succ (Nat.zero)) (Nat.succ (Nat.succ (Nat.succ (Nat.zero))))
      ||| (y658 === Nat.succ q1 &&& (_sub y655 q1 q1 &&& get_capacity y656 y657 (Nat.succ (Nat.zero)) (Nat.succ (Nat.succ (Nat.succ (Nat.succ q1)))))) )
  and _greaterAddGet_capacitySubGet_capacity y659 y660 y663 y664 y665 y666 =
    fresh (q1 q2 q3)
      ( y665 === Nat.succ q1
      &&& (y666 === Nat.zero)
      &&& _addGet_capacitySubGet_capacity y659 y660 y663 y664 q1
      ||| ( y665 === Nat.succ q1
          &&& (y666 === Nat.succ q2)
          &&& (_addSub (Nat.succ (Nat.succ q1)) y659 q3 (Nat.zero) &&& get_capacityGet_capacity y660 q3 y663 y664 (Nat.succ (Nat.succ (Nat.succ q2))) (Nat.succ (Nat.succ (Nat.succ q1))) &&& greater q1 q2) ) )
  and _addGet_capacitySubGet_capacity y667 y668 y671 y672 y673 = _get_capacitySubGet_capacity y667 y668 y671 y672 y673
  and _get_capacitySubGet_capacity y674 y675 y677 y678 y679 =
    y677 === fst_ () &&& ___subGet_capacity y674 y675 y678 y679 ||| (y677 === snd_ () &&& ____subGet_capacity y674 y675 y678 y679)
  and ___subGet_capacity y680 y681 y682 y683 = y680 === Nat.succ y683 &&& get_capacity y681 y682 (Nat.succ (Nat.succ (Nat.zero))) (Nat.succ (Nat.succ (Nat.succ y683)))
  and ____subGet_capacity y684 y685 y686 y687 =
    fresh (q1)
      ( y687 === Nat.zero &&& (y684 === y687)
      &&& get_capacity y685 y686 (Nat.succ (Nat.succ (Nat.zero))) (Nat.succ (Nat.succ (Nat.succ (Nat.zero))))
      ||| (y687 === Nat.succ q1 &&& _____subGet_capacity y684 y685 y686 q1) )
  and _____subGet_capacity y688 y689 y690 y691 =
    fresh (q1)
      ( y691 === Nat.zero &&& (y688 === y691)
      &&& get_capacity y689 y690 (Nat.succ (Nat.succ (Nat.zero))) (Nat.succ (Nat.succ (Nat.succ (Nat.succ (Nat.zero)))))
      ||| (y691 === Nat.succ q1 &&& (_sub y688 q1 q1 &&& get_capacity y689 y690 (Nat.succ (Nat.succ (Nat.zero))) (Nat.succ (Nat.succ (Nat.succ (Nat.succ (Nat.succ q1))))))) )
  and ___anotherBottle y693 = y693 === fst_ ()
  and _____________doStepCheckAnswer_ y709 y710 y711 =
    fresh (q1 q2) (y709 === Pair.pair q1 q2 &&& ____createStateCheckAnswer_ y710 q2 (Nat.zero) y711 q1 q2)
  in
  checkAnswer x0 x1 x2 *)