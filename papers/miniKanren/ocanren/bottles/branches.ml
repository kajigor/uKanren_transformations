open OCanren
open OCanren.Std
open Helper

let topLevel x0 x1 x2 =
  let rec checkAnswer y0 y1 y2 =
    fresh (q1)
      ( (y0 === Std.List.nil ()) &&& (__isFinishState y2 (o ()) (o ()))
      ||| ((y0 === Std.( % ) (Pair.pair (fill ()) (fst_ ())) q1) &&& doStepCheckAnswer_ y1 y2 q1)
      ||| ((y0 === Std.( % ) (Pair.pair (fill ()) (snd_ ())) q1) &&& _____________doStepCheckAnswer_ y1 y2 q1)
      ||| ((y0 === Std.( % ) (Pair.pair (empty ()) (fst_ ())) q1) &&& ___fst_FancyEqDoStepCheckAnswer_ y1 y2 q1)
      ||| ((y0 === Std.( % ) (Pair.pair (empty ()) (snd_ ())) q1) &&& __snd_FancyEqDoStepCheckAnswer_ y1 y2 q1) )
  and doStepCheckAnswer_ y5 y6 y7 = fresh (q1 q2) (y5 === Pair.pair q1 q2 &&& _______createStateCheckAnswer_ y6 y7 q1 q2 (o ()) q1)
  and __isFinishState y45 y46 y47 =
    fresh (q1 q2 q3 q4)
      ( y46 === o ()
      &&& (y45 === o ())
      &&& _______fancyEq q1 y47 (o ())
      ||| (y46 === s q2 &&& (y45 === s q3) &&& __fancyEqFancyEq y47 q2 q3)
      ||| (y46 === o () &&& (y45 === s q4) &&& _______fancyEq !!true y47 (s q4))
      ||| (y46 === s q2 &&& (y45 === o ()) &&& _______fancyEq !!true y47 (o ()))
      ||| (y46 === s q2 &&& (y45 === s q3) &&& ___fancyEqFancyEq y47 q2 q3) )
  and __fancyEqFancyEq y50 y52 y53 =
    fresh (q1 q2 q3)
      ( y52 === o ()
      &&& (y53 === o ())
      &&& _______fancyEq q1 y50 (s (o ()))
      ||| (y52 === s q2 &&& (y53 === s q3) &&& (_______fancyEq !!true q2 q3 &&& _______fancyEq q1 y50 (s (s q3)))) )
  and _______fancyEq y57 y58 y59 =
    fresh (q1 q2 q3)
      ( y58 === o ()
      &&& (y59 === o ())
      &&& (y57 === !!true)
      ||| (y58 === o () &&& (y59 === s q1) &&& (y57 === !!false))
      ||| (y58 === s q2 &&& (y59 === o ()) &&& (y57 === !!false))
      ||| (y58 === s q2 &&& (y59 === s q3) &&& _______fancyEq y57 q2 q3) )
  and ___fancyEqFancyEq y60 y61 y62 =
    fresh (q1 q2 q3)
      ( y61 === o ()
      &&& (y62 === s q1)
      &&& _______fancyEq !!true y60 (s (s q1))
      ||| (y61 === s q2 &&& (y62 === o ()) &&& _______fancyEq !!true y60 (s (o ())))
      ||| (y61 === s q2 &&& (y62 === s q3) &&& (_______fancyEq !!false q2 q3 &&& _______fancyEq !!true y60 (s (s q3)))) )
  and ____createStateCheckAnswer_ y159 y160 y161 y163 y164 y165 = ____checkAnswer_ y159 y160 y161 y163 y164 y165
  and ____checkAnswer_ y166 y167 y168 y169 y170 y171 =
    fresh (q1 q2)
      ( y169 === Std.List.nil () &&& __isFinishState y166 y168 y167
      ||| (y169 === Std.( % ) (Pair.pair (fill ()) (fst_ ())) q1 &&& (y168 === o ()) &&& _______doStepCheckAnswer_ y166 y170 y171 q1 y167)
      ||| (y169 === Std.( % ) (Pair.pair (fill ()) (snd_ ())) q1 &&& (y167 === o ()) &&& ________doStepCheckAnswer_ y166 y170 y171 q1 y168)
      ||| (y169 === Std.( % ) (Pair.pair (empty ()) (fst_ ())) q1 &&& __fst_FancyEqDoStepCheckAnswer_ y166 y170 y171 q1 y167 y168)
      ||| (y169 === Std.( % ) (Pair.pair (empty ()) (snd_ ())) q1 &&& _snd_FancyEqDoStepCheckAnswer_ y166 y170 y171 q1 y168 y167)
      ||| ( y169
          === Std.( % ) (Pair.pair (pour ()) (fst_ ())) q1
          &&& (y168 === s q2)
          &&& _anotherBottleGet_capacityFancyEqDoStepCheckAnswer_ y166 y170 y171 q1 y167 q2 )
      ||| ( y169
          === Std.( % ) (Pair.pair (pour ()) (snd_ ())) q1
          &&& (y167 === s q2)
          &&& __anotherBottleGet_capacityFancyEqDoStepCheckAnswer_ y166 y170 y171 q1 y168 q2 ) )
  and _______doStepCheckAnswer_ y172 y173 y174 y175 y177 = _______createStateCheckAnswer_ y172 y175 y173 y174 y177 y173
  and ________doStepCheckAnswer_ y184 y185 y186 y187 y189 = ____createStateCheckAnswer_ y184 y186 y189 y187 y185 y186
  and __fst_FancyEqDoStepCheckAnswer_ y190 y191 y192 y193 y195 y196 = ____fancyEqDoStepCheckAnswer_ y190 y193 y195 y196 y191 y192
  and ____fancyEqDoStepCheckAnswer_ y198 y199 y201 y202 y203 y204 =
    fresh (q1 q2)
      ( y202 === o ()
      &&& (y203 === o ())
      &&& _________doStepCheckAnswer_ y198 y199 y201 y204 (o ()) (o ()) (o ())
      ||| (y202 === s q1 &&& (y203 === s q2) &&& (_________doStepCheckAnswer_ y198 y199 y201 y204 (s q1) (s q2) (s q2) &&& _______fancyEq !!true q1 q2)) )
  and _________doStepCheckAnswer_ y205 y206 y208 y209 y210 y211 y212 = ____checkAnswer_ y205 y208 (o ()) y206 y212 y209
  and _snd_FancyEqDoStepCheckAnswer_ y213 y214 y215 y216 y218 y219 = _____fancyEqDoStepCheckAnswer_ y213 y216 y218 y219 y214 y215
  and _____fancyEqDoStepCheckAnswer_ y221 y222 y224 y225 y226 y227 =
    fresh (q1 q2)
      ( y225 === o ()
      &&& (y227 === o ())
      &&& __________doStepCheckAnswer_ y221 y222 y224 y226 (o ()) (o ()) (o ())
      ||| (y225 === s q1 &&& (y227 === s q2) &&& (__________doStepCheckAnswer_ y221 y222 y224 y226 (s q1) (s q2) (s q2) &&& _______fancyEq !!true q1 q2)) )
  and __________doStepCheckAnswer_ y228 y229 y231 y232 y233 y234 y235 = ____createStateCheckAnswer_ y228 (o ()) y231 y229 y232 y235
  and _anotherBottleGet_capacityFancyEqDoStepCheckAnswer_ y236 y237 y238 y239 y241 y244 = _get_capacityFancyEqDoStepCheckAnswer_ y236 y237 y238 y239 y241 y244
  and _get_capacityFancyEqDoStepCheckAnswer_ y245 y246 y247 y248 y250 y252 = ______fancyEqDoStepCheckAnswer_ y245 y248 y250 y252 y246 y247
  and ______fancyEqDoStepCheckAnswer_ y253 y254 y256 y257 y258 y259 =
    fresh (q1 q2 q3)
      ( y256 === o ()
      &&& (y259 === s q1)
      &&& ___________doStepCheckAnswer_ y253 y254 y257 y258 (o ()) (s q1) (s q1)
      ||| (y256 === s q2 &&& (y259 === o ()) &&& ___________doStepCheckAnswer_ y253 y254 y257 y258 (s q2) (o ()) (o ()))
      ||| (y256 === s q2 &&& (y259 === s q3) &&& (___________doStepCheckAnswer_ y253 y254 y257 y258 (s q2) (s q3) (s q3) &&& _______fancyEq !!false q2 q3)) )
  and ___________doStepCheckAnswer_ y260 y261 y263 y264 y265 y266 y267 =
    fresh (q1 q2 q3 q4 q5)
      ( addGreaterAddSubCreateStateCheckAnswer_ y260 y261 y264 y267 y265 q1 q2 q3 y263
      &&& anotherBottleGet_capacityGet_capacityGet_capacity y264 y266 q1 q2 q3 q4 q5
      &&& _anotherBottle q4 &&& _anotherBottle q5
      ||| (addGreaterAddCreateStateCheckAnswer_ y260 y261 y264 y267 y265 q1 y263 &&& anotherBottleGet_capacity y264 y266 q1) )
  and addGreaterAddSubCreateStateCheckAnswer_ y268 y269 y271 y272 y273 y274 y276 y278 y279 =
    fresh (q1 q2)
      ( y279 === o ()
      &&& greaterAddSubCreateStateCheckAnswer_ y268 y269 y271 y272 y274 y276 y278 y273
      ||| (y279 === s q1 &&& (addAddSubCreateStateCheckAnswer_ y268 y269 y271 y272 y273 y276 y278 q1 q2 (s q1) &&& greater (s (s q2)) y274)) )
  and greaterAddSubCreateStateCheckAnswer_ y281 y282 y284 y285 y286 y288 y290 y291 =
    fresh (q1 q2)
      ( y286 === o ()
      &&& _addSubCreateStateCheckAnswer_ y281 y282 y284 y285 y288 y290 y291 (o ())
      ||| (y286 === s (o ()) &&& (y291 === s q1) &&& _addSubCreateStateCheckAnswer_ y281 y282 y284 y285 y288 y290 (s q1) (o ()))
      ||| (y286 === s (s q2) &&& (y291 === s q1) &&& _greaterAddSubCreateStateCheckAnswer_ y281 y282 y284 y285 y288 y290 q1 q2) )
  and _subCreateStateCheckAnswer_ y318 y319 y321 y322 y324 y325 y326 =
    fresh (q1 q2)
      ( y325 === o ()
      &&& _______createStateCheckAnswer_ y318 y319 y321 y322 y324 y326
      ||| (y325 === s q1 &&& (y326 === o ()) &&& _______createStateCheckAnswer_ y318 y319 y321 y322 y324 (o ()))
      ||| (y325 === s q1 &&& (y326 === s q2) &&& _subCreateStateCheckAnswer_ y318 y319 y321 y322 y324 q1 q2) )
  and _______createStateCheckAnswer_ y327 y328 y330 y331 y332 y333 = ____checkAnswer_ y327 y332 y333 y328 y330 y331
  and _greaterAddSubCreateStateCheckAnswer_ y334 y335 y337 y338 y340 y342 y343 y344 =
    fresh (q1 q2)
      ( y343 === s q1
      &&& (y344 === o ())
      &&& _addSubCreateStateCheckAnswer_ y334 y335 y337 y338 y340 y342 (s (s q1)) (o ())
      ||| (y343 === s q1 &&& (y344 === s q2) &&& (_addSubCreateStateCheckAnswer_ y334 y335 y337 y338 y340 y342 (s (s q1)) (o ()) &&& greater q1 q2)) )
  and greater y345 y346 = fresh (q1 q2) (y345 === s q1 &&& (y346 === o ()) ||| (y345 === s q1 &&& (y346 === s q2) &&& greater q1 q2))
  and addAddSubCreateStateCheckAnswer_ y347 y348 y350 y351 y352 y354 y356 y357 y358 y359 =
    fresh (q1 q2)
      ( y357 === o () &&& (y352 === y358)
      &&& _addSubCreateStateCheckAnswer_ y347 y348 y350 y351 y354 y356 y358 y359
      ||| (y357 === s q1 &&& (y358 === s q2) &&& addAddSubCreateStateCheckAnswer_ y347 y348 y350 y351 y352 y354 y356 q1 q2 y359) )
  and _addSubCreateStateCheckAnswer_ y360 y361 y363 y364 y366 y368 y369 y370 =
    fresh (q1)
      ( y370 === o ()
      &&& _subCreateStateCheckAnswer_ y360 y361 y363 y364 y366 y368 (s y369)
      ||| (y370 === s q1 &&& __addSubCreateStateCheckAnswer_ y360 y361 y363 y364 y366 y368 y369 q1) )
  and __addSubCreateStateCheckAnswer_ y371 y372 y374 y375 y377 y378 y379 y380 =
    fresh (q1 q2)
      ( y380 === o ()
      &&& _subCreateStateCheckAnswer_ y371 y372 y374 y375 y377 y378 (s (s y379))
      ||| (y380 === s q1 &&& (_subCreateStateCheckAnswer_ y371 y372 y374 y375 y377 y378 (s (s (s q2))) &&& add y379 q1 q2)) )
  and add y382 y383 y384 = fresh (q1 q2) (y383 === o () &&& (y382 === y384) ||| (y383 === s q1 &&& (y384 === s q2) &&& add y382 q1 q2))
  and anotherBottleGet_capacityGet_capacityGet_capacity y385 y386 y387 y389 y390 y391 y392 =
    get_capacityGet_capacityGet_capacity y385 y386 y387 y389 y390 y391 y392
  and get_capacityGet_capacityGet_capacity y393 y394 y395 y396 y397 y398 y399 = y395 === y394 &&& get_capacityGet_capacity y396 y397 y398 y399 y393 y394
  and get_capacityGet_capacity y400 y401 y402 y403 y404 y405 =
    y402 === fst_ () &&& fst_Get_capacity y400 y401 y403 y404 y405 ||| (y402 === snd_ () &&& snd_Get_capacity y400 y401 y403 y404 y405)
  and fst_Get_capacity y406 y407 y408 y409 y410 = y407 === y409 &&& get_capacity y406 y408 y409 y410
  and get_capacity y411 y412 y413 y414 = y412 === fst_ () &&& fst_ y411 y413 y414 ||| (y412 === snd_ () &&& snd_ y411 y413 y414)
  and fst_ y415 y416 y417 = y415 === y416
  and snd_ y418 y419 y420 = y418 === y420
  and snd_Get_capacity y421 y422 y423 y424 y425 = y422 === y425 &&& get_capacity y421 y423 y424 y425
  and _anotherBottle y427 = y427 === snd_ ()
  and addGreaterAddCreateStateCheckAnswer_ y428 y429 y431 y432 y433 y434 y436 =
    fresh (q1 q2)
      ( y436 === o ()
      &&& greaterAddCreateStateCheckAnswer_ y428 y429 y431 y432 y434 y433
      ||| (y436 === s q1 &&& (addAddCreateStateCheckAnswer_ y428 y429 y431 y432 y433 q1 q2 (s q1) &&& _greater (s (s q2)) y434)) )
  and greaterAddCreateStateCheckAnswer_ y438 y439 y441 y442 y443 y445 =
    fresh (q1 q2 q3)
      ( y443 === s q1
      &&& (y445 === o ())
      &&& __addCreateStateCheckAnswer_ y438 y439 y441 y442 (o ()) (o ())
      ||| (y443 === s (s q2) &&& (y445 === s q3) &&& _greaterAddCreateStateCheckAnswer_ y438 y439 y441 y442 q3 q2) )
  and _greaterAddCreateStateCheckAnswer_ y452 y453 y455 y456 y458 y459 =
    fresh (q1 q2)
      ( y458 === o ()
      &&& __addCreateStateCheckAnswer_ y452 y453 y455 y456 (s (o ())) (o ())
      ||| (y458 === s q1 &&& (y459 === s q2) &&& (__addCreateStateCheckAnswer_ y452 y453 y455 y456 (s (s q1)) (o ()) &&& _greater q1 q2)) )
  and _greater y460 y461 = fresh (q1 q2) (y460 === o () ||| (y460 === s q1 &&& (y461 === s q2) &&& _greater q1 q2))
  and addAddCreateStateCheckAnswer_ y469 y470 y472 y473 y474 y476 y477 y478 =
    fresh (q1 q2)
      ( y476 === o () &&& (y474 === y477)
      &&& __addCreateStateCheckAnswer_ y469 y470 y472 y473 y477 y478
      ||| (y476 === s q1 &&& (y477 === s q2) &&& addAddCreateStateCheckAnswer_ y469 y470 y472 y473 y474 q1 q2 y478) )
  and __addCreateStateCheckAnswer_ y479 y480 y482 y483 y485 y486 =
    fresh (q1)
      ( y486 === o ()
      &&& _______createStateCheckAnswer_ y479 y480 y482 y483 (s y485) (o ())
      ||| (y486 === s q1 &&& ___addCreateStateCheckAnswer_ y479 y480 y482 y483 y485 q1) )
  and ___addCreateStateCheckAnswer_ y487 y488 y490 y491 y492 y493 =
    fresh (q1 q2)
      ( y493 === o ()
      &&& _______createStateCheckAnswer_ y487 y488 y490 y491 (s (s y492)) (o ())
      ||| (y493 === s q1 &&& (_______createStateCheckAnswer_ y487 y488 y490 y491 (s (s (s q2))) (o ()) &&& add y492 q1 q2)) )
  and anotherBottleGet_capacity y495 y496 y497 = get_capacity y497 (snd_ ()) y495 y496
  and __anotherBottleGet_capacityFancyEqDoStepCheckAnswer_ y499 y500 y501 y502 y504 y507 =
    __get_capacityFancyEqDoStepCheckAnswer_ y499 y500 y501 y502 y504 y507
  and __get_capacityFancyEqDoStepCheckAnswer_ y508 y509 y510 y511 y513 y515 = _______fancyEqDoStepCheckAnswer_ y508 y511 y513 y515 y509 y510
  and _______fancyEqDoStepCheckAnswer_ y516 y517 y519 y520 y521 y522 =
    fresh (q1 q2 q3)
      ( y519 === o ()
      &&& (y521 === s q1)
      &&& ____________doStepCheckAnswer_ y516 y517 y520 y522 (o ()) (s q1) (s q1)
      ||| (y519 === s q2 &&& (y521 === o ()) &&& ____________doStepCheckAnswer_ y516 y517 y520 y522 (s q2) (o ()) (o ()))
      ||| (y519 === s q2 &&& (y521 === s q3) &&& (____________doStepCheckAnswer_ y516 y517 y520 y522 (s q2) (s q3) (s q3) &&& _______fancyEq !!false q2 q3)) )
  and ____________doStepCheckAnswer_ y523 y524 y526 y527 y528 y529 y530 =
    fresh (q1 q2 q3 q4 q5 q6)
      ( addGreaterAddSub y526 y528 q1 q2 q3
      &&& _get_capacityGet_capacityGet_capacity y527 y529 q1 q4 q3 q5 q6
      &&& ____createStateCheckAnswer_ y523 q2 q4 y524 y530 y527 &&& ___anotherBottle q5 &&& ___anotherBottle q6
      ||| (_addGreaterAddCreateStateCheckAnswer_ y523 y524 y526 y527 y530 y528 q1 &&& get_capacity q1 (fst_ ()) y529 y527) )
  and addGreaterAddSub y531 y532 y534 y535 y537 =
    fresh (q1 q2) (y532 === o () &&& greaterAddSub y531 y534 y535 y537 ||| (y532 === s q1 &&& (addAddSub y531 y535 y537 q1 q2 (s q1) &&& greater (s q2) y534)))
  and greaterAddSub y538 y539 y540 y542 =
    fresh (q1 q2)
      ( y539 === o ()
      &&& _addSub y538 y540 y542 (o ())
      ||| (y539 === s (o ()) &&& (y538 === s q1) &&& _addSub (s q1) y540 y542 (o ()))
      ||| (y539 === s (s q2) &&& (y538 === s q1) &&& _greaterAddSub y540 y542 q1 q2) )
  and _sub y550 y551 y552 =
    fresh (q1 q2)
      ( y551 === o () &&& (y550 === y552)
      ||| (y551 === s q1 &&& (y552 === o ()) &&& (y550 === o ()))
      ||| (y551 === s q1 &&& (y552 === s q2) &&& _sub y550 q1 q2) )
  and _greaterAddSub y553 y555 y556 y557 =
    fresh (q1 q2)
      ( y556 === s q1
      &&& (y557 === o ())
      &&& _addSub (s (s q1)) y553 y555 (o ())
      ||| (y556 === s q1 &&& (y557 === s q2) &&& (_addSub (s (s q1)) y553 y555 (o ()) &&& greater q1 q2)) )
  and addAddSub y558 y559 y561 y562 y563 y564 =
    fresh (q1 q2)
      (y562 === o () &&& (y563 === s y558) &&& _addSub y558 y559 y561 y564 ||| (y562 === s q1 &&& (y563 === s q2) &&& addAddSub y558 y559 y561 q1 q2 y564))
  and _addSub y565 y566 y568 y569 =
    fresh (q1 q2) (y569 === o () &&& _sub y566 y568 (s y565) ||| (y569 === s q1 &&& (add (s y565) q1 q2 &&& _sub y566 y568 (s q2))))
  and _get_capacityGet_capacityGet_capacity y570 y571 y572 y573 y574 y575 y576 = y572 === y571 &&& get_capacityGet_capacity y573 y574 y575 y576 y571 y570
  and _addGreaterAddCreateStateCheckAnswer_ y577 y578 y580 y581 y582 y583 y585 =
    fresh (q1 q2)
      ( y583 === o ()
      &&& __greaterAddCreateStateCheckAnswer_ y577 y578 y580 y581 y582 y585
      ||| (y583 === s q1 &&& (_addAddCreateStateCheckAnswer_ y577 y578 y580 y581 y582 q1 q2 (s q1) &&& _greater (s q2) y585)) )
  and __greaterAddCreateStateCheckAnswer_ y587 y588 y590 y591 y592 y593 =
    fresh (q1 q2 q3)
      ( y593 === s q1
      &&& (y590 === o ())
      &&& ______addCreateStateCheckAnswer_ y587 y588 (o ()) y591 y592 (o ())
      ||| (y593 === s (s q2) &&& (y590 === s q3) &&& ___greaterAddCreateStateCheckAnswer_ y587 y588 y591 y592 q3 q2) )
  and ___greaterAddCreateStateCheckAnswer_ y601 y602 y604 y605 y607 y608 =
    fresh (q1 q2)
      ( y607 === o ()
      &&& ______addCreateStateCheckAnswer_ y601 y602 (s (o ())) y604 y605 (o ())
      ||| (y607 === s q1 &&& (y608 === s q2) &&& (______addCreateStateCheckAnswer_ y601 y602 (s (s q1)) y604 y605 (o ()) &&& _greater q1 q2)) )
  and _addAddCreateStateCheckAnswer_ y616 y617 y619 y620 y621 y623 y624 y625 =
    fresh (q1 q2)
      ( y623 === o ()
      &&& (y624 === s y619)
      &&& ______addCreateStateCheckAnswer_ y616 y617 y619 y620 y621 y625
      ||| (y623 === s q1 &&& (y624 === s q2) &&& _addAddCreateStateCheckAnswer_ y616 y617 y619 y620 y621 q1 q2 y625) )
  and ______addCreateStateCheckAnswer_ y626 y627 y629 y630 y631 y633 =
    fresh (q1 q2)
      ( y633 === o ()
      &&& ____createStateCheckAnswer_ y626 (o ()) (s y629) y627 y631 y630
      ||| (y633 === s q1 &&& (____createStateCheckAnswer_ y626 (o ()) (s q2) y627 y631 y630 &&& add (s y629) q1 q2)) )
  and anotherBottleGet_capacityGreaterAddGet_capacitySubGet_capacity y634 y635 y638 y639 y642 y643 =
    get_capacityGreaterAddGet_capacitySubGet_capacity y634 y635 y638 y639 y642 y643
  and get_capacityGreaterAddGet_capacitySubGet_capacity y644 y645 y647 y648 y651 y652 = greaterAddGet_capacitySubGet_capacity y644 y645 y647 y648 y651 y652
  and greaterAddGet_capacitySubGet_capacity y653 y654 y655 y656 y659 y660 =
    fresh (q1 q2)
      ( y653 === s q1
      &&& (y654 === o ())
      &&& addGet_capacitySubGet_capacity y655 y656 y659 y660 q1
      ||| (y653 === s q1 &&& (y654 === s q2) &&& _greaterAddGet_capacitySubGet_capacity y655 y656 y659 y660 q1 q2) )
  and addGet_capacitySubGet_capacity y661 y662 y665 y666 y667 = get_capacitySubGet_capacity y661 y662 y665 y666 y667
  and get_capacitySubGet_capacity y668 y669 y671 y672 y673 =
    y671 === fst_ () &&& fst_SubGet_capacity y668 y669 y672 y673 ||| (y671 === snd_ () &&& snd_SubGet_capacity y668 y669 y672 y673)
  and fst_SubGet_capacity y674 y675 y677 y678 = subGet_capacity y674 y675 y677 y678
  and subGet_capacity y679 y680 y681 y682 = y679 === s y682 &&& get_capacity y680 y681 (s (o ())) (s (s y682))
  and snd_SubGet_capacity y683 y684 y686 y687 = _subGet_capacity y683 y684 y686 y687
  and _subGet_capacity y688 y689 y690 y691 =
    fresh (q1)
      (y691 === o () &&& (y688 === y691) &&& get_capacity y689 y690 (s (o ())) (s (s (o ()))) ||| (y691 === s q1 &&& __subGet_capacity y688 y689 y690 q1))
  and __subGet_capacity y692 y693 y694 y695 =
    fresh (q1)
      ( y695 === o () &&& (y692 === y695)
      &&& get_capacity y693 y694 (s (o ())) (s (s (s (o ()))))
      ||| (y695 === s q1 &&& (_sub y692 q1 q1 &&& get_capacity y693 y694 (s (o ())) (s (s (s (s q1)))))) )
  and _greaterAddGet_capacitySubGet_capacity y696 y697 y700 y701 y702 y703 =
    fresh (q1 q2 q3)
      ( y702 === s q1
      &&& (y703 === o ())
      &&& _addGet_capacitySubGet_capacity y696 y697 y700 y701 q1
      ||| ( y702 === s q1
          &&& (y703 === s q2)
          &&& (_addSub (s (s q1)) y696 q3 (o ()) &&& get_capacityGet_capacity y697 q3 y700 y701 (s (s (s q2))) (s (s (s q1))) &&& greater q1 q2) ) )
  and _addGet_capacitySubGet_capacity y704 y705 y708 y709 y710 = _get_capacitySubGet_capacity y704 y705 y708 y709 y710
  and _get_capacitySubGet_capacity y711 y712 y714 y715 y716 =
    y714 === fst_ () &&& _fst_SubGet_capacity y711 y712 y715 y716 ||| (y714 === snd_ () &&& _snd_SubGet_capacity y711 y712 y715 y716)
  and _fst_SubGet_capacity y717 y718 y720 y721 = ___subGet_capacity y717 y718 y720 y721
  and ___subGet_capacity y722 y723 y724 y725 = y722 === s y725 &&& get_capacity y723 y724 (s (s (o ()))) (s (s (s y725)))
  and _snd_SubGet_capacity y726 y727 y729 y730 = ____subGet_capacity y726 y727 y729 y730
  and ____subGet_capacity y731 y732 y733 y734 =
    fresh (q1)
      ( y734 === o () &&& (y731 === y734)
      &&& get_capacity y732 y733 (s (s (o ()))) (s (s (s (o ()))))
      ||| (y734 === s q1 &&& _____subGet_capacity y731 y732 y733 q1) )
  and _____subGet_capacity y735 y736 y737 y738 =
    fresh (q1)
      ( y738 === o () &&& (y735 === y738)
      &&& get_capacity y736 y737 (s (s (o ()))) (s (s (s (s (o ())))))
      ||| (y738 === s q1 &&& (_sub y735 q1 q1 &&& get_capacity y736 y737 (s (s (o ()))) (s (s (s (s (s q1))))))) )
  and ___anotherBottle y740 = y740 === fst_ ()
  and _____________doStepCheckAnswer_ y756 y757 y758 = fresh (q1 q2) (y756 === Pair.pair q1 q2 &&& ____createStateCheckAnswer_ y757 q2 (o ()) y758 q1 q2)
  and ___fst_FancyEqDoStepCheckAnswer_ y760 y761 y762 = fresh (q1 q2) (y760 === Pair.pair q1 q2 &&& ____fancyEqDoStepCheckAnswer_ y761 y762 (o ()) (o ()) q1 q2)
  and __snd_FancyEqDoStepCheckAnswer_ y765 y766 y767 =
    fresh (q1 q2) (y765 === Pair.pair q1 q2 &&& _____fancyEqDoStepCheckAnswer_ y766 y767 (o ()) (o ()) q1 q2)
  in
  checkAnswer x0 x1 x2