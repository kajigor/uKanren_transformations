sorto y0 =
  fresh q1, q2, q3, q4, q5, q6, q7 in
    ((y0 = (q1 :: (q2 :: (q3 :: (q4 :: (q5 :: (q6 :: [q7])))))) /\ fn1 q1 q2 q3 q4 q5 q6 q7));

fn1 y23 y26 y29 y32 y35 y38 y39 =
  fresh q1, q2, q3, q4 in
    (((fn2 q1 q2 q3 q4 y26 y29 y32 y35 y38 y39 /\ fn3 q1 q2 q3 y23 q4) \/ (fn4 q1 q2 q3 q4 y26 y29 y32 y35 y38 y39 /\ fn89 q1 q2 q3 y23 q4)));

fn2 y41 y45 y51 y59 y60 y63 y66 y69 y72 y73 =
  fn5 y41 y45 y51 y59 y60 y63 y66 y69 y72 y73;

fn5 y75 y77 y83 y91 y92 y95 y98 y101 y104 y105 =
  fresh q1, q2, q3 in
    (((y75 = Zero /\ fn6 q1 q2 q3 y95 y98 y101 y104 y105 /\ fn51 y77 q1 y83 q2 y91 y92 q3) \/
    (y75 = Succ Zero /\ fn6 q1 q2 q3 y95 y98 y101 y104 y105 /\ fn52 y77 q1 y83 q2 y91 y92 q3) \/
    (y75 = Succ Succ Zero /\ fn6 q1 q2 q3 y95 y98 y101 y104 y105 /\ fn53 y77 q1 y83 q2 y91 y92 q3) \/
    (y75 = Succ Succ Succ Zero /\ fn6 q1 q2 q3 y95 y98 y101 y104 y105 /\ fn54 y77 q1 y83 q2 y91 y92 q3) \/
    (y75 = Zero /\ fn55 q1 q2 q3 y95 y98 y101 y104 y105 /\ fn54 y77 q1 y83 q2 y91 y92 q3) \/
    (y75 = Succ Zero /\ fn60 q1 q2 q3 y95 y98 y101 y104 y105 /\ fn54 y77 q1 y83 q2 y91 y92 q3) \/
    (y75 = Succ Succ Zero /\ fn68 q1 q2 q3 y95 y98 y101 y104 y105 /\ fn54 y77 q1 y83 q2 y91 y92 q3)));

fn6 y107 y111 y117 y118 y121 y124 y127 y128 =
  fn7 y107 y111 y117 y118 y121 y124 y127 y128;

fn7 y130 y132 y138 y139 y142 y145 y148 y149 =
  fresh q1, q2 in
    (((y130 = Zero /\ fn8 q1 q2 y142 y145 y148 y149 /\ fn11 y132 q1 y138 y139 q2) \/
    (y130 = Succ Zero /\ fn8 q1 q2 y142 y145 y148 y149 /\ fn24 y132 q1 y138 y139 q2) \/
    (y130 = Succ Succ Zero /\ fn8 q1 q2 y142 y145 y148 y149 /\ fn25 y132 q1 y138 y139 q2) \/
    (y130 = Succ Succ Succ Zero /\ fn8 q1 q2 y142 y145 y148 y149 /\ fn26 y132 q1 y138 y139 q2) \/
    (y130 = Zero /\ fn27 q1 q2 y142 y145 y148 y149 /\ fn26 y132 q1 y138 y139 q2) \/
    (y130 = Succ Zero /\ fn32 q1 q2 y142 y145 y148 y149 /\ fn26 y132 q1 y138 y139 q2) \/
    (y130 = Succ Succ Zero /\ fn40 q1 q2 y142 y145 y148 y149 /\ fn26 y132 q1 y138 y139 q2)));

fn8 y151 y155 y156 y159 y162 y163 =
  fn9 y151 y155 y156 y159 y162 y163;

fn9 y165 y167 y168 y171 y174 y175 =
  fresh q1 in
    (((y165 = Zero /\ fn10 q1 y171 y174 y175 /\ fn11 y167 y168 q1) \/
    (y165 = Succ Zero /\ fn10 q1 y171 y174 y175 /\ fn12 y167 y168 q1) \/
    (y165 = Succ Succ Zero /\ fn10 q1 y171 y174 y175 /\ fn13 y167 y168 q1) \/
    (y165 = Succ Succ Succ Zero /\ fn10 q1 y171 y174 y175 /\ fn14 y167 y168 q1) \/
    (y165 = Zero /\ fn15 q1 y171 y174 y175 /\ fn14 y167 y168 q1) \/
    (y165 = Succ Zero /\ fn18 q1 y171 y174 y175 /\ fn14 y167 y168 q1) \/
    (y165 = Succ Succ Zero /\ fn21 q1 y171 y174 y175 /\ fn14 y167 y168 q1)));

fn10 y177 y178 y181 y182 = fn12 y177 y178 y181 y182;

fn12 y184 y185 y187 y188 =
  ((y185 = Zero /\ y184 = y185 /\ fn14 (Succ Succ Succ Zero) y187 y188) \/
  (y185 = Succ Zero /\ y184 = y185 /\ fn14 (Succ Succ Succ Zero) y187 y188) \/
  (y185 = Succ Succ Zero /\ y184 = y185 /\ fn14 (Succ Succ Succ Zero) y187 y188) \/
  (y185 = Succ Succ Succ Zero /\ y184 = y185 /\ fn14 (Succ Succ Succ Zero) y187 y188) \/
  (y185 = Succ Succ Succ Zero /\ y184 = Zero /\ fn14 Zero y187 y188) \/
  (y185 = Succ Succ Succ Zero /\ y184 = Succ Zero /\ fn14 (Succ Zero) y187 y188) \/
  (y185 = Succ Succ Succ Zero /\ y184 = Succ Succ Zero /\ fn14 (Succ Succ Zero) y187 y188));

fn11 y189 y190 y191 =
  (y191 = Zero /\ y190 = Zero /\ y189 = y190);

fn12 y192 y193 y194 =
  ((y194 = Succ Zero /\ y193 = Zero /\ y192 = y193) \/
  (y194 = Succ Zero /\ y193 = Succ Zero /\ y192 = y193) \/
   (y194 = Zero /\ y193 = Succ Zero /\ y192 = y194));

fn13 y195 y196 y197 =
  ((y197 = Succ Succ Zero /\ y196 = Zero /\ y195 = y196) \/
   (y197 = Succ Succ Zero /\ y196 = Succ Zero /\ y195 = y196) \/
   (y197 = Succ Succ Zero /\ y196 = Succ Succ Zero /\ y195 = y196) \/
   (y197 = Zero /\ y196 = Succ Succ Zero /\ y195 = y197) \/
   (y197 = Succ Zero /\ y196 = Succ Succ Zero /\ y195 = y197));

fn14 y198 y199 y200 =
  ((y200 = Succ Succ Succ Zero /\ y199 = Zero /\ y198 = y199) \/
  (y200 = Succ Succ Succ Zero /\ y199 = Succ Zero /\ y198 = y199) \/
  (y200 = Succ Succ Succ Zero /\ y199 = Succ Succ Zero /\ y198 = y199) \/
  (y200 = Succ Succ Succ Zero /\ y199 = Succ Succ Succ Zero /\ y198 = y199) \/
  (y200 = Zero /\ y199 = Succ Succ Succ Zero /\ y198 = y200) \/
  (y200 = Succ Zero /\ y199 = Succ Succ Succ Zero /\ y198 = y200) \/
  (y200 = Succ Succ Zero /\ y199 = Succ Succ Succ Zero /\ y198 = y200));

fn15 y201 y202 y205 y206 =
  (fn16 y201 y202 y205 y206 \/ fn17 y201 y202 y205 y206);

fn16 y208 y209 y211 y212 =
  (y209 = Zero /\ y208 = y209 /\ fn14 Zero y211 y212);

fn17 y213 y214 y216 y217 =
  ((y214 = Zero /\ y213 = y214 /\ fn11 (Succ Succ Succ Zero) y216 y217) \/
  (y214 = Succ Zero /\ y213 = y214 /\ fn11 (Succ Succ Succ Zero) y216 y217) \/
  (y214 = Succ Succ Zero /\ y213 = y214 /\ fn11 (Succ Succ Succ Zero) y216 y217) \/
  (y214 = Succ Succ Succ Zero /\ y213 = y214 /\ fn11 (Succ Succ Succ Zero) y216 y217) \/
  (y214 = Succ Succ Succ Zero /\ y213 = Zero /\ fn11 Zero y216 y217) \/
  (y214 = Succ Succ Succ Zero /\ y213 = Succ Zero /\ fn11 (Succ Zero) y216 y217) \/
  (y214 = Succ Succ Succ Zero /\ y213 = Succ Succ Zero /\ fn11 (Succ Succ Zero) y216 y217));

fn18 y218 y219 y222 y223 =
  (fn19 y218 y219 y222 y223 \/ fn20 y218 y219 y222 y223);

fn19 y225 y226 y228 y229 =
  ((y226 = Zero /\ y225 = y226 /\ fn14 (Succ Zero) y228 y229) \/
  (y226 = Succ Zero /\ y225 = y226 /\ fn14 (Succ Zero) y228 y229) \/
  (y226 = Succ Zero /\ y225 = Zero /\ fn14 Zero y228 y229));

fn20 y230 y231 y233 y234 =
  ((y231 = Zero /\ y230 = y231 /\ fn12 (Succ Succ Succ Zero) y233 y234) \/
  (y231 = Succ Zero /\ y230 = y231 /\ fn12 (Succ Succ Succ Zero) y233 y234) \/
  (y231 = Succ Succ Zero /\ y230 = y231 /\ fn12 (Succ Succ Succ Zero) y233 y234) \/
  (y231 = Succ Succ Succ Zero /\ y230 = y231 /\ fn12 (Succ Succ Succ Zero) y233 y234) \/
  (y231 = Succ Succ Succ Zero /\ y230 = Zero /\ fn12 Zero y233 y234) \/
  (y231 = Succ Succ Succ Zero /\ y230 = Succ Zero /\ fn12 (Succ Zero) y233 y234) \/
  (y231 = Succ Succ Succ Zero /\ y230 = Succ Succ Zero /\ fn12 (Succ Succ Zero) y233 y234));

fn21 y235 y236 y239 y240 =
  (fn22 y235 y236 y239 y240 \/ fn23 y235 y236 y239 y240);

fn22 y242 y243 y245 y246 =
  ((y243 = Zero /\ y242 = y243 /\ fn14 (Succ Succ Zero) y245 y246) \/
  (y243 = Succ Zero /\ y242 = y243 /\ fn14 (Succ Succ Zero) y245 y246) \/
  (y243 = Succ Succ Zero /\ y242 = y243 /\ fn14 (Succ Succ Zero) y245 y246) \/
  (y243 = Succ Succ Zero /\ y242 = Zero /\ fn14 Zero y245 y246) \/
  (y243 = Succ Succ Zero /\ y242 = Succ Zero /\ fn14 (Succ Zero) y245 y246));

fn23 y247 y248 y250 y251 =
  ((y248 = Zero /\ y247 = y248 /\ fn13 (Succ Succ Succ Zero) y250 y251) \/
  (y248 = Succ Zero /\ y247 = y248 /\ fn13 (Succ Succ Succ Zero) y250 y251) \/
  (y248 = Succ Succ Zero /\ y247 = y248 /\ fn13 (Succ Succ Succ Zero) y250 y251) \/
  (y248 = Succ Succ Succ Zero /\ y247 = y248 /\ fn13 (Succ Succ Succ Zero) y250 y251) \/
  (y248 = Succ Succ Succ Zero /\ y247 = Zero /\ fn13 Zero y250 y251) \/
  (y248 = Succ Succ Succ Zero /\ y247 = Succ Zero /\ fn13 (Succ Zero) y250 y251) \/
  (y248 = Succ Succ Succ Zero /\ y247 = Succ Succ Zero /\ fn13 (Succ Succ Zero) y250 y251));

fn11 y252 y253 y254 y255 y256 =
  (y253 = Zero /\ y252 = Zero /\ fn11 y254 y255 y256);

fn24 y258 y259 y260 y261 y262 =
  ((y259 = Succ Zero /\ y258 = Zero /\ fn11 y260 y261 y262) \/
  (y259 = Succ Zero /\ y258 = Succ Zero /\ fn12 y260 y261 y262) \/
  (y259 = Zero /\ y258 = y259 /\ fn12 y260 y261 y262));

fn25 y264 y265 y266 y267 y268 =
  ((y265 = Succ Succ Zero /\ y264 = Zero /\ fn11 y266 y267 y268) \/
   (y265 = Succ Succ Zero /\ y264 = Succ Zero /\ fn12 y266 y267 y268) \/
   (y265 = Succ Succ Zero /\ y264 = Succ Succ Zero /\ fn13 y266 y267 y268) \/
   (y265 = Zero /\ y264 = y265 /\ fn13 y266 y267 y268) \/
   (y265 = Succ Zero /\ y264 = y265 /\ fn13 y266 y267 y268));

fn26 y270 y271 y272 y273 y274 =
  ((y271 = Succ Succ Succ Zero /\ y270 = Zero /\ fn11 y272 y273 y274) \/
  (y271 = Succ Succ Succ Zero /\ y270 = Succ Zero /\ fn12 y272 y273 y274) \/
  (y271 = Succ Succ Succ Zero /\ y270 = Succ Succ Zero /\ fn13 y272 y273 y274) \/
  (y271 = Succ Succ Succ Zero /\ y270 = Succ Succ Succ Zero /\ fn14 y272 y273 y274) \/
  (y271 = Zero /\ y270 = y271 /\ fn14 y272 y273 y274) \/
  (y271 = Succ Zero /\ y270 = y271 /\ fn14 y272 y273 y274) \/
  (y271 = Succ Succ Zero /\ y270 = y271 /\ fn14 y272 y273 y274));

fn27 y276 y280 y281 y284 y287 y288 =
  (fn28 y276 y280 y281 y284 y287 y288 \/ fn29 y276 y280 y281 y284 y287 y288);

fn28 y290 y292 y293 y296 y299 y300 =
  fresh q1 in
    ((y290 = Zero /\ fn15 q1 y296 y299 y300 /\ fn11 y292 y293 q1));

fn29 y302 y304 y305 y308 y311 y312 =
  fresh q1, q2, q3, q4 in
    (((y302 = Zero /\ fn30 q1 y308 y311 y312 /\ fn14 y304 y305 q1)));

fn30 y314 y315 y318 y319 =
  fn31 y314 y315 y318 y319;

fn31 y321 y322 y324 y325 =
  (y322 = Zero /\ y321 = y322 /\ fn11 Zero y324 y325);

fn32 y326 y330 y331 y334 y337 y338 =
  (fn33 y326 y330 y331 y334 y337 y338 \/ fn34 y326 y330 y331 y334 y337 y338);

fn33 y340 y342 y343 y346 y349 y350 =
  fresh q1 in
    (((y340 = Zero /\ fn18 q1 y346 y349 y350 /\ fn11 y342 y343 q1) \/
    (y340 = Succ Zero /\ fn18 q1 y346 y349 y350 /\ fn12 y342 y343 q1) \/
    (y340 = Zero /\ fn15 q1 y346 y349 y350 /\ fn12 y342 y343 q1)));

fn34 y352 y354 y355 y358 y361 y362 =
  fresh q1, q2, q3, q4 in
    (((y352 = Zero /\ fn35 q1 y358 y361 y362 /\ fn14 y354 y355 q1) \/
    (y352 = Succ Zero /\ fn38 q1 y358 y361 y362 /\ fn14 y354 y355 q1)));

fn35 y364 y365 y368 y369 =
  (fn36 y364 y365 y368 y369 \/ fn37 y364 y365 y368 y369);

fn36 y371 y372 y374 y375 =
  (y372 = Zero /\ y371 = y372 /\ fn12 Zero y374 y375);

fn37 y376 y377 y379 y380 =
  ((y377 = Zero /\ y376 = y377 /\ fn11 (Succ Zero) y379 y380) \/
    (y377 = Succ Zero /\ y376 = y377 /\ fn11 (Succ Zero) y379 y380) \/
    (y377 = Succ Zero /\ y376 = Zero /\ fn11 Zero y379 y380));

fn38 y381 y382 y385 y386 =
  fn39 y381 y382 y385 y386;

fn39 y388 y389 y391 y392 =
  ((y389 = Zero /\ y388 = y389 /\ fn12 (Succ Zero) y391 y392) \/
  (y389 = Succ Zero /\ y388 = y389 /\ fn12 (Succ Zero) y391 y392) \/
  (y389 = Succ Zero /\ y388 = Zero /\ fn12 Zero y391 y392));

fn40 y393 y397 y398 y401 y404 y405 =
  (fn41 y393 y397 y398 y401 y404 y405 \/ fn42 y393 y397 y398 y401 y404 y405);

fn41 y407 y409 y410 y413 y416 y417 =
  fresh q1 in
    (((y407 = Zero /\ fn21 q1 y413 y416 y417 /\ fn11 y409 y410 q1) \/
    (y407 = Succ Zero /\ fn21 q1 y413 y416 y417 /\ fn12 y409 y410 q1) \/
    (y407 = Succ Succ Zero /\ fn21 q1 y413 y416 y417 /\ fn13 y409 y410 q1) \/
    (y407 = Zero /\ fn15 q1 y413 y416 y417 /\ fn13 y409 y410 q1) \/
    (y407 = Succ Zero /\ fn18 q1 y413 y416 y417 /\ fn13 y409 y410 q1)));

fn42 y419 y421 y422 y425 y428 y429 =
  fresh q1, q2, q3, q4 in
    (((y419 = Zero /\ fn43 q1 y425 y428 y429 /\ fn14 y421 y422 q1) \/
    (y419 = Succ Zero /\ fn46 q1 y425 y428 y429 /\ fn14 y421 y422 q1) \/
    (y419 = Succ Succ Zero /\ fn49 q1 y425 y428 y429 /\ fn14 y421 y422 q1)));

fn43 y431 y432 y435 y436 =
  (fn44 y431 y432 y435 y436 \/ fn45 y431 y432 y435 y436);

fn44 y438 y439 y441 y442 =
  (y439 = Zero /\ y438 = y439 /\ fn13 Zero y441 y442);

fn45 y443 y444 y446 y447 =
  ((y444 = Zero /\ y443 = y444 /\ fn11 (Succ Succ Zero) y446 y447) \/
    (y444 = Succ Zero /\ y443 = y444 /\ fn11 (Succ Succ Zero) y446 y447) \/
    (y444 = Succ Succ Zero /\ y443 = y444 /\ fn11 (Succ Succ Zero) y446 y447) \/
    (y444 = Succ Succ Zero /\ y443 = Zero /\ fn11 Zero y446 y447) \/
    (y444 = Succ Succ Zero /\ y443 = Succ Zero /\ fn11 (Succ Zero) y446 y447));

fn46 y448 y449 y452 y453 =
  (fn47 y448 y449 y452 y453 \/ fn48 y448 y449 y452 y453);

fn47 y455 y456 y458 y459 =
  ((y456 = Zero /\ y455 = y456 /\ fn13 (Succ Zero) y458 y459) \/
    (y456 = Succ Zero /\ y455 = y456 /\ fn13 (Succ Zero) y458 y459) \/
    (y456 = Succ Zero /\ y455 = Zero /\ fn13 Zero y458 y459));

fn48 y460 y461 y463 y464 =
  ((y461 = Zero /\ y460 = y461 /\ fn12 (Succ Succ Zero) y463 y464) \/
    (y461 = Succ Zero /\ y460 = y461 /\ fn12 (Succ Succ Zero) y463 y464) \/
    (y461 = Succ Succ Zero /\ y460 = y461 /\ fn12 (Succ Succ Zero) y463 y464) \/
    (y461 = Succ Succ Zero /\ y460 = Zero /\ fn12 Zero y463 y464) \/
    (y461 = Succ Succ Zero /\ y460 = Succ Zero /\ fn12 (Succ Zero) y463 y464));

fn49 y465 y466 y469 y470 =
  fn50 y465 y466 y469 y470;

fn50 y472 y473 y475 y476 =
  ((y473 = Zero /\ y472 = y473 /\ fn13 (Succ Succ Zero) y475 y476) \/
  (y473 = Succ Zero /\ y472 = y473 /\ fn13 (Succ Succ Zero) y475 y476) \/
  (y473 = Succ Succ Zero /\ y472 = y473 /\ fn13 (Succ Succ Zero) y475 y476) \/
  (y473 = Succ Succ Zero /\ y472 = Zero /\ fn13 Zero y475 y476) \/
  (y473 = Succ Succ Zero /\ y472 = Succ Zero /\ fn13 (Succ Zero) y475 y476));

fn51 y477 y478 y479 y480 y482 y483 y484 = (y478 = Zero /\ y477 = Zero /\ fn11 y479 y480 y482 y483 y484)
fn52 y486 y487 y488 y489 y491 y492 y493 = ((y487 = Succ Zero /\ y486 = Zero /\ fn11 y488 y489 y491 y492 y493) \/ (y487 = Succ Zero /\ y486 = Succ Zero /\ fn24 y488 y489 y491 y492 y493) \/ (y487 = Zero /\ y486 = y487 /\ fn24 y488 y489 y491 y492 y493))
fn53 y495 y496 y497 y498 y500 y501 y502 = ((y496 = Succ Succ Zero /\ y495 = Zero /\ fn11 y497 y498 y500 y501 y502) \/ (y496 = Succ Succ Zero /\ y495 = Succ Zero /\ fn24 y497 y498 y500 y501 y502) \/ (y496 = Succ Succ Zero /\ y495 = Succ Succ Zero /\ fn25 y497 y498 y500 y501 y502) \/ (y496 = Zero /\ y495 = y496 /\ fn25 y497 y498 y500 y501 y502) \/ (y496 = Succ Zero /\ y495 = y496 /\ fn25 y497 y498 y500 y501 y502))
fn54 y504 y505 y506 y507 y509 y510 y511 = ((y505 = Succ Succ Succ Zero /\ y504 = Zero /\ fn11 y506 y507 y509 y510 y511) \/ (y505 = Succ Succ Succ Zero /\ y504 = Succ Zero /\ fn24 y506 y507 y509 y510 y511) \/ (y505 = Succ Succ Succ Zero /\ y504 = Succ Succ Zero /\ fn25 y506 y507 y509 y510 y511) \/ (y505 = Succ Succ Succ Zero /\ y504 = Succ Succ Succ Zero /\ fn26 y506 y507 y509 y510 y511) \/ (y505 = Zero /\ y504 = y505 /\ fn26 y506 y507 y509 y510 y511) \/ (y505 = Succ Zero /\ y504 = y505 /\ fn26 y506 y507 y509 y510 y511) \/ (y505 = Succ Succ Zero /\ y504 = y505 /\ fn26 y506 y507 y509 y510 y511))
fn55 y513 y517 y523 y524 y527 y530 y533 y534 = (fn56 y513 y517 y523 y524 y527 y530 y533 y534 \/ fn57 y513 y517 y523 y524 y527 y530 y533 y534)
fn56 y536 y538 y544 y545 y548 y551 y554 y555 = fresh q1 q2 ((y536 = Zero /\ fn27 q1 q2 y548 y551 y554 y555 /\ fn11 y538 q1 y544 y545 q2))
fn57 y557 y559 y565 y566 y569 y572 y575 y576 = fresh q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 (((y557 = Zero /\ ____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y569 q6 q7 y572 q8 q9 y575 y576 q10 /\ fn11 y559 q1 y565 y566 q5) \/ (y557 = Succ Zero /\ ____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y569 q6 q7 y572 q8 q9 y575 y576 q10 /\ fn24 y559 q1 y565 y566 q5) \/ (y557 = Succ Succ Zero /\ ____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y569 q6 q7 y572 q8 q9 y575 y576 q10 /\ fn25 y559 q1 y565 y566 q5) \/ (y557 = Succ Succ Succ Zero /\ ____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y569 q6 q7 y572 q8 q9 y575 y576 q10 /\ fn26 y559 q1 y565 y566 q5) \/ (y557 = Zero /\ fn58 q1 q5 y569 y572 y575 y576 /\ fn26 y559 q1 y565 y566 q5) \/ (y557 = Succ Zero /\ ______minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y569 q6 q7 y572 q8 q9 y575 y576 q10 /\ fn26 y559 q1 y565 y566 q5) \/ (y557 = Succ Succ Zero /\ _______minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y569 q6 q7 y572 q8 q9 y575 y576 q10 /\ fn26 y559 q1 y565 y566 q5)))
fn58 y578 y582 y583 y586 y589 y590 = fn59 y578 y582 y583 y586 y589 y590
fn59 y592 y594 y595 y598 y601 y602 = fresh q1 ((y592 = Zero /\ fn30 q1 y598 y601 y602 /\ fn11 y594 y595 q1))
fn60 y604 y608 y614 y615 y618 y621 y624 y625 = (fn61 y604 y608 y614 y615 y618 y621 y624 y625 \/ fn62 y604 y608 y614 y615 y618 y621 y624 y625)
fn61 y627 y629 y635 y636 y639 y642 y645 y646 = fresh q1 q2 (((y627 = Zero /\ fn32 q1 q2 y639 y642 y645 y646 /\ fn11 y629 q1 y635 y636 q2) \/ (y627 = Succ Zero /\ fn32 q1 q2 y639 y642 y645 y646 /\ fn24 y629 q1 y635 y636 q2) \/ (y627 = Zero /\ fn27 q1 q2 y639 y642 y645 y646 /\ fn24 y629 q1 y635 y636 q2)))
fn62 y648 y650 y656 y657 y660 y663 y666 y667 = fresh q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 (((y648 = Zero /\ ________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y660 q6 q7 y663 q8 q9 y666 y667 q10 /\ fn11 y650 q1 y656 y657 q5) \/ (y648 = Succ Zero /\ ________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y660 q6 q7 y663 q8 q9 y666 y667 q10 /\ fn24 y650 q1 y656 y657 q5) \/ (y648 = Succ Succ Zero /\ ________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y660 q6 q7 y663 q8 q9 y666 y667 q10 /\ fn25 y650 q1 y656 y657 q5) \/ (y648 = Succ Succ Succ Zero /\ ________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y660 q6 q7 y663 q8 q9 y666 y667 q10 /\ fn26 y650 q1 y656 y657 q5) \/ (y648 = Zero /\ fn63 q1 q5 y660 y663 y666 y667 /\ fn26 y650 q1 y656 y657 q5) \/ (y648 = Succ Zero /\ fn66 q1 q5 y660 y663 y666 y667 /\ fn26 y650 q1 y656 y657 q5) \/ (y648 = Succ Succ Zero /\ ___________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y660 q6 q7 y663 q8 q9 y666 y667 q10 /\ fn26 y650 q1 y656 y657 q5)))
fn63 y669 y673 y674 y677 y680 y681 = (fn64 y669 y673 y674 y677 y680 y681 \/ fn65 y669 y673 y674 y677 y680 y681)
fn64 y683 y685 y686 y689 y692 y693 = fresh q1 ((y683 = Zero /\ fn35 q1 y689 y692 y693 /\ fn11 y685 y686 q1))
fn65 y695 y697 y698 y701 y704 y705 = fresh q1 q2 q3 q4 (((y695 = Zero /\ ______minmaxoMinmaxoMinmaxo q1 y701 q2 q3 y704 y705 q4 /\ fn11 y697 y698 q1) \/ (y695 = Succ Zero /\ ______minmaxoMinmaxoMinmaxo q1 y701 q2 q3 y704 y705 q4 /\ fn12 y697 y698 q1) \/ (y695 = Zero /\ fn30 q1 y701 y704 y705 /\ fn12 y697 y698 q1)))
fn66 y707 y711 y712 y715 y718 y719 = fn67 y707 y711 y712 y715 y718 y719
fn67 y721 y723 y724 y727 y730 y731 = fresh q1 (((y721 = Zero /\ fn38 q1 y727 y730 y731 /\ fn11 y723 y724 q1) \/ (y721 = Succ Zero /\ fn38 q1 y727 y730 y731 /\ fn12 y723 y724 q1) \/ (y721 = Zero /\ fn35 q1 y727 y730 y731 /\ fn12 y723 y724 q1)))
fn68 y733 y737 y743 y744 y747 y750 y753 y754 = (fn69 y733 y737 y743 y744 y747 y750 y753 y754 \/ fn70 y733 y737 y743 y744 y747 y750 y753 y754)
fn69 y756 y758 y764 y765 y768 y771 y774 y775 = fresh q1 q2 (((y756 = Zero /\ fn40 q1 q2 y768 y771 y774 y775 /\ fn11 y758 q1 y764 y765 q2) \/ (y756 = Succ Zero /\ fn40 q1 q2 y768 y771 y774 y775 /\ fn24 y758 q1 y764 y765 q2) \/ (y756 = Succ Succ Zero /\ fn40 q1 q2 y768 y771 y774 y775 /\ fn25 y758 q1 y764 y765 q2) \/ (y756 = Zero /\ fn27 q1 q2 y768 y771 y774 y775 /\ fn25 y758 q1 y764 y765 q2) \/ (y756 = Succ Zero /\ fn32 q1 q2 y768 y771 y774 y775 /\ fn25 y758 q1 y764 y765 q2)))
fn70 y777 y779 y785 y786 y789 y792 y795 y796 = fresh q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 (((y777 = Zero /\ ____________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y789 q6 q7 y792 q8 q9 y795 y796 q10 /\ fn11 y779 q1 y785 y786 q5) \/ (y777 = Succ Zero /\ ____________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y789 q6 q7 y792 q8 q9 y795 y796 q10 /\ fn24 y779 q1 y785 y786 q5) \/ (y777 = Succ Succ Zero /\ ____________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y789 q6 q7 y792 q8 q9 y795 y796 q10 /\ fn25 y779 q1 y785 y786 q5) \/ (y777 = Succ Succ Succ Zero /\ ____________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y789 q6 q7 y792 q8 q9 y795 y796 q10 /\ fn26 y779 q1 y785 y786 q5) \/ (y777 = Zero /\ fn71 q1 q5 y789 y792 y795 y796 /\ fn26 y779 q1 y785 y786 q5) \/ (y777 = Succ Zero /\ fn74 q1 q5 y789 y792 y795 y796 /\ fn26 y779 q1 y785 y786 q5) \/ (y777 = Succ Succ Zero /\ fn77 q1 q5 y789 y792 y795 y796 /\ fn26 y779 q1 y785 y786 q5)))
fn71 y798 y802 y803 y806 y809 y810 = (fn72 y798 y802 y803 y806 y809 y810 \/ fn73 y798 y802 y803 y806 y809 y810)
fn72 y812 y814 y815 y818 y821 y822 = fresh q1 ((y812 = Zero /\ fn43 q1 y818 y821 y822 /\ fn11 y814 y815 q1))
fn73 y824 y826 y827 y830 y833 y834 = fresh q1 q2 q3 q4 (((y824 = Zero /\ _______minmaxoMinmaxoMinmaxo q1 y830 q2 q3 y833 y834 q4 /\ fn11 y826 y827 q1) \/ (y824 = Succ Zero /\ _______minmaxoMinmaxoMinmaxo q1 y830 q2 q3 y833 y834 q4 /\ fn12 y826 y827 q1) \/ (y824 = Succ Succ Zero /\ _______minmaxoMinmaxoMinmaxo q1 y830 q2 q3 y833 y834 q4 /\ fn13 y826 y827 q1) \/ (y824 = Zero /\ fn30 q1 y830 y833 y834 /\ fn13 y826 y827 q1) \/ (y824 = Succ Zero /\ ______minmaxoMinmaxoMinmaxo q1 y830 q2 q3 y833 y834 q4 /\ fn13 y826 y827 q1)))
fn74 y836 y840 y841 y844 y847 y848 = (fn75 y836 y840 y841 y844 y847 y848 \/ fn76 y836 y840 y841 y844 y847 y848)
fn75 y850 y852 y853 y856 y859 y860 = fresh q1 (((y850 = Zero /\ fn46 q1 y856 y859 y860 /\ fn11 y852 y853 q1) \/ (y850 = Succ Zero /\ fn46 q1 y856 y859 y860 /\ fn12 y852 y853 q1) \/ (y850 = Zero /\ fn43 q1 y856 y859 y860 /\ fn12 y852 y853 q1)))
fn76 y862 y864 y865 y868 y871 y872 = fresh q1 q2 q3 q4 (((y862 = Zero /\ ___________minmaxoMinmaxoMinmaxo q1 y868 q2 q3 y871 y872 q4 /\ fn11 y864 y865 q1) \/ (y862 = Succ Zero /\ ___________minmaxoMinmaxoMinmaxo q1 y868 q2 q3 y871 y872 q4 /\ fn12 y864 y865 q1) \/ (y862 = Succ Succ Zero /\ ___________minmaxoMinmaxoMinmaxo q1 y868 q2 q3 y871 y872 q4 /\ fn13 y864 y865 q1) \/ (y862 = Zero /\ fn35 q1 y868 y871 y872 /\ fn13 y864 y865 q1) \/ (y862 = Succ Zero /\ fn38 q1 y868 y871 y872 /\ fn13 y864 y865 q1)))
fn77 y874 y878 y879 y882 y885 y886 = fn78 y874 y878 y879 y882 y885 y886
fn78 y888 y890 y891 y894 y897 y898 = fresh q1 (((y888 = Zero /\ fn49 q1 y894 y897 y898 /\ fn11 y890 y891 q1) \/ (y888 = Succ Zero /\ fn49 q1 y894 y897 y898 /\ fn12 y890 y891 q1) \/ (y888 = Succ Succ Zero /\ fn49 q1 y894 y897 y898 /\ fn13 y890 y891 q1) \/ (y888 = Zero /\ fn43 q1 y894 y897 y898 /\ fn13 y890 y891 q1) \/ (y888 = Succ Zero /\ fn46 q1 y894 y897 y898 /\ fn13 y890 y891 q1)))
fn3 y900 y901 y903 y905 y906 = ((y900 = Succ Succ Zero /\ fn52 (Succ Zero) y901 Zero y903 Zero y905 y906) \/ (y900 = Succ Zero /\ fn53 (Succ Zero) y901 Zero y903 Zero y905 y906))
fn4 y908 y912 y918 y926 y927 y930 y933 y936 y939 y940 = (fn79 y908 y912 y918 y926 y927 y930 y933 y936 y939 y940 \/ fn80 y908 y912 y918 y926 y927 y930 y933 y936 y939 y940)
fn79 y942 y944 y950 y958 y959 y962 y965 y968 y971 y972 = fresh q1 q2 q3 (((y942 = Zero /\ fn68 q1 q2 q3 y962 y965 y968 y971 y972 /\ fn51 y944 q1 y950 q2 y958 y959 q3) \/ (y942 = Succ Zero /\ fn68 q1 q2 q3 y962 y965 y968 y971 y972 /\ fn52 y944 q1 y950 q2 y958 y959 q3) \/ (y942 = Succ Succ Zero /\ fn68 q1 q2 q3 y962 y965 y968 y971 y972 /\ fn53 y944 q1 y950 q2 y958 y959 q3) \/ (y942 = Zero /\ fn55 q1 q2 q3 y962 y965 y968 y971 y972 /\ fn53 y944 q1 y950 q2 y958 y959 q3) \/ (y942 = Succ Zero /\ fn60 q1 q2 q3 y962 y965 y968 y971 y972 /\ fn53 y944 q1 y950 q2 y958 y959 q3)))
fn80 y974 y976 y982 y990 y991 y994 y997 y1000 y1003 y1004 = fresh q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12 q13 q14 q15 q16 q17 q18 (((y974 = Zero /\ ____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 y994 q12 q13 y997 q14 q15 y1000 q16 q17 y1003 y1004 q18 /\ fn51 y976 q1 y982 q5 y990 y991 q11) \/ (y974 = Succ Zero /\ ____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 y994 q12 q13 y997 q14 q15 y1000 q16 q17 y1003 y1004 q18 /\ fn52 y976 q1 y982 q5 y990 y991 q11) \/ (y974 = Succ Succ Zero /\ ____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 y994 q12 q13 y997 q14 q15 y1000 q16 q17 y1003 y1004 q18 /\ fn53 y976 q1 y982 q5 y990 y991 q11) \/ (y974 = Succ Succ Succ Zero /\ ____minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 y994 q12 q13 y997 q14 q15 y1000 q16 q17 y1003 y1004 q18 /\ fn54 y976 q1 y982 q5 y990 y991 q11) \/ (y974 = Zero /\ fn81 q1 q5 q11 y994 y997 y1000 y1003 y1004 /\ fn54 y976 q1 y982 q5 y990 y991 q11) \/ (y974 = Succ Zero /\ fn84 q1 q5 q11 y994 y997 y1000 y1003 y1004 /\ fn54 y976 q1 y982 q5 y990 y991 q11) \/ (y974 = Succ Succ Zero /\ fn87 q1 q5 q11 y994 y997 y1000 y1003 y1004 /\ fn54 y976 q1 y982 q5 y990 y991 q11)))
fn81 y1006 y1010 y1016 y1017 y1020 y1023 y1026 y1027 = (fn82 y1006 y1010 y1016 y1017 y1020 y1023 y1026 y1027 \/ fn83 y1006 y1010 y1016 y1017 y1020 y1023 y1026 y1027)
fn82 y1029 y1031 y1037 y1038 y1041 y1044 y1047 y1048 = fresh q1 q2 ((y1029 = Zero /\ fn71 q1 q2 y1041 y1044 y1047 y1048 /\ fn11 y1031 q1 y1037 y1038 q2))
fn83 y1050 y1052 y1058 y1059 y1062 y1065 y1068 y1069 = fresh q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 (((y1050 = Zero /\ _______minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y1062 q6 q7 y1065 q8 q9 y1068 y1069 q10 /\ fn11 y1052 q1 y1058 y1059 q5) \/ (y1050 = Succ Zero /\ _______minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y1062 q6 q7 y1065 q8 q9 y1068 y1069 q10 /\ fn24 y1052 q1 y1058 y1059 q5) \/ (y1050 = Succ Succ Zero /\ _______minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y1062 q6 q7 y1065 q8 q9 y1068 y1069 q10 /\ fn25 y1052 q1 y1058 y1059 q5) \/ (y1050 = Zero /\ fn58 q1 q5 y1062 y1065 y1068 y1069 /\ fn25 y1052 q1 y1058 y1059 q5) \/ (y1050 = Succ Zero /\ ______minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y1062 q6 q7 y1065 q8 q9 y1068 y1069 q10 /\ fn25 y1052 q1 y1058 y1059 q5)))
fn84 y1071 y1075 y1081 y1082 y1085 y1088 y1091 y1092 = (fn85 y1071 y1075 y1081 y1082 y1085 y1088 y1091 y1092 \/ fn86 y1071 y1075 y1081 y1082 y1085 y1088 y1091 y1092)
fn85 y1094 y1096 y1102 y1103 y1106 y1109 y1112 y1113 = fresh q1 q2 (((y1094 = Zero /\ fn74 q1 q2 y1106 y1109 y1112 y1113 /\ fn11 y1096 q1 y1102 y1103 q2) \/ (y1094 = Succ Zero /\ fn74 q1 q2 y1106 y1109 y1112 y1113 /\ fn24 y1096 q1 y1102 y1103 q2) \/ (y1094 = Zero /\ fn71 q1 q2 y1106 y1109 y1112 y1113 /\ fn24 y1096 q1 y1102 y1103 q2)))
fn86 y1115 y1117 y1123 y1124 y1127 y1130 y1133 y1134 = fresh q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 (((y1115 = Zero /\ ___________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y1127 q6 q7 y1130 q8 q9 y1133 y1134 q10 /\ fn11 y1117 q1 y1123 y1124 q5) \/ (y1115 = Succ Zero /\ ___________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y1127 q6 q7 y1130 q8 q9 y1133 y1134 q10 /\ fn24 y1117 q1 y1123 y1124 q5) \/ (y1115 = Succ Succ Zero /\ ___________minmaxoMinmaxoMinmaxoMinmaxoMinmaxoMinmaxo q1 q2 q3 q4 q5 y1127 q6 q7 y1130 q8 q9 y1133 y1134 q10 /\ fn25 y1117 q1 y1123 y1124 q5) \/ (y1115 = Zero /\ fn63 q1 q5 y1127 y1130 y1133 y1134 /\ fn25 y1117 q1 y1123 y1124 q5) \/ (y1115 = Succ Zero /\ fn66 q1 q5 y1127 y1130 y1133 y1134 /\ fn25 y1117 q1 y1123 y1124 q5)))
fn87 y1136 y1140 y1146 y1147 y1150 y1153 y1156 y1157 = fn88 y1136 y1140 y1146 y1147 y1150 y1153 y1156 y1157
fn88 y1159 y1161 y1167 y1168 y1171 y1174 y1177 y1178 = fresh q1 q2 (((y1159 = Zero /\ fn77 q1 q2 y1171 y1174 y1177 y1178 /\ fn11 y1161 q1 y1167 y1168 q2) \/ (y1159 = Succ Zero /\ fn77 q1 q2 y1171 y1174 y1177 y1178 /\ fn24 y1161 q1 y1167 y1168 q2) \/ (y1159 = Succ Succ Zero /\ fn77 q1 q2 y1171 y1174 y1177 y1178 /\ fn25 y1161 q1 y1167 y1168 q2) \/ (y1159 = Zero /\ fn71 q1 q2 y1171 y1174 y1177 y1178 /\ fn25 y1161 q1 y1167 y1168 q2) \/ (y1159 = Succ Zero /\ fn74 q1 q2 y1171 y1174 y1177 y1178 /\ fn25 y1161 q1 y1167 y1168 q2)))
fn89 y1180 y1181 y1183 y1185 y1186 = ((y1180 = Succ Succ Succ Zero /\ fn52 (Succ Zero) y1181 Zero y1183 Zero y1185 y1186) \/ (y1180 = Succ Zero /\ fn54 (Succ Zero) y1181 Zero y1183 Zero y1185 y1186))

sorto x0