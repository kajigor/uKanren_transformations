sorto y0 = (fresh q1, q2, q3, q4 in (((y0 == (Zero :: q1) & appendoSortoSortoSplito q2 q3 & sortoSplito q1 q2 q3) | (y0 == (q4 :: q1) & ___appendoAppendoSortoSortoSplitoSortoSplito q4 q1))));

sortoSplito y1 y3 y4 = (fresh q1, q2 in ((y3 == Succ (q1) & y1 == (Succ (q1) :: q2) & splito q2 y4)));

splito y5 y6 = (fresh q1, q2, q3 in (((y6 == [] & y5 == []) | (y6 == (Succ (q1) :: q2) & y5 == (Succ (q1) :: q3) & splito q3 q2))));

appendoSortoSortoSplito y7 y8 = (fresh q1, q2 in (((y7 == Succ (Zero) & _appendoSortoSortoSplito q1 q2 & _____sortoSplito y8 ((q1 :: q2))) | _appendoAppendoSortoSortoSplitoSortoSplito y7 y8)));

_appendoSortoSortoSplito y19 y20 = (fresh q1, q2 in (((y19 == Succ (Succ (Zero)) & __appendoSortoSortoSplito q1 q2 & ____sortoSplito y20 ((q1 :: q2))) | appendoAppendoSortoSortoSplitoSortoSplito y19 y20)));

__appendoSortoSortoSplito y31 y32 = (fresh q1 in ((y31 == Succ (Succ (Succ (Zero))) & ___sortoSplito y32 q1 & _sorto q1)));

___sortoSplito y37 y39 = _____splito y39 y37 [];

_sorto y42 = y42 == [];

appendoAppendoSortoSortoSplitoSortoSplito y43 y44 = (fresh q1 in ((y43 == Succ (Succ (Succ (Zero))) & appendoSortoSortoSplitoSplito y44 q1 & _sorto q1)));

appendoSortoSortoSplitoSplito y54 y55 = (fresh q1 in ((sortoSplitoSplito y54 y55 q1 & _sorto q1)));

sortoSplitoSplito y62 y63 y66 = splitoSplito y62 y63 y66;

splitoSplito y67 y68 y70 = (fresh q1, q2, q3 in (((y70 == [] & _____splito y68 y67 ([Succ (Succ (Zero))])) | (y70 == (Succ (Succ (Succ (q1))) :: q2) & ________splito [] q3 q2 & _____splito y68 y67 ((Succ (Succ (Zero)) :: (Succ (Succ (Succ (q1))) :: q3)))))));

_____splito y74 y75 y76 = (fresh q1, q2, q3, q4, q5 in (((y76 == [] & y75 == [] & y74 == []) | (y76 == (q1 :: q2) & le q1 & y75 == (q1 :: q3) & _____splito y74 q3 q2) | (y75 == (Succ (Succ (Succ (Succ (q4)))) :: q3) & y74 == (Succ (Succ (Succ (Succ (q4)))) :: q5) & _____splito q5 q3 y76))));

le y77 = (fresh q1 in ((y77 == Zero | (y77 == Succ (q1) & _le q1))));

_le y78 = (fresh q1 in ((y78 == Zero | (y78 == Succ (q1) & __le q1))));

__le y79 = (y79 == Zero | y79 == Succ (Zero));

_appendoAppendoSortoSortoSplitoSortoSplito y80 y81 = (fresh q1, q2 in (((y80 == Succ (Succ (Zero)) & ______appendoSortoSortoSplitoSplito y81 ((q1 :: q2)) & __appendoSortoSortoSplito q1 q2) | __appendoAppendoSortoSortoSplitoSortoSplito y80 y81)));

________splito y119 y120 y121 = (fresh q1, q2, q3, q4, q5 in (((y121 == [] & y120 == [] & y119 == []) | (y120 == (q1 :: q2) & _le q1 & y119 == (q1 :: q3) & ________splito q3 q2 y121) | (y121 == (Succ (Succ (Succ (q4))) :: q5) & y120 == (Succ (Succ (Succ (q4))) :: q2) & ________splito y119 q2 q5))));

__appendoAppendoSortoSortoSplitoSortoSplito y122 y123 = (fresh q1 in ((y122 == Succ (Succ (Succ (Zero))) & __appendoSortoSortoSplitoSplito y123 q1 & _sorto q1)));

__appendoSortoSortoSplitoSplito y133 y134 = (fresh q1, q2 in (((___appendoSortoSortoSplito q1 q2 & __sortoSplitoSplito y133 y134 q1 q2) | appendoAppendoSortoSortoSplitoSortoSplitoSplito y133 y134)));

__sortoSplitoSplito y141 y142 y145 y146 = (fresh q1 in ((y145 == Succ (Succ (q1)) & __splitoSplito y141 y142 y146 q1)));

__splitoSplito y147 y148 y150 y151 = (fresh q1, q2, q3 in (((y150 == [] & _____splito y148 y147 ((Succ (Zero) :: [Succ (Succ (y151))]))) | (y150 == (Succ (Succ (q1)) :: q2) & ___________splito [] q3 q2 & _____splito y148 y147 ((Succ (Zero) :: (Succ (Succ (y151)) :: (Succ (Succ (q1)) :: q3))))))));

___appendoSortoSortoSplito y152 y153 = (fresh q1 in ((y152 == Succ (Succ (Zero)) & ____sortoSplito y153 q1 & _sorto q1)));

____sortoSplito y158 y160 = ________splito [] y158 y160;

appendoAppendoSortoSortoSplitoSortoSplitoSplito y161 y162 = (fresh q1 in ((appendoSortoSortoSplitoSplitoSplito y161 y162 q1 & _sorto q1)));

appendoSortoSortoSplitoSplitoSplito y174 y175 y177 = (fresh q1 in ((sortoSplitoSplitoSplito y174 y175 y177 q1 & _sorto q1)));

sortoSplitoSplitoSplito y184 y185 y187 y190 = splitoSplitoSplito y184 y185 y187 y190;

splitoSplitoSplito y191 y192 y194 y196 = (fresh q1, q2, q3 in (((y196 == [] & ___splitoSplito y191 y192 y194 []) | (y196 == (Succ (Succ (q1)) :: q2) & ___splitoSplito y191 y192 y194 ((Succ (Succ (q1)) :: q3)) & ___________splito [] q3 q2))));

___splitoSplito y197 y198 y200 y201 = (fresh q1, q2, q3 in ((____splitoSplito y197 y198 y200 y201 | (y200 == (Succ (Succ (Succ (q1))) :: q2) & ________splito ((Succ (Zero) :: y201)) q3 q2 & _____splito y198 y197 ((Succ (Succ (Zero)) :: (Succ (Succ (Succ (q1))) :: q3)))))));

____splitoSplito y202 y203 y204 y206 = (fresh q1, q2, q3, q4, q5 in (((y206 == [] & y204 == [] & _____splito y203 y202 ((Succ (Succ (Zero)) :: [Succ (Zero)]))) | (y206 == (q1 :: q2) & leSplito y202 y203 q1 q3 & ________splito q2 q3 y204) | (y204 == (Succ (Succ (Succ (q4))) :: q5) & ________splito y206 q3 q5 & _____splito y203 y202 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Succ (Succ (Succ (q4))) :: q3))))))));

leSplito y207 y208 y209 y210 = (fresh q1 in (((y209 == Zero & _____splito y208 y207 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: y210))))) | (y209 == Succ (q1) & _leSplito y207 y208 y210 q1))));

_leSplito y211 y212 y213 y214 = ((y214 == Zero & _____splito y212 y211 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Succ (Zero) :: y213))))) | (y214 == Succ (Zero) & _____splito y212 y211 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Succ (Succ (Zero)) :: y213))))));

___appendoAppendoSortoSortoSplitoSortoSplito y215 y216 = (fresh q1, q2 in (((y215 == Succ (Zero) & ___appendoSortoSortoSplitoSplito y216 q1 q2 & _appendoSortoSortoSplito q1 q2) | ____appendoAppendoSortoSortoSplitoSortoSplito y215 y216)));

___appendoSortoSortoSplitoSplito y226 y233 y234 = (fresh q1 in ((___sortoSplitoSplito y226 q1 y233 y234 & _sorto q1)));

___sortoSplitoSplito y235 y238 y239 y240 = _____splitoSplito y235 y238 y239 y240;

_____splitoSplito y241 y243 y244 y245 = (fresh q1, q2, q3 in (((y243 == [] & ___________splito [Zero] y241 ((y244 :: y245))) | (y243 == (Succ (q1) :: q2) & splito q3 q2 & ___________splito ((Zero :: (Succ (q1) :: q3))) y241 ((y244 :: y245))))));

___________splito y254 y255 y256 = (fresh q1, q2, q3, q4, q5 in (((y256 == [] & y255 == [] & y254 == []) | (y255 == (q1 :: q2) & __le q1 & y254 == (q1 :: q3) & ___________splito q3 q2 y256) | (y256 == (Succ (Succ (q4)) :: q5) & y255 == (Succ (Succ (q4)) :: q2) & ___________splito y254 q2 q5))));

____appendoAppendoSortoSortoSplitoSortoSplito y257 y258 = (fresh q1, q2 in (((y257 == Succ (Succ (Zero)) & ____appendoSortoSortoSplitoSplito y258 q1 q2 & __appendoSortoSortoSplito q1 q2) | _____appendoAppendoSortoSortoSplitoSortoSplito y257 y258)));

____appendoSortoSortoSplitoSplito y268 y275 y276 = (fresh q1, q2 in (((____appendoSortoSortoSplito q1 q2 & ____sortoSplitoSplito y268 y275 y276 q1 q2) | _appendoAppendoSortoSortoSplitoSortoSplitoSplito y268 y275 y276)));

____sortoSplitoSplito y277 y280 y281 y282 y283 = (fresh q1 in ((y282 == Succ (q1) & ______splitoSplito y277 y280 y281 y283 q1)));

______splitoSplito y284 y285 y286 y288 y289 = (fresh q1, q2, q3 in (((y288 == [] & ________splito ((Zero :: [Succ (y289)])) y284 ((y285 :: y286))) | (y288 == (Succ (q1) :: q2) & splito q3 q2 & ________splito ((Zero :: (Succ (y289) :: (Succ (q1) :: q3)))) y284 ((y285 :: y286))))));

____appendoSortoSortoSplito y290 y291 = (fresh q1 in ((y290 == Succ (Zero) & _____sortoSplito y291 q1 & _sorto q1)));

_____sortoSplito y296 y298 = ___________splito [] y296 y298;

_appendoAppendoSortoSortoSplitoSortoSplitoSplito y299 y304 y305 = (fresh q1 in ((_appendoSortoSortoSplitoSplitoSplito y299 q1 y304 y305 & _sorto q1)));

_appendoSortoSortoSplitoSplitoSplito y313 y315 y316 y317 = (fresh q1 in ((_sortoSplitoSplitoSplito y313 y315 y316 y317 q1 & _sorto q1)));

_sortoSplitoSplitoSplito y324 y326 y327 y328 y331 = _splitoSplitoSplito y324 y326 y327 y328 y331;

_splitoSplitoSplito y332 y334 y335 y336 y338 = (fresh q1, q2, q3 in (((y338 == [] & _______splitoSplito y332 y334 y335 y336 []) | (y338 == (Succ (q1) :: q2) & _______splitoSplito y332 y334 y335 y336 ((Succ (q1) :: q3)) & splito q3 q2))));

_______splitoSplito y339 y341 y342 y343 y344 = (fresh q1, q2, q3 in ((________splitoSplito y339 y341 y342 y343 y344 | (y341 == (Succ (Succ (q1)) :: q2) & ___________splito ((Zero :: y344)) q3 q2 & ________splito ((Succ (Zero) :: (Succ (Succ (q1)) :: q3))) y339 ((y342 :: y343))))));

________splitoSplito y345 y346 y347 y348 y350 = (fresh q1, q2, q3, q4, q5 in (((y350 == [] & y346 == [] & ________splito ((Succ (Zero) :: [Zero])) y345 ((y347 :: y348))) | (y350 == (q1 :: q2) & __leSplito y345 y347 y348 q1 q3 & ___________splito q2 q3 y346) | (y346 == (Succ (Succ (q4)) :: q5) & ___________splito y350 q3 q5 & ________splito ((Succ (Zero) :: (Zero :: (Succ (Succ (q4)) :: q3)))) y345 ((y347 :: y348))))));

__leSplito y351 y352 y353 y354 y355 = ((y354 == Zero & ________splito ((Succ (Zero) :: (Zero :: (Zero :: y355)))) y351 ((y352 :: y353))) | (y354 == Succ (Zero) & ________splito ((Succ (Zero) :: (Zero :: (Succ (Zero) :: y355)))) y351 ((y352 :: y353))));

_____appendoAppendoSortoSortoSplitoSortoSplito y356 y357 = (fresh q1 in ((y356 == Succ (Succ (Succ (Zero))) & _____appendoSortoSortoSplitoSplito y357 q1 & _sorto q1)));

_____appendoSortoSortoSplitoSplito y367 y368 = (fresh q1, q2 in (((_____appendoSortoSortoSplito q1 q2 & _____sortoSplitoSplito y367 y368 q1 q2) | __appendoAppendoSortoSortoSplitoSortoSplitoSplito y367 y368)));

_____sortoSplitoSplito y375 y376 y379 y380 = (fresh q1 in ((y379 == Succ (q1) & _________splitoSplito y375 y376 y380 q1)));

_________splitoSplito y381 y382 y384 y385 = (fresh q1, q2, q3 in (((y384 == [] & _____splito y382 y381 ((Zero :: [Succ (y385)]))) | (y384 == (Succ (q1) :: q2) & splito q3 q2 & _____splito y382 y381 ((Zero :: (Succ (y385) :: (Succ (q1) :: q3))))))));

_____appendoSortoSortoSplito y386 y387 = (fresh q1, q2 in (((y386 == Succ (Zero) & ___appendoSortoSortoSplito q1 q2 & _____sortoSplito y387 ((q1 :: q2))) | ______appendoAppendoSortoSortoSplitoSortoSplito y386 y387)));

______appendoAppendoSortoSortoSplitoSortoSplito y392 y393 = (fresh q1 in ((y392 == Succ (Succ (Zero)) & ______appendoSortoSortoSplitoSplito y393 q1 & _sorto q1)));

______appendoSortoSortoSplitoSplito y403 y404 = (fresh q1 in ((______sortoSplitoSplito y403 y404 q1 & _sorto q1)));

______sortoSplitoSplito y411 y412 y415 = __________splitoSplito y411 y412 y415;

__________splitoSplito y416 y417 y419 = (fresh q1, q2, q3 in (((y419 == [] & ________splito ([Succ (Zero)]) y416 y417) | (y419 == (Succ (Succ (q1)) :: q2) & ___________splito [] q3 q2 & ________splito ((Succ (Zero) :: (Succ (Succ (q1)) :: q3))) y416 y417))));

__appendoAppendoSortoSortoSplitoSortoSplitoSplito y420 y421 = (fresh q1, q2 in (((__appendoSortoSortoSplitoSplitoSplito y420 y421 q1 q2 & ___appendoSortoSortoSplito q1 q2) | ___appendoAppendoSortoSortoSplitoSortoSplitoSplito y420 y421)));

__appendoSortoSortoSplitoSplitoSplito y433 y434 y442 y443 = (fresh q1 in ((__sortoSplitoSplitoSplito y433 y434 q1 y442 y443 & _sorto q1)));

__sortoSplitoSplitoSplito y444 y445 y449 y450 y451 = __splitoSplitoSplito y444 y445 y449 y450 y451;

__splitoSplitoSplito y452 y453 y456 y457 y458 = (fresh q1, q2, q3 in (((y456 == [] & ___________splitoSplito y452 y453 y457 y458 []) | (y456 == (Succ (q1) :: q2) & ___________splitoSplito y452 y453 y457 y458 ((Succ (q1) :: q3)) & splito q3 q2))));

___________splitoSplito y459 y460 y462 y463 y464 = (fresh q1 in ((____________splitoSplito y459 y460 y462 y463 y464 | (y462 == Succ (Succ (q1)) & ______________splitoSplito y459 y460 y464 y463 q1))));

____________splitoSplito y465 y466 y467 y468 y470 = (fresh q1, q2, q3, q4 in (((y470 == (q1 :: q2) & ___leSplito y465 y466 q1 q3 & ___________splito q2 q3 ((y467 :: y468))) | (y467 == Succ (Succ (q4)) & _____________splitoSplito y465 y466 y470 y468 q4))));

___leSplito y471 y472 y473 y474 = ((y473 == Zero & _____splito y472 y471 ((Succ (Zero) :: (Zero :: (Zero :: y474))))) | (y473 == Succ (Zero) & _____splito y472 y471 ((Succ (Zero) :: (Zero :: (Succ (Zero) :: y474))))));

_____________splitoSplito y475 y476 y477 y479 y480 = (fresh q1, q2, q3, q4, q5 in (((y479 == [] & y477 == [] & _____splito y476 y475 ((Succ (Zero) :: (Zero :: [Succ (Succ (y480))])))) | (y477 == (q1 :: q2) & ____leSplito y475 y476 y480 q1 q3 & ___________splito q2 q3 y479) | (y479 == (Succ (Succ (q4)) :: q5) & ___________splito y477 q3 q5 & _____splito y476 y475 ((Succ (Zero) :: (Zero :: (Succ (Succ (y480)) :: (Succ (Succ (q4)) :: q3)))))))));

____leSplito y481 y482 y483 y484 y485 = ((y484 == Zero & _____splito y482 y481 ((Succ (Zero) :: (Zero :: (Succ (Succ (y483)) :: (Zero :: y485)))))) | (y484 == Succ (Zero) & _____splito y482 y481 ((Succ (Zero) :: (Zero :: (Succ (Succ (y483)) :: (Succ (Zero) :: y485)))))));

______________splitoSplito y486 y487 y488 y490 y491 = (fresh q1, q2, q3 in ((_______________splitoSplito y486 y487 y490 y491 y488 | (y490 == (Succ (Succ (q1)) :: q2) & ___________splito ((Zero :: y488)) q3 q2 & _____splito y487 y486 ((Succ (Zero) :: (Succ (Succ (y491)) :: (Succ (Succ (q1)) :: q3))))))));

_______________splitoSplito y492 y493 y494 y495 y497 = (fresh q1, q2, q3, q4, q5 in (((y497 == [] & y494 == [] & _____splito y493 y492 ((Succ (Zero) :: (Succ (Succ (y495)) :: [Zero])))) | (y497 == (q1 :: q2) & _____leSplito y492 y493 y495 q1 q3 & ___________splito q2 q3 y494) | (y494 == (Succ (Succ (q4)) :: q5) & ___________splito y497 q3 q5 & _____splito y493 y492 ((Succ (Zero) :: (Succ (Succ (y495)) :: (Zero :: (Succ (Succ (q4)) :: q3)))))))));

_____leSplito y498 y499 y500 y501 y502 = ((y501 == Zero & _____splito y499 y498 ((Succ (Zero) :: (Succ (Succ (y500)) :: (Zero :: (Zero :: y502)))))) | (y501 == Succ (Zero) & _____splito y499 y498 ((Succ (Zero) :: (Succ (Succ (y500)) :: (Zero :: (Succ (Zero) :: y502)))))));

___appendoAppendoSortoSortoSplitoSortoSplitoSplito y503 y504 = (fresh q1 in ((___appendoSortoSortoSplitoSplitoSplito y503 y504 q1 & _sorto q1)));

___appendoSortoSortoSplitoSplitoSplito y516 y517 y519 = (fresh q1, q2 in (((____appendoSortoSortoSplito q1 q2 & ___sortoSplitoSplitoSplito y516 y517 y519 q1 q2) | appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplito y516 y517 y519)));

___sortoSplitoSplitoSplito y526 y527 y529 y532 y533 = (fresh q1 in ((y532 == Succ (q1) & ___splitoSplitoSplito y526 y527 y529 y533 q1)));

___splitoSplitoSplito y534 y535 y537 y539 y540 = (fresh q1, q2, q3 in (((y539 == [] & ________________splitoSplito y534 y535 y537 y540 []) | (y539 == (Succ (q1) :: q2) & ________________splitoSplito y534 y535 y537 y540 ((Succ (q1) :: q3)) & splito q3 q2))));

________________splitoSplito y541 y542 y544 y545 y546 = (fresh q1, q2, q3 in ((_________________splitoSplito y541 y542 y544 y545 y546 | (y544 == (Succ (Succ (Succ (q1))) :: q2) & ________splito ((Zero :: (Succ (y545) :: y546))) q3 q2 & _____splito y542 y541 ((Succ (Succ (Zero)) :: (Succ (Succ (Succ (q1))) :: q3)))))));

_________________splitoSplito y547 y548 y549 y550 y551 = (fresh q1, q2, q3 in (((______leSplito y547 y548 q1 y550 & ________splito y551 q1 y549) | (y549 == (Succ (Succ (Succ (q2))) :: q3) & ________splito ((Succ (y550) :: y551)) q1 q3 & _____splito y548 y547 ((Succ (Succ (Zero)) :: (Zero :: (Succ (Succ (Succ (q2))) :: q1))))))));

______leSplito y553 y554 y555 y556 = ((y556 == Zero & _____splito y554 y553 ((Succ (Succ (Zero)) :: (Zero :: (Succ (Zero) :: y555))))) | (y556 == Succ (Zero) & _____splito y554 y553 ((Succ (Succ (Zero)) :: (Zero :: (Succ (Succ (Zero)) :: y555))))));

appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplito y557 y558 y560 = (fresh q1 in ((appendoSortoSortoSplitoSplitoSplitoSplito y557 y558 y560 q1 & _sorto q1)));

appendoSortoSortoSplitoSplitoSplitoSplito y572 y573 y575 y577 = (fresh q1 in ((sortoSplitoSplitoSplitoSplito y572 y573 y575 y577 q1 & _sorto q1)));

sortoSplitoSplitoSplitoSplito y584 y585 y587 y589 y592 = splitoSplitoSplitoSplito y584 y585 y587 y589 y592;

splitoSplitoSplitoSplito y593 y594 y596 y598 y600 = (fresh q1, q2, q3 in (((y600 == [] & ____splitoSplitoSplito y593 y594 y596 y598 []) | (y600 == (Succ (q1) :: q2) & ____splitoSplitoSplito y593 y594 y596 y598 ((Succ (q1) :: q3)) & splito q3 q2))));

____splitoSplitoSplito y601 y602 y604 y606 y607 = (fresh q1, q2, q3 in ((_____splitoSplitoSplito y601 y602 y604 y606 y607 | (y606 == (Succ (Succ (q1)) :: q2) & ___splitoSplito y601 y602 y604 ((Succ (Succ (q1)) :: q3)) & ___________splito ((Zero :: y607)) q3 q2))));

_____splitoSplitoSplito y608 y609 y611 y612 y614 = (fresh q1, q2, q3, q4, q5 in (((y614 == [] & y612 == [] & ___splitoSplito y608 y609 y611 [Zero]) | (y614 == (q1 :: q2) & leSplitoSplito y608 y609 y611 q1 q3 & ___________splito q2 q3 y612) | (y612 == (Succ (Succ (q4)) :: q5) & ___splitoSplito y608 y609 y611 ((Zero :: (Succ (Succ (q4)) :: q3))) & ___________splito y614 q3 q5))));

leSplitoSplito y615 y616 y618 y619 y620 = ((y619 == Zero & ___splitoSplito y615 y616 y618 ((Zero :: (Zero :: y620)))) | (y619 == Succ (Zero) & ___splitoSplito y615 y616 y618 ((Zero :: (Succ (Zero) :: y620)))));


? sorto x0