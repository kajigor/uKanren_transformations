check y0 = (fresh q1, q2 in ((y0 == (q1 :: q2) & one_stepCheck q1 q2)));

one_stepCheck y1 y2 = (fresh q1 in ((y1 == (One, q1) & notEqStickGetSetCheck y2 q1)));

notEqStickGetSetCheck y4 y6 = ((y6 == Two & _check y4) | (y6 == Thr & __________________________check y4));

_check y7 = (fresh q1, q2 in ((y7 == (q1 :: q2) & _one_stepCheck q1 q2)));

_one_stepCheck y8 y9 = (fresh q1, q2 in (((y8 == (q1, Thr) & _notEqStickGetSetCheck y9 q1) | (y8 == (q1, q2) & ______________________notEqStickGetGetLessSetSetCheck y9 q1 q2))));

_notEqStickGetSetCheck y11 y12 = ((y12 == One & __check y11) | (y12 == Two & __________________________check y11));

__check y18 = (fresh q1, q2 in ((y18 == (q1 :: q2) & __one_stepCheck q1 q2)));

__one_stepCheck y19 y20 = (fresh q1, q2 in ((y19 == (q1, q2) & notEqStickGetGetLessSetSetCheck y20 q1 q2)));

notEqStickGetGetLessSetSetCheck y22 y24 y25 = ((y25 == One & y24 == Two & ___check y22) | (y25 == Thr & y24 == Two & _________________________check y22) | (y25 == One & y24 == Thr & _check y22));

___check y31 = (fresh q1, q2 in ((y31 == (q1 :: q2) & ___one_stepCheck q1 q2)));

___one_stepCheck y32 y33 = (fresh q1, q2 in (((y32 == (q1, Two) & __notEqStickGetSetCheck y33 q1) | (y32 == (q1, q2) & ____________________notEqStickGetGetLessSetSetCheck y33 q1 q2))));

__notEqStickGetSetCheck y35 y36 = ((y36 == One & __check y35) | (y36 == Thr & ____check y35));

____check y42 = (fresh q1, q2 in ((y42 == (q1 :: q2) & ____one_stepCheck q1 q2)));

____one_stepCheck y43 y44 = (fresh q1, q2 in (((y43 == (q1, Thr) & ___notEqStickGetSetCheck y44 q1) | (y43 == (q1, q2) & ___________________notEqStickGetGetLessSetSetCheck y44 q1 q2))));

___notEqStickGetSetCheck y46 y47 = ((y47 == One & _____check y46) | (y47 == Two & ___check y46));

_____check y53 = (fresh q1, q2 in ((y53 == (q1 :: q2) & _____one_stepCheck q1 q2)));

_____one_stepCheck y54 y55 = (fresh q1, q2 in ((y54 == (q1, q2) & _notEqStickGetGetLessSetSetCheck y55 q1 q2)));

_notEqStickGetGetLessSetSetCheck y57 y59 y60 = ((y60 == One & y59 == Two & __________________________check y57) | (y60 == One & y59 == Thr & ____check y57) | (y60 == Two & y59 == Thr & ______check y57));

______check y66 = (fresh q1, q2 in ((y66 == (q1 :: q2) & ______one_stepCheck q1 q2)));

______one_stepCheck y67 y68 = (fresh q1, q2 in (((y67 == (q1, Thr) & ____notEqStickGetSetCheck y68 q1) | (y67 == (q1, q2) & __________________notEqStickGetGetLessSetSetCheck y68 q1 q2))));

____notEqStickGetSetCheck y70 y71 = ((y71 == One & _______check y70) | (y71 == Two & _____check y70));

_______check y77 = (fresh q1, q2 in ((y77 == (q1 :: q2) & _______one_stepCheck q1 q2)));

_______one_stepCheck y78 y79 = (fresh q1, q2 in (((y78 == (q1, One) & _____notEqStickGetSetCheck y79 q1) | (y78 == (q1, q2) & _________________notEqStickGetGetLessSetSetCheck y79 q1 q2))));

_____notEqStickGetSetCheck y81 y82 = ((y82 == Two & ________check y81) | (y82 == Thr & ______check y81));

________check y88 = (fresh q1, q2 in ((y88 == (q1 :: q2) & ________one_stepCheck q1 q2)));

________one_stepCheck y89 y90 = (fresh q1, q2 in ((y89 == (q1, q2) & __notEqStickGetGetLessSetSetCheck y90 q1 q2)));

__notEqStickGetGetLessSetSetCheck y92 y94 y95 = ((y95 == Two & y94 == One & _______check y92) | (y95 == Thr & y94 == One & _________check y92) | (y95 == Thr & y94 == Two & ________________________check y92));

_________check y101 = (fresh q1, q2 in ((y101 == (q1 :: q2) & _________one_stepCheck q1 q2)));

_________one_stepCheck y102 y103 = (fresh q1, q2 in (((y102 == (q1, One) & ______notEqStickGetSetCheck y103 q1) | (y102 == (q1, q2) & _______________notEqStickGetGetLessSetSetCheck y103 q1 q2))));

______notEqStickGetSetCheck y105 y106 = ((y106 == Two & __________check y105) | (y106 == Thr & ________check y105));

__________check y112 = (fresh q1, q2 in ((y112 == (q1 :: q2) & __________one_stepCheck q1 q2)));

__________one_stepCheck y113 y114 = (fresh q1, q2 in (((y113 == (q1, Two) & _______notEqStickGetSetCheck y114 q1) | (y113 == (q1, q2) & ______________notEqStickGetGetLessSetSetCheck y114 q1 q2))));

_______notEqStickGetSetCheck y116 y117 = ((y117 == One & _________check y116) | (y117 == Thr & ___________check y116));

___________check y123 = (fresh q1, q2 in ((y123 == (q1 :: q2) & ___________one_stepCheck q1 q2)));

___________one_stepCheck y124 y125 = (fresh q1, q2 in ((y124 == (q1, q2) & ___notEqStickGetGetLessSetSetCheck y125 q1 q2)));

___notEqStickGetGetLessSetSetCheck y127 y129 y130 = ((y130 == Thr & y129 == One & ____________check y127) | (y130 == One & y129 == Two & ______________check y127) | (y130 == Thr & y129 == Two & __________check y127));

____________check y136 = (fresh q1, q2 in ((y136 == (q1 :: q2) & ____________one_stepCheck q1 q2)));

____________one_stepCheck y137 y138 = (fresh q1, q2 in (((y137 == (q1, One) & ________notEqStickGetSetCheck y138 q1) | (y137 == (q1, q2) & ____notEqStickGetGetLessSetSetCheck y138 q1 q2))));

________notEqStickGetSetCheck y140 y141 = ((y141 == Two & ________________________check y140) | (y141 == Thr & ___________check y140));

____notEqStickGetGetLessSetSetCheck y147 y149 y150 = (y150 == Thr & y149 == Two & _____________check y147);

_____________check y156 = (fresh q1, q2 in ((y156 == [] | (y156 == (q1 :: q2) & _____________one_stepCheck q1 q2))));

_____________one_stepCheck y157 y158 = (fresh q1 in ((y157 == (Thr, q1) & _________notEqStickGetSetCheck y158 q1)));

_________notEqStickGetSetCheck y160 y162 = ((y162 == One & ________________________check y160) | (y162 == Two & ____________check y160));

______________check y163 = (fresh q1, q2 in ((y163 == (q1 :: q2) & ______________one_stepCheck q1 q2)));

______________one_stepCheck y164 y165 = (fresh q1, q2 in (((y164 == (q1, Two) & __________notEqStickGetSetCheck y165 q1) | (y164 == (q1, q2) & _____________notEqStickGetGetLessSetSetCheck y165 q1 q2))));

__________notEqStickGetSetCheck y167 y168 = ((y168 == One & ___________check y167) | (y168 == Thr & _______________check y167));

_______________check y174 = (fresh q1, q2 in ((y174 == (q1 :: q2) & _______________one_stepCheck q1 q2)));

_______________one_stepCheck y175 y176 = (fresh q1, q2 in (((y175 == (q1, Thr) & ___________notEqStickGetSetCheck y176 q1) | (y175 == (q1, q2) & ____________notEqStickGetGetLessSetSetCheck y176 q1 q2))));

___________notEqStickGetSetCheck y178 y179 = ((y179 == One & ________________check y178) | (y179 == Two & ______________check y178));

________________check y185 = (fresh q1, q2 in ((y185 == (q1 :: q2) & ________________one_stepCheck q1 q2)));

________________one_stepCheck y186 y187 = (fresh q1, q2 in ((y186 == (q1, q2) & _____notEqStickGetGetLessSetSetCheck y187 q1 q2)));

_____notEqStickGetGetLessSetSetCheck y189 y191 y192 = ((y192 == Two & y191 == One & _________________check y189) | (y192 == One & y191 == Thr & _______________check y189) | (y192 == Two & y191 == Thr & _______________________check y189));

_________________check y198 = (fresh q1, q2 in ((y198 == (q1 :: q2) & _________________one_stepCheck q1 q2)));

_________________one_stepCheck y199 y200 = (fresh q1, q2 in (((y199 == (q1, One) & ____________notEqStickGetSetCheck y200 q1) | (y199 == (q1, q2) & __________notEqStickGetGetLessSetSetCheck y200 q1 q2))));

____________notEqStickGetSetCheck y202 y203 = ((y203 == Two & ________________check y202) | (y203 == Thr & __________________check y202));

__________________check y209 = (fresh q1, q2 in ((y209 == (q1 :: q2) & __________________one_stepCheck q1 q2)));

__________________one_stepCheck y210 y211 = (fresh q1, q2 in (((y210 == (q1, Thr) & _____________notEqStickGetSetCheck y211 q1) | (y210 == (q1, q2) & _________notEqStickGetGetLessSetSetCheck y211 q1 q2))));

_____________notEqStickGetSetCheck y213 y214 = ((y214 == One & _________________check y213) | (y214 == Two & ___________________check y213));

___________________check y220 = (fresh q1, q2 in ((y220 == (q1 :: q2) & ___________________one_stepCheck q1 q2)));

___________________one_stepCheck y221 y222 = (fresh q1, q2 in ((y221 == (q1, q2) & ______notEqStickGetGetLessSetSetCheck y222 q1 q2)));

______notEqStickGetGetLessSetSetCheck y224 y226 y227 = ((y227 == Two & y226 == One & ____________________check y224) | (y227 == Thr & y226 == One & _____________________check y224) | (y227 == Two & y226 == Thr & __________________check y224));

____________________check y233 = (fresh q1, q2 in ((y233 == (q1 :: q2) & ____________________one_stepCheck q1 q2)));

____________________one_stepCheck y234 y235 = (fresh q1, q2 in (((y234 == (q1, One) & ______________notEqStickGetSetCheck y235 q1) | (y234 == (q1, q2) & _______notEqStickGetGetLessSetSetCheck y235 q1 q2))));

______________notEqStickGetSetCheck y237 y238 = ((y238 == Two & ___________________check y237) | (y238 == Thr & _______________________check y237));

_______notEqStickGetGetLessSetSetCheck y244 y246 y247 = (y247 == Thr & y246 == Two & _____________________check y244);

_____________________check y253 = (fresh q1, q2 in ((y253 == (q1 :: q2) & _____________________one_stepCheck q1 q2)));

_____________________one_stepCheck y254 y255 = (fresh q1, q2 in (((y254 == (q1, One) & _______________notEqStickGetSetCheck y255 q1) | (y254 == (q1, q2) & ________notEqStickGetGetLessSetSetCheck y255 q1 q2))));

_______________notEqStickGetSetCheck y257 y258 = ((y258 == Two & _________________________check y257) | (y258 == Thr & ___________________check y257));

________notEqStickGetGetLessSetSetCheck y264 y266 y267 = (y267 == Two & y266 == Thr & ____________________check y264);

_________notEqStickGetGetLessSetSetCheck y273 y275 y276 = (y276 == Two & y275 == One & ______________________check y273);

______________________check y282 = (fresh q1, q2 in ((y282 == (q1 :: q2) & ______________________one_stepCheck q1 q2)));

______________________one_stepCheck y283 y284 = (fresh q1 in ((y283 == (Two, q1) & ________________notEqStickGetSetCheck y284 q1)));

________________notEqStickGetSetCheck y286 y288 = ((y288 == One & __________________check y286) | (y288 == Thr & _________________check y286));

__________notEqStickGetGetLessSetSetCheck y289 y291 y292 = (y292 == Two & y291 == Thr & ______________________check y289);

_______________________check y298 = (fresh q1, q2 in ((y298 == (q1 :: q2) & _______________________one_stepCheck q1 q2)));

_______________________one_stepCheck y299 y300 = (fresh q1, q2 in (((y299 == (q1, Thr) & _________________notEqStickGetSetCheck y300 q1) | (y299 == (q1, q2) & ___________notEqStickGetGetLessSetSetCheck y300 q1 q2))));

_________________notEqStickGetSetCheck y302 y303 = ((y303 == One & ____________________check y302) | (y303 == Two & ________________check y302));

___________notEqStickGetGetLessSetSetCheck y309 y311 y312 = (y312 == One & y311 == Two & _______________check y309);

____________notEqStickGetGetLessSetSetCheck y318 y320 y321 = (y321 == Two & y320 == One & _______________________check y318);

_____________notEqStickGetGetLessSetSetCheck y327 y329 y330 = (y330 == Thr & y329 == One & __________check y327);

______________notEqStickGetGetLessSetSetCheck y336 y338 y339 = (y339 == One & y338 == Thr & ______________check y336);

_______________notEqStickGetGetLessSetSetCheck y345 y347 y348 = (y348 == Two & y347 == Thr & _______check y345);

________________________check y354 = (fresh q1, q2 in ((y354 == (q1 :: q2) & ________________________one_stepCheck q1 q2)));

________________________one_stepCheck y355 y356 = (fresh q1, q2 in (((y355 == (q1, Two) & __________________notEqStickGetSetCheck y356 q1) | (y355 == (q1, q2) & ________________notEqStickGetGetLessSetSetCheck y356 q1 q2))));

__________________notEqStickGetSetCheck y358 y359 = ((y359 == One & ____________check y358) | (y359 == Thr & ________check y358));

________________notEqStickGetGetLessSetSetCheck y365 y367 y368 = (y368 == Thr & y367 == One & _____________check y365);

_________________notEqStickGetGetLessSetSetCheck y374 y376 y377 = (y377 == Thr & y376 == Two & _________check y374);

__________________notEqStickGetGetLessSetSetCheck y383 y385 y386 = (y386 == One & y385 == Two & ____check y383);

___________________notEqStickGetGetLessSetSetCheck y392 y394 y395 = (y395 == Two & y394 == One & ______check y392);

____________________notEqStickGetGetLessSetSetCheck y401 y403 y404 = (y404 == Thr & y403 == One & _________________________check y401);

_________________________check y410 = (fresh q1, q2 in ((y410 == (q1 :: q2) & _________________________one_stepCheck q1 q2)));

_________________________one_stepCheck y411 y412 = (fresh q1, q2 in (((y411 == (q1, Two) & ___________________notEqStickGetSetCheck y412 q1) | (y411 == (q1, q2) & _____________________notEqStickGetGetLessSetSetCheck y412 q1 q2))));

___________________notEqStickGetSetCheck y414 y415 = ((y415 == One & _____________________check y414) | (y415 == Thr & __check y414));

_____________________notEqStickGetGetLessSetSetCheck y421 y423 y424 = (y424 == One & y423 == Thr & ___check y421);

______________________notEqStickGetGetLessSetSetCheck y430 y432 y433 = (y433 == One & y432 == Two & check y430);

__________________________check y439 = (fresh q1, q2 in ((y439 == (q1 :: q2) & __________________________one_stepCheck q1 q2)));

__________________________one_stepCheck y440 y441 = (fresh q1, q2 in (((y440 == (q1, Two) & ____________________notEqStickGetSetCheck y441 q1) | (y440 == (q1, q2) & _______________________notEqStickGetGetLessSetSetCheck y441 q1 q2))));

____________________notEqStickGetSetCheck y443 y444 = ((y444 == One & _____check y443) | (y444 == Thr & _check y443));

_______________________notEqStickGetGetLessSetSetCheck y450 y452 y453 = (y453 == One & y452 == Thr & check y450);


? check x0