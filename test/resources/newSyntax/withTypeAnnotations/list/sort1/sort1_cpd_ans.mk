sorto y0 = (fresh q1, q2 in (((y0 == (Succ (Zero) :: q1) & sortoSplito q1) | (y0 == (q2 :: q1) & _______appendoAppendoSortoSortoSplitoSortoSplito q2 q1))));

sortoSplito y1 = _________splito y1 [];

appendoSortoSortoSplitoSplito y58 = sortoSplitoSplito y58;

sortoSplitoSplito y67 = splitoSplito y67;

splitoSplito y72 = (fresh q1, q2 in ((___________splito y72 ([Succ (Zero)]) | (_________splito q1 [] & ___________splito y72 ((Succ (Zero) :: (Succ (Succ (q2)) :: q1)))))));

___splitoSplito y193 y197 = (fresh q1, q2 in ((_________splito q1 ((Succ (Succ (Zero)) :: y197)) & ___________splito y193 ((Succ (Zero) :: (Succ (Succ (q2)) :: q1))))));

_______splito y233 y234 = (fresh q1, q2, q3, q4 in (((y234 == [] & y233 == []) | (y234 == (q1 :: q2) & le q1 & y233 == (q1 :: q3) & _______splito q3 q2) | (y233 == (Succ (Succ (Succ (q4))) :: q3) & _______splito q3 y234))));

le y235 = (fresh q1 in ((y235 == Zero | (y235 == Succ (q1) & _le q1))));

_le y236 = (y236 == Zero | y236 == Succ (Zero));

_________splito y272 y273 = (fresh q1, q2, q3, q4 in (((y273 == [] & y272 == []) | (y273 == (q1 :: q2) & _le q1 & y272 == (q1 :: q3) & _________splito q3 q2) | (y272 == (Succ (Succ (q4)) :: q3) & _________splito q3 y273))));

______splitoSplito y320 y324 = (fresh q1, q2 in ((_______splitoSplito y320 y324 | (_______splito q1 ((Zero :: y324)) & _________splito y320 ((Succ (Succ (Zero)) :: (Succ (Succ (Succ (q2))) :: q1)))))));

_______splitoSplito y325 y329 = (fresh q1, q2, q3, q4 in (((y329 == [] & _________splito y325 ((Succ (Succ (Zero)) :: [Zero]))) | (y329 == (q1 :: q2) & leSplito y325 q1 q3 & _______splito q3 q2) | (_______splito q3 y329 & _________splito y325 ((Succ (Succ (Zero)) :: (Zero :: (Succ (Succ (Succ (q4))) :: q3))))))));

leSplito y330 y332 y333 = (fresh q1 in (((y332 == Zero & _________splito y330 ((Succ (Succ (Zero)) :: (Zero :: (Zero :: y333))))) | (y332 == Succ (q1) & _leSplito y330 y333 q1))));

_leSplito y334 y336 y337 = ((y337 == Zero & _________splito y334 ((Succ (Succ (Zero)) :: (Zero :: (Succ (Zero) :: y336))))) | (y337 == Succ (Zero) & _________splito y334 ((Succ (Succ (Zero)) :: (Zero :: (Succ (Succ (Zero)) :: y336))))));

___________splito y373 y374 = (fresh q1, q2, q3 in (((y374 == [] & y373 == []) | (y374 == (Zero :: q1) & y373 == (Zero :: q2) & ___________splito q2 q1) | (y373 == (Succ (q3) :: q2) & ___________splito q2 y374))));

_________splitoSplito y434 y438 = (fresh q1, q2 in ((__________splitoSplito y434 y438 | (_______splito q1 ((Zero :: y438)) & ___________splito y434 ((Succ (Succ (Zero)) :: (Succ (Succ (Succ (q2))) :: q1)))))));

__________splitoSplito y439 y443 = (fresh q1, q2, q3, q4 in (((y443 == [] & ___________splito y439 ((Succ (Succ (Zero)) :: [Zero]))) | (y443 == (q1 :: q2) & __leSplito y439 q1 q3 & _______splito q3 q2) | (_______splito q3 y443 & ___________splito y439 ((Succ (Succ (Zero)) :: (Zero :: (Succ (Succ (Succ (q4))) :: q3))))))));

__leSplito y444 y446 y447 = (fresh q1 in (((y446 == Zero & ___________splito y444 ((Succ (Succ (Zero)) :: (Zero :: (Zero :: y447))))) | (y446 == Succ (q1) & ___leSplito y444 y447 q1))));

___leSplito y448 y450 y451 = ((y451 == Zero & ___________splito y448 ((Succ (Succ (Zero)) :: (Zero :: (Succ (Zero) :: y450))))) | (y451 == Succ (Zero) & ___________splito y448 ((Succ (Succ (Zero)) :: (Zero :: (Succ (Succ (Zero)) :: y450))))));

___________splitoSplito y491 y495 = (fresh q1, q2 in ((____________splitoSplito y491 y495 | (_________splito q1 ((Zero :: y495)) & ___________splito y491 ((Succ (Zero) :: (Succ (Succ (q2)) :: q1)))))));

____________splitoSplito y496 y500 = (fresh q1, q2, q3, q4 in (((y500 == [] & ___________splito y496 ((Succ (Zero) :: [Zero]))) | (y500 == (q1 :: q2) & ____leSplito y496 q1 q3 & _________splito q3 q2) | (_________splito q3 y500 & ___________splito y496 ((Succ (Zero) :: (Zero :: (Succ (Succ (q4)) :: q3))))))));

____leSplito y501 y503 y504 = ((y503 == Zero & ___________splito y501 ((Succ (Zero) :: (Zero :: (Zero :: y504))))) | (y503 == Succ (Zero) & ___________splito y501 ((Succ (Zero) :: (Zero :: (Succ (Zero) :: y504))))));

____splitoSplitoSplito y552 y558 = (fresh q1, q2 in ((_____splitoSplitoSplito y552 y558 | (___splitoSplito y552 ((Succ (Succ (Succ (q1))) :: q2)) & _______splito q2 ((Zero :: y558))))));

_____splitoSplitoSplito y559 y565 = (fresh q1, q2, q3, q4 in (((y565 == [] & ___splitoSplito y559 [Zero]) | (y565 == (q1 :: q2) & leSplitoSplito y559 q1 q3 & _______splito q3 q2) | (___splitoSplito y559 ((Zero :: (Succ (Succ (Succ (q4))) :: q3))) & _______splito q3 y565))));

leSplitoSplito y566 y570 y571 = (fresh q1 in (((y570 == Zero & ___splitoSplito y566 ((Zero :: (Zero :: y571)))) | (y570 == Succ (q1) & _leSplitoSplito y566 y571 q1))));

_leSplitoSplito y572 y576 y577 = ((y577 == Zero & ___splitoSplito y572 ((Zero :: (Succ (Zero) :: y576)))) | (y577 == Succ (Zero) & ___splitoSplito y572 ((Zero :: (Succ (Succ (Zero)) :: y576)))));

_______appendoAppendoSortoSortoSplitoSortoSplito y578 y579 = ((y578 == Zero & appendoSortoSortoSplitoSplito y579) | ________appendoAppendoSortoSortoSplitoSortoSplito y578 y579);

________appendoAppendoSortoSortoSplitoSortoSplito y591 y592 = ((y591 == Succ (Succ (Zero)) & ______appendoSortoSortoSplitoSplito y592) | _________appendoAppendoSortoSortoSplitoSortoSplito y591 y592);

______appendoSortoSortoSplitoSplito y604 = (______sortoSplitoSplito y604 | ____appendoAppendoSortoSortoSplitoSortoSplitoSplito y604);

______sortoSplitoSplito y613 = _____________splitoSplito y613;

_____________splitoSplito y618 = (fresh q1, q2 in ((_______splito y618 ([Succ (Zero)]) | (_________splito q1 [] & _______splito y618 ((Succ (Zero) :: (Succ (Succ (q2)) :: q1)))))));

____appendoAppendoSortoSortoSplitoSortoSplitoSplito y622 = ____appendoSortoSortoSplitoSplitoSplito y622;

____appendoSortoSortoSplitoSplitoSplito y637 = ____sortoSplitoSplitoSplito y637;

____sortoSplitoSplitoSplito y648 = ______splitoSplitoSplito y648;

______splitoSplitoSplito y655 = (fresh q1, q2 in ((______________splitoSplito y655 [] | (______________splitoSplito y655 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

______________splitoSplito y661 y665 = (fresh q1, q2 in ((___________splito q1 ((Succ (Zero) :: y665)) & _______splito y661 ((Zero :: (Succ (q2) :: q1))))));

_________appendoAppendoSortoSortoSplitoSortoSplito y666 y667 = ((y666 == Succ (Zero) & _______appendoSortoSortoSplitoSplito y667) | ___________appendoAppendoSortoSortoSplitoSortoSplito y666 y667);

_______appendoSortoSortoSplitoSplito y679 = (_______sortoSplitoSplito y679 | _____appendoAppendoSortoSortoSplitoSortoSplitoSplito y679);

_______sortoSplitoSplito y688 = _______________splitoSplito y688;

_______________splitoSplito y693 = (fresh q1, q2 in ((_________splito y693 ([Succ (Zero)]) | (_________splito q1 [] & _________splito y693 ((Succ (Zero) :: (Succ (Succ (q2)) :: q1)))))));

_____appendoAppendoSortoSortoSplitoSortoSplitoSplito y717 = (_____appendoSortoSortoSplitoSplitoSplito y717 | ______appendoAppendoSortoSortoSplitoSortoSplitoSplito y717);

_____appendoSortoSortoSplitoSplitoSplito y732 = _____sortoSplitoSplitoSplito y732;

_____sortoSplitoSplitoSplito y743 = _______splitoSplitoSplito y743;

_______splitoSplitoSplito y750 = (fresh q1, q2 in ((________________splitoSplito y750 [] | (________________splitoSplito y750 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

________________splitoSplito y756 y760 = (fresh q1, q2 in ((___________splito q1 ((Succ (Zero) :: y760)) & _________splito y756 ((Zero :: (Succ (q2) :: q1))))));

______appendoAppendoSortoSortoSplitoSortoSplitoSplito y761 = ______appendoSortoSortoSplitoSplitoSplito y761;

______appendoSortoSortoSplitoSplitoSplito y776 = (______sortoSplitoSplitoSplito y776 | _appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplito y776);

______sortoSplitoSplitoSplito y787 = ________splitoSplitoSplito y787;

________splitoSplitoSplito y794 = (fresh q1, q2 in ((_________________splitoSplito y794 [] | (_________________splitoSplito y794 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

_________________splitoSplito y800 y804 = (fresh q1, q2 in ((__________________splitoSplito y800 y804 | (_______splito q1 ((Succ (Zero) :: y804)) & _________splito y800 ((Succ (Succ (Zero)) :: (Succ (Succ (Succ (q2))) :: q1)))))));

__________________splitoSplito y805 y809 = (fresh q1, q2, q3, q4 in (((y809 == [] & _________splito y805 ((Succ (Succ (Zero)) :: [Succ (Zero)]))) | (y809 == (q1 :: q2) & _____leSplito y805 q1 q3 & _______splito q3 q2) | (_______splito q3 y809 & _________splito y805 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Succ (Succ (Succ (q4))) :: q3))))))));

_____leSplito y810 y812 y813 = (fresh q1 in (((y812 == Zero & _________splito y810 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: y813))))) | (y812 == Succ (q1) & ______leSplito y810 y813 q1))));

______leSplito y814 y816 y817 = ((y817 == Zero & _________splito y814 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Succ (Zero) :: y816))))) | (y817 == Succ (Zero) & _________splito y814 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Succ (Succ (Zero)) :: y816))))));

_appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplito y818 = _appendoSortoSortoSplitoSplitoSplitoSplito y818;

_appendoSortoSortoSplitoSplitoSplitoSplito y835 = _sortoSplitoSplitoSplitoSplito y835;

_sortoSplitoSplitoSplitoSplito y848 = _splitoSplitoSplitoSplito y848;

_splitoSplitoSplitoSplito y857 = (fresh q1, q2 in ((_________splitoSplitoSplito y857 [] | (_________splitoSplitoSplito y857 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

_________splitoSplitoSplito y865 y871 = (fresh q1, q2 in ((______splitoSplito y865 ((Succ (q1) :: q2)) & ___________splito q2 ((Succ (Zero) :: y871)))));

___________appendoAppendoSortoSortoSplitoSortoSplito y872 y873 = (y872 == Zero & ________appendoSortoSortoSplitoSplito y873);

________appendoSortoSortoSplitoSplito y885 = (sortoSplitoSplito y885 | _______appendoAppendoSortoSortoSplitoSortoSplitoSplito y885);

_______appendoAppendoSortoSortoSplitoSortoSplitoSplito y927 = (_______appendoSortoSortoSplitoSplitoSplito y927 | ________appendoAppendoSortoSortoSplitoSortoSplitoSplito y927);

_______appendoSortoSortoSplitoSplitoSplito y942 = _______sortoSplitoSplitoSplito y942;

_______sortoSplitoSplitoSplito y953 = __________splitoSplitoSplito y953;

__________splitoSplitoSplito y960 = (fresh q1, q2 in ((___________________splitoSplito y960 [] | (___________________splitoSplito y960 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

___________________splitoSplito y966 y970 = (fresh q1, q2 in ((___________splito q1 ((Succ (Zero) :: y970)) & ___________splito y966 ((Zero :: (Succ (q2) :: q1))))));

________appendoAppendoSortoSortoSplitoSortoSplitoSplito y971 = (________appendoSortoSortoSplitoSplitoSplito y971 | _________appendoAppendoSortoSortoSplitoSortoSplitoSplito y971);

________appendoSortoSortoSplitoSplitoSplito y986 = (________sortoSplitoSplitoSplito y986 | __appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplito y986);

________sortoSplitoSplitoSplito y997 = ___________splitoSplitoSplito y997;

___________splitoSplitoSplito y1004 = (fresh q1, q2 in ((____________________splitoSplito y1004 [] | (____________________splitoSplito y1004 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

____________________splitoSplito y1010 y1014 = (fresh q1, q2 in ((_____________________splitoSplito y1010 y1014 | (_______splito q1 ((Succ (Zero) :: y1014)) & ___________splito y1010 ((Succ (Succ (Zero)) :: (Succ (Succ (Succ (q2))) :: q1)))))));

_____________________splitoSplito y1015 y1019 = (fresh q1, q2, q3, q4 in (((y1019 == [] & ___________splito y1015 ((Succ (Succ (Zero)) :: [Succ (Zero)]))) | (y1019 == (q1 :: q2) & _______leSplito y1015 q1 q3 & _______splito q3 q2) | (_______splito q3 y1019 & ___________splito y1015 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Succ (Succ (Succ (q4))) :: q3))))))));

_______leSplito y1020 y1022 y1023 = (fresh q1 in (((y1022 == Zero & ___________splito y1020 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Zero :: y1023))))) | (y1022 == Succ (q1) & ________leSplito y1020 y1023 q1))));

________leSplito y1024 y1026 y1027 = ((y1027 == Zero & ___________splito y1024 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Succ (Zero) :: y1026))))) | (y1027 == Succ (Zero) & ___________splito y1024 ((Succ (Succ (Zero)) :: (Succ (Zero) :: (Succ (Succ (Zero)) :: y1026))))));

__appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplito y1028 = __appendoSortoSortoSplitoSplitoSplitoSplito y1028;

__appendoSortoSortoSplitoSplitoSplitoSplito y1045 = __sortoSplitoSplitoSplitoSplito y1045;

__sortoSplitoSplitoSplitoSplito y1058 = __splitoSplitoSplitoSplito y1058;

__splitoSplitoSplitoSplito y1067 = (fresh q1, q2 in ((____________splitoSplitoSplito y1067 [] | (____________splitoSplitoSplito y1067 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

____________splitoSplitoSplito y1075 y1081 = (fresh q1, q2 in ((_________splitoSplito y1075 ((Succ (q1) :: q2)) & ___________splito q2 ((Succ (Zero) :: y1081)))));

_________appendoAppendoSortoSortoSplitoSortoSplitoSplito y1082 = _________appendoSortoSortoSplitoSplitoSplito y1082;

_________appendoSortoSortoSplitoSplitoSplito y1097 = (_________sortoSplitoSplitoSplito y1097 | ___appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplito y1097);

_________sortoSplitoSplitoSplito y1108 = _____________splitoSplitoSplito y1108;

_____________splitoSplitoSplito y1115 = (fresh q1, q2 in ((______________________splitoSplito y1115 [] | (______________________splitoSplito y1115 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

______________________splitoSplito y1121 y1125 = (fresh q1, q2 in ((_______________________splitoSplito y1121 y1125 | (_________splito q1 ((Succ (Zero) :: y1125)) & ___________splito y1121 ((Succ (Zero) :: (Succ (Succ (q2)) :: q1)))))));

_______________________splitoSplito y1126 y1130 = (fresh q1, q2, q3, q4 in (((y1130 == [] & ___________splito y1126 ((Succ (Zero) :: [Succ (Zero)]))) | (y1130 == (q1 :: q2) & _________leSplito y1126 q1 q3 & _________splito q3 q2) | (_________splito q3 y1130 & ___________splito y1126 ((Succ (Zero) :: (Succ (Zero) :: (Succ (Succ (q4)) :: q3))))))));

_________leSplito y1131 y1133 y1134 = ((y1133 == Zero & ___________splito y1131 ((Succ (Zero) :: (Succ (Zero) :: (Zero :: y1134))))) | (y1133 == Succ (Zero) & ___________splito y1131 ((Succ (Zero) :: (Succ (Zero) :: (Succ (Zero) :: y1134))))));

___appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplito y1135 = (___appendoSortoSortoSplitoSplitoSplitoSplito y1135 | ____appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplito y1135);

___appendoSortoSortoSplitoSplitoSplitoSplito y1152 = ___sortoSplitoSplitoSplitoSplito y1152;

___sortoSplitoSplitoSplitoSplito y1165 = ___splitoSplitoSplitoSplito y1165;

___splitoSplitoSplitoSplito y1174 = (fresh q1, q2 in ((______________splitoSplitoSplito y1174 [] | (______________splitoSplitoSplito y1174 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

______________splitoSplitoSplito y1182 y1188 = (fresh q1, q2 in ((___________splitoSplito y1182 ((Succ (q1) :: q2)) & ___________splito q2 ((Succ (Zero) :: y1188)))));

____appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplito y1189 = ____appendoSortoSortoSplitoSplitoSplitoSplito y1189;

____appendoSortoSortoSplitoSplitoSplitoSplito y1206 = (____sortoSplitoSplitoSplitoSplito y1206 | appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplitoSplito y1206);

____sortoSplitoSplitoSplitoSplito y1219 = ____splitoSplitoSplitoSplito y1219;

____splitoSplitoSplitoSplito y1228 = (fresh q1, q2 in ((_______________splitoSplitoSplito y1228 [] | (_______________splitoSplitoSplito y1228 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

_______________splitoSplitoSplito y1236 y1242 = (fresh q1, q2 in ((________________splitoSplitoSplito y1236 y1242 | (___splitoSplito y1236 ((Succ (Succ (Succ (q1))) :: q2)) & _______splito q2 ((Succ (Zero) :: y1242))))));

________________splitoSplitoSplito y1243 y1249 = (fresh q1, q2, q3, q4 in (((y1249 == [] & ___splitoSplito y1243 ([Succ (Zero)])) | (y1249 == (q1 :: q2) & __leSplitoSplito y1243 q1 q3 & _______splito q3 q2) | (___splitoSplito y1243 ((Succ (Zero) :: (Succ (Succ (Succ (q4))) :: q3))) & _______splito q3 y1249))));

__leSplitoSplito y1250 y1254 y1255 = (fresh q1 in (((y1254 == Zero & ___splitoSplito y1250 ((Succ (Zero) :: (Zero :: y1255)))) | (y1254 == Succ (q1) & ___leSplitoSplito y1250 y1255 q1))));

___leSplitoSplito y1256 y1260 y1261 = ((y1261 == Zero & ___splitoSplito y1256 ((Succ (Zero) :: (Succ (Zero) :: y1260)))) | (y1261 == Succ (Zero) & ___splitoSplito y1256 ((Succ (Zero) :: (Succ (Succ (Zero)) :: y1260)))));

appendoAppendoSortoSortoSplitoSortoSplitoSplitoSplitoSplito y1262 = appendoSortoSortoSplitoSplitoSplitoSplitoSplito y1262;

appendoSortoSortoSplitoSplitoSplitoSplitoSplito y1281 = sortoSplitoSplitoSplitoSplitoSplito y1281;

sortoSplitoSplitoSplitoSplitoSplito y1296 = splitoSplitoSplitoSplitoSplito y1296;

splitoSplitoSplitoSplitoSplito y1307 = (fresh q1, q2 in ((_____splitoSplitoSplitoSplito y1307 [] | (_____splitoSplitoSplitoSplito y1307 ((Succ (Succ (q1)) :: q2)) & _________splito q2 []))));

_____splitoSplitoSplitoSplito y1317 y1325 = (fresh q1, q2 in ((____splitoSplitoSplito y1317 ((Succ (q1) :: q2)) & ___________splito q2 ((Succ (Zero) :: y1325)))));


? sorto x0