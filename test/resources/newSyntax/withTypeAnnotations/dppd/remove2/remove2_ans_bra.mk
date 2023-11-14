rr y0 = (fresh q1, q2, q3 in ((y0 == (q1 :: (q2 :: q3)) & gG q1 q2 q3)));

gG y1 y5 y6 = (fresh q1, q2 in (((y6 == (q1 :: q2) & y1 == y5 & hG y5 q1 q2) | _neqHG y1 y5 y6)));

hG y9 y10 y11 = (fresh q1, q2 in (((y11 == [] & ______g y9 y10 y10 [] []) | (y11 == (q1 :: q2) & _gG y9 y10 q1 q2))));

_gG y14 y15 y16 y17 = ((y15 == y16 & fG y14 y17 y16) | neqHG y14 y15 y16 y17);

fG y18 y19 y20 = (fresh q1, q2 in (((y19 == [] & ______g y18 y20 y20 [] []) | (y19 == (q1 :: q2) & __hG y18 q2 q1 y20 y20))));

f y36 y37 = (fresh q1, q2 in (((y37 == [] & y36 == []) | (y36 == (q1 :: q2) & h q1 q2 y37))));

h y42 y43 y44 = (fresh q1, q2 in (((y44 == [y42] & y43 == []) | (y43 == (q1 :: q2) & ____g y42 y44 q1 q2))));

____g y45 y46 y47 y48 = (fresh q1 in (((y46 == (y47 :: q1) & y45 == y47 & f y48 q1) | (y46 == (y45 :: q1) & neq y45 y47 & h y47 y48 q1))));

neq y49 y50 = (fresh q1, q2, q3 in (((y50 == Succ (q1) & y49 == Zero) | (y50 == Zero & y49 == Succ (q1)) | (y50 == Succ (q2) & y49 == Succ (q3) & neq q3 q2))));

neqHG y51 y52 y54 y55 = (fresh q1, q2, q3 in (((y54 == Succ (q1) & y52 == Zero & __hG y51 y55 (Succ (q1)) Zero Zero) | (y54 == Zero & y52 == Succ (q1) & __hG y51 y55 Zero (Succ (q1)) (Succ (q1))) | (y54 == Succ (q2) & __hG y51 y55 (Succ (q2)) (Succ (q3)) (Succ (q3)) & y52 == Succ (q3) & neq q3 q2))));

__hG y56 y58 y59 y60 y61 = (fresh q1, q2 in (((y58 == [] & ______g y56 y60 y61 [y59] [y59]) | (y58 == (q1 :: q2) & ___gG y56 y59 y60 y61 q1 q2))));

___gG y66 y68 y69 y70 y71 y72 = (fresh q1 in (((y68 == y71 & _fG y66 y69 y70 y72 y71) | (neq y68 y71 & h y71 y72 q1 & ______g y66 y69 y70 ((y68 :: q1)) ((y68 :: q1))))));

_fG y73 y74 y75 y76 y77 = (fresh q1, q2, q3 in (((y76 == [] & ______g y73 y74 y75 [y77] [y77]) | (y76 == (q1 :: q2) & h q1 q2 q3 & ______g y73 y74 y75 ((y77 :: q3)) ((y77 :: q3))))));

______g y79 y80 y81 y82 y83 = (fresh q1 in (((y80 == y79 & y79 == Succ (Zero) & f y83 ((Zero :: (Succ (Succ (Zero)) :: (Zero :: [Zero]))))) | (y80 == Zero & y79 == Succ (Zero) & f ((y81 :: y82)) ((Zero :: (Succ (Succ (Zero)) :: (Zero :: [Zero]))))) | (y80 == Succ (Succ (q1)) & y79 == Succ (Zero) & f ((y81 :: y82)) ((Zero :: (Succ (Succ (Zero)) :: (Zero :: [Zero]))))))));

_neqHG y86 y87 y88 = (fresh q1, q2, q3 in (((y87 == Succ (q1) & y86 == Zero & hG Zero (Succ (q1)) y88) | (y87 == Zero & y86 == Succ (q1) & hG (Succ (q1)) Zero y88) | (y87 == Succ (q2) & hG (Succ (q3)) (Succ (q2)) y88 & y86 == Succ (q3) & neq q3 q2))));


? rr x0