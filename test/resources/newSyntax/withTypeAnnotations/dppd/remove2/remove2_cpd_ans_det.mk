rr y0 = (fresh q1, q2, q3 in ((y0 == (q1 :: (q2 :: q3)) & gG q1 q2 q3)));

gG y1 y5 y6 = (fresh q1, q2 in (((y6 == (q1 :: q2) & y1 == y5 & hG y5 q1 q2) | _neqHG y1 y5 y6)));

hG y9 y10 y11 = (fresh q1, q2 in (((y11 == [] & _______g y9 y10 y10 [] []) | (y11 == (q1 :: q2) & _gG y9 y10 q1 q2))));

_gG y14 y15 y16 y17 = ((y15 == y16 & fG y14 y17 y16) | neqHG y14 y15 y16 y17);

fG y18 y19 y20 = (fresh q1, q2 in (((y19 == [] & _______g y18 y20 y20 [] []) | (y19 == (q1 :: q2) & __hG y18 q2 q1 y20 y20))));

f y38 y39 = (fresh q1, q2 in (((y39 == [] & y38 == []) | (y38 == (q1 :: q2) & h q1 q2 y39))));

_neqG y46 = (fresh q1 in (_____g Zero ((Zero :: (Succ (Succ (Zero)) :: (Zero :: [Zero])))) q1 y46));

h y49 y50 y51 = (fresh q1, q2 in (((y51 == [y49] & y50 == []) | (y50 == (q1 :: q2) & _____g y49 y51 q1 q2))));

_____g y52 y53 y54 y55 = (fresh q1 in (((y53 == (y54 :: q1) & y52 == y54 & f y55 q1) | (y53 == (y52 :: q1) & neq y52 y54 & h y54 y55 q1))));

neq y56 y57 = (fresh q1, q2, q3 in (((y57 == Succ (q1) & y56 == Zero) | (y57 == Zero & y56 == Succ (q1)) | (y57 == Succ (q2) & y56 == Succ (q3) & neq q3 q2))));

neqHG y58 y59 y61 y62 = (fresh q1, q2, q3 in (((y61 == Succ (q1) & y59 == Zero & __hG y58 y62 (Succ (q1)) Zero Zero) | (y61 == Zero & y59 == Succ (q1) & __hG y58 y62 Zero (Succ (q1)) (Succ (q1))) | (y61 == Succ (q2) & __hG y58 y62 (Succ (q2)) (Succ (q3)) (Succ (q3)) & y59 == Succ (q3) & neq q3 q2))));

__hG y63 y65 y66 y67 y68 = (fresh q1, q2 in (((y65 == [] & _______g y63 y67 y68 [y66] [y66]) | (y65 == (q1 :: q2) & ___gG y63 y66 y67 y68 q1 q2))));

___gG y73 y75 y76 y77 y78 y79 = (fresh q1 in (((y75 == y78 & _fG y73 y76 y77 y79 y78) | (neq y75 y78 & h y78 y79 q1 & _______g y73 y76 y77 ((y75 :: q1)) ((y75 :: q1))))));

_fG y80 y81 y82 y83 y84 = (fresh q1, q2, q3 in (((y83 == [] & _______g y80 y81 y82 [y84] [y84]) | (y83 == (q1 :: q2) & h q1 q2 q3 & _______g y80 y81 y82 ((y84 :: q3)) ((y84 :: q3))))));

_______g y86 y87 y88 y89 y90 = (fresh q1, q2 in (((y87 == y86 & y86 == Succ (Zero) & f y90 ((Zero :: (Succ (Succ (Zero)) :: (Zero :: [Zero]))))) | (y89 == (q1 :: q2) & neq (Succ (Zero)) y87 & y86 == Succ (Zero) & _____g y88 ((Zero :: (Succ (Succ (Zero)) :: (Zero :: [Zero])))) q1 q2))));

_neqHG y93 y94 y95 = (fresh q1, q2, q3 in (((y94 == Succ (q1) & y93 == Zero & hG Zero (Succ (q1)) y95) | (y94 == Zero & y93 == Succ (q1) & hG (Succ (q1)) Zero y95) | (y94 == Succ (q2) & hG (Succ (q3)) (Succ (q2)) y95 & y93 == Succ (q3) & neq q3 q2))));


? rr x0