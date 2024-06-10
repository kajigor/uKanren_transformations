rr y0 = (fresh q1, q2, q3 in ((y0 == (q1 :: (q2 :: q3)) & gG q1 q2 q3)));

gG y1 y5 y6 = (fresh q1, q2 in (((y6 == (q1 :: q2) & y1 == y5 & hG y5 q1 q2) | _neqHG y1 y5 y6)));

hG y9 y10 y11 = (fresh q1, q2 in (((y11 == [] & _______g y9 y10 y10 [] []) | (y11 == (q1 :: q2) & _gG y9 y10 q1 q2))));

_gG y14 y15 y16 y17 = ((y15 == y16 & fG y14 y17 y16) | neqHG y14 y15 y16 y17);

fG y18 y19 y20 = (fresh q1, q2 in (((y19 == [] & _______g y18 y20 y20 [] []) | (y19 == (q1 :: q2) & __hG y18 q2 q1 y20 y20))));

f y39 y40 = (fresh q1, q2 in (((y40 == [] & y39 == []) | (y39 == (q1 :: q2) & h q1 q2 y40))));

_neqG y47 = (fresh q1 in (_____g Zero ((Zero :: [Succ (Succ (Zero))])) q1 y47));

h y50 y51 y52 = (fresh q1, q2 in (((y52 == [y50] & y51 == []) | (y51 == (q1 :: q2) & _____g y50 y52 q1 q2))));

_____g y53 y54 y55 y56 = (fresh q1 in (((y54 == (y55 :: q1) & y53 == y55 & f y56 q1) | (y54 == (y53 :: q1) & neq y53 y55 & h y55 y56 q1))));

neq y57 y58 = (fresh q1, q2, q3 in (((y58 == Succ (q1) & y57 == Zero) | (y58 == Zero & y57 == Succ (q1)) | (y58 == Succ (q2) & y57 == Succ (q3) & neq q3 q2))));

neqHG y59 y60 y62 y63 = (fresh q1, q2, q3 in (((y62 == Succ (q1) & y60 == Zero & __hG y59 y63 (Succ (q1)) Zero Zero) | (y62 == Zero & y60 == Succ (q1) & __hG y59 y63 Zero (Succ (q1)) (Succ (q1))) | (y62 == Succ (q2) & __hG y59 y63 (Succ (q2)) (Succ (q3)) (Succ (q3)) & y60 == Succ (q3) & neq q3 q2))));

__hG y64 y66 y67 y68 y69 = (fresh q1, q2 in (((y66 == [] & _______g y64 y68 y69 [y67] [y67]) | (y66 == (q1 :: q2) & ___gG y64 y67 y68 y69 q1 q2))));

___gG y74 y76 y77 y78 y79 y80 = (fresh q1 in (((y76 == y79 & _fG y74 y77 y78 y80 y79) | (neq y76 y79 & h y79 y80 q1 & _______g y74 y77 y78 ((y76 :: q1)) ((y76 :: q1))))));

_fG y81 y82 y83 y84 y85 = (fresh q1, q2, q3 in (((y84 == [] & _______g y81 y82 y83 [y85] [y85]) | (y84 == (q1 :: q2) & h q1 q2 q3 & _______g y81 y82 y83 ((y85 :: q3)) ((y85 :: q3))))));

_______g y87 y88 y89 y90 y91 = (fresh q1, q2 in (((y88 == y87 & y87 == Succ (Zero) & f y91 ((Zero :: [Succ (Succ (Zero))]))) | (y90 == (q1 :: q2) & neq (Succ (Zero)) y88 & y87 == Succ (Zero) & _____g y89 ((Zero :: [Succ (Succ (Zero))])) q1 q2))));

_neqHG y94 y95 y96 = (fresh q1, q2, q3 in (((y95 == Succ (q1) & y94 == Zero & hG Zero (Succ (q1)) y96) | (y95 == Zero & y94 == Succ (q1) & hG (Succ (q1)) Zero y96) | (y95 == Succ (q2) & hG (Succ (q3)) (Succ (q2)) y96 & y94 == Succ (q3) & neq q3 q2))));


? rr x0