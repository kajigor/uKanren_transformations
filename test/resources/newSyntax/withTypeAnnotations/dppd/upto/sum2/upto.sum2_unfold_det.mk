sumtrsquaretr y0 y1 y2 y3 y4 = (fresh q1, q2, q3, q4, q5, q6, q7, q8 in (((y0 == O & multiplyMultiplyAddAddAdd y4 y1 q1 q2 & _multiply y2 y2 q1 & _multiply y3 y3 q2) | (y4 == S (q3) & addMultiplyAddMultiplyAddAddAddAddAdd q4 q5 q6 q7 q8 q3 & y0 == S (q4) & multiplyMultiply y1 q5 q8 & _multiply y2 y2 q6 & _multiply y3 y3 q7))));

multiplyMultiplyAddAddAdd y5 y6 y10 y11 = (fresh q1, q2, q3, q4, q5, q6 in (((y6 == O & addAdd y5 y10 y11) | (y6 == S (q1) & addAddAdd q2 q3 y10 q1 q4 & y5 == S (q4) & _multiply (S (q1)) q1 q3 & add q5 q1 q6 & _multiply (S (q1)) q1 q6 & add q2 y11 (S (q5))))));

addAdd y13 y15 y16 = (fresh q1, q2 in (((y16 == O & add y13 y15 O) | (y16 == S (q1) & add q2 q1 O & add y13 y15 (S (q2))))));

_multiply y20 y21 y22 = (fresh q1, q2 in (((y22 == O & y21 == O) | (y21 == S (q1) & add y22 y20 q2 & _multiply y20 q1 q2))));

_addAdd y23 y24 y25 y27 = (fresh q1, q2 in (((y25 == O & add y27 y24 y23) | (y27 == S (q1) & y25 == S (q2) & _addAdd y23 y24 q2 q1))));

add y28 y29 y30 = (fresh q1, q2 in (((y29 == O & y28 == y30) | (y29 == S (q1) & y28 == S (q2) & add q2 q1 y30))));

__addAdd y31 y32 y33 y34 y35 = (fresh q1, q2, q3 in (((y33 == O & y31 == y35 & add y34 O y32) | (y35 == S (q1) & y34 == S (q2) & y33 == S (q3) & __addAdd y31 y32 q3 q2 q1))));

addMultiplyAddMultiplyAddAddAddAddAdd y38 y40 y42 y44 y45 y49 = (fresh q1, q2, q3, q4, q5, q6, q7, q8, q9 in (((y38 == O & _addAdd q1 y42 y40 y49 & add q1 y44 (S (y45))) | (y49 == S (q2) & addAddAddAdd q1 y40 y42 q3 (S (S (q4))) q2 & y38 == S (q3) & __addAdd q5 (S (S (q6))) q3 q7 q4 & _multiply (S (S (q3))) q3 q5 & add q6 q3 q8 & _multiply (S (S (q3))) q3 q8 & add q9 q7 y45 & add q1 y44 (S (S (q9)))))));

addAddAddAdd y50 y51 y52 y53 y54 y58 = (fresh q1, q2 in (((y53 == O & addAddAdd y50 y51 y52 y54 y58) | (y58 == S (q1) & y53 == S (q2) & addAddAddAdd y50 y51 y52 q2 y54 q1))));

addAddAdd y59 y60 y61 y62 y65 = (fresh q1, q2 in (((y62 == O & _addAdd y59 y61 y60 y65) | (y65 == S (q1) & y62 == S (q2) & addAddAdd y59 y60 y61 q2 q1))));

multiplyMultiply y66 y67 y68 = (fresh q1, q2, q3, q4, q5 in (((y67 == O & y66 == O & _multiply O O y68) | (y68 == S (q1) & __addAdd q2 q3 q4 q1 q5 & y66 == S (q4) & y67 == S (q5) & _multiply (S (q4)) q4 q2 & _multiply (S (q4)) q4 q3))));


? sumtrsquaretr x0 x1 x2 x3 x4