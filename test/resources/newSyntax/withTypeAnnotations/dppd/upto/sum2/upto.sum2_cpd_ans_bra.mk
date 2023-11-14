sumtrsquaretr y0 y1 y2 y3 y4 = (fresh q1, q2, q3, q4, q5, q6, q7, q8, q9 in (((y0 == O & squareSquareAddAddAddAdd y4 y1 q1 q2 q3 & _square y2 q1 & _square y3 q2 & _square O q3) | (y4 == S (q4) & addMultiplySquareAddAddAddAddAdd q5 q6 q7 q8 q9 q4 & y0 == S (q5) & squareSquare y1 q6 q9 & _square y2 q7 & _square y3 q8))));

squareSquareAddAddAddAdd y5 y6 y10 y12 y13 = (fresh q1, q2 in (((y6 == O & squareAddAddAdd y5 y10 y12 y13) | (y6 == S (q1) & y5 == S (q2) & addMultiplySquareAddAddAddAdd y10 y12 y13 q1 q2))));

squareAddAddAdd y15 y17 y19 y20 = (fresh q1 in (((y20 == O & addAdd y15 y17 y19) | (y20 == S (q1) & addAddAdd y15 y17 y19 q1))));

addAdd y22 y24 y25 = (fresh q1, q2 in (((y25 == O & add y22 y24 O) | (y25 == S (q1) & add q2 q1 O & add y22 y24 (S (q2))))));

add y26 y27 y28 = (fresh q1, q2 in (((y27 == O & y26 == y28) | (y27 == S (q1) & y26 == S (q2) & add q2 q1 y28))));

addAddAdd y29 y31 y32 y33 = (fresh q1, q2 in (((y33 == O & _addAdd y29 y31 y32 O) | (y33 == S (q1) & _addAdd y29 y31 y32 (S (q2)) & add q2 q1 O))));

_addAdd y35 y37 y38 y39 = (fresh q1, q2 in (((y38 == O & add y35 y37 (S (y39))) | (y38 == S (q1) & add q2 q1 (S (y39)) & add y35 y37 (S (q2))))));

addMultiplySquareAddAddAddAdd y41 y43 y44 y47 y50 = (fresh q1, q2, q3, q4 in (((y47 == O & _squareAddAddAdd y43 y44 y41 y50) | (y50 == S (q1) & squareAddAdd q2 y43 y44 (S (q3)) & y47 == S (q3) & __addAddAdd q2 y41 q3 (S (S (q4))) q1 & __addMultiply q3 q4 (S (q3)) q3))));

_squareAddAddAdd y53 y54 y56 y57 = (fresh q1 in (((y54 == O & _addAdd y57 y56 y53 O) | (y54 == S (q1) & _addAddAdd y53 y56 y57 q1))));

_addAddAdd y59 y60 y61 y62 = (fresh q1, q2 in (((y62 == O & _addAdd y61 y60 y59 (S (O))) | (y62 == S (q1) & _addAdd y61 y60 y59 (S (q2)) & add q2 q1 (S (O))))));

__addAddAdd y64 y65 y66 y67 y70 = (fresh q1, q2 in (((y66 == O & __addAdd y64 y65 y67 y70) | (y70 == S (q1) & y66 == S (q2) & __addAddAdd y64 y65 q2 y67 q1))));

__addAdd y71 y72 y73 y75 = (fresh q1, q2 in (((y73 == O & add y75 y72 y71) | (y75 == S (q1) & y73 == S (q2) & __addAdd y71 y72 q2 q1))));

multiply y76 y77 y78 = (fresh q1, q2 in (((y78 == O & y77 == O) | (y78 == S (q1) & y77 == S (q2) & __addMultiply y76 q1 y76 q2))));

squareAddAdd y79 y81 y82 y84 = (fresh q1 in (((y84 == O & _addAdd y79 y81 y82 O) | (y84 == S (q1) & addMultiplyAddAdd y79 y81 y82 q1))));

addMultiplyAddAdd y85 y87 y88 y90 = (fresh q1 in (((y90 == O & _addAdd y85 y87 y88 (S (S (S (O))))) | (y90 == S (q1) & addAddAddMultiplyAddAdd y85 y87 y88 q1))));

addAddAddMultiplyAddAdd y92 y94 y95 y99 = (fresh q1, q2, q3, q4, q5 in (((y99 == O & _addAdd y92 y94 y95 (S (S (S (S (S (S (S (S (O)))))))))) | (y99 == S (q1) & __addMultiply q1 q2 (S (S (S (q1)))) q1 & _addAdd y92 y94 y95 (S (S (S (q3)))) & add q3 q1 (S (S (S (S (q4))))) & add q4 q1 (S (S (S (S (q5))))) & add q5 q1 (S (S (S (S (q2)))))))));

__addMultiply y111 y113 y114 y115 = (fresh q1, q2 in (((y111 == O & multiply y114 y115 y113) | (y113 == S (q1) & y111 == S (q2) & __addMultiply q2 q1 y114 y115))));

_square y116 y117 = (fresh q1 in (((y117 == O & y116 == O) | (y116 == S (q1) & __addMultiply (S (q1)) y117 q1 q1))));

addMultiplySquareAddAddAddAddAdd y119 y121 y123 y126 y128 y130 = (fresh q1, q2, q3, q4, q5 in (((y119 == O & __squareAddAddAdd y126 y128 q1 y130 & add q1 y121 y123) | (y130 == S (q2) & addAddAddAdd q3 y121 y123 q4 (S (S (q5))) q2 & y119 == S (q4) & _squareAddAdd q3 y126 y128 (S (q4)) & __addMultiply q4 q5 (S (q4)) q4))));

__squareAddAddAdd y133 y135 y136 y137 = _addAdd y137 y136 y133 y135;

addAddAddAdd y138 y139 y140 y141 y142 y146 = (fresh q1, q2 in (((y141 == O & __addAddAdd y138 y140 y142 y139 y146) | (y146 == S (q1) & y141 == S (q2) & addAddAddAdd y138 y139 y140 q2 y142 q1))));

_squareAddAdd y147 y149 y151 y152 = (fresh q1 in (((y152 == O & add y147 y149 (S (y151))) | (y152 == S (q1) & _addMultiplyAddAdd y147 y149 y151 q1))));

_addMultiplyAddAdd y153 y154 y155 y157 = (fresh q1 in (((y157 == O & add y153 y154 (S (S (S (S (y155)))))) | (y157 == S (q1) & _addAddAddMultiplyAddAdd y153 y154 y155 q1))));

_addAddAddMultiplyAddAdd y160 y161 y162 y165 = (fresh q1, q2, q3, q4, q5 in (((y165 == O & add y160 y161 (S (S (S (S (S (S (S (S (S (y162))))))))))) | (y165 == S (q1) & __addMultiply q1 q2 (S (S (S (q1)))) q1 & __addAdd y162 (S (S (S (S (q3))))) q1 q4 & add q3 q1 (S (S (S (S (q5))))) & add q5 q1 (S (S (S (S (q2))))) & add y160 y161 (S (S (S (S (q4)))))))));

squareSquare y169 y170 y171 = (fresh q1, q2 in (((y170 == O & y169 == O & _square O y171) | (y170 == S (q1) & y169 == S (q2) & addMultiplySquare y171 q2 q1))));

addMultiplySquare y172 y174 y175 = (fresh q1, q2, q3 in (((y175 == O & y174 == O & _square (S (O)) y172) | (y175 == S (q1) & __addMultiply q2 q3 (S (q2)) q2 & y174 == S (q2) & add q1 q2 (S (S (q3))) & _square (S (S (q2))) y172))));


? sumtrsquaretr x0 x1 x2 x3 x4