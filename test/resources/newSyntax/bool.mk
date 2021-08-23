nando a b c = 
  ( a == Falso & b == Falso & c == Trueo ) |
  ( a == Falso & b == Trueo & c == Trueo ) |
  ( a == Trueo & b == Falso & c == Trueo ) |
  ( a == Trueo & b == Trueo & c == Falso ) ;

noto a na = nando a a na; 

oro a b c =
  nando a a aa & nando b b bb & nando aa bb c; 

ando a b c = nando a b ab & nando ab ab c ; 

? noto x nx & oro x nx t & ando Trueo t z
